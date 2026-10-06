"""Nastaveni (settings.ccp): barevna schemata, zachyceni klavesy, ulozeni
a zruseni zmen, zapis palety do Next registru.

Vychozi konfigurace a dekodovaci tabulky klaves jsou skutecna data z cc.bin
(podle symbolu BASIC sestaveni). Klavesy dava sluzba KEYSCAN.
"""

import unittest

from cctest import ROOT, build_basic, build_plugin
from fakeesx import CTX, SERVICES, SVC_TRAPS, FeatureHost

ABI = 5
STYLES, ACTIONS = 16, 33
COLOUR_BYTES = STYLES * 2 + 4
PALETTE, KEYS, SCHEME, TABLES = 0x9400, 0x9480, 0x94C0, 0x9500
BREAK, ENTER, DOWN, UP, LEFT, RIGHT = 1, 13, 10, 11, 8, 9


def cc_data():
    """Vychozi Cfg a tabulky klaves z cc.bin."""
    s = build_basic()
    image = (ROOT / "cc.bin").read_bytes()
    take = lambda name, size: image[s[name] - s["S1"]:s[name] - s["S1"] + size]
    return {"palette": take("cfgStyleColours", COLOUR_BYTES), "keys": take("cfgKeyBindings", ACTIONS),
            "scheme": take("cfgColourScheme", 1)[0], "norm": take("NORMTAB", 40),
            "caps": take("CAPSTAB", 40), "sym": take("SYMTAB", 40)}


def palette_writes(palette):
    """Ocekavane zapisy apply_colours: pro kazdy styl index 16*g (pozadi)
    a 16*g+3 (popredi), barva RRRGGGBB a modry bit z poslednich 4 bajtu."""
    blue = palette[STYLES * 2:]
    out = [(0x43, 0x30)]
    for g in range(STYLES):
        for k, index in ((2 * g, 16 * g), (2 * g + 1, 16 * g + 3)):
            out += [(0x40, index), (0x44, palette[k]), (0x44, (blue[k // 8] >> (k % 8)) & 1)]
    return out


class Settings(unittest.TestCase):
    def setUp(self):
        self.cfg = cc_data()

    def run_plugin(self, keys):
        binary, s = build_plugin("settings")
        host = FeatureHost(binary, {})
        m = host.m
        cfg = self.cfg
        m.write(PALETTE, cfg["palette"])
        m.write(KEYS, cfg["keys"])
        m.put(SCHEME, cfg["scheme"])
        m.write(TABLES, cfg["norm"] + cfg["caps"] + cfg["sym"])
        queue, state = list(keys), {"release": False, "idle": 0}

        def keyscan():
            """Jako KEYSCAN v CC: E = index klavesy ($FF nic), D = shift."""
            if state["release"] or not queue:
                state["release"] = False
                state["idle"] += 1
                if state["idle"] > 1000:
                    raise AssertionError("plugin ceka na klavesu, ale zadna neni")
                m.cpu.de = 0xFFFF
            else:
                state["release"], state["idle"] = True, 0
                m.cpu.de = self.scan_code(queue.pop(0))
            m.ret()

        table = bytearray()
        for i, handler in enumerate([host.svc_print, host.svc_window, host.done, keyscan]):
            table += (SVC_TRAPS + 4 * i).to_bytes(2, "little")
            m.trap(SVC_TRAPS + 4 * i, handler)
        for addr in (TABLES + 80, TABLES + 40, TABLES):           # SYMTAB, CAPSTAB, NORMTAB
            table += addr.to_bytes(2, "little")
        m.write(SERVICES, bytes(table))
        ctx = bytearray(9)
        ctx[0] = ABI
        ctx[1:3], ctx[3:5], ctx[7:9] = (PALETTE.to_bytes(2, "little"), KEYS.to_bytes(2, "little"),
                                        SCHEME.to_bytes(2, "little"))
        m.write(CTX, bytes(ctx))
        m.interrupts = True
        m.cpu.iff1 = m.cpu.iff2 = 1
        host.run(s["plugin_start"], max_steps=5_000_000)
        self.assertEqual(queue, [], "vsechny klavesy pouzity")
        self.host = host
        return {"result": m.get(CTX + 5), "error": m.get(CTX + 6),
                "palette": bytes(m.mem[PALETTE:PALETTE + COLOUR_BYTES]),
                "keys": bytes(m.mem[KEYS:KEYS + ACTIONS]), "scheme": m.get(SCHEME)}

    def scan_code(self, key):
        """Znak nebo kod -> DE z KEYSCAN (hleda v tabulkach z cc.bin)."""
        cfg = self.cfg
        code = key if isinstance(key, int) else ord(key)
        for shift, table in ((0xFF, cfg["norm"]), (0x27, cfg["caps"]), (0x18, cfg["sym"])):
            if code in table[:40] and code:
                return shift << 8 | table.index(code)
        raise KeyError(key)

    def nextreg_writes(self):
        return [(reg, value) for kind, reg, value in self.host.m.events
                if kind == "nextreg" and reg in (0x40, 0x43, 0x44)]

    def test_cancel_restores_everything(self):
        free = next(c for c in "xqwzjk" if ord(c) not in self.cfg["keys"])
        out = self.run_plugin([ENTER, RIGHT, ENTER, free, BREAK])
        self.assertEqual(out["result"], 0)
        self.assertEqual((out["palette"], out["keys"], out["scheme"]),
                         (self.cfg["palette"], self.cfg["keys"], self.cfg["scheme"]))
        writes = self.nextreg_writes()
        self.assertEqual(writes[-len(palette_writes(out["palette"])):], palette_writes(out["palette"]),
                         "zruseni vrati i paletu v registrech")

    def test_save_keeps_scheme_and_key(self):
        free = next(c for c in "xqwzjk" if ord(c) not in self.cfg["keys"])
        out = self.run_plugin([ENTER, RIGHT, DOWN, ENTER, free, "s"])
        self.assertEqual(out["result"], 1)
        self.assertEqual(out["scheme"], (self.cfg["scheme"] + 1) % 3)
        expected = bytearray(self.cfg["keys"])
        expected[1] = ord(free)
        self.assertEqual(out["keys"], bytes(expected))
        self.assertEqual(self.nextreg_writes(), palette_writes(out["palette"]),
                         "apply_colours zapsal novou paletu")
        self.assertNotEqual(out["palette"], self.cfg["palette"])

    def test_three_cycles_return_to_default(self):
        out = self.run_plugin([ENTER, ENTER, ENTER, "s"])
        self.assertEqual(out["scheme"], 0)
        self.assertEqual(out["palette"], self.cfg["palette"], "schema 0 = vychozi barvy z cc.asm")

    def test_conflicting_key_is_refused(self):
        used = chr(self.cfg["keys"][20])                     # klavesa jine akce
        free = next(c for c in "xqwzjk" if ord(c) not in self.cfg["keys"])
        out = self.run_plugin([RIGHT, ENTER, used, free, "s"])
        self.assertTrue(any("Shortcut already used" in t for t in self.host.printed))
        expected = bytearray(self.cfg["keys"])
        expected[0] = ord(free)
        self.assertEqual(out["keys"], bytes(expected))

    def test_screen(self):
        self.run_plugin([BREAK])
        text = "\n".join(self.host.text())
        for label in ("Settings", "Colours", "Keys", "Colour scheme", "Normal files"):
            self.assertIn(label, text)


if __name__ == "__main__":
    unittest.main()
