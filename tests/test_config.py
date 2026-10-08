"""cc.cfg: nacteni (createCfg) a priprava (EXTRA_CFG_PREPARE) pro soubory ze
vsech verzi CC na skutecnem Z80 kodu obou sestaveni.

Formaty souboru (bez 128B hlavicky +3DOS):
  CC 0.6-1.2  539 B  cesty, kurzory, okna, cursorComp
  (dev)       541 B  + cfgUseKMouse, cfgDirsFirst
  CC 1.3      542 B  + cfgSortMode
  CC 1.4      591 B  + mapa palety (16 B) + klavesy (33 B)
  CC 1.5+     613 B  + barvy (36 B) misto mapy, klavesy, verze 1, schema

+3DOS volani pro cc.cfg (IDE_PATH, DOS_OPEN/READ/WRITE/CLOSE) jsou pasti
v Pythonu nad jednim souborem v pameti.
"""

import random
import unittest

from cctest import ROOT
from test_core import EXTRA_PAGE, STACK, cc_machine

SIZE = 613
OLD_SIZES = {"0.6": 539, "dev": 541, "1.3": 542, "1.4": 591}
EOF_ERROR = 25


class FakeCfgDos:
    """Jediny soubor c:/sys/cc.cfg; None = soubor neexistuje."""

    def __init__(self, m, data):
        self.m, self.data, self.pos = m, None if data is None else bytearray(data), 0
        self.opened = []
        for addr, handler in ((0x01B1, self.ide_path), (0x0106, self.open), (0x0112, self.read),
                              (0x0115, self.write), (0x0109, self.close)):
            m.trap(addr, lambda h=handler: self.done(h()))

    def done(self, result):
        ok, zero = result
        cpu = self.m.cpu
        cpu.f = (cpu.f & ~0x41) | (0x01 if ok else 0) | (0x40 if zero else 0)
        self.m.ret()

    def ide_path(self):
        return True, False

    def open(self):
        self.opened.append(self.m.cstring(self.m.cpu.hl, end=(0xFF,)).decode())
        self.pos = 0
        if self.data is None:
            self.data = bytearray()
            return True, True                       # novy soubor: Z
        return True, False

    def read(self):
        cpu = self.m.cpu
        want = cpu.de or 0x10000
        chunk = self.data[self.pos:self.pos + want]
        self.m.write(cpu.hl, bytes(chunk))
        self.pos += len(chunk)
        if len(chunk) < want:                       # konec souboru: chyba, DE = neprectene
            cpu.a, cpu.de = EOF_ERROR, want - len(chunk)
            return False, False
        return True, False

    def write(self):
        cpu = self.m.cpu
        data = bytes(self.m.mem[cpu.hl:cpu.hl + cpu.de])
        self.data[self.pos:self.pos + len(data)] = data
        self.pos += len(data)
        return True, False

    def close(self):
        return True, False


def migrate_colours(defaults, palette_map):
    """Model prevodu barev z CC 1.4 (EXTRA_CFG_PREPARE): styl si vezme barvy
    fyzicke skupiny z horniho nibblu mapy, i s nejnizsimi bity modre."""
    out = bytearray(defaults)
    out[32:36] = bytes(4)
    for style in range(16):
        src, dst = (palette_map[style] & 0xF0) >> 3, style * 2
        out[dst:dst + 2] = defaults[src:src + 2]
        for channel in range(2):
            bit = src + channel
            if defaults[32 + bit // 8] & (1 << bit % 8):
                out[32 + (dst + channel) // 8] |= 1 << (dst + channel) % 8
    return bytes(out)


def for_each_build(cls):
    for variant in ("basic", "dot"):
        name = f"{cls.__name__}_{variant}"
        globals()[name] = type(name, (cls, unittest.TestCase), {"variant": variant})
    return cls


@for_each_build
class Config:
    variant = None

    def setUp(self):
        self.m, self.s = cc_machine(self.variant)
        s = self.s
        self.cfg = s["Cfg"]
        self.defaults = bytes(self.m.mem[self.cfg:self.cfg + SIZE])
        self.off = {name: s[name] - self.cfg for name in (
            "PATHRIGHT", "POSKURZL", "cursorComp", "cfgUseKMouse", "cfgDirsFirst",
            "cfgSortMode", "cfgStyleColours", "cfgKeyBindings", "cfgColourVersion",
            "cfgColourScheme")}
        self.assertEqual(s["DelkaCfg"], SIZE)
        self.assertEqual(self.off["cfgColourVersion"], 611)

    # --- soubory -------------------------------------------------------

    def theme(self, data):
        o = self.off
        return (bytes(data[o["cfgStyleColours"]:o["cfgStyleColours"] + 36]),
                bytes(data[o["cfgKeyBindings"]:o["cfgKeyBindings"] + 33]),
                data[o["cfgColourVersion"]], data[o["cfgColourScheme"]])

    def default_theme(self):
        return self.theme(self.defaults)

    def user_part(self, size):
        """Prvnich size bajtu, jak je zapsala starsi verze: vlastni cesty,
        kurzory a prepinace."""
        data = bytearray(self.defaults[:size])
        o = self.off
        left, right = b"D:/games/demo\xff", b"C:/sys\xff"
        data[0:len(left)] = left
        data[o["PATHRIGHT"]:o["PATHRIGHT"] + len(right)] = right
        data[o["POSKURZL"]] = 7
        data[o["cursorComp"]] = 2
        if size > o["cfgUseKMouse"]:
            data[o["cfgUseKMouse"]] = 1
            data[o["cfgDirsFirst"]] = 1
        if size > o["cfgSortMode"]:
            data[o["cfgSortMode"]] = 2
        return data

    def custom_keys(self):
        keys = list(self.theme(self.defaults)[1])
        random.Random(7).shuffle(keys)
        return bytes(keys)

    def load(self, data):
        """createCfg + EXTRA_CFG_PREPARE jako pri startu CC. Vraci Cfg."""
        m, s = self.m, self.s
        self.dos = FakeCfgDos(m, data)
        m.call(s["createCfg"], sp=STACK)
        m.map(6, 0)
        m.map(7, EXTRA_PAGE)
        m.call(s["EXTRA_CFG_PREPARE"], sp=STACK)
        self.assertEqual(self.dos.opened, ["c:/sys/cc.cfg"])
        return bytes(m.mem[self.cfg:self.cfg + SIZE])

    def assert_user_part_kept(self, got, data, size):
        self.assertEqual(got[:size], bytes(data[:size]), "cesty, kurzory a prepinace ze souboru")

    # --- testy ---------------------------------------------------------

    def test_missing_file_is_created_with_defaults(self):
        got = self.load(None)
        self.assertEqual(got, self.defaults)
        self.assertEqual(bytes(self.dos.data), self.defaults)

    def test_current_file_is_kept(self):
        data = self.user_part(SIZE)
        o = self.off
        colours = bytes(random.Random(3).randrange(256) for _ in range(36))
        data[o["cfgStyleColours"]:o["cfgStyleColours"] + 36] = colours
        data[o["cfgKeyBindings"]:o["cfgKeyBindings"] + 33] = self.custom_keys()
        data[o["cfgColourScheme"]] = 3
        self.assertEqual(self.load(data), bytes(data))

    def test_files_without_theme_get_default_keys_and_colours(self):
        """CC 0.6-1.3: driv se konec souboru bral jako format 1.4 a z vychozich
        barev a klaves vznikl nesmysl (sipky, ENTER i '5' = Quit jinde)."""
        for version, size in OLD_SIZES.items():
            if size >= 591:
                continue
            with self.subTest(version=version):
                self.setUp()
                data = self.user_part(size)
                got = self.load(data)
                self.assert_user_part_kept(got, data, size)
                self.assertEqual(self.theme(got), self.default_theme())
                self.assertEqual(got[size:], self.defaults[size:])

    def test_cc14_file_is_migrated(self):
        o = self.off
        keys = self.custom_keys()
        for name, palette_map in (("identita", bytes(16 * i for i in range(16))),
                                  ("prohozene styly", bytes([16, 0] + [16 * i for i in range(2, 16)]))):
            with self.subTest(map=name):
                self.setUp()
                data = self.user_part(542) + palette_map + keys
                self.assertEqual(len(data), 591)
                got = self.load(data)
                self.assert_user_part_kept(got, data, 542)
                colours, got_keys, version, scheme = self.theme(got)
                self.assertEqual(got_keys, keys)
                self.assertEqual(colours, migrate_colours(self.default_theme()[0], palette_map))
                self.assertEqual((version, scheme), (1, 3))

    def test_broken_keys_are_reset(self):
        """Soubor, ktery uz drivejsi verze spatne prevedla a ulozila (verze 1),
        a soubor z CC 1.4 spustene po 1.5 (prepsala jen prvnich 591 B)."""
        o = self.off
        default_colours, default_keys = self.default_theme()[:2]
        # stary chybny prevod 539B souboru: konec souboru = vychozi data
        broken = self.user_part(539) + self.defaults[539:]
        palette_map = self.defaults[542:558]
        broken[o["cfgStyleColours"]:o["cfgStyleColours"] + 36] = migrate_colours(default_colours, palette_map)
        broken[o["cfgKeyBindings"]:o["cfgKeyBindings"] + 33] = self.defaults[558:591]
        broken[o["cfgColourScheme"]] = 3
        # CC 1.4 po 1.5: mapa palety a klavesy 1.4 pres zacatek novych dat
        hybrid = self.user_part(SIZE)
        hybrid[542:591] = bytes(16 * i for i in range(16)) + default_keys
        for name, data in (("spatny prevod", broken), ("1.4 po 1.5", hybrid)):
            with self.subTest(file=name):
                self.setUp()
                got = self.load(data)
                self.assert_user_part_kept(got, data, 542)
                self.assertEqual(self.theme(got), self.default_theme())

    def test_invalid_key_values(self):
        o = self.off
        for name, change in (("nula", (5, 0)), ("BREAK", (9, 1)), ("dvakrat", (32, None))):
            with self.subTest(key=name):
                self.setUp()
                data = self.user_part(SIZE)
                keys = bytearray(self.custom_keys())
                index, value = change
                keys[index] = keys[0] if value is None else value
                data[o["cfgKeyBindings"]:o["cfgKeyBindings"] + 33] = keys
                got = self.load(data)
                self.assertEqual(self.theme(got), self.default_theme())

    def test_out_of_range_switches_are_reset(self):
        o = self.off
        data = self.user_part(SIZE)
        data[o["cursorComp"]] = 7
        data[o["cfgUseKMouse"]] = 5
        data[o["cfgDirsFirst"]] = 1
        data[o["cfgSortMode"]] = 9
        got = self.load(data)
        self.assertEqual([got[o[n]] for n in ("cursorComp", "cfgUseKMouse", "cfgDirsFirst", "cfgSortMode")],
                         [0, 0, 1, 0])
        for name, value in (("cursorComp", 3), ("cfgSortMode", 2)):
            with self.subTest(highest=name):
                self.setUp()
                data = self.user_part(SIZE)
                data[o[name]] = value
                self.assertEqual(self.load(data)[o[name]], value)


if __name__ == "__main__":
    unittest.main()
