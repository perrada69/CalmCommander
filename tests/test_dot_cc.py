"""Cast CC, ktera vaze dot command (cc.asm s CC_DOT), na skutecnem Z80 kodu."""

import unittest

from cctest import Machine, SENTINEL, ROOT, build_dot, hidden


LOAD, CLEAR, SPECTRUM, OUT = 0xEF, 0xFD, 0xA3, 0xDF
TABCOMP = {0: 0xA0, 1: 0x90, 2: 0xC0, 3: 0xB0}     # 128k, 48k, Pentagon, Next


def load_cc(m, cc):
    """Nahraje CC jako zavadec: hlavni kod od S1, extra banku do jeji stranky."""
    main = (ROOT / "build/dot/ccd.bin").read_bytes()
    xb = (ROOT / "build/dot/ccd_xb.bin").read_bytes()
    m.write(cc["S1"], main)
    m.set_page(cc["dot_xb_page"], xb)


class DotCC(unittest.TestCase):
    def setUp(self):
        self.cc, _ = build_dot()
        self.m = Machine()
        load_cc(self.m, self.cc)

    def command(self, name, comp=3):
        """extra_dot_basic_cmd pro soubor name -> (Z, delka, priznaky, tokeny)."""
        m, cc = self.m, self.cc
        m.write(cc["cmd2"], name.encode() + b"\0")
        m.put(cc["cursorComp"], comp)
        m.map(7, cc["dot_xb_page"])
        m.call(cc["extra_dot_basic_cmd"], sp=0x7100)
        m.map(7, 1)
        z = bool(m.cpu.f & 0x40)
        buf = cc["dot_cmd_buf"]
        size = m.get(buf)
        return z, size, m.get(buf + 1), bytes(m.mem[buf + 2:buf + 2 + size])

    def test_nex_has_no_basic_command(self):
        z, *_ = self.command("Game.NEX")
        self.assertTrue(z, "NEX jde pres nexload, ne pres BASIC")

    def test_bas(self):
        z, size, flags, body = self.command("baSnake.bas")
        self.assertFalse(z)
        self.assertEqual(flags, 0, "BAS na rychlosti uzivatele")
        self.assertEqual(body, b":" + bytes([CLEAR]) + b"65367" + hidden(65367)
                         + b":" + bytes([LOAD]) + b'"baSnake.bas"')
        self.assertEqual(size, len(body))

    def test_tap_next(self):
        _, _, flags, body = self.command("TRASHMAN.TAP", comp=3)
        self.assertEqual(flags, 1)
        self.assertEqual(body, b':.tapein "TRASHMAN.TAP":' + bytes([LOAD]) + b'"t:":'
                         + bytes([LOAD]) + b'""')

    def test_tap_other_machines_follow_cc_bas_loader(self):
        for comp in (0, 1, 2):
            with self.subTest(comp=comp):
                _, _, flags, body = self.command("game.tap", comp=comp)
                self.assertEqual(flags, 1)
                expected = (b":" + bytes([SPECTRUM])
                            + b":" + bytes([OUT]) + b"9275" + hidden(9275) + b",3" + hidden(3)
                            + b":" + bytes([OUT]) + b"9531" + hidden(9531) + b",0" + hidden(TABCOMP[comp])
                            + b':.tapein "game.tap":' + bytes([LOAD]) + b'""')
                self.assertEqual(body, expected)

    def test_snapshots_use_spectrum(self):
        for name in ("game.sna", "GAME.Z80", "demo.snx"):
            with self.subTest(name=name):
                _, _, flags, body = self.command(name)
                self.assertEqual(flags, 1, "snapshoty na 3,5 MHz")
                self.assertEqual(body, b":" + bytes([SPECTRUM]) + b'"' + name.encode() + b'"')

    def test_long_name_fits(self):
        name = "A" * 56 + ".tap"
        _, size, _, body = self.command(name, comp=1)
        self.assertIn(name.encode(), body)
        self.assertLess(size + 2, 160, "vejde se do bufferu zavadece (INJECT_MAX)")

    def return_to_loader(self, entry, mmu7=24, a=None):
        """Zavola cestu zpet do zavadece: zasobnik zavadece je ve strance 1."""
        m, cc = self.m, self.cc
        m.put(0xFFF0, SENTINEL, 2)
        m.put(cc["dot_basic_sp"], 0xFFF0, 2)
        m.put(cc["dot_attached"], 1)
        m.map(7, mmu7)
        m.cpu.sp = 0x7100
        m.cpu.pc = cc[entry]
        if a is not None:
            m.cpu.a = a
        m.run()
        return m.get(cc["dot_exit_code"])

    def test_dot_return_maps_page_1_for_loader_stack(self):
        """CC v MMU7 casto nechava LFN stranku - navrat musi namapovat stranku 1."""
        code = self.return_to_loader("dot_return", mmu7=24, a=0)
        self.assertEqual(code, 0)
        self.assertEqual(self.m.mmu[7], 1)
        self.assertEqual(self.m.cpu.sp, 0xFFF2)

    def test_attached_launch_codes(self):
        cc = self.cc
        for name, expected in (("game.nex", cc["DOT_EXIT_LAUNCH"]),
                               ("game.bas", cc["DOT_EXIT_BASIC"]),
                               ("game.tap", cc["DOT_EXIT_BASIC"])):
            with self.subTest(name=name):
                self.m.write(cc["cmd2"], name.encode() + b"\0")
                self.m.put(cc["cursorComp"], 3)
                self.assertEqual(self.return_to_loader("dot_attached_launch"), expected)
                self.assertEqual(self.m.mmu[7], 1)

    def test_dot_entry_switches_to_cc_stack(self):
        m, cc = self.m, self.cc
        m.trap(cc["START"], lambda: setattr(m.cpu, "pc", SENTINEL))
        m.cpu.sp = 0xFFFE
        m.cpu.pc = cc["dot_entry"]
        m.run()
        self.assertEqual(m.word(cc["dot_basic_sp"]), 0xFFFE, "SP zavadece ulozeno")
        self.assertEqual(m.cpu.sp, cc["S1"], "CC bezi na svem zasobniku jako po CLEAR 28927")


if __name__ == "__main__":
    unittest.main()
