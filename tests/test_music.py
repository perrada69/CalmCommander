"""Hudebni pluginy: prehravani s prerusenim a pravidlo MMU2.

Obsluha IM1 v ROM zapisuje FRAMES a stav klavesnice do systemovych promennych
v MMU2. Kdyz prerusenim prijde ve chvili, kdy je v MMU2 skladba, zapise se do
ni (v BASIC verzi) nebo v dot verzi spadne obsluha NextZXOS. Testy proto pri
kazdem preruseni kontroluji, ze MMU2 drzi stranku 10 a IY = $5C3A.
"""

import struct
import unittest

from cctest import ROOT
from pluginhost import PluginHost, assert_golden

VIEWTYPE_PT3, VIEWTYPE_PT2, VIEWTYPE_STC, VIEWTYPE_STP, VIEWTYPE_SQT = 3, 4, 5, 6, 7
FIXTURES = ROOT / "tests" / "fixtures"
ENTER, SPACE = (0xBF, 0), (0x7F, 0)


def turbosound(module):
    """Dva moduly PT3 za sebou s paticku, kterou hleda setup_pt3_mode."""
    size = struct.pack("<H", len(module))
    return module + module + b"PT3!" + size + b"PT3!" + size + b"02TS"


class Music(unittest.TestCase):
    # plugin po startu 25 snimku ignoruje klavesnici (input_arm_delay), at ho
    # nezastavi klavesa, kterou byl spusten - stisk tedy prijde az potom
    def play(self, plugin, view_type, data, key, press_at=40, hold=5):
        host = PluginHost(self, plugin, "song", data, view_type)
        m = host.m
        m.interrupts = True
        bad = []

        def frame(m):
            if m.mmu[2] != 10 or m.cpu.iy != 0x5C3A:
                bad.append((m.frames, m.mmu[2], m.cpu.iy, m.cpu.pc))
            if m.frames == press_at:
                m.keys = {key}
            elif m.frames == press_at + hold:
                m.keys = set()

        m.isr_checks.append(frame)
        result = host.run(max_steps=5_000_000)
        ay = [v for p, v in m.port_writes if p == 0xFFFD]
        return host, result, bad, ay

    def check(self, plugin, view_type, data, name):
        for key, expected in ((ENTER, 0), (SPACE, 1)):
            with self.subTest(key="ENTER" if key == ENTER else "SPACE"):
                host, result, bad, ay = self.play(plugin, view_type, data, key)
                m = host.m
                self.assertEqual(bad, [], "preruseni se skladbou v MMU2 (snimek, MMU2, IY, PC)")
                self.assertGreater(m.frames, 40, "prehravalo se s prerusenim")
                self.assertEqual(result, expected, "ENTER = konec (0), SPACE = dalsi (1)")
                self.assertEqual(m.mmu[2:6], [10, 11, 4, 5], "MMU2-5 vraceny")
                self.assertTrue(ay, "prehravac psal do AY")
                if key == ENTER:
                    assert_golden(self, f"music_{name}", host.text())

    def test_pt3(self):
        self.check("pt3test", VIEWTYPE_PT3, (ROOT / "000.pt3").read_bytes(), "pt3")

    def test_pt3_turbosound(self):
        data = turbosound((ROOT / "000.pt3").read_bytes())
        self.check("pt3test", VIEWTYPE_PT3, data, "pt3_ts")
        host, *_ = self.play("pt3test", VIEWTYPE_PT3, data, ENTER)
        self.assertTrue(any("PT3TS" in line for line in host.text()), "rozpoznano TurboSound")

    def test_sqt(self):
        song = ROOT / "Factor6 - Cover of 'Hung Up' by Madonna (2xAY ACB).sqt"
        self.check("sqtest", VIEWTYPE_SQT, song.read_bytes(), "sqt")

    def test_pt2(self):
        self.check("pt2test", VIEWTYPE_PT2, (FIXTURES / "Panda - RUSH.pt2").read_bytes(), "pt2")

    def test_stc(self):
        self.check("stctest", VIEWTYPE_STC, (FIXTURES / "Sergey Bulba - Again.stc").read_bytes(), "stc")

    def test_stp(self):
        self.check("stptest", VIEWTYPE_STP, (FIXTURES / "ABK0.stp").read_bytes(), "stp")


if __name__ == "__main__":
    unittest.main()
