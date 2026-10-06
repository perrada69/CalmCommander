"""Editor (edit.ccp): psani, mazani, pohyb, hex rezim a ukladani.

Ulozeny soubor (sluzba EXTRACT) se porovnava s modelem editoru v Pythonu.
Editor ceka mezi klavesami pres HALT, takze test bezi s prerusenim.
"""

import random
import unittest

from pluginhost import PluginHost, C_DIRTY, CTX

VIEWTYPE_EDIT = 12
BREAK, LEFT, RIGHT, BACKSPACE, ENTER = 1, 8, 9, 12, 13
SAVE, SAVE_AS, HEX, TEXT = 128, 129, 130, 131


class Model:
    """Chovani editoru: znak se vklada pred kurzor, kurzor nejde za posledni
    bajt souboru, backspace maze znak pred kurzorem."""

    def __init__(self, data):
        self.buf = bytearray(data)
        self.cur = 0

    def clamp(self, pos):
        return pos if pos < len(self.buf) else max(0, len(self.buf) - 1)

    def key(self, key):
        if 32 <= key < 128 or key == ENTER:
            self.buf.insert(self.cur, key)
            self.cur = self.clamp(self.cur + 1)
        elif key == RIGHT:
            self.cur = self.clamp(self.cur + 1)
        elif key == LEFT:
            self.cur = max(0, self.cur - 1)
        elif key == BACKSPACE and self.buf:
            if self.cur:
                self.cur -= 1
                del self.buf[self.cur]
            elif len(self.buf) == 1:
                del self.buf[0]
            self.cur = self.clamp(self.cur)


class EditPlugin(unittest.TestCase):
    def edit(self, data, keys, name="note.txt"):
        host = PluginHost(self, "edit", name, data, VIEWTYPE_EDIT, keys=keys,
                          menu_sites=["plugin_start.input", "try_exit.wait", "line_input.wait"])
        host.m.interrupts = True
        host.m.cpu.iff1 = host.m.cpu.iff2 = 1     # CC vola plugin s povolenym prerusenim
        host.run(max_steps=5_000_000)
        return host

    def test_type_and_save(self):
        data = b"Hello\nWorld\n"
        keys = [ord(c) for c in "Hi "] + [RIGHT] * 5 + [ord("!"), SAVE, BREAK]
        host = self.edit(data, keys)
        model = Model(data)
        for key in keys[:-2]:
            model.key(key)
        self.assertEqual(host.files, [("note.txt", bytes(model.buf), None)])
        self.assertEqual(bytes(model.buf), b"Hi Hello!\nWorld\n")
        self.assertEqual(host.m.get(CTX + C_DIRTY), 1, "CC ma panel nacist znovu")
        self.assertIn("Hi Hello!", host.rows()[3])

    def test_random_edits_match_model(self):
        rnd = random.Random(2026)
        data = b"The quick brown fox\njumps over\nthe lazy dog.\n"
        for round_ in range(3):
            with self.subTest(round=round_):
                keys = [rnd.choice([LEFT, RIGHT, RIGHT, BACKSPACE, ord("x"), ord("Y"), ord(" ")])
                        for _ in range(25)]
                host = self.edit(data, keys + [SAVE, BREAK])
                model = Model(data)
                for key in keys:
                    model.key(key)
                self.assertEqual(host.files[0][1], bytes(model.buf), keys)

    def test_backspace_at_start_does_nothing(self):
        host = self.edit(b"abc\n", [BACKSPACE, BACKSPACE, SAVE, BREAK])
        self.assertEqual(host.files, [("note.txt", b"abc\n", None)])

    def test_hex_mode_overwrites_bytes(self):
        data = bytes(range(16))
        host = self.edit(data, [ord("4"), ord("1"), ord("f"), ord("F"), SAVE, BREAK], name="data.bin")
        self.assertEqual(host.files[0][1], b"\x41\xff" + data[2:], "binarni soubor se otevre v hex")

    def test_hex_and_back_to_text(self):
        data = b"ABCD\n"
        host = self.edit(data, [HEX, ord("6"), ord("1"), TEXT, ord("z"), SAVE, BREAK])
        self.assertEqual(host.files[0][1], b"azBCD\n")

    def test_exit_with_changes_asks(self):
        host = self.edit(b"abc\n", [ord("x"), BREAK, ord("i")])
        self.assertEqual(host.files, [], "I = zahodit zmeny")
        self.assertIn("Changed:", host.snapshots[-1][31])

    def test_exit_with_changes_save(self):
        host = self.edit(b"abc\n", [ord("x"), BREAK, ord("s")])
        self.assertEqual(host.files, [("note.txt", b"xabc\n", None)])

    def test_exit_question_can_be_cancelled(self):
        host = self.edit(b"abc\n", [ord("x"), BREAK, BREAK, ord("y"), SAVE, BREAK])
        self.assertEqual(host.files, [("note.txt", b"xyabc\n", None)])

    def test_save_as(self):
        keys = [ord("x"), SAVE_AS] + [BACKSPACE] * 8 + [ord(c) for c in "new.txt"] + [ENTER, BREAK]
        host = self.edit(b"abc\n", keys)
        self.assertEqual(host.files, [("new.txt", b"xabc\n", None)])

    def test_save_as_cancelled(self):
        host = self.edit(b"abc\n", [ord("x"), SAVE_AS, BREAK, BREAK, ord("i")])
        self.assertEqual(host.files, [])


if __name__ == "__main__":
    unittest.main()
