"""Ovladani mysi (kmouse/ui.a80) v obou sestavenich CC.

Napovedy klaves se kontroluji na skutecnych textech z dialogu CC a pluginu:
klik na slovo v radku tilemapy ma dat stejnou klavesu, jakou napoveda
popisuje. Dale pravidla podle kontextu (panely, menu, dialog, plugin) a cela
cesta pres INKEY, KEYSCAN_UI a dialogy potvrd a prepsani souboru.
"""

import unittest

from cctest import SENTINEL
from test_core import EXTRA_PAGE, STACK, cc_machine

TILEMAP = 0x4000
CTX_DIALOG, CTX_MAIN, CTX_PLUGIN = 0, 1, 2
LEFT, RIGHT = 2, 1                        # bity tlacitek v CONTRB
NO_KEY = 0xFFFF
ENTER, BREAK, SPACE, DELETE = 13, 1, 32, 12
UP, DOWN, CUR_LEFT, CUR_RIGHT = 11, 10, 8, 9
VIEWTYPE_TEXT, VIEWTYPE_ZXSCREEN, VIEWTYPE_PT3, VIEWTYPE_TAP = 1, 2, 3, 11

BOTTOM_BAR = [(2, "1: LEFT"), (10, "2: RIGHT"), (19, "3: VIEW"), (27, "4: EDIT"),
              (35, "5: COPY"), (43, "6: MOVE"), (51, "7: MKDIR"), (60, "8: DELETE"),
              (70, "0: MENU")]
TEXT_STATUS = "Line:     1 /   120   Mode: TEXT   BREAK=exit T=text H=hex D=dec /=find"
TAP_HELP = " BREAK=exit  Up/Dn  PgUp/PgDn  e:export  CAPS+e:export all  d:+3DOS header"
HELP_HINT = "TYPE filter  DELETE erase  UP/DOWN scroll  BREAK close"
SETTINGS_HINT = "LEFT/RIGHT tab  UP/DOWN move  ENTER select/edit  S = save"
EDIT_HELP = "Mode: TEXT  EXT+S save  EXT+E as  EXT+F find  EXT+H hex  EXT+T text"
EDIT_DIRTY = "Changed:  S = overwrite  E = save as  I = ignore"
VIEWTYPE_EDIT = 12
EDIT_KEYS = {"save": 128, "as": 129, "hex": 130, "text": 131, "find": 132}

# (text radku, slovo pod mysi, kolikaty vyskyt, ocekavany znak z INKEY nebo None)
HINTS = [
    ("ENTER = yes", "yes", 0, ENTER), ("ENTER = yes", "ENTER", 0, ENTER),
    ("ENTER = yes", "=", 0, ENTER), ("BREAK = no", "no", 0, BREAK),
    ("BREAK = cancel", "cancel", 0, BREAK), ("N = no", "N", 0, ord("n")),
    ("N = no", "no", 0, ord("n")), ("ENTER = continue", "continue", 0, ENTER),
    ("SPACE = yes to all directory", "directory", 0, SPACE),
    ("BREAK: close this window", "window", 0, BREAK),
    ("[ ENTER close ]", "close", 0, ENTER), ("[ SPACE next ]", "next", 0, SPACE),
    ("[ SPACE next ]", "]", 0, SPACE), ("[ ENTER close ]", "[", 0, None),
    ("ENTER stop", "stop", 0, ENTER), ("ENTER save  BREAK cancel", "save", 0, ENTER),
    ("ENTER save  BREAK cancel", "cancel", 0, BREAK),
    (TEXT_STATUS, "T=text", 0, ord("t")), (TEXT_STATUS, "hex", 0, ord("h")),
    (TEXT_STATUS, "/=find", 0, ord("/")), (TEXT_STATUS, "exit", 0, BREAK),
    (TEXT_STATUS, "TEXT", 0, None), (TEXT_STATUS, "120", 0, None), (TEXT_STATUS, "Line:", 0, None),
    (TAP_HELP, "Up", 0, UP), (TAP_HELP, "Dn", 0, DOWN), (TAP_HELP, "PgUp", 0, CUR_LEFT),
    (TAP_HELP, "PgDn", 0, CUR_RIGHT), (TAP_HELP, "export", 0, ord("e")),
    (TAP_HELP, "all", 0, ord("E")), (TAP_HELP, "header", 0, ord("d")),
    (HELP_HINT, "erase", 0, DELETE), (HELP_HINT, "UP", 0, UP), (HELP_HINT, "DOWN", 0, DOWN),
    (HELP_HINT, "scroll", 0, None), (HELP_HINT, "filter", 0, None), (HELP_HINT, "close", 0, BREAK),
    (SETTINGS_HINT, "RIGHT", 0, CUR_RIGHT), (SETTINGS_HINT, "tab", 0, None),
    (SETTINGS_HINT, "select/edit", 0, ENTER), (SETTINGS_HINT, "save", 0, ord("s")),
    (EDIT_DIRTY, "overwrite", 0, ord("s")), (EDIT_DIRTY, "as", 0, ord("e")),
    (EDIT_DIRTY, "ignore", 0, ord("i")), (EDIT_DIRTY, "Changed:", 0, None),
    ("Press any key to continue.", "continue", 0, ENTER),
    ("Not found - press any key", "Not", 0, ENTER),
    # bez klavesy
    ("I'm sorry, but you can't copy or move files to the same", "copy", 0, None),
    ("C:/games/A Long Directory", "Long", 0, None), ("D: 06.10.2026", "D:", 0, None),
    ("T: 12:30:00", "12:30:00", 0, None), ("Size: 1234", "1234", 0, None),
    ("x=5", "x=5", 0, None), ("readme.txt   1234  06.10.26", "readme.txt", 0, None),
]


def for_each_build(cls):
    for variant in ("basic", "dot"):
        name = f"{cls.__name__}_{variant}"
        globals()[name] = type(name, (cls, unittest.TestCase), {"variant": variant})
    return cls


class MouseBase:
    variant = None

    def setUp(self):
        m, s = self.m, self.s = cc_machine(self.variant)
        m.put(s["cfgUseKMouse"], 1)
        m.write(s["OLDCO"], bytes([m.mouse_x, m.mouse_y]))   # citace = OLDCO: mys stoji
        m.put(s["wheelOld"], m.mouse_wheel)
        m.write(TILEMAP, b" \x10" * 80 * 32)
        m.trap(0x01CC, self.no_time)                         # gettime: hodiny nejsou

    def no_time(self):
        self.m.carry(False)
        self.m.ret()

    def print_at(self, col, row, text):
        self.m.write(TILEMAP + row * 160 + col * 2, b"".join(bytes([ord(c), 16]) for c in text))

    def point(self, col, row):
        self.m.write(self.s["COORD"], bytes([col * 2, row * 8 + 3]))

    def extra(self, routine, **regs):
        m = self.m
        old = m.mmu[7]
        m.map(7, EXTRA_PAGE)
        m.call(self.s[routine], sp=STACK, **regs)
        m.map(7, old)
        return m.cpu.a, m.cpu.de

    def char(self, de):
        """Znak, ktery by z kodu klavesy udelalo INKEY."""
        if de == NO_KEY:
            return None
        d, e = de >> 8, de & 0xFF
        table = {0x18: "SYMTAB", 0x27: "CAPSTAB"}.get(d, "NORMTAB")
        return self.m.get(self.s[table] + e)

    def click(self, buttons=LEFT, hold=20):
        self.m.mouse_buttons, self.m.mouse_hold = buttons, hold


# ---------------------------------------------------------------------------
# napovedy klaves
# ---------------------------------------------------------------------------

@for_each_build
class Hints(MouseBase):
    def hint(self, line, word, nth=0, offset=0, row=12):
        self.print_at(0, row, line)
        col = -1
        for _ in range(nth + 1):
            col = line.index(word, col + 1)
        self.point(col + offset, row)
        _, de = self.extra("extra_mouse_hint")
        return de

    def test_real_texts(self):
        for line, word, nth, expected in HINTS:
            with self.subTest(text=line, click=word):
                self.m.write(TILEMAP, b" \x10" * 80 * 32)
                self.assertEqual(self.char(self.hint(line, word, nth)), expected)

    def test_every_letter_of_a_hint(self):
        for offset in range(len("ENTER = continue")):
            with self.subTest(offset=offset):
                expected = None if "ENTER = continue"[offset] == " " else ENTER
                self.assertEqual(self.char(self.hint("ENTER = continue", "ENTER", 0, offset)), expected)

    def test_bottom_bar(self):
        line = [" "] * 80
        for col, text in BOTTOM_BAR:
            line[col:col + len(text)] = text
        line = "".join(line)
        for col, text in BOTTOM_BAR:
            for word in text.split():
                with self.subTest(click=word):
                    self.assertEqual(self.char(self.hint(line, word, row=31)), ord(text[0]))

    def test_ext_is_symbol_shifted(self):
        self.assertEqual(self.hint("EXT+S save  EXT+E as", "save"), 0x1800 | 30, "SYMBOL+S")
        self.assertEqual(self.hint("EXT+S save  EXT+E as", "as"), 0x1800 | 21, "SYMBOL+E")

    def test_caps_enter_is_caps_shifted(self):
        de = self.hint("CAPS+ENTER = all", "all")
        self.assertEqual(de >> 8, 0x27, "CAPS SHIFT")
        self.assertEqual(self.char(de), ENTER)

    def test_frame_and_space_are_not_hints(self):
        self.m.write(TILEMAP + 12 * 160, bytes([18, 16]) + b"E\x10N\x10T\x10E\x10R\x10")
        self.assertEqual(self.extra_at(0, 12), NO_KEY, "ramecek")
        self.assertEqual(self.extra_at(2, 12), NO_KEY, "samotne ENTER za rameckem")
        self.assertEqual(self.hint("ENTER = yes", " ", 0), NO_KEY, "mezera neni slovo")
        self.assertEqual(self.extra_at(79, 31), NO_KEY)

    def extra_at(self, col, row):
        self.point(col, row)
        return self.extra("extra_mouse_hint")[1]

    def test_slash_variants_need_direct_click(self):
        self.assertEqual(self.hint(TAP_HELP, "/", 0), NO_KEY, "lomitko v Up/Dn")


# ---------------------------------------------------------------------------
# pravidla podle kontextu
# ---------------------------------------------------------------------------

@for_each_build
class Contexts(MouseBase):
    def event(self, ctx, buttons):
        return self.extra("extra_mouse_event", a=ctx, b=buttons)

    def test_right_button_is_break_outside_panels(self):
        for ctx in (CTX_DIALOG, CTX_PLUGIN):
            with self.subTest(ctx=ctx):
                self.m.put(self.s["TLACITKO"], RIGHT)
                a, de = self.event(ctx, RIGHT)
                self.assertEqual((a, self.char(de)), (BREAK, BREAK))
                self.assertEqual(self.m.get(self.s["TLACITKO"]), 0, "klik spotrebovan")

    def test_main_loop_keeps_panel_handling(self):
        s = self.s
        self.print_at(2, 10, "ENTER = yes.txt")              # i takove jmeno v panelu
        for buttons, (col, row) in ((LEFT, (4, 10)), (RIGHT, (4, 10)), (LEFT, (2, 0))):
            with self.subTest(buttons=buttons, row=row):
                self.m.put(s["TLACITKO"], buttons)
                self.point(col, row)
                self.assertEqual(self.event(CTX_MAIN, buttons), (0, NO_KEY))
                self.assertEqual(self.m.get(s["TLACITKO"]), buttons, "LEVE_TLACITKO ho jeste potrebuje")
        self.m.mouse_wheel = 3
        self.assertEqual(self.event(CTX_MAIN, 0), (0, NO_KEY))
        self.assertEqual(self.m.get(s["wheelOld"]), 15, "kolecko v panelech cte loop0")

    def test_main_loop_bottom_bar(self):
        self.print_at(35, 31, "5: COPY")
        self.point(39, 31)
        self.assertEqual(self.event(CTX_MAIN, LEFT)[0], ord("5"))

    def test_menu_click_left_to_menu(self):
        self.m.put(self.s["zobrazeneMenu"], 1)
        self.print_at(10, 5, "ENTER = yes")
        self.point(12, 5)
        self.assertEqual(self.event(CTX_DIALOG, LEFT), (0, NO_KEY))
        self.assertEqual(self.event(CTX_DIALOG, RIGHT)[0], BREAK, "prave tlacitko zavre menu")

    def test_wheel(self):
        cases = [(15, 0, DOWN), (0, 15, UP), (4, 5, DOWN), (5, 4, UP), (3, 6, DOWN),
                 (7, 7, 0), (0, 8, 0)]
        for old, new, expected in cases:
            with self.subTest(old=old, new=new):
                self.m.put(self.s["wheelOld"], old)
                self.m.mouse_wheel = new
                self.assertEqual(self.event(CTX_DIALOG, 0)[0], expected)
                self.assertEqual(self.m.get(self.s["wheelOld"]), new)

    def test_plugin_hints_only_outside_content(self):
        s = self.s
        self.m.put(s["viewPluginType"], VIEWTYPE_TEXT)
        self.print_at(1, 10, "h=hex inside the file")
        self.print_at(1, 31, TEXT_STATUS)
        self.point(2, 10)
        self.assertEqual(self.extra("EXTRA_MOUSE_PLUGIN", a=LEFT)[0], 0, "obsah souboru")
        self.point(1 + TEXT_STATUS.index("H=hex"), 31)
        self.assertEqual(self.extra("EXTRA_MOUSE_PLUGIN", a=LEFT)[0], ord("h"))

    def test_editor_ext_keys(self):
        s = self.s
        self.m.put(s["viewPluginType"], VIEWTYPE_EDIT)
        self.print_at(1, 31, EDIT_HELP)
        for word, code in EDIT_KEYS.items():
            for target in (word, "EXT+" + {"save": "S", "as": "E", "hex": "H", "text": "T",
                                          "find": "F"}[word]):
                with self.subTest(click=target):
                    self.point(1 + EDIT_HELP.index(target), 31)
                    self.assertEqual(self.extra("EXTRA_MOUSE_PLUGIN", a=LEFT)[0], code)
        self.point(2, 31)                                    # "Mode:"
        self.assertEqual(self.extra("EXTRA_MOUSE_PLUGIN", a=LEFT)[0], 0)
        self.print_at(1, 31, EDIT_DIRTY.ljust(78))
        self.point(1 + EDIT_DIRTY.index("overwrite"), 31)
        self.assertEqual(self.extra("EXTRA_MOUSE_PLUGIN", a=LEFT)[0], ord("s"))
        self.point(5, 10)
        self.assertEqual(self.extra("EXTRA_MOUSE_PLUGIN", a=LEFT)[0], 0, "text souboru")

    def test_plugins_with_own_buttons(self):
        s = self.s
        self.print_at(12, 29, "[ ENTER close ]")
        self.point(14, 29)
        for kind in (VIEWTYPE_ZXSCREEN, VIEWTYPE_PT3):
            with self.subTest(kind=kind):
                self.m.put(s["viewPluginType"], kind)
                self.assertEqual(self.extra("EXTRA_MOUSE_PLUGIN", a=LEFT)[0], 0, "vlastni obsluha")
                self.assertEqual(self.extra("EXTRA_MOUSE_PLUGIN", a=RIGHT)[0], BREAK)
        self.m.put(s["viewPluginType"], VIEWTYPE_TAP)
        self.assertEqual(self.extra("EXTRA_MOUSE_PLUGIN", a=LEFT)[0], ENTER)

    def test_waits_for_release(self):
        self.print_at(10, 15, "ENTER = yes")
        self.point(12, 15)
        self.click(LEFT, hold=50)
        self.assertEqual(self.event(CTX_DIALOG, LEFT)[0], ENTER)
        self.assertEqual(self.m.mouse_buttons, 0, "vratil se az po uvolneni")
        self.assertIn(("nextreg", 0x35, 48), self.m.events, "kurzor se kreslil i pri cekani")


# ---------------------------------------------------------------------------
# INKEY, KEYSCAN_UI a dialogy
# ---------------------------------------------------------------------------

@for_each_build
class Dialogs(MouseBase):
    def setUp(self):
        super().setUp()
        self.m.interrupts = True
        self.m.cpu.iff1 = self.m.cpu.iff2 = 1

    def inkey_from(self, caller=None):
        """INKEY vracejici se na caller (napr. loop0_inkey_ret), nebo z dialogu."""
        m, s = self.m, self.s
        if caller is None:
            m.call(s["INKEY"], sp=STACK)
        else:
            m.trap(caller, lambda: setattr(m.cpu, "pc", SENTINEL))
            m.cpu.sp = STACK
            m.push(caller)
            m.cpu.pc = s["INKEY"]
            m.run()
        return m.cpu.a

    def test_inkey_click_on_hint(self):
        self.print_at(60, 15, "ENTER = yes")
        self.point(61, 15)
        self.click()
        self.assertEqual(self.inkey_from(), ENTER)
        self.assertEqual(self.m.get(self.s["TLACITKO"]), 0)

    def test_inkey_click_elsewhere_left_to_dialog(self):
        self.point(5, 5)
        self.click(hold=3)
        self.assertEqual(self.inkey_from(), 0)
        self.assertEqual(self.m.get(self.s["TLACITKO"]) & LEFT, LEFT)

    def test_inkey_main_loop(self):
        s = self.s
        self.print_at(35, 31, "5: COPY")
        self.point(36, 31)
        self.click()
        self.assertEqual(self.inkey_from(s["loop0_inkey_ret"]), ord("5"))
        self.point(10, 10)
        self.click(hold=3)
        self.assertEqual(self.inkey_from(s["loop0_inkey_ret"]), 0, "klik do panelu pro LEVE_TLACITKO")

    def test_inkey_right_button(self):
        self.click(RIGHT)
        self.assertEqual(self.inkey_from(), BREAK)

    def test_keyscan_ui(self):
        m, s = self.m, self.s
        self.extra("EXTRA_KEYSCAN_UI")                         # tlacitka pusta
        self.click(RIGHT, hold=5)
        m.call(s["KEYSCAN_UI"], sp=STACK)
        self.assertEqual(m.cpu.de, 0x2720, "prave tlacitko = CAPS+SPACE")
        m.mouse_buttons, m.mouse_hold = RIGHT, None            # drzene: zadny novy stisk
        m.call(s["KEYSCAN_UI"], sp=STACK)
        m.call(s["KEYSCAN_UI"], sp=STACK)
        self.assertEqual(m.cpu.de & 0xFF, 0xFF, "drzene tlacitko se neopakuje")
        m.mouse_buttons = 0
        m.mouse_wheel = 0                                      # wheelOld 15 -> 0: dolu
        m.call(s["KEYSCAN_UI"], sp=STACK)
        self.assertEqual(self.char(m.cpu.de), DOWN)
        m.keys = {(0xBF, 0)}
        m.call(s["KEYSCAN_UI"], sp=STACK)
        self.assertEqual(self.char(m.cpu.de), ENTER, "klavesnice dal funguje")

    def run_potvrd(self, col, row, buttons=LEFT):
        m, s = self.m, self.s
        result = {}
        m.trap(s["loop0"], lambda: (result.setdefault("loop0", True), setattr(m.cpu, "pc", SENTINEL)))
        self.point(col, row)
        self.click(buttons)
        m.call(s["potvrd"], sp=STACK, max_steps=200_000)
        return "loop0" in result

    def test_potvrd_yes(self):
        self.assertFalse(self.run_potvrd(64, 15), "ENTER = yes: potvrd se vrati")

    def test_potvrd_no(self):
        self.assertTrue(self.run_potvrd(64, 14), "BREAK = no: zpet do loop0")

    def test_potvrd_right_button_cancels(self):
        self.assertTrue(self.run_potvrd(5, 5, RIGHT))

    def test_overwrite_choice(self):
        m, s = self.m, self.s
        for (col, row), expected in (((15, 20), ord("n")), ((14, 21), 2), ((48, 20), 1)):
            with self.subTest(click=(col, row)):
                m.call(s["overwrite_draw_options"], sp=STACK)
                self.point(col, row)
                frames = {"n": 0}

                def press_later(m):                    # stisk az po prvnim cteni
                    frames["n"] += 1
                    if frames["n"] == 6:
                        self.click(hold=10)
                m.isr_checks[:] = [press_later]
                m.call(s["overwrite_choice"], sp=STACK, max_steps=200_000)
                self.assertEqual(m.cpu.a, expected)


if __name__ == "__main__":
    unittest.main()
