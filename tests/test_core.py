"""Rutiny jadra CC (cc.asm a functions/*.asm) na skutecnem Z80 kodu.

Kazdy test bezi proti obema sestavenim - BASIC verzi (cc.bin) i dot commandu
(build/dot/ccd.bin) - protoze se kod podminene preklada (CC_DOT) a adresy se
lisi. Vysledky se porovnavaji s nezavislou implementaci v Pythonu.
"""

import random
import re
import unittest

from cctest import Machine, ROOT, build_basic, build_dot

EXTRA_PAGE = 90                   # EXTRA_BANK_PAGE, extra banka na $E000
STACK = 0x6F00                    # pod S1 ($7100) je volno
SCRATCH = 0x6000

VIEWTYPE = {"TEXT": 1, "ZXSCREEN": 2, "PT3": 3, "PT2": 4, "STC": 5, "STP": 6, "SQT": 7,
            "NXI": 8, "BAS": 10, "TAP": 11, "TRD": 13, "ZIP": 14}


def cc_machine(variant):
    """Stroj s nahranym CC jako po startu: hlavni kod od S1 (MMU3-6 = stranky
    11, 4, 5, 0) a extra banka ve strance 90."""
    if variant == "basic":
        syms, main, xb = build_basic(), "cc.bin", "cc_xb.bin"
    else:
        syms, main, xb = build_dot()[0], "build/dot/ccd.bin", "build/dot/ccd_xb.bin"
    m = Machine()
    m.write(syms["S1"], (ROOT / main).read_bytes())
    m.set_page(EXTRA_PAGE, (ROOT / xb).read_bytes())
    return m, syms


class Core:
    """Zaklad testu; for_each_build z nej udela tridu pro kazde sestaveni."""
    variant = None

    def setUp(self):
        self.m, self.s = cc_machine(self.variant)


def for_each_build(cls):
    for variant in ("basic", "dot"):
        name = f"{cls.__name__}_{variant}"
        globals()[name] = type(name, (cls, unittest.TestCase), {"variant": variant})
    return cls


# ---------------------------------------------------------------------------
# Hledani podle masky (Select files, Find)
# ---------------------------------------------------------------------------

def wildcard_reference(pattern, text):
    rx = "".join(".*" if c == "*" else "." if c == "?" else re.escape(c) for c in pattern)
    return re.fullmatch(rx, text, re.S | re.I | re.A) is not None


@for_each_build
class Wildcard(Core):
    FIXED = [("*.tap", "GAME.TAP", True), ("*.TAP", "game.tap", True),
             ("*.t?p", "x.tip", True), ("*.tap", "game.tapx", False),
             ("g*e", "GAME", True), ("g*e", "GAMES", False), ("*", "", True),
             ("", "", True), ("", "a", False), ("?", "", False), ("**a*", "bab", True),
             ("a*b*c", "aXbYbZc", True), ("a*b*c", "aXbYc_", False)]

    def match(self, m, s, pattern, text, end):
        m.write(SCRATCH, pattern.encode() + bytes([end]))
        m.write(SCRATCH + 0x100, text.encode() + bytes([end]))
        m.call(s["wildcard_match_ci"], sp=STACK, de=SCRATCH, hl=SCRATCH + 0x100)
        return bool(m.cpu.f & 0x40)

    def test_fixed_cases(self):
        m, s = self.m, self.s
        for pattern, text, expected in self.FIXED:
            for end in (0, 255):
                self.assertEqual(self.match(m, s, pattern, text, end), expected,
                                 f"maska {pattern!r} na {text!r} (konec {end})")

    def test_random_against_python(self):
        rnd = random.Random(1966)
        cases = [("".join(rnd.choice("aAb.*?") for _ in range(rnd.randint(0, 6))),
                  "".join(rnd.choice("aAbB.") for _ in range(rnd.randint(0, 8))),
                  rnd.choice((0, 255))) for _ in range(1500)]
        m, s = self.m, self.s
        wrong = [(p, t) for p, t, end in cases
                 if self.match(m, s, p, t, end) != wildcard_reference(p, t)]
        self.assertEqual(wrong, [], "maska se lisi od Pythonu")


# ---------------------------------------------------------------------------
# Vyber pluginu podle pripony a velikosti (view_select_plugin)
# ---------------------------------------------------------------------------

@for_each_build
class SelectPlugin(Core):
    CASES = [  # 8.3 jmeno, velikost, typ, plugin
        ("SONG    PT3", 3000, "PT3", "pt3test.ccp"),
        ("song    pt3", 3000, "PT3", "pt3test.ccp"),
        ("SONG    PT2", 3000, "PT2", "pt2test.ccp"),
        ("SONG    STC", 3000, "STC", "stctest.ccp"),
        ("song    stp", 3000, "STP", "stptest.ccp"),
        ("SONG    SQT", 3000, "SQT", "sqtest.ccp"),
        ("README  TXT", 100, "TEXT", "text.ccp"),
        ("main    asm", 100, "TEXT", "text.ccp"),
        ("CONFIG  CFG", 100, "TEXT", "text.ccp"),
        ("set     ini", 100, "TEXT", "text.ccp"),
        ("SNAKE   BAS", 2000, "BAS", "bas.ccp"),
        ("GAME    TAP", 40000, "TAP", "tap.ccp"),
        ("DISK    TRD", 655360, "TRD", "trd.ccp"),
        ("DISK    scl", 20000, "TRD", "trd.ccp"),
        ("ARCHIVE ZIP", 1000, "ZIP", "zip.ccp"),
        ("SCREEN  SCR", 6912, "ZXSCREEN", "zxscreen.ccp"),
        ("ANYTHINGBIN", 6912, "ZXSCREEN", "zxscreen.ccp"),
        ("PICTURE NXI", 6912, "NXI", "nxi.ccp"),          # NXI ma prednost pred SCR
        ("picture nxi", 49664, "NXI", "nxi.ccp"),
        ("BIG     TXT", 0x11A00, "TEXT", "text.ccp"),     # 6912 jen v dolnim slove
    ]
    NO_VIEWER = [("README     ", 100), ("PROGRAM BIN", 1000), ("ANYTHINGBIN", 0x11B00),
                 ("SCREEN  SCR", 6913), ("GAME    NEX", 1000)]

    def select(self, m, s, name83, size):
        m.write(s["TMP83"], name83.encode())
        m.put(s["viewFileSizeLo"], size & 0xFFFF, 2)
        m.put(s["viewFileSizeHi"], size >> 16, 2)
        m.call(s["view_select_plugin"], sp=STACK)
        if m.cpu.f & 1:
            return None
        plugin = m.cstring(m.word(s["viewPluginName"]), end=(255,)).decode()
        return m.get(s["viewPluginType"]), plugin

    def test_extensions(self):
        m, s = self.m, self.s
        for name83, size, kind, plugin in self.CASES:
            got = self.select(m, s, name83, size)
            self.assertIsNotNone(got, name83)
            self.assertEqual(got[0], VIEWTYPE[kind], name83)
            self.assertEqual(got[1], plugin, name83)

    def test_unknown_files_have_no_viewer(self):
        m, s = self.m, self.s
        for name83, size in self.NO_VIEWER:
            self.assertIsNone(self.select(m, s, name83, size), name83)

    def test_plugin_directory(self):
        m, s = self.m, self.s
        expected = {"basic": b"c:/CalmCommander/plugin", "dot": b"c:/sys/cc"}[self.variant]
        self.assertEqual(m.cstring(s["viewPluginDir"], end=(255,)), expected)
        self.assertEqual(m.cstring(s["sysCopyPluginDir"], end=(255,)), expected)


# ---------------------------------------------------------------------------
# Razeni panelu (sort_loaded_dir)
# ---------------------------------------------------------------------------

CATALOG_PAGE, LFN_PAGE = 74, 24   # levy panel: buffl a lfnpage
MAXLEN = 261 + 4 + 4              # LFN, delka, datum, cas
LFN_PER_PAGE = 30

ENTRIES = [  # jmeno, adresar, datum, cas (MS-DOS)
    ("Games", True, 0x5A21, 0x6000), ("docs", True, 0x5A21, 0x6001),
    ("readme.txt", False, 0x5B01, 0x1000), ("B.bas", False, 0x4A10, 0x0000),
    ("a.TAP", False, 0x5B01, 0x1000), ("zeta", False, 0x5B01, 0x1001),
    ("Alpha.z80", False, 0x3000, 0x0000), ("music.pt3", False, 0x5C00, 0x0000),
    ("Zeta.tap", False, 0x2000, 0x0001), ("_tools", True, 0x5B01, 0x1000),
    ("long file name with spaces.txt", False, 0x5000, 0x0000),
    ("archive.v2.zip", False, 0x5001, 0x0000), ("noext.", False, 0x5002, 0x0000),
] + [(f"file{i:02d}.{'bin' if i % 3 else 'dat'}", i % 7 == 0, 0x5000 + i % 5, i)
     for i in range(24)]               # pres 30 polozek = vic LFN stranek


def short_name(name, is_dir):
    stem, _, ext = name.upper().partition(".")
    raw = bytearray((stem[:8].ljust(8) + ext[:3].ljust(3)).encode())
    if is_dir:
        raw[7] |= 0x80                    # adresar = bit 7 osmeho znaku
    return bytes(raw)


def sort_key(name):
    return bytes(ord(c) - 32 if "a" <= c <= "z" else ord(c) for c in name)


def extension(name):
    return name.rpartition(".")[2] if "." in name else ""


def reference_order(entries, mode, dirs_first):
    def key(e):
        name, is_dir, date, time = e
        group = 0 if (is_dir or not dirs_first) else 1
        if mode == 0:
            return group, sort_key(name)
        if mode == 1:
            return group, sort_key(extension(name)), sort_key(name)
        return group, -date, -time, sort_key(name)
    return sorted(entries, key=key)


@for_each_build
class SortPanel(Core):
    def load_panel(self, m, s, entries):
        items = [(".", True, 0, 0), ("..", True, 0, 0)] + entries
        catalog = bytearray(13)
        for i, (name, is_dir, date, time) in enumerate(items):
            raw = (b".          " if name == "." else b"..         " if name == ".."
                   else short_name(name, is_dir))
            catalog += raw + i.to_bytes(2, "little")      # velikost = znacka polozky
        m.set_page(CATALOG_PAGE, bytes(catalog))
        for i, (name, is_dir, date, time) in enumerate(items):
            block = bytearray(MAXLEN)
            block[:len(name) + 1] = name.encode() + b"\xff"
            block[261:265] = i.to_bytes(4, "little")
            block[265:269] = date.to_bytes(2, "little") + time.to_bytes(2, "little")
            page, slot = divmod(i, LFN_PER_PAGE)
            m.set_page(LFN_PAGE + page, bytes(block), slot * MAXLEN)
        m.put(s["OKNO"], 0)
        m.put(s["ALLFILES"], len(items), 2)
        return items

    def read_panel(self, m, count):
        catalog = m.page(CATALOG_PAGE)
        out = []
        for i in range(count):
            page, slot = divmod(i, LFN_PER_PAGE)
            block = m.page(LFN_PAGE + page)[slot * MAXLEN:(slot + 1) * MAXLEN]
            name = bytes(block[:block.index(0xFF)]).decode()
            entry = catalog[13 + i * 13:26 + i * 13]
            out.append((name, entry[11] | entry[12] << 8, int.from_bytes(block[261:265], "little")))
        return out

    def sort(self, mode, dirs_first):
        m, s = self.m, self.s
        items = self.load_panel(m, s, ENTRIES)
        m.put(s["cfgSortMode"], mode)
        m.put(s["cfgDirsFirst"], dirs_first)
        m.call(s["sort_loaded_dir_trampoline"], sp=STACK)
        panel = self.read_panel(m, len(items))
        expected = [".", ".."] + [e[0] for e in reference_order(ENTRIES, mode, dirs_first)]
        self.assertEqual([name for name, _, _ in panel], expected)
        for name, cat_mark, lfn_mark in panel:
            self.assertEqual(cat_mark, lfn_mark, f"{name}: katalog a LFN od sebe")
            self.assertEqual(items[cat_mark][0], name)
        self.assertEqual(m.mmu[5:8], [5, 0, 1], "MMU5-7 vraceny (MMU6 = 0: kod casti 2)")

    def test_by_name(self):
        self.sort(0, 0)

    def test_by_name_dirs_first(self):
        self.sort(0, 1)

    def test_by_extension(self):
        self.sort(1, 0)

    def test_by_extension_dirs_first(self):
        self.sort(1, 1)

    def test_by_date_newest_first(self):
        self.sort(2, 0)

    def test_by_date_dirs_first(self):
        self.sort(2, 1)

    def test_part2_code_survives(self):
        """Regrese KS3: razeni nesmi nechat v MMU6 nic jineho nez kod casti 2."""
        m, s = self.m, self.s
        self.load_panel(m, s, ENTRIES)
        before = bytes(m.page(0))
        m.call(s["sort_loaded_dir_trampoline"], sp=STACK)
        self.assertEqual(m.mmu[6], 0)
        self.assertEqual(bytes(m.mem[0xC000:0xE000])[:0x1F00], before[:0x1F00])


# ---------------------------------------------------------------------------
# Cisla, datum a cas
# ---------------------------------------------------------------------------

def tilemap_text(m, x, y, length):
    addr = 0x4000 + y * 160 + x * 2
    return bytes(m.mem[addr:addr + length * 2:2]).decode("latin-1")


@for_each_build
class Numbers(Core):
    def test_num_five_digits(self):
        values = [0, 1, 9, 10, 99, 100, 12345, 65535] + random.Random(5).sample(range(65536), 40)
        m, s = self.m, self.s
        for value in values:
            m.call(s["NUM"], sp=STACK, hl=value)
            self.assertEqual(bytes(m.mem[s["NUMBUF"]:s["NUMBUF"] + 5]).decode(), f"{value:05d}")

    def test_d32b(self):
        rnd = random.Random(32)
        values = [0, 7, 10, 99999, 100000, 100005, 4294967295] + [rnd.getrandbits(32) for _ in range(30)]
        m, s = self.m, self.s
        for value in values:
            for digits, fill in ((10, 32), (8, 32), (5, 32), (10, 0)):
                m.write(s["NUMB"], b"\xAA" * 11)
                m.call(s["D32B"], sp=STACK, iy=0x5C3A, hl=value & 0xFFFF, de=value >> 16,
                       b=digits, c=fill)
                text = str(value % 10 ** digits)
                want = text.rjust(digits) if fill else text
                got = bytes(m.mem[s["NUMB"]:s["NUMB"] + len(want)]).decode("latin-1")
                self.assertEqual(got, want, f"{value} na {digits} mist, vypln {fill}")

    def test_showdate(self):
        dates = [(1, 1, 1980), (31, 12, 2107), (6, 10, 2026), (29, 2, 2000), (9, 9, 1999)]
        m, s = self.m, self.s
        for day, month, year in dates:
            value = (year - 1980) << 9 | month << 5 | day
            m.call(s["showdate"], sp=STACK, de=value, hl=17 * 256 + 15)
            self.assertEqual(tilemap_text(m, 17, 15, 10), f"{day:02d}.{month:02d}.{year}")

    def test_showtime(self):
        times = [(0, 0, 0), (23, 59, 58), (12, 34, 56), (9, 5, 2)]
        m, s = self.m, self.s
        for h, mi, sec in times:
            value = h << 11 | mi << 5 | sec // 2
            m.call(s["showtime"], sp=STACK, de=value, hl=40 * 256 + 3)
            self.assertEqual(tilemap_text(m, 40, 3, 8), f"{h:02d}:{mi:02d}:{sec:02d}")


if __name__ == "__main__":
    unittest.main()
