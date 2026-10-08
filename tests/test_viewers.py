"""Prohlizece textu, BASICu a obrazku na skutecnem Z80 kodu pluginu.

Text a BASIC se kontroluji proti obsahu tilemapy (radky 3-29, sloupce 1-78),
obrazky proti obsahu stranek Layer 2 (banka 32 = stranky 64-69), ktery se
spocita v Pythonu primo ze souboru.
"""

import unittest

from cctest import ROOT
from pluginhost import PluginHost, assert_golden

VIEWTYPE_TEXT, VIEWTYPE_ZXSCREEN, VIEWTYPE_NXI, VIEWTYPE_BAS = 1, 2, 8, 10
DOWN, UP, PAGE_DOWN, PAGE_UP, BREAK = 10, 11, 9, 8, 1
ROWS, COLS = 27, 78
L2_PAGES = range(64, 70)


def text_area(rows):
    """Textova plocha prohlizece: radky 3-29, sloupce 1-78, bez mezer na konci."""
    return [row[1:1 + COLS].rstrip() for row in rows[3:3 + ROWS]]


def status(rows):
    """Stavovy radek s jednou mezerou mezi slovy (cisla jsou zarovnana)."""
    return " ".join(rows[31].split())


# ---------------------------------------------------------------------------
# Text
# ---------------------------------------------------------------------------

def text_reference(data, first_line=0):
    """Jak ma vypadat stranka: CR se ignoruje, LF konci radek, TAB je mezera,
    ostatni ridici znaky tecka, dlouhy radek se zalomi po 78 znacich."""
    out = []
    for line in data.replace(b"\r", b"").split(b"\n")[first_line:]:
        row = ""
        for b in line:
            if len(row) == COLS:
                out.append(row)
                row = ""
            row += " " if b == 9 else "." if b < 32 else chr(b) if b < 127 else "~"
        out.append(row)
        if len(out) >= ROWS:
            break
    out = [r.rstrip() for r in out[:ROWS]]
    return out + [""] * (ROWS - len(out))


def sample_text(count=120, newline=b"\n"):
    fox = "The quick brown fox jumps over the lazy dog. "
    lines = [f"{i:03d} {fox[:(i * 7) % 46]}" for i in range(count)]
    return newline.join(line.encode() for line in lines) + newline


class TextPlugin(unittest.TestCase):
    def view(self, data, keys=(), name="test.txt"):
        host = PluginHost(self, "text", name, data, VIEWTYPE_TEXT, keys=keys,
                          menu_sites=["plugin_start.input", "find_prompt.input",
                                      "find_execute.wait"])
        self.assertEqual(host.run(), BREAK)
        return host

    def test_first_page(self):
        data = (b"plain line\n\tindented by tab\ncontrol \x01\x02 chars\n"
                + b"W" * 100 + b" wrapped\n" + b"x" * 78 + b"\nafter exact 78\n")
        host = self.view(data)
        self.assertEqual(text_area(host.snapshots[0]), text_reference(data))
        self.assertEqual(host.rows()[1][2:7], "VIEW:")
        self.assertIn("test.txt", host.rows()[1])

    def test_crlf_same_as_lf(self):
        lf = self.view(sample_text(40)).snapshots[0]
        crlf = self.view(sample_text(40, b"\r\n")).snapshots[0]
        self.assertEqual(text_area(lf), text_area(crlf))
        self.assertEqual(text_area(lf), text_reference(sample_text(40)))

    def test_status_counts_lines(self):
        host = self.view(sample_text(120))
        self.assertTrue(status(host.snapshots[0]).startswith("Line: 1 / 120"),
                        status(host.snapshots[0]))

    def test_scrolling(self):
        data = sample_text(120)
        keys = [DOWN] * 30 + [UP] * 5 + [PAGE_DOWN, PAGE_DOWN, PAGE_UP] + [DOWN] * 3 + [UP] * 2
        host = self.view(data, keys)
        line, total = 0, 120
        for step, key in enumerate([None] + keys):
            if key == DOWN:
                line += 1
            elif key == UP:
                line = max(0, line - 1)
            elif key == PAGE_DOWN:
                line = min(total - 1, line + ROWS)
            elif key == PAGE_UP:
                line = max(0, line - ROWS)
            screen = host.snapshots[step]
            self.assertEqual(text_area(screen), text_reference(data, line),
                             f"po klavese {step} ({key}) ma byt nahore radek {line}")
            self.assertTrue(status(screen).startswith(f"Line: {line + 1} / {total}"),
                            status(screen))

    def test_hex_and_dec_modes(self):
        data = bytes(range(256)) * 2
        host = self.view(data, [ord("h"), DOWN, ord("d"), ord("t")])
        hex_page, hex_down, dec_page, text_page = host.snapshots[1:5]
        self.assertEqual(text_area(hex_down)[:ROWS - 1], text_area(hex_page)[1:])
        # DOWN v hex rezimu = offset 16, to je v textu radek 1 (LF je na offsetu 10)
        self.assertEqual(text_area(text_page), text_reference(data, 1))
        assert_golden(self, "text_hex_dec", {"hex": text_area(hex_page) + [status(hex_page)],
                                             "dec": text_area(dec_page) + [status(dec_page)]})

    def test_find_moves_to_line_with_match(self):
        data = sample_text(120).replace(b"077 ", b"077 NeedLe ")
        keys = [ord("/")] + [ord(c) for c in "needle"] + [13]
        host = self.view(data, keys)
        self.assertEqual(text_area(host.snapshots[-1]), text_reference(data, 77))

    def test_find_wraps_to_start(self):
        data = sample_text(120).replace(b"005 ", b"005 marker ")
        keys = [PAGE_DOWN, PAGE_DOWN, ord("/")] + [ord(c) for c in "MARKER"] + [13]
        host = self.view(data, keys)
        self.assertEqual(text_area(host.snapshots[-1]), text_reference(data, 5))

    def test_find_not_found(self):
        keys = [ord("/")] + [ord(c) for c in "zzz"] + [13, ord("x")]
        host = self.view(sample_text(50), keys)
        self.assertIn("Not found", host.snapshots[-2][31])
        self.assertEqual(text_area(host.snapshots[-1]), text_reference(sample_text(50)))


# ---------------------------------------------------------------------------
# BASIC
# ---------------------------------------------------------------------------

PRINT, REM, LET, FOR, TO, NEXT, BORDER = 0xF5, 0xEA, 0xF1, 0xEB, 0xCC, 0xF3, 0xE7


def number(text):
    """Cislo v radku BASICu: cislice a skryty 5bajtovy tvar."""
    value = int(text)
    return text.encode() + bytes([0x0E, 0, 0, value & 0xFF, value >> 8, 0])


def basic_line(num, body):
    return num.to_bytes(2, "big") + (len(body) + 1).to_bytes(2, "little") + body + b"\r"


def sample_program(count=90):
    out = b""
    for i in range(count):
        n = 10 * (i + 1)
        kind = i % 5
        if kind == 0:
            body = bytes([PRINT]) + f'"Line {n}"'.encode()
        elif kind == 1:
            body = bytes([LET]) + b"a=" + number(str(i * 37))
        elif kind == 2:
            body = bytes([REM]) + b" " + b"long comment " * (1 + i % 9)
        elif kind == 3:
            body = (bytes([FOR]) + b"i=" + number("1") + bytes([TO]) + number(str(i))
                    + b":" + bytes([NEXT]) + b"i")
        else:
            body = bytes([BORDER]) + number(str(i % 8))
        out += basic_line(n, body)
    return out


def plus3dos(data, kind=0, p1=0x8000, p2=None):
    """+3DOS hlavicka (128 B) pred daty souboru."""
    header = bytearray(128)
    header[:8] = b"PLUS3DOS"
    header[8], header[9], header[10] = 0x1A, 1, 0
    header[11:15] = (len(data) + 128).to_bytes(4, "little")
    header[15] = kind
    header[16:18] = len(data).to_bytes(2, "little")
    header[18:20] = p1.to_bytes(2, "little")
    header[20:22] = (len(data) if p2 is None else p2).to_bytes(2, "little")
    header[127] = sum(header[:127]) & 0xFF
    return bytes(header) + data


class BasPlugin(unittest.TestCase):
    def view(self, data, keys=(), name="test.bas"):
        host = PluginHost(self, "bas", name, data, VIEWTYPE_BAS, keys=keys,
                          menu_sites=["plugin_start.input"])
        self.assertEqual(host.run(), BREAK)
        return host

    def test_cc_bas_listing(self):
        host = self.view((ROOT / "cc.bas").read_bytes(), name="cc.bas")
        assert_golden(self, "bas_cc_listing", text_area(host.snapshots[0]))

    def test_generated_listing(self):
        data = sample_program()
        host = self.view(data)
        page = text_area(host.snapshots[0])
        assert_golden(self, "bas_generated_page", page)
        numbers = [int(row.split()[0]) for row in page if row[:1].strip().isdigit()]
        self.assertEqual(numbers, list(range(10, 10 * (len(numbers) + 1), 10)),
                         "cisla radku po sobe, kazde jednou")

    def test_header_is_skipped(self):
        data = sample_program(30)
        raw = text_area(self.view(data).snapshots[0])
        self.assertEqual(text_area(self.view(plus3dos(data)).snapshots[0]), raw)

    def test_scrolling_matches_full_render(self):
        """Posun o radek kresli jen novy radek (s cache), stranka vse znovu.
        Obe cesty musi dat stejnou obrazovku."""
        data = sample_program()
        downs = 40
        keys = [DOWN] * downs + [PAGE_UP] + [PAGE_DOWN] + [UP] * 5 + [DOWN] * 8 + [PAGE_DOWN]
        host = self.view(data, keys)
        shots = [text_area(s) for s in host.snapshots]
        visual = shots[0] + [shots[i][-1] for i in range(1, downs + 1)]
        for i in range(downs + 1):
            self.assertEqual(shots[i], visual[i:i + ROWS], f"po {i} posunech dolu")
        pos = downs
        for step, key in enumerate(keys[downs:], start=downs + 1):
            pos = {PAGE_UP: max(0, pos - ROWS), PAGE_DOWN: pos + ROWS,
                   UP: pos - 1, DOWN: pos + 1}[key]
            if pos + ROWS <= len(visual):
                self.assertEqual(shots[step], visual[pos:pos + ROWS], f"klavesa {step}, radek {pos}")
            else:
                visual += shots[step][len(visual) - pos:]


# ---------------------------------------------------------------------------
# Obrazky: SCR a NXI do Layer 2
# ---------------------------------------------------------------------------

def scr_to_l2(scr):
    """256x192 bajtu, kazdy pixel = index barvy ZX (ink/paper + 8 pro BRIGHT)."""
    out = bytearray()
    for y in range(192):
        row = (y & 0xC0) << 5 | (y & 7) << 8 | (y & 0x38) << 2
        for col in range(32):
            attr = scr[6144 + (y >> 3) * 32 + col]
            bright = 8 if attr & 0x40 else 0
            ink, paper = (attr & 7) + bright, ((attr >> 3) & 7) + bright
            bits = scr[row + col]
            out += bytes(ink if bits & (0x80 >> b) else paper for b in range(8))
    return bytes(out)


def layer2(host):
    return b"".join(bytes(host.m.page(p)) for p in L2_PAGES)


def palette_writes(m):
    """Hodnoty zapsane do NextReg $44 (9bit paleta, po dvou bajtech)."""
    return [value for kind, reg, value in m.events if kind == "nextreg" and reg == 0x44]


class ImageViewer:
    plugin = view_type = None
    ENTER, SPACE = 13, 32

    def view(self, data, keys, name):
        host = PluginHost(self, self.plugin, name, data, self.view_type, keys=keys,
                          menu_sites=["wait_command.press"])
        m = host.m
        m.nextregs.update({0x12: 8, 0x43: 0x05, 0x69: 0x01})
        result = host.run()
        return host, result

    def check_exit(self, host, result, expected):
        m = host.m
        self.assertEqual(result, expected, "ENTER = konec (0), SPACE = dalsi (1)")
        self.assertEqual(m.mmu[2:8], [10, 11, 4, 5, 82, 81], "MMU vraceny")
        self.assertEqual(m.read_nextreg(0x69), 0x01, "Layer 2 zase skryta")
        self.assertEqual(m.read_nextreg(0x12), 8, "stranka Layer 2 vracena")
        self.assertEqual(m.read_nextreg(0x43), 0x05, "rizeni palety vraceno")
        self.assertIn(("nextreg", 0x69, 0x81), m.events, "obrazek byl zobrazen")
        self.assertTrue(m.cpu.iff1, "preruseni zase povolena")


class ZxScreenPlugin(ImageViewer, unittest.TestCase):
    plugin, view_type = "zxscreen", VIEWTYPE_ZXSCREEN
    PALETTE = [0x00, 0x02, 0xA0, 0xA2, 0x14, 0x16, 0xB4, 0xB6,
               0x00, 0x03, 0xE0, 0xE3, 0x1C, 0x1F, 0xFC, 0xFF]

    def screens(self):
        return sorted((ROOT / "images").glob("*.scr"))[:3]

    def test_conversion_matches_python(self):
        for path in self.screens():
            with self.subTest(path.name):
                scr = path.read_bytes()
                host, result = self.view(scr, [self.ENTER], path.name)
                self.assertEqual(layer2(host), scr_to_l2(scr))
                self.check_exit(host, result, 0)

    def test_plus3dos_header(self):
        scr = self.screens()[0].read_bytes()
        host, result = self.view(plus3dos(scr, kind=3, p1=16384), [self.SPACE], "head.scr")
        self.assertEqual(layer2(host), scr_to_l2(scr))
        self.check_exit(host, result, 1)

    def test_palette(self):
        host, _ = self.view(self.screens()[0].read_bytes(), [self.ENTER], "a.scr")
        expected = [v for colour in self.PALETTE for v in (colour, 0)]
        self.assertEqual(palette_writes(host.m), expected)


class NxiPlugin(ImageViewer, unittest.TestCase):
    plugin, view_type = "nxi", VIEWTYPE_NXI

    def test_with_palette(self):
        data = (ROOT / "screen_edited.nxi").read_bytes()
        self.assertEqual(len(data), 512 + 49152)
        host, result = self.view(data, [self.ENTER], "screen_edited.nxi")
        self.assertEqual(layer2(host), data[512:])
        self.assertEqual(palette_writes(host.m),
                         [v for i in range(256) for v in (data[2 * i], data[2 * i + 1] & 1)])
        self.check_exit(host, result, 0)

    def test_without_palette(self):
        image = bytes((x * 7 + y) & 0xFF for y in range(192) for x in range(256))
        host, result = self.view(image, [self.SPACE], "raw.nxi")
        self.assertEqual(layer2(host), image)
        self.assertEqual(palette_writes(host.m), [v for i in range(256) for v in (i, 0)])
        self.check_exit(host, result, 1)

    def test_wrong_size_is_refused(self):
        host = PluginHost(self, "nxi", "bad.nxi", bytes(1000), VIEWTYPE_NXI)
        host.run()
        self.assertTrue(host.m.cpu.f & 1, "carry = neumim zobrazit")
        self.assertEqual(palette_writes(host.m), [])
        self.assertEqual(host.m.mmu[2:8], [10, 11, 4, 5, 82, 81])


if __name__ == "__main__":
    unittest.main()
