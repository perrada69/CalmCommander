"""Pluginy prohlizece na skutecnem Z80 kodu, s falesnym hostitelem.

Obsah rozbalenych/exportovanych souboru se overuje nezavisle v Pythonu
(zipfile, vlastni rozbor TAP/TRD/SCL). Jmena souboru, hlavicky +3DOS a text
na obrazovce hlida "golden" snimek v tests/golden.
"""

import unittest
import zipfile

from cctest import ROOT
from pluginhost import PluginHost, assert_golden

VIEWTYPE_TAP, VIEWTYPE_TRD, VIEWTYPE_ZIP = 11, 13, 14


def summary(host):
    return [{"name": name, "size": len(data), "p3dos": header}
            for name, data, header in host.files]


class ZipPlugin(unittest.TestCase):
    def host(self, keys):
        data = (ROOT / "test.zip").read_bytes()
        return PluginHost(self, "zip", "test.zip", data, VIEWTYPE_ZIP, keys=keys,
                          menu_sites=["plugin_start.input"])

    def test_extract_all_matches_zipfile(self):
        host = self.host([ord("E")])
        host.run()
        archive = zipfile.ZipFile(ROOT / "test.zip")
        expected = [archive.read(i) for i in archive.infolist() if not i.is_dir()]
        written = [data for _, data, _ in host.files]
        self.assertEqual(len(written), len(expected), host.text())
        for i, (got, want) in enumerate(zip(written, expected)):
            self.assertEqual(got, want, f"polozka {i} ({host.files[i][0]})")
        assert_golden(self, "zip_extract_all", {"files": summary(host), "screen": host.text()})

    def test_listing(self):
        host = self.host([])
        self.assertEqual(host.run(), 1, "BREAK konci prohlizec s A=1")
        assert_golden(self, "zip_listing", host.text())

    def test_extract_single_after_moving_down(self):
        host = self.host([10, 10, ord("e")])           # dolu, dolu, rozbal
        host.run()
        archive = zipfile.ZipFile(ROOT / "test.zip")
        self.assertEqual(len(host.files), 1)
        self.assertIn(host.files[0][1], [archive.read(n) for n in archive.namelist()])
        assert_golden(self, "zip_extract_third", summary(host))


def trd_entries(image):
    """Katalog TR-DOS: 16 bajtu na polozku, data na (stopa*16+sektor)*256."""
    out = []
    for i in range(128):
        e = image[i * 16:(i + 1) * 16]
        if e[0] == 0:
            break
        length = e[11] | e[12] << 8
        offset = (e[15] * 16 + e[14]) * 256
        out.append({"deleted": e[0] == 1, "type": chr(e[8]), "param": e[9] | e[10] << 8,
                    "data": image[offset:offset + length]})
    return out


def scl_entries(image):
    """SCL: "SINCLAIR", pocet, 14bajtove polozky, data souboru za sebou."""
    assert image[:8] == b"SINCLAIR"
    count = image[8]
    pos = 9 + count * 14
    out = []
    for i in range(count):
        e = image[9 + i * 14:9 + (i + 1) * 14]
        length, sectors = e[11] | e[12] << 8, e[13]
        out.append({"type": chr(e[8]), "data": image[pos:pos + (length or sectors * 256)]})
        pos += sectors * 256
    return out


def tap_blocks(tape):
    """Bloky TAP: [delka][flag][data][checksum] -> (flag, data bez flagu a souctu)."""
    pos, out = 0, []
    while pos + 2 <= len(tape):
        size = tape[pos] | tape[pos + 1] << 8
        block = tape[pos + 2:pos + 2 + size]
        out.append((block[0], block[1:-1]))
        pos += 2 + size
    return out


class TrdPlugin(unittest.TestCase):
    def host(self, name, keys):
        data = (ROOT / name).read_bytes()
        return data, PluginHost(self, "trd", name.split("/")[-1], data, VIEWTYPE_TRD,
                                keys=keys, menu_sites=["plugin_start.input"])

    def test_trd_extract_all_skips_deleted(self):
        image, host = self.host("test.trd", [ord("E")])
        host.run()
        expected = [e for e in trd_entries(image) if not e["deleted"]]
        self.assertEqual([d for _, d, _ in host.files], [e["data"] for e in expected])
        types = {"B": 0, "D": 1, "C": 3}
        for (_, _, header), entry in zip(host.files, expected):
            self.assertEqual(header["type"], types[entry["type"]])
            if entry["type"] == "C":
                self.assertEqual(header["p1"], entry["param"], "CODE: adresa nahrani")
        assert_golden(self, "trd_extract_all", {"files": summary(host), "screen": host.text()})

    def test_trd_beyond_64k_uses_seek(self):
        image, host = self.host("test.trd", [ord("E")])
        host.run()
        self.assertEqual(host.files[-1][1], trd_entries(image)[-1]["data"], "FARFILE za 64K")

    def test_trd_header_toggle_exports_raw(self):
        image, host = self.host("test.trd", [ord("d"), ord("e")])
        host.run()
        self.assertEqual(len(host.files), 1)
        self.assertIsNone(host.files[0][2], "d vypne +3DOS hlavicku")
        self.assertEqual(host.files[0][1], trd_entries(image)[0]["data"])

    def test_scl_samples(self):
        for name in ("SCL/dogma.scl", "SCL/BORED.SCL", "SCL/atarin.scl"):
            with self.subTest(name=name):
                image, host = self.host(name, [ord("E")])
                host.run()
                self.assertEqual([d for _, d, _ in host.files],
                                 [e["data"] for e in scl_entries(image)])
                stem = name.split("/")[-1].replace(".", "_")
                assert_golden(self, f"scl_{stem}", {"files": summary(host), "screen": host.text()})


class TapPlugin(unittest.TestCase):
    def test_export_all_matches_tape_blocks(self):
        tape = (ROOT / "Aeon.tap").read_bytes()
        host = PluginHost(self, "tap", "Aeon.tap", tape, VIEWTYPE_TAP, keys=[ord("E")],
                          menu_sites=["plugin_start.input"])
        host.run()
        blocks = tap_blocks(tape)
        # datove bloky s hlavickou pred sebou: obsah bez flagu a kontrolniho souctu
        expected = [(blocks[i][1], blocks[i - 1][1]) for i in range(1, len(blocks))
                    if blocks[i][0] == 0xFF and blocks[i - 1][0] == 0 and len(blocks[i - 1][1]) == 17]
        self.assertEqual([d for _, d, _ in host.files], [data for data, _ in expected])
        for (_, _, p3), (_, header) in zip(host.files, expected):
            self.assertEqual(p3["type"], header[0], "typ z hlavicky TAP")
            self.assertEqual(p3["p1"], header[13] | header[14] << 8, "param1 z hlavicky TAP")
        assert_golden(self, "tap_aeon_export_all", {"files": summary(host), "screen": host.text()})


if __name__ == "__main__":
    unittest.main()
