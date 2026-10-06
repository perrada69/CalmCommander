"""Zalozky (bookmarks.ccp): pridani, seznam, filtr, vyber a prevod stareho
formatu souboru c:/sys/bookmark.cfg.

Plugin cte klavesnici sam pres porty, klavesy maka Typist z cctest.
Soubor zalozek je ve falesnem esxDOS a kontroluje se bajt po bajtu.
"""

import unittest

from cctest import ROOT, Typist, build_plugin
from fakeesx import CTX, FeatureHost

ABI, ADD, LIST = 2, 1, 2
NAME, PATH = 25, 264
RECORD = NAME + 1 + PATH
OLD_NAME = 13
OLD_RECORD = OLD_NAME + 1 + PATH
RESULT_BUFFER = 0x9800


def record(name, path, drive="C", name_size=NAME):
    raw_name = name.ljust(name_size - 1).encode() + b"\0"
    raw_path = path.encode() + b"\xff"
    return raw_name + drive.encode() + raw_path.ljust(PATH, b"\0")


def records(data, size=RECORD):
    return [data[i:i + size] for i in range(0, len(data), size)]


TREE = {"sys": {}, "games": {"A Long Directory": {"x.tap": b"1"}, "demos": {}}}


class Bookmarks(unittest.TestCase):
    def run_plugin(self, mode, keys, cfg=None, path="C:/GAMES/ALONGD~1/"):
        binary, s = build_plugin("bookmarks")
        tree = {k: dict(v) if isinstance(v, dict) else v for k, v in TREE.items()}
        if cfg is not None:
            tree["sys"] = {"bookmark.cfg": cfg}
        host = FeatureHost(binary, tree)
        host.services([host.svc_print, host.svc_window])
        host.m.cpu.iff1 = host.m.cpu.iff2 = 1
        self.typist = Typist(host.m, keys, (s["read_key"], s["symtab"]))
        ctx = bytearray(10)
        ctx[0], ctx[1], ctx[4] = ABI, mode, ord("C")
        ctx[2:4] = host.string(path, 255).to_bytes(2, "little")
        ctx[8:10] = RESULT_BUFFER.to_bytes(2, "little")
        host.m.write(CTX, bytes(ctx))
        host.run(s["plugin_start"], max_steps=5_000_000)
        self.assertEqual(host.fs.handles, {}, "zustaly otevrene handly")
        m = host.m
        self.result = {"result": m.get(CTX + 5), "error": m.get(CTX + 6),
                       "drive": chr(m.get(CTX + 7)),
                       "path": m.cstring(RESULT_BUFFER, end=(0, 255)).decode()}
        self.screen = "\n".join(host.text())
        self.assertEqual(self.typist.queue, [], "vsechny klavesy pouzity")
        return host

    def cfg(self, host):
        return host.fs.tree()["sys"].get("bookmark.cfg")

    # --- pridani ------------------------------------------------------------

    def test_add_creates_file(self):
        host = self.run_plugin(ADD, list("My game") + [13])
        data = self.cfg(host)
        self.assertEqual(len(data), RECORD)
        self.assertEqual(data[:NAME], b"My game\0" + b" " * 16 + b"\0", "jmeno ukoncene nulou")
        self.assertEqual(data[NAME:NAME + 1 + 19], b"CC:/GAMES/ALONGD~1/\xff")
        self.assertIn("C:/games/A Long Directory", self.screen, "cesta s dlouhymi jmeny")

    def test_add_appends(self):
        old = record("first", "C:/DEMOS/")
        host = self.run_plugin(ADD, list("second") + [13], cfg=old)
        data = self.cfg(host)
        self.assertEqual(records(data)[0], old)
        self.assertEqual(records(data)[1][:6], b"second")

    def test_add_backspace_and_cancel(self):
        host = self.run_plugin(ADD, list("abX") + [12, "c", 13])
        self.assertEqual(self.cfg(host)[:4], b"abc\0")
        host = self.run_plugin(ADD, list("zzz") + [1])
        self.assertIsNone(self.cfg(host), "BREAK nic nezapise")

    def test_add_limit(self):
        full = b"".join(record(f"b{i}", "C:/") for i in range(200))
        host = self.run_plugin(ADD, list("x") + [13, 13], cfg=full)
        self.assertEqual(self.cfg(host), full)

    @unittest.expectedFailure
    def test_old_format_is_migrated(self):
        """ZNAMA CHYBA: migration_read_old_record a migration_write_new_record
        po F_READ/F_WRITE delaji "ld a,b : or c : jr nz,migration_short_io",
        ale BC je pocet prenesenych bajtu (na tom stoji i kopirovani v
        syscopy). Prevod proto skonci hlaskou o chybe souboru a stary soubor
        zustane beze zmeny - jako build/bookmark.cfg.after-failed-migration.
        Soubor je skutecny stary bookmark.cfg (build/bookmark.cfg.pre-name24.bak).
        """
        old = (ROOT / "tests" / "fixtures" / "bookmark-old-format.cfg").read_bytes()
        host = self.run_plugin(LIST, [1], cfg=old)      # BREAK zavre seznam i hlasku
        expected = [o[:OLD_NAME] + bytes(NAME - OLD_NAME) + o[OLD_NAME:]
                    for o in records(old, OLD_RECORD)]
        self.assertEqual(records(self.cfg(host)), expected)

    # --- seznam -------------------------------------------------------------

    SAMPLE = b"".join([record("Games", "C:/GAMES/"), record("Demos", "C:/DEMOS/"),
                       record("Long dir", "C:/GAMES/ALONGD~1/", drive="D")])

    def test_select_second(self):
        self.run_plugin(LIST, [10, 13], cfg=self.SAMPLE)
        self.assertEqual(self.result["result"], 1)
        self.assertEqual((self.result["drive"], self.result["path"]), ("C", "C:/DEMOS/"))

    def test_cancel(self):
        self.run_plugin(LIST, [10, 1], cfg=self.SAMPLE)
        self.assertEqual(self.result["result"], 0)

    def test_filter_is_case_insensitive_substring(self):
        self.run_plugin(LIST, list("DIR") + [13], cfg=self.SAMPLE)
        self.assertEqual((self.result["drive"], self.result["path"]), ("D", "C:/GAMES/ALONGD~1/"))

    def test_filter_backspace(self):
        self.run_plugin(LIST, list("mo") + [12, 12, 10, 10, 13], cfg=self.SAMPLE)
        self.assertEqual(self.result["path"], "C:/GAMES/ALONGD~1/")

    def test_cursor_stops_at_last(self):
        self.run_plugin(LIST, [10] * 5 + [13], cfg=self.SAMPLE)
        self.assertEqual(self.result["path"], "C:/GAMES/ALONGD~1/")

    def test_empty_list_message(self):
        host = self.run_plugin(LIST, [13])
        self.assertEqual(self.result["result"], 0)
        self.assertIsNone(self.cfg(host))

    def test_list_screen(self):
        self.run_plugin(LIST, [1], cfg=self.SAMPLE)
        for name in ("Games", "Demos", "Long dir"):
            self.assertIn(name, self.screen)


if __name__ == "__main__":
    unittest.main()
