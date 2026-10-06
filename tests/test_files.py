"""Pluginy, ktere pracuji se soubory: dir_info (pocet polozek a velikost
adresare) a syscopy (kopirovani, presun a mazani adresaroveho stromu).

Bezi nad falesnym esxDOS (fakeesx.py). Vysledny strom se porovnava se stromem
spocitanym v Pythonu.
"""

import unittest

from cctest import build_plugin
from fakeesx import CTX, FeatureHost, EACCES, ENOENT

ABI = 1
DEPTH_LIMIT = 11                       # MAX_DEPTH v pluginech


def walk(tree, depth=0, limit=DEPTH_LIMIT):
    """(polozek, soucet velikosti) jako v dir_info: obsah adresaru do hloubky limit."""
    items = size = 0
    for value in tree.values():
        items += 1
        if isinstance(value, dict):
            if depth < limit:
                sub_items, sub_size = walk(value, depth + 1, limit)
                items += sub_items
                size += sub_size
        else:
            size += value if isinstance(value, int) else len(value)
    return items, size


def nested(levels, leaf=b"deep"):
    """Retez adresaru l0/l1/... se souborem v kazde urovni."""
    tree = {"file.txt": leaf}
    for i in reversed(range(levels)):
        tree = {f"level{i}": tree, f"f{i}.bin": bytes([i]) * (i + 1)}
    return tree


SAMPLE = {
    "readme.txt": b"Calm Commander\r\n" * 3,
    "Long File Name.dat": bytes(range(256)) * 9,
    "empty.bin": b"",
    "big.bin": bytes((i * 31) & 0xFF for i in range(5000)),
    "Docs": {
        "manual.txt": b"x" * 2049,           # vic nez jeden 2K blok kopirovani
        "Images": {"a.scr": bytes(6912), "b.scr": b"\x55" * 6912},
        "Empty Dir": {},
    },
    "src": {"main.asm": b"  org $8000\n", "lib": {"x.i.asm": b"; x\n"}},
}


# ---------------------------------------------------------------------------
# dir_info
# ---------------------------------------------------------------------------

class DirInfo(unittest.TestCase):
    def info(self, tree, path="C:/games", name="target", denied=()):
        binary, s = build_plugin("dir_info")
        host = FeatureHost(binary, tree)
        host.fs.denied = set(denied)
        host.services([host.svc_print])
        src, nm = host.string(path, 255), host.string(name, 255)
        ctx = bytearray(18)
        ctx[0] = ABI
        ctx[1:3], ctx[3:5] = src.to_bytes(2, "little"), nm.to_bytes(2, "little")
        ctx[16:18] = (0x9100).to_bytes(2, "little")
        host.m.write(CTX, bytes(ctx))
        host.run(s["plugin_start"])
        m = host.m
        return host, {"result": m.get(CTX + 5), "error": m.get(CTX + 6),
                      "stage": m.get(CTX + 7), "items": m.word(CTX + 8) | m.word(CTX + 10) << 16,
                      "size": m.word(CTX + 12) | m.word(CTX + 14) << 16}

    def test_counts_tree(self):
        _, info = self.info({"games": {"target": SAMPLE}})
        self.assertEqual(info["result"], 0)
        self.assertEqual((info["items"], info["size"]), walk(SAMPLE))

    def test_32bit_size(self):
        tree = {"a.bin": 70000, "b.bin": 0x00FFFFFF, "sub": {"c.bin": 0x01000001, "d": {"e": 123}}}
        _, info = self.info({"games": {"target": tree}})
        self.assertEqual((info["items"], info["size"]), walk(tree))

    def test_depth_limit(self):
        tree = nested(14)
        _, info = self.info({"games": {"target": tree}})
        self.assertEqual(info["result"], 0)
        self.assertEqual((info["items"], info["size"]), walk(tree))
        self.assertLess(info["items"], walk(tree, limit=99)[0], "hlubsi urovne se nepocitaji")

    def test_short_name_and_trailing_slash(self):
        tree = {"games": {"A Long Directory": {"x.txt": b"123"}}}
        _, info = self.info(tree, path="c:/GAMES/", name="ALONGD~1")
        self.assertEqual((info["result"], info["items"], info["size"]), (0, 1, 3))

    def test_missing_directory(self):
        _, info = self.info({"games": {}})
        self.assertEqual((info["result"], info["error"], info["stage"]), (1, ENOENT, 0x20))

    def test_path_too_long(self):
        _, info = self.info({}, path="C:/" + "x" * 300)
        self.assertEqual((info["result"], info["error"], info["stage"]), (1, 0x7D, 0x10))

    @unittest.expectedFailure
    def test_unreadable_subdir_is_reported(self):
        """ZNAMA CHYBA: count_dir po "call count_dir" dela "pop af", ktery
        prepise carry potomka - chyba v podadresari se ztrati a vysledek je
        neuplny soucet hlaseny jako uspech."""
        tree = {"games": {"target": {"a.txt": b"12345", "Locked": {"b.txt": b"x"}}}}
        _, info = self.info(tree, denied={"LOCKED"})
        self.assertEqual((info["result"], info["error"]), (1, EACCES))

    def test_closes_every_handle(self):
        host, _ = self.info({"games": {"target": SAMPLE}})
        self.assertEqual(host.fs.handles, {}, "zustaly otevrene handly")


# ---------------------------------------------------------------------------
# syscopy
# ---------------------------------------------------------------------------

COPY, MOVE, DELETE = 0, 1, 2


class SysCopy(unittest.TestCase):
    def run_copy(self, tree, mode, name="Docs", src="C:/from", dst="C:/to",
                 overwrite=(), cancel_after=None, denied=(), max_steps=50_000_000):
        binary, s = build_plugin("syscopy")
        host = FeatureHost(binary, tree)
        host.fs.denied = set(denied)
        answers = list(overwrite)
        self.asked = []

        def svc_overwrite():
            self.asked.append(host.m.cstring(host.m.cpu.hl, end=(0, 255)).decode())
            host.m.map(7, 0)                 # CC si MMU7 premapuje (plugin ho obnovi sam)
            host.done(answers.pop(0) if answers else 13)

        host.services([host.svc_print, host.svc_window, svc_overwrite])
        if cancel_after is not None:
            calls = {"n": 0}

            def count_reads(original=host.fs.read):
                calls["n"] += 1
                if calls["n"] == cancel_after:
                    host.m.keys = {(0xFE, 0), (0x7F, 0)}     # CAPS + SPACE
                return original()
            host.fs.read = count_reads
        short = host.fs.lookup(f"{src}/{name}")
        ctx = bytearray(23)
        ctx[0], ctx[1] = ABI, mode
        for offset, text in ((2, src), (4, dst), (6, short.short if short else name), (20, name)):
            ctx[offset:offset + 2] = host.string(text, 255).to_bytes(2, "little")
        ctx[18:20] = (0x9100).to_bytes(2, "little")
        host.m.write(CTX, bytes(ctx))
        host.run(s["plugin_start"], max_steps=max_steps)
        m = host.m
        info = {"result": m.get(CTX + 8), "error": m.get(CTX + 9), "stage": m.get(CTX + 22)}
        self.assertEqual(host.fs.handles, {}, "zustaly otevrene handly")
        self.assertEqual(m.mmu[6:8], [82, 99])
        return host, info

    def test_copy_tree(self):
        host, info = self.run_copy({"from": {"Docs": SAMPLE}, "to": {}}, COPY)
        self.assertEqual(info["result"], 0, info)
        self.assertEqual(host.fs.tree()["to"], {"Docs": SAMPLE})
        self.assertEqual(host.fs.tree()["from"], {"Docs": SAMPLE}, "zdroj zustal")

    def test_copy_into_existing_dir_merges(self):
        tree = {"from": {"Docs": {"a.txt": b"new", "b.txt": b"b"}},
                "to": {"Docs": {"keep.txt": b"keep"}}}
        host, info = self.run_copy(tree, COPY)
        self.assertEqual(info["result"], 0)
        self.assertEqual(host.fs.tree()["to"]["Docs"], {"keep.txt": b"keep", "a.txt": b"new", "b.txt": b"b"})

    def test_overwrite_answers(self):
        tree = {"from": {"Docs": {"a.txt": b"NEW A", "b.txt": b"NEW B", "c.txt": b"NEW C"}},
                "to": {"Docs": {"a.txt": b"old a", "b.txt": b"old b"}}}
        host, info = self.run_copy(tree, COPY, overwrite=[ord("n"), 13])
        self.assertEqual(info["result"], 0)
        self.assertEqual(self.asked, ["a.txt", "b.txt"], "ptal se jen na existujici soubory")
        self.assertEqual(host.fs.tree()["to"]["Docs"],
                         {"a.txt": b"old a", "b.txt": b"NEW B", "c.txt": b"NEW C"})

    def test_overwrite_break_cancels(self):
        tree = {"from": {"Docs": {"a.txt": b"NEW", "z.txt": b"Z"}}, "to": {"Docs": {"a.txt": b"old"}}}
        host, info = self.run_copy(tree, COPY, overwrite=[1])
        self.assertEqual((info["result"], info["error"]), (1, 0x7C))
        self.assertEqual(host.fs.tree()["to"]["Docs"], {"a.txt": b"old"})

    def test_caps_space_cancels_and_removes_partial_file(self):
        tree = {"from": {"Docs": {"big.bin": bytes(10000)}}, "to": {}}
        host, info = self.run_copy(tree, COPY, cancel_after=2)
        self.assertEqual((info["result"], info["error"]), (1, 0x7C))
        self.assertEqual(host.fs.tree()["to"], {"Docs": {}}, "rozkopirovany soubor smazan")

    def test_move_tree(self):
        host, info = self.run_copy({"from": {"Docs": SAMPLE, "other": b"o"}, "to": {}}, MOVE)
        self.assertEqual(info["result"], 0, info)
        self.assertEqual(host.fs.tree(), {"from": {"other": b"o"}, "to": {"Docs": SAMPLE}})

    def test_delete_tree(self):
        host, info = self.run_copy({"from": {"Docs": SAMPLE, "other": b"o"}}, DELETE)
        self.assertEqual(info["result"], 0, info)
        self.assertEqual(host.fs.tree(), {"from": {"other": b"o"}})

    def test_delete_uses_short_names(self):
        tree = {"from": {"A Long Directory Name": {"Some Long File.txt": b"1", "x": {"Deep Long.bin": b"2"}}}}
        host, info = self.run_copy(tree, DELETE, name="A Long Directory Name")
        self.assertEqual(info["result"], 0, info)
        self.assertEqual(host.fs.tree(), {"from": {}})
        unlinked = [c[1] for c in host.fs.calls if c[0] == "unlink"]
        self.assertTrue(all("~" in p for p in unlinked), unlinked)

    def test_destination_inside_source_is_refused(self):
        # cesta panelu i 8.3 jmeno jsou od NextZXOS, porovnavaji se presne
        host, info = self.run_copy({"from": {"Docs": {"a": b"1", "sub": {}}}}, COPY,
                                   src="C:/FROM", dst="C:/FROM/DOCS/SUB")
        self.assertEqual((info["result"], info["error"], info["stage"]), (1, 0x7E, 0x12))
        self.assertEqual([c for c in host.fs.calls if c[0] in ("mkdir", "open")], [])

    def test_similar_prefix_is_not_nested(self):
        host, info = self.run_copy({"from": {"Docs": {"a": b"1"}, "Docs2": {}}}, COPY,
                                   src="C:/FROM", dst="C:/FROM/DOCS2")
        self.assertEqual(info["result"], 0, info)

    def test_missing_source(self):
        _, info = self.run_copy({"from": {}, "to": {}}, COPY)
        self.assertEqual((info["result"], info["error"]), (1, ENOENT))

    # --- znama chyba: delete_dir ignoruje chybu v podadresari -------------
    # Po "call delete_dir" nasleduje "pop af", ktery prepise carry potomka.
    # Rodic pak adresar znovu otevre, najde stale tentyz podadresar a zkousi
    # ho mazat dokola - CC se zasekne (copy_dir to resi pres childCarry).

    @unittest.expectedFailure
    def test_delete_error_in_subdir_is_reported(self):
        tree = {"from": {"Docs": {"a.txt": b"1", "sub": {"locked.txt": b"x", "b.txt": b"2"}}}}
        _, info = self.run_copy(tree, DELETE, denied={"LOCKED.TXT"}, max_steps=200_000)
        self.assertEqual((info["result"], info["error"]), (1, EACCES))

    @unittest.expectedFailure
    def test_deep_tree_delete_reports_error(self):
        """Hlubsi nez MAX_DEPTH: mazani ma skoncit chybou $7F."""
        _, info = self.run_copy({"from": {"Docs": nested(13)}}, DELETE, max_steps=200_000)
        self.assertEqual((info["result"], info["error"]), (1, 0x7F))

    @unittest.expectedFailure
    def test_deep_tree_copy_is_complete_or_fails(self):
        """ZNAMA CHYBA: od hloubky MAX_DEPTH copy_dir polozky tise preskoci
        a hlasi uspech - soubory od 12. urovne v cili chybi."""
        host, info = self.run_copy({"from": {"Docs": nested(13)}, "to": {}}, COPY)
        if info["result"] == 0:
            self.assertEqual(host.fs.tree()["to"], {"Docs": nested(13)})


if __name__ == "__main__":
    unittest.main()
