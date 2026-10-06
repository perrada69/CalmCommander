"""Falesny esxDOS (RST $08) se souborovym systemem v Pythonu.

Pluginy dir_info, syscopy a bookmarks pracuji se soubory pres esxDOS volani
F_OPENDIR/F_READDIR/F_OPEN/... Tady je obsluhuje Python nad stromem v pameti.
Adresar se chova jako na FAT: smazana polozka zustane v adresari jako dira,
takze otevreny handle se po smazani neposune, a podadresare zacinaji polozkami
"." a "..". Kazda polozka ma dlouhe jmeno (LFN) a krátke 8.3; cesty se
hledaji podle obou, bez ohledu na velikost pismen. Format zaznamu a chybove
kody podle nextzxos_api.txt.
"""

from cctest import Machine

F_OPEN, F_CLOSE, F_READ, F_WRITE, F_SEEK = 0x9A, 0x9B, 0x9D, 0x9E, 0x9F
F_OPENDIR, F_READDIR, F_MKDIR, F_RMDIR, F_UNLINK = 0xA3, 0xA4, 0xAA, 0xAB, 0xAD
ENOENT, EACCES, EBADF, EISDIR, ENOTDIR, EEXIST = 5, 8, 13, 16, 17, 18
ATTR_DIR, ATTR_ARCHIVE = 0x10, 0x20
MODE_WRITE, MODE_LFN = 0x02, 0x10
TIME, DATE = 0x6000, 0x5B26           # 12:00:00, 6.9.2025


class Node:
    def __init__(self, name, data=None, size=None):
        self.name = name
        self.short = None
        self.is_dir = data is None and size is None
        self.slots = [] if self.is_dir else None     # polozky, smazana = None
        self.data = bytearray(data or b"")
        self.virtual_size = size                      # jen velikost, bez obsahu

    @property
    def size(self):
        return self.virtual_size if self.virtual_size is not None else len(self.data)

    def children(self):
        return [n for n in self.slots if n is not None and n.name not in (".", "..")]

    def find(self, name):
        name = name.upper()
        for n in self.children():
            if name in (n.name.upper(), n.short):
                return n
        return None

    def add(self, node):
        node.short = short_name(node.name, {n.short for n in self.children()})
        if node.is_dir:
            node.slots = [Node.marker("."), Node.marker("..")]
        self.slots.append(node)
        return node

    @staticmethod
    def marker(name):
        node = Node(name)
        node.short = name
        return node


def short_name(name, taken):
    """8.3 jmeno jako ve Windows: velka pismena, ~N pri zkraceni."""
    stem, dot, ext = name.rpartition(".") if "." in name.strip(".") else (name, "", "")
    clean = lambda s: "".join(c for c in s.upper() if c.isalnum() or c in "_-!#$%&'()@^`{}~")
    base, ext3 = clean(stem), clean(ext)[:3]
    if base == stem.upper() and len(base) <= 8 and ext3 == ext.upper() and len(ext) <= 3:
        candidate = base + ("." + ext3 if ext3 else "")
        if candidate not in taken:
            return candidate
    for i in range(1, 100):
        tail = f"~{i}"
        candidate = base[:8 - len(tail)] + tail + ("." + ext3 if ext3 else "")
        if candidate not in taken:
            return candidate
    raise ValueError(name)


class FakeEsx:
    def __init__(self, m, tree):
        self.m = m
        self.root = Node("")
        self.build(self.root, tree)
        self.handles = {}
        self.denied = set()         # jmena (velkymi), ktera nejde smazat ani otevrit
        self.calls = []             # (funkce, cesta nebo handle)
        m.trap(0x0008, self.rst8)

    def build(self, parent, tree):
        for name, value in tree.items():
            if isinstance(value, dict):
                self.build(parent.add(Node(name)), value)
            elif isinstance(value, int):
                parent.add(Node(name, size=value))
            else:
                parent.add(Node(name, data=value))

    # --- strom --------------------------------------------------------------

    def split(self, path):
        path = path.replace("\\", "/")
        if len(path) >= 2 and path[1] == ":":
            path = path[2:]
        return [p for p in path.split("/") if p]

    def lookup(self, path):
        node = self.root
        for part in self.split(path):
            if not node.is_dir:
                return None
            node = node.find(part)
            if node is None:
                return None
        return node

    def parent_of(self, path):
        parts = self.split(path)
        parent = self.lookup("/".join(parts[:-1]))
        return parent, parts[-1] if parts else ""

    def tree(self, node=None):
        """Strom zpet jako slovnik: adresar = dict, soubor = bytes (nebo velikost)."""
        node = node or self.root
        return {n.name: self.tree(n) if n.is_dir else
                (n.virtual_size if n.virtual_size is not None else bytes(n.data))
                for n in node.children()}

    def remove(self, node, parent):
        parent.slots[parent.slots.index(node)] = None

    # --- RST $08 ------------------------------------------------------------

    def rst8(self):
        m, cpu = self.m, self.m.cpu
        ret = m.word(cpu.sp)
        code = m.get(ret)
        m.put(cpu.sp, ret + 1, 2)
        handler = {F_OPENDIR: self.opendir, F_READDIR: self.readdir, F_CLOSE: self.close,
                   F_OPEN: self.open, F_READ: self.read, F_WRITE: self.write,
                   F_SEEK: self.seek, F_MKDIR: self.mkdir, F_RMDIR: self.rmdir,
                   F_UNLINK: self.unlink}.get(code)
        if handler is None:
            raise AssertionError(f"esxDOS ${code:02X} neni emulovano (z ${ret - 1:04X})")
        error = handler()
        if error:
            cpu.a = error
        m.carry(bool(error))
        m.ret()

    def path(self):
        return self.m.cstring(self.m.cpu.ix).decode("latin-1")

    def handle(self, entry):
        h = next(i for i in range(1, 256) if i not in self.handles)   # jako esxDOS: volne cislo
        self.handles[h] = entry
        self.m.cpu.a = h
        return 0

    def is_denied(self, node):
        return node.name.upper() in self.denied or node.short in self.denied

    def opendir(self):
        path = self.path()
        self.calls.append(("opendir", path))
        node = self.lookup(path)
        if node is None:
            return ENOENT
        if self.is_denied(node):
            return EACCES
        if not node.is_dir:
            return ENOTDIR
        return self.handle({"dir": node, "pos": 0, "lfn": bool(self.m.cpu.b & MODE_LFN)})

    def readdir(self):
        m, cpu = self.m, self.m.cpu
        entry = self.handles.get(cpu.a)
        if not entry or "dir" not in entry:
            return EBADF
        slots = entry["dir"].slots
        while entry["pos"] < len(slots) and slots[entry["pos"]] is None:
            entry["pos"] += 1
        if entry["pos"] >= len(slots):
            cpu.a = 0
            return 0
        node = slots[entry["pos"]]
        entry["pos"] += 1
        name = node.name if entry["lfn"] else node.short
        record = (bytes([ATTR_DIR if node.is_dir else ATTR_ARCHIVE]) + name.encode("latin-1")
                  + b"\0" + TIME.to_bytes(2, "little") + DATE.to_bytes(2, "little")
                  + (0 if node.is_dir else node.size).to_bytes(4, "little"))
        m.write(cpu.ix, record)
        cpu.a = 1
        return 0

    def close(self):
        self.calls.append(("close", self.m.cpu.a))
        if self.handles.pop(self.m.cpu.a, None) is None:
            return EBADF
        self.m.cpu.a = 0
        return 0

    def open(self):
        cpu = self.m.cpu
        path, mode = self.path(), cpu.b
        self.calls.append(("open", path, mode))
        node = self.lookup(path)
        create = mode & 0x0C
        if node is not None and node.is_dir:
            return EISDIR
        if node is None:
            if not create:
                return ENOENT
            parent, name = self.parent_of(path)
            if parent is None or not parent.is_dir:
                return ENOENT
            node = parent.add(Node(name, data=b""))
        elif create == 0x04:
            return EEXIST
        elif create == 0x0C:
            node.data = bytearray()
            node.virtual_size = None
        return self.handle({"file": node, "pos": 0, "write": bool(mode & MODE_WRITE)})

    def file(self):
        entry = self.handles.get(self.m.cpu.a)
        return entry if entry and "file" in entry else None

    def read(self):
        m, cpu = self.m, self.m.cpu
        entry = self.file()
        if entry is None:
            return EBADF
        node = entry["file"]
        chunk = bytes(node.data[entry["pos"]:entry["pos"] + cpu.bc])
        m.write(cpu.ix, chunk)
        entry["pos"] += len(chunk)
        cpu.bc = cpu.de = len(chunk)
        cpu.hl = (cpu.ix + len(chunk)) & 0xFFFF
        return 0

    def write(self):
        m, cpu = self.m, self.m.cpu
        entry = self.file()
        if entry is None or not entry["write"]:
            return EBADF
        data = bytes(m.mem[cpu.ix:cpu.ix + cpu.bc])
        node, pos = entry["file"], entry["pos"]
        if len(node.data) < pos:
            node.data += bytes(pos - len(node.data))
        node.data[pos:pos + len(data)] = data
        entry["pos"] += len(data)
        cpu.bc = len(data)                  # BC = pocet skutecne zapsanych bajtu
        return 0

    def seek(self):
        cpu = self.m.cpu
        entry = self.file()
        if entry is None:
            return EBADF
        offset = cpu.bc << 16 | cpu.de
        pos = {0: offset, 1: entry["pos"] + offset, 2: entry["pos"] - offset}[cpu.ix & 0xFF]
        entry["pos"] = max(0, min(pos, entry["file"].size))
        cpu.bc, cpu.de = entry["pos"] >> 16, entry["pos"] & 0xFFFF
        return 0

    def mkdir(self):
        path = self.path()
        self.calls.append(("mkdir", path))
        if self.lookup(path) is not None:
            return EEXIST
        parent, name = self.parent_of(path)
        if parent is None or not parent.is_dir:
            return ENOENT
        parent.add(Node(name))
        return 0

    def rmdir(self):
        path = self.path()
        self.calls.append(("rmdir", path))
        node = self.lookup(path)
        if node is None:
            return ENOENT
        if not node.is_dir:
            return ENOTDIR
        if node.children():
            return EACCES
        self.remove(node, self.parent_of(path)[0])
        return 0

    def unlink(self):
        path = self.path()
        self.calls.append(("unlink", path))
        node = self.lookup(path)
        if node is None:
            return ENOENT
        if node.is_dir:
            return EISDIR
        if self.is_denied(node):
            return EACCES
        self.remove(node, self.parent_of(path)[0])
        return 0


# ---------------------------------------------------------------------------
# Hostitel pro pluginy s vlastnim kontextem (dir_info, syscopy, bookmarks)
# ---------------------------------------------------------------------------

TILEMAP = 0x4000
CTX, SERVICES, STRINGS = 0x9000, 0x9100, 0x9200
SVC_TRAPS = 0x0200
PLUGIN_PAGE, WORK_PAGE = 82, 99


class FeatureHost:
    """Plugin na $C000 (MMU6), pracovni stranka na $E000 (MMU7) jako v CC.
    services = seznam funkci obsluhy sluzeb v poradi tabulky (po 2 bajtech)."""

    def __init__(self, plugin_binary, tree):
        m = self.m = Machine()
        m.set_page(PLUGIN_PAGE, plugin_binary)
        m.map(6, PLUGIN_PAGE)
        m.map(7, WORK_PAGE)
        m.write(TILEMAP, b" \x00" * 80 * 32)
        self.fs = FakeEsx(m, tree)
        self.strings = STRINGS
        self.printed = []           # vsechny texty poslane sluzbou PRINT

    def string(self, text, end=0):
        """Ulozi retezec do pameti hostitele, vrati jeho adresu."""
        addr = self.strings
        raw = text.encode("latin-1") if isinstance(text, str) else text
        self.m.write(addr, raw + bytes([end]))
        self.strings += len(raw) + 1
        return addr

    def services(self, handlers):
        table = bytearray()
        for i, handler in enumerate(handlers):
            addr = SVC_TRAPS + 4 * i
            table += addr.to_bytes(2, "little")
            self.m.trap(addr, handler)
        self.m.write(SERVICES, bytes(table))

    def done(self, value=0):
        cpu = self.m.cpu
        cpu.a = value
        cpu.f = (cpu.f & ~0x41) | (0x40 if value == 0 else 0)
        self.m.ret()

    def svc_print(self):
        m = self.m
        addr = TILEMAP + m.cpu.l * 160 + m.cpu.h * 2
        self.printed.append(m.cstring(m.cpu.de).decode("latin-1"))
        for ch in m.cstring(m.cpu.de):
            m.put(addr, ch)
            m.put(addr + 1, m.cpu.a)
            addr += 2
        self.done()

    def svc_window(self):
        m = self.m
        x, y, w, h = m.cpu.h, m.cpu.l, m.cpu.b, m.cpu.c
        for row in range(y, min(y + h, 32)):
            m.write(TILEMAP + row * 160 + x * 2, b" \x00" * max(0, min(w, 80 - x)))
        self.done()

    def text(self):
        rows = []
        for y in range(32):
            row = self.m.mem[TILEMAP + y * 160:TILEMAP + (y + 1) * 160:2]
            rows.append("".join(chr(c) if 32 <= c < 127 else "~" for c in row).rstrip())
        while rows and not rows[-1]:
            rows.pop()
        return rows

    def run(self, start, max_steps=20_000_000):
        self.m.call(start, sp=0x7100, hl=CTX, de=SERVICES, max_steps=max_steps)
        return self.m.cpu.a
