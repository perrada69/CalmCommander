"""Falesny hostitel pro pluginy prohlizece (*.ccp).

Plugin bezi na $C000 ve strance 82 jako v CC, data souboru jsou v datovych
strankach a sluzby hostitele (SERVICE_*) jsou pasti v Pythonu. Vstup z
klavesnice je skript: klavesy dostava jen volani z mist, ktera test oznaci
jako hlavni nabidku pluginu (poznaji se podle navratove adresy), ostatni
volani vstupu - napr. kontrola BREAK behem dlouheho rozbalovani - dostanou
"nic nestisknuto".
"""

import json
import os

from cctest import Machine, ROOT, build_plugin

ABI = 1
PLUGIN_PAGE = 82
DATA_PAGES = [81, 83, 85, 87, 73, 75, 77, 79]   # viewDataPages v functions/viewer.asm
CTX, SERVICES, PAGE_TABLE, FILENAME, CURPATH = 0x9000, 0x9100, 0x9140, 0x9200, 0x9300
SVC_TRAPS = 0x0200
SERVICE_NAMES = ["print", "inkey", "window", "layer0", "input", "extract", "beep",
                 "extract_seek", "read_at", "write_open", "write_chunk", "write_close"]
# VIEWCTX offsety (plugin/plugin_api.i.asm)
C_TYPE, C_FILENAME, C_SIZE, C_DATA_PAGE, C_DATA_ADDR, C_READ_LEN = 1, 2, 4, 8, 9, 11
C_PAGE_COUNT, C_DATA_PAGES, C_SERVICES, C_CURPATH = 13, 14, 16, 18
C_EXTRACT_OFF, C_DIRTY, C_P3_TYPE, C_P3_P1, C_P3_P2, C_OFHI = 21, 40, 41, 42, 44, 46

GOLDEN = ROOT / "tests" / "golden"
TILEMAP = 0x4000                  # tilemapa CC: 80x32, na znak 2 bajty (znak, atribut)


class PluginHost:
    def __init__(self, test, plugin, filename, data, view_type, keys=(), menu_sites=()):
        self.test = test
        self.binary, self.s = build_plugin(plugin)
        m = self.m = Machine()
        self.source = data
        m.set_page(PLUGIN_PAGE, self.binary)
        m.map(6, PLUGIN_PAGE)
        for i, page in enumerate(DATA_PAGES):
            m.set_page(page, data[i * 8192:(i + 1) * 8192])
        pages = min(len(DATA_PAGES), (len(data) + 8191) // 8192)
        m.map(7, DATA_PAGES[0])
        m.write(TILEMAP, b" \x00" * 80 * 32)          # cista obrazovka CC
        m.write(FILENAME, filename.encode() + b"\0")
        m.write(CURPATH, b"c:/test\xff")
        m.write(PAGE_TABLE, bytes(DATA_PAGES))
        ctx = bytearray(48)
        ctx[0] = ABI
        ctx[C_TYPE] = view_type
        ctx[C_FILENAME:C_FILENAME + 2] = FILENAME.to_bytes(2, "little")
        ctx[C_SIZE:C_SIZE + 4] = len(data).to_bytes(4, "little")
        ctx[C_DATA_PAGE] = DATA_PAGES[0]
        ctx[C_DATA_ADDR:C_DATA_ADDR + 2] = (0xE000).to_bytes(2, "little")
        ctx[C_READ_LEN:C_READ_LEN + 2] = min(len(data), 8192).to_bytes(2, "little")
        ctx[C_PAGE_COUNT] = pages
        ctx[C_DATA_PAGES:C_DATA_PAGES + 2] = PAGE_TABLE.to_bytes(2, "little")
        ctx[C_SERVICES:C_SERVICES + 2] = SERVICES.to_bytes(2, "little")
        ctx[C_CURPATH:C_CURPATH + 2] = CURPATH.to_bytes(2, "little")
        ctx[C_P3_TYPE] = 0xFF
        m.write(CTX, bytes(ctx))
        table = bytearray()
        for i, name in enumerate(SERVICE_NAMES):
            addr = SVC_TRAPS + 4 * i
            table += addr.to_bytes(2, "little")
            m.trap(addr, getattr(self, "svc_" + name))
        m.write(SERVICES, bytes(table))
        self.keys = list(keys)
        # navesti v pluginu ukazuje na "call call_input", navrat je o 3 bajty dal
        self.menu_sites = {self.s[site] + 3 if isinstance(site, str) else site
                           for site in menu_sites}
        self.idle_inputs = 0
        self.last_key = 0
        self.screen = {}
        self.files = []            # (jmeno, data, p3dos hlavicka nebo None)
        self.open_file = None
        self.input_calls = 0
        self.snapshots = []        # obrazovka (rows()) pred kazdou klavesou z nabidky

    # --- beh ---------------------------------------------------------------

    def run(self, max_steps=20_000_000):
        m = self.m
        m.cpu.iy = 0x5C3A                        # jako v CC (systemove promenne)
        m.call(self.s["plugin_start"], sp=0x7100, a=ABI, hl=CTX, de=SERVICES,
               max_steps=max_steps)
        return m.cpu.a

    # --- pomocne -----------------------------------------------------------

    def caller(self):
        """Navratova adresa za "call call_input" v pluginu (2. polozka zasobniku)."""
        m = self.m
        return m.word((m.cpu.sp + 2) & 0xFFFF)

    def name_at(self, addr):
        raw = bytes(self.m.mem[addr:addr + 16])
        for end in (0, 255):
            if end in raw:
                raw = raw[:raw.index(end)]
        return raw.decode("latin-1")

    def data_pages(self, offset, count):
        out = bytearray()
        while count:
            page, off = divmod(offset, 8192)
            chunk = min(count, 8192 - off)
            out += self.m.page(DATA_PAGES[page])[off:off + chunk]
            offset += chunk
            count -= chunk
        return bytes(out)

    def p3dos(self):
        m = self.m
        kind = m.get(CTX + C_P3_TYPE)
        if kind == 0xFF:
            return None
        return {"type": kind, "p1": m.word(CTX + C_P3_P1), "p2": m.word(CTX + C_P3_P2)}

    def ok(self, value=0):
        """Navrat ze sluzby: A = hodnota, Z podle A (jako "or a"), bez carry."""
        cpu = self.m.cpu
        cpu.a = value
        cpu.f = (cpu.f & ~0x41) | (0x40 if value == 0 else 0)
        self.m.ret()

    def rows(self):
        """Cela tilemapa jako 32 radku po 80 znacich (mimo ASCII "~")."""
        m = self.m
        return ["".join(chr(c) if 32 <= c < 127 else "~"
                        for c in m.mem[TILEMAP + y * 160:TILEMAP + (y + 1) * 160:2])
                for y in range(32)]

    def text(self):
        """Obrazovka z tilemapy na $4000 jako radky textu (bez prazdnych
        radku na konci). Znaky mimo ASCII jsou "~"."""
        m = self.m
        rows = []
        for y in range(32):
            row = m.mem[TILEMAP + y * 160:TILEMAP + (y + 1) * 160:2]
            rows.append("".join(chr(c) if 32 <= c < 127 else "~" for c in row).rstrip())
        while rows and not rows[-1]:
            rows.pop()
        return rows

    # --- sluzby ------------------------------------------------------------

    def svc_print(self):
        """Jako print v CC: H = sloupec, L = radek, DE = text, A = atribut."""
        m = self.m
        addr = TILEMAP + m.cpu.l * 160 + m.cpu.h * 2
        for ch in m.cstring(m.cpu.de):
            m.put(addr, ch)
            m.put(addr + 1, m.cpu.a)
            addr += 2
        self.ok()

    def svc_inkey(self):
        self.ok(self.keys.pop(0) if self.keys else 1)

    def svc_window(self):
        """Jako window v CC: H,L = levy horni roh, B,C = sirka a vyska."""
        m = self.m
        x, y, w, h = m.cpu.h, m.cpu.l, m.cpu.b, m.cpu.c
        for row in range(y, min(y + h, 32)):
            addr = TILEMAP + row * 160 + x * 2
            m.write(addr, b" \x00" * max(0, min(w, 80 - x)))
        self.ok()

    def svc_layer0(self):
        self.ok()

    def svc_beep(self):
        self.ok()

    def svc_input(self):
        self.input_calls += 1
        if self.caller() not in self.menu_sites:
            self.idle_inputs += 1
            if self.idle_inputs > 300_000:
                raise AssertionError(f"plugin stale cte vstup mimo nabidku "
                                     f"(volani z ${self.caller():04X})")
            return self.ok(0)
        self.idle_inputs = 0
        if self.last_key:
            self.last_key = 0                    # klavesa pustena
            return self.ok(0)
        self.snapshots.append(self.rows())
        key = self.keys.pop(0) if self.keys else 1
        self.last_key = key
        self.ok(key)

    def svc_extract(self):
        m = self.m
        name = self.name_at(m.cpu.hl)
        self.files.append((name, self.data_pages(m.cpu.de, m.cpu.bc), self.p3dos()))
        m.put(CTX + C_DIRTY, 1)
        self.ok()

    def svc_extract_seek(self):
        m = self.m
        offset = m.cpu.de | m.word(CTX + C_OFHI) << 16
        name = self.name_at(m.cpu.hl)
        self.files.append((name, self.source[offset:offset + m.cpu.bc], self.p3dos()))
        m.put(CTX + C_DIRTY, 1)
        self.ok()

    def svc_read_at(self):
        m = self.m
        page, at, count = m.cpu.c, m.cpu.hl, m.cpu.de
        offset = m.word(CTX + C_EXTRACT_OFF) | m.word(CTX + C_OFHI) << 16
        self.test.assertLessEqual(at + count, 8192, "READ_AT pres konec stranky")
        m.set_page(page, self.source[offset:offset + count], at)
        self.ok()

    def svc_write_open(self):
        m = self.m
        self.test.assertIsNone(self.open_file, "WRITE_OPEN bez WRITE_CLOSE")
        self.open_file = [self.name_at(m.cpu.hl), bytearray(), self.p3dos()]
        self.ok()

    def svc_write_chunk(self):
        m = self.m
        self.test.assertIsNotNone(self.open_file, "WRITE_CHUNK bez WRITE_OPEN")
        self.open_file[1] += self.data_pages(m.cpu.de, m.cpu.bc)
        self.ok()

    def svc_write_close(self):
        name, data, header = self.open_file
        self.files.append((name, bytes(data), header))
        self.open_file = None
        self.m.put(CTX + C_DIRTY, 1)
        self.ok()


def assert_golden(test, name, value):
    """Porovna value s ulozenym snimkem tests/golden/<name>.json. Kdyz snimek
    chybi (nebo CC_UPDATE_GOLDEN=1), ulozi ho - zkontrolujte ho pak rucne."""
    GOLDEN.mkdir(parents=True, exist_ok=True)
    path = GOLDEN / f"{name}.json"
    text = json.dumps(value, indent=1, ensure_ascii=False, sort_keys=True)
    if not path.exists() or os.environ.get("CC_UPDATE_GOLDEN"):
        path.write_text(text + "\n", encoding="utf-8")
        return
    test.assertEqual(json.loads(path.read_text(encoding="utf-8")), json.loads(text),
                     f"vystup se lisi od tests/golden/{name}.json "
                     "(zamerna zmena? CC_UPDATE_GOLDEN=1)")
