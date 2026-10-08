"""Spousteni skutecneho Z80 kodu Calm Commanderu v emulatoru.

Vzor je projekt Nextheus: kod se prelozi sjasmplusem se symboly a bezi
v emulatoru z balicku z80 (pip install z80). Stranky Nextu, MMU, Next
registry, porty a sluzby OS (esxDOS, +3DOS, ROM) emuluje Python. Na CSpect
ani na SD image se nesahá.

Balicek z80 neumi instrukce Z80N. CC a pluginy pouzivaji jen nextreg, mul d,e
a add hl,a: na kazdy vyskyt jejich kodu v namapovanych strankach se nastavi
breakpoint a instrukce se provede v Pythonu. Breakpoint na datech nevadi -
zastavi se jen tehdy, kdyz na nej skutecne skoci PC.
"""

from functools import lru_cache
from pathlib import Path
import re
import subprocess

import z80


ROOT = Path(__file__).resolve().parents[1]
BUILD = ROOT / "build" / "test"
# stejny assembler jako compile.bat (cmd najde sjasmplus.exe nejdriv v koreni projektu)
SJASMPLUS = str(ROOT / "sjasmplus.exe") if (ROOT / "sjasmplus.exe").exists() else "sjasmplus"

ROM = 255                     # hodnota MMU0/1 pro ROM
DIV = 1000                    # pseudo-stranka: DivMMC RAM s dot commandem na $2000
RAM_1MB = 96                  # 8K stranek RAM na 1MB Nextu (0-95), 2MB ma 224
SENTINEL = 0x0100             # navratova adresa testu: zde beh konci
RST18_RETURN = 0x0102         # kam se vraci rutina zavolana pres RST $18

Z80N = {b"\xed\x91": "nextreg_nn", b"\xed\x92": "nextreg_na",
        b"\xed\x30": "mul", b"\xed\x31": "add_hl_a"}


# ---------------------------------------------------------------------------
# Sestaveni
# ---------------------------------------------------------------------------

def parse_symbols(path):
    symbols = {}
    for line in Path(path).read_text(errors="replace").splitlines():
        m = re.fullmatch(r"(\S+): EQU 0x([0-9A-Fa-f]+)", line.strip(), re.IGNORECASE)
        if m:
            symbols[m[1]] = int(m[2], 16)
    return symbols


def assemble(source, name, *args):
    """Prelozi zdroj z korene projektu, vrati symboly. Vystupy SAVEBIN jsou
    stejne jako pri compile.bat, symboly a listing jdou do build/test."""
    BUILD.mkdir(parents=True, exist_ok=True)
    sym = BUILD / f"{name}.sym"
    cmd = [SJASMPLUS, "--nologo", f"--sym={sym}", f"--lst={BUILD / (name + '.lst')}",
           *args, source]
    result = subprocess.run(cmd, cwd=ROOT, capture_output=True, text=True)
    if result.returncode:
        raise RuntimeError(f"sjasmplus {source} selhal:\n{result.stdout}\n{result.stderr}")
    return parse_symbols(sym)


@lru_cache(maxsize=None)
def build_dot():
    """CC pro dot command + zavadec. Vraci (symboly CC, symboly zavadece)."""
    cc = assemble("cc.asm", "ccd", "-DCC_DOT", "--exp=build/dot/ccd.exp")
    loader = assemble("dot/ccdot.asm", "ccdot", f"--raw={BUILD / 'cc'}")
    return cc, loader


@lru_cache(maxsize=None)
def build_basic():
    return assemble("cc.asm", "cc")


@lru_cache(maxsize=None)
def build_plugin(name):
    syms = assemble(f"plugin/{name}.asm", name)
    return (ROOT / "plugin" / f"{name}.ccp").read_bytes(), syms


# ---------------------------------------------------------------------------
# Stroj
# ---------------------------------------------------------------------------

class Machine:
    def __init__(self, ram_pages=RAM_1MB):
        self.cpu = z80.Z80Machine()
        self.mem = self.cpu.memory
        self.pages = {}
        # Stroj je 1MB Next: namapovani stranky, ktera tam neni, nebo Layer 2
        # mimo prvni RAM cip se zapise sem a beh pak skonci chybou.
        self.ram_pages = ram_pages
        self.bad_pages = []
        self.mmu = [ROM, ROM, 10, 11, 4, 5, 0, 1]
        for slot in range(8):
            self.pages.setdefault(self.mmu[slot], bytearray(8192))
        self.armed = {slot: set() for slot in range(8)}
        self.aliased = [False] * 8
        self.z80n_cache = {}
        self.traps = {}
        self.nextregs = {0x07: 0, 0x15: 0, 0x43: 0, 0x68: 0, 0x69: 0, 0x6B: 0, 0xB8: 0x82}
        self.selected_reg = 0
        self.port_writes = []          # (port, hodnota) v poradi
        self.events = []               # volny zaznam pro testy
        self.keys = set()              # stisknute klavesy: (radek $xxFE, bit)
        # kmouse: citace pozice ($FBDF X, $FFDF Y), tlacitka (bit 0 prave,
        # bit 1 leve, bit 2 prostredni) a kolecko 0..15 (horni pulka $FADF).
        # mouse_hold = kolikrat se jeste precte $FADF, nez se tlacitka pusti.
        self.mouse_x = self.mouse_y = 0x80
        self.mouse_buttons = 0
        self.mouse_hold = None
        self.mouse_wheel = 15
        self.border = None
        self.interrupts = False        # dorucovat IM1 preruseni na konci snimku
        self.isr_checks = []           # funkce volane pri kazdem preruseni
        self.frames = 0
        self.steps = 0
        self.cpu.set_input_callback(self.port_in)
        self.cpu.set_output_callback(self.port_out)
        self.cpu.set_write_callback(self.aliased_write)
        self.trap(SENTINEL, None)
        self.trap(0x0038, self.isr_rom)

    # --- pamet a stranky ---------------------------------------------------

    def page(self, page):
        """Aktualni obsah stranky (i kdyz je zrovna namapovana)."""
        for slot in range(8):
            if self.mmu[slot] == page:
                return bytearray(self.mem[slot * 8192:(slot + 1) * 8192])
        return self.pages.setdefault(page, bytearray(8192))

    def set_page(self, page, data, offset=0):
        content = self.page(page)
        content[offset:offset + len(data)] = data
        self.pages[page] = content
        self.z80n_cache.pop(page, None)
        for slot in range(8):
            if self.mmu[slot] == page:
                self.mem[slot * 8192:(slot + 1) * 8192] = content
                self.rearm(slot)

    def map(self, slot, page):
        if page not in (ROM, DIV) and page >= self.ram_pages:
            self.bad_pages.append(f"MMU{slot} = {page}")
        if self.mmu[slot] == page:
            return
        start = slot * 8192
        old = self.mmu[slot]
        if self.mmu.count(old) == 1:
            self.pages[old] = bytearray(self.mem[start:start + 8192])
        content = self.page(page)          # i z jineho slotu, kde uz je namapovana
        self.mmu[slot] = page
        self.mem[start:start + 8192] = content
        self.rearm(slot)
        self.update_aliases()

    def update_aliases(self):
        """Stranka namapovana ve vice slotech je na Nextu jedna pamet: zapisy
        do jednoho slotu se zrcadli do ostatnich (callback jen na techto
        adresach, jinde bezi emulator naplno)."""
        cpu = self.cpu
        for slot in range(8):
            aliased = self.mmu.count(self.mmu[slot]) > 1
            if aliased != self.aliased[slot]:
                start = slot * 8192
                if aliased:
                    cpu.mark_addrs(start, 8192, cpu.WRITE_MARK)
                else:
                    cpu.unmark_addrs(start, 8192, cpu.WRITE_MARK)
                self.aliased[slot] = aliased

    def aliased_write(self, addr, value):
        slot, offset = divmod(addr, 8192)
        page = self.mmu[slot]
        for other in range(8):
            if self.mmu[other] == page:
                self.mem[other * 8192 + offset] = value

    def z80n_offsets(self, slot):
        """Pozice kodu Z80N ve strance. Vysledek se pamatuje, dokud stranku
        neprepise Python (nacteni kodu). Zapisy procesoru Z80N instrukce
        nevytvari (kod si CC ani pluginy neskladaji) a zastaraly breakpoint
        na datech nevadi - instrukce se pri zastaveni overuje podle bajtu."""
        page = self.mmu[slot]
        cached = self.z80n_cache.get(page)
        if cached is None:
            block = bytes(self.mem[slot * 8192:(slot + 1) * 8192])
            found = []
            for opcode in Z80N:
                i = block.find(opcode)
                while i >= 0:
                    found.append(i)
                    i = block.find(opcode, i + 1)
            cached = self.z80n_cache[page] = tuple(found)
        return cached

    def rearm(self, slot):
        for addr in self.armed[slot]:
            if addr not in self.traps:
                self.cpu.clear_breakpoint(addr)
        start = slot * 8192
        found = {start + offset for offset in self.z80n_offsets(slot)}
        for addr in found:
            self.cpu.set_breakpoint(addr)
        self.armed[slot] = found

    def write(self, addr, data):
        self.mem[addr:addr + len(data)] = data
        self.sync_aliases(addr, len(data))
        self.rearm_range(addr, len(data))

    def sync_aliases(self, addr, size):
        for slot in {a // 8192 for a in range(addr, addr + max(size, 1))}:
            if self.mmu.count(self.mmu[slot]) > 1:
                block = bytes(self.mem[slot * 8192:(slot + 1) * 8192])
                for other in range(8):
                    if other != slot and self.mmu[other] == self.mmu[slot]:
                        self.mem[other * 8192:(other + 1) * 8192] = block

    def rearm_range(self, addr, size):
        for slot in {a // 8192 for a in range(addr, addr + max(size, 1))}:
            self.z80n_cache.pop(self.mmu[slot], None)
            self.rearm(slot)

    def put(self, addr, value, size=1):
        self.mem[addr:addr + size] = value.to_bytes(size, "little")
        self.sync_aliases(addr, size)

    def get(self, addr, size=1):
        return int.from_bytes(self.mem[addr:addr + size], "little")

    def word(self, addr):
        return self.get(addr, 2)

    def cstring(self, addr, end=(0,)):
        out = bytearray()
        while self.mem[addr] not in end:
            out.append(self.mem[addr])
            addr += 1
        return bytes(out)

    # --- Next registry a porty --------------------------------------------

    def nextreg(self, reg, value):
        if 0x50 <= reg <= 0x57:
            self.map(reg - 0x50, value)
        elif reg in (0x12, 0x13) and (value & 0x7F) * 2 + 5 >= RAM_1MB:
            # Layer 2 256x192 = 3 banky a musi byt v prvnim RAM cipu i na 2MB
            self.bad_pages.append(f"Layer 2 (${reg:02X}) v bance {value}")
        self.nextregs[reg] = value
        self.events.append(("nextreg", reg, value))

    def read_nextreg(self, reg):
        if 0x50 <= reg <= 0x57:
            return self.mmu[reg - 0x50]
        return self.nextregs.get(reg, 0)

    def port_in(self, port):
        if port == 0x253B:
            return self.read_nextreg(self.selected_reg)
        if port & 0xFF == 0xFE:
            # klavesa = (horni bajt portu jeji pulrady, bit), napr. ENTER = ($BF, 0);
            # pulrada je vybrana, kdyz je jeji nulovy bit nulovy i v adrese portu
            value = 0xFF
            for row, bit in self.keys:
                if not (port >> 8) & (~row & 0xFF):
                    value &= ~(1 << bit)
            return value & 0xFF
        if port == 0xFBDF:
            return self.mouse_x
        if port == 0xFFDF:
            return self.mouse_y
        if port == 0xFADF:
            value = (self.mouse_wheel & 15) << 4 | (0x0F & ~self.mouse_buttons)
            if self.mouse_hold is not None:
                self.mouse_hold -= 1
                if self.mouse_hold <= 0:
                    self.mouse_buttons, self.mouse_hold = 0, None
            return value
        return 0xFF

    def port_out(self, port, value):
        self.port_writes.append((port, value))
        if port == 0x243B:
            self.selected_reg = value
        elif port == 0x253B:
            self.nextreg(self.selected_reg, value)
        elif port & 0xFF == 0xFE:
            self.border = value & 7
        elif port == 0x7FFD:
            bank = value & 7
            self.map(6, bank * 2)
            self.map(7, bank * 2 + 1)

    def writes_to(self, port):
        return [i for i, (p, _) in enumerate(self.port_writes) if p == port]

    # --- pasti -------------------------------------------------------------

    def trap(self, addr, handler):
        self.traps[addr] = handler
        self.cpu.set_breakpoint(addr)

    def ret(self):
        self.cpu.pc = self.word(self.cpu.sp)
        self.cpu.sp = (self.cpu.sp + 2) & 0xFFFF

    def push(self, value):
        self.cpu.sp = (self.cpu.sp - 2) & 0xFFFF
        self.put(self.cpu.sp, value, 2)

    def carry(self, flag):
        self.cpu.f = (self.cpu.f | 1) if flag else (self.cpu.f & ~1)

    def isr_rom(self):
        """IM1 obsluha ROM3: pise FRAMES do systemovych promennych - proto
        musi byt v MMU2 stranka 10 a IY = $5C3A."""
        self.frames += 1
        for check in self.isr_checks:
            check(self)
        frames = self.word(0x5C78) + 1
        self.put(0x5C78, frames & 0xFFFF, 2)
        self.cpu.iff1 = self.cpu.iff2 = 1
        self.ret()

    # --- beh ---------------------------------------------------------------

    def call(self, addr, sp=None, max_steps=2_000_000, **regs):
        """Zavola rutinu na addr, vrati se po jejim RET na SENTINEL."""
        if sp is not None:
            self.cpu.sp = sp
        for name, value in regs.items():
            setattr(self.cpu, name, value)
        self.push(SENTINEL)
        self.cpu.pc = addr
        self.run(max_steps)

    def run(self, max_steps=2_000_000):
        cpu = self.cpu
        for _ in range(max_steps):
            self.steps += 1
            pc = cpu.pc
            if pc == SENTINEL:
                if self.bad_pages:
                    raise AssertionError(f"pamet, kterou {self.ram_pages * 8}K Next nema: "
                                         + ", ".join(dict.fromkeys(self.bad_pages)))
                return
            handler = self.traps.get(pc)
            if handler is not None:
                handler()
                continue
            op = bytes(self.mem[pc:pc + 2])
            kind = Z80N.get(op)
            if kind and pc in self.armed[pc // 8192]:
                self.z80n(kind, pc)
                continue
            cpu.step_over_breakpoint()
            cpu.ticks_to_stop = 20000
            events = cpu.run()
            if events & cpu._END_OF_FRAME and self.interrupts and cpu.iff1:
                cpu.on_handle_active_int()
        raise AssertionError(f"kod se nevratil, PC=${self.cpu.pc:04X}")

    def z80n(self, kind, pc):
        cpu = self.cpu
        if kind == "nextreg_nn":
            reg, value = self.mem[pc + 2], self.mem[pc + 3]
            cpu.pc = pc + 4
            self.nextreg(reg, value)
        elif kind == "nextreg_na":
            reg = self.mem[pc + 2]
            cpu.pc = pc + 3
            self.nextreg(reg, cpu.a)
        elif kind == "mul":
            cpu.de = (cpu.d * cpu.e) & 0xFFFF
            cpu.pc = pc + 2
        elif kind == "add_hl_a":
            cpu.hl = (cpu.hl + cpu.a) & 0xFFFF
            cpu.pc = pc + 2


# ---------------------------------------------------------------------------
# Pomocne vypocty pro BASIC
# ---------------------------------------------------------------------------

def hidden(value):
    """Skryty 5bajtovy tvar maleho cisla v radku BASICu."""
    return bytes([0x0E, 0, 0, value & 0xFF, value >> 8, 0])


# ---------------------------------------------------------------------------
# Klavesnice: psani pres maticove porty
# ---------------------------------------------------------------------------

MATRIX = {0xFE: "^zxcv", 0xFD: "asdfg", 0xFB: "qwert", 0xF7: "12345",
          0xEF: "09876", 0xDF: "poiuy", 0xBF: "\rlkjh", 0x7F: " $mnb"}   # ^ CAPS, $ SYM
CAPS, SYM = (0xFE, 0), (0x7F, 1)
CAPS_KEYS = {1: " ", 8: "5", 9: "8", 10: "6", 11: "7", 12: "0"}    # BREAK, sipky, DELETE
SYM_KEYS = {"/": "v", ".": "m", ",": "n", "-": "j", "_": "0", ":": "z", "?": "c"}


def matrix_key(char):
    for row, keys in MATRIX.items():
        if char in keys:
            return row, keys.index(char)
    raise KeyError(char)


def keys_for(key):
    """Mnozina stisknutych klaves pro znak nebo kod (1 BREAK, 8-12, 13 ENTER)."""
    if isinstance(key, int):
        if key == 13:
            return {matrix_key("\r")}
        return {CAPS, matrix_key(CAPS_KEYS[key])}
    if key in SYM_KEYS:
        return {SYM, matrix_key(SYM_KEYS[key])}
    if key.isupper():
        return {CAPS, matrix_key(key.lower())}
    return {matrix_key(key)}


class Typist:
    """Mackani klaves pro kod, ktery cte klavesnici sam (in a,(c) a halt).
    Dalsi klavesa se stiskne, az kdyz preruseni zastihne program uvnitr
    rutiny cteni klaves (waiting), takze se nic neztrati ani nezdvoji."""

    def __init__(self, m, keys, waiting, hold=4, gap=2):
        self.m, self.queue, self.waiting = m, list(keys), waiting
        self.hold, self.gap = hold, gap
        self.pressed, self.count, self.idle = False, 0, 0
        m.interrupts = True
        m.isr_checks.append(self.tick)

    def tick(self, m):
        self.count += 1
        if self.pressed:
            if self.count >= self.hold:
                m.keys, self.pressed, self.count = set(), False, 0
            return
        interrupted = m.word(m.cpu.sp)              # v ISR je PC = $0038
        inside = self.waiting[0] <= interrupted < self.waiting[1]
        if self.count >= self.gap and inside:
            if not self.queue:
                self.idle += 1
                if self.idle > 50:
                    raise AssertionError("program ceka na dalsi klavesu, ale zadna neni")
                return
            m.keys, self.pressed, self.count = keys_for(self.queue.pop(0)), True, 0


def keyscan(m):
    """Jako KEYSCAN v CC: (D, E) pro stisknute klavesy v m.keys.
    D = $27 CAPS SHIFT, $18 SYMBOL SHIFT, jinak $FF; E = index 0..38 do
    NORMTAB/CAPSTAB/SYMTAB, nebo $FF. Shift se jako klavesa nepocita."""
    keys = set(m.keys)
    d = 0x27 if CAPS in keys else 0x18 if SYM in keys else 0xFF
    base = 47
    for row in (0xFE, 0xFD, 0xFB, 0xF7, 0xEF, 0xDF, 0xBF, 0x7F):
        bits = [bit for r, bit in keys if r == row and (r, bit) not in (CAPS, SYM)]
        if bits:
            return d, base - 8 * (min(bits) + 1)
        base -= 1
    return d, 0xFF
