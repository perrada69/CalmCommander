"""Zavadec dot commandu .cc (dot/ccdot.asm) na skutecnem Z80 kodu.

Zavadec bezi jako v DivMMC na $2000. Sluzby esxDOS/+3DOS a ROM jsou pasti
v Pythonu, CC samotne nahrazuje "falesne CC": pameť BASICu poniči, Next
registry prenastavi a vrati zvoleny vystupni kod. Testy tak overuji, ze
zavadec po sobe vsechno uklidi, at CC dela cokoli.
"""

import random
import unittest

from cctest import (Machine, ROM, DIV, RST18_RETURN, SENTINEL, build_dot, BUILD, hidden)


BASIC_PAGES = (10, 11, 4, 5, 0, 1, 6, 7)
# pevne stranky CC (mapa v cc.asm): LFN, Layer 2, pracovni, getdir, katalogy,
# savescr, data prohlizece, plugin, extra banka
CC_PAGES = (set(range(24, 71)) | set(range(72, 80)) | {81, 82, 83, 85, 87, 90})
USER_NEXTREGS = {0x07: 2, 0x14: 0xE3, 0x15: 0x01, 0x2F: 0, 0x30: 0, 0x31: 0,
                 0x4A: 0, 0x4B: 0xE3, 0x4C: 0x0F, 0x68: 0x00, 0x69: 0x00,
                 0x6B: 0x00, 0x6C: 0x00, 0x6E: 0x6C, 0x6F: 0x0C, 0x43: 0x00}

# systemove promenne
VARS, PROG, NXTLIN, E_LINE, K_CUR, CH_ADD = 0x5C4B, 0x5C53, 0x5C55, 0x5C59, 0x5C5B, 0x5C5D
WORKSP, STKBOT, STKEND, BORDCR = 0x5C61, 0x5C63, 0x5C65, 0x5C48
POINTERS = [0x5C4B + 2 * i for i in range(14)]     # VARS .. STKEND

TOKENS = {"PRINT": 0xF5, "LOAD": 0xEF, "CLEAR": 0xFD}


def basic_line(number, body):
    body = body + b"\r"
    return number.to_bytes(2, "big") + len(body).to_bytes(2, "little") + body


class DotEnv:
    """Prostredi jednoho spusteni .cc."""

    def __init__(self, test, *, program=b"", edit=b".cc", args=None, in_use=(),
                 total=224, alloc_limit=None, dot_file=None, dosversion_ok=True):
        self.cc, self.ld = build_dot()
        m = self.m = Machine(ram_pages=total)
        self.test = test
        rnd = random.Random(1234)
        # pamet BASICu: nahodny obsah, at je kazdy neobnoveny bajt videt
        for page in BASIC_PAGES:
            m.set_page(page, bytes(rnd.randrange(256) for _ in range(8192)))
        # BASIC: program od $5CCB, promenne, editacni radek
        prog = 0x5CCB
        vars_ = prog + len(program)
        e_line = vars_ + 1
        edit_line = edit + b"\r\x80"
        worksp = e_line + len(edit_line)
        m.write(prog, program + b"\x80" + edit_line)
        for addr, value in ((PROG, prog), (VARS, vars_), (E_LINE, e_line),
                            (NXTLIN, prog), (K_CUR, e_line), (CH_ADD, e_line),
                            (WORKSP, worksp), (STKBOT, worksp), (STKEND, worksp)):
            m.put(addr, value, 2)
        m.put(BORDCR, 0x38)
        if args is not None:
            m.write(0x5B00, args + b"\r")              # argumenty .cc (tiskarni buffer)
        self.layout = dict(prog=prog, vars=vars_, e_line=e_line, worksp=worksp)
        self.original = {page: bytes(m.page(page)) for page in BASIC_PAGES}
        m.nextregs.update(USER_NEXTREGS)
        # DivMMC: ROM s pastmi a RAM se zavadecem
        self.dot_file = dot_file if dot_file is not None else (BUILD / "cc").read_bytes()
        m.map(1, DIV)
        m.set_page(DIV, self.dot_file[:8192])
        self.file_pos = 8192
        # +3DOS/IDE_BANK
        self.total = total
        self.reserved = set(range(18)) | set(in_use)
        self.initial_reserved = set(self.reserved)
        self.alloc_limit = alloc_limit
        self.dosversion_ok = dosversion_ok
        self.printed = bytearray()
        self.p3dos_calls = []
        self.rst20 = None
        self.cc_runs = 0
        self.cc_behaviour = None
        m.trap(0x0008, self.rst8)
        m.trap(0x0010, self.rst10)
        m.trap(0x0018, self.rst18)
        m.trap(0x0020, self.rst20_trap)
        m.trap(RST18_RETURN, m.ret)
        self.edit_bc = e_line + 1                     # za teckou
        self.args = args

    # --- spusteni ----------------------------------------------------------

    def run(self, bc=None, hl=None):
        m = self.m
        args_addr = 0x5B00 if self.args is not None else 0
        m.cpu.alt_hl = 0x2758                        # H'L' BASICu
        m.cpu.ix = 0x1234
        self.entry_sp = 0xFF40
        m.call(self.ld["dot_start"], sp=self.entry_sp,
               hl=args_addr if hl is None else hl,
               bc=self.edit_bc if bc is None else bc)
        return m.cpu

    # --- sluzby ------------------------------------------------------------

    def rst8(self):
        m, cpu = self.m, self.m.cpu
        ret = m.word(cpu.sp)
        hook = m.mem[ret]
        m.put(cpu.sp, ret + 1, 2)
        if hook == 0x88:                                   # M_DOSVERSION
            if self.dosversion_ok:
                cpu.bc, cpu.de, cpu.a = 0x4E58, 0x0210, 0
                cpu.f = (cpu.f | 0x40) & ~1
            else:
                cpu.a = 14
                m.carry(True)
        elif hook == 0x8D:                                 # M_GETHANDLE
            cpu.a = 7
            m.carry(False)
        elif hook == 0x9D:                                 # F_READ
            count = cpu.bc
            data = self.dot_file[self.file_pos:self.file_pos + count]
            self.file_pos += len(data)
            m.write(cpu.hl, data)
            cpu.bc = cpu.de = len(data)
            cpu.hl = (cpu.hl + len(data)) & 0xFFFF
            m.carry(False)
        elif hook == 0x9B:                                 # F_CLOSE
            m.carry(False)
        elif hook == 0x94:                                 # M_P3DOS
            self.p3dos(cpu.de, cpu.alt_hl, cpu.alt_de)
        else:
            raise AssertionError(f"neznamy esxDOS hook ${hook:02X}")
        m.ret()

    def p3dos(self, call, hl, de):
        m, cpu = self.m, self.m.cpu
        self.test.assertEqual(call, 0x01BD, "M_P3DOS jen pro IDE_BANK")
        self.test.assertNotIn(cpu.sp // 8192, (1,), "M_P3DOS se zasobnikem v okne dotu")
        reason, page = hl & 0xFF, de & 0xFF
        self.p3dos_calls.append((reason, page))
        m.events.append(("p3dos", reason, page))
        ok, result = True, page
        if reason == 0:
            result = self.total
        elif reason == 1:
            free = [p for p in range(self.total - 1, -1, -1) if p not in self.reserved]
            allocated = len([r for r, _ in self.p3dos_calls if r == 1])
            if not free or (self.alloc_limit is not None and allocated > self.alloc_limit):
                ok = False
            else:
                result = free[0]
                self.reserved.add(result)
        elif reason == 2:
            ok = page < self.total and page not in self.reserved
            if ok:
                self.reserved.add(page)
        elif reason == 3:
            ok = page in self.reserved and page not in self.initial_reserved
            self.reserved.discard(page)
        cpu.e = result
        m.carry(ok)                                         # +3DOS: Fc=1 = uspech

    def rst10(self):
        self.printed.append(self.m.cpu.a)
        self.m.ret()

    def rst18(self):
        m, cpu = self.m, self.m.cpu
        ret = m.word(cpu.sp)
        target = m.word(ret)
        if target == self.cc["dot_entry"]:
            self.test.assertGreaterEqual(cpu.sp, 0xE000, "zasobnik RST $18 musi lezet ve strance 1")
        cpu.sp = (cpu.sp + 2) & 0xFFFF
        cpu.pc = ret + 2
        if target == self.cc["dot_entry"]:
            self.fake_cc()
        elif target == 0x1655:
            self.make_room()
        else:
            raise AssertionError(f"RST $18 na ${target:04X}")

    def rst20_trap(self):
        m = self.m
        self.rst20 = (m.cpu.hl, m.cpu.sp)
        m.events.append(("rst20",))
        m.cpu.pc = SENTINEL

    def make_room(self):
        """ROM3 MAKE-ROOM ($1655) vcetne POINTERS: misto o BC bajtech pred HL."""
        m, cpu = self.m, self.m.cpu
        at, size = cpu.hl, cpu.bc
        old_end = m.word(STKEND)
        for addr in POINTERS:
            value = m.word(addr)
            if value > at:
                m.put(addr, value + size, 2)
        m.write(at + size, bytes(m.mem[at:old_end + 1]))
        cpu.hl, cpu.de = at - 1, at + size - 1

    # --- falesne CC --------------------------------------------------------

    def fake_cc(self):
        """Co by udelalo CC: prepise pamet BASICu, zmeni displej a vrati kod."""
        m = self.m
        self.cc_runs += 1
        self.test.assertEqual(m.mmu[2:8], [10, 11, 4, 5, 0, 1], "CC se vola se standardnim MMU")
        for page in (10, 11, 4, 5, 0, 6, 7):
            m.set_page(page, b"\xAA" * 8192)
        for reg in USER_NEXTREGS:
            m.nextreg(reg, 0x5A)
        m.map(7, 90)                                       # CC necha v MMU7 jinou stranku
        m.map(7, 1)
        code, payload = self.cc_behaviour or (0, None)
        m.put(self.cc["dot_exit_code"], code)
        if payload is not None:
            m.write(self.cc["dot_cmd_buf"], payload)


def assert_basic_restored(test, env):
    """Pamet BASICu bajt po bajtu jako pred .cc. Vyjimka: par bajtu tesne
    pod zasobnikem BASICu - pamet pod SP je volna a pouziva ji navrat
    i volani M_P3DOS pri uvolnovani stranek."""
    for page in BASIC_PAGES:
        now, before = bytearray(env.m.page(page)), bytearray(env.original[page])
        if page == 1:
            lo, hi = env.entry_sp - 64 - 0xE000, env.entry_sp - 0xE000
            now[lo:hi] = before[lo:hi]
        test.assertEqual(bytes(now), bytes(before), f"stranka {page} neobnovena")


class DotLoaderQuit(unittest.TestCase):
    def test_quit_restores_basic_memory_registers_and_frees_pages(self):
        env = DotEnv(self)
        cpu = env.run()
        m = env.m
        self.assertEqual(env.cc_runs, 1)
        self.assertFalse(cpu.f & 1, "Quit konci bez chyby")
        assert_basic_restored(self, env)
        for reg, value in USER_NEXTREGS.items():
            self.assertEqual(m.nextregs[reg], value, f"NextReg ${reg:02X} neobnoven")
        self.assertEqual(m.mmu[2:8], [10, 11, 4, 5, 0, 1])
        self.assertEqual(env.reserved, env.initial_reserved, "vsechny stranky uvolnene")
        self.assertEqual(m.border, 7, "border podle BORDCR")
        self.assertEqual(cpu.sp, env.entry_sp, "navrat na zasobnik BASICu")
        self.assertEqual(cpu.alt_hl, 0x2758, "H'L' BASICu zustal")
        self.assertEqual(cpu.ix, 0x1234, "IX zustal")

    def test_reserves_all_fixed_cc_pages(self):
        env = DotEnv(self)
        env.run()
        reserved = {page for reason, page in env.p3dos_calls if reason == 2}
        self.assertEqual(reserved, CC_PAGES)
        allocated = [r for r, _ in env.p3dos_calls if r == 1]
        self.assertEqual(len(allocated), 8, "8 stranek na zalohu")

    def test_runs_on_1mb_next(self):
        """1MB Next ma stranky 0-95: CC se tam musi vejit i se zalohou."""
        env = DotEnv(self, total=96)
        env.run()
        self.assertEqual(env.cc_runs, 1)
        reserved = {page for reason, page in env.p3dos_calls if reason == 2}
        self.assertEqual(reserved, CC_PAGES)
        start = env.ld["backupPages"] - 0x2000                # zavadec je v DivMMC na $2000
        backup = env.m.page(DIV)[start:start + 8]
        self.assertTrue(all(page < 96 for page in backup), list(backup))
        self.assertFalse(set(backup) & CC_PAGES, "zaloha mimo stranky CC")
        self.assertEqual(env.reserved, env.initial_reserved, "vsechny stranky uvolnene")

    def test_1mb_next_with_top_pages_taken(self):
        """NextZXOS prideluje shora: par obsazenych hornich stranek CC nevadi."""
        env = DotEnv(self, total=96, in_use=(95, 94, 93))
        env.run()
        self.assertEqual(env.cc_runs, 1)
        self.assertEqual(env.reserved, env.initial_reserved)

    def test_no_7ffd_write_after_last_m_p3dos(self):
        """Zapis $7FFD po poslednim M_P3DOS zasekl NextZXOS po navratu z dotu."""
        env = DotEnv(self)
        env.run()
        events = env.m.events
        last_p3dos = max(i for i, e in enumerate(events) if e[0] == "p3dos")
        self.assertEqual(env.m.writes_to(0x7FFD), [], "zavadec nema na $7FFD sahat vubec")
        self.assertTrue(any(e[0] == "p3dos" and e[1] == 3 for e in events[last_p3dos:]))

    def test_cc_left_with_lfn_page_in_mmu7_still_returns(self):
        env = DotEnv(self)
        env.run()
        self.assertEqual(env.m.cpu.pc, SENTINEL)


class DotLoaderErrors(unittest.TestCase):
    def message(self, env):
        cpu = env.m.cpu
        self.assertTrue(cpu.f & 1, "chyba = Fc=1")
        self.assertEqual(cpu.a, 0, "vlastni chybova hlaska")
        text = bytearray()
        addr = cpu.hl
        while True:
            c = env.m.mem[addr]
            text.append(c & 0x7F)
            if c & 0x80:
                return text.decode()
            addr += 1

    def test_page_in_use_refuses_and_frees_everything(self):
        env = DotEnv(self, in_use=(87,))
        env.run()
        self.assertEqual(self.message(env), "Page 087 in use")
        self.assertEqual(env.cc_runs, 0)
        self.assertEqual(env.reserved, env.initial_reserved)

    def test_missing_page_refuses(self):
        """Stroj bez nektere stranky CC: radsi chyba nez zapis do prazdna."""
        env = DotEnv(self, total=80)
        env.run()
        self.assertEqual(self.message(env), "Not enough memory")
        self.assertEqual(env.cc_runs, 0)
        self.assertEqual(env.reserved, env.initial_reserved)

    def test_not_enough_memory_for_backup(self):
        env = DotEnv(self, alloc_limit=3)
        env.run()
        self.assertEqual(self.message(env), "Not enough memory")
        self.assertEqual(env.reserved, env.initial_reserved)

    def test_requires_nextzxos(self):
        env = DotEnv(self, dosversion_ok=False)
        env.run()
        self.assertEqual(self.message(env), "Requires NextZXOS")
        self.assertEqual(env.p3dos_calls, [])

    def test_truncated_file_restores_memory(self):
        env = DotEnv(self)
        env.dot_file = env.dot_file[:9000]
        env.run()
        self.assertEqual(self.message(env), "CC load error")
        self.assertEqual(env.cc_runs, 0)
        assert_basic_restored(self, env)
        self.assertEqual(env.reserved, env.initial_reserved)

    def test_argument_prints_usage(self):
        env = DotEnv(self, args=b"-h")
        cpu = env.run()
        self.assertFalse(cpu.f & 1)
        self.assertIn(b"Usage: .cc", bytes(env.printed))
        self.assertEqual(env.p3dos_calls, [])
        self.assertEqual(env.cc_runs, 0)


class DotLoaderLaunch(unittest.TestCase):
    def test_nex_launch_frees_pages_and_jumps_on_basic_stack(self):
        env = DotEnv(self)
        env.cc_behaviour = (env.cc["DOT_EXIT_LAUNCH"], None)
        env.run()
        self.assertIsNotNone(env.rst20, "RST $20")
        hl, sp = env.rst20
        self.assertEqual(hl, env.cc["dot_launch"])
        # entrySp = entry_sp - 2 (navrat testu), RST $20 na nej ulozi svuj navrat
        self.assertEqual(sp, env.entry_sp - 4, "spousteni na zasobniku BASICu")
        self.assertEqual(env.reserved, env.initial_reserved)

    def command(self, env, flags, body):
        return bytes([len(body), flags]) + body

    def test_direct_command_gets_start_command_after_cc(self):
        env = DotEnv(self, edit=b".cc")
        inject = b":" + bytes([TOKENS["LOAD"]]) + b'"game.bas"'
        env.cc_behaviour = (env.cc["DOT_EXIT_BASIC"], self.command(env, 0, inject))
        cpu = env.run()
        m = env.m
        self.assertFalse(cpu.f & 1)
        e_line = m.word(E_LINE)
        self.assertEqual(m.cstring(e_line, (0x0D,)), b".cc" + inject)
        self.assertEqual(m.word(WORKSP), env.layout["worksp"] + len(inject))
        self.assertEqual(m.word(STKEND), env.layout["worksp"] + len(inject))
        self.assertEqual(m.word(VARS), env.layout["vars"], "program se nemenil")
        self.assertEqual(m.nextregs[0x07], USER_NEXTREGS[0x07], "BAS: rychlost uzivatele")

    def test_speed_flag_sets_3_5_mhz(self):
        env = DotEnv(self)
        env.cc_behaviour = (env.cc["DOT_EXIT_BASIC"], self.command(env, 1, b':\xa3"x.snx"'))
        env.run()
        self.assertEqual(env.m.nextregs[0x07], 0)

    def test_inserted_before_following_statements(self):
        env = DotEnv(self, edit=b'.cc "a:b":' + bytes([TOKENS["PRINT"]]) + b"1" + hidden(1))
        inject = b":" + bytes([TOKENS["LOAD"]]) + b'"x"'
        env.cc_behaviour = (env.cc["DOT_EXIT_BASIC"], self.command(env, 0, inject))
        env.run()
        m = env.m
        line = m.cstring(m.word(E_LINE), (0x0D,))
        self.assertEqual(line, b'.cc "a:b"' + inject + b":" + bytes([TOKENS["PRINT"]]) + b"1" + hidden(1))

    def test_program_line_is_extended_and_length_fixed(self):
        line10 = basic_line(10, b".cc")
        line20 = basic_line(20, bytes([TOKENS["PRINT"]]) + b"2" + hidden(2))
        env = DotEnv(self, program=line10 + line20, edit=bytes([0xF7]))   # RUN
        inject = b":" + bytes([TOKENS["LOAD"]]) + b'"g.bas"'
        env.cc_behaviour = (env.cc["DOT_EXIT_BASIC"], self.command(env, 0, inject))
        env.run(bc=env.layout["prog"] + 4 + 1)                        # ".cc" v radku 10, za teckou
        m = env.m
        prog = env.layout["prog"]
        expected = basic_line(10, b".cc" + inject) + line20
        self.assertEqual(bytes(m.mem[prog:prog + len(expected)]), expected)
        self.assertEqual(m.word(VARS), env.layout["vars"] + len(inject))
        self.assertEqual(m.word(E_LINE), env.layout["e_line"] + len(inject))

    def test_skips_hidden_numbers_when_looking_for_end(self):
        # skryte cislo obsahuje bajt ':' ($3A) - nesmi se brat jako konec prikazu
        body = b".cc " + b"58" + bytes([0x0E, 0, 0, 0x3A, 0, 0])
        env = DotEnv(self, edit=body)
        inject = b":" + bytes([TOKENS["CLEAR"]])
        env.cc_behaviour = (env.cc["DOT_EXIT_BASIC"], self.command(env, 0, inject))
        env.run()
        m = env.m
        line = bytes(m.mem[m.word(E_LINE):m.word(E_LINE) + len(body) + len(inject)])
        self.assertEqual(line, body + inject)

    def test_command_outside_basic_reports_error(self):
        env = DotEnv(self)
        env.cc_behaviour = (env.cc["DOT_EXIT_BASIC"], self.command(env, 0, b":\xfd"))
        env.run(bc=0x4000)                                           # mimo program i E_LINE
        self.assertTrue(env.m.cpu.f & 1)
        self.assertEqual(env.reserved, env.initial_reserved)


if __name__ == "__main__":
    unittest.main()
