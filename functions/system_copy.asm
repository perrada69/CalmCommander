; ============================================================
; System copy plugin host.
; Loads c:/CalmCommander/plugin/syscopy.ccp and calls it with a
; tiny context. The plugin performs recursive directory copy.
; ============================================================

        include "plugin/syscopy_api.i.asm"

SYS_COPY_PLUGIN_BANK    equ 41
SYS_COPY_PLUGIN_PAGE    equ SYS_COPY_PLUGIN_BANK*2
SYS_COPY_WORK_PAGE      equ 99

system_copy_single_dir_from_entry
        call system_copy_dir_from_entry
        jr c,syscopy_error

        call obnov_okna
        call syscopy_restore_current_path
        jp loop0


system_copy_dir_from_entry
        push hl
        call FINDLFN
        call syscopy_prepare_context
        pop hl

        call syscopy_load_plugin
        ret c

        call syscopy_call_plugin
        ld a,(sysCopyContext+SYSCOPYCTX_RESULT)
        or a
        jr nz,.fail

        call syscopy_restore_current_path
        or a
        ret

.fail
        scf
        ret


system_copy_dir_from_index
        ld hl,(cislo_souboru+1)
        dec hl
        call FINDLFN
        call syscopy_is_dot_lfn
        jr z,.skip_dot
        call syscopy_prepare_context

        call syscopy_load_plugin
        ret c

        call syscopy_call_plugin
        ld a,(sysCopyContext+SYSCOPYCTX_RESULT)
        or a
        jr nz,.fail

        call syscopy_restore_current_path
        or a
        ret
.fail
        scf
        ret

.skip_dot
        xor a
        ret


system_delete_dir_from_index
        ld hl,(cislo_souboru+1)
        dec hl
        call FINDLFN

system_delete_dir_from_lfn
        call syscopy_prepare_context
        ld a,2
        ld (sysCopyContext+SYSCOPYCTX_MODE),a

        call syscopy_load_plugin
        ret c

        call syscopy_call_plugin
        ld a,(sysCopyContext+SYSCOPYCTX_RESULT)
        or a
        jr nz,.fail

        call syscopy_restore_current_path
        or a
        ret
.fail
        scf
        ret


syscopy_error
        call syscopy_show_error
        jp copyend


syscopy_prepare_context
        ld a,SYSCOPY_ABI
        ld (sysCopyContext+SYSCOPYCTX_ABI),a
        ld a,(ismove)
        ld (sysCopyContext+SYSCOPYCTX_MODE),a

        call syscopy_get_active_path
        ld (sysCopyContext+SYSCOPYCTX_SRC_PATH),hl
        call syscopy_get_other_path
        ld (sysCopyContext+SYSCOPYCTX_DST_PATH),hl

        call syscopy_make_83_name
        ld hl,sysCopyName
        ld (sysCopyContext+SYSCOPYCTX_NAME),hl
        call syscopy_make_lfn_name
        ld hl,LFNNAME
        ld (sysCopyContext+SYSCOPYCTX_LFN_NAME),hl

        xor a
        ld (sysCopyContext+SYSCOPYCTX_RESULT),a
        ld (sysCopyContext+SYSCOPYCTX_ERROR),a
        ld (sysCopyContext+SYSCOPYCTX_STAGE),a
        ld hl,0
        ld (sysCopyContext+SYSCOPYCTX_FILES_LO),hl
        ld (sysCopyContext+SYSCOPYCTX_FILES_HI),hl
        ld (sysCopyContext+SYSCOPYCTX_DIRS_LO),hl
        ld (sysCopyContext+SYSCOPYCTX_DIRS_HI),hl
        ld hl,sysCopyServices
        ld (sysCopyContext+SYSCOPYCTX_SERVICES),hl
        ret


syscopy_make_lfn_name
        ld hl,LFNNAME+259
.scan
        ld a,(hl)
        cp 32
        jr z,.trim
        cp 255
        jr z,.trim
        or a
        jr z,.trim
        inc hl
        xor a
        ld (hl),a
        ret
.trim
        dec hl
        jr .scan


syscopy_make_83_name
        ld hl,TMP83
        ld de,sysCopyName
        ld b,8
.name
        ld a,(hl)
        res 7,a
        cp 32
        jr z,.ext_test
        ld (de),a
        inc de
        inc hl
        djnz .name
        jr .ext_test_ready
.ext_test
        inc hl
        djnz .ext_test
.ext_test_ready
        ld hl,TMP83+8
        ld b,3
.ext_probe
        ld a,(hl)
        res 7,a
        cp 32
        jr nz,.has_ext
        inc hl
        djnz .ext_probe
        jr .done
.has_ext
        ld a,"."
        ld (de),a
        inc de
        ld hl,TMP83+8
        ld b,3
.ext
        ld a,(hl)
        res 7,a
        cp 32
        jr z,.done
        ld (de),a
        inc de
        inc hl
        djnz .ext
.done
        xor a
        ld (de),a
        ret


syscopy_is_dot_lfn
        ld hl,LFNNAME

syscopy_is_dot_name
        ld a,(hl)
        cp "."
        jr nz,.no
        inc hl
        ld a,(hl)
        cp 32
        jr z,.yes
        cp 255
        jr z,.yes
        cp "."
        jr nz,.no
        inc hl
        ld a,(hl)
        cp 32
        jr z,.yes
        cp 255
        jr z,.yes
.no
        ld a,1
        or a
        ret
.yes
        xor a
        ret


syscopy_get_active_path
        ld hl,pathl
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        ret


syscopy_get_other_path
        ld a,(OKNO)
        xor 16
        ld (OKNO),a
        call syscopy_get_active_path
        push hl
        ld a,(OKNO)
        xor 16
        ld (OKNO),a
        pop hl
        ret


syscopy_load_plugin
        call dospage

        ld a,"C"
        call $012d
        ld hl,sysCopyPluginDir
        xor a
        call $01b1

        ld b,0
        ld c,1
        ld e,2
syscopyLoadName
        ld hl,sysCopyPluginName
        call 0106h
        jr nc,syscopy_load_openerr

        ld b,0
        ld c,SYS_COPY_PLUGIN_BANK
syscopyLoadSize
        ld de,SYSCOPY_PLUGIN_SIZE
        ld hl,SYSCOPY_PLUGIN_ADDRESS
        call 0112h
        push af
        ld b,0
        call 0109h
        call syscopy_restore_current_path_dos
        call basicpage
        pop af
        xor a
        ret

syscopy_load_openerr
        call syscopy_restore_current_path_dos
        call basicpage
        scf
        ret


syscopy_restore_current_path
        call dospage
        call syscopy_restore_current_path_dos
        call basicpage
        ret


syscopy_restore_current_path_dos
        call set_active_panel_drive
        call syscopy_get_active_path
        xor a
        call $01b1
        ret


syscopy_call_plugin
        ld a,$56
        call ReadNextReg2A
        ld (sysCopySavedMmu6),a
        ld a,$57
        call ReadNextReg2A
        ld (sysCopySavedMmu7),a

        ld a,SYS_COPY_PLUGIN_PAGE
        nextreg $56,a
        ld a,SYS_COPY_WORK_PAGE
        nextreg $57,a

        ld hl,sysCopyContext
        ld de,sysCopyServices
        call SYSCOPY_PLUGIN_ADDRESS

        ld a,(sysCopySavedMmu6)
        nextreg $56,a
        ld a,(sysCopySavedMmu7)
        nextreg $57,a
        ret


syscopy_show_error
        call savescr
        ld hl,10 * 256 + 10
        ld bc,60 * 256 + 5
        ld a,16
        call window

        ld hl,11*256+11
        ld a,16
        ld de,sysCopyErrorTxt
        call print

        ld hl,11*256+13
        ld a,16
        ld de,sysCopyErrorHintTxt
        call print

        call syscopy_prepare_error_diag
        ld hl,11*256+14
        ld a,16
        ld de,sysCopyErrorDiagTxt
        call print

        ld hl,11*256+15
        ld a,48
        ld de,pressanykeytxt
        call print

        xor a
        ld (TLACITKO),a
        call INKEY
        ret


syscopy_prepare_error_diag
        ld a,(sysCopyContext+SYSCOPYCTX_STAGE)
        ld de,sysCopyErrorDiagTxt+7
        call syscopy_write_hex_byte
        ld a,(sysCopyContext+SYSCOPYCTX_ERROR)
        ld de,sysCopyErrorDiagTxt+18
        call syscopy_write_hex_byte
        ret


syscopy_write_hex_byte
        push af
        rrca
        rrca
        rrca
        rrca
        call syscopy_write_hex_nibble
        pop af
        inc de
        call syscopy_write_hex_nibble
        ret


syscopy_write_hex_nibble
        and $0f
        add a,"0"
        cp "9"+1
        jr c,.store
        add a,"A"-"9"-1
.store
        ld (de),a
        ret


sysCopyServices
        defw print

sysCopyContext  defs SYSCOPYCTX_SIZE
sysCopyName     defs 13
sysCopySavedMmu6 defb 0
sysCopySavedMmu7 defb 0

sysCopyPluginDir  defb "c:/CalmCommander/plugin",255
sysCopyPluginName defb "syscopy.ccp",255
sysCopyErrorTxt   defb "Directory copy failed.",0
sysCopyErrorHintTxt defb "Check syscopy.ccp and free space.",0
sysCopyErrorDiagTxt defb "Stage $00  Error $00",0
