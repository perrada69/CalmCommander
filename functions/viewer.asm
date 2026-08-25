; ============================================================
; Modular file viewer loader.
; CC only prepares a context, loads data and loads an external
; plugin from plugin/*.ccp. The plugin runs at $C000 and sees
; file data at $E000.
; ============================================================

VIEW_DATA_BANK       equ 40
VIEW_PLUGIN_BANK     equ 41
VIEW_DATA_PAGE       equ VIEW_DATA_BANK*2+1
VIEW_PLUGIN_PAGE     equ VIEW_PLUGIN_BANK*2
VIEW_PLUGIN_ABI      equ 1
VIEW_PLUGIN_ADDRESS  equ 49152
VIEW_DATA_ADDRESS    equ 57344
VIEW_TEXT_MAX_READ   equ 8192
VIEW_PLUGIN_MAX_SIZE equ 4096
; The plugin page is 8K, so a plugin that cannot live inside 4096 bytes may
; ask for the whole of it. The ZIP viewer does: an inflate engine will not
; fit next to the archive listing.
VIEW_PLUGIN_BIG_SIZE equ 8192
VIEW_DATA_MAX_PAGES  equ 8

; PluginContext offsets
VIEWCTX_ABI          equ 0
VIEWCTX_TYPE         equ 1
VIEWCTX_FILENAME     equ 2
VIEWCTX_SIZE_LO      equ 4
VIEWCTX_SIZE_HI      equ 6
VIEWCTX_DATA_PAGE    equ 8
VIEWCTX_DATA_ADDR    equ 9
VIEWCTX_READ_LEN     equ 11
VIEWCTX_PAGE_COUNT   equ 13
VIEWCTX_DATA_PAGES   equ 14
VIEWCTX_SERVICES     equ 16
VIEWCTX_CURPATH      equ 18   ; pointer to active panel path string
VIEWCTX_EXTRACT_FLAG equ 20
VIEWCTX_EXTRACT_OFF  equ 21
VIEWCTX_EXTRACT_CNT  equ 23
VIEWCTX_EXTRACT_NAME equ 25   ; 16 bytes, 0/255 terminated
VIEWCTX_DIRTY        equ 40
VIEWCTX_P3DOS_TYPE   equ 41    ; 1 byte: $FF=no header, else TAP type (0=BASIC,1=NumArr,2=StrArr,3=Code)
VIEWCTX_P3DOS_P1     equ 42    ; 2 bytes: TAP param1
VIEWCTX_P3DOS_P2     equ 44    ; 2 bytes: TAP param2
VIEWCTX_EXTRACT_OFHI equ 46    ; 2 bytes: bits 16-31 of the source offset
                               ; (SERVICE_EXTRACT_SEEK only; bits 0-15 come in DE)
VIEWCTX_SIZE         equ 48



VIEWTYPE_TEXT        equ 1
VIEWTYPE_ZXSCREEN    equ 2
VIEWTYPE_PT3         equ 3
VIEWTYPE_PT2         equ 4
VIEWTYPE_STC         equ 5
VIEWTYPE_STP         equ 6
VIEWTYPE_SQT         equ 7
VIEWTYPE_NXI         equ 8
VIEWTYPE_HELLO       equ 9
VIEWTYPE_BAS         equ 10
VIEWTYPE_TAP         equ 11
VIEWTYPE_EDIT        equ 12
VIEWTYPE_TRD         equ 13
VIEWTYPE_ZIP         equ 14
VIEW_EDIT_KEY_SAVE   equ 128
VIEW_EDIT_KEY_SAVEAS equ 129
VIEW_EDIT_KEY_HEX    equ 130
VIEW_EDIT_KEY_TEXT   equ 131
VIEW_EDIT_KEY_FIND   equ 132

view_file
        call view_prepare_current_file
        jp c,view_no_viewer_or_skip

        call view_make_cmd2
        call view_select_plugin
        jr nc,view_run_selected_plugin
        call view_choose_plugin_dialog
        jp c,loop0

view_run_selected_plugin

        call view_load_data_blocks
        jp c,view_file_error

        call view_load_plugin
        jp c,view_plugin_error

        call view_fill_context
        ld a,(OKNO)
        ld (viewSavedOKNO),a
        call savescr
        call view_init_plugin_input
        call view_call_plugin
        ld a,(viewPluginType)
        cp VIEWTYPE_ZXSCREEN
        jr z,.full_restore
        cp VIEWTYPE_NXI
        jr z,.full_restore
        cp VIEWTYPE_HELLO
        jr z,.full_restore
        call view_restore_saved_screen
        ld a,(viewPluginType)
        cp VIEWTYPE_PT3
        jr z,.music_result
        cp VIEWTYPE_PT2
        jr z,.music_result
        cp VIEWTYPE_STC
        jr z,.music_result
        cp VIEWTYPE_STP
        jr z,.music_result
        cp VIEWTYPE_SQT
        jr z,.music_result
        cp VIEWTYPE_TAP
        jr z,.extract_capable
        cp VIEWTYPE_TRD
        jr z,.extract_capable
        cp VIEWTYPE_ZIP
        jr nz,.done
.extract_capable
        call view_handle_plugin_extract
        ld a,(viewPluginContext+VIEWCTX_DIRTY)
        or a
        jr z,.done
        call view_reload_active_panel
        jp loop0
.music_result
        ld a,(viewPluginResult)
        cp 1
        jr nz,.done
        ld (viewNextAfterDown),a
        jp down
.done
        ld a,(viewPluginContext+VIEWCTX_DIRTY)
        or a
        jr z,.done_no_reload
        call view_reload_active_panel
.done_no_reload
        xor a
        ld (viewNextAfterDown),a
        jp loop0
.full_restore
        ld a,(viewPluginResult)
        cp 1
        jr nz,.full_restore_done
        ld (viewNextAfterDown),a
        call view_restore_full_ui
        jp down
.full_restore_done
        xor a
        ld (viewNextAfterDown),a
        call view_restore_full_ui
        jp loop0


view_plugin_menu
        call view_prepare_current_file
        jp c,view_no_viewer_or_skip

        call view_make_cmd2
        call view_choose_plugin_dialog
        jp c,loop0
        jp view_run_selected_plugin


edit_file
        call view_prepare_current_file
        jp c,view_no_viewer_or_skip

        call view_make_cmd2
        ld a,VIEWTYPE_EDIT
        ld (viewPluginType),a
        ld hl,viewEditPluginName
        ld (viewPluginName),hl
        jp view_run_selected_plugin


; Prepare TMP83/LFNNAME for the current cursor item.
; Carry set means unsupported item (directory or open error).
view_prepare_current_file
        ld hl,POSKURZL
        call ROZHOD
        ld a,(hl)
        ld l,a
        ld h,0

        push hl
        ld hl,STARTWINL
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        ex de,hl
        pop hl
        add hl,de
        push hl
        inc hl
        call find83
        pop hl
        call FINDLFN
        call syscopy_is_dot_lfn
        jr z,.unsupported

        ld ix,TMP83
        bit 7,(ix+7)
        jr nz,.unsupported

        ld b,11
        ld hl,TMP83
.clear83
        res 7,(hl)
        inc hl
        djnz .clear83

        ld hl,TMP83+10
.trim83
        ld a,(hl)
        cp 32
        jr nz,.end83
        dec hl
        jr .trim83
.end83
        ld a,255
        inc hl
        ld (hl),a

        ld hl,LFNNAME+254
.trimlfn
        ld a,(hl)
        cp 32
        jr nz,.endlfn
        dec hl
        jr .trimlfn
.endlfn
        xor a
        inc hl
        ld (hl),a

        ld hl,(LFNNAME+261)
        ld (viewFileSizeLo),hl
        ld hl,(LFNNAME+263)
        ld (viewFileSizeHi),hl

        xor a
        ret

.unsupported
        scf
        ret


view_make_cmd2
        ld hl,cmd2
        ld de,cmd2+1
        ld bc,99
        xor a
        ld (hl),a
        ldir

        ld hl,LFNNAME
        ld de,cmd2
        ld bc,99
.copy
        ld a,(hl)
        cp 255
        jr z,.done
        or a
        jr z,.done
        ld (de),a
        inc hl
        inc de
        dec bc
        ld a,b
        or c
        jr nz,.copy
.done
        xor a
        ld (de),a
        ret


view_make_short_name
        ld hl,viewShortName
        ld de,viewShortName+1
        ld bc,12
        xor a
        ld (hl),a
        ldir

        ld de,viewShortName
        ld hl,TMP83
        ld b,8
.copy_name
        ld a,(hl)
        cp 32
        jr z,.name_done
        ld (de),a
        inc hl
        inc de
        djnz .copy_name
.name_done
        ld hl,TMP83+8
        ld a,(hl)
        cp 32
        jr z,.done
        ld a,"."
        ld (de),a
        inc de
        ld b,3
.copy_ext
        ld a,(hl)
        cp 32
        jr z,.done
        ld (de),a
        inc hl
        inc de
        djnz .copy_ext
.done
        xor a
        ld (de),a
        ret


; ================================================================
; view_select_plugin: pick the viewer plugin for the file under the
; cursor from its extension. Carry set = nothing here can show it.
;
; viewExtTable is scanned in order, so an extension that has to win over
; a later one must come first. NXI is tested before the screen size
; check, because a 6912 byte .nxi would otherwise look like a SCR dump.
; ================================================================
view_select_plugin
        call view_make_short_name

        ld hl,viewShortName
        ld de,ext_nxi
        call pripony
        jr z,.nxi
        ld hl,viewShortName
        ld de,ext_NXI
        call pripony
        jr nz,.not_nxi
.nxi
        ld a,VIEWTYPE_NXI
        ld hl,viewNxiPluginName
        jr .store

.not_nxi
        call view_is_zx_screen
        jr nz,.scan
        ld a,VIEWTYPE_ZXSCREEN
        ld hl,viewZxScreenPluginName
        jr .store

.scan
        ld ix,viewExtTable
.loop
        ld e,(ix+0)
        ld d,(ix+1)
        ld a,d
        or e
        scf
        ret z                       ; end of table: no viewer for this one
        ld hl,viewShortName
        call pripony
        jr z,.hit
        ld bc,VIEW_EXT_ENTRY
        add ix,bc
        jr .loop
.hit
        ld a,(ix+2)
        ld l,(ix+3)
        ld h,(ix+4)
.store
        ld (viewPluginType),a
        ld (viewPluginName),hl
        xor a
        ret


; One row per extension: the ".xyz" text, the plugin type and the plugin
; file name. Upper and lower case need rows of their own because pripony
; compares case sensitively.
VIEW_EXT_ENTRY equ 5

viewExtTable
        defw ext_stc : defb VIEWTYPE_STC : defw viewStcPluginName
        defw ext_STC : defb VIEWTYPE_STC : defw viewStcPluginName
        defw ext_stp : defb VIEWTYPE_STP : defw viewStpPluginName
        defw ext_STP : defb VIEWTYPE_STP : defw viewStpPluginName
        defw ext_sqt : defb VIEWTYPE_SQT : defw viewSqtPluginName
        defw ext_SQT : defb VIEWTYPE_SQT : defw viewSqtPluginName
        defw ext_pt2 : defb VIEWTYPE_PT2 : defw viewPt2PluginName
        defw ext_PT2 : defb VIEWTYPE_PT2 : defw viewPt2PluginName
        defw ext_pt3 : defb VIEWTYPE_PT3 : defw viewPt3PluginName
        defw ext_PT3 : defb VIEWTYPE_PT3 : defw viewPt3PluginName
        defw ext_txt : defb VIEWTYPE_TEXT : defw viewTextPluginName
        defw ext_TXT : defb VIEWTYPE_TEXT : defw viewTextPluginName
        defw ext_asm : defb VIEWTYPE_TEXT : defw viewTextPluginName
        defw ext_ASM : defb VIEWTYPE_TEXT : defw viewTextPluginName
        defw ext_bas : defb VIEWTYPE_BAS : defw viewBasPluginName
        defw ext_BAS : defb VIEWTYPE_BAS : defw viewBasPluginName
        defw ext_cfg : defb VIEWTYPE_TEXT : defw viewTextPluginName
        defw ext_CFG : defb VIEWTYPE_TEXT : defw viewTextPluginName
        defw ext_ini : defb VIEWTYPE_TEXT : defw viewTextPluginName
        defw ext_INI : defb VIEWTYPE_TEXT : defw viewTextPluginName
        defw ext_tap : defb VIEWTYPE_TAP : defw viewTapPluginName
        defw ext_TAP : defb VIEWTYPE_TAP : defw viewTapPluginName
        ; SCL archives are handled by the TRD plugin, which tells the two
        ; formats apart by their signature rather than by the extension
        defw ext_trd : defb VIEWTYPE_TRD : defw viewTrdPluginName
        defw ext_TRD : defb VIEWTYPE_TRD : defw viewTrdPluginName
        defw ext_scl : defb VIEWTYPE_TRD : defw viewTrdPluginName
        defw ext_SCL : defb VIEWTYPE_TRD : defw viewTrdPluginName
        defw ext_zip : defb VIEWTYPE_ZIP : defw viewZipPluginName
        defw ext_ZIP : defb VIEWTYPE_ZIP : defw viewZipPluginName
        defw 0


view_is_zx_screen
        ld hl,(viewFileSizeHi)
        ld a,h
        or l
        ret nz
        ld hl,(viewFileSizeLo)
        ld de,6912
        or a
        sbc hl,de
        ret


view_plugin_menu_print_items
        ld a,11
        ld (viewPluginMenuPrintRow),a
        ld b,VIEW_PLUGIN_MENU_VISIBLE
.clear
        push bc
        ld h,27
        ld a,(viewPluginMenuPrintRow)
        ld l,a
        ld a,144
        ld de,viewPluginMenuBlankTxt
        call print
        ld hl,viewPluginMenuPrintRow
        inc (hl)
        pop bc
        djnz .clear

        xor a
.print
        cp VIEW_PLUGIN_MENU_VISIBLE
        ret nc
        push af
        call view_plugin_menu_print_visible_row
        pop af
        inc a
        jr .print


view_plugin_menu_print_visible_row
        ld c,a
        ld a,(viewPluginMenuTop)
        add a,c
        cp VIEW_PLUGIN_MENU_COUNT
        ret nc
        call view_plugin_menu_get_entry
        inc hl
        inc hl
        inc hl
        ld e,(hl)
        inc hl
        ld d,(hl)
        ld a,c
        add a,11
        ld h,27
        ld l,a
        ld a,144
        call print
        ret


view_plugin_menu_write_cursor
        ld (viewPluginMenuColor+1),a
        ld a,(viewPluginMenuCursor)
        add a,11
        ld e,a
        ld d,160
        mul d,e
        ld hl,$4000+27*2+1
        add hl,de
viewPluginMenuColor
        ld a,144
        ld b,28
.loop
        ld (hl),a
        inc hl
        inc hl
        djnz .loop
        ret


view_plugin_menu_set_plugin
        ld a,(viewPluginMenuCursor)
        ld b,a
        ld a,(viewPluginMenuTop)
        add a,b
        call view_plugin_menu_get_entry
        ld a,(hl)
        ld (viewPluginType),a
        inc hl
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        ld (viewPluginName),hl
        ret


view_plugin_menu_get_entry
        ld e,a
        ld d,5
        mul d,e
        ld hl,viewPluginMenuTable
        add hl,de
        ret


view_load_data_blocks
        call dospage

        ld a,1
        ld (viewErrorStage),a
        call view_set_current_path

        ld a,2
        ld (viewErrorStage),a
        ld b,0
        ld c,1
        ld e,2
        ld hl,TMP83
        call 0106h

        ld hl,(viewFileSizeLo)
        ld (viewRemainingLo),hl
        ld hl,(viewFileSizeHi)
        ld (viewRemainingHi),hl
        xor a
        ld (viewDataPageCount),a
        ld hl,0
        ld (viewFirstReadLen),hl

.read_loop
        ld a,3
        ld (viewErrorStage),a
        ld a,(viewDataPageCount)
        cp VIEW_DATA_MAX_PAGES
        jr z,.close_ok

        ld hl,(viewRemainingLo)
        ld de,(viewRemainingHi)
        ld a,h
        or l
        or d
        or e
        jr z,.close_ok

        call view_set_next_read_len
        ld a,(viewDataPageCount)
        ld hl,viewDataBanks
        add a,l
        ld l,a
        jr nc,.page_ok
        inc h
.page_ok
        ld c,(hl)
        ld b,0
        ld de,(viewReadLen)
        ld hl,VIEW_DATA_ADDRESS
        call 0112h

        ld a,(viewDataPageCount)
        or a
        jr nz,.not_first_page
        ld hl,(viewReadLen)
        ld (viewFirstReadLen),hl
.not_first_page

        ld hl,(viewRemainingLo)
        ld de,(viewRemainingHi)
        ld bc,(viewReadLen)
        call sub32
        ld (viewRemainingLo),hl
        ld (viewRemainingHi),de

        ld a,(viewDataPageCount)
        inc a
        ld (viewDataPageCount),a
        jr .read_loop

.close_ok
        ld b,0
        call 0109h
        call basicpage

        xor a
        ret

.openerr
        call basicpage
.readerr
        scf
        ret


view_load_plugin
        call dospage

        ld a,4
        ld (viewErrorStage),a
        ld a,"C"
        call $012d
        ld hl,viewPluginDir
        xor a
        call $01b1

        ld a,5
        ld (viewErrorStage),a
        ld b,0
        ld c,1
        ld d,0                  ; create action 0: fail if it is not there
        ld e,2
        ld hl,(viewPluginName)
        call 0106h
        jr nc,.openerr          ; DOS reports success with carry set

        ld a,6
        ld (viewErrorStage),a
        ld b,0
        ld c,VIEW_PLUGIN_BANK
        ld de,VIEW_PLUGIN_MAX_SIZE
        ld a,(viewPluginType)
        cp VIEWTYPE_ZIP
        jr nz,.size_ok
        ld de,VIEW_PLUGIN_BIG_SIZE
.size_ok
        ld hl,VIEW_PLUGIN_ADDRESS
        call 0112h
        push af
        ld b,0
        call 0109h
        call view_restore_current_path
        call basicpage
        pop af
        jr nc,.readerr          ; a short read means a truncated plugin

        xor a
        ret

.openerr
        call view_restore_current_path
        call basicpage
.readerr
        scf
        ret


view_restore_current_path
        call view_set_current_path
        ret


view_set_current_path
        call set_active_panel_drive
        ld hl,pathl
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        xor a
        call $01b1
        ret


view_set_next_read_len
        ld hl,(viewRemainingHi)
        ld a,h
        or l
        jr nz,.max
        ld hl,(viewRemainingLo)
        ld de,VIEW_TEXT_MAX_READ
        or a
        sbc hl,de
        jr nc,.max
        add hl,de
        ld (viewReadLen),hl
        ret
.max
        ld hl,VIEW_TEXT_MAX_READ
        ld (viewReadLen),hl
        ret


view_fill_context
        ld a,VIEW_PLUGIN_ABI
        ld (viewPluginContext+VIEWCTX_ABI),a
        ld a,(viewPluginType)
        ld (viewPluginContext+VIEWCTX_TYPE),a
        ld hl,cmd2
        ld (viewPluginContext+VIEWCTX_FILENAME),hl
        ld hl,(viewFileSizeLo)
        ld (viewPluginContext+VIEWCTX_SIZE_LO),hl
        ld hl,(viewFileSizeHi)
        ld (viewPluginContext+VIEWCTX_SIZE_HI),hl
        ld a,VIEW_DATA_PAGE
        ld (viewPluginContext+VIEWCTX_DATA_PAGE),a
        ld hl,VIEW_DATA_ADDRESS
        ld (viewPluginContext+VIEWCTX_DATA_ADDR),hl
        ld hl,(viewFirstReadLen)
        ld (viewPluginContext+VIEWCTX_READ_LEN),hl
        ld a,(viewDataPageCount)
        ld (viewPluginContext+VIEWCTX_PAGE_COUNT),a
        ld hl,viewDataPages
        ld (viewPluginContext+VIEWCTX_DATA_PAGES),hl
        ld hl,viewServices
        ld (viewPluginContext+VIEWCTX_SERVICES),hl
        ; store pointer to active panel path string
        ld hl,pathl
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a              ; HL = active panel path string address
        ld (viewPluginContext+VIEWCTX_CURPATH),hl
        xor a
        ld (viewPluginContext+VIEWCTX_EXTRACT_FLAG),a
        ld (viewPluginContext+VIEWCTX_DIRTY),a
        ld a,$FF
        ld (viewPluginContext+VIEWCTX_P3DOS_TYPE),a
        ret


view_call_plugin
        ld a,$52
        call ReadNextReg2A
        ld (viewSavedMmu2),a
        ld a,$56
        call ReadNextReg2A
        ld (viewSavedMmu6),a
        ld a,$57
        call ReadNextReg2A
        ld (viewSavedMmu7),a

        ld a,VIEW_PLUGIN_PAGE
        nextreg $56,a
        ld a,VIEW_DATA_PAGE
        nextreg $57,a

        ld a,VIEW_PLUGIN_ABI
        ld hl,viewPluginContext
        ld de,viewServices
        call VIEW_PLUGIN_ADDRESS
        ld (viewPluginResult),a

        ld a,(viewSavedMmu6)
        nextreg $56,a
        ld a,(viewSavedMmu7)
        nextreg $57,a
        ld a,(viewSavedMmu2)
        nextreg $52,a
        ret


view_restore_saved_screen
        ;xor a
        ;ld (viewNextAfterDown),a
        nextreg MMU7_E000_NR_57,EXTRA_BANK_PAGE   ; mapuj extra banku (sipka + specialchar tam jsou)
        ld hl,sipka
        ld bc,16*16*1
        ld a,0
        call LoadSprites                      ; externí

        call VSE_NASTAV
        ld a,(viewSavedOKNO)
        ld (OKNO),a
        call loadscr
        ret


view_restore_full_ui
        ;xor a
        ;ld (viewNextAfterDown),a

        nextreg MMU7_E000_NR_57,EXTRA_BANK_PAGE   ; mapuj extra banku (sipka + specialchar tam jsou)
        ld hl,sipka
        ld bc,16*16*1
        ld a,0
        call LoadSprites                      ; externí

        call VSE_NASTAV
        call kresli
        ld a,3
        ld (OKNO),a
        ld hl,(adrl)
        ld (adrs+1),hl
        call showwin
        ld a,19
        ld (OKNO),a
        ld hl,(adrr)
        ld (adrs+1),hl
        call showwin
        ld a,(viewSavedOKNO)
        ld (OKNO),a
        call zobraz_nadpis
        ld a,32
        call writecur
        ret


view_handle_plugin_extract
        ld a,(viewPluginContext+VIEWCTX_EXTRACT_FLAG)
        or a
        ret z
        xor a
        ld (viewPluginContext+VIEWCTX_EXTRACT_FLAG),a
        ld hl,viewPluginContext+VIEWCTX_EXTRACT_NAME
        ld de,(viewPluginContext+VIEWCTX_EXTRACT_OFF)
        ld bc,(viewPluginContext+VIEWCTX_EXTRACT_CNT)
        call svc_extract_to_file
        ret


view_reload_active_panel
        ld a,(viewSavedOKNO)
        ld (OKNO),a
        call reload_dir
        ld hl,adrl
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        ld (adrs+1),hl
        call showwin
        ld a,32
        call writecur
        ret


; ================================================================
; Extraction services. Both write a byte range into a new file in the
; active panel directory; they differ only in where the bytes come from.
;
; SERVICE_EXTRACT takes them from the loaded data pages, so a plugin can
; write back data it has modified in RAM (the editor saves this way).
; SERVICE_EXTRACT_SEEK re-reads them from the source file instead, which
; is the only way to reach data past the 64KB the viewer keeps in RAM
; (TRD disk images are up to 640K).
; The shared steps live in view_extract_prepare/open_dest/exit.
; ================================================================

; view_extract_prepare: HL=plugin filename, DE=offset bits 0-15,
; BC=byte count. Saves the paging state and builds the output path.
view_extract_prepare
        ld (viewExtractOff),de
        ld (viewExtractCnt),bc
        call view_save_paging
        jp view_make_extract_path


; view_save_paging: remember the slots the plugin was running with, so
; view_extract_exit can put them back once DOS has moved them around.
view_save_paging
        ld a,$56
        call ReadNextReg2A
        ld (viewExtractSavedMmu6),a
        ld a,$57
        call ReadNextReg2A
        ld (viewExtractSavedMmu7),a
        ret


; view_extract_open_dest: create the output file as file 1 and fill in its
; +3DOS header when the plugin asked for one. Carry set on failure.
; Must be called while in dospage.
view_extract_open_dest
        ld a,(viewPluginContext+VIEWCTX_P3DOS_TYPE)
        ld d,2                  ; create action 2: raw file, no header
        cp $FF
        jr z,.do_open
        ld d,1                  ; create action 1: file with +3DOS header
.do_open
        ld b,1                  ; file number
        ld c,2                  ; exclusive write
        ld e,4                  ; erase existing, then create
        ld hl,viewPluginDosName
        call $0106
        ccf
        ret c                   ; DOS reports success with carry set
        ld a,(viewPluginContext+VIEWCTX_P3DOS_TYPE)
        cp $FF
        ret z
        ld b,1
        call svc_fill_p3dos_header
        or a
        ret


; view_extract_exit: A = error code, 0 = success. Restores the paging
; state and returns with carry set when the extraction failed.
view_extract_exit
        ld (viewExtractError),a
        call basicpage
        call view_restore_extract_state
        ld a,(viewExtractError)
        or a
        ret z
        scf
        ret


; svc_extract_to_file: HL=plugin filename, DE=data offset, BC=byte count.
; The chunk is copied out of the data pages into a private buffer below
; $C000, from where DOS_WRITE takes it without touching the directory cache.
svc_extract_to_file
        call view_extract_prepare
        call dospage
        call view_set_current_path
        call view_extract_open_dest
        jr c,.open_fail
        call basicpage
        call view_write_loop
        jr c,.write_fail

        call dospage
        ld b,1
        call $0109
        jr nc,.close_error
        ld a,1
        ld (viewPluginContext+VIEWCTX_DIRTY),a
        xor a
        jp view_extract_exit

.write_fail
        call dospage
        ld b,1
        call $0109
        ld a,2
        jp view_extract_exit
.close_error
        ld a,3
        jp view_extract_exit
.open_fail
        ld a,1
        jp view_extract_exit


; view_write_loop: write viewExtractCnt bytes, taken from viewExtractOff
; in the data pages, to the already open file 1. Entered and left in
; basicpage state; carry set on failure.
view_write_loop
        ld hl,(viewExtractCnt)
        ld a,h
        or l
        ret z
        call view_extract_copy_chunk
        call dospage
        ld b,1
        ld c,PAGE_BUFF
        ld de,(viewExtractChunk)
        ld hl,49152
        call $0115
        jr nc,.fail
        call basicpage
        ld hl,(viewExtractOff)
        ld de,(viewExtractChunk)
        add hl,de
        ld (viewExtractOff),hl
        ld hl,(viewExtractCnt)
        or a
        sbc hl,de
        ld (viewExtractCnt),hl
        jr view_write_loop
.fail
        call basicpage
        scf
        ret


; ================================================================
; Streaming output. SERVICE_EXTRACT and SERVICE_EXTRACT_SEEK each
; create, fill and close a file in a single call, so a member can never
; be longer than the 16 bit count they take. These three split that
; apart, letting a plugin hand over a file of any length a piece at a
; time - the ZIP plugin inflates into a 32K window and flushes it as it
; fills. File 1 stays open between the calls; the source file a plugin
; reads with SERVICE_READ_AT uses file 0, so the two never collide.
; ================================================================

; svc_write_open: HL = plugin filename. Creates it and leaves it open.
svc_write_open
        ld de,0
        ld bc,0
        call view_extract_prepare
        call dospage
        call view_set_current_path
        call view_extract_open_dest
        ld a,1
        jp c,view_extract_exit
        xor a
        jp view_extract_exit


; svc_write_chunk: DE = offset in the data pages, BC = byte count.
svc_write_chunk
        ld (viewExtractOff),de
        ld (viewExtractCnt),bc
        call view_save_paging
        call view_write_loop
        ld a,2
        jp c,view_extract_exit
        xor a
        jp view_extract_exit


; svc_write_close: close the output file and mark the panel for reload.
svc_write_close
        call view_save_paging
        call dospage
        ld b,1
        call $0109
        ld a,3
        jr nc,.failed
        ld a,1
        ld (viewPluginContext+VIEWCTX_DIRTY),a
        xor a
.failed
        jp view_extract_exit


; svc_extract_seek: HL=plugin filename, DE=source offset bits 0-15,
; BC=byte count. Bits 16-31 of the offset come from VIEWCTX_EXTRACT_OFHI.
; The source file is reopened and streamed straight into the output file,
; so the data may sit anywhere in it, not just in the loaded 64KB.
; Error codes 1-3 match svc_extract_to_file; 4-6 are specific to this path.
svc_extract_seek
        call view_extract_prepare
        ld hl,(viewPluginContext+VIEWCTX_EXTRACT_OFHI)
        ld (viewExtractOffHi),hl

        call dospage
        call view_set_current_path

        ; source (file 0): must exist, ignore any header, pointer at 0
        ld b,0
        ld c,1                  ; read access
        ld d,0                  ; create action 0: error when missing
        ld e,2                  ; open action 2: ignore header
        ld hl,TMP83
        call $0106
        ld a,4
        jp nc,view_extract_exit

        ld b,0
        ld hl,(viewExtractOff)
        ld de,(viewExtractOffHi)
        call $0136              ; DOS_SET_POSITION, DEHL = byte offset
        jr nc,.seek_fail

        call view_extract_open_dest
        jr c,.dest_fail

.copy_loop
        ld hl,(viewExtractCnt)
        ld a,h
        or l
        jr z,.close_ok

        ld de,LENGHT_BUFFER     ; same transfer size the file copier uses
        or a
        sbc hl,de
        jr nc,.chunk_set        ; a whole chunk or more is left: keep DE
        ld de,(viewExtractCnt)  ; tail shorter than one chunk
.chunk_set
        ld (viewExtractChunk),de

        ld b,0
        ld c,PAGE_BUFF
        ld de,(viewExtractChunk)
        ld hl,49152
        call $0112
        jr nc,.read_fail

        ld b,1
        ld c,PAGE_BUFF
        ld de,(viewExtractChunk)
        ld hl,49152
        call $0115
        jr nc,.write_fail

        ld hl,(viewExtractCnt)
        ld de,(viewExtractChunk)
        or a
        sbc hl,de
        ld (viewExtractCnt),hl
        jr .copy_loop

.close_ok
        ld b,1
        call $0109
        jr nc,.close_error
        ld b,0
        call $0109
        ld a,1
        ld (viewPluginContext+VIEWCTX_DIRTY),a
        xor a
        jp view_extract_exit

.write_fail
        ld a,2
        jr .close_dest
.read_fail
        ld a,6
.close_dest
        ld (viewExtractError),a
        ld b,1
        call $0109
        jr .close_src
.close_error
        ld a,3
        jr .store_err
.dest_fail
        ld a,1
        jr .store_err
.seek_fail
        ld a,5
.store_err
        ld (viewExtractError),a
.close_src
        ld b,0
        call $0109
        ld a,(viewExtractError)
        jp view_extract_exit


; ================================================================
; svc_read_at: read a byte range of the file being viewed into RAM.
;   C  = destination 8K page (a number out of VIEWCTX_DATA_PAGES),
;   HL = offset inside that page (0-8191), DE = byte count,
;   source offset bits 0-15 in VIEWCTX_EXTRACT_OFF, bits 16-31 in
;   VIEWCTX_EXTRACT_OFHI.
; Returns A = 0 and carry clear on success; 4 = open, 5 = seek, 6 = read.
;
; The viewer preloads only the first 64KB of a file, which is no use to a
; plugin whose index sits at the end - a ZIP keeps its directory there.
; DOS_READ pages by 16K bank, so the 8K page has to be split into the
; bank number and the address that half of it appears at.
; ================================================================
svc_read_at
        ld (viewReadAtLen),de
        ld a,h
        and $1F
        or $C0
        ld h,a
        bit 0,c
        jr z,.lower_half
        set 5,h                 ; odd page: it shows up at $E000
.lower_half
        ld (viewReadAtAddr),hl
        srl c
        ld a,c
        ld (viewReadAtBank),a

        call view_save_paging
        call dospage
        call view_set_current_path

        ld b,0
        ld c,1                  ; read access
        ld d,0                  ; create action 0: error when missing
        ld e,2                  ; open action 2: ignore any header
        ld hl,TMP83
        call $0106
        ld a,4
        jp nc,view_extract_exit

        ld b,0
        ld hl,(viewPluginContext+VIEWCTX_EXTRACT_OFF)
        ld de,(viewPluginContext+VIEWCTX_EXTRACT_OFHI)
        call $0136              ; DOS_SET_POSITION, DEHL = byte offset
        ld a,5
        jr nc,.failed

        ld b,0
        ld a,(viewReadAtBank)
        ld c,a
        ld de,(viewReadAtLen)
        ld hl,(viewReadAtAddr)
        call $0112
        ld a,6
        jr nc,.failed
        xor a
.failed
        ld (viewExtractError),a
        ld b,0
        call $0109
        ld a,(viewExtractError)
        jp view_extract_exit


view_make_extract_path
        ld de,viewPluginDosName
        ld b,63
.copy_name
        ld a,(hl)
        or a
        jr z,.name_done
        cp 255
        jr z,.name_done
        ld (de),a
        inc hl
        inc de
        djnz .copy_name
.name_done
        ld a,255
        ld (de),a
        ret


view_restore_extract_state
        ld a,(viewExtractSavedMmu6)
        nextreg $56,a
        ld a,(viewExtractSavedMmu7)
        nextreg $57,a
        ret


view_extract_copy_chunk
        ld de,(viewExtractOff)
        ld a,d
        and $E0
        rlca
        rlca
        rlca
        ld l,a
        ld h,0
        ld bc,viewDataPages
        add hl,bc
        ld a,(hl)
        nextreg $57,a

        ld a,d
        and $1F
        or $E0
        ld h,a
        ld l,e

        push hl
        ld a,d
        and $1F
        ld b,a
        ld c,e
        ld hl,$2000
        or a
        sbc hl,bc              ; bytes left in current 8K source page
        ld bc,(viewExtractCnt)
        or a
        sbc hl,bc
        jr c,.use_page_left
        ld d,b
        ld e,c
        jr .cap_to_buffer
.use_page_left
        add hl,bc
        ex de,hl
.cap_to_buffer
        ld hl,96
        or a
        sbc hl,de
        jr nc,.got_chunk
        ld de,96
.got_chunk
        ld (viewExtractChunk),de
        push de
        pop bc
        pop hl
        nextreg $56,PAGE_BUFF*2
        ld de,49152
        ldir
        ld a,(viewExtractSavedMmu6)
        nextreg $56,a
        ld a,(viewExtractSavedMmu7)
        nextreg $57,a
        ret


viewServices
        defw print
        defw INKEY
        defw window
        defw layer0
        defw view_plugin_input_nowait
        defw svc_extract_to_file
        defw beepk
        defw svc_extract_seek
        defw svc_read_at
        defw svc_write_open
        defw svc_write_chunk
        defw svc_write_close


view_init_plugin_input
        xor a
        ld (TLACITKO),a
        call view_read_wheel
        ld (viewWheelOld),a
        xor a
        ld (viewMusicSetupDirty),a
        ld a,(viewPluginType)
        cp VIEWTYPE_PT3
        jr z,.music
        cp VIEWTYPE_PT2
        jr z,.music
        cp VIEWTYPE_STC
        jr z,.music
        cp VIEWTYPE_STP
        jr z,.music
        cp VIEWTYPE_SQT
        ret nz
.music
        call view_music_apply_saved_setup
        ld a,1
        ld (viewMusicSetupDirty),a
        ret


view_music_apply_saved_setup
        ld a,$06
        call ReadNextReg2A
        and $FC
        ld b,a
        ld a,(viewMusicChipMode)
        or a
        ld a,b
        jr z,.set_chip
        or $01
.set_chip
        nextreg $06,a

        ld a,$08
        call ReadNextReg2A
        and $DF
        ld b,a
        ld a,(viewMusicStereoMode)
        or a
        ld a,b
        jr z,.set_stereo
        or $20
.set_stereo
        nextreg $08,a
        ret


view_plugin_input_nowait
        ld a,(viewSavedMmu6)
        nextreg $56,a
        call view_plugin_input_body
        push af
        ld a,VIEW_PLUGIN_PAGE
        nextreg $56,a
        pop af
        ret

view_plugin_input_body
        call MOUSE
        push af
        ld hl,(COORD)
        ld de,(lastCoordMouse)
        or a
        sbc hl,de
        jr z,.no_mouse_move
        call showSprite
.no_mouse_move
        call MOUSE
        ld hl,(COORD)
        ld (lastCoordMouse),hl
        pop af
        push af
        ld a,(viewMusicSetupDirty)
        or a
        jr z,.setup_ok
        xor a
        ld (viewMusicSetupDirty),a
        call view_music_show_setup
.setup_ok
        pop af
        bit 0,a
        jp nz,.mouse_click
        bit 1,a
        jp nz,.mouse_click

        call view_read_wheel
        ld b,a
        ld a,(viewWheelOld)
        ld c,a
        cp b
        jr z,.keyboard
        ld a,b
        ld (viewWheelOld),a
        ld a,c
        cp 15
        jr z,.keyboard
        or a
        jr z,.keyboard
        ld a,b
        cp c
        jr c,.wheel_up
        ld a,10
        ret
.wheel_up
        ld a,11
        ret

.keyboard
        call KEYSCAN
        ld a,e
        inc a
        jp z,.no_key
        ld a,(viewPluginType)
        cp VIEWTYPE_EDIT
        jr z,.edit_special
.check_music_special
        cp VIEWTYPE_PT3
        jp z,.music_scan_keyboard
        cp VIEWTYPE_PT2
        jp z,.music_scan_keyboard
        cp VIEWTYPE_STC
        jp z,.music_scan_keyboard
        cp VIEWTYPE_STP
        jp z,.music_scan_keyboard
        cp VIEWTYPE_SQT
        jp nz,.map_keyboard
.music_scan_keyboard
        ld a,e
        cp 38
        jp z,view_music_set_ay
        cp 2
        jp z,view_music_set_ym
        cp 0
        jp z,view_music_set_abc
        cp 15
        jp z,view_music_set_acb
.map_keyboard
        ld a,d
        ld hl,SYMTAB
        cp $18
        jr z,.map
        ld hl,CAPSTAB
        cp $27
        jr z,.map
        ld hl,NORMTAB
.map
        ld d,0
        add hl,de
        ld a,(hl)
        or a
        ret z
        ld b,a
        ld a,(viewPluginType)
        cp VIEWTYPE_PT3
        jr z,.music_keyboard
        cp VIEWTYPE_PT2
        jr z,.music_keyboard
        cp VIEWTYPE_STC
        jr z,.music_keyboard
        cp VIEWTYPE_STP
        jr z,.music_keyboard
        cp VIEWTYPE_SQT
        jr z,.music_keyboard
        cp VIEWTYPE_HELLO
        jr z,.player_keyboard
        ld a,b
        ret
.edit_special
        ld a,d
        cp $18
        jr nz,.edit_not_special
        ld hl,.edit_key_table
        ld b,5
.edit_key_loop
        ld a,(hl)
        cp e
        inc hl
        jr z,.edit_key_hit
        inc hl
        djnz .edit_key_loop
.edit_not_special
        jr .map_keyboard
.edit_key_hit
        ld a,(hl)
        ret
.edit_key_table
        defb 30,VIEW_EDIT_KEY_SAVE
        defb 21,VIEW_EDIT_KEY_SAVEAS
        defb 1,VIEW_EDIT_KEY_HEX
        defb 5,VIEW_EDIT_KEY_TEXT
        defb 14,VIEW_EDIT_KEY_FIND
.player_keyboard
        ld a,b
        cp 13
        jr z,.music_key_stop
        cp " "
        jr z,.music_key_next
        ret
.music_keyboard
        ld a,b
        cp 13
        jr z,.music_key_stop
        cp " "
        jr z,.music_key_next
        cp "a"
        jp z,view_music_set_ay
        cp "A"
        jp z,view_music_set_ay
        cp "y"
        jp z,view_music_set_ym
        cp "Y"
        jp z,view_music_set_ym
        cp "b"
        jp z,view_music_set_abc
        cp "B"
        jp z,view_music_set_abc
        cp "c"
        jp z,view_music_set_acb
        cp "C"
        jp z,view_music_set_acb
        ret
.music_key_stop
        ld a,1
        ret
.music_key_next
        ld a,2
        ret
.no_key
        xor a
        ret

.mouse_click
        ld a,(viewPluginType)
        cp VIEWTYPE_ZXSCREEN
        jp z,view_image_mouse_click
        cp VIEWTYPE_NXI
        jp z,view_image_mouse_click
        sub VIEWTYPE_EDIT
        ret z
        add a,VIEWTYPE_EDIT
        cp VIEWTYPE_PT3
        jr z,.music_click
        cp VIEWTYPE_PT2
        jr z,.music_click
        cp VIEWTYPE_STC
        jr z,.music_click
        cp VIEWTYPE_STP
        jr z,.music_click
        cp VIEWTYPE_SQT
        jr z,.music_click
        cp VIEWTYPE_HELLO
        jr z,.hello_click
        jr .generic_mouse_click
.music_click
        call view_pt3_mouse_click
        ret nz
.hello_click
        call view_player_mouse_click
        ret nz
.generic_mouse_click
        ld a,13
        ret


view_image_mouse_click
        ld a,(COORD+1)
        cp 224
        jr c,.outside
        cp 248
        jr nc,.outside
        ld a,(COORD+0)
        cp 24
        jr c,.outside
        cp 52
        jr c,.stop
        cp 96
        jr c,.outside
        cp 124
        jr c,.next
.outside
        xor a
        ret
.stop
        ld a,1
        ret
.next
        ld a,2
        ret


view_pt3_mouse_click
        ld a,(COORD+1)
        cp 68
        jr c,.not_setup
        cp 84
        jr nc,.not_setup
        ld a,(COORD+0)
        ld b,a
        ld a,(viewMusicDrawOffset)
        or a
        jr z,.no_setup_offset
        ld c,a
        ld a,b
        sub c
        jr c,.window
        jr .setup_x
.no_setup_offset
        ld a,b
.setup_x
        cp 16
        jr c,.window
        cp 24
        jp c,view_music_set_ay
        cp 32
        jp c,view_music_set_ym
        cp 40
        jp c,view_music_set_abc
        cp 52
        jp c,view_music_set_acb
        jr .window
.not_setup
        ld a,(COORD+1)
        cp 188
        jr c,.window
        cp 204
        jr nc,.outside
        ld a,(COORD+0)
        cp 3
        jr c,.outside
        cp 28
        jr c,.stop
        cp 132
        jr c,.outside
        cp 154
        jr nc,.outside
        ld a,2
        or a
        ret
.stop
.outside
        ld a,1
        or a
        ret
.window
        ld a,(COORD+1)
        cp 40
        jr c,.outside
        cp 164
        jr nc,.outside
        ld a,(COORD+0)
        cp 157
        jr nc,.outside
        xor a
        ret


view_player_mouse_click
        ld a,(COORD+1)
        cp 188
        jr c,.window
        cp 204
        jr nc,.outside
        ld a,(COORD+0)
        cp 3
        jr c,.outside
        cp 28
        jr c,.stop
        cp 132
        jr c,.outside
        cp 154
        jr nc,.outside
        ld a,2
        or a
        ret
.stop
        ld a,1
        or a
        ret
.window
        ld a,(COORD+1)
        cp 40
        jr c,.outside
        cp 164
        jr nc,.outside
        ld a,(COORD+0)
        cp 157
        jr nc,.outside
        xor a
        ret
.outside
        ld a,1
        or a
        ret


view_music_set_ay
        ld a,$06
        call ReadNextReg2A
        and $FC
        or $01
        nextreg $06,a
        ld a,1
        ld (viewMusicChipMode),a
        jp view_music_show_setup


view_music_set_ym
        ld a,$06
        call ReadNextReg2A
        and $FC
        nextreg $06,a
        xor a
        ld (viewMusicChipMode),a
        jp view_music_show_setup


view_music_set_abc
        ld a,$08
        call ReadNextReg2A
        and $DF
        nextreg $08,a
        xor a
        ld (viewMusicStereoMode),a
        jp view_music_show_setup


view_music_set_acb
        ld a,$08
        call ReadNextReg2A
        or $20
        nextreg $08,a
        ld a,1
        ld (viewMusicStereoMode),a
        jp view_music_show_setup


view_music_mark_setup_dirty
        ld a,1
        ld (viewMusicSetupDirty),a
        xor a
        ret


view_music_show_setup
        ld a,$52
        call ReadNextReg2A
        ld (viewTempMmu2),a
        ld a,(viewSavedMmu2)
        nextreg $52,a
        ld a,(viewSavedMmu6)
        nextreg $56,a
        ld a,(viewSavedMmu7)
        nextreg $57,a

        xor a
        ld (viewMusicDrawOffset),a
        ld a,($45B0)
        cp "["
        jr z,.offset_ok
        cp " "
        jr z,.offset_ok
        ld a,4
        ld (viewMusicDrawOffset),a
.offset_ok

        ld hl,$45B0
        call view_music_apply_offset
        ld de,viewMusicAyActiveTxt
        ld a,(viewMusicChipMode)
        or a
        jr nz,.print_ay
        ld de,viewMusicAyTxt
.print_ay
        call view_music_draw_text

        ld hl,$45B8
        call view_music_apply_offset
        ld de,viewMusicYmActiveTxt
        ld a,(viewMusicChipMode)
        or a
        jr z,.print_ym
        ld de,viewMusicYmTxt
.print_ym
        call view_music_draw_text

        ld hl,$45C0
        call view_music_apply_offset
        ld de,viewMusicAbcActiveTxt
        ld a,(viewMusicStereoMode)
        or a
        jr z,.print_abc
        ld de,viewMusicAbcTxt
.print_abc
        call view_music_draw_text

        ld hl,$45CA
        call view_music_apply_offset
        ld de,viewMusicAcbActiveTxt
        ld a,(viewMusicStereoMode)
        or a
        jr nz,.print_acb
        ld de,viewMusicAcbTxt
.print_acb
        call view_music_draw_text

        ld a,(viewTempMmu2)
        nextreg $52,a
        ld a,VIEW_PLUGIN_PAGE
        nextreg $56,a
        ld a,VIEW_DATA_PAGE
        nextreg $57,a
        xor a
        ret


view_music_apply_offset
        ld a,(viewMusicDrawOffset)
        add a,l
        ld l,a
        ret nc
        inc h
        ret


view_music_draw_text
        ld a,(de)
        or a
        ret z
        ld (hl),a
        inc hl
        ld (hl),16
        inc hl
        inc de
        jr view_music_draw_text


view_read_wheel
        ld bc,$fadf
        in a,(c)
        and $F0
        rrca
        rrca
        rrca
        rrca
        ret


view_no_viewer_or_skip
        ld a,(viewNextAfterDown)
        or a
        jp nz,down

view_no_viewer
        ld de,viewNoViewerTxt
        jp view_error_dialog

view_file_error
        ld de,viewFileErrorTxt
        jp view_error_dialog

view_plugin_error
        ld de,viewPluginErrorTxt
        jp view_error_dialog

view_error_dialog
        push de
        call savescr
        ld hl,10 * 256 + 10
        ld bc,60 * 256 + 8
        ld a,144
        call window
        ld hl,12*256+11
        ld a,144
        ld de,viewErrorTitleTxt
        call print
        pop de
        ld hl,12*256+13
        ld a,144
        call print
        ld hl,12*256+15
        ld a,144
        ld de,viewTryOtherTxt
        call print
        ld hl,49*256+17
        ld a,16
        ld de,conttxt
        call print
.wait
        xor a
        ld (TLACITKO),a
        call INKEY
        cp 13
        jr nz,.wait
        call loadscr
        jp loop0


viewPluginContext    defs VIEWCTX_SIZE
viewPluginName       defw 0
viewPluginType       defb 0
viewFileSizeLo       defw 0
viewFileSizeHi       defw 0
viewRemainingLo      defw 0
viewRemainingHi      defw 0
viewReadLen          defw 0
viewFirstReadLen     defw 0
viewDataPageCount    defb 0
viewSavedMmu6        defb 0
viewSavedMmu7        defb 0
viewSavedMmu2        defb 0
viewTempMmu2         defb 0
viewSavedOKNO        defb 3
viewErrorStage       defb 0
viewWheelOld         defb 0
viewPluginResult     defb 0
viewNextAfterDown    defb 0
viewMusicSetupDirty  defb 0
viewMusicChipMode    defb 1
viewMusicStereoMode  defb 0
viewMusicDrawOffset  defb 0
viewPluginMenuCursor defb 0
viewPluginMenuTop    defb 0
viewPluginMenuPrintRow defb 0
viewShortName        defs 13

; DOS reads use 16K banks. Plugins receive the MMU7 page numbers
; that expose the upper 8K of each bank at $E000.
viewDataBanks        defb 40,42,43,44,45,46,47,48
viewDataPages        defb 81,85,87,89,91,93,95,97

VIEW_PLUGIN_MENU_VISIBLE equ 7
VIEW_PLUGIN_MENU_COUNT equ 10
VIEW_PLUGIN_MENU_LAST equ VIEW_PLUGIN_MENU_COUNT-1
viewPluginMenuMouseArea defb 45,88,115,143
viewPluginMenuTable
        defb VIEWTYPE_TEXT : defw viewTextPluginName : defw viewPluginMenuTextTxt
        defb VIEWTYPE_ZXSCREEN : defw viewZxScreenPluginName : defw viewPluginMenuZxTxt
        defb VIEWTYPE_NXI : defw viewNxiPluginName : defw viewPluginMenuNxiTxt
        defb VIEWTYPE_PT3 : defw viewPt3PluginName : defw viewPluginMenuPt3Txt
        defb VIEWTYPE_PT2 : defw viewPt2PluginName : defw viewPluginMenuPt2Txt
        defb VIEWTYPE_STC : defw viewStcPluginName : defw viewPluginMenuStcTxt
        defb VIEWTYPE_STP : defw viewStpPluginName : defw viewPluginMenuStpTxt
        defb VIEWTYPE_SQT : defw viewSqtPluginName : defw viewPluginMenuSqtTxt
        defb VIEWTYPE_HELLO : defw viewHelloPluginName : defw viewPluginMenuHelloTxt
        defb VIEWTYPE_BAS : defw viewBasPluginName : defw viewPluginMenuBasTxt

viewPluginDir          defb "c:/CalmCommander/plugin",255
viewTextPluginName     defb "text.ccp",255
viewZxScreenPluginName defb "zxscreen.ccp",255
viewNxiPluginName      defb "nxi.ccp",255
viewPt3PluginName      defb "pt3test.ccp",255
viewPt2PluginName      defb "pt2test.ccp",255
viewStcPluginName      defb "stctest.ccp",255
viewStpPluginName      defb "stptest.ccp",255
viewSqtPluginName      defb "sqtest.ccp",255
viewHelloPluginName    defb "HelloWord.ccp",255
viewBasPluginName      defb "bas.ccp",255
viewTapPluginName      defb "tap.ccp",255
viewTrdPluginName      defb "trd.ccp",255
viewZipPluginName      defb "zip.ccp",255
viewEditPluginName     defb "edit.ccp",255
viewMusicAyTxt         defb " AY ",0
viewMusicYmTxt         defb " YM ",0
viewMusicAbcTxt        defb " ABC ",0
viewMusicAcbTxt        defb " ACB ",0
viewMusicAyActiveTxt   defb "[AY]",0
viewMusicYmActiveTxt   defb "[YM]",0
viewMusicAbcActiveTxt  defb "[ABC]",0
viewMusicAcbActiveTxt  defb "[ACB]",0
viewPluginMenuTitleTxt defb "Viewer:",0
viewPluginMenuTextTxt  defb "Text text.ccp",0
viewPluginMenuZxTxt    defb "SCR zxscreen.ccp",0
viewPluginMenuNxiTxt   defb "NXI nxi.ccp",0
viewPluginMenuPt3Txt   defb "PT3 pt3test.ccp",0
viewPluginMenuPt2Txt   defb "PT2 pt2test.ccp",0
viewPluginMenuStcTxt   defb "STC stctest.ccp",0
viewPluginMenuStpTxt   defb "STP stptest.ccp",0
viewPluginMenuSqtTxt   defb "SQT sqtest.ccp",0
viewPluginMenuHelloTxt defb "Hello HelloWord.ccp",0
viewPluginMenuBasTxt   defb "BAS bas.ccp",0
viewPluginMenuBlankTxt defb "                            ",0

viewErrorTitleTxt       defb "Viewer:",0
viewNoViewerTxt         defb "No viewer.",0
viewFileErrorTxt        defb "Cannot open file.",0
viewPluginErrorTxt      defb "Cannot load plugin.",0
viewTryOtherTxt         defb "Try another file.",0
viewPluginDosName       defs 64
viewExtractOff          defw 0
viewExtractCnt          defw 0
viewExtractChunk        defw 0
viewExtractHandle       defb 0
viewExtractSavedMmu6    defb 0
viewExtractSavedMmu7    defb 0
viewExtractError        defb 0
viewExtractOffHi        defw 0
viewReadAtLen           defw 0
viewReadAtAddr          defw 0
viewReadAtBank          defb 0
