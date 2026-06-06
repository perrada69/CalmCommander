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
VIEWCTX_SIZE         equ 18

VIEWTYPE_TEXT        equ 1
VIEWTYPE_ZXSCREEN    equ 2
VIEWTYPE_PT3         equ 3

view_file
        call view_prepare_current_file
        jp c,view_no_viewer_or_skip

        call view_make_cmd2
        call view_select_plugin
        jp c,view_no_viewer_or_skip

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
        call view_restore_saved_screen
        ld a,(viewPluginType)
        cp VIEWTYPE_PT3
        jr nz,.done
        ld a,(viewPluginResult)
        cp 1
        jr nz,.done
        ld (viewNextAfterDown),a
        jp down
.done
        xor a
        ld (viewNextAfterDown),a
        jp loop0
.full_restore
        xor a
        ld (viewNextAfterDown),a
        call view_restore_full_ui
        jp loop0


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


view_select_plugin
        call view_make_short_name
        call view_is_zx_screen
        jp z,.zxscreen

        ld hl,viewShortName
        ld de,ext_pt3
        call pripony
        jp z,.pt3
        ld hl,viewShortName
        ld de,ext_PT3
        call pripony
        jp z,.pt3

        ld hl,viewShortName
        ld de,ext_txt
        call pripony
        jr z,.text
        ld hl,viewShortName
        ld de,ext_TXT
        call pripony
        jr z,.text
        ld hl,viewShortName
        ld de,ext_asm
        call pripony
        jr z,.text
        ld hl,viewShortName
        ld de,ext_ASM
        call pripony
        jr z,.text
        ld hl,viewShortName
        ld de,ext_bas
        call pripony
        jr z,.text
        ld hl,viewShortName
        ld de,ext_BAS
        call pripony
        jr z,.text
        ld hl,viewShortName
        ld de,ext_cfg
        call pripony
        jr z,.text
        ld hl,viewShortName
        ld de,ext_CFG
        call pripony
        jr z,.text
        ld hl,viewShortName
        ld de,ext_ini
        call pripony
        jr z,.text
        ld hl,viewShortName
        ld de,ext_INI
        call pripony
        jr z,.text

        scf
        ret

.text
        ld a,VIEWTYPE_TEXT
        ld (viewPluginType),a
        ld hl,viewTextPluginName
        ld (viewPluginName),hl
        xor a
        ret

.zxscreen
        ld a,VIEWTYPE_ZXSCREEN
        ld (viewPluginType),a
        ld hl,viewZxScreenPluginName
        ld (viewPluginName),hl
        xor a
        ret

.pt3
        ld a,VIEWTYPE_PT3
        ld (viewPluginType),a
        ld hl,viewPt3PluginName
        ld (viewPluginName),hl
        xor a
        ret


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


view_load_data_blocks
        call dospage

        ld a,1
        ld (viewErrorStage),a
        call view_set_current_path

        ld a,2
        ld (viewErrorStage),a
        call view_copy_tmp83_for_debug
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
        ld hl,viewPluginDir
        xor a
        call $01b1

        ld a,5
        ld (viewErrorStage),a
        ld b,0
        ld c,1
        ld e,2
        ld hl,(viewPluginName)
        call 0106h

        ld a,6
        ld (viewErrorStage),a
        ld b,0
        ld c,VIEW_PLUGIN_BANK
        ld de,VIEW_PLUGIN_MAX_SIZE
        ld hl,VIEW_PLUGIN_ADDRESS
        call 0112h
        push af
        ld b,0
        call 0109h
        call view_restore_current_path
        call basicpage
        pop af

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
        ld hl,pathl
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        push hl
        call view_copy_path_for_debug
        pop hl
        xor a
        call $01b1
        ret


view_copy_path_for_debug
        push de
        push bc
        ld de,viewDebugPath
        ld bc,59
.copy
        ld a,(hl)
        cp 255
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
        pop bc
        pop de
        ret


view_copy_tmp83_for_debug
        push hl
        push de
        push bc
        ld hl,TMP83
        ld de,viewDebug83
        ld b,11
.copy
        ld a,(hl)
        cp 255
        jr z,.done
        ld (de),a
        inc hl
        inc de
        djnz .copy
.done
        xor a
        ld (de),a
        pop bc
        pop de
        pop hl
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
        ret


view_call_plugin
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
        ret


view_restore_saved_screen
        call VSE_NASTAV
        ld a,(viewSavedOKNO)
        ld (OKNO),a
        call loadscr
        ret


view_restore_full_ui
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


viewServices
        defw print
        defw INKEY
        defw window
        defw layer0
        defw view_plugin_input_nowait


view_init_plugin_input
        xor a
        ld (TLACITKO),a
        call view_read_wheel
        ld (viewWheelOld),a
        ret


view_plugin_input_nowait
        call MOUSE
        push af
        ld hl,(COORD)
        ld de,(lastCoordMouse)
        or a
        sbc hl,de
        jr z,.no_mouse_move
        ld a,(viewSavedMmu6)
        nextreg $56,a
        call showSprite
        ld a,VIEW_PLUGIN_PAGE
        nextreg $56,a
.no_mouse_move
        call MOUSE
        ld hl,(COORD)
        ld (lastCoordMouse),hl
        pop af
        bit 0,a
        jr nz,.mouse_click
        bit 1,a
        jr nz,.mouse_click

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
        jr z,.no_key
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
        ret
.no_key
        xor a
        ret
.mouse_click
        ld a,13
        ret


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


view_stage_to_text
        ld a,(viewErrorStage)
        cp 1
        jr z,.set_file_path
        cp 2
        jr z,.open_file
        cp 3
        jr z,.read_file
        cp 4
        jr z,.set_plugin_path
        cp 5
        jr z,.open_plugin
        cp 6
        jr z,.read_plugin
        ld hl,viewStageUnknownTxt
        jr .copy
.set_file_path
        ld hl,viewStageSetFilePathTxt
        jr .copy
.open_file
        ld hl,viewStageOpenFileTxt
        jr .copy
.read_file
        ld hl,viewStageReadFileTxt
        jr .copy
.set_plugin_path
        ld hl,viewStageSetPluginPathTxt
        jr .copy
.open_plugin
        ld hl,viewStageOpenPluginTxt
        jr .copy
.read_plugin
        ld hl,viewStageReadPluginTxt
.copy
        ld de,viewDebugStage
        ld bc,23
.copy_loop
        ld a,(hl)
        ld (de),a
        or a
        ret z
        inc hl
        inc de
        dec bc
        ld a,b
        or c
        jr nz,.copy_loop
        xor a
        ld (de),a
        ret

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
viewSavedOKNO        defb 3
viewErrorStage       defb 0
viewWheelOld         defb 0
viewPluginResult     defb 0
viewNextAfterDown    defb 0
viewShortName        defs 13

; DOS reads use 16K banks. Plugins receive the MMU7 page numbers
; that expose the upper 8K of each bank at $E000.
viewDataBanks        defb 40,42,43,44,45,46,47,48
viewDataPages        defb 81,85,87,89,91,93,95,97

viewPluginDir          defb "c:/CalmCommander/plugin",255
viewTextPluginName     defb "text.ccp",255
viewZxScreenPluginName defb "zxscreen.ccp",255
viewPt3PluginName      defb "pt3test.ccp",255

viewErrorTitleTxt       defb "Viewer:",0
viewNoViewerTxt         defb "No viewer is available for this file.",0
viewFileErrorTxt        defb "Cannot open selected file.",0
viewPluginErrorTxt      defb "Cannot load viewer plugin.",0
viewTryOtherTxt         defb "Install a matching plugin or use another file.",0
viewStageLabelTxt       defb "Stage:",0
viewPathLabelTxt        defb "Path:",0
viewNameLabelTxt        defb "Name:",0
view83LabelTxt          defb "83:",0
viewStageUnknownTxt     defb "unknown",0
viewStageSetFilePathTxt defb "set file path",0
viewStageOpenFileTxt    defb "open file",0
viewStageReadFileTxt    defb "read file",0
viewStageSetPluginPathTxt defb "set plugin path",0
viewStageOpenPluginTxt  defb "open plugin",0
viewStageReadPluginTxt  defb "read plugin",0
viewDebugStage          defs 24
viewDebugPath           defs 60
viewDebug83             defs 12
