dir_info_trim_lfn_zero
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


dir_info_prepare_context
        ld a,DIRINFO_ABI
        ld (dirInfoContext+DIRINFOCTX_ABI),a
        call syscopy_get_active_path
        ld (dirInfoContext+DIRINFOCTX_SRC_PATH),hl
        ld hl,LFNNAME
        ld (dirInfoContext+DIRINFOCTX_NAME),hl
        xor a
        ld (dirInfoContext+DIRINFOCTX_RESULT),a
        ld (dirInfoContext+DIRINFOCTX_ERROR),a
        ld (dirInfoContext+DIRINFOCTX_STAGE),a
        ld hl,0
        ld (dirInfoContext+DIRINFOCTX_ITEMS_LO),hl
        ld (dirInfoContext+DIRINFOCTX_ITEMS_HI),hl
        ld (dirInfoContext+DIRINFOCTX_SIZE_LO),hl
        ld (dirInfoContext+DIRINFOCTX_SIZE_HI),hl
        ld (dirInfoContext+DIRINFOCTX_SERVICES),hl
        ret


dir_info_load_plugin
        ld hl,dirInfoPluginName
        ld (syscopyLoadName+1),hl
        ld hl,DIRINFO_PLUGIN_SIZE
        ld (syscopyLoadSize+1),hl
        call syscopy_load_plugin
        push af
        ld hl,sysCopyPluginName
        ld (syscopyLoadName+1),hl
        ld hl,SYSCOPY_PLUGIN_SIZE
        ld (syscopyLoadSize+1),hl
        pop af
        ret


dir_info_run_from_info
        call dir_info_trim_lfn_zero
        call dir_info_show_loading
        call dir_info_prepare_context
        call dir_info_load_plugin
        jp c,dir_info_plugin_error
        call syscopy_call_plugin
        ld a,(dirInfoContext+DIRINFOCTX_RESULT)
        or a
        jp nz,dir_info_plugin_error
        call loadscr
        ld a,1
        ld (dirInfoActive),a
        jp info_file_show_ready


dir_info_show_loading
        call savescr
        ld hl,10 * 256 + 10
        ld bc,60 * 256 + 6
        ld a,16
        call window

        ld hl,11*256+11
        ld a,16
        ld de,dirInfoLoadingTitleTxt
        call print
        ld hl,11*256+13
        ld a,16
        ld de,dirInfoLoadingTxt
        call print
        ld hl,11*256+14
        ld a,16
        ld de,dirInfoPleaseWaitTxt
        jp print


dir_info_plugin_error
        call loadscr
        call savescr
        ld hl,10 * 256 + 10
        ld bc,60 * 256 + 4
        ld a,16
        call window
        ld hl,11*256+11
        ld a,16
        ld de,dirInfoLoadErrorTxt
        call print
        ld hl,11*256+13
        ld a,48
        ld de,pressanykeytxt
        call print
        xor a
        ld (TLACITKO),a
        call INKEY
        call loadscr
        jp loop0


dir_info_print_items_if_active
        ld a,(dirInfoActive)
        or a
        ret z
        ld hl,11*256+17
        ld a,16
        ld de,dirInfoItemsTxt
        call print
        ld hl,16*256+17
        ld (dec32pos+1),hl
        ld hl,(dirInfoContext+DIRINFOCTX_ITEMS_LO)
        ld de,(dirInfoContext+DIRINFOCTX_ITEMS_HI)
        ld b,10
        ld a,16
        ld (decink+1),a
        call DEC32
        xor a
        ld (decink+1),a
        ret


dir_info_get_size_value
        ld a,(dirInfoActive)
        or a
        jr z,.file_size
        ld hl,(dirInfoContext+DIRINFOCTX_SIZE_LO)
        ld de,(dirInfoContext+DIRINFOCTX_SIZE_HI)
        ret
.file_size
        ld hl,(LFNNAME+261)
        ld de,(LFNNAME+261+2)
        ret


dirInfoLoadingTitleTxt defb "Directory info",0
dirInfoLoadingTxt      defb "Loading directory information...",0
dirInfoPleaseWaitTxt   defb "Please wait a moment.",0
dirInfoItemsTxt        defb "Items:",0
dirInfoLoadErrorTxt    defb "Directory info failed.",0
