; ============================================================
; Viewer code that lives in the second part of the binary.
; The plugin chooser is pure UI: it never pages DOS in and is never
; called from a plugin service, so it does not have to stay resident
; in the $A000 part where the extraction services must live.
; ============================================================


view_choose_plugin_dialog
        call savescr
        xor a
        ld (viewPluginMenuCursor),a
        ld (viewPluginMenuTop),a
        call view_read_wheel
        ld (viewWheelOld),a

        ld hl,23*256+8
        ld bc,34*256+12
        ld a,144
        call window
        ld hl,25*256+9
        ld a,144
        ld de,viewPluginMenuTitleTxt
        call print
        call view_plugin_menu_print_items
        ld a,64
        call view_plugin_menu_write_cursor

.loop
        xor a
        ld (TLACITKO),a
        call INKEY
        cp 1
        jp z,.cancel
        cp 10
        jp z,.down
        cp 11
        jp z,.up
        cp 13
        jp z,.enter

        call view_plugin_menu_wheel
        cp 10
        jp z,.down
        cp 11
        jp z,.up

        ld a,(TLACITKO)
        bit 1,a
        jp nz,.mouse
        jp .loop

.down
        ld a,(viewPluginMenuCursor)
        cp VIEW_PLUGIN_MENU_VISIBLE-1
        jr z,.down_scroll
        ld b,a
        ld a,(viewPluginMenuTop)
        add a,b
        cp VIEW_PLUGIN_MENU_LAST
        jp z,.loop
        ld a,144
        call view_plugin_menu_write_cursor
        ld hl,viewPluginMenuCursor
        inc (hl)
        ld a,64
        call view_plugin_menu_write_cursor
        jp .loop

.down_scroll
        ld a,(viewPluginMenuTop)
        add a,VIEW_PLUGIN_MENU_VISIBLE
        cp VIEW_PLUGIN_MENU_COUNT
        jp nc,.loop
        ld hl,viewPluginMenuTop
        inc (hl)
        call view_plugin_menu_print_items
        ld a,64
        call view_plugin_menu_write_cursor
        jp .loop

.up
        ld a,(viewPluginMenuCursor)
        or a
        jp z,.up_scroll
        ld a,144
        call view_plugin_menu_write_cursor
        ld hl,viewPluginMenuCursor
        dec (hl)
        ld a,64
        call view_plugin_menu_write_cursor
        jp .loop

.up_scroll
        ld a,(viewPluginMenuTop)
        or a
        jp z,.loop
        ld hl,viewPluginMenuTop
        dec (hl)
        call view_plugin_menu_print_items
        ld a,64
        call view_plugin_menu_write_cursor
        jp .loop

.mouse
        ld hl,viewPluginMenuMouseArea
        call CONTROL
        jp c,.loop
        ld a,(COORD+1)
        ld d,a
        ld e,8
        call deleno8
        ld a,d
        cp 11
        jp c,.loop
        cp 18
        jp nc,.loop
        sub 11
        cp VIEW_PLUGIN_MENU_VISIBLE
        jp nc,.loop
        ld c,a
        ld a,(viewPluginMenuTop)
        add a,c
        cp VIEW_PLUGIN_MENU_COUNT
        jp nc,.loop
        ld a,c
        ld (viewPluginMenuCursor),a
        jp .enter

.enter
        call loadscr
        call view_plugin_menu_set_plugin
        xor a
        ret

.cancel
        xor a
        ld (viewNextAfterDown),a
        call loadscr
        scf
        ret


view_plugin_menu_wheel
        call view_read_wheel
        ld b,a
        ld a,(viewWheelOld)
        ld c,a
        cp b
        jr z,.no_wheel
        ld a,b
        ld (viewWheelOld),a
        ld a,c
        cp 15
        jr z,.no_wheel
        or a
        jr z,.no_wheel
        ld a,b
        cp c
        jr c,.wheel_up
        ld a,10
        ret
.wheel_up
        ld a,11
        ret
.no_wheel
        xor a
        ret



; Kept here rather than with the other extensions in cc.asm: the first
; part of the binary is full, and only view_select_plugin reads these.
ext_trd defb ".trd"
ext_TRD defb ".TRD"
ext_scl defb ".scl"
ext_SCL defb ".SCL"
