        DEVICE ZXSPECTRUMNEXT
        org SETTINGS_PLUGIN_ADDRESS

        include "settings_api.i.asm"

PLUGIN_STACK       equ $DFFE
BACKUP_PALETTE     equ $E000
BACKUP_KEYS        equ $E020
BACKUP_COLOURS     equ $E060
BACKUP_SCHEME      equ $E0A0
LINE_BUFFER        equ $E100
KEY_NAME_BUFFER    equ $E180

BOX_X              equ 4
BOX_Y              equ 3
BOX_WIDTH          equ 72
BOX_HEIGHT         equ 25
LIST_X             equ BOX_X+2
LIST_Y             equ BOX_Y+6
VISIBLE_ROWS       equ 15
LIST_WIDTH         equ BOX_WIDTH-4
COLOUR_ROW_COUNT   equ SETTINGS_STYLE_COUNT+1

plugin_start
        ld (ctxPtr),hl
        ld (svcPtr),de
        ld (savedSp),sp
        ld sp,PLUGIN_STACK
        ld ix,(ctxPtr)
        xor a
        ld (ix+SETTINGSCTX_RESULT),a
        ld (ix+SETTINGSCTX_ERROR),a
        ld a,(ix+SETTINGSCTX_ABI)
        cp SETTINGS_ABI
        jp nz,.bad_abi
        call patch_services
        call backup_config
        xor a
        ld (currentTab),a
        ld (currentIndex),a
        ld (topIndex),a
        call draw_screen

.input
        call read_key
        cp 1
        jp z,.cancel
        cp 13
        jr z,.enter
        cp 11
        jr z,.up
        cp 10
        jr z,.down
        cp 8
        jr z,.left
        cp 9
        jr z,.right
        cp "s"
        jr z,.save
        cp "S"
        jr z,.save
        jr .input
.up
        call move_up
        call draw_rows
        jr .input
.down
        call move_down
        call draw_rows
        jr .input
.left
        ld a,(currentTab)
        or a
        jr z,.input
        xor a
        jr .set_tab
.right
        ld a,(currentTab)
        or a
        jr nz,.input
        ld a,1
.set_tab
        ld (currentTab),a
        xor a
        ld (currentIndex),a
        ld (topIndex),a
        call draw_screen
        jp .input
.enter
        ld a,(currentTab)
        or a
        jr nz,.capture
        ld a,(currentIndex)
        or a
        call z,cycle_scheme
        ld a,(currentIndex)
        or a
        call nz,edit_colour
        call draw_screen
        jr .input
.capture
        call capture_key
        call draw_screen
        jr .input
.save
        ld ix,(ctxPtr)
        ld a,1
        ld (ix+SETTINGSCTX_RESULT),a
        jr .done
.cancel
        call restore_config
        jr .done
.bad_abi
        ld a,$7f
        ld (ix+SETTINGSCTX_ERROR),a
.done
        ld sp,(savedSp)
        ret


backup_config
        ld ix,(ctxPtr)
        ld l,(ix+SETTINGSCTX_KEYS)
        ld h,(ix+SETTINGSCTX_KEYS+1)
        ld de,BACKUP_KEYS
        ld bc,SETTINGS_ACTION_COUNT
        ldir
        ld l,(ix+SETTINGSCTX_PALETTE)
        ld h,(ix+SETTINGSCTX_PALETTE+1)
        ld de,BACKUP_COLOURS
        ld bc,SETTINGS_COLOUR_BYTES
        ldir
        ld l,(ix+SETTINGSCTX_SCHEME)
        ld h,(ix+SETTINGSCTX_SCHEME+1)
        ld a,(hl)
        ld (BACKUP_SCHEME),a
        ret

restore_config
        ld ix,(ctxPtr)
        ld e,(ix+SETTINGSCTX_KEYS)
        ld d,(ix+SETTINGSCTX_KEYS+1)
        ld hl,BACKUP_KEYS
        ld bc,SETTINGS_ACTION_COUNT
        ldir
        ld e,(ix+SETTINGSCTX_PALETTE)
        ld d,(ix+SETTINGSCTX_PALETTE+1)
        ld hl,BACKUP_COLOURS
        ld bc,SETTINGS_COLOUR_BYTES
        ldir
        ld l,(ix+SETTINGSCTX_SCHEME)
        ld h,(ix+SETTINGSCTX_SCHEME+1)
        ld a,(BACKUP_SCHEME)
        ld (hl),a
        jp apply_colours


draw_screen
        ld hl,BOX_X*256+BOX_Y
        ld bc,BOX_WIDTH*256+BOX_HEIGHT
        ld a,16
        call call_window
        ld b,BOX_X+2
        ld c,BOX_Y+1
        ld hl,titleText
        ld a,16
        call plot_string
        ld b,BOX_X+2
        ld c,BOX_Y+3
        ld hl,coloursTabIdle
        ld a,16
        call plot_string
        ld b,BOX_X+18
        ld c,BOX_Y+3
        ld hl,keysTabIdle
        ld a,16
        call plot_string
        ld a,(currentTab)
        or a
        ld b,BOX_X+2
        ld hl,coloursTabActive
        jr z,.active_tab
        ld b,BOX_X+18
        ld hl,keysTabActive
.active_tab
        ld c,BOX_Y+3
        ld a,64
        call plot_string
        ld b,LIST_X
        ld c,BOX_Y+5
        ld hl,colourHeader
        ld a,(currentTab)
        or a
        jr z,.header
        ld hl,keyHeader
.header
        ld a,16
        call plot_string
        ld b,LIST_X
        ld c,BOX_Y+BOX_HEIGHT-2
        ld hl,hintText
        ld a,16
        call plot_string
        jp draw_rows

draw_rows
        xor a
        ld (visibleRow),a
.loop
        call draw_one_row
        ld a,(visibleRow)
        inc a
        ld (visibleRow),a
        cp VISIBLE_ROWS
        jr nz,.loop
        ret

draw_one_row
        ld hl,LINE_BUFFER
        ld de,LINE_BUFFER+1
        ld bc,LIST_WIDTH-1
        ld (hl),' '
        ldir
        xor a
        ld (LINE_BUFFER+LIST_WIDTH),a
        ld a,(visibleRow)
        ld hl,topIndex
        add a,(hl)
        ld (drawIndex),a
        ld b,a
        call current_count
        cp b
        jr z,.plot
        jr c,.plot
        ld a,(currentIndex)
        cp b
        jr nz,.content
        ld a,'>'
        ld (LINE_BUFFER),a
.content
        ld a,(currentTab)
        or a
        call z,prepare_colour_row
        ld a,(currentTab)
        or a
        call nz,prepare_key_row
.plot
        ld b,LIST_X
        ld a,(visibleRow)
        add a,LIST_Y
        ld c,a
        ld hl,LINE_BUFFER
        ld a,16
        ld d,a
        ld a,(currentTab)
        or a
        jr nz,.normal_attr
        ld a,(drawIndex)
        or a
        jr z,.normal_attr
        dec a
        cp SETTINGS_STYLE_COUNT
        jr nc,.normal_attr
        add a,a
        add a,a
        add a,a
        add a,a
        jr .print
.normal_attr
        ld a,d
.print
        jp plot_string

prepare_colour_row
        ld a,(drawIndex)
        or a
        jp z,prepare_scheme_row
        dec a
        ld (rowStyle),a
        add a,a
        ld e,a
        ld d,0
        ld hl,styleNameTable
        add hl,de
        ld e,(hl)
        inc hl
        ld d,(hl)
        ex de,hl
        ld de,LINE_BUFFER+2
        ld b,23
        call copy_field
        ld hl,bgText
        ld de,LINE_BUFFER+27
        ld b,3
        call copy_field
        ld a,(rowStyle)
        call colour_ptr_for_style
        ld a,(rowStyle)
        add a,a
        ld de,LINE_BUFFER+30
        call format_rgb_at
        ld hl,fgText
        ld de,LINE_BUFFER+40
        ld b,3
        call copy_field
        ld a,(rowStyle)
        call colour_ptr_for_style
        inc hl
        ld a,(rowStyle)
        add a,a
        inc a
        ld de,LINE_BUFFER+43
        call format_rgb_at
        ret

prepare_scheme_row
        ld hl,schemeLabel
        ld de,LINE_BUFFER+2
        ld b,23
        call copy_field
        ld ix,(ctxPtr)
        ld l,(ix+SETTINGSCTX_SCHEME)
        ld h,(ix+SETTINGSCTX_SCHEME+1)
        ld a,(hl)
        cp SETTINGS_SCHEME_CUSTOM+1
        jr c,.valid
        ld a,SETTINGS_SCHEME_CUSTOM
.valid
        add a,a
        ld e,a
        ld d,0
        ld hl,schemeNameTable
        add hl,de
        ld e,(hl)
        inc hl
        ld d,(hl)
        ex de,hl
        ld de,LINE_BUFFER+27
        ld b,16
        jp copy_field

prepare_key_row
        ld a,(drawIndex)
        add a,a
        ld e,a
        ld d,0
        ld hl,actionNameTable
        add hl,de
        ld e,(hl)
        inc hl
        ld d,(hl)
        ex de,hl
        ld de,LINE_BUFFER+2
        ld b,30
        call copy_field
        call draw_key_value
        call format_key_name_entry
        ld hl,KEY_NAME_BUFFER
        ld de,LINE_BUFFER+36
        ld b,28
        jp copy_field


move_up
        ld a,(currentIndex)
        or a
        ret z
        dec a
        ld (currentIndex),a
        ld hl,topIndex
        cp (hl)
        ret nc
        ld (hl),a
        ret

move_down
        call current_count
        ld b,a
        ld a,(currentIndex)
        inc a
        cp b
        ret nc
        ld (currentIndex),a
        ld b,a
        ld a,(topIndex)
        add a,VISIBLE_ROWS
        cp b
        ret nz
        ld a,b
        sub VISIBLE_ROWS-1
        ld (topIndex),a
        ret

current_count
        ld a,(currentTab)
        or a
        ld a,COLOUR_ROW_COUNT
        ret z
        ld a,SETTINGS_ACTION_COUNT
        ret


; IN: A=style 0..15, OUT: HL=background RGB333 pair for that style.
colour_ptr_for_style
        add a,a
        ld e,a
        ld d,0
        ld ix,(ctxPtr)
        ld l,(ix+SETTINGSCTX_PALETTE)
        ld h,(ix+SETTINGSCTX_PALETTE+1)
        add hl,de
        ret


; IN: HL=RRRGGGBB byte, A=colour index 0..31, DE=output.
format_rgb_at
        ld (colourIndexWork),a
        ld c,(hl)
        ld a,c
        rlca
        rlca
        rlca
        and 7
        add a,'0'
        ld (de),a
        inc de
        ld a,'/'
        ld (de),a
        inc de
        ld a,c
        rrca
        rrca
        and 7
        add a,'0'
        ld (de),a
        inc de
        ld a,'/'
        ld (de),a
        inc de
        ld a,c
        and 3
        add a,a
        ld c,a
        ld a,(colourIndexWork)
        call colour_blue_bit
        or c
        add a,'0'
        ld (de),a
        ret


; IN: A=colour index 0..31. OUT: A=blue bit 0/1, other registers preserved.
colour_blue_bit
        push bc
        push de
        push hl
        ld b,a
        and 7
        ld e,a
        ld d,0
        ld hl,bitMasks
        add hl,de
        ld a,(hl)
        ld c,a
        ld a,b
        rrca
        rrca
        rrca
        and 3
        ld e,a
        ld d,0
        ld ix,(ctxPtr)
        ld l,(ix+SETTINGSCTX_PALETTE)
        ld h,(ix+SETTINGSCTX_PALETTE+1)
        ld a,l
        add a,SETTINGS_COLOUR_PRIMARY_BYTES
        ld l,a
        jr nc,.no_carry
        inc h
.no_carry
        add hl,de
        ld a,(hl)
        and c
        ld a,0
        jr z,.store
        inc a
.store
        ld (blueResult),a
        pop hl
        pop de
        pop bc
        ld a,(blueResult)
        ret


cycle_scheme
        ld ix,(ctxPtr)
        ld l,(ix+SETTINGSCTX_SCHEME)
        ld h,(ix+SETTINGSCTX_SCHEME+1)
        ld a,(hl)
        inc a
        cp SETTINGS_SCHEME_COUNT
        jr c,.selected
        xor a
.selected
        ld (selectedScheme),a
        ld (hl),a
        add a,a
        ld e,a
        ld d,0
        ld hl,presetDataTable
        add hl,de
        ld e,(hl)
        inc hl
        ld d,(hl)
        ex de,hl
        ld ix,(ctxPtr)
        ld e,(ix+SETTINGSCTX_PALETTE)
        ld d,(ix+SETTINGSCTX_PALETTE+1)
        ld bc,SETTINGS_COLOUR_BYTES
        ldir
        jp apply_colours


edit_colour
        ld a,(currentIndex)
        dec a
        ld (editingStyle),a
        xor a
        ld (editingComponent),a
        call draw_colour_editor
.input
        call read_key
        cp 1
        ret z
        cp 11
        jr z,.up
        cp 10
        jr z,.down
        cp 8
        jr z,.decrease
        cp 9
        jr z,.increase
        jr .input
.up
        ld a,(editingComponent)
        or a
        jr z,.input
        dec a
        ld (editingComponent),a
        call draw_colour_editor
        jr .input
.down
        ld a,(editingComponent)
        cp 5
        jr z,.input
        inc a
        ld (editingComponent),a
        call draw_colour_editor
        jr .input
.decrease
        xor a
        call adjust_component
        call draw_colour_editor
        jr .input
.increase
        ld a,1
        call adjust_component
        call draw_colour_editor
        jr .input


draw_colour_editor
        ld hl,11*256+7
        ld bc,58*256+18
        ld a,16
        call call_window
        ld b,13
        ld c,8
        ld hl,editTitle
        ld a,16
        call plot_string
        ld a,(editingStyle)
        add a,a
        ld e,a
        ld d,0
        ld hl,styleNameTable
        add hl,de
        ld e,(hl)
        inc hl
        ld d,(hl)
        ex de,hl
        ld b,29
        ld c,8
        ld a,16
        call plot_string
        xor a
        ld (drawComponent),a
.row
        ld a,(drawComponent)
        add a,a
        ld e,a
        ld d,0
        ld hl,componentNameTable
        add hl,de
        ld e,(hl)
        inc hl
        ld d,(hl)
        ex de,hl
        ld b,17
        ld a,(drawComponent)
        add a,11
        ld c,a
        ld a,(editingComponent)
        ld d,a
        ld a,(drawComponent)
        cp d
        ld a,16
        jr nz,.row_attr
        ld a,64
.row_attr
        ld (componentAttr),a
        call plot_string
        ld a,(drawComponent)
        call component_value
        add a,'0'
        ld (componentValueText),a
        ld b,48
        ld a,(drawComponent)
        add a,11
        ld c,a
        ld hl,componentValueText
        ld a,(componentAttr)
        call plot_string
        ld a,(drawComponent)
        inc a
        ld (drawComponent),a
        cp 6
        jr nz,.row
        ld b,17
        ld c,19
        ld hl,previewLabel
        ld a,16
        call plot_string
        ld b,29
        ld c,19
        ld hl,previewText
        ld a,(editingStyle)
        add a,a
        add a,a
        add a,a
        add a,a
        call plot_string
        ld b,15
        ld c,22
        ld hl,editorHint
        ld a,16
        jp plot_string


; IN: A=component 0..5. OUT: A=value 0..7.
component_value
        ld (componentWork),a
        call component_address
        ld b,a
        ld a,(hl)
        ld c,a
        ld a,b
        or a
        jr z,.red
        dec a
        jr z,.green
        ld a,c
        and 3
        add a,a
        ld c,a
        ld a,(editingColourIndex)
        call colour_blue_bit
        or c
        ret
.red
        ld a,c
        rlca
        rlca
        rlca
        and 7
        ret
.green
        ld a,c
        rrca
        rrca
        and 7
        ret


; Uses componentWork. OUT: HL=selected RGB pair, A=channel 0..2.
component_address
        ld a,(editingStyle)
        add a,a
        ld (editingColourIndex),a
        ld a,(editingStyle)
        call colour_ptr_for_style
        ld a,(componentWork)
        cp 3
        jr nc,.background
        inc hl
        ld a,(editingColourIndex)
        inc a
        ld (editingColourIndex),a
        ld a,(componentWork)
        ret
.background
        sub 3
        ret


; IN: A=0 decrease, A=1 increase.
adjust_component
        ld (adjustDirection),a
        ld a,(editingComponent)
        ld (componentWork),a
        call component_value
        ld b,a
        ld a,(adjustDirection)
        or a
        jr z,.less
        ld a,b
        cp 7
        ret z
        inc b
        jr .store
.less
        ld a,b
        or a
        ret z
        dec b
.store
        ld a,(editingComponent)
        ld (componentWork),a
        call component_address
        ld c,a
        ld a,c
        or a
        jr z,.store_red
        dec a
        jr z,.store_green
        ld a,(hl)
        and $fc
        ld c,a
        ld a,b
        rrca
        and 3
        or c
        ld (hl),a
        ld a,b
        and 1
        ld b,a
        ld a,(editingColourIndex)
        call set_colour_blue_bit
        jr .changed
.store_red
        ld a,(hl)
        and $1f
        ld c,a
        ld a,b
        rrca
        rrca
        rrca
        or c
        ld (hl),a
        jr .changed
.store_green
        ld a,(hl)
        and $e3
        ld c,a
        ld a,b
        add a,a
        add a,a
        or c
        ld (hl),a
.changed
        ld ix,(ctxPtr)
        ld l,(ix+SETTINGSCTX_SCHEME)
        ld h,(ix+SETTINGSCTX_SCHEME+1)
        ld (hl),SETTINGS_SCHEME_CUSTOM
        jp apply_colours


; IN: A=colour index, B=blue bit 0/1.
set_colour_blue_bit
        ld (blueSetValue),a
        push hl
        push de
        ld a,(blueSetValue)
        ld c,a
        and 7
        ld e,a
        ld d,0
        ld hl,bitMasks
        add hl,de
        ld a,(hl)
        ld (blueMaskWork),a
        ld a,c
        rrca
        rrca
        rrca
        and 3
        ld e,a
        ld d,0
        ld ix,(ctxPtr)
        ld l,(ix+SETTINGSCTX_PALETTE)
        ld h,(ix+SETTINGSCTX_PALETTE+1)
        ld a,l
        add a,SETTINGS_COLOUR_PRIMARY_BYTES
        ld l,a
        jr nc,.no_carry
        inc h
.no_carry
        add hl,de
        ld a,b
        or a
        ld a,(blueMaskWork)
        jr z,.clear
        or (hl)
        ld (hl),a
        jr .done
.clear
        cpl
        and (hl)
        ld (hl),a
.done
        pop de
        pop hl
        ret


selected_key_ptr
        ld ix,(ctxPtr)
        ld l,(ix+SETTINGSCTX_KEYS)
        ld h,(ix+SETTINGSCTX_KEYS+1)
        ld a,(currentIndex)
        ld e,a
        ld d,0
        add hl,de
        ret

draw_key_value
        ld ix,(ctxPtr)
        ld l,(ix+SETTINGSCTX_KEYS)
        ld h,(ix+SETTINGSCTX_KEYS+1)
        ld a,(drawIndex)
        ld e,a
        ld d,0
        add hl,de
        ld a,(hl)
        ret

capture_key
        ld b,LIST_X
        ld c,BOX_Y+BOX_HEIGHT-2
        ld hl,captureText
        ld a,48
        call plot_string
.read
        call read_key
        cp 1
        ret z
        ld (capturedKey),a
        call selected_key_ptr
        ld (selectedKeyPtr),hl
        ld a,(capturedKey)
        cp (hl)
        ret z
        call key_already_used
        jr c,.conflict
        ld hl,(selectedKeyPtr)
        ld a,(capturedKey)
        ld (hl),a
        ret
.conflict
        ld b,LIST_X
        ld c,BOX_Y+BOX_HEIGHT-2
        ld hl,keyConflictText
        ld a,48
        call plot_string
        ld a,(capturedKey)
        call format_key_name_entry
        ld b,LIST_X+23
        ld c,BOX_Y+BOX_HEIGHT-2
        ld hl,KEY_NAME_BUFFER
        ld a,48
        call plot_string
        jr .read


; Carry is set when capturedKey is already assigned to another action.
key_already_used
        ld ix,(ctxPtr)
        ld l,(ix+SETTINGSCTX_KEYS)
        ld h,(ix+SETTINGSCTX_KEYS+1)
        ld de,(selectedKeyPtr)
        ld b,SETTINGS_ACTION_COUNT
.scan
        ld a,h
        cp d
        jr nz,.compare
        ld a,l
        cp e
        jr z,.next
.compare
        ld a,(capturedKey)
        cp (hl)
        jr z,.used
.next
        inc hl
        djnz .scan
        or a
        ret
.used
        scf
        ret


; A=INKEY code -> readable zero-terminated key name.
format_key_name
        ld de,KEY_NAME_BUFFER
        cp 1
        ld hl,keyBreak
        jp z,.copy_known
        cp 4
        ld hl,keyCs3
        jp z,.copy_known
        cp 5
        ld hl,keyCs4
        jp z,.copy_known
        cp 6
        ld hl,keyCs2
        jp z,.copy_known
        cp 7
        ld hl,keyCs1
        jp z,.copy_known
        cp 8
        ld hl,keyCs5
        jp z,.copy_known
        cp 9
        ld hl,keyCs8
        jp z,.copy_known
        cp 10
        ld hl,keyCs6
        jp z,.copy_known
        cp 11
        ld hl,keyCs7
        jp z,.copy_known
        cp 12
        ld hl,keyDelete
        jp z,.copy_known
        cp 13
        ld hl,keyEnter
        jp z,.copy_known
        cp 15
        ld hl,keyCs9
        jp z,.copy_known
        cp 32
        ld hl,keySpace
        jp z,.copy_known
        cp 127
        ld hl,keySsI
        jp z,.copy_known
        cp 199
        ld hl,keySsQ
        jp z,.copy_known
        cp 200
        ld hl,keySsE
        jp z,.copy_known
        cp 201
        ld hl,keySsW
        jp z,.copy_known
        cp 'A'
        jr c,.plain_or_code
        cp 'Z'+1
        jr nc,.plain_or_code
        ld hl,keyCapsPrefix
        call copy_zero_string
        dec de
        ld a,(formattedKey)
        ld (de),a
        inc de
        xor a
        ld (de),a
        ret
.plain_or_code
        cp 32
        jr c,.code
        cp 127
        jr nc,.code
        cp 'a'
        jr c,.store_plain
        cp 'z'+1
        jr nc,.store_plain
        sub 32
.store_plain
        ld (de),a
        inc de
        xor a
        ld (de),a
        ret
.code
        push af
        ld a,'$'
        ld (de),a
        inc de
        pop af
        push af
        rrca
        rrca
        rrca
        rrca
        call hex_digit
        ld (de),a
        inc de
        pop af
        call hex_digit
        ld (de),a
        inc de
        xor a
        ld (de),a
        ret
.copy_known
        jp copy_zero_string

; Preserve the original character for the CAPS+ branch above.
; This tiny entry wrapper avoids keeping it live across string copying.
format_key_name_entry
        ld (formattedKey),a
        jp format_key_name

copy_zero_string
        ld a,(hl)
        ld (de),a
        inc hl
        inc de
        or a
        jr nz,copy_zero_string
        ret

hex_digit
        and $0f
        add a,'0'
        cp '9'+1
        ret c
        add a,'A'-'9'-1
        ret

copy_field
        ld a,(hl)
        or a
        ret z
        ld (de),a
        inc hl
        inc de
        djnz copy_field
        ret


; IN: B=x, C=y, HL=text, A=legacy palette attribute.
plot_string
        ex de,hl
        ld h,b
        ld l,c
        jp call_print


; Upload the packed theme directly. The plugin owns MMU6/MMU7 while open, so
; a resident callback is unnecessary; NextRegs remain globally accessible.
apply_colours
        push af
        push bc
        push de
        push hl
        push ix
        nextreg $43,%0'011'0000
        ld ix,(ctxPtr)
        ld l,(ix+SETTINGSCTX_PALETTE)
        ld h,(ix+SETTINGSCTX_PALETTE+1)
        push hl
        ld de,SETTINGS_COLOUR_PRIMARY_BYTES
        add hl,de
        push hl
        pop ix
        pop hl
        ld d,(ix+0)
        ld e,1
        ld b,SETTINGS_STYLE_COUNT
        ld c,0
.group
        ld a,c
        nextreg $40,a
        call .write_colour
        ld a,c
        add a,3
        nextreg $40,a
        call .write_colour
        ld a,c
        add a,16
        ld c,a
        djnz .group
        pop ix
        pop hl
        pop de
        pop bc
        pop af
        ret
.write_colour
        ld a,(hl)
        inc hl
        nextreg $44,a
        ld a,d
        and e
        jr z,.blue_zero
        ld a,1
.blue_zero
        nextreg $44,a
        rlc e
        ret nc
        inc ix
        ld d,(ix+0)
        ret

patch_services
        ld ix,(svcPtr)
        ld l,(ix+SETTINGS_SERVICE_PRINT)
        ld h,(ix+SETTINGS_SERVICE_PRINT+1)
        ld (call_print+1),hl
        ld l,(ix+SETTINGS_SERVICE_WINDOW)
        ld h,(ix+SETTINGS_SERVICE_WINDOW+1)
        ld (call_window+1),hl
        ld l,(ix+SETTINGS_SERVICE_KEYSCAN)
        ld h,(ix+SETTINGS_SERVICE_KEYSCAN+1)
        ld (call_keyscan+1),hl
        ld l,(ix+SETTINGS_SERVICE_SYMTAB)
        ld h,(ix+SETTINGS_SERVICE_SYMTAB+1)
        ld (symTablePtr),hl
        ld l,(ix+SETTINGS_SERVICE_CAPSTAB)
        ld h,(ix+SETTINGS_SERVICE_CAPSTAB+1)
        ld (capsTablePtr),hl
        ld l,(ix+SETTINGS_SERVICE_NORMTAB)
        ld h,(ix+SETTINGS_SERVICE_NORMTAB+1)
        ld (normTablePtr),hl
        ret

call_print
        jp 0
call_window
        jp 0
call_keyscan
        jp 0

; Use Calm Commander's resident KEYSCAN and its resident decoder tables.
; The full INKEY entry also services the mouse and can call banked UI code,
; so the plugin uses the safe keyboard core exposed by the ABI instead.
read_key
        ei
        ld b,2
.delay
        halt
        djnz .delay
        call call_keyscan
        ld a,e
        inc a
        jr z,read_key
        ld a,d
        ld hl,(symTablePtr)
        cp $18
        jr z,.decode
        ld hl,(capsTablePtr)
        cp $27
        jr z,.decode
        ld hl,(normTablePtr)
.decode
        ld d,0
        add hl,de
        ld a,(hl)
        or a
        jr z,read_key
        push af
.release
        ei
        halt
        call call_keyscan
        ld a,e
        inc a
        jr nz,.release
        pop af
        ret


titleText        defb " Settings",0
coloursTabIdle   defb "  Colours  ",0
coloursTabActive defb "[ Colours ]",0
keysTabIdle      defb "  Keys  ",0
keysTabActive    defb "[ Keys ]",0
colourHeader     defb "  Interface style          BG R/G/B   FG R/G/B",0
keyHeader        defb "  Action                              Shortcut",0
hintText         defb "LEFT/RIGHT tab  UP/DOWN move  ENTER select/edit  S = save",0
captureText      defb "Press new shortcut (BREAK cancels capture)                         ",0
keyConflictText  defb "Shortcut already used:                                               ",0
schemeLabel      defb "Colour scheme",0
bgText           defb "BG ",0
fgText           defb "FG ",0

schemeNameTable
        defw schemeDefault,schemeLight,schemeDark,schemeCustom
schemeDefault defb "Default",0
schemeLight   defb "Light",0
schemeDark    defb "Dark",0
schemeCustom  defb "Custom",0

styleNameTable
        defw styleNormal,styleDialog,styleCursor,styleButton,styleMenuSelect
        defw styleMarkedCursor,styleMarked,styleDirectory,styleExecutable
        defw styleViewer,stylePlugin1,stylePlugin2,stylePlugin3
        defw stylePluginGreen,stylePluginYellow,stylePluginRed
styleNameTableEnd
        assert styleNameTableEnd-styleNameTable = SETTINGS_STYLE_COUNT*2
styleNormal       defb "Normal files",0
styleDialog       defb "Dialogs and title",0
styleCursor       defb "Panel cursor",0
styleButton       defb "Buttons and prompts",0
styleMenuSelect   defb "Menu selection",0
styleMarked       defb "Marked files",0
styleMarkedCursor defb "Marked cursor",0
styleDirectory    defb "Directories",0
styleExecutable   defb "Executable files",0
styleViewer       defb "Viewer and editor",0
stylePlugin1      defb "Plugin colour 1",0
stylePlugin2      defb "Plugin colour 2",0
stylePlugin3      defb "Plugin colour 3",0
stylePluginGreen  defb "Plugin title / green",0
stylePluginYellow defb "Plugin warning / yellow",0
stylePluginRed    defb "Plugin error / red",0

editTitle        defb "Edit colours:",0
componentNameTable
        defw componentFgR,componentFgG,componentFgB
        defw componentBgR,componentBgG,componentBgB
componentFgR defb "Foreground red",0
componentFgG defb "Foreground green",0
componentFgB defb "Foreground blue",0
componentBgR defb "Background red",0
componentBgG defb "Background green",0
componentBgB defb "Background blue",0
previewLabel defb "Preview:",0
previewText  defb " Calm Commander colour preview ",0
editorHint   defb "UP/DOWN component  LEFT/RIGHT value  BREAK done",0
componentValueText defb "0",0

presetDataTable
        defw presetDefaultColours,presetLightColours,presetDarkColours
presetDefaultColours
        EMIT_SETTINGS_DEFAULT_COLOURS
presetLightColours
        db $ff,$00,$db,$01,$57,$00,$0a,$ff
        db $33,$ff,$f4,$00,$f9,$24,$ff,$12
        db $ff,$34,$ff,$25,$ff,$b0,$ff,$14
        db $ff,$12,$ff,$14,$ff,$d0,$ff,$c0
        db $d7,$d6,$d5,$5f
presetDarkColours
        db $00,$db,$01,$ff,$0f,$ff,$b7,$00
        db $33,$ff,$d0,$00,$68,$f9,$00,$3f
        db $00,$5c,$05,$db,$00,$fc,$00,$3d
        db $00,$3b,$00,$1c,$00,$fc,$00,$e4
        db $2b,$c2,$5b,$df

actionNameTable
        defw actSysInfo,actDown,actUp,actPageDown,actPageUp,actSwitch,actEnter,actDelete
        defw actParent,actRename,actMenu,actCopy,actMove,actMark,actMkdir,actDriveL
        defw actDriveR,actSelect,actInvert,actDeselect,actSearch,actLeftPanel
        defw actRightPanel,actView,actEdit,actPlugins,actBookmarkAdd,actBookmarkList
        defw actHelp,actAttr,actFileInfo,actQuit,actSettings
actionNameTableEnd
        assert actionNameTableEnd-actionNameTable = SETTINGS_ACTION_COUNT*2
actSysInfo      defb "About Calm Commander",0
actDown         defb "Cursor down",0
actUp           defb "Cursor up",0
actPageDown     defb "Page down",0
actPageUp       defb "Page up",0
actSwitch       defb "Switch panel",0
actEnter        defb "Open / Enter",0
actDelete       defb "Delete",0
actParent       defb "Parent directory",0
actRename       defb "Rename",0
actMenu         defb "Menu",0
actCopy         defb "Copy",0
actMove         defb "Move",0
actMark         defb "Mark file",0
actMkdir        defb "Create directory",0
actDriveL       defb "Left drive",0
actDriveR       defb "Right drive",0
actSelect       defb "Select by mask",0
actInvert       defb "Invert selection",0
actDeselect     defb "Deselect by mask",0
actSearch       defb "Search",0
actLeftPanel    defb "Activate left panel",0
actRightPanel   defb "Activate right panel",0
actView         defb "View file",0
actEdit         defb "Edit file",0
actPlugins      defb "Plugin menu",0
actBookmarkAdd  defb "Add bookmark",0
actBookmarkList defb "Show bookmarks",0
actHelp         defb "Help",0
actAttr         defb "Change attributes",0
actFileInfo     defb "File info",0
actQuit         defb "Quit",0
actSettings     defb "Settings",0

keyBreak       defb "BREAK",0
keyCs1         defb "CAPS+1",0
keyCs2         defb "CAPS+2",0
keyCs3         defb "CAPS+3",0
keyCs4         defb "CAPS+4",0
keyCs5         defb "CAPS+5",0
keyCs6         defb "CAPS+6",0
keyCs7         defb "CAPS+7",0
keyCs8         defb "CAPS+8",0
keyCs9         defb "CAPS+9",0
keyDelete      defb "DELETE",0
keyEnter       defb "ENTER",0
keySpace       defb "SPACE",0
keySsI         defb "SS+I",0
keySsQ         defb "SS+Q",0
keySsE         defb "SS+E",0
keySsW         defb "SS+W",0
keyCapsPrefix  defb "CAPS+",0

ctxPtr         defw 0
svcPtr         defw 0
savedSp        defw 0
currentTab     defb 0
currentIndex   defb 0
topIndex       defb 0
visibleRow     defb 0
drawIndex      defb 0
rowStyle       defb 0
capturedKey    defb 0
selectedKeyPtr defw 0
formattedKey   defb 0
selectedScheme defb 0
editingStyle   defb 0
editingComponent defb 0
editingColourIndex defb 0
drawComponent defb 0
componentWork defb 0
componentAttr defb 0
adjustDirection defb 0
colourIndexWork defb 0
blueResult defb 0
blueSetValue defb 0
blueMaskWork defb 0
bitMasks defb 1,2,4,8,16,32,64,128
symTablePtr    defw 0
capsTablePtr   defw 0
normTablePtr   defw 0

plugin_end
        assert plugin_end-plugin_start <= SETTINGS_PLUGIN_SIZE
        SAVEBIN "plugin/settings.ccp",SETTINGS_PLUGIN_ADDRESS,SETTINGS_PLUGIN_SIZE
