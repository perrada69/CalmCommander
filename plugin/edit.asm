        DEVICE ZXSPECTRUMNEXT
        org VIEW_PLUGIN_ADDRESS

        include "plugin_api.i.asm"

SCREEN_BASE     equ $4000
TEXT_TOP        equ SCREEN_BASE+160*3+2
TEXT_ROWS       equ 27
TEXT_COLS       equ 78
HEX_BYTES_ROW   equ 16
STATUS_POS      equ SCREEN_BASE+160*31+2
SEARCH_MAX      equ 32
NAME_MAX        equ 48

plugin_start
        ld (ctxPtr),hl
        ld (svcPtr),de
        call patch_services
        call init_context

        ld hl,0 * 256 + 0
        ld bc,78 * 256 + 30
        ld a,16
        call call_window
        ld hl,2*256+1
        ld a,16
        ld de,title
        call call_print
        ld ix,(ctxPtr)
        ld e,(ix+VIEWCTX_FILENAME)
        ld d,(ix+VIEWCTX_FILENAME+1)
        ld hl,15*256+1
        ld a,16
        call call_print

        call render
.input
        call call_input
        or a
        jr z,.input
        cp 1
        jp z,try_exit
        cp 10
        jp z,move_down
        cp 11
        jp z,move_up
        cp 9
        jp z,move_right
        cp 8
        jp z,move_left
        cp 12
        jp z,backspace_key
        cp 132
        jp z,find_prompt
        cp 128
        jp z,save_current
        cp 129
        jp z,save_as
        cp 130
        jp z,hex_mode
        cp 131
        jp z,text_mode
        ld b,a
        ld a,(viewMode)
        or a
        ld a,b
        jp nz,hex_key
        cp 13
        jp z,write_text_char
        cp 32
        jp c,.input
        cp 128
        jp nc,.input
write_text_char
        call key_beep
        ld hl,(curOffset)
        ld (oldOffset),hl
        call insert_current_byte
        call mark_dirty
        call inc_cursor
        call ensure_cursor_visible
        call render
        call key_repeat_delay
        jp plugin_start.input

text_mode
        xor a
        ld (viewMode),a
        call render
        jp plugin_start.input

hex_mode
        ld a,1
        ld (viewMode),a
        ld a,$ff
        ld (hexPending),a
        call render
        jp plugin_start.input

move_right
        ld hl,(curOffset)
        ld (oldOffset),hl
        call inc_cursor
        call cursor_moved
        call key_beep
        call arrow_repeat_delay
        jp plugin_start.input

move_left
        ld hl,(curOffset)
        ld a,h
        or l
        jp z,plugin_start.input
        ld (oldOffset),hl
        dec hl
        ld (curOffset),hl
        call cursor_moved
        call key_beep
        call arrow_repeat_delay
        jp plugin_start.input

backspace_key
        ld hl,(loadedSize)
        ld a,h
        or l
        jp z,plugin_start.input
        ld hl,(curOffset)
        ld a,h
        or l
        jr nz,.move_before
        ld hl,(loadedSize)
        dec hl
        ld a,h
        or l
        jp nz,plugin_start.input
        ld hl,0
        jr .delete
.move_before
        dec hl
.delete
        ld (curOffset),hl
        ld (oldOffset),hl
        ld a,$ff
        ld (hexPending),a
        call delete_current_byte
        call mark_dirty
        call ensure_cursor_visible
        call render
        call key_beep
        call key_repeat_delay
        jp plugin_start.input

move_down
        ld a,(viewMode)
        or a
        jr z,.text
        ld hl,(curOffset)
        ld (oldOffset),hl
        ld de,HEX_BYTES_ROW
        add hl,de
        call clamp_to_last
        ld (curOffset),hl
        call render
        call key_beep
        call arrow_repeat_delay
        jp plugin_start.input
.text
        ld hl,(curOffset)
        ld (oldOffset),hl
        call text_move_down
        call cursor_changed
        jp z,plugin_start.input
        call cursor_moved
        call key_beep
        call arrow_repeat_delay
        jp plugin_start.input

move_up
        ld a,(viewMode)
        or a
        jr z,.text
        ld hl,(curOffset)
        ld (oldOffset),hl
        ld de,HEX_BYTES_ROW
        or a
        sbc hl,de
        jr nc,.store
        ld hl,0
.store
        ld (curOffset),hl
        call render
        call key_beep
        call arrow_repeat_delay
        jp plugin_start.input
.text
        ld hl,(curOffset)
        ld (oldOffset),hl
        call text_move_up
        call cursor_changed
        jp z,plugin_start.input
        call cursor_moved
        call key_beep
        call arrow_repeat_delay
        jp plugin_start.input

try_exit
        ld a,(dirty)
        or a
        ret z
        call status_dirty
.wait
        call call_input
        or a
        jr z,.wait
        cp "i"
        ret z
        cp "I"
        ret z
        cp "s"
        jp z,save_current_then_exit
        cp "S"
        jp z,save_current_then_exit
        cp "e"
        jp z,save_as_then_exit
        cp "E"
        jp z,save_as_then_exit
        cp 1
        jp z,render
        jr .wait

save_current_then_exit
        call save_current_raw
        ret

save_as_then_exit
        call save_as_raw
        ret

save_current
        call save_current_raw
        jp plugin_start.input

save_current_raw
        ld ix,(ctxPtr)
        ld l,(ix+VIEWCTX_FILENAME)
        ld h,(ix+VIEWCTX_FILENAME+1)
        jr save_name_hl

save_as
        call save_as_raw
        jp plugin_start.input

save_as_raw
        call copy_current_filename_to_save_name
        ld de,saveName
        ld b,NAME_MAX
        ld hl,saveAsText
        call line_input
        cp 1
        ret z
        ld hl,saveName
        ld a,(hl)
        or a
        ret z
save_name_hl
        call status_saving
        ld de,0
        ld bc,(loadedSize)
        call call_extract
        ret c
        xor a
        ld (dirty),a
        ld ix,(ctxPtr)
        ld (ix+VIEWCTX_DIRTY),1
        call status_saved
        ret

copy_current_filename_to_save_name
        ld ix,(ctxPtr)
        ld l,(ix+VIEWCTX_FILENAME)
        ld h,(ix+VIEWCTX_FILENAME+1)
        ld de,saveName
        ld b,NAME_MAX
.copy
        ld a,(hl)
        cp 255
        jr z,.done
        or a
        jr z,.done
        ld (de),a
        inc hl
        inc de
        djnz .copy
.done
        xor a
        ld (de),a
        ret

find_prompt
        ld de,searchBuf
        ld b,SEARCH_MAX
        ld hl,findText
        call line_input
        cp 1
        jp z,render
        ld hl,searchBuf
        ld a,(hl)
        or a
        jp z,render
        call find_next
        call render
        jp plugin_start.input

find_next
        ld hl,(curOffset)
        ld (scanOffset),hl
.scan
        ld hl,(scanOffset)
        call offset_at_end
        jr nc,.not_found
        call compare_here
        jr z,.found
        ld hl,(scanOffset)
        inc hl
        ld (scanOffset),hl
        jr .scan
.found
        ld hl,(scanOffset)
        ld (curOffset),hl
        call ensure_cursor_visible
        ret
.not_found
        call status_not_found
        ret

compare_here
        ld hl,(scanOffset)
        ld (matchOffset),hl
        ld de,searchBuf
.loop
        ld a,(de)
        or a
        ret z
        ld c,a
        ld hl,(matchOffset)
        call offset_at_end
        jr nc,.fail
        call read_byte_at_offset
        cp c
        jr nz,.fail
        inc de
        ld hl,(matchOffset)
        inc hl
        ld (matchOffset),hl
        jr .loop
.fail
        or 1
        ret

hex_key
        call hex_value
        jp c,plugin_start.input
        call key_beep
        ld c,a
        ld a,(hexPending)
        cp $ff
        jr nz,.low
        ld a,c
        add a,a
        add a,a
        add a,a
        add a,a
        ld (hexPending),a
        call render
        call key_repeat_delay
        jp plugin_start.input
.low
        or c
        call write_current_byte
        ld a,$ff
        ld (hexPending),a
        call mark_dirty
        call inc_cursor
        call render
        call key_repeat_delay
        jp plugin_start.input

hex_value
        cp "0"
        jr c,.bad
        cp "9"+1
        jr c,.num
        cp "A"
        jr c,.lower
        cp "F"+1
        jr c,.upper
.lower
        cp "a"
        jr c,.bad
        cp "f"+1
        jr nc,.bad
        sub "a"-10
        or a
        ret
.upper
        sub "A"-10
        or a
        ret
.num
        sub "0"
        or a
        ret
.bad
        scf
        ret

render
        ld a,(viewMode)
        or a
        call z,render_text
        ld a,(viewMode)
        or a
        call nz,render_hex
        call render_status
        ret

render_text
        ld hl,(viewOffset)
        ld (renderOffset),hl
        ld de,TEXT_TOP
        ld b,TEXT_ROWS
.row
        push bc
        ld b,TEXT_COLS
.col
        ld hl,(renderOffset)
        call offset_at_end
        jr nc,.fill_row
        call set_cursor_attr
        call read_byte_at_offset
        cp 13
        jr z,.newline
        cp 10
        jr z,.newline
        cp 9
        jr nz,.not_tab
        ld a,32
.not_tab
        cp 32
        jr c,.dot
        cp 127
        jr c,.char
.dot
        ld a,"."
        jr .char
.char
        ld (de),a
        inc de
        ld a,(cellAttr)
        ld (de),a
        inc de
        ld hl,(renderOffset)
        inc hl
        ld (renderOffset),hl
        djnz .col
        jr .row_done
.newline
        ld a,32
        ld (de),a
        inc de
        ld a,(cellAttr)
        ld (de),a
        inc de
        push bc
        call advance_newline
        pop bc
        dec b
.fill_row
        ld a,b
        or a
        jr z,.row_done
.fill_loop
        ld a,32
        ld (de),a
        inc de
        ld a,16
        ld (de),a
        inc de
        djnz .fill_loop
.row_done
        ld hl,160-TEXT_COLS*2
        add hl,de
        ex de,hl
        pop bc
        djnz .row
        ret

set_cursor_attr
        push de
        push hl
        ld de,(curOffset)
        or a
        sbc hl,de
        ld a,16
        jr nz,.store
        ld a,64
.store
        ld (cellAttr),a
        pop hl
        pop de
        ret

advance_newline
        ld hl,(renderOffset)
        call read_byte_at_offset
        ld (newlineChar),a
        ld hl,(renderOffset)
        inc hl
        ld (renderOffset),hl
        ld a,(newlineChar)
        cp 13
        ret nz
        ld hl,(renderOffset)
        call offset_at_end
        ret nc
        call read_byte_at_offset
        cp 10
        ret nz
        ld hl,(renderOffset)
        inc hl
        ld (renderOffset),hl
        ret

cursor_moved
        ld a,(viewMode)
        or a
        jr nz,.full
        ld hl,(oldOffset)
        call set_offset_normal_attr
        jr c,.full
        ld hl,(curOffset)
        call set_offset_cursor_attr
        jr c,.full
        ret
.full
        call ensure_cursor_visible
        call render
        ret

ensure_cursor_visible
        ld hl,(curOffset)
        call find_screen_attr_for_offset
        ret nc
        ld hl,(curOffset)
        ld de,(viewOffset)
        or a
        sbc hl,de
        jr c,.above
.scroll_down
        call advance_view_one_line
        jr c,.above
        ld hl,(curOffset)
        call find_screen_attr_for_offset
        ret nc
        jr .scroll_down
.above
        call find_current_line_start
        ld (viewOffset),hl
        ret

advance_view_one_line
        ld hl,(viewOffset)
        ld (lineStart),hl
        call find_next_line_start
        ret c
        ld (viewOffset),hl
        or a
        ret

set_offset_normal_attr
        call find_screen_attr_for_offset
        ret c
        ld (hl),16
        ret

set_offset_cursor_attr
        call find_screen_attr_for_offset
        ret c
        ld (hl),64
        ret

find_screen_attr_for_offset
        ld (targetOffset),hl
        ld hl,(viewOffset)
        ld (scanOffset),hl
        ld de,TEXT_TOP
        ld b,TEXT_ROWS
.row
        push bc
        ld b,TEXT_COLS
.col
        ld hl,(scanOffset)
        push de
        ld de,(targetOffset)
        or a
        sbc hl,de
        pop de
        jr z,.found
        ld hl,(scanOffset)
        call offset_at_end
        jr nc,.not_found_pop
        call read_byte_at_offset
        cp 13
        jr z,.newline
        cp 10
        jr z,.newline
        ld hl,(scanOffset)
        inc hl
        ld (scanOffset),hl
        inc de
        inc de
        djnz .col
        jr .row_done
.newline
        call advance_scan_newline
        ld a,b
        or a
        jr z,.row_done
.skip_rest
        inc de
        inc de
        djnz .skip_rest
        jr .row_done
.found
        inc de
        ex de,hl
        pop bc
        or a
        ret
.not_found_pop
        pop bc
        scf
        ret
.row_done
        ld hl,160-TEXT_COLS*2
        add hl,de
        ex de,hl
        pop bc
        djnz .row
        scf
        ret

advance_scan_newline
        ld hl,(scanOffset)
        call read_byte_at_offset
        ld (newlineChar),a
        ld hl,(scanOffset)
        inc hl
        ld (scanOffset),hl
        ld a,(newlineChar)
        cp 13
        ret nz
        ld hl,(scanOffset)
        call offset_at_end
        ret nc
        call read_byte_at_offset
        cp 10
        ret nz
        ld hl,(scanOffset)
        inc hl
        ld (scanOffset),hl
        ret

text_move_down
        call get_current_column
        call find_current_line_start
        ld (lineStart),hl
        call find_next_line_start
        ret c
        jp move_to_column_from_hl

text_move_up
        call get_current_column
        call find_current_line_start
        ld a,h
        or l
        ret z
        ld (lineStart),hl
        call find_previous_line_start
        jp move_to_column_from_hl

get_current_column
        call find_current_line_start
        ld de,(curOffset)
        or a
        ex de,hl
        sbc hl,de
        ld a,l
        ld (desiredCol),a
        ret

find_current_line_start
        ld hl,0
        ld (scanOffset),hl
        ld (lineStart),hl
.loop
        ld hl,(scanOffset)
        ld de,(curOffset)
        or a
        sbc hl,de
        jr nc,.done
        ld hl,(scanOffset)
        call offset_at_end
        jr nc,.done
        call read_byte_at_offset
        cp 13
        jr z,.newline
        cp 10
        jr z,.newline
        ld hl,(scanOffset)
        inc hl
        ld (scanOffset),hl
        jr .loop
.newline
        call advance_scan_newline
        ld hl,(scanOffset)
        ld (lineStart),hl
        jr .loop
.done
        ld hl,(lineStart)
        ret

find_next_line_start
        ld hl,(lineStart)
        ld (scanOffset),hl
.loop
        ld hl,(scanOffset)
        call offset_at_end
        jr nc,.fail
        call read_byte_at_offset
        cp 13
        jr z,.newline
        cp 10
        jr z,.newline
        ld hl,(scanOffset)
        inc hl
        ld (scanOffset),hl
        jr .loop
.newline
        call advance_scan_newline
        ld hl,(scanOffset)
        call offset_at_end
        jr nc,.fail
        ld hl,(scanOffset)
        or a
        ret
.fail
        scf
        ret

find_previous_line_start
        ld hl,0
        ld (scanOffset),hl
        ld (prevLineStart),hl
.loop
        ld hl,(scanOffset)
        ld de,(lineStart)
        or a
        sbc hl,de
        jr nc,.done
        ld hl,(prevLineStart)
        ld (targetOffset),hl
        ld hl,(scanOffset)
        ld (prevLineStart),hl
.seek_eol
        ld hl,(scanOffset)
        ld de,(lineStart)
        or a
        sbc hl,de
        jr nc,.done
        ld hl,(scanOffset)
        call offset_at_end
        jr nc,.done
        call read_byte_at_offset
        cp 13
        jr z,.newline
        cp 10
        jr z,.newline
        ld hl,(scanOffset)
        inc hl
        ld (scanOffset),hl
        jr .seek_eol
.newline
        call advance_scan_newline
        jr .loop
.done
        ld hl,(targetOffset)
        ret

move_to_column_from_hl
        ld (scanOffset),hl
        ld a,(desiredCol)
        ld b,a
.loop
        ld a,b
        or a
        jr z,.store
        ld hl,(scanOffset)
        call offset_at_end
        jr nc,.store
        call read_byte_at_offset
        cp 13
        jr z,.store
        cp 10
        jr z,.store
        ld hl,(scanOffset)
        inc hl
        ld (scanOffset),hl
        djnz .loop
.store
        ld hl,(scanOffset)
        ld (curOffset),hl
        ret

render_hex
        ld hl,(curOffset)
        ld (renderOffset),hl
        ld de,TEXT_TOP
        ld b,TEXT_ROWS
.row
        push bc
        ld b,HEX_BYTES_ROW
.bytes
        ld hl,(renderOffset)
        call offset_at_end
        jr nc,.blank
        call read_byte_at_offset
        call put_hex_byte
        jr .next
.blank
        ld a,32
        call put_pair
        call put_pair
.next
        ld a,32
        call put_cell
        ld hl,(renderOffset)
        inc hl
        ld (renderOffset),hl
        djnz .bytes
        ld hl,160-(HEX_BYTES_ROW*3*2)
        add hl,de
        ex de,hl
        pop bc
        djnz .row
        ld hl,TEXT_TOP+1
        ld a,64
        ld (hl),a
        ld hl,TEXT_TOP+3
        ld (hl),a
        ret

put_hex_byte
        push af
        rrca
        rrca
        rrca
        rrca
        call put_hex_nibble
        pop af
put_hex_nibble
        and $0f
        add a,"0"
        cp "9"+1
        jr c,put_cell
        add a,7
put_cell
        ld (de),a
        inc de
        ld a,16
        ld (de),a
        inc de
        ret

put_pair
        call put_cell
        ret

render_status
        call clear_status
        ld hl,1*256+31
        ld a,32
        ld de,statusText
        call call_print
        ld a,(viewMode)
        or a
        ld de,textModeText
        jr z,.mode
        ld de,hexModeText
.mode
        ld hl,7*256+31
        ld a,32
        call call_print
        ld hl,13*256+31
        ld a,32
        ld de,helpText
        call call_print
        ret

line_input
        push bc
        push de
        call clear_status
        ld (lineLabelPtr),hl
        ld hl,1*256+31
        ld a,32
        ld de,(lineLabelPtr)
        call call_print
        pop de
        pop bc
        ld (inputPtr),de
        ld a,b
        ld (inputMax),a
        call measure_input_value
        call render_input_value
.wait
        call call_input
        or a
        jr z,.wait
        cp 1
        jr z,.done
        cp 13
        jr z,.done
        cp 12
        jr z,.back
        cp 32
        jr c,.wait
        cp 128
        jr nc,.wait
        ld (inputChar),a
        ld a,(inputLen)
        ld c,a
        ld a,(inputMax)
        cp c
        jr z,.wait
        jr c,.wait
        push bc
        ld de,(inputPtr)
        ld b,0
        ld a,(inputLen)
        ld c,a
        ld hl,0
        add hl,de
        add hl,bc
        ld a,(inputChar)
        ld (hl),a
        inc hl
        ld (hl),0
        ld hl,inputLen
        inc (hl)
        pop bc
        call render_input_value
        call wait_key_release
        jr .wait
.back
        ld a,(inputLen)
        or a
        jr z,.wait
        dec a
        ld (inputLen),a
        push bc
        ld de,(inputPtr)
        ld b,0
        ld c,a
        ld hl,0
        add hl,de
        add hl,bc
        ld (hl),0
        pop bc
        call render_input_value
        call wait_key_release
        jr .wait
.done
        push af
        call wait_key_release
        pop af
        ret

render_input_value
        ld de,SCREEN_BASE+160*31+13*2
        ld b,65
.clear
        ld a,32
        ld (de),a
        inc de
        ld (de),a
        inc de
        djnz .clear
        ld hl,13*256+31
        ld a,32
        ld de,(inputPtr)
        call call_print
        call draw_input_cursor
        ret

draw_input_cursor
        ld a,(inputLen)
        add a,a
        ld c,a
        ld b,0
        ld hl,SCREEN_BASE+160*31+13*2+1
        add hl,bc
        ld (hl),64
        ret

measure_input_value
        ld de,(inputPtr)
        ld hl,0
        add hl,de
        ld c,0
.loop
        ld a,(hl)
        or a
        jr z,.done
        ld a,c
        ld b,a
        ld a,(inputMax)
        cp b
        jr z,.truncate
        inc c
        inc hl
        jr .loop
.truncate
        xor a
        ld (hl),a
.done
        ld a,c
        ld (inputLen),a
        ret

clear_text_area
        ld de,TEXT_TOP
        ld b,TEXT_ROWS
.row
        push bc
        ld b,TEXT_COLS
.col
        ld a,32
        ld (de),a
        inc de
        ld a,16
        ld (de),a
        inc de
        djnz .col
        ld hl,160-TEXT_COLS*2
        add hl,de
        ex de,hl
        pop bc
        djnz .row
        ret

clear_status
        ld de,STATUS_POS
        ld b,78
.loop
        ld a,32
        ld (de),a
        inc de
        ld (de),a
        inc de
        djnz .loop
        ret

status_saved
        call clear_status
        ld hl,1*256+31
        ld a,32
        ld de,savedText
        jp call_print

status_saving
        call clear_status
        ld hl,1*256+31
        ld a,32
        ld de,savingText
        jp call_print

status_dirty
        call clear_status
        ld hl,1*256+31
        ld a,64
        ld de,dirtyText
        jp call_print

status_not_found
        call clear_status
        ld hl,1*256+31
        ld a,64
        ld de,notFoundText
        jp call_print

init_context
        ld ix,(ctxPtr)
        ld l,(ix+VIEWCTX_DATA_PAGES)
        ld h,(ix+VIEWCTX_DATA_PAGES+1)
        ld (dataPagesPtr),hl
        ld l,(ix+VIEWCTX_SIZE_LO)
        ld h,(ix+VIEWCTX_SIZE_LO+1)
        ld a,(ix+VIEWCTX_SIZE_HI)
        or (ix+VIEWCTX_SIZE_HI+1)
        jr z,.size_ok
        ld hl,$ffff
.size_ok
        ld (loadedSize),hl
        ld hl,0
        ld (curOffset),hl
        ld (viewOffset),hl
        xor a
        ld (dirty),a
        ld a,$ff
        ld (hexPending),a
        call detect_binary
        ret

detect_binary
        xor a
        ld (viewMode),a
        ld hl,0
        ld (scanOffset),hl
.loop
        ld hl,(scanOffset)
        call offset_at_end
        ret nc
        call read_byte_at_offset
        cp 9
        jr z,.next
        cp 10
        jr z,.next
        cp 13
        jr z,.next
        cp 32
        jr c,.binary
.next
        ld hl,(scanOffset)
        inc hl
        ld (scanOffset),hl
        jr .loop
.binary
        ld a,1
        ld (viewMode),a
        ret

inc_cursor
        ld hl,(curOffset)
        inc hl
        call clamp_to_last
        ld (curOffset),hl
        ret

clamp_to_last
        push hl
        ld de,(loadedSize)
        or a
        sbc hl,de
        pop hl
        ret c
        ld hl,(loadedSize)
        ld a,h
        or l
        ret z
        dec hl
        ret

offset_at_end
        push de
        push hl
        ld de,(loadedSize)
        or a
        sbc hl,de
        pop hl
        pop de
        ret

mark_dirty
        ld a,1
        ld (dirty),a
        ret

wait_key_release
        call call_input
        or a
        jr nz,wait_key_release
        ret

arrow_repeat_delay
        ld b,5
.wait
        halt
        djnz .wait
        ret

key_repeat_delay
        ld b,7
.wait
        halt
        djnz .wait
        ret

cursor_changed
        ld hl,(curOffset)
        ld de,(oldOffset)
        or a
        sbc hl,de
        ret

key_beep
        push af
        call call_beep
        pop af
        ret

insert_current_byte
        ld (insertChar),a
        ld hl,(loadedSize)
        ld a,h
        cp $ff
        jr nz,.space_ok
        ld a,l
        cp $ff
        jp z,write_current_from_insert
.space_ok
        ld hl,(loadedSize)
        ld a,h
        or l
        jr z,.write_insert
        ld (shiftDst),hl
        dec hl
        ld (shiftSrc),hl
.shift_loop
        ld hl,(shiftSrc)
        ld de,(curOffset)
        or a
        sbc hl,de
        jr c,.write_insert
        ld hl,(shiftSrc)
        call read_byte_at_offset
        ld (shiftChar),a
        ld hl,(shiftDst)
        ld a,(shiftChar)
        call write_byte_at_offset
        ld hl,(shiftSrc)
        ld a,h
        or l
        jr z,.write_insert
        dec hl
        ld (shiftSrc),hl
        ld hl,(shiftDst)
        dec hl
        ld (shiftDst),hl
        jr .shift_loop
.write_insert
        ld hl,(curOffset)
        ld a,(insertChar)
        call write_byte_at_offset
        ld hl,(loadedSize)
        inc hl
        ld (loadedSize),hl
        ret

delete_current_byte
        ld hl,(curOffset)
        ld (shiftDst),hl
        inc hl
        ld (shiftSrc),hl
.shift_loop
        ld hl,(shiftSrc)
        ld de,(loadedSize)
        or a
        sbc hl,de
        jr nc,.done
        ld hl,(shiftSrc)
        call read_byte_at_offset
        ld (shiftChar),a
        ld hl,(shiftDst)
        ld a,(shiftChar)
        call write_byte_at_offset
        ld hl,(shiftSrc)
        inc hl
        ld (shiftSrc),hl
        ld hl,(shiftDst)
        inc hl
        ld (shiftDst),hl
        jr .shift_loop
.done
        ld hl,(loadedSize)
        dec hl
        ld (loadedSize),hl
        ld hl,(curOffset)
        call clamp_to_last
        ld (curOffset),hl
        ret

write_current_from_insert
        ld a,(insertChar)
        jp write_current_byte

read_byte_at_offset
        call map_offset
        ld a,(hl)
        ret

write_byte_at_offset
        push af
        call map_offset
        pop af
        ld (hl),a
        ret

write_current_byte
        push af
        ld hl,(curOffset)
        call map_offset
        pop af
        ld (hl),a
        ret

map_offset
        push de
        ld a,h
        and $E0
        rlca
        rlca
        rlca
        ld e,a
        ld d,0
        push hl
        ld hl,(dataPagesPtr)
        add hl,de
        ld a,(hl)
        nextreg $57,a
        pop hl
        ld a,h
        and $1f
        or $e0
        ld h,a
        pop de
        ret

patch_services
        ld ix,(svcPtr)
        ld l,(ix+SERVICE_PRINT)
        ld h,(ix+SERVICE_PRINT+1)
        ld (call_print+1),hl
        ld l,(ix+SERVICE_INPUT_NOWAIT)
        ld h,(ix+SERVICE_INPUT_NOWAIT+1)
        ld (call_input+1),hl
        ld l,(ix+SERVICE_WINDOW)
        ld h,(ix+SERVICE_WINDOW+1)
        ld (call_window+1),hl
        ld l,(ix+SERVICE_EXTRACT)
        ld h,(ix+SERVICE_EXTRACT+1)
        ld (call_extract+1),hl
        ld l,(ix+SERVICE_BEEP)
        ld h,(ix+SERVICE_BEEP+1)
        ld (call_beep+1),hl
        ret

call_print
        call 0
        ret
call_input
        call 0
        ret
call_window
        call 0
        ret
call_extract
        call 0
        ret
call_beep
        call 0
        ret

ctxPtr       defw 0
svcPtr       defw 0
dataPagesPtr defw 0
loadedSize   defw 0
curOffset    defw 0
oldOffset    defw 0
viewOffset   defw 0
renderOffset defw 0
scanOffset   defw 0
matchOffset  defw 0
targetOffset defw 0
lineStart    defw 0
prevLineStart defw 0
lineLabelPtr defw 0
inputPtr     defw 0
inputLen     defb 0
inputMax     defb 0
inputChar    defb 0
insertChar   defb 0
shiftChar    defb 0
shiftSrc     defw 0
shiftDst     defw 0
cellAttr     defb 16
newlineChar  defb 0
desiredCol   defb 0
viewMode     defb 0
dirty        defb 0
hexPending   defb $ff
searchBuf    defs SEARCH_MAX+1
saveName     defs NAME_MAX+1

title        defb "EDIT:",0
statusText   defb "Mode:",0
textModeText defb "TEXT",0
hexModeText  defb "HEX",0
helpText     defb "EXT+S save EXT+E as EXT+F find EXT+H/T",0
findText     defb "Find:",0
saveAsText   defb "Save as:",0
savedText    defb "Saved",0
savingText   defb "Saving...",0
notFoundText defb "Not found",0
dirtyText    defb "Changed: S overwrite, E save as, I ignore",0

plugin_end
        assert plugin_end - plugin_start <= VIEW_PLUGIN_SIZE
        SAVEBIN "plugin/edit.ccp", VIEW_PLUGIN_ADDRESS, VIEW_PLUGIN_SIZE
