        DEVICE ZXSPECTRUMNEXT
        org VIEW_PLUGIN_ADDRESS

        include "plugin_api.i.asm"

SCREEN_BASE     equ $4000
TEXT_TOP        equ SCREEN_BASE+160*3+2
TEXT_ROWS       equ 27
TEXT_COLS       equ 78
TEXT_LINE_BYTES equ TEXT_COLS*2
TEXT_ROW_SKIP   equ 160-TEXT_LINE_BYTES
HEX_BYTES_PER_ROW equ 16
DEC_BYTES_PER_ROW equ 8
STATUS_LINE     equ SCREEN_BASE+160*31
STATUS_INNER    equ STATUS_LINE+2
FIND_INPUT_POS  equ STATUS_LINE+7*2
SEARCH_MAX      equ 24
FIND_INPUT_ATTR equ 32
FIND_CURSOR_ATTR equ 64

plugin_start
        ld (ctxPtr),hl
        ld (svcPtr),de
        call patch_services
        call init_context

        ld hl,0 * 256 + 0
        ld bc,78 * 256 + 30
        ld a,16
        call call_window

        ld hl,title
        call print_de_at_2_1

        ld ix,(ctxPtr)
        ld e,(ix+VIEWCTX_FILENAME)
        ld d,(ix+VIEWCTX_FILENAME+1)
        ld hl,15*256+1
        ld a,16
        call call_print

        call render_page

.input
        call call_input
        or a
        jr z,.input
        cp 1
        ret z
        cp 10
        jp z,.line_down
        cp 11
        jp z,.line_up
        cp 9
        jp z,.page_down
        cp 8
        jp z,.page_up
        cp "h"
        jp z,.hex_mode
        cp "H"
        jp z,.hex_mode
        cp "d"
        jp z,.dec_mode
        cp "D"
        jp z,.dec_mode
        cp "t"
        jp z,.text_mode
        cp "T"
        jp z,.text_mode
        cp "/"
        jp z,.find
        jp .input

.line_down
        ld hl,(curOffset)
        ld (oldOffset),hl
        call move_line_down
        call offset_changed
        jr z,.input
        ld a,(viewMode)
        or a
        jr z,.line_down_text
        call scroll_text_up
        call render_bottom_line
        call render_status
        jp .input
.line_down_text
        call scroll_text_up
        call render_bottom_line
        call render_status
        jp .input
.line_up
        ld hl,(curOffset)
        ld (oldOffset),hl
        call move_line_up
        call offset_changed
        jr z,.input
        ld a,(viewMode)
        or a
        jr z,.line_up_text
        call scroll_text_down
        call render_top_line
        call render_status
        jp .input
.line_up_text
        call scroll_text_down
        call render_top_line
        call render_status
        jp .input
.page_down
        ld a,(viewMode)
        or a
        jr z,.page_down_text
        ld b,TEXT_ROWS
.pd
        push bc
        call move_line_down
        pop bc
        djnz .pd
        call render_page
        jp .input
.page_down_text
        ld hl,(curLine)
        ld de,TEXT_ROWS
        add hl,de
        ld (targetOffset),hl
        ld hl,(totalLines)
        dec hl
        ld de,(targetOffset)
        or a
        sbc hl,de
        jr c,.page_down_use_max
        ld hl,(targetOffset)
.page_down_use_max
        ld (curLine),hl
        call seek_text_line_by_number
        call render_page
        jp .input
.page_up
        ld a,(viewMode)
        or a
        jr z,.page_up_text
        ld b,TEXT_ROWS
.pu
        push bc
        call move_line_up
        pop bc
        djnz .pu
        call render_page
        jp .input
.page_up_text
        ld hl,(curLine)
        ld de,TEXT_ROWS
        or a
        sbc hl,de
        jr nc,.page_up_store
        ld hl,0
.page_up_store
        ld (curLine),hl
        call seek_text_line_by_number
        call render_page
        jp .input
.hex_mode
        ld a,1
        ld (viewMode),a
        call align_cur_binary
        call render_page
        jp .input
.dec_mode
        ld a,2
        ld (viewMode),a
        call align_cur_binary
        call render_page
        jp .input
.text_mode
        xor a
        ld (viewMode),a
        call seek_text_line_start
        call render_page
        jp .input
.find
        call find_prompt
        call render_page
        jp .input


init_context
        ld ix,(ctxPtr)
        ld l,(ix+VIEWCTX_DATA_PAGES)
        ld h,(ix+VIEWCTX_DATA_PAGES+1)
        ld (dataPagesPtr),hl
        ld a,(ix+VIEWCTX_PAGE_COUNT)
        ld (pageCount),a

        ld l,(ix+VIEWCTX_SIZE_LO)
        ld h,(ix+VIEWCTX_SIZE_LO+1)
        ld a,(ix+VIEWCTX_SIZE_HI)
        or (ix+VIEWCTX_SIZE_HI+1)
        jr nz,.cap
        ld a,(pageCount)
        cp 8
        jr nz,.store_size
.cap
        ld hl,$ffff
.store_size
        ld (loadedSize),hl
        ld hl,0
        ld (curOffset),hl
        ld (curLine),hl
        call count_lines
        ret


render_page
        ld a,(viewMode)
        or a
        jp z,render_text_page
        cp 1
        jp z,render_hex_page
        jp render_dec_page


render_text_page
        call clear_text_area
        ld hl,(curOffset)
        ld (renderOffset),hl
        ld de,TEXT_TOP
        ld (lineStart),de
        xor a
        ld (textRow),a
        ld (textCol),a
        call render_status
        ld de,(lineStart)

.loop
        ld a,(textRow)
        cp TEXT_ROWS
        ret nc
        ld hl,(renderOffset)
        push de
        call offset_at_end
        pop de
        ret nc
        push de
        call read_byte_at_render
        pop de
        cp 13
        jr z,.loop
        cp 10
        jr z,.newline
        cp 9
        jr nz,.not_tab
        ld a,32
.not_tab
        cp 32
        jr nc,.char_ok
        ld a,"."
.char_ok
        ld (tmpChar),a
        ld a,(textCol)
        cp TEXT_COLS
        call z,newline
        ld a,(textRow)
        cp TEXT_ROWS
        ret nc
        ld a,(tmpChar)
        ld (de),a
        inc de
        ld a,16
        ld (de),a
        inc de
        ld a,(textCol)
        inc a
        ld (textCol),a
        jr .loop
.newline
        call newline
        jr .loop


clear_text_area
        ld de,TEXT_TOP
        ld b,TEXT_ROWS
.row
        push bc
        push de
        ld b,TEXT_COLS
.col
        ld a,32
        ld (de),a
        inc de
        ld a,16
        ld (de),a
        inc de
        djnz .col
        pop de
        ld hl,160
        add hl,de
        ex de,hl
        pop bc
        djnz .row
        ret


scroll_text_up
        ld hl,TEXT_TOP+160
        ld de,TEXT_TOP
        ld a,TEXT_ROWS-1
.row
        push af
        ld bc,TEXT_LINE_BYTES
        ldir
        ld bc,TEXT_ROW_SKIP
        add hl,bc
        ex de,hl
        add hl,bc
        ex de,hl
        pop af
        dec a
        jr nz,.row
        ret


scroll_text_down
        ld hl,TEXT_TOP+160*(TEXT_ROWS-2)+TEXT_LINE_BYTES-1
        ld de,TEXT_TOP+160*(TEXT_ROWS-1)+TEXT_LINE_BYTES-1
        ld a,TEXT_ROWS-1
.row
        push af
        ld bc,TEXT_LINE_BYTES
        lddr
        ld bc,TEXT_ROW_SKIP
        or a
        sbc hl,bc
        ex de,hl
        or a
        sbc hl,bc
        ex de,hl
        pop af
        dec a
        jr nz,.row
        ret


render_top_line
        ld hl,(curOffset)
        ld de,TEXT_TOP
        jp render_single_line


render_bottom_line
        ld a,(viewMode)
        or a
        jp nz,render_binary_bottom_line
        ld hl,(curOffset)
        ld (scanOffset),hl
        ld b,TEXT_ROWS-1
.scan
        push bc
        call move_scan_line_down
        pop bc
        djnz .scan
        ld hl,(scanOffset)
        ld de,TEXT_TOP+160*(TEXT_ROWS-1)
        jp render_single_line


render_binary_bottom_line
        ld hl,(curOffset)
        ld (scanOffset),hl
        ld b,TEXT_ROWS-1
.scan
        push bc
        ld hl,(scanOffset)
        call get_binary_row_bytes
        add hl,de
        ld (scanOffset),hl
        pop bc
        djnz .scan
        ld de,TEXT_TOP+160*(TEXT_ROWS-1)
        ld hl,(scanOffset)
        push de
        call offset_at_end
        pop de
        jp c,render_single_line
        ld (lineStart),de
        jp clear_screen_line


move_scan_line_down
        ld hl,(scanOffset)
        call offset_at_end
        ret nc
        xor a
        ld (textCol),a
.scan
        call read_byte_at_scan
        cp 13
        jr z,.next
        cp 10
        ret z
        ld hl,textCol
        inc (hl)
        ld a,(hl)
        cp TEXT_COLS
        ret nc
.next
        ld hl,(scanOffset)
        call offset_at_end
        jr c,.scan
        ret


render_single_line
        ld a,(viewMode)
        or a
        jr z,render_text_single_line
        cp 1
        jp z,render_hex_single_line
        jp render_dec_single_line


render_text_single_line
        ld (renderOffset),hl
        ld (lineStart),de
        call clear_screen_line
        ld de,(lineStart)
        xor a
        ld (textCol),a
.loop
        ld a,(textCol)
        cp TEXT_COLS
        ret nc
        ld hl,(renderOffset)
        push de
        call offset_at_end
        pop de
        ret nc
        push de
        call read_byte_at_render
        pop de
        cp 13
        jr z,.loop
        cp 10
        ret z
        cp 9
        jr nz,.not_tab
        ld a,32
.not_tab
        cp 32
        jr nc,.char_ok
        ld a,"."
.char_ok
        ld (de),a
        inc de
        ld a,16
        ld (de),a
        inc de
        ld a,(textCol)
        inc a
        ld (textCol),a
        jr .loop


clear_screen_line
        ld de,(lineStart)
        ld b,TEXT_COLS
.col
        ld a,32
        ld (de),a
        inc de
        ld a,16
        ld (de),a
        inc de
        djnz .col
        ret


newline
        push hl
        ld hl,(lineStart)
        ld de,160
        add hl,de
        ld (lineStart),hl
        ex de,hl
        xor a
        ld (textCol),a
        ld hl,textRow
        inc (hl)
        pop hl
        ret


move_line_down
        ld a,(viewMode)
        or a
        jr z,move_text_line_down
        ld hl,(curOffset)
        call offset_at_end
        ret nc
        ld hl,(curOffset)
        call get_binary_row_bytes
        add hl,de
        call offset_at_end
        ret nc
        ld (curOffset),hl
        ld hl,(curLine)
        inc hl
        ld (curLine),hl
        ret


move_text_line_down
        ld hl,(curOffset)
        call offset_at_end
        ret nc
        ld (scanOffset),hl
        call move_scan_line_down
        ld hl,(scanOffset)
        call offset_at_end
        ret nc
        ld hl,(scanOffset)
        ld (curOffset),hl
        ld hl,(curLine)
        inc hl
        ld (curLine),hl
        ret


move_line_up
        ld a,(viewMode)
        or a
        jr z,move_text_line_up
        ld hl,(curLine)
        ld a,h
        or l
        ret z
        ld hl,(curOffset)
        ld a,h
        or l
        ret z
        call get_binary_row_bytes
        ld d,0
        or a
        sbc hl,de
        jr nc,.store
        ld hl,0
.store
        ld (curOffset),hl
        ld hl,(curLine)
        ld a,h
        or l
        ret z
        dec hl
        ld (curLine),hl
        ret


move_text_line_up
        ld hl,(curOffset)
        ld a,h
        or l
        ret z
        ld (targetOffset),hl
        ld hl,0
        ld (prevLine),hl
        ld (scanOffset),hl
.forward
        ld hl,(scanOffset)
        ld (matchOffset),hl
        call move_scan_line_down
        ld hl,(scanOffset)
        ld de,(matchOffset)
        or a
        sbc hl,de
        jr z,.use_prev
        ld hl,(scanOffset)
        ld de,(targetOffset)
        or a
        sbc hl,de
        jr nc,.use_prev
        ld hl,(scanOffset)
        ld (prevLine),hl
        jr .forward
.use_prev
        ld hl,(prevLine)
        ld (curOffset),hl
        jr .line_done
.line_done
        ld hl,(curLine)
        ld a,h
        or l
        ret z
        dec hl
        ld (curLine),hl
        ret


read_byte_at_cur
        ld hl,(curOffset)
        call read_byte_at_offset
        ld hl,(curOffset)
        inc hl
        ld (curOffset),hl
        ret

read_byte_at_render
        ld hl,(renderOffset)
        call read_byte_at_offset
        ld hl,(renderOffset)
        inc hl
        ld (renderOffset),hl
        ret

read_byte_at_scan
        ld hl,(scanOffset)
        call read_byte_at_offset
        ld hl,(scanOffset)
        inc hl
        ld (scanOffset),hl
        ret


render_hex_page
        call clear_text_area
        ld hl,(curOffset)
        ld (renderOffset),hl
        ld de,TEXT_TOP
        xor a
        ld (textRow),a
        call render_status
        ld de,TEXT_TOP
.loop
        ld a,(textRow)
        cp TEXT_ROWS
        ret nc
        push de
        ld hl,(renderOffset)
        call offset_at_end
        pop de
        ret nc
        push de
        ld hl,(renderOffset)
        call render_hex_single_line
        pop de
        ld hl,160
        add hl,de
        ex de,hl
        ld hl,textRow
        inc (hl)
        jr .loop


render_dec_page
        call clear_text_area
        ld hl,(curOffset)
        ld (renderOffset),hl
        ld de,TEXT_TOP
        xor a
        ld (textRow),a
        call render_status
        ld de,TEXT_TOP
.loop
        ld a,(textRow)
        cp TEXT_ROWS
        ret nc
        push de
        ld hl,(renderOffset)
        call offset_at_end
        pop de
        ret nc
        push de
        ld hl,(renderOffset)
        call render_dec_single_line
        pop de
        ld hl,160
        add hl,de
        ex de,hl
        ld hl,textRow
        inc (hl)
        jr .loop


render_hex_single_line
        ld (renderOffset),hl
        ld (lineStart),de
        call clear_screen_line
        ld de,(lineStart)
        ld hl,(renderOffset)
        call print_word_hex
        ld a,":"
        call put_char
        ld a,32
        call put_char
        ld b,HEX_BYTES_PER_ROW
.bytes
        push bc
        ld hl,(renderOffset)
        call offset_at_end
        jr nc,.blank_byte
        call read_byte_at_offset
        call print_byte_hex
        jr .byte_done
.blank_byte
        ld a,32
        call put_char
        call put_char
.byte_done
        ld a,32
        call put_char
        ld hl,(renderOffset)
        inc hl
        ld (renderOffset),hl
        pop bc
        djnz .bytes
        ld a,32
        call put_char
        ld hl,(lineStart)
        ld de,(6+HEX_BYTES_PER_ROW*3+2)*2
        add hl,de
        ex de,hl
        ld hl,(renderOffset)
        ld bc,-HEX_BYTES_PER_ROW
        add hl,bc
        ld (renderOffset),hl
        ld b,HEX_BYTES_PER_ROW
        jr render_ascii_tail


render_dec_single_line
        ld (renderOffset),hl
        ld (lineStart),de
        call clear_screen_line
        ld de,(lineStart)
        ld hl,(renderOffset)
        call print_word_hex
        ld a,":"
        call put_char
        ld a,32
        call put_char
        ld b,DEC_BYTES_PER_ROW
.bytes
        push bc
        ld hl,(renderOffset)
        call offset_at_end
        jr nc,.blank_byte
        call read_byte_at_offset
        call print_byte_dec
        jr .byte_done
.blank_byte
        ld a,32
        call put_char
        call put_char
        call put_char
.byte_done
        ld a,32
        call put_char
        ld hl,(renderOffset)
        inc hl
        ld (renderOffset),hl
        pop bc
        djnz .bytes
        ld hl,(lineStart)
        ld de,(6+DEC_BYTES_PER_ROW*4+2)*2
        add hl,de
        ex de,hl
        ld hl,(renderOffset)
        ld bc,-DEC_BYTES_PER_ROW
        add hl,bc
        ld (renderOffset),hl
        ld b,DEC_BYTES_PER_ROW

render_ascii_tail
.ascii
        push bc
        ld hl,(renderOffset)
        call offset_at_end
        jr nc,.blank
        call read_byte_at_offset
        cp 32
        jr c,.dot
        cp 127
        jr c,.ok
.dot
        ld a,"."
.ok
        call put_char
        jr .next
.blank
        ld a,32
        call put_char
.next
        ld hl,(renderOffset)
        inc hl
        ld (renderOffset),hl
        pop bc
        djnz .ascii
        ret


find_prompt
        call clear_status_line
        ld hl,1*256+31
        ld a,FIND_INPUT_ATTR
        ld de,findLabel
        call call_print
        xor a
        ld (searchLen),a
        ld (searchCursor),a
        ld (searchBuf),a
        ld hl,FIND_INPUT_POS
        ld (searchScreenPtr),hl
        call find_render_input
        call find_wait_input_release
.input
        call call_input
        or a
        jr z,.input
        call find_beep
        cp 1
        ret z
        cp 13
        jp z,find_execute
        cp 8
        jr z,.left
        cp 9
        jr z,.right
        cp 12
        jr z,.backspace
        cp 199
        jr z,.delete
        cp 32
        jr c,.ignored
        cp 128
        jr nc,.ignored
        call find_insert_char
        call find_render_input
        call find_wait_input_release
        jr .input
.left
        ld a,(searchCursor)
        or a
        jr z,.ignored
        dec a
        ld (searchCursor),a
        call find_render_input
        call find_wait_input_release
        jr .input
.right
        ld a,(searchCursor)
        ld b,a
        ld a,(searchLen)
        cp b
        jr z,.ignored
        ld a,b
        inc a
        ld (searchCursor),a
        call find_render_input
        call find_wait_input_release
        jr .input
.backspace
        ld a,(searchCursor)
        or a
        jr z,.ignored
        dec a
        ld (searchCursor),a
        call find_delete_at_cursor
        call find_render_input
        call find_wait_input_release
        jr .input
.delete
        call find_delete_at_cursor
        call find_render_input
        call find_wait_input_release
        jr .input
.ignored
        call find_wait_input_release
        jp .input


find_render_input
        ld de,FIND_INPUT_POS
        ld hl,searchBuf
        ld b,SEARCH_MAX
        xor a
        ld (findRenderPos),a
.loop
        ld a,(findRenderPos)
        ld c,a
        ld a,(searchLen)
        cp c
        jr z,.blank
        jr c,.blank
        ld a,(hl)
        inc hl
        jr .char_ready
.blank
        ld a,32
.char_ready
        ld (de),a
        inc de
        push hl
        ld a,(findRenderPos)
        ld c,a
        ld a,(searchCursor)
        cp SEARCH_MAX
        jr nz,.cursor_ready
        ld a,SEARCH_MAX-1
.cursor_ready
        cp c
        ld a,FIND_INPUT_ATTR
        jr nz,.attr_ready
        ld a,FIND_CURSOR_ATTR
.attr_ready
        ld (de),a
        inc de
        ld hl,findRenderPos
        inc (hl)
        pop hl
        djnz .loop
        ret


find_insert_char
        ld c,a
        ld a,(searchLen)
        cp SEARCH_MAX
        ret nc
        ld b,a
        ld a,(searchCursor)
        ld e,b
        sub e
        neg
        ld b,a
        ld a,(searchLen)
        ld e,a
        ld d,0
        ld hl,searchBuf
        add hl,de
        ld a,b
        or a
        jr z,.store
.shift
        dec hl
        ld a,(hl)
        inc hl
        ld (hl),a
        dec hl
        djnz .shift
.store
        ld a,(searchCursor)
        ld e,a
        ld d,0
        ld hl,searchBuf
        add hl,de
        ld (hl),c
        ld hl,searchLen
        inc (hl)
        ld a,(hl)
        ld e,a
        ld d,0
        ld hl,searchBuf
        add hl,de
        ld (hl),0
        ld hl,searchCursor
        inc (hl)
        ret


find_delete_at_cursor
        ld a,(searchCursor)
        ld b,a
        ld a,(searchLen)
        cp b
        ret z
        ret c
        ld e,b
        ld d,0
        ld hl,searchBuf
        add hl,de
        ex de,hl
        inc hl
.shift
        ld a,(hl)
        ld (de),a
        inc hl
        inc de
        or a
        jr nz,.shift
        ld hl,searchLen
        dec (hl)
        ret


find_wait_input_release
        call call_input
        or a
        jr nz,find_wait_input_release
        ret


find_beep
        push af
        ld a,$11
        out ($fe),a
        ld b,$18
.wait
        djnz .wait
        ld a,$09
        out ($fe),a
        pop af
        ret


find_execute
        ld a,(searchLen)
        or a
        ret z
        ld hl,(curOffset)
        inc hl
        call clamp_hl_to_size
        ld (scanOffset),hl
        call find_from_scan
        jr z,.found
        ld hl,0
        ld (scanOffset),hl
        call find_from_scan
        jr z,.found
        call clear_status_line
        ld hl,1*256+31
        ld a,32
        ld de,notFoundText
        call call_print
        call find_wait_input_release
.wait
        call call_input
        or a
        jr z,.wait
        ret
.found
        ld hl,(scanOffset)
        ld (curOffset),hl
        ld a,(viewMode)
        or a
        jp nz,align_cur_binary
        jp seek_text_line_start


find_from_scan
.loop
        ld hl,(scanOffset)
        call offset_at_end
        jr nc,.fail
        call compare_at_scan
        ret z
        ld hl,(scanOffset)
        inc hl
        ld (scanOffset),hl
        jr .loop
.fail
        or 1
        ret


compare_at_scan
        ld hl,(scanOffset)
        ld (matchOffset),hl
        ld de,searchBuf
        ld a,(searchLen)
        ld b,a
.loop
        ld hl,(matchOffset)
        call offset_at_end
        jr nc,.fail
        call read_byte_at_offset
        call to_upper
        ld c,a
        ld a,(de)
        call to_upper
        cp c
        jr nz,.fail
        inc de
        ld hl,(matchOffset)
        inc hl
        ld (matchOffset),hl
        djnz .loop
        xor a
        ret
.fail
        or 1
        ret


seek_text_line_start
        ld hl,(curOffset)
        ld (targetOffset),hl
        ld hl,0
        ld (curLine),hl
        ld (prevLine),hl
        ld (scanOffset),hl
        ld hl,(targetOffset)
        ld a,h
        or l
        ret z
.loop
        ld hl,(scanOffset)
        ld (prevLine),hl
        call move_scan_line_down
        ld hl,(scanOffset)
        ld de,(targetOffset)
        or a
        sbc hl,de
        jr z,.exact
        jr nc,.done
        ld hl,(scanOffset)
        ld hl,(curLine)
        inc hl
        ld (curLine),hl
        jr .loop
.exact
        ld hl,(curLine)
        inc hl
        ld (curLine),hl
        ld hl,(scanOffset)
        ld (curOffset),hl
        ret
.done
        ld hl,(prevLine)
        ld (curOffset),hl
        ret


seek_text_line_by_number
        ld hl,(curLine)
        ld (targetOffset),hl
        ld hl,0
        ld (scanOffset),hl
.loop
        ld hl,(targetOffset)
        ld a,h
        or l
        jr z,.done
        call move_scan_line_down
        ld hl,(scanOffset)
        call offset_at_end
        jr nc,.done
        ld hl,(targetOffset)
        dec hl
        ld (targetOffset),hl
        jr .loop
.done
        ld hl,(scanOffset)
        ld (curOffset),hl
        ret


to_upper
        cp "a"
        ret c
        cp "z"+1
        ret nc
        sub 32
        ret


clear_status_line
        ld de,STATUS_INNER
        ld b,TEXT_COLS
.loop
        ld a,32
        ld (de),a
        inc de
        ld a,32
        ld (de),a
        inc de
        djnz .loop
        ret


put_char
        push af
        ld (de),a
        inc de
        ld a,16
        ld (de),a
        inc de
        pop af
        ret


print_word_hex
        ld a,h
        call print_byte_hex
        ld a,l
        jp print_byte_hex


print_byte_hex
        push af
        rrca
        rrca
        rrca
        rrca
        call print_nibble
        pop af
print_nibble
        and $0f
        add a,"0"
        cp "9"+1
        jr c,.ok
        add a,7
.ok
        jp put_char


print_byte_dec
        ld c,a
        ld b,0
.hund
        ld a,c
        cp 100
        jr c,.tens
        sub 100
        ld c,a
        inc b
        jr .hund
.tens
        ld a,b
        add a,"0"
        call put_char
        ld b,0
.ten_loop
        ld a,c
        cp 10
        jr c,.ones
        sub 10
        ld c,a
        inc b
        jr .ten_loop
.ones
        ld a,b
        add a,"0"
        call put_char
        ld a,c
        add a,"0"
        jp put_char


align_cur_binary
        ld hl,(curOffset)
        ld a,(viewMode)
        cp 1
        jr nz,.dec
        ld a,l
        and $f0
        ld l,a
        ld (curOffset),hl
        srl h
        rr l
        srl h
        rr l
        srl h
        rr l
        srl h
        rr l
        ld (curLine),hl
        ret
.dec
        ld a,l
        and $f8
        ld l,a
        ld (curOffset),hl
        srl h
        rr l
        srl h
        rr l
        srl h
        rr l
        ld (curLine),hl
        ret


get_binary_row_bytes
        ld d,0
        ld a,(viewMode)
        cp 1
        ld e,HEX_BYTES_PER_ROW
        ret z
        ld e,DEC_BYTES_PER_ROW
        ret


get_binary_total_rows
        ld hl,(loadedSize)
        ld a,h
        or l
        ret z
        dec hl
        ld a,(viewMode)
        cp 1
        ld b,4
        jr z,.shift
        ld b,3
.shift
        srl h
        rr l
        djnz .shift
        inc hl
        ret


clamp_hl_to_size
        push hl
        ld de,(loadedSize)
        or a
        sbc hl,de
        pop hl
        ret c
        ld hl,(loadedSize)
        ret


read_byte_at_offset
        push de
        push hl
        ld a,h
        and $E0
        ld e,0
        cp $20
        jr c,.got_page
        inc e
        cp $40
        jr c,.sub20
        inc e
        cp $60
        jr c,.sub40
        inc e
        cp $80
        jr c,.sub60
        inc e
        cp $A0
        jr c,.sub80
        inc e
        cp $C0
        jr c,.suba0
        inc e
        cp $E0
        jr c,.subc0
        inc e
        jr .sube0
.sub20
        ld a,h
        sub $20
        jr .set_h
.sub40
        ld a,h
        sub $40
        jr .set_h
.sub60
        ld a,h
        sub $60
        jr .set_h
.sub80
        ld a,h
        sub $80
        jr .set_h
.suba0
        ld a,h
        sub $A0
        jr .set_h
.subc0
        ld a,h
        sub $C0
        jr .set_h
.sube0
        ld a,h
        sub $E0
        jr .set_h
.got_page
        ld a,h
.set_h
        ld (tmpMappedH),a
        ld a,e
        ld e,a
        ld d,0
        ld hl,(dataPagesPtr)
        add hl,de
        ld a,(hl)
        nextreg $57,a
        pop hl
        ld a,(tmpMappedH)
        or $E0
        ld h,a
        ld a,(hl)
        pop de
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


offset_changed
        ld hl,(curOffset)
        ld de,(oldOffset)
        or a
        sbc hl,de
        ret


count_lines
        ld hl,0
        ld (totalLines),hl
        ld (scanOffset),hl
        ld hl,(loadedSize)
        ld a,h
        or l
        jr z,.finish_empty
.loop
        ld hl,(scanOffset)
        call offset_at_end
        jr nc,.finish
        ld hl,(totalLines)
        inc hl
        ld (totalLines),hl
        call move_scan_line_down
        jr .loop
.finish
        ld hl,(totalLines)
        ld a,h
        or l
        ret nz
.finish_empty
        ld hl,(totalLines)
        inc hl
        ld (totalLines),hl
        ret


render_status
        call clear_status_line
        ld hl,1*256+31
        ld a,32
        ld de,lineLabel
        ld a,(viewMode)
        or a
        jr z,.label_ready
        ld de,rowLabel
.label_ready
        ld a,32
        call call_print
        ld hl,(curLine)
        inc hl
        call make_decimal
        ld hl,7*256+31
        ld a,32
        ld de,numBuf
        call call_print
        ld hl,13*256+31
        ld a,32
        ld de,slashText
        call call_print
        ld a,(viewMode)
        or a
        jr z,.text_total
        call get_binary_total_rows
        jr .total_ready
.text_total
        ld hl,(totalLines)
.total_ready
        call make_decimal
        ld hl,15*256+31
        ld a,32
        ld de,numBuf
        call call_print
        ld hl,23*256+31
        ld a,32
        ld de,modeLabel
        call call_print
        call get_mode_text
        ld hl,29*256+31
        ld a,32
        call call_print
        ld hl,36*256+31
        ld a,32
        ld de,helpText
        call call_print
        ret


get_mode_text
        ld a,(viewMode)
        or a
        jr z,.text
        cp 1
        jr z,.hex
        ld de,modeDecText
        ret
.hex
        ld de,modeHexText
        ret
.text
        ld de,modeTextText
        ret


make_decimal
        ld de,numBuf
        ld b,5
        ld a,32
.clear
        ld (de),a
        inc de
        djnz .clear
        xor a
        ld (de),a
        ld de,numBuf
        ld bc,10000
        call .digit
        ld bc,1000
        call .digit
        ld bc,100
        call .digit
        ld bc,10
        call .digit
        ld a,l
        add a,"0"
        ld (de),a
        call trim_num
        ret
.digit
        xor a
.dig_loop
        or a
        sbc hl,bc
        jr c,.dig_done
        inc a
        jr .dig_loop
.dig_done
        add hl,bc
        add a,"0"
        ld (de),a
        inc de
        ret


trim_num
        ld hl,numBuf
        ld b,4
.loop
        ld a,(hl)
        cp "0"
        ret nz
        ld (hl),32
        inc hl
        djnz .loop
        ret


print_de_at_2_1
        ex de,hl
        ld hl,2*256+1
        ld a,16
call_print
        call 0
        ret

call_input
        call 0
        ret

call_window
        call 0
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
        ret

ctxPtr       defw 0
svcPtr       defw 0
dataPagesPtr defw 0
loadedSize   defw 0
curOffset    defw 0
renderOffset defw 0
scanOffset   defw 0
prevLine     defw 0
oldOffset    defw 0
curLine      defw 0
totalLines   defw 0
lineStart    defw 0
searchScreenPtr defw 0
matchOffset  defw 0
targetOffset defw 0
textCol      defb 0
textRow      defb 0
pageCount    defb 0
viewMode     defb 0
searchLen    defb 0
searchCursor defb 0
findRenderPos defb 0
tmpChar      defb 0
tmpMappedH   defb 0
numBuf       defs 6
searchBuf    defs SEARCH_MAX+1
title        defb "VIEW:",0
lineLabel    defb "Line:",0
rowLabel     defb "Row:",0
slashText    defb "/",0
modeLabel    defb "Mode:",0
modeTextText defb "TEXT",0
modeHexText  defb "HEX",0
modeDecText  defb "DEC",0
findLabel    defb "Find:",0
notFoundText defb "Not found - press any key",0
helpText     defb "BREAK=exit T=text H=hex D=dec /=find",0

plugin_end
        assert plugin_end - plugin_start <= VIEW_PLUGIN_SIZE
        SAVEBIN "plugin/TXT,ASM,BAS,CFG,INI_Text-Viewer.CCP", VIEW_PLUGIN_ADDRESS, VIEW_PLUGIN_SIZE
