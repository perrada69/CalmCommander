        DEVICE ZXSPECTRUMNEXT
        org VIEW_PLUGIN_ADDRESS

        include "plugin_api.i.asm"

SCREEN_BASE     equ $4000
TEXT_TOP        equ SCREEN_BASE+160*3+2
TEXT_ROWS       equ 27
TEXT_COLS       equ 78
TEXT_LINE_BYTES equ TEXT_COLS*2
TEXT_ROW_SKIP   equ 160-TEXT_LINE_BYTES

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

        ld de,helpText
        ld hl,34*256+31
        ld a,32
        call call_print

        call render_page

.input
        call call_input
        or a
        jr z,.input
        cp 1
        ret z
        cp 10
        jr z,.line_down
        cp 11
        jr z,.line_up
        cp 9
        jr z,.page_down
        cp 8
        jr z,.page_up
        jr .input

.line_down
        ld hl,(curOffset)
        ld (oldOffset),hl
        call move_line_down
        call offset_changed
        jr z,.input
        call scroll_text_up
        call render_bottom_line
        call render_status
        jr .input
.line_up
        ld hl,(curOffset)
        ld (oldOffset),hl
        call move_line_up
        call offset_changed
        jr z,.input
        call scroll_text_down
        call render_top_line
        call render_status
        jr .input
.page_down
        ld b,TEXT_ROWS
.pd
        push bc
        call move_line_down
        pop bc
        djnz .pd
        call render_page
        jr .input
.page_up
        ld b,TEXT_ROWS
.pu
        push bc
        call move_line_up
        pop bc
        djnz .pu
        call render_page
        jr .input


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


move_scan_line_down
        ld hl,(scanOffset)
        call offset_at_end
        ret nc
.scan
        call read_byte_at_scan
        cp 10
        ret z
        ld hl,(scanOffset)
        call offset_at_end
        jr c,.scan
        ret


render_single_line
        ld (renderOffset),hl
        ld (lineStart),de
        call clear_screen_line
        ld de,(lineStart)
        xor a
        ld (textCol),a
.loop
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
        ld (tmpChar),a
        ld a,(textCol)
        cp TEXT_COLS
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
        ld hl,(curOffset)
        call offset_at_end
        ret nc
.scan
        call read_byte_at_cur
        cp 10
        jr z,.done
        ld hl,(curOffset)
        call offset_at_end
        jr c,.scan
.done
        ld hl,(curLine)
        inc hl
        ld (curLine),hl
        ret


move_line_up
        ld hl,(curOffset)
        ld a,h
        or l
        ret z
        ld de,0
        ld (prevLine),de
        ld (scanOffset),de
.scan
        ld hl,(scanOffset)
        ld de,(curOffset)
        or a
        sbc hl,de
        jr nc,.done
        call read_byte_at_scan
        cp 10
        jr nz,.scan
        ld hl,(scanOffset)
        ld de,(curOffset)
        or a
        sbc hl,de
        jr nc,.done
        ld hl,(scanOffset)
        ld (prevLine),hl
        jr .scan
.done
        ld hl,(prevLine)
        ld (curOffset),hl
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
        ld de,(loadedSize)
        or a
        sbc hl,de
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
.loop
        ld hl,(scanOffset)
        call offset_at_end
        jr nc,.finish
        call read_byte_at_scan
        cp 10
        jr nz,.loop
        ld hl,(totalLines)
        inc hl
        ld (totalLines),hl
        jr .loop
.finish
        ld hl,(totalLines)
        inc hl
        ld (totalLines),hl
        ret


render_status
        ld hl,1*256+31
        ld a,32
        ld de,lineLabel
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
        ld hl,(totalLines)
        call make_decimal
        ld hl,15*256+31
        ld a,32
        ld de,numBuf
        call call_print
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
textCol      defb 0
textRow      defb 0
pageCount    defb 0
tmpChar      defb 0
tmpMappedH   defb 0
numBuf       defs 6
title        defb "VIEW:",0
lineLabel    defb "Line:",0
slashText    defb "/",0
helpText     defb "BREAK close  UP/DOWN line  LEFT/RIGHT page",0

plugin_end
        assert plugin_end - plugin_start <= VIEW_PLUGIN_SIZE
        SAVEBIN "plugin/text.ccp", VIEW_PLUGIN_ADDRESS, VIEW_PLUGIN_SIZE
