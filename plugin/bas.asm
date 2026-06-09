        DEVICE ZXSPECTRUMNEXT
        org VIEW_PLUGIN_ADDRESS

        include "plugin_api.i.asm"

SCREEN_BASE     equ $4000
TEXT_TOP        equ SCREEN_BASE+160*3+2
TEXT_ROWS       equ 27
TEXT_COLS       equ 78
TEXT_LINE_BYTES equ TEXT_COLS*2
TEXT_ROW_SKIP   equ 160-TEXT_LINE_BYTES
STATUS_LINE     equ SCREEN_BASE+160*31
STATUS_INNER    equ STATUS_LINE+2

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
        ld hl,9*256+1
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
        jp .input

.line_down
        call move_line_down
        jr z,.input
        call scroll_up
        call render_bottom_line
        call render_status
        jp .input

.line_up
        call move_line_up
        jr z,.input
        call scroll_down
        call render_top_line
        call render_status
        jp .input

.page_down
        ld b,TEXT_ROWS
.pd
        push bc
        call move_line_down
        pop bc
        jr z,.pd_done
        djnz .pd
.pd_done
        call render_page
        jp .input

.page_up
        ld b,TEXT_ROWS
.pu
        push bc
        call move_line_up
        pop bc
        jr z,.pu_done
        djnz .pu
.pu_done
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
        call detect_plus3dos            ; sets basStart (0 or 128)
        ld hl,(basStart)
        ld (curOffset),hl
        ld hl,0
        ld (curLine),hl
        call count_lines
        ret


; detect +3DOS header: if file starts with "PLUS3DOS" set basStart=128, else 0
detect_plus3dos
        ld hl,0    : call read_byte_at_offset : cp 'P' : jr nz,.raw
        ld hl,1    : call read_byte_at_offset : cp 'L' : jr nz,.raw
        ld hl,2    : call read_byte_at_offset : cp 'U' : jr nz,.raw
        ld hl,3    : call read_byte_at_offset : cp 'S' : jr nz,.raw
        ld hl,4    : call read_byte_at_offset : cp '3' : jr nz,.raw
        ld hl,5    : call read_byte_at_offset : cp 'D' : jr nz,.raw
        ld hl,6    : call read_byte_at_offset : cp 'O' : jr nz,.raw
        ld hl,7    : call read_byte_at_offset : cp 'S' : jr nz,.raw
        ld hl,128
        ld (basStart),hl
        ret
.raw
        ld hl,0
        ld (basStart),hl
        ret


count_lines
        ld hl,0
        ld (totalLines),hl
        ld hl,(basStart)
        ld (scanOffset),hl
.loop
        ld hl,(scanOffset)
        call offset_at_end
        jr nc,.done
        call skip_bas_line
        ld hl,(totalLines)
        inc hl
        ld (totalLines),hl
        jr .loop
.done
        ret


; advance scanOffset past one BASIC line
; line: [line_hi][line_lo][len_lo][len_hi][content+$0D (len bytes total)]
skip_bas_line
        ld hl,(scanOffset)
        call offset_at_end
        ret nc
        push hl                         ; save scanOffset value
        inc hl
        inc hl                          ; hl = scanOffset+2 = len_lo position
        call read_byte_at_offset        ; A = len_lo
        ld e,a
        ld hl,(scanOffset)
        inc hl
        inc hl
        inc hl                          ; hl = scanOffset+3 = len_hi position
        call read_byte_at_offset        ; A = len_hi
        ld d,a
        pop hl                          ; hl = scanOffset
        ld bc,4
        add hl,bc
        add hl,de                       ; hl = scanOffset + 4 + len
        ld (scanOffset),hl
        ret


render_page
        call clear_text_area
        ld hl,(curOffset)
        ld (renderOffset),hl
        ld de,TEXT_TOP
        xor a
        ld (textRow),a
        call render_status
        ld de,TEXT_TOP                  ; reload: render_status clobbers de
.loop
        ld a,(textRow)
        cp TEXT_ROWS
        ret nc
        ld hl,(renderOffset)
        call offset_at_end
        ret nc
        push de
        call render_bas_line            ; de = screen row addr, renderOffset = data
        pop de
        ld hl,160
        add hl,de
        ex de,hl                        ; de = next screen row
        ld hl,textRow
        inc (hl)
        ld hl,(renderOffset)
        ld (scanOffset),hl
        push de
        call skip_bas_line              ; skip_bas_line clobbers de via d,e
        pop de
        ld hl,(scanOffset)
        ld (renderOffset),hl
        jr .loop


; render one BASIC line: data from renderOffset, screen at de
render_bas_line
        ld (lineStart),de
        xor a
        ld (textCol),a
        call clear_screen_line
        ld de,(lineStart)
        ; read big-endian line number into HL
        ld hl,(renderOffset)
        call read_byte_at_offset
        ld b,a
        ld hl,(renderOffset)
        inc hl
        call read_byte_at_offset
        ld l,a
        ld h,b                          ; HL = line number
        push de
        call make_decimal               ; fills numBuf, clobbers de
        pop de
        call print_numBuf               ; print numBuf chars via de
        ld a,(textCol)
        cp TEXT_COLS
        ret nc
        ld a," "
        call put_char
        ld hl,textCol
        inc (hl)
        ; content starts at renderOffset+4
        ld hl,(renderOffset)
        ld bc,4
        add hl,bc
        ld (contentOffset),hl
.loop
        ld a,(textCol)
        cp TEXT_COLS
        ret nc
        ld hl,(contentOffset)
        call offset_at_end
        ret nc
        call read_byte_at_offset
        ld hl,(contentOffset)
        inc hl
        ld (contentOffset),hl
        cp $0D
        ret z
        cp $0E
        jr z,.skip_float
        cp $81
        jr nc,.keyword
        cp $20
        jr c,.loop                      ; skip control codes below space
        call put_char
        ld hl,textCol
        inc (hl)
        jr .loop
.skip_float
        ; $0E is a number marker; 5-byte ZX float follows — skip it
        ld hl,(contentOffset)
        ld bc,5
        add hl,bc
        ld (contentOffset),hl
        jr .loop
.keyword
        sub $81                         ; convert token to 0-based index
        call print_token
        ld a,(textCol)
        cp TEXT_COLS
        jr nc,.loop
        ld a,' '
        call put_char
        ld hl,textCol
        inc (hl)
        jr .loop


; print zero-terminated token string by 0-based index in A
; uses DE (tilemap write pointer) and (textCol)
print_token
        ld b,a
        ld hl,tokenTable
        or a
        jr z,.print
.skip
        ld a,(hl)
        inc hl
        or a
        jr nz,.skip                     ; scan past current token chars
        djnz .skip                      ; one token done; if more, keep skipping
.print
        ld a,(hl)
        or a
        ret z
        push hl
        call put_char
        ld hl,textCol
        inc (hl)
        ld a,(textCol)
        pop hl
        inc hl
        cp TEXT_COLS
        ret nc
        jr .print


; print zero-terminated string from numBuf to tilemap at DE
print_numBuf
        ld hl,numBuf
.loop
        ld a,(hl)
        or a
        ret z
        push hl
        call put_char
        ld hl,textCol
        inc (hl)
        ld a,(textCol)
        pop hl
        inc hl
        cp TEXT_COLS
        ret nc
        jr .loop


scroll_up
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


scroll_down
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
        ld (renderOffset),hl
        ld de,TEXT_TOP
        call render_bas_line
        ret


render_bottom_line
        ld hl,(curOffset)
        ld (scanOffset),hl
        ld b,TEXT_ROWS-1
.scan
        push bc
        call skip_bas_line
        pop bc
        djnz .scan
        ld hl,(scanOffset)
        ld (renderOffset),hl
        ld de,TEXT_TOP+160*(TEXT_ROWS-1)
        ld hl,(renderOffset)
        call offset_at_end
        jr nc,.blank
        call render_bas_line
        ret
.blank
        ld (lineStart),de
        call clear_screen_line
        ret


; returns NZ if moved, Z if already at end
move_line_down
        ld hl,(curOffset)
        call offset_at_end
        jr nc,.no_move
        ld hl,(curOffset)
        ld (scanOffset),hl
        call skip_bas_line
        ld hl,(scanOffset)
        call offset_at_end
        jr nc,.no_move
        ld hl,(scanOffset)
        ld (curOffset),hl
        ld hl,(curLine)
        inc hl
        ld (curLine),hl
        xor a
        inc a                           ; NZ = moved
        ret
.no_move
        xor a                           ; Z = at end
        ret


; returns NZ if moved, Z if already at beginning
move_line_up
        ld hl,(curLine)
        ld a,h
        or l
        ret z                           ; Z = already at line 0
        ld hl,(basStart)
        ld (scanOffset),hl
        ld hl,(curLine)
        dec hl                          ; hl = target line index
        ld a,h
        or l
        jr z,.done                      ; target is line 0; scanOffset already 0
        ld b,h
        ld c,l
.scan
        call skip_bas_line
        dec bc
        ld a,b
        or c
        jr nz,.scan
.done
        ld hl,(scanOffset)
        ld (curOffset),hl
        ld hl,(curLine)
        dec hl
        ld (curLine),hl
        xor a
        inc a                           ; NZ = moved
        ret


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


put_char
        ld (de),a
        inc de
        ld a,16
        ld (de),a
        inc de
        ret


render_status
        ld de,STATUS_INNER
        ld b,TEXT_COLS
.cl
        ld a,32
        ld (de),a
        inc de
        ld a,32
        ld (de),a
        inc de
        djnz .cl
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
        ld hl,36*256+31
        ld a,32
        ld de,helpText
        call call_print
        ret


make_decimal
        ld de,numBuf
        ld b,5
        ld a,32
.cl
        ld (de),a
        inc de
        djnz .cl
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
.dl
        or a
        sbc hl,bc
        jr c,.dd
        inc a
        jr .dl
.dd
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
.sub20  ld a,h : sub $20 : jr .set_h
.sub40  ld a,h : sub $40 : jr .set_h
.sub60  ld a,h : sub $60 : jr .set_h
.sub80  ld a,h : sub $80 : jr .set_h
.suba0  ld a,h : sub $A0 : jr .set_h
.subc0  ld a,h : sub $C0 : jr .set_h
.sube0  ld a,h : sub $E0 : jr .set_h
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

ctxPtr          defw 0
svcPtr          defw 0
dataPagesPtr    defw 0
loadedSize      defw 0
curOffset       defw 0
basStart        defw 0
renderOffset    defw 0
scanOffset      defw 0
contentOffset   defw 0
curLine         defw 0
totalLines      defw 0
lineStart       defw 0
textCol         defb 0
textRow         defb 0
pageCount       defb 0
tmpMappedH      defb 0
numBuf          defs 6
title           defb "BASIC:",0
lineLabel       defb "Line:",0
slashText       defb "/",0
helpText        defb "BREAK=exit",0

tokenTable
        ; ZX Spectrum Next tokens: $81-$A4
        defb "TIME",0       ; $81
        defb "PRIVATE",0    ; $82
        defb "IF",0         ; $83
        defb "ENDIF",0      ; $84
        defb "EXIT",0       ; $85
        defb "REF",0        ; $86
        defb "PEEK$",0      ; $87
        defb "REG",0        ; $88
        defb "DPOKE",0      ; $89
        defb "DPEEK",0      ; $8A
        defb "MOD",0        ; $8B
        defb "<<",0         ; $8C
        defb ">>",0         ; $8D
        defb "UNTIL",0      ; $8E
        defb "ERROR",0      ; $8F
        defb "ON",0         ; $90
        defb "DEFPROC",0    ; $91
        defb "ENDPROC",0    ; $92
        defb "PROC",0       ; $93
        defb "LOCAL",0      ; $94
        defb "DRIVER",0     ; $95
        defb "WHILE",0      ; $96
        defb "REPEAT",0     ; $97
        defb "ELSE",0       ; $98
        defb "REMOUNT",0    ; $99
        defb "BANK",0       ; $9A
        defb "TILE",0       ; $9B
        defb "LAYER",0      ; $9C
        defb "PALETTE",0    ; $9D
        defb "SPRITE",0     ; $9E
        defb "PWD",0        ; $9F
        defb "CD",0         ; $A0
        defb "MKDIR",0      ; $A1
        defb "RMDIR",0      ; $A2
        defb "SPECTRUM",0   ; $A3
        defb "PLAY",0       ; $A4
        ; Classic ZX Spectrum tokens: $A5-$FF
        defb "RND",0        ; $A5
        defb "INKEY$",0     ; $A6
        defb "PI",0         ; $A7
        defb "FN",0         ; $A8
        defb "POINT",0      ; $A9
        defb "SCREEN$",0    ; $AA
        defb "ATTR",0       ; $AB
        defb "AT",0         ; $AC
        defb "TAB",0        ; $AD
        defb "VAL$",0       ; $AE
        defb "CODE",0       ; $AF
        defb "VAL",0        ; $B0
        defb "LEN",0        ; $B1
        defb "SIN",0        ; $B2
        defb "COS",0        ; $B3
        defb "TAN",0        ; $B4
        defb "ASN",0        ; $B5
        defb "ACS",0        ; $B6
        defb "ATN",0        ; $B7
        defb "LN",0         ; $B8
        defb "EXP",0        ; $B9
        defb "INT",0        ; $BA
        defb "SQR",0        ; $BB
        defb "SGN",0        ; $BC
        defb "ABS",0        ; $BD
        defb "PEEK",0       ; $BE
        defb "IN",0         ; $BF
        defb "USR",0        ; $C0
        defb "STR$",0       ; $C1
        defb "CHR$",0       ; $C2
        defb "NOT",0        ; $C3
        defb "BIN",0        ; $C4
        defb "OR",0         ; $C5
        defb "AND",0        ; $C6
        defb "<=",0         ; $C7
        defb ">=",0         ; $C8
        defb "<>",0         ; $C9
        defb "LINE",0       ; $CA
        defb "THEN",0       ; $CB
        defb "TO",0         ; $CC
        defb "STEP",0       ; $CD
        defb "DEF FN",0     ; $CE
        defb "CAT",0        ; $CF
        defb "FORMAT",0     ; $D0
        defb "MOVE",0       ; $D1
        defb "ERASE",0      ; $D2
        defb "OPEN #",0     ; $D3
        defb "CLOSE #",0    ; $D4
        defb "MERGE",0      ; $D5
        defb "VERIFY",0     ; $D6
        defb "BEEP",0       ; $D7
        defb "CIRCLE",0     ; $D8
        defb "INK",0        ; $D9
        defb "PAPER",0      ; $DA
        defb "FLASH",0      ; $DB
        defb "BRIGHT",0     ; $DC
        defb "INVERSE",0    ; $DD
        defb "OVER",0       ; $DE
        defb "OUT",0        ; $DF
        defb "LPRINT",0     ; $E0
        defb "LLIST",0      ; $E1
        defb "STOP",0       ; $E2
        defb "READ",0       ; $E3
        defb "DATA",0       ; $E4
        defb "RESTORE",0    ; $E5
        defb "NEW",0        ; $E6
        defb "BORDER",0     ; $E7
        defb "CONTINUE",0   ; $E8
        defb "DIM",0        ; $E9
        defb "REM",0        ; $EA
        defb "FOR",0        ; $EB
        defb "GO TO",0      ; $EC
        defb "GO SUB",0     ; $ED
        defb "INPUT",0      ; $EE
        defb "LOAD",0       ; $EF
        defb "LIST",0       ; $F0
        defb "LET",0        ; $F1
        defb "PAUSE",0      ; $F2
        defb "NEXT",0       ; $F3
        defb "POKE",0       ; $F4
        defb "PRINT",0      ; $F5
        defb "PLOT",0       ; $F6
        defb "RUN",0        ; $F7
        defb "SAVE",0       ; $F8
        defb "RANDOMIZE",0  ; $F9
        defb "IF",0         ; $FA
        defb "CLS",0        ; $FB
        defb "DRAW",0       ; $FC
        defb "CLEAR",0      ; $FD
        defb "RETURN",0     ; $FE
        defb "COPY",0       ; $FF

plugin_end
        assert plugin_end - plugin_start <= VIEW_PLUGIN_SIZE
        SAVEBIN "plugin/bas.ccp", VIEW_PLUGIN_ADDRESS, VIEW_PLUGIN_SIZE
