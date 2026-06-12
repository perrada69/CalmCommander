        DEVICE ZXSPECTRUMNEXT
        org VIEW_PLUGIN_ADDRESS

        include "plugin_api.i.asm"

; ---- screen layout ----
; Tilemap: 160 bytes/row, 2 bytes/tile (char + attr)
; Window: col=0 row=3 width=78 height=24
;   inner rows 4-25 (borders at 3 and 26)
;   row 4  : title  (TAP: filename  NN blk)
;   row 5  : column header
;   rows 6-23: entries (CONTENT_ROWS=18)
;   row 25 : help

WIN_COL         equ 0
WIN_ROW         equ 3
WIN_WIDTH       equ 78
WIN_HEIGHT      equ 24

TITLE_ROW       equ 4
HEADER_ROW      equ 5
CONTENT_ROW     equ 6
CONTENT_ROWS    equ 18
HELP_ROW        equ 25

ATTR_NORMAL     equ 16
ATTR_TITLE      equ 208
ATTR_HEADER     equ 176
ATTR_BASIC      equ 160
ATTR_CODE       equ 192
ATTR_SELECTED   equ 112
ATTR_DATA       equ 16

TYPE_BASIC      equ 0
TYPE_NUMARRAY   equ 1
TYPE_STRARRAY   equ 2
TYPE_CODE       equ 3

MAX_ENTRIES     equ 64


; ================================================================
; Plugin entry point
; HL = context pointer, DE = services pointer
; ================================================================
plugin_start
        ld (ctxPtr),hl
        ld (svcPtr),de
        call patch_services
        call init_context
        call scan_tap_blocks

        ld hl,WIN_COL*256+WIN_ROW
        ld bc,WIN_WIDTH*256+WIN_HEIGHT
        ld a,ATTR_NORMAL
        call call_window

        call render_title
        call render_col_header
        call render_help
        call render_page
        call render_name_preview

.input
        call call_input
        or a
        jr z,.input
        cp 1
        ret z           ; BREAK: return A=1 (exit viewer)

        cp 10
        jp z,.down
        cp 11
        jp z,.up
        cp 9
        jp z,.pgdn
        cp 8
        jp z,.pgup
        cp "E"
        jp z,.extract_all
        cp "e"
        jp z,.extract
        cp "d"
        jp z,.toggle_p3dos
        jp .input

; ---- cursor down ----
.down
        ld a,(curEntry)
        ld b,a
        ld a,(totalBlocks)
        dec a           ; last valid index
        cp b
        jp z,.input     ; already at last
        ld a,(curEntry)
        inc a
        ld (curEntry),a
        call set_selected_entry
        ; scroll if new curEntry >= topEntry + CONTENT_ROWS
        ld b,a
        ld a,(topEntry)
        add a,CONTENT_ROWS
        cp b
        jp nc,.do_render
        ld hl,topEntry
        inc (hl)
        jp .do_render

; ---- cursor up ----
.up
        ld a,(curEntry)
        or a
        jp z,.input     ; already at first
        dec a
        ld (curEntry),a
        call set_selected_entry
        ; scroll if new curEntry < topEntry
        ld b,a
        ld a,(topEntry)
        cp b
        jp c,.do_render ; topEntry < curEntry: no scroll
        or a
        jp z,.do_render ; topEntry = 0: can't scroll up
        ld hl,topEntry
        dec (hl)
        jp .do_render

; ---- page down ----
.pgdn
        ld a,(totalBlocks)
        or a
        jp z,.input
        dec a
        ld b,a          ; B = last valid index
        ld a,(curEntry)
        cp b
        jp z,.input
        add a,CONTENT_ROWS
        cp b
        jp nc,.pgdn_cap
        ld (curEntry),a
        call set_selected_entry
        jp .pgdn_top
.pgdn_cap
        ld a,b
        ld (curEntry),a
        call set_selected_entry
.pgdn_top
        ld a,(curEntry)
        sub CONTENT_ROWS-1
        jr nc,.pgdn_set
        xor a
.pgdn_set
        ld (topEntry),a
        jp .do_render

; ---- page up ----
.pgup
        ld a,(curEntry)
        or a
        jp z,.input
        sub CONTENT_ROWS
        jr nc,.pgup_set
        xor a
.pgup_set
        ld (curEntry),a
        call set_selected_entry
        ld (topEntry),a

.do_render
        call render_page
        call render_name_preview
        ; wait for key release before accepting next input
.wait_release
        call call_input
        or a
        jr nz,.wait_release
        jp .input


; ---- extract ('E' key) ----
.extract
        call do_extract_prepared
        jp .wait_release


; ---- extract all ('CAPS+E') ----
.extract_all
        call do_extract_all
        jp .wait_release

; ---- toggle +3DOS header ('d' key) ----
.toggle_p3dos
        ld a,(p3dosEnabled)
        xor 1
        ld (p3dosEnabled),a
        call render_p3dos_flag
        jp .wait_release


; ================================================================
; get_entry_offset: A = entry index → HL = file offset (from entryOffsets[A])
; ================================================================
get_entry_offset
        add a,a             ; index * 2
        ld l,a
        ld h,0
        ld de,entryOffsets
        add hl,de
        ld e,(hl)
        inc hl
        ld d,(hl)
        ex de,hl
        ret


set_selected_entry
        ld (selectedEntry),a
        ret


; ================================================================
; do_extract: A = entry index. Extracts payload to file.
; HEADER (flag=0, len=19) → extracts paired DATA block, names it from header.
; DATA/other → extracts this block's payload, names it BLCKnn.BIN.
; The viewer extract service performs the actual esxDOS write.
; ================================================================
do_extract
        ld (extractEntry),a
        xor a
        ld (extractStatus),a
        call prepare_extract_payload
        cp 255
        ret z
        ld (payloadEntry),a
        ld a,(payloadEntry)
        call get_payload_range

        ; HL = source offset (16-bit), BC = byte count
        ld (extractOff),hl
        ld h,b : ld l,c : ld (extractCnt),hl
        ld a,(extractEntry)
        call prepare_extract_name
        call do_extract_prepared
        ret


do_extract_prepared
        xor a
        ld (extractStatus),a
        call clear_debug_area
        ld de,strExportName
        ld hl,42*256+CONTENT_ROW
        ld a,ATTR_CODE
        call call_print
        ld de,extractName
        ld hl,48*256+CONTENT_ROW
        ld a,ATTR_CODE
        call call_print
        ld hl,(extractCnt)
        call make_decimal
        ld de,strExportLen
        ld hl,42*256+(CONTENT_ROW+1)
        ld a,ATTR_CODE
        call call_print
        ld de,numBuf
        ld hl,48*256+(CONTENT_ROW+1)
        ld a,ATTR_CODE
        call call_print
        ld de,strExportIdx
        ld hl,56*256+(CONTENT_ROW+1)
        ld a,ATTR_CODE
        call call_print
        ld a,(payloadEntry)
        inc a
        ld l,a
        ld h,0
        call make_decimal
        ld de,numBuf+3
        ld hl,60*256+(CONTENT_ROW+1)
        ld a,ATTR_CODE
        call call_print

        call setup_p3dos_context
        ld hl,extractName
        ld de,(extractOff)
        ld bc,(extractCnt)
        call call_extract
        jr c,.export_fail
        xor a
        ld (extractStatus),a
        ld de,strExportOK
        ld hl,42*256+(CONTENT_ROW+2)
        ld a,ATTR_CODE
        call call_print
        ret
.export_fail
        ld (extractStatus),a
        add a,'0'
        ld (strExportFailCode),a
        ld de,strExportFail
        ld hl,42*256+(CONTENT_ROW+2)
        ld a,ATTR_CODE
        call call_print
.de_ret
        ret


do_extract_one
        call do_extract
        ret


do_extract_all
        xor a
        ld (bulkIdx),a
        ld hl,0
        ld (bulkPtr),hl
.loop
        ld hl,(bulkPtr)
        ld de,(loadedSize)
        or a
        sbc hl,de
        jp nc,.all_ok

        ld a,(bulkIdx)
        ld b,a
        ld a,(totalBlocks)
        cp b
        jp z,.all_ok
        jp c,.all_ok

        ld a,(bulkIdx)
        ld (extractEntry),a
        ld (payloadEntry),a

        ld hl,(bulkPtr)
        call read_byte_at_offset
        ld (tmpBlockLen),a
        ld hl,(bulkPtr)
        inc hl
        call read_byte_at_offset
        ld (tmpBlockLen+1),a
        ld hl,(tmpBlockLen)
        ld a,h
        or l
        jp z,.all_ok

        ld hl,(bulkPtr)
        inc hl
        inc hl
        call read_byte_at_offset
        ld (tmpBlockFlag),a

        ld hl,(bulkPtr)
        ld de,(tmpBlockLen)
        add hl,de
        inc hl
        inc hl
        ld (bulkNextPtr),hl

        ld a,(tmpBlockFlag)
        or a
        jr nz,.raw_block
        ld hl,(tmpBlockLen)
        ld de,19
        or a
        sbc hl,de
        jr nz,.raw_block
        jr .header_block

.raw_block
        ld a,(bulkIdx)
        ld (extractEntry),a
        ld (payloadEntry),a
        call build_blk_name
        ld hl,(bulkPtr)
        inc hl
        inc hl
        inc hl
        ld (extractOff),hl
        ld hl,(tmpBlockLen)
        dec hl
        dec hl
        ld (extractCnt),hl
        ld hl,(bulkNextPtr)
        push hl
        ld a,(bulkIdx)
        inc a
        push af
        call do_extract_prepared
        pop af
        ld (bulkIdx),a
        pop hl
        ld (bulkPtr),hl
        ld a,(extractStatus)
        or a
        jp nz,.all_fail
        jp .loop

.header_block
        ld a,(bulkIdx)
        ld (nameSourceEntry),a
        call build_name_from_source

        ld a,(bulkIdx)
        inc a
        ld b,a
        ld a,(totalBlocks)
        cp b
        jp z,.skip_header
        jp c,.skip_header

        ld hl,(bulkNextPtr)
        call read_byte_at_offset
        ld (tmpBlockLen),a
        ld hl,(bulkNextPtr)
        inc hl
        call read_byte_at_offset
        ld (tmpBlockLen+1),a
        ld hl,(tmpBlockLen)
        ld a,h
        or l
        jp z,.skip_header

        ld hl,(bulkNextPtr)
        inc hl
        inc hl
        call read_byte_at_offset
        or a
        jp z,.skip_header

        ld a,(bulkIdx)
        inc a
        ld (payloadEntry),a
        ld hl,(bulkNextPtr)
        inc hl
        inc hl
        inc hl
        ld (extractOff),hl
        ld hl,(tmpBlockLen)
        dec hl
        dec hl
        ld (extractCnt),hl

        ld hl,(bulkNextPtr)
        ld de,(tmpBlockLen)
        add hl,de
        inc hl
        inc hl
        push hl
        ld a,(bulkIdx)
        inc a
        inc a
        push af
        call do_extract_prepared
        pop af
        ld (bulkIdx),a
        pop hl
        ld (bulkPtr),hl
        ld a,(extractStatus)
        or a
        jp nz,.all_fail
        jp .loop

.skip_header
        ld hl,(bulkNextPtr)
        ld (bulkPtr),hl
        ld a,(bulkIdx)
        inc a
        ld (bulkIdx),a
        jp .loop

.all_ok
        call clear_debug_area
        ld de,strExportAllOK
        ld hl,42*256+CONTENT_ROW
        ld a,ATTR_CODE
        call call_print
        ret

.all_fail
        add a,'0'
        ld (strExportAllFailCode),a
        call clear_debug_area
        ld de,strExportAllFail
        ld hl,42*256+CONTENT_ROW
        ld a,ATTR_CODE
        call call_print
        ret


render_name_preview
        ld a,(curEntry)
        call set_selected_entry
        ld (extractEntry),a
        call prepare_extract_payload
        ld (payloadEntry),a
        ld a,(curEntry)
        call prepare_extract_name
        call clear_debug_area
        ld de,strExportName
        ld hl,42*256+CONTENT_ROW
        ld a,ATTR_CODE
        call call_print
        ld de,extractName
        ld hl,48*256+CONTENT_ROW
        ld a,ATTR_CODE
        call call_print
        ld a,(payloadEntry)
        cp 255
        ret z
        call get_payload_range
        ld (extractOff),hl
        ld h,b : ld l,c : ld (extractCnt),hl
        ld h,b
        ld l,c
        call make_decimal
        ld de,strExportLen
        ld hl,42*256+(CONTENT_ROW+1)
        ld a,ATTR_CODE
        call call_print
        ld de,numBuf
        ld hl,48*256+(CONTENT_ROW+1)
        ld a,ATTR_CODE
        call call_print
        ld de,strExportIdx
        ld hl,56*256+(CONTENT_ROW+1)
        ld a,ATTR_CODE
        call call_print
        ld a,(payloadEntry)
        inc a
        ld l,a
        ld h,0
        call make_decimal
        ld de,numBuf+3
        ld hl,60*256+(CONTENT_ROW+1)
        ld a,ATTR_CODE
        call call_print
        ret


prepare_extract_payload
        ld (extractEntry),a
        call is_standard_header
        jr nz,.raw
        ld a,(extractEntry)
        jp get_header_data_entry
.raw
        ld a,(extractEntry)
        ret


prepare_extract_name
        ld (extractEntry),a
        call is_standard_header
        jr nz,.raw
        ld a,(extractEntry)
        ld (nameSourceEntry),a
        jp build_name_from_source
.raw
        ld a,(extractEntry)
        call set_immediate_name_source
        jp nz,build_blk_name
        jp build_name_from_source


; A = entry index. Returns A = parent header index, or 255 if none.
get_entry_parent
        ld l,a
        ld h,0
        ld de,entryParents
        add hl,de
        ld a,(hl)
        ret


; A = header entry index. Returns following data entry, or 255 if not paired.
get_header_data_entry
        ld (parentCandidate),a
        ld l,a
        ld a,(totalBlocks)
        dec a
        cp l
        jr z,.none
        ld a,(parentCandidate)
        inc a
        ld (parentCandidate),a
        call is_standard_header
        jr z,.none
        ld a,(parentCandidate)
        ret
.none
        ld a,255
        ret


; A = entry index. Returns previous entry if it is a standard header, else 255.
get_immediate_parent
        or a
        jr z,.none
        dec a
        ld (parentCandidate),a
        call is_standard_header
        jr nz,.none
        ld a,(parentCandidate)
        ret
.none
        ld a,255
        ret


; A = entry index. Z if previous entry is a header and nameSourceEntry was set.
set_immediate_name_source
        or a
        jr z,.none
        dec a
        ld (nameSourceEntry),a
        call is_standard_header
        jr nz,.none
        xor a
        ret
.none
        or 1
        ret


; A = entry index. Z if this entry is a standard Spectrum header.
is_standard_header
        call get_entry_offset
        ld (readPtr),hl
        call read_next_byte
        ld (tmpBlockLen),a
        call read_next_byte
        ld (tmpBlockLen+1),a
        call read_next_byte
        ld (tmpBlockFlag),a
        ld a,(tmpBlockFlag)
        or a
        jr nz,.not_header
        ld a,(tmpBlockLen+1)
        or a
        jr nz,.not_header
        ld a,(tmpBlockLen)
        cp 19
        ret z
.not_header
        or 1
        ret


; A = entry index. Returns HL = payload offset, BC = payload length.
get_payload_range
        call get_entry_offset
        ld (tmpRangeStart),hl
        push hl
        call read_byte_at_offset
        ld c,a
        pop hl
        inc hl
        push hl
        call read_byte_at_offset
        ld b,a
        pop hl
        ld hl,(tmpRangeStart)
        inc hl
        inc hl
        inc hl                         ; source is past length + flag
        dec bc                         ; skip flag
        dec bc                         ; skip checksum
        ret


build_blk_name
        ld hl,strBlkNameTemplate
        ld de,extractName
        ld bc,11
        ldir
        ld hl,extractName
        inc hl
        inc hl
        inc hl
        inc hl
        ld a,(extractEntry)
        inc a
        ld b,'0'
.tens
        cp 10
        jr c,.ones
        sub 10
        inc b
        jr .tens
.ones
        ld c,a
        ld a,b : ld (hl),a : inc hl
        ld a,c : add a,'0' : ld (hl),a
        ret


clear_extract_name
        push af
        push bc
        push hl
        ld hl,extractName
        ld b,16
.loop
        xor a
        ld (hl),a
        inc hl
        djnz .loop
        pop hl
        pop bc
        pop af
        ret


; A = standard header entry index. Builds output name from the header name.
build_name_from_source
        ld a,(nameSourceEntry)
        call clear_extract_name
        call get_entry_offset
        ld (readPtr),hl
        call read_next_byte
        call read_next_byte
        call read_next_byte
        call read_next_byte            ; type
        ld (extractHeaderType),a
        ld hl,extractName
        xor a
        ld (extractNameUsed),a
        ld (extractNameHasDot),a
        ld e,10
.name_loop
        ld (extractNamePtr),hl
        call read_next_byte
        ld hl,(extractNamePtr)
        cp ' '
        jr z,.skip_char
        call sanitize_filename_char
        cp '.'
        jr nz,.store_char
        ld a,1
        ld (extractNameHasDot),a
        ld a,'.'
.store_char
        ld (hl),a
        inc hl
        ld a,1
        ld (extractNameUsed),a
        jr .skip_char
.skip_char
        dec e
        jr nz,.name_loop
        ld a,(extractNameUsed)
        or a
        jr nz,.has_name
        call build_blk_name
        ret
.has_name
        ld a,(extractNameHasDot)
        or a
        jr z,.add_extension
        ld (hl),0
        ret
.add_extension
        ld a,(extractHeaderType)
        or a
        jr nz,.ext_bin
        ld a,'.' : ld (hl),a : inc hl
        ld a,'B' : ld (hl),a : inc hl
        ld a,'A' : ld (hl),a : inc hl
        ld a,'S' : ld (hl),a : inc hl
        jr .done
.ext_bin
        ld a,'.' : ld (hl),a : inc hl
        ld a,'B' : ld (hl),a : inc hl
        ld a,'I' : ld (hl),a : inc hl
        ld a,'N' : ld (hl),a : inc hl
.done
        ld (hl),0
        ret


sanitize_filename_char
        cp 'A'
        jr c,.maybe_digit
        cp 'Z'+1
        ret c
.maybe_digit
        cp '0'
        jr c,.maybe_dot
        cp '9'+1
        ret c
.maybe_dot
        cp 'a'
        jr c,.dot_test
        cp 'z'+1
        ret c
.dot_test
        cp '.'
        ret z
        cp '_'
        ret z
        cp '-'
        ret z
        ld a,'_'
        ret


; ================================================================
; read_full_tap_header: A = header entry index.
; Reads type, param1, param2 from the TAP header block.
; Sets extractHeaderType, tapParam1, tapParam2.
; ================================================================
read_full_tap_header
        call get_entry_offset
        ld (readPtr),hl
        call read_next_byte         ; len_lo  (skip)
        call read_next_byte         ; len_hi  (skip)
        call read_next_byte         ; flag    (skip)
        call read_next_byte         ; type
        ld (extractHeaderType),a
        ld b,10
.skip_name
        call read_next_byte         ; name chars (skip)
        djnz .skip_name
        call read_next_byte         ; data_len lo (skip – viewer uses extractCnt)
        call read_next_byte         ; data_len hi (skip)
        call read_next_byte         ; param1 lo
        ld (tapParam1),a
        call read_next_byte         ; param1 hi
        ld (tapParam1+1),a
        call read_next_byte         ; param2 lo
        ld (tapParam2),a
        call read_next_byte         ; param2 hi
        ld (tapParam2+1),a
        ret


; ================================================================
; setup_p3dos_context: set VIEWCTX_P3DOS_* fields before extraction.
; Uses extractEntry to locate the TAP header block.
; Sets type/param1/param2 in context, or $FF if no header applies.
; ================================================================
setup_p3dos_context
        ld a,(p3dosEnabled)
        or a
        jp z,.no_p3dos
        ld a,(extractEntry)
        call is_standard_header
        jr z,.from_this_header
        ; not a header: check if the previous entry is a paired header
        ld a,(extractEntry)
        call get_immediate_parent
        cp 255
        jr z,.no_p3dos
        ; A = parent header entry index
        jr .read_header
.from_this_header
        ld a,(extractEntry)
.read_header
        call read_full_tap_header
        ld ix,(ctxPtr)
        ld a,(extractHeaderType)
        ld (ix+VIEWCTX_P3DOS_TYPE),a
        ld a,(tapParam1)
        ld (ix+VIEWCTX_P3DOS_P1),a
        ld a,(tapParam1+1)
        ld (ix+VIEWCTX_P3DOS_P1+1),a
        ld a,(tapParam2)
        ld (ix+VIEWCTX_P3DOS_P2),a
        ld a,(tapParam2+1)
        ld (ix+VIEWCTX_P3DOS_P2+1),a
        ret
.no_p3dos
        ld ix,(ctxPtr)
        ld a,$FF
        ld (ix+VIEWCTX_P3DOS_TYPE),a
        ret


clear_debug_area
        ld b,CONTENT_ROW
.row
        ld de,strDebugBlank
        ld h,42
        ld l,b
        ld a,ATTR_NORMAL
        push bc
        call call_print
        pop bc
        inc b
        ld a,b
        cp CONTENT_ROW+5
        jr c,.row
        ret


; ================================================================
; init_context: read plugin context into local variables
; ================================================================
init_context
        ld ix,(ctxPtr)
        ld l,(ix+VIEWCTX_DATA_PAGES)
        ld h,(ix+VIEWCTX_DATA_PAGES+1)
        ld (dataPagesPtr),hl
        ld a,(ix+VIEWCTX_PAGE_COUNT)
        ld (pageCount),a
        ld l,(ix+VIEWCTX_SIZE_LO)
        ld h,(ix+VIEWCTX_SIZE_LO+1) ; HL = lower 16 bits of file size
        ld a,(ix+VIEWCTX_SIZE_HI)
        or (ix+VIEWCTX_SIZE_HI+1)   ; nonzero → file > 64KB → cap
        jr z,.store
.cap    ld hl,$ffff
.store  ld (loadedSize),hl
        xor a
        ld (curEntry),a
        ld (topEntry),a
        call set_selected_entry
        ret


; ================================================================
; scan_tap_blocks
; Walk through file data, collect start offset of each block.
; TAP block: [len_lo][len_hi][...len bytes of data...]
; Stores up to MAX_ENTRIES start offsets in entryOffsets[].
; ================================================================
scan_tap_blocks
        xor a
        ld (totalBlocks),a
        dec a
        ld (lastHeader),a
        ld hl,0
        ld (readPtr),hl     ; readPtr = current file offset

.loop
        ; bounds check: readPtr >= loadedSize?
        ld hl,(readPtr)
        ld de,(loadedSize)
        or a
        sbc hl,de
        jr nc,.done

        ; table full?
        ld a,(totalBlocks)
        cp MAX_ENTRIES
        jr nc,.done
        ld (scanIndex),a

        ; store current readPtr in entryOffsets[totalBlocks]
        add a,a             ; index * 2
        ld c,a
        ld b,0
        ld hl,entryOffsets
        add hl,bc
        ld de,(readPtr)
        ld (hl),e
        inc hl
        ld (hl),d

        ; totalBlocks++
        ld hl,totalBlocks
        inc (hl)

        ; read 2-byte block length (LE), advances readPtr by 2
        call read_next_byte
        ld e,a
        call read_next_byte
        ld d,a              ; DE = block length
        ld (tmpBlockLen),de

        ; zero-length block = malformed, stop
        ld a,d
        or e
        jr z,.done

        ; read flag byte and store parent relationship for this entry
        call read_next_byte
        ld (tmpBlockFlag),a
        ld a,(scanIndex)
        ld c,a
        ld b,0
        ld hl,entryParents
        add hl,bc
        ld a,(tmpBlockFlag)
        or a
        jr nz,.not_header
        ld a,(tmpBlockLen+1)
        or a
        jr nz,.not_header
        ld a,(tmpBlockLen)
        cp 19
        jr nz,.not_header
        ld (hl),255
        ld a,(scanIndex)
        ld (lastHeader),a
        jr .advance
.not_header
        ld a,(lastHeader)
        ld (hl),a
        ld a,255
        ld (lastHeader),a

.advance
        ; advance readPtr past remaining block data after the flag byte
        ; if addition overflows 16 bits the next block is past the 64KB
        ; address window, so its start address is unknowable — stop here.
        ld de,(tmpBlockLen)
        dec de
        ld hl,(readPtr)
        add hl,de
        jr c,.done          ; overflow: stop scanning
        ld (readPtr),hl
        jr .loop
.done
        ret


; ================================================================
; render_page: clear content area, render visible entries
; ================================================================
render_page
        call clear_content_area

        ld a,(topEntry)     ; A = entry index to render
        ld d,CONTENT_ROW    ; D = screen row

.loop
        ; stop when entry >= totalBlocks
        ld b,a
        ld a,(totalBlocks)
        cp b                ; sets flags: Z if equal, C if totalBlocks < entry
        ld a,b              ; restore entry (flags preserved from cp)
        jr z,.done
        jr c,.done

        ; stop when row >= CONTENT_ROW + CONTENT_ROWS
        ; use B to preserve entry so flags from cp survive
        ld b,a
        ld a,d
        cp CONTENT_ROW+CONTENT_ROWS
        ld a,b              ; restore entry (flags preserved from cp)
        jr nc,.done

        push af             ; save entry index (A) across render_entry call
        push de             ; save D (row) — render_entry clobbers D via mul
        call render_entry   ; A = entry index, D = screen row
        pop de              ; restore D
        pop af              ; restore entry index into A
        inc a               ; next entry
        inc d               ; next row
        jr .loop
.done
        ret


; ================================================================
; render_entry: draw one entry row
; A = entry index (0-based), D = screen row
; ================================================================
render_entry
        ld (renderIdx),a

        ; screen address = $4000 + D*160 + 2  (col 1 inside border)
        ld e,d
        ld d,160
        mul d,e
        ld hl,$4002
        add hl,de
        ld (screenPos),hl

        ; set readPtr to block start offset
        ld a,(renderIdx)
        add a,a
        ld c,a
        ld b,0
        ld hl,entryOffsets
        add hl,bc
        ld e,(hl)
        inc hl
        ld d,(hl)
        ld (readPtr),de

        ; read block length (2 bytes LE)
        call read_next_byte
        ld e,a
        call read_next_byte
        ld d,a
        ld (blockLen),de

        ; read flag byte
        call read_next_byte
        ld (tapFlag),a

        ; standard header: flag==0 AND blockLen==19
        or a
        jr nz,.raw
        ld hl,(blockLen)
        ld de,19
        or a
        sbc hl,de
        jr nz,.raw
        call render_std_header
        ret
.raw
        call render_raw_block
        ret


; ================================================================
; render_std_header: flag==0, len==19
; readPtr now points to type byte
; ================================================================
render_std_header
        ; read type (1 byte)
        call read_next_byte
        ld (tapType),a

        ; read name (10 bytes) into nameBuf
        ld de,nameBuf
        ld b,10
.rn     call read_next_byte
        cp ' '
        jr c,.bad
        cp $7F
        jr c,.ok
.bad    ld a,'?'
.ok     ld (de),a
        inc de
        djnz .rn
        xor a
        ld (de),a           ; null terminate

        ; read data_length (2 bytes LE)
        call read_next_byte
        ld e,a
        call read_next_byte
        ld d,a
        ld (tapDataLen),de

        ; read param1 (2 bytes LE): LINE for BASIC, start addr for CODE
        call read_next_byte
        ld e,a
        call read_next_byte
        ld d,a
        ld (tapParam1),de

        ; choose attribute: SELECTED if curEntry matches, else type-based
        ld a,(renderIdx)
        ld b,a
        ld a,(curEntry)
        cp b
        jr z,.sel
        ld a,(tapType)
        cp TYPE_BASIC
        jr z,.abasic
        cp TYPE_CODE
        jr z,.acode
        ld a,ATTR_NORMAL
        jr .aset
.sel    ld a,(renderIdx)
        call set_selected_entry
        ld a,ATTR_SELECTED
        jr .aset
.abasic ld a,ATTR_BASIC
        jr .aset
.acode  ld a,ATTR_CODE
.aset   ld (curAttr),a

        ld de,(screenPos)

        ; ## (entry number, 2 chars right-justified, 1-based)
        ld a,(renderIdx)
        inc a
        ld l,a
        ld h,0
        call write_num2

        ld a,' '
        call put_char

        ; type (7 chars padded)
        ld a,(tapType)
        call write_type7

        ld a,' '
        call put_char

        ; name (10 chars padded)
        ld hl,nameBuf
        ld b,10
        call write_padded

        ld a,' '
        call put_char

        ; data length (5 decimal chars + 'b')
        ld hl,(tapDataLen)
        call write_dec5
        ld a,'b'
        call put_char
        ld a,' '
        call put_char

        ; param info
        ld a,(tapType)
        cp TYPE_BASIC
        jr z,.pi_basic
        cp TYPE_CODE
        jr z,.pi_code
        cp TYPE_NUMARRAY
        jr z,.pi_arr
        cp TYPE_STRARRAY
        jr z,.pi_arr
        ret

.pi_basic
        ld hl,(tapParam1)
        bit 7,h
        ret nz              ; >= $8000: no autostart line
        ld hl,strLine
        call write_str
        ld hl,(tapParam1)
        call write_dec5
        ret

.pi_code
        ld hl,(tapParam1)
        call write_hex_word
        ret

.pi_arr
        ld hl,(tapParam1)
        ld a,l
        and $3F
        or a
        ret z
        cp 27
        ret nc
        add a,'A'-1
        call put_char
        ret


; ================================================================
; render_raw_block: non-standard block
; ================================================================
render_raw_block
        ld a,(renderIdx)
        ld b,a
        ld a,(curEntry)
        cp b
        jr z,.sel
        ld a,ATTR_DATA
        jr .aset
.sel    ld a,(renderIdx)
        call set_selected_entry
        ld a,ATTR_SELECTED
.aset   ld (curAttr),a

        ld de,(screenPos)

        ; entry number
        ld a,(renderIdx)
        inc a
        ld l,a
        ld h,0
        call write_num2

        ld a,' '
        call put_char

        ; type label
        ld a,(tapFlag)
        or a
        jr nz,.is_data
        ld hl,strUnknown
        jr .wtype
.is_data
        ld hl,strData
.wtype  ld b,7
        call write_padded

        ld a,' '
        call put_char

        ; no name for raw blocks
        ld hl,strDashes
        ld b,10
        call write_padded

        ld a,' '
        call put_char

        ; block length
        ld hl,(blockLen)
        call write_dec5
        ld a,'b'
        call put_char
        ret


; ================================================================
; render_title
; ================================================================
render_title
        ld b,TITLE_ROW
        ld a,ATTR_TITLE
        call clear_row

        ; "TAP: " label
        ld de,strTap
        ld hl,1*256+TITLE_ROW
        ld a,ATTR_TITLE
        call call_print

        ; filename from context
        ld ix,(ctxPtr)
        ld l,(ix+VIEWCTX_FILENAME)
        ld h,(ix+VIEWCTX_FILENAME+1)
        ex de,hl
        ld hl,6*256+TITLE_ROW
        ld a,ATTR_TITLE
        call call_print

        ; block count at right: "NN blk" at col 68
        ld a,(totalBlocks)
        ld h,0
        ld l,a
        call make_decimal   ; fills numBuf
        ld de,numBuf+3      ; last 2 digits (covers 1-64)
        ld hl,68*256+TITLE_ROW
        ld a,ATTR_TITLE
        call call_print
        ld de,strBlk
        ld hl,71*256+TITLE_ROW
        ld a,ATTR_TITLE
        call call_print
        ret


; ================================================================
; render_col_header
; ================================================================
render_col_header
        ld b,HEADER_ROW
        ld a,ATTR_HEADER
        call clear_row

        ld de,strColHdr
        ld hl,1*256+HEADER_ROW
        ld a,ATTR_HEADER
        call call_print
        call render_p3dos_flag
        ret


; ================================================================
; render_p3dos_flag: show "+3DOS:YES" or "+3DOS:NO " in header row
; ================================================================
render_p3dos_flag
        ld de,str3dosLabel
        ld hl,55*256+HEADER_ROW
        ld a,ATTR_HEADER
        call call_print
        ld a,(p3dosEnabled)
        or a
        jr z,.no
        ld de,strYes
        jr .pr
.no     ld de,strNo
.pr     ld hl,61*256+HEADER_ROW
        ld a,ATTR_HEADER
        call call_print
        ret


; ================================================================
; render_help
; ================================================================
render_help
        ld de,strHelp
        ld hl,2*256+HELP_ROW
        ld a,ATTR_NORMAL
        call call_print
        ret


; ================================================================
; clear_content_area: fill CONTENT_ROWS rows with spaces
; ================================================================
clear_content_area
        ld a,CONTENT_ROW
        ld e,a
        ld d,160
        mul d,e
        ld hl,$4002
        add hl,de           ; HL = start of first content row, col 1

        ld b,CONTENT_ROWS
.row    push bc
        push hl
        ld b,76
.col    ld a,' '
        ld (hl),a
        inc hl
        ld a,ATTR_NORMAL
        ld (hl),a
        inc hl
        djnz .col
        pop hl
        ld de,160
        add hl,de
        pop bc
        djnz .row
        ret


; ================================================================
; clear_row: fill one screen row with spaces
; B = row number, A = attribute
; ================================================================
clear_row
        ld c,a              ; save attribute
        ld e,b
        ld d,160
        mul d,e             ; DE = row*160
        ld hl,$4002
        add hl,de
        ld b,76
.l      ld a,' '
        ld (hl),a
        inc hl
        ld a,c
        ld (hl),a
        inc hl
        djnz .l
        ret


; ================================================================
; Character output helpers (write to DE = screen pointer)
; ================================================================

; put one character (A) to (DE), advance DE by 2
put_char
        ld (de),a
        inc de
        ld a,(curAttr)
        ld (de),a
        inc de
        ret

; write null-terminated string HL to screen DE
write_str
        ld a,(hl)
        or a
        ret z
        ld (de),a
        inc de
        ld a,(curAttr)
        ld (de),a
        inc de
        inc hl
        jr write_str

; write B chars from HL to screen DE, space-pad if string ends early
write_padded
        ld a,(hl)
        or a
        jr z,.pad
        ld (de),a
        inc de
        ld a,(curAttr)
        ld (de),a
        inc de
        inc hl
        djnz write_padded
        ret
.pad    ld a,' '
        ld (de),a
        inc de
        ld a,(curAttr)
        ld (de),a
        inc de
        djnz .pad
        ret

; write entry number HL as 2-char right-justified decimal (1-99)
write_num2
        ld a,l
        cp 10
        jr nc,.two
        ld a,' '
        call put_char
        ld a,l
        add a,'0'
        jp put_char
.two    ld b,0
.div    cp 10
        jr c,.dd
        sub 10
        inc b
        jr .div
.dd     push af
        ld a,b
        add a,'0'
        call put_char
        pop af
        add a,'0'
        jp put_char

; write type string for type A (7 chars padded)
write_type7
        cp TYPE_BASIC
        ld hl,strBasic
        jr z,.p
        cp TYPE_CODE
        ld hl,strCode
        jr z,.p
        cp TYPE_NUMARRAY
        ld hl,strNArray
        jr z,.p
        cp TYPE_STRARRAY
        ld hl,strSArray
        jr z,.p
        ld hl,strUnknown
.p      ld b,7
        jp write_padded

; write HL as 5-digit right-justified decimal into screen DE
write_dec5
        push de
        call make_decimal
        pop de
        ld hl,numBuf
        ld b,5
        jp write_padded

; write "$XXXX" hex word for HL
write_hex_word
        ld a,'$'
        call put_char
        ld a,h
        call write_hex_byte
        ld a,l
        ; fall through to write_hex_byte

write_hex_byte
        push af
        rrca : rrca : rrca : rrca
        and $0F
        call write_hex_nib
        pop af
        and $0F
        ; fall through

write_hex_nib
        cp 10
        jr c,.d
        add a,'A'-10
        jp put_char
.d      add a,'0'
        jp put_char


; ================================================================
; make_decimal: HL → numBuf (5 chars right-justified, space-padded)
; Preserves DE.
; ================================================================
make_decimal
        push de
        ld de,numBuf
        ld b,5
.cl     ld a,' '
        ld (de),a
        inc de
        djnz .cl
        xor a
        ld (de),a           ; null terminate at numBuf+5
        ld de,numBuf
        ld bc,10000
        call .dig
        ld bc,1000
        call .dig
        ld bc,100
        call .dig
        ld bc,10
        call .dig
        ld a,l
        add a,'0'
        ld (de),a
        ; trim leading zeros to spaces
        ld hl,numBuf
        ld b,4
.tr     ld a,(hl)
        cp '0'
        jr nz,.trd
        ld (hl),' '
        inc hl
        djnz .tr
.trd    pop de
        ret
.dig    xor a
.dl     or a
        sbc hl,bc
        jr c,.dd
        inc a
        jr .dl
.dd     add hl,bc
        add a,'0'
        ld (de),a
        inc de
        ret


; ================================================================
; read_next_byte: read byte at readPtr, increment readPtr
; Returns A = byte read. Preserves DE.
; ================================================================
read_next_byte
        ld hl,(readPtr)
        call read_byte_at_offset
        ld hl,(readPtr)
        inc hl
        ld (readPtr),hl
        ret


; ================================================================
; read_byte_at_offset: read one byte from paged data
; HL = file offset (0x0000..0xFFFF maps to data pages 0..7)
; Returns A = byte. Clobbers HL (to physical address). Preserves DE.
; ================================================================
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


; ================================================================
; Service call infrastructure (self-patched by patch_services)
; ================================================================
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
        ret

; call_print: DE=string, HL=col*256+row, A=attr
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


; ================================================================
; Variables
; ================================================================
ctxPtr          defw 0
svcPtr          defw 0
dataPagesPtr    defw 0
loadedSize      defw 0
pageCount       defb 0
readPtr         defw 0
tmpMappedH      defb 0
curAttr         defb ATTR_NORMAL
totalBlocks     defb 0
topEntry        defb 0
curEntry        defb 0
selectedEntry   defb 0
blockLen        defw 0
tapFlag         defb 0
tapType         defb 0
tapDataLen      defw 0
tapParam1       defw 0
renderIdx       defb 0
screenPos       defw 0

numBuf          defs 7      ; 5 digits + null + 1 spare
nameBuf         defs 11     ; 10 chars + null
extractName     defs 16     ; output filename for 'e' extraction (255-terminated)
extractOff      defw 0      ; current write offset into loaded data
extractCnt      defw 0      ; remaining bytes to write
extractEntry    defb 0
payloadEntry    defb 0
extractNameUsed defb 0
extractNameHasDot defb 0
tmpBlockLen     defw 0
tmpBlockFlag    defb 0
tmpRangeStart   defw 0
extractHeaderType defb 0
tapParam2       defw 0
extractNamePtr  defw 0
extractStatus   defb 0
bulkIdx         defb 0
bulkPtr         defw 0
bulkNextPtr     defw 0
scanIndex       defb 0
lastHeader      defb 0
parentCandidate defb 0
nameSourceEntry defb 0
p3dosEnabled    defb 1

; ================================================================
; Strings
; ================================================================
strTap          defb "TAP: ",0
strBlk          defb "blk",0
; Column header aligned with data rows:
;  col: 1-2=## 3=sp 4-10=Type 11=sp 12-21=Name 22=sp 23-27=Size 28=b 29=sp 30+=Info
strColHdr       defb "## Type    Name        Size   Info",0
strHelp         defb " BREAK=exit  Up/Dn  PgUp/PgDn  e:export  CAPS+e:export all  d:+3DOS header",0
strLine         defb "LINE:",0
strBasic        defb "BASIC  ",0
strCode         defb "CODE   ",0
strNArray       defb "NARRAY ",0
strSArray       defb "SARRAY ",0
strData         defb "DATA   ",0
strUnknown      defb "???    ",0
strDashes       defb "----------",0
strBlkNameTemplate defb "BLCK00.BIN",0
strExportOK     defb "OK",0
strExportFail   defb "FAIL "
strExportFailCode defb "?",0
strExportName   defb "NAME:",0
strExportLen    defb "LEN:",0
strExportIdx    defb "IDX:",0
strExportAllOK  defb "ALL OK",0
strExportAllFail defb "ALL FAIL "
strExportAllFailCode defb "?",0
strDebugBlank   defb "                                ",0
str3dosLabel    defb "+3DOS:",0
strYes          defb "YES",0
strNo           defb "NO ",0

; ================================================================
; Entry offset table (2 bytes per entry, up to MAX_ENTRIES)
; ================================================================
entryOffsets    defs MAX_ENTRIES*2
entryParents    defs MAX_ENTRIES

plugin_end
        assert plugin_end - plugin_start <= VIEW_PLUGIN_SIZE
        SAVEBIN "plugin/tap.ccp", VIEW_PLUGIN_ADDRESS, VIEW_PLUGIN_SIZE
