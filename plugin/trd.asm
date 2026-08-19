        DEVICE ZXSPECTRUMNEXT
        org VIEW_PLUGIN_ADDRESS

        include "plugin_api.i.asm"

; ---- screen layout ----
; Tilemap: 160 bytes/row, 2 bytes/tile (char + attr)
; Window: col=0 row=3 width=78 height=24
;   inner rows 4-25 (borders at 3 and 26)
;   row 4  : title  (TRD: filename            80T/2S  NNN fl)
;   row 5  : column header + disk label, free space, +3DOS flag
;   rows 6-23: entries (CONTENT_ROWS=18)
;   row 25 : help
; Extraction status is printed from column 42 downwards, so the entry
; rows must stay inside the first 40 columns.

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
ATTR_SELECTED   equ 32      ; palette group 32 = the cursor bar the panels use
ATTR_DATA       equ 16
ATTR_BUSY       equ 224     ; window background, yellow ink: no colour block

; ---- TR-DOS image layout ----
; Catalogue: 128 entries of 16 bytes in track 0, sectors 0-7 ($0000-$07FF)
;   +0  8 bytes  name (byte 0: $00 = end of catalogue, $01 = deleted file)
;   +8  1 byte   type letter: B=BASIC, C=CODE, D=data array, #=print stream
;   +9  2 bytes  CODE: load address / BASIC: program length without variables
;   +11 2 bytes  length in bytes
;   +13 1 byte   length in sectors
;   +14 1 byte   first sector (0-15)
;   +15 1 byte   first track (logical: physical track * sides + side)
; TR-DOS never fragments a file, so the image offset of its data is simply
;   (track * 16 + sector) * 256
; which is why exporting only needs a seek plus one linear copy.
; Sector 8 of track 0 holds the disk information block.

MAX_ENTRIES     equ 128
INFO_TYPE       equ $08E3
INFO_FREE       equ $08E5
INFO_ID         equ $08E7
INFO_LABEL      equ $08F5
TRDOS_ID        equ $10
MIN_SECTORS     equ 10          ; catalogue + info sector must be readable


; ================================================================
; Plugin entry point
; HL = context pointer, DE = services pointer
; ================================================================
plugin_start
        ld (ctxPtr),hl
        ld (svcPtr),de
        call patch_services
        call init_context
        call scan_catalog
        call read_disk_info

        ld hl,WIN_COL*256+WIN_ROW
        ld bc,WIN_WIDTH*256+WIN_HEIGHT
        ld a,ATTR_NORMAL
        call call_window

        call render_title
        call render_col_header
        call render_help
        call render_page
        call render_selection_info

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
        ld a,(totalFiles)
        or a
        jp z,.input
        ld b,a
        ld a,(curEntry)
        inc a
        cp b
        jp nc,.input        ; already on the last entry
        ld (curEntry),a
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
        jp z,.input
        dec a
        ld (curEntry),a
        ; scroll if new curEntry < topEntry
        ld b,a
        ld a,(topEntry)
        cp b
        jp c,.do_render     ; topEntry < curEntry: no scroll
        or a
        jp z,.do_render     ; topEntry = 0: cannot scroll up
        ld hl,topEntry
        dec (hl)
        jp .do_render

; ---- page down ----
.pgdn
        ld a,(totalFiles)
        or a
        jp z,.input
        dec a
        ld b,a              ; B = last valid index
        ld a,(curEntry)
        cp b
        jp z,.input
        add a,CONTENT_ROWS
        cp b
        jp c,.pgdn_set
        ld a,b
.pgdn_set
        ld (curEntry),a
        sub CONTENT_ROWS-1
        jr nc,.pgdn_top
        xor a
.pgdn_top
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
        ld (topEntry),a

.do_render
        call render_page
        call render_selection_info
        ; wait for key release before accepting next input
.wait_release
        call call_input
        or a
        jr nz,.wait_release
        jp .input

; ---- extract selected entry ('e') ----
.extract
        call show_busy_one
        call do_extract_current
        call restore_help
        jp .wait_release

; ---- extract every live entry ('CAPS+e') ----
.extract_all
        call show_busy_all
        call do_extract_all
        call restore_help
        jp .wait_release

; ---- toggle +3DOS header ('d') ----
.toggle_p3dos
        ld a,(p3dosEnabled)
        xor 1
        ld (p3dosEnabled),a
        call render_p3dos_flag
        jp .wait_release


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

        ; totalSectors = filesize / 256, needed to reject entries pointing
        ; outside the image. Sizes above 16MB are clamped; no TRD is that big.
        ld l,(ix+VIEWCTX_SIZE_LO+1)     ; bits 8-15 -> low byte
        ld h,(ix+VIEWCTX_SIZE_HI)       ; bits 16-23 -> high byte
        ld a,(ix+VIEWCTX_SIZE_HI+1)
        or a
        jr z,.store
        ld hl,$ffff
.store  ld (totalSectors),hl

        xor a
        ld (curEntry),a
        ld (topEntry),a

        ; an image without a full catalogue cannot be listed
        ld a,(pageCount)
        or a
        jr z,.invalid
        ld hl,(totalSectors)
        ld de,MIN_SECTORS
        or a
        sbc hl,de
        jr c,.invalid
        ld a,1
        ld (trdValid),a
        ret
.invalid
        xor a
        ld (trdValid),a
        ret


; ================================================================
; scan_catalog: count catalogue entries up to the end marker.
; Deleted entries keep their slot, so they stay visible and can still
; be exported one by one.
; ================================================================
scan_catalog
        xor a
        ld (totalFiles),a
        ld a,(trdValid)
        or a
        ret z
.loop
        ld a,(totalFiles)
        cp MAX_ENTRIES
        ret nc
        ld c,0
        call entry_byte
        or a
        ret z               ; name[0] = 0: end of catalogue
        ld hl,totalFiles
        inc (hl)
        jr .loop


; ================================================================
; read_disk_info: disk type, free sectors, TR-DOS id and label
; ================================================================
read_disk_info
        ld hl,labelBuf
        ld (hl),0
        ld a,(trdValid)
        or a
        ret z
        ld hl,INFO_TYPE
        call read_byte_at_offset
        ld (diskType),a
        ld hl,INFO_ID
        call read_byte_at_offset
        ld (diskId),a
        ld hl,INFO_FREE
        call read_byte_at_offset
        ld e,a
        ld hl,INFO_FREE+1
        call read_byte_at_offset
        ld d,a
        ld (diskFree),de
        ld hl,INFO_LABEL
        ld de,labelBuf
        ld b,8
        jp read_text


; ================================================================
; entry_offset: A = entry index -> HL = catalogue offset (index * 16)
; ================================================================
entry_offset
        ld l,a
        ld h,0
        add hl,hl
        add hl,hl
        add hl,hl
        add hl,hl
        ret


; entry_byte: A = entry index, C = offset inside entry -> A = byte
entry_byte
        call entry_offset
        ld b,0
        add hl,bc
        jp read_byte_at_offset


; entry_word: A = entry index, C = offset inside entry -> HL = word (LE)
entry_word
        call entry_offset
        ld b,0
        add hl,bc
        push hl
        call read_byte_at_offset
        ld e,a
        pop hl
        inc hl
        call read_byte_at_offset
        ld d,a
        ex de,hl
        ret


; ================================================================
; read_entry_fields: A = entry index. Loads every field of the entry
; into the entry* variables and validates its sector range.
; ================================================================
read_entry_fields
        ld (fieldIdx),a
        ld c,0
        call entry_byte
        ld (entryFirstChar),a
        ld a,(fieldIdx)
        ld c,8
        call entry_byte
        ld (entryType),a
        ld a,(fieldIdx)
        ld c,9
        call entry_word
        ld (entryParam),hl
        ld a,(fieldIdx)
        ld c,11
        call entry_word
        ld (entryLen),hl
        ld a,(fieldIdx)
        ld c,13
        call entry_byte
        ld (entrySectors),a
        ld a,(fieldIdx)
        ld c,14
        call entry_byte
        ld (entrySector),a
        ld a,(fieldIdx)
        ld c,15
        call entry_byte
        ld (entryTrack),a

        ; first sector of the file inside the image
        ld a,(entryTrack)
        ld l,a
        ld h,0
        add hl,hl
        add hl,hl
        add hl,hl
        add hl,hl           ; track * 16
        ld a,(entrySector)
        ld e,a
        ld d,0
        add hl,de
        ld (entryStartSec),hl

        ; byte length: trust the stored value only while it fits inside the
        ; sectors the catalogue reserved for the file
        ld a,(entrySectors)
        ld h,a
        ld l,0
        ld (entryMaxLen),hl         ; sectors * 256
        ld de,(entryLen)
        ld a,d
        or e
        jr z,.use_max
        ld hl,(entryMaxLen)
        ex de,hl                    ; HL = stored length, DE = reserved
        or a
        sbc hl,de
        jr z,.range
        jr c,.range
.use_max
        ld hl,(entryMaxLen)
        ld (entryLen),hl
.range
        call check_entry_range
        ld a,0
        jr nc,.range_ok
        inc a
.range_ok
        ld (entryBad),a
        ret


; ================================================================
; check_entry_range: carry set when the entry holds no data or reaches
; past the end of the image. Uses the fields loaded above.
; ================================================================
check_entry_range
        ld a,(entrySectors)
        or a
        scf
        ret z
        ld l,a
        ld h,0
        ld de,(entryStartSec)
        add hl,de           ; HL = first sector after the file
        ex de,hl
        ld hl,(totalSectors)
        or a
        sbc hl,de
        ret c               ; file ends past the image: carry set
        or a
        ret


; ================================================================
; read_text: HL = file offset, DE = destination, B = count.
; Copies printable characters, terminates with NUL.
; ================================================================
read_text
        ld (namePtr),hl
.loop
        ld hl,(namePtr)
        call read_byte_at_offset
        cp ' '
        jr c,.bad
        cp $7F
        jr c,.ok
.bad    ld a,'?'
.ok     ld (de),a
        inc de
        ld hl,(namePtr)
        inc hl
        ld (namePtr),hl
        djnz .loop
        xor a
        ld (de),a
        ret


; A = entry index -> nameBuf = 8 printable chars + NUL
read_entry_name
        call entry_offset
        ld de,nameBuf
        ld b,8
        jp read_text


; ================================================================
; render_page: clear content area, render visible entries
; ================================================================
render_page
        call clear_content_area
        ld a,(trdValid)
        or a
        jr z,.not_trd
        ld a,(totalFiles)
        or a
        jr z,.empty

        ld a,(topEntry)     ; A = entry index to render
        ld d,CONTENT_ROW    ; D = screen row
.loop
        ld b,a
        ld a,(totalFiles)
        cp b
        ld a,b
        jr z,.done
        jr c,.done

        ld b,a
        ld a,d
        cp CONTENT_ROW+CONTENT_ROWS
        ld a,b
        jr nc,.done

        push af
        push de
        call render_entry
        pop de
        pop af
        inc a
        inc d
        jr .loop
.done
        ret
.empty
        ld de,strEmpty
        ld hl,2*256+CONTENT_ROW
        ld a,ATTR_NORMAL
        jp call_print
.not_trd
        ld de,strNotTrd
        ld hl,2*256+CONTENT_ROW
        ld a,ATTR_NORMAL
        jp call_print


; ================================================================
; render_entry: draw one catalogue row.
; A = entry index (0-based), D = screen row
;  col 0-2 index, 4-11 name, 13-18 type, 20-25 size, 27-32 track/sector,
;  34-38 load address or BASIC program length
; ================================================================
render_entry
        ld (renderIdx),a
        ld a,d
        ld (renderRow),a
        ld a,(renderIdx)
        call read_entry_fields
        ld a,(renderIdx)
        call read_entry_name

        ; screen address = $4000 + row*160 + 2 (col 1, inside the border)
        ld a,(renderRow)
        ld e,a
        ld d,160
        mul d,e
        ld hl,$4002
        add hl,de
        ld (screenPos),hl

        ; attribute: selection wins, then deleted, then file type
        ld a,(renderIdx)
        ld b,a
        ld a,(curEntry)
        cp b
        jr z,.sel
        ld a,(entryFirstChar)
        cp 1
        jr z,.adata
        ld a,(entryType)
        cp 'B'
        jr z,.abasic
        cp 'C'
        jr z,.acode
        ld a,ATTR_NORMAL
        jr .aset
.sel    ld a,ATTR_SELECTED
        jr .aset
.abasic ld a,ATTR_BASIC
        jr .aset
.acode  ld a,ATTR_CODE
        jr .aset
.adata  ld a,ATTR_DATA
.aset   ld (curAttr),a

        ld de,(screenPos)

        ; entry number (1-based, 3 chars)
        ld a,(renderIdx)
        inc a
        ld l,a
        ld h,0
        call write_dec3
        ld a,' '
        call put_char

        ; name (8 chars)
        ld hl,nameBuf
        ld b,8
        call write_padded
        ld a,' '
        call put_char

        ; type (6 chars)
        call write_type6
        ld a,' '
        call put_char

        ; length in bytes (5 digits + 'b')
        ld hl,(entryLen)
        call write_dec5
        ld a,'b'
        call put_char
        ld a,' '
        call put_char

        ; position on the disk: track/sector
        ld a,(entryTrack)
        ld l,a
        ld h,0
        call write_dec3
        ld a,'/'
        call put_char
        ld a,(entrySector)
        ld l,a
        ld h,0
        call write_dec2
        ld a,' '
        call put_char

        ; info column
        ld a,(entryBad)
        or a
        jr nz,.info_bad
        ld a,(entryFirstChar)
        cp 1
        jr z,.info_del
        ld a,(entryType)
        cp 'B'
        jr z,.info_basic
        ld hl,(entryParam)
        jp write_hex_word
.info_basic
        ld hl,(entryParam)
        jp write_dec5
.info_bad
        ld hl,strBad
        ld b,5
        jp write_padded
.info_del
        ld hl,strDel
        ld b,5
        jp write_padded


; write the type column (6 chars) for the type letter in entryType
write_type6
        ld a,(entryType)
        cp 'B'
        ld hl,strTBasic
        jr z,.p
        cp 'C'
        ld hl,strTCode
        jr z,.p
        cp 'D'
        ld hl,strTData
        jr z,.p
        cp '#'
        ld hl,strTPrint
        jr z,.p
        ; unknown type: show the raw letter
        ld hl,strTOther
        ld (hl),a
        cp ' '
        jr c,.other_bad
        cp $7F
        jr c,.p
.other_bad
        ld (hl),'?'
.p      ld b,6
        jp write_padded


; ================================================================
; render_title
; ================================================================
render_title
        ld b,TITLE_ROW
        ld a,ATTR_TITLE
        call clear_row

        ld de,strTrd
        ld hl,1*256+TITLE_ROW
        ld a,ATTR_TITLE
        call call_print

        ld ix,(ctxPtr)
        ld l,(ix+VIEWCTX_FILENAME)
        ld h,(ix+VIEWCTX_FILENAME+1)
        ex de,hl
        ld hl,6*256+TITLE_ROW
        ld a,ATTR_TITLE
        call call_print

        ; disk geometry
        call disk_type_text
        ld hl,64*256+TITLE_ROW
        ld a,ATTR_TITLE
        call call_print

        ; number of catalogue entries
        ld a,(totalFiles)
        ld l,a
        ld h,0
        call make_decimal
        ld de,numBuf+2
        ld hl,70*256+TITLE_ROW
        ld a,ATTR_TITLE
        call call_print
        ld de,strFl
        ld hl,74*256+TITLE_ROW
        ld a,ATTR_TITLE
        jp call_print


; DE = text for the disk type byte
disk_type_text
        ld a,(diskId)
        cp TRDOS_ID
        ld de,strGeoBad
        ret nz
        ld a,(diskType)
        ld de,strGeo802
        cp $16
        ret z
        ld de,strGeo402
        cp $17
        ret z
        ld de,strGeo801
        cp $18
        ret z
        ld de,strGeo401
        cp $19
        ret z
        ld de,strGeoBad
        ret


; ================================================================
; render_col_header: column titles plus disk label and free space
; ================================================================
render_col_header
        ld b,HEADER_ROW
        ld a,ATTR_HEADER
        call clear_row

        ld de,strColHdr
        ld hl,1*256+HEADER_ROW
        ld a,ATTR_HEADER
        call call_print

        ld de,labelBuf
        ld hl,41*256+HEADER_ROW
        ld a,ATTR_HEADER
        call call_print

        ld hl,(diskFree)
        call make_decimal
        ld de,numBuf+1
        ld hl,51*256+HEADER_ROW
        ld a,ATTR_HEADER
        call call_print
        ld de,strFree
        ld hl,55*256+HEADER_ROW
        ld a,ATTR_HEADER
        call call_print

        jp render_p3dos_flag


; ================================================================
; render_p3dos_flag: show "+3DOS:YES" or "+3DOS:NO " in the header row
; ================================================================
render_p3dos_flag
        ld de,str3dosLabel
        ld hl,62*256+HEADER_ROW
        ld a,ATTR_HEADER
        call call_print
        ld a,(p3dosEnabled)
        or a
        jr z,.no
        ld de,strYes
        jr .pr
.no     ld de,strNo
.pr     ld hl,68*256+HEADER_ROW
        ld a,ATTR_HEADER
        jp call_print


; ================================================================
; render_help
; ================================================================
render_help
        ld de,strHelp
        ld hl,2*256+HELP_ROW
        ld a,ATTR_NORMAL
        jp call_print


; ================================================================
; Busy banner. Every exported file means reopening the image, seeking,
; creating the output and closing both, which takes long enough that
; the plugin has to say it is working before the first name appears.
; ================================================================
show_busy_all
        ld de,strBusyAll
        jr show_busy
show_busy_one
        ld de,strBusyOne
show_busy
        push de
        ld b,HELP_ROW
        ld a,ATTR_BUSY
        call clear_row
        pop de
        ld hl,2*256+HELP_ROW
        ld a,ATTR_BUSY
        jp call_print

restore_help
        ld b,HELP_ROW
        ld a,ATTR_NORMAL
        call clear_row
        jp render_help


; ================================================================
; show_progress: "FILE: n/m" for the entry being written
; ================================================================
show_progress
        ld a,(extractIdx)
        inc a
        ld l,a
        ld h,0
        call make_decimal
        ld hl,numBuf+2
        ld de,progressBuf
        ld bc,3
        ldir
        ld a,'/'
        ld (de),a
        inc de
        ld a,(totalFiles)
        ld l,a
        ld h,0
        call make_decimal       ; preserves DE
        ld hl,numBuf+2
        ld bc,3
        ldir
        xor a
        ld (de),a
        ld de,strFileLabel
        ld hl,42*256+(CONTENT_ROW+3)
        ld a,ATTR_CODE
        call call_print
        ld de,progressBuf
        ld hl,48*256+(CONTENT_ROW+3)
        ld a,ATTR_CODE
        jp call_print


; ================================================================
; render_selection_info: output name and size for the current entry
; ================================================================
render_selection_info
        call clear_debug_area
        ld a,(totalFiles)
        or a
        ret z
        ld a,(curEntry)
        call read_entry_fields
        ld a,(curEntry)
        call build_extract_name
        jp show_export_target


; prints "NAME: xxx" and "LEN: nnnnn" for the prepared extraction
show_export_target
        ld de,strExportName
        ld hl,42*256+CONTENT_ROW
        ld a,ATTR_CODE
        call call_print
        ld de,extractName
        ld hl,48*256+CONTENT_ROW
        ld a,ATTR_CODE
        call call_print
        ld hl,(entryLen)
        call make_decimal
        ld de,strExportLen
        ld hl,42*256+(CONTENT_ROW+1)
        ld a,ATTR_CODE
        call call_print
        ld de,numBuf
        ld hl,48*256+(CONTENT_ROW+1)
        ld a,ATTR_CODE
        jp call_print


; ================================================================
; do_extract_current: export the entry under the cursor
; ================================================================
do_extract_current
        ld a,(totalFiles)
        or a
        ret z
        ld a,(curEntry)
        jp do_extract_entry


; ================================================================
; do_extract_entry: A = entry index.
; Writes the file through the seeking extract service, so data anywhere
; in the image is reachable, not just the first 64KB the viewer loaded.
; extractStatus is 0 on success.
; ================================================================
do_extract_entry
        ld (extractIdx),a
        xor a
        ld (extractStatus),a
        ld a,(extractIdx)
        call read_entry_fields
        ld a,(entryBad)
        or a
        jr nz,.bad
        ld a,(extractIdx)
        call build_extract_name
        call clear_debug_area
        call show_export_target
        call show_progress
        ld de,strWorking
        ld hl,42*256+(CONTENT_ROW+2)
        ld a,ATTR_CODE
        call call_print

        ; 24-bit image offset = first sector * 256
        ld hl,(entryStartSec)
        ld a,h
        ld h,l
        ld l,0
        ld (extractOffLo),hl
        ld l,a
        ld h,0
        ld (extractOffHi),hl

        call setup_p3dos_context
        ld ix,(ctxPtr)
        ld hl,(extractOffHi)
        ld (ix+VIEWCTX_EXTRACT_OFHI),l
        ld (ix+VIEWCTX_EXTRACT_OFHI+1),h
        ld hl,extractName
        ld de,(extractOffLo)
        ld bc,(entryLen)
        call call_extract_seek
        jr c,.fail
        xor a
        ld (extractStatus),a
        ld de,strExportOK
        jr .report
.fail
        ld (extractStatus),a
        add a,'0'
        ld (strExportFailCode),a
        ld de,strExportFail
        jr .report
.bad
        ld a,255
        ld (extractStatus),a
        call clear_debug_area
        ld de,strExportBad
.report
        ld hl,42*256+(CONTENT_ROW+2)
        ld a,ATTR_CODE
        jp call_print


; ================================================================
; do_extract_all: export every live entry. Deleted entries and entries
; pointing outside the image are skipped; the first failure stops.
; ================================================================
do_extract_all
        xor a
        ld (bulkIdx),a
.loop
        ld a,(bulkIdx)
        ld b,a
        ld a,(totalFiles)
        cp b
        jr z,.all_ok
        jr c,.all_ok

        ld a,(bulkIdx)
        call read_entry_fields
        ld a,(entryFirstChar)
        cp 1
        jr z,.skip
        ld a,(entryBad)
        or a
        jr nz,.skip

        ld a,(bulkIdx)
        call do_extract_entry
        ld a,(extractStatus)
        or a
        jr nz,.all_fail
.skip
        ld hl,bulkIdx
        inc (hl)
        jr .loop

.all_ok
        call clear_debug_area
        ld de,strExportAllOK
        ld hl,42*256+CONTENT_ROW
        ld a,ATTR_CODE
        jp call_print

.all_fail
        add a,'0'
        ld (strExportAllFailCode),a
        call clear_debug_area
        ld de,strExportAllFail
        ld hl,42*256+CONTENT_ROW
        ld a,ATTR_CODE
        jp call_print


; ================================================================
; setup_p3dos_context: map the TR-DOS type onto the +3DOS header the
; host writes. Unknown types are exported raw.
;   B -> BASIC, param2 = program length without variables
;   C -> CODE,  param1 = load address
;   D -> numeric array
; ================================================================
setup_p3dos_context
        ld ix,(ctxPtr)
        ld a,(p3dosEnabled)
        or a
        jr z,.raw
        ld a,(entryType)
        cp 'B'
        jr z,.basic
        cp 'C'
        jr z,.code
        cp 'D'
        jr z,.data
.raw
        ld a,$FF
        ld (ix+VIEWCTX_P3DOS_TYPE),a
        ret
.basic
        xor a
        ld (ix+VIEWCTX_P3DOS_TYPE),a
        ld hl,32768                 ; no autostart line
        call store_p1
        ld hl,(entryParam)
        jp store_p2
.code
        ld a,3
        ld (ix+VIEWCTX_P3DOS_TYPE),a
        ld hl,(entryParam)
        call store_p1
        ld hl,32768
        jp store_p2
.data
        ld a,1
        ld (ix+VIEWCTX_P3DOS_TYPE),a
        ld hl,(entryParam)
        call store_p1
        ld hl,32768
        jp store_p2

store_p1
        ld (ix+VIEWCTX_P3DOS_P1),l
        ld (ix+VIEWCTX_P3DOS_P1+1),h
        ret
store_p2
        ld (ix+VIEWCTX_P3DOS_P2),l
        ld (ix+VIEWCTX_P3DOS_P2+1),h
        ret


; ================================================================
; build_extract_name: A = entry index -> extractName
; TR-DOS names may hold any byte, so characters are sanitised and
; spaces dropped. The extension follows the type letter.
; ================================================================
build_extract_name
        ld (nameIdx),a
        call clear_extract_name
        ld a,(nameIdx)
        call entry_offset
        ld (namePtr),hl
        ld hl,extractName
        ld (extractNamePtr),hl
        xor a
        ld (extractNameUsed),a
        ld b,8
.loop
        push bc
        ld hl,(namePtr)
        call read_byte_at_offset
        ld c,a
        ld hl,(namePtr)
        inc hl
        ld (namePtr),hl
        ld a,c
        cp ' '
        jr z,.skip
        call sanitize_filename_char
        ld hl,(extractNamePtr)
        ld (hl),a
        inc hl
        ld (extractNamePtr),hl
        ld a,1
        ld (extractNameUsed),a
.skip
        pop bc
        djnz .loop

        ld a,(extractNameUsed)
        or a
        jr z,build_num_name
        ld hl,(extractNamePtr)
        ld a,(entryType)
        cp 'B'
        jr nz,.ext_bin
        ld a,'.' : ld (hl),a : inc hl
        ld a,'B' : ld (hl),a : inc hl
        ld a,'A' : ld (hl),a : inc hl
        ld a,'S' : ld (hl),a : inc hl
        jr .term
.ext_bin
        ld a,'.' : ld (hl),a : inc hl
        ld a,'B' : ld (hl),a : inc hl
        ld a,'I' : ld (hl),a : inc hl
        ld a,'N' : ld (hl),a : inc hl
.term
        ld (hl),0
        ret


; name made only of spaces or unusable bytes: fall back to FILEnnn
build_num_name
        ld hl,strNumName
        ld de,extractName
        ld bc,12
        ldir
        ld a,(nameIdx)
        inc a
        ld l,a
        ld h,0
        call make_decimal
        ld hl,numBuf+2
        ld de,extractName+4
        ld b,3
.digit  ld a,(hl)
        cp ' '
        jr nz,.store
        ld a,'0'
.store  ld (de),a
        inc hl
        inc de
        djnz .digit
        ld a,(entryType)
        cp 'B'
        ret nz
        ld hl,extractName+8
        ld (hl),'B' : inc hl
        ld (hl),'A' : inc hl
        ld (hl),'S'
        ret


clear_extract_name
        ld hl,extractName
        ld b,16
.loop
        ld (hl),0
        inc hl
        djnz .loop
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
        cp '_'
        ret z
        cp '-'
        ret z
        ld a,'_'
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
; clear_content_area: fill CONTENT_ROWS rows with spaces
; ================================================================
clear_content_area
        ld a,CONTENT_ROW
        ld e,a
        ld d,160
        mul d,e
        ld hl,$4002
        add hl,de

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
; clear_row: fill one screen row with spaces. B = row, A = attribute
; ================================================================
clear_row
        ld c,a
        ld e,b
        ld d,160
        mul d,e
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

; put one character (A) at (DE), advance DE by 2
put_char
        ld (de),a
        inc de
        ld a,(curAttr)
        ld (de),a
        inc de
        ret

; write B chars from HL at DE, space-padded when the string ends early
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

; write HL as right-justified decimal: 5, 3 or 2 characters
write_dec5
        push de
        call make_decimal
        pop de
        ld hl,numBuf
        ld b,5
        jp write_padded

write_dec3
        push de
        call make_decimal
        pop de
        ld hl,numBuf+2
        ld b,3
        jp write_padded

write_dec2
        push de
        call make_decimal
        pop de
        ld hl,numBuf+3
        ld b,2
        jp write_padded

; write "$XXXX" for HL
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
; make_decimal: HL -> numBuf (5 chars right-justified, space-padded)
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
        ld (de),a
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
; read_byte_at_offset: read one byte of the loaded file data.
; HL = file offset ($0000-$FFFF, i.e. data pages 0-7) -> A = byte.
; Clobbers HL, preserves BC and DE.
; ================================================================
read_byte_at_offset
        push de
        ld a,h
        rlca
        rlca
        rlca
        and 7               ; A = offset / 8192
        ld e,a
        ld d,0
        push hl
        ld hl,(dataPagesPtr)
        add hl,de
        ld a,(hl)
        nextreg $57,a
        pop hl
        ld a,h
        and $1F
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
        ld l,(ix+SERVICE_EXTRACT_SEEK)
        ld h,(ix+SERVICE_EXTRACT_SEEK+1)
        ld (call_extract_seek+1),hl
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

; call_extract_seek: HL=name, DE=offset bits 0-15, BC=count
call_extract_seek
        call 0
        ret


; ================================================================
; Variables
; ================================================================
ctxPtr          defw 0
svcPtr          defw 0
dataPagesPtr    defw 0
pageCount       defb 0
totalSectors    defw 0
trdValid        defb 0
diskType        defb 0
diskId          defb 0
diskFree        defw 0
totalFiles      defb 0
topEntry        defb 0
curEntry        defb 0
curAttr         defb ATTR_NORMAL
renderIdx       defb 0
renderRow       defb 0
screenPos       defw 0

fieldIdx        defb 0
entryFirstChar  defb 0
entryType       defb 0
entryParam      defw 0
entryLen        defw 0
entryMaxLen     defw 0
entrySectors    defb 0
entrySector     defb 0
entryTrack      defb 0
entryStartSec   defw 0
entryBad        defb 0

numBuf          defs 7      ; 5 digits + null + 1 spare
progressBuf     defs 8      ; "nnn/nnn" + null
nameBuf         defs 9      ; 8 chars + null
labelBuf        defs 9      ; disk label, 8 chars + null
namePtr         defw 0
nameIdx         defb 0
extractName     defs 16     ; output filename, NUL terminated
extractNamePtr  defw 0
extractNameUsed defb 0
extractOffLo    defw 0
extractOffHi    defw 0
extractIdx      defb 0
extractStatus   defb 0
bulkIdx         defb 0
p3dosEnabled    defb 1

; ================================================================
; Strings
; ================================================================
strTrd          defb "TRD: ",0
strFl           defb "fl",0
strGeo802       defb "80T/2S",0
strGeo402       defb "40T/2S",0
strGeo801       defb "80T/1S",0
strGeo401       defb "40T/1S",0
strGeoBad       defb "??????",0
; Column header aligned with the data rows:
;  0-2=## 4-11=Name 13-18=Type 20-25=Size 27-32=Trk/Sc 34-38=Start
strColHdr       defb " ## Name     Type   Size   Trk/Sc Start",0
strFree         defb " free",0
strHelp         defb " BREAK=exit  Up/Dn  PgUp/PgDn  e:export  CAPS+e:export all  d:+3DOS header",0
strTBasic       defb "BASIC ",0
strTCode        defb "CODE  ",0
strTData        defb "DATA  ",0
strTPrint       defb "PRINT ",0
strTOther       defb "?     ",0
strDel          defb "DEL  ",0
strBad          defb "BAD  ",0
strEmpty        defb "Catalogue is empty.",0
strNotTrd       defb "Not a readable TR-DOS image.",0
strNumName      defb "FILE000.BIN",0
; result strings are padded so they cover the "WORKING..." they replace
strExportOK     defb "OK        ",0
strExportFail   defb "FAIL "
strExportFailCode defb "?    ",0
strWorking      defb "WORKING...",0
strBusyAll      defb "EXPORTING ALL FILES - PLEASE WAIT...",0
strBusyOne      defb "EXPORTING - PLEASE WAIT...",0
strFileLabel    defb "FILE:",0
strExportBad    defb "SKIPPED (bad range)",0
strExportName   defb "NAME:",0
strExportLen    defb "LEN:",0
strExportAllOK  defb "ALL OK",0
strExportAllFail defb "ALL FAIL "
strExportAllFailCode defb "?",0
strDebugBlank   defb "                                ",0
str3dosLabel    defb "+3DOS:",0
strYes          defb "YES",0
strNo           defb "NO ",0

plugin_end
        assert plugin_end - plugin_start <= VIEW_PLUGIN_SIZE
        SAVEBIN "plugin/trd.ccp", VIEW_PLUGIN_ADDRESS, VIEW_PLUGIN_SIZE
