        DEVICE ZXSPECTRUMNEXT
        org VIEW_PLUGIN_ADDRESS

        include "plugin_api.i.asm"

; ---- screen layout ----
; Tilemap: 160 bytes/row, 2 bytes/tile (char + attr)
; Window: col=0 row=3 width=78 height=24
;   inner rows 4-25 (borders at 3 and 26)
;   row 4  : title  (ZIP: filename                       NNN files)
;   row 5  : column header, plus a warning when the listing is cut short
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
ATTR_STORED     equ 160     ; can be written out exactly as it stands
ATTR_PACKED     equ 192     ; needs an inflate the plugin does not have yet
ATTR_SELECTED   equ 32      ; palette group 32 = the cursor bar the panels use
ATTR_DIR        equ 16
ATTR_BUSY       equ 224     ; window background, yellow ink: no colour block

; ---- ZIP layout ----
; A ZIP is read back to front: the end of central directory record (EOCD)
; sits at the very end and points at the central directory, which is the
; only place that lists every member of the archive.
;
; EOCD, signature PK 05 06:
;   +4  2  this disk            +6  2  disk holding the directory
;   +8  2  entries on this disk +10 2  entries in total
;   +12 4  directory size       +16 4  directory offset
;   +20 2  comment length, then the comment itself
;
; Central directory entry, signature PK 01 02, 46 bytes plus three
; variable length fields:
;   +8  2  flags (bit 0 = encrypted)
;   +10 2  method (0 = stored, 8 = deflate)
;   +16 4  crc32
;   +20 4  compressed size      +24 4  uncompressed size
;   +28 2  name length          +30 2  extra length   +32 2 comment length
;   +42 4  offset of the local header
;   +46    name, extra field, comment
;
; Local file header, signature PK 03 04, 30 bytes plus name and extra:
;   +26 2  name length          +28 2  extra length
; The lengths in the local header are its own and need not match the ones
; in the directory, so the data offset of a member can only be worked out
; by reading that header:
;   data = local header offset + 30 + its name length + its extra length

MAX_ENTRIES     equ 128
CD_MAX_PAGES    equ 7           ; the directory copy lives in data pages 0-6
CD_MAX_BYTES    equ CD_MAX_PAGES*8192
SCRATCH_PAGE_IX equ 7           ; last data page: EOCD scan and local headers
IN_PAGE_IX      equ 4           ; staging page for compressed input; the
                                ; ring only reaches pages 0-3
EOCD_SCAN       equ 8192        ; how much of the tail is searched for the EOCD
EOCD_MIN        equ 22          ; an EOCD without a comment
LOCAL_HDR       equ 30
DISP_NAME       equ 15          ; width of the name column

; ---- inflate working area ----
; Output goes into a 32K ring in data pages 0-3. That is exactly the
; largest distance deflate can reference, so the ring doubles as the
; LZ77 window and needs no separate copy. Whole 8K pages are handed to
; the output file as they fill, which is what lets a member be any size
; at all instead of being capped by what fits in RAM. Page 7 stays
; scratch for local headers.
WIN_PAGES       equ 4
WIN_SIZE        equ WIN_PAGES*8192
WIN_MASK_HI     equ (WIN_SIZE-1)>>8     ; masks a ring offset held in H
FLUSH_SIZE      equ 8192        ; one page, handed over at a time
IN_BUF_SIZE     equ 512         ; compressed data is pulled in in chunks
MAXBITS         equ 15
NSYMS           equ 288         ; literal/length alphabet
NDIST           equ 30
NLENGTHS        equ NSYMS+32    ; code lengths for both tables while building

; entryKind values
KIND_STORED     equ 0           ; ready to be written out as it stands
KIND_DIR        equ 1
KIND_CRYPT      equ 2
KIND_PACKED     equ 3           ; deflate
KIND_METHOD     equ 4           ; a compression method we do not implement

; zipError values
ZERR_NONE       equ 0
ZERR_NOEOCD     equ 1
ZERR_ZIP64      equ 2
ZERR_READ       equ 3

; inflErr values, reported as the export failure code
IERR_NONE       equ 0
IERR_EOF        equ 1           ; stream ended in the middle of a block
IERR_READ       equ 2
IERR_BLOCK      equ 3           ; reserved block type
IERR_CODE       equ 4           ; no code matched the bits in the stream
IERR_SYMBOL     equ 5           ; symbol outside the alphabet
IERR_DIST       equ 6           ; back reference points before the output
IERR_SPACE      equ 7           ; over-subscribed Huffman code
IERR_LEN        equ 8           ; stored block length check failed
IERR_OVER       equ 9           ; output length disagrees with the directory
IERR_CRC        equ 10          ; the bytes came out wrong
IERR_WRITE      equ 11          ; the output file would not take the data
IERR_ABORT      equ 12          ; BREAK during a long export
IERR_DSYM       equ 13          ; distance symbol outside the alphabet
IERR_HDR        equ 14          ; dynamic block header out of range
IERR_LENS       equ 15          ; more code lengths than the header allows


; ================================================================
; Plugin entry point
; HL = context pointer, DE = services pointer
; ================================================================
plugin_start
        ld (ctxPtr),hl
        ld (svcPtr),de
        call patch_services
        call init_context
        call find_eocd
        call read_central_dir
        call scan_entries

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
        jr c,.pgdn_last     ; past 255 entries the addition wraps
        cp b
        jp c,.pgdn_set
.pgdn_last
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
.wait_release
        call call_input
        or a
        jr nz,.wait_release
        jp .input

; ---- extract the entry under the cursor ----
.extract
        xor a
        ld (abortFlag),a
        call show_busy_one
        call do_extract_current
        call restore_help
        jp .wait_release

; ---- extract every entry ----
.extract_all
        xor a
        ld (abortFlag),a
        call show_busy_all
        call do_extract_all
        call restore_help
        jp .wait_release


; ================================================================
; init_context: read plugin context into local variables
; ================================================================
init_context
        ld ix,(ctxPtr)
        ld l,(ix+VIEWCTX_DATA_PAGES)
        ld h,(ix+VIEWCTX_DATA_PAGES+1)
        ld (dataPagesPtr),hl
        ld l,(ix+VIEWCTX_SIZE_LO)
        ld h,(ix+VIEWCTX_SIZE_LO+1)
        ld (fileSizeLo),hl
        ld l,(ix+VIEWCTX_SIZE_HI)
        ld h,(ix+VIEWCTX_SIZE_HI+1)
        ld (fileSizeHi),hl
        xor a
        ld (curEntry),a
        ld (topEntry),a
        ld (totalFiles),a
        ld (zipError),a
        ld (cdTrunc),a
        ret


; ================================================================
; find_eocd: pull the last EOCD_SCAN bytes of the file into the scratch
; page and search them backwards for the end of central directory
; record. Backwards, because the archive comment may contain the
; signature itself and the real record is the last one.
; ================================================================
find_eocd
        ld hl,(fileSizeHi)
        ld a,h
        or l
        jr nz,.window_full
        ld hl,(fileSizeLo)
        ld de,EOCD_SCAN
        or a
        sbc hl,de
        jr nc,.window_full

        ld hl,(fileSizeLo)          ; shorter than the window: read it whole
        ld (scanLen),hl
        ld de,EOCD_MIN
        or a
        sbc hl,de
        jp c,.no_eocd
        ld hl,0
        ld (srcOffLo),hl
        ld (srcOffHi),hl
        jr .read

.window_full
        ld hl,EOCD_SCAN
        ld (scanLen),hl
        ld hl,(fileSizeLo)
        ld de,(fileSizeHi)
        ld bc,EOCD_SCAN
        call sub32_16
        ld (srcOffLo),hl
        ld (srcOffHi),de

.read
        call scratch_page_no
        ld hl,0
        ld de,(scanLen)
        call read_source
        jp c,.read_failed
        call map_scratch

        ld hl,(scanLen)             ; last position an EOCD could start at
        ld de,EOCD_MIN
        or a
        sbc hl,de
        jp c,.no_eocd
        ld de,$E000
        add hl,de
.scan
        ld a,(hl)
        cp "P"
        jr nz,.prev
        push hl
        inc hl
        ld a,(hl)
        cp "K"
        jr nz,.no_match
        inc hl
        ld a,(hl)
        cp 5
        jr nz,.no_match
        inc hl
        ld a,(hl)
        cp 6
        jr nz,.no_match
        pop hl
        jr .found
.no_match
        pop hl
.prev
        ld a,h
        cp $E0
        jr nz,.step_back
        ld a,l
        or a
        jr z,.no_eocd               ; reached the start of the window
.step_back
        dec hl
        jr .scan

.found
        ld de,10
        add hl,de
        ld e,(hl)
        inc hl
        ld d,(hl)
        ld (zipEntries),de
        inc hl
        ld e,(hl)
        inc hl
        ld d,(hl)
        inc hl
        ld (cdSizeLo),de
        ld e,(hl)
        inc hl
        ld d,(hl)
        inc hl
        ld (cdSizeHi),de
        ld e,(hl)
        inc hl
        ld d,(hl)
        inc hl
        ld (cdOffLo),de
        ld e,(hl)
        inc hl
        ld d,(hl)
        ld (cdOffHi),de

        ; ZIP64 parks $FFFFFFFF here and puts the real offset in a record
        ; of its own, which this plugin does not read
        ld hl,(cdOffHi)
        ld a,h
        and l
        inc a
        jr nz,.ok
        ld a,ZERR_ZIP64
        jr .fail
.ok
        xor a
        ld (zipError),a
        ret

.no_eocd
        ld a,ZERR_NOEOCD
        jr .fail
.read_failed
        ld a,ZERR_READ
.fail
        ld (zipError),a
        ld hl,0
        ld (cdSizeLo),hl
        ld (cdSizeHi),hl
        ret


; ================================================================
; read_central_dir: copy the directory into data pages 0-6. Anything
; past 56KB of directory is dropped, which only bites on archives with
; several thousand members.
; ================================================================
read_central_dir
        ld hl,0
        ld (cdLen),hl
        ld a,(zipError)
        or a
        ret nz

        ld hl,(cdSizeHi)
        ld a,h
        or l
        jr nz,.clamp
        ld hl,(cdSizeLo)
        ld de,CD_MAX_BYTES
        or a
        sbc hl,de
        jr nc,.clamp
        ld hl,(cdSizeLo)
        jr .store_len
.clamp
        ld a,1
        ld (cdTrunc),a
        ld hl,CD_MAX_BYTES
.store_len
        ld (cdLen),hl
        ld a,h
        or l
        ret z

        xor a
        ld (cdPageIx),a
.page_loop
        ld a,(cdPageIx)
        cp CD_MAX_PAGES
        ret nc

        call cd_page_base           ; HL = cdPageIx * 8192
        ld (cdChunkOff),hl
        ex de,hl
        ld hl,(cdLen)
        or a
        sbc hl,de                   ; HL = bytes still to fetch
        ret z
        ret c
        ld de,8192
        or a
        sbc hl,de
        jr c,.tail
        ld hl,8192
        jr .have_chunk
.tail
        add hl,de
.have_chunk
        ld (cdChunkLen),hl

        ld hl,(cdOffLo)
        ld de,(cdOffHi)
        ld bc,(cdChunkOff)
        call add32_16
        ld (srcOffLo),hl
        ld (srcOffHi),de

        ld a,(cdPageIx)
        call data_page_no           ; C = page number
        ld hl,0
        ld de,(cdChunkLen)
        call read_source
        jr nc,.next
        ld a,ZERR_READ
        ld (zipError),a
        ld hl,0
        ld (cdLen),hl
        ret
.next
        ld hl,cdPageIx
        inc (hl)
        jr .page_loop


; HL = cdPageIx * 8192
cd_page_base
        ld a,(cdPageIx)
        ld l,a
        ld h,0
        add hl,hl
        add hl,hl
        add hl,hl
        add hl,hl
        add hl,hl               ; index * 32
        ld h,l
        ld l,0                  ; and * 256 on top: index * 8192
        ret


; ================================================================
; scan_entries: walk the directory copy and remember where each entry
; starts. The fields themselves are read back from those offsets on
; demand rather than unpacked into a table, which keeps the plugin RAM
; free for what comes next.
; ================================================================
scan_entries
        xor a
        ld (totalFiles),a
        ld hl,(cdLen)
        ld a,h
        or l
        ret z
        ld hl,0
        ld (cdPtr),hl
.loop
        ld a,(totalFiles)
        cp MAX_ENTRIES
        ret nc

        ; the whole fixed header has to fit inside the copy
        ld hl,(cdLen)
        ld de,(cdPtr)
        or a
        sbc hl,de
        ld de,46
        or a
        sbc hl,de
        ret c

        ld hl,(cdPtr)
        call read_cd_byte
        cp "P"
        ret nz
        ld hl,(cdPtr)
        inc hl
        call read_cd_byte
        cp "K"
        ret nz
        ld hl,(cdPtr)
        inc hl
        inc hl
        call read_cd_byte
        cp 1
        ret nz
        ld hl,(cdPtr)
        ld de,3
        add hl,de
        call read_cd_byte
        cp 2
        ret nz

        ld a,(totalFiles)
        call entry_slot
        ld de,(cdPtr)
        ld (hl),e
        inc hl
        ld (hl),d
        ld hl,totalFiles
        inc (hl)

        ; step over the fixed part and the three variable length fields
        ld hl,(cdPtr)
        ld de,28
        add hl,de
        call read_cd_word
        ld (skipName),hl
        ld hl,(cdPtr)
        ld de,30
        add hl,de
        call read_cd_word
        ld (skipExtra),hl
        ld hl,(cdPtr)
        ld de,32
        add hl,de
        call read_cd_word
        ld de,(skipName)
        add hl,de
        ld de,(skipExtra)
        add hl,de
        ld de,46
        add hl,de
        ld de,(cdPtr)
        add hl,de
        ld (cdPtr),hl
        jp .loop


; A = entry index -> HL = its slot in entryOffsets
entry_slot
        ld l,a
        ld h,0
        add hl,hl
        ld de,entryOffsets
        add hl,de
        ret


; ================================================================
; read_entry_fields: A = entry index. Pulls every field the listing and
; the extractor need out of the directory copy.
; ================================================================
read_entry_fields
        call entry_slot
        ld e,(hl)
        inc hl
        ld d,(hl)
        ld (entryBase),de

        ld hl,(entryBase)
        ld de,8
        add hl,de
        call read_cd_word
        ld (entryFlags),hl

        ld hl,(entryBase)
        ld de,10
        add hl,de
        call read_cd_word
        ld (entryMethod),hl

        ld hl,(entryBase)
        ld de,16
        add hl,de
        call read_cd_dword
        ld (entryCrcLo),hl
        ld (entryCrcHi),de

        ld hl,(entryBase)
        ld de,20
        add hl,de
        call read_cd_dword
        ld (entryCompLo),hl
        ld (entryCompHi),de

        ld hl,(entryBase)
        ld de,24
        add hl,de
        call read_cd_dword
        ld (entryUncLo),hl
        ld (entryUncHi),de

        ld hl,(entryBase)
        ld de,28
        add hl,de
        call read_cd_word
        ld a,h
        or a
        ld a,l
        jr z,.name_ok
        ld a,255            ; a name longer than 255 is clipped here
.name_ok
        ld (nameShort),a

        ld hl,(entryBase)
        ld de,42
        add hl,de
        call read_cd_dword
        ld (entryLocalLo),hl
        ld (entryLocalHi),de

        call locate_basename
        jp classify_entry


; ================================================================
; locate_basename: narrow the entry name down to the part after the
; last separator, which is what both the listing and the output file
; name are built from. A name ending in a separator is a directory
; entry, which ZIP uses to record empty folders.
; ================================================================
locate_basename
        ld hl,(entryBase)
        ld de,46
        add hl,de
        ld (baseOff),hl
        ld a,(nameShort)
        ld (baseLen),a
        xor a
        ld (isDir),a
        ld a,(baseLen)
        or a
        ret z

        ld e,a
        dec e
        ld d,0
        ld hl,(baseOff)
        add hl,de
        call read_cd_byte
        cp "/"
        jr nz,.scan
        ld a,1
        ld (isDir),a
        ld hl,baseLen
        dec (hl)

.scan
        ld a,(baseLen)
        or a
        ret z
        ld b,a
.loop
        ld a,b
        dec a
        ld (scanIdx),a
        ld e,a
        ld d,0
        ld hl,(baseOff)
        add hl,de
        push bc
        call read_cd_byte
        pop bc
        cp "/"
        jr z,.hit
        cp 92               ; backslash: DOS made archives use it
        jr z,.hit
        djnz .loop
        ret
.hit
        ld a,(scanIdx)
        inc a
        ld e,a
        ld d,0
        ld hl,(baseOff)
        add hl,de
        ld (baseOff),hl
        ld hl,baseLen
        ld a,(hl)
        sub e
        ld (hl),a
        ret


; ================================================================
; classify_entry: decide what can be done with the entry under the
; cursor. Both stored and deflated members are streamed out a piece at
; a time, so neither has a size limit.
; ================================================================
classify_entry
        ld a,(isDir)
        or a
        ld a,KIND_DIR
        jr nz,.set
        ld hl,(entryFlags)
        bit 0,l
        ld a,KIND_CRYPT
        jr nz,.set
        ld hl,(entryMethod)
        ld a,h
        or a
        jr nz,.method_bad
        ld a,l
        or a
        jr z,.stored
        cp 8
        ld a,KIND_PACKED
        jr z,.set
.method_bad
        ld a,KIND_METHOD
        jr .set
.stored
        xor a
.set
        ld (entryKind),a
        ret


; ================================================================
; build_display_name: basename into nameBuf, DISP_NAME chars at most.
; Directory entries keep their slash so the listing shows what they are.
; ================================================================
build_display_name
        ld hl,nameBuf
        ld (dstPtr),hl
        ld a,(baseLen)
        ld b,a
        ld a,DISP_NAME
        cp b
        jr nc,.len_ok
        ld b,a
.len_ok
        ld a,b
        or a
        jr z,.dir_mark
        ld hl,(baseOff)
        ld (walkPtr),hl
.loop
        push bc
        ld hl,(walkPtr)
        call read_cd_byte
        cp " "
        jr c,.bad
        cp $7F
        jr c,.store
.bad
        ld a,"?"
.store
        ld hl,(dstPtr)
        ld (hl),a
        inc hl
        ld (dstPtr),hl
        ld hl,(walkPtr)
        inc hl
        ld (walkPtr),hl
        pop bc
        djnz .loop

.dir_mark
        ld a,(isDir)
        or a
        jr z,.term
        ld hl,(dstPtr)
        ld de,nameBuf+DISP_NAME
        or a
        sbc hl,de
        jr nc,.term         ; no room left for the slash
        ld hl,(dstPtr)
        ld (hl),"/"
        inc hl
        ld (dstPtr),hl
.term
        ld hl,(dstPtr)
        ld (hl),0
        ret


; ================================================================
; Reading the directory copy. The 56KB of it live in data pages 0-6,
; one 8K page at a time in the single slot a plugin can remap.
; ================================================================

; read_cd_byte: HL = offset in the directory copy -> A. Preserves BC, DE.
read_cd_byte
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


; read_cd_word: HL = offset -> HL = little endian word. Preserves DE.
read_cd_word
        push hl
        call read_cd_byte
        ld c,a
        pop hl
        inc hl
        call read_cd_byte
        ld h,a
        ld l,c
        ret


; read_cd_dword: HL = offset -> HL = low word, DE = high word
read_cd_dword
        push hl
        call read_cd_word
        ld (dwLow),hl
        pop hl
        inc hl
        inc hl
        call read_cd_word
        ex de,hl
        ld hl,(dwLow)
        ret


; A = data page index -> C = the 8K page number the host gave us
data_page_no
        ld l,a
        ld h,0
        ld de,(dataPagesPtr)
        add hl,de
        ld c,(hl)
        ret

scratch_page_no
        ld a,SCRATCH_PAGE_IX
        jr data_page_no

; map the scratch page in, so it can be read with plain memory accesses
map_scratch
        call scratch_page_no
        ld a,c
        nextreg $57,a
        ret


; ================================================================
; read_source: C = destination page, HL = offset in that page,
; DE = byte count, source offset in srcOffLo/srcOffHi.
; Carry set on failure.
; ================================================================
read_source
        push bc
        push de
        push hl
        ld ix,(ctxPtr)
        ld hl,(srcOffLo)
        ld (ix+VIEWCTX_EXTRACT_OFF),l
        ld (ix+VIEWCTX_EXTRACT_OFF+1),h
        ld hl,(srcOffHi)
        ld (ix+VIEWCTX_EXTRACT_OFHI),l
        ld (ix+VIEWCTX_EXTRACT_OFHI+1),h
        pop hl
        pop de
        pop bc
        jp call_read_at


; ================================================================
; 32 bit helpers. A 32 bit value lives in HL (low word) and DE (high).
; ================================================================

; HL:DE = HL:DE + BC
add32_16
        add hl,bc
        ret nc
        inc de
        ret

; HL:DE = HL:DE - BC
sub32_16
        or a
        sbc hl,bc
        ret nc
        dec de
        ret

; div32_10: HL:DE divided by 10, remainder in A.
; The dividend shifts left out of the top into the remainder while the
; quotient shifts in at the bottom, so it ends up back in HL:DE.
div32_10
        xor a
        ld b,32
.loop
        add hl,hl
        rl e
        rl d
        rla
        cp 10
        jr c,.next
        sub 10
        inc l
.next
        djnz .loop
        ret


; ================================================================
; make_decimal32: HL = low word, DE = high word -> numBuf, 8 characters
; right justified and space padded. Anything wider shows as ########,
; which no ZIP member on a Spectrum is ever going to hit.
; ================================================================
make_decimal32
        push hl
        push de
        ld hl,numBuf
        ld b,8
.blank
        ld (hl)," "
        inc hl
        djnz .blank
        ld (hl),0
        pop de
        pop hl
        ld ix,numBuf+7
        ld b,8
.digit
        push bc
        call div32_10
        add a,"0"
        ld (ix+0),a
        dec ix
        pop bc
        ld a,h
        or l
        or d
        or e
        ret z
        djnz .digit
        ld hl,numBuf
        ld b,8
.over
        ld (hl),"#"
        inc hl
        djnz .over
        ret


; num32_to_buf: HL = pointer to a 4 byte little endian value -> numBuf
num32_to_buf
        ld e,(hl)
        inc hl
        ld d,(hl)
        inc hl
        ld c,(hl)
        inc hl
        ld b,(hl)
        ex de,hl                ; HL = low word
        ld d,b
        ld e,c                  ; DE = high word
        jp make_decimal32


; ================================================================
; render_page: clear the content area and draw the visible entries
; ================================================================
render_page
        call clear_content_area
        ld a,(zipError)
        or a
        jr nz,.error
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
.error
        call error_text
        ld hl,2*256+CONTENT_ROW
        ld a,ATTR_NORMAL
        jp call_print


; DE = the message for the current zipError
error_text
        ld a,(zipError)
        cp ZERR_ZIP64
        ld de,strErrZip64
        ret z
        cp ZERR_READ
        ld de,strErrRead
        ret z
        ld de,strErrNoEocd
        ret


; ================================================================
; render_entry: draw one directory row.
; A = entry index (0-based), D = screen row
;  col 0-2 index, 4-18 name, 20-27 size, 29-36 packed size, 38 method
; ================================================================
render_entry
        ld (renderIdx),a
        ld a,d
        ld (renderRow),a
        ld a,(renderIdx)
        call read_entry_fields
        call build_display_name

        ; screen address = $4000 + row*160 + 2 (col 1, inside the border)
        ld a,(renderRow)
        ld e,a
        ld d,160
        mul d,e
        ld hl,$4002
        add hl,de
        ld (screenPos),hl

        ; attribute: the cursor bar wins, then what can be done with it
        ld a,(renderIdx)
        ld b,a
        ld a,(curEntry)
        cp b
        jr z,.sel
        ld a,(entryKind)
        cp KIND_DIR
        jr z,.adir
        or a
        jr z,.astored
        ld a,ATTR_PACKED
        jr .aset
.sel
        ld a,ATTR_SELECTED
        jr .aset
.adir
        ld a,ATTR_DIR
        jr .aset
.astored
        ld a,ATTR_STORED
.aset
        ld (curAttr),a

        ld de,(screenPos)

        ld a,(renderIdx)
        inc a
        ld l,a
        ld h,0
        call write_dec3
        ld a," "
        call put_char

        ld hl,nameBuf
        ld b,DISP_NAME
        call write_padded
        ld a," "
        call put_char

        ld hl,entryUncLo
        call write_num32
        ld a," "
        call put_char

        ld hl,entryCompLo
        call write_num32
        ld a," "
        call put_char

        call method_char
        jp put_char


; A = the letter shown in the method column
method_char
        ld a,(entryKind)
        cp KIND_DIR
        ld a,"/"
        ret z
        ld a,(entryKind)
        cp KIND_CRYPT
        ld a,"X"
        ret z
        ld hl,(entryMethod)
        ld a,h
        or a
        jr nz,.other
        ld a,l
        or a
        ld a,"S"
        ret z
        ld a,l
        cp 8
        ld a,"D"
        ret z
.other
        ld a,"?"
        ret


; ================================================================
; render_title
; ================================================================
render_title
        ld b,TITLE_ROW
        ld a,ATTR_TITLE
        call clear_row

        ld de,strZip
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

        ld a,(totalFiles)
        ld l,a
        ld h,0
        ld de,0
        call make_decimal32
        ld de,numBuf+5
        ld hl,68*256+TITLE_ROW
        ld a,ATTR_TITLE
        call call_print
        ld de,strFiles
        ld hl,72*256+TITLE_ROW
        ld a,ATTR_TITLE
        jp call_print


; ================================================================
; render_col_header: column titles, plus a warning when the listing
; does not cover the whole archive
; ================================================================
render_col_header
        ld b,HEADER_ROW
        ld a,ATTR_HEADER
        call clear_row

        ld de,strColHdr
        ld hl,1*256+HEADER_ROW
        ld a,ATTR_HEADER
        call call_print

        ld a,(zipError)
        or a
        ret nz
        ld a,(cdTrunc)
        or a
        jr nz,.warn
        ld hl,(zipEntries)
        ld a,(totalFiles)
        ld e,a
        ld d,0
        or a
        sbc hl,de
        ret z
.warn
        ld de,strTrunc
        ld hl,45*256+HEADER_ROW
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
; Busy banner. Every export reopens the archive, seeks and creates the
; output file, which takes long enough that the plugin has to say it is
; working before the first name appears.
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
        ld de,0
        call make_decimal32
        ld hl,numBuf+5
        ld de,progressBuf
        ld bc,3
        ldir
        ld a,"/"
        ld (de),a
        inc de
        push de
        ld a,(totalFiles)
        ld l,a
        ld h,0
        ld de,0
        call make_decimal32
        pop de
        ld hl,numBuf+5
        ld bc,3
        ldir
        xor a
        ld (de),a
        ld de,strFileLabel
        ld hl,42*256+(CONTENT_ROW+3)
        ld a,ATTR_PACKED
        call call_print
        ld de,progressBuf
        ld hl,48*256+(CONTENT_ROW+3)
        ld a,ATTR_PACKED
        jp call_print


; ================================================================
; render_selection_info: output name, size and kind for the entry
; under the cursor
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


show_export_target
        ld de,strExportName
        ld hl,42*256+CONTENT_ROW
        ld a,ATTR_PACKED
        call call_print
        ld de,extractName
        ld hl,48*256+CONTENT_ROW
        ld a,ATTR_PACKED
        call call_print

        ld hl,entryUncLo
        call num32_to_buf
        ld de,strExportLen
        ld hl,42*256+(CONTENT_ROW+1)
        ld a,ATTR_PACKED
        call call_print
        ld de,numBuf
        ld hl,48*256+(CONTENT_ROW+1)
        ld a,ATTR_PACKED
        call call_print

        ld de,strExportType
        ld hl,42*256+(CONTENT_ROW+2)
        ld a,ATTR_PACKED
        call call_print
        call kind_text
        ld hl,48*256+(CONTENT_ROW+2)
        ld a,ATTR_PACKED
        jp call_print


; DE = the fixed width name of the current entryKind
kind_text
        ld a,(entryKind)
        ld de,strKindStored
        or a
        ret z
        cp KIND_DIR
        ld de,strKindDir
        ret z
        cp KIND_CRYPT
        ld de,strKindCrypt
        ret z
        cp KIND_PACKED
        ld de,strKindPacked
        ret z
        ld de,strKindMethod
        ret


; ================================================================
; do_extract_current: write out the entry under the cursor
; ================================================================
do_extract_current
        ld a,(totalFiles)
        or a
        ret z
        ld a,(curEntry)
        jp do_extract_entry


; ================================================================
; do_extract_entry: A = entry index. Stored members are copied straight
; out of the archive by the seeking extract service, so their data may
; sit anywhere in the file rather than in the 64KB the viewer loaded.
; extractStatus is 0 on success.
; ================================================================
do_extract_entry
        ld (extractIdx),a
        xor a
        ld (extractStatus),a
        ld a,(extractIdx)
        call read_entry_fields
        ld a,(extractIdx)
        call build_extract_name
        call clear_debug_area
        call show_export_target
        call show_progress

        ld a,(entryKind)
        cp KIND_PACKED
        jr z,.supported
        or a
        jp nz,.unsupported
.supported
        ld de,strWorking
        ld hl,42*256+(CONTENT_ROW+5)
        ld a,ATTR_PACKED
        call call_print

        call find_data_offset
        jr c,.fail_code

        ld ix,(ctxPtr)
        ld a,$FF
        ld (ix+VIEWCTX_P3DOS_TYPE),a    ; a ZIP member is a raw file
        ld hl,extractName
        call call_write_open
        jr c,.fail_code

        ld a,(entryKind)
        cp KIND_PACKED
        jr z,.inflate_it
        call stream_stored
        jr .finish
.inflate_it
        call run_inflate
.finish
        ld (opCode),a
        jr nc,.wrote_ok
        call call_write_close       ; the file is closed either way
        call restore_central_dir
        ld a,(opCode)
        jr .fail_code
.wrote_ok
        call call_write_close
        ld (opCode),a
        ; the output ran through the pages the directory copy lives in
        call restore_central_dir
        ld a,(opCode)
        or a
        jr nz,.fail_code

.ok
        xor a
        ld (extractStatus),a
        ld de,strExportOK
        jr .report
.fail_code
        ld (extractStatus),a
        ld a,(abortFlag)
        or a
        ld de,strAborted
        jr nz,.report
        ld a,(extractStatus)
        call fail_code_char
        ld (strExportFailCode),a
        ld de,strExportFail
        jr .report
.unsupported
        ld a,255
        ld (extractStatus),a
        call kind_text
.report
        ld hl,42*256+(CONTENT_ROW+5)
        ld a,ATTR_PACKED
        jp call_print


; A = failure code -> A = the character shown after FAIL. Codes above 9
; become letters, so an inflate failure stays apart from an extract one.
fail_code_char
        cp 10
        jr c,.digit
        add a,"A"-10
        ret
.digit
        add a,"0"
        ret


; ================================================================
; find_data_offset: work out where the member data starts. The local
; header carries its own name and extra lengths, which need not match
; the ones in the directory, so it has to be read.
; Carry set on failure, with A = the failure code.
; ================================================================
find_data_offset
        ld hl,(entryLocalLo)
        ld (srcOffLo),hl
        ld hl,(entryLocalHi)
        ld (srcOffHi),hl
        call scratch_page_no
        ld hl,0
        ld de,LOCAL_HDR
        call read_source
        jr c,.io_fail
        call map_scratch

        ld a,($E000)
        cp "P"
        jr nz,.bad_local
        ld a,($E001)
        cp "K"
        jr nz,.bad_local
        ld a,($E002)
        cp 3
        jr nz,.bad_local
        ld a,($E003)
        cp 4
        jr nz,.bad_local

        ld hl,(entryLocalLo)
        ld de,(entryLocalHi)
        ld bc,LOCAL_HDR
        call add32_16
        ld bc,($E000+26)        ; the local name length, not the one in
        call add32_16           ; the directory: they may differ
        ld bc,($E000+28)
        call add32_16
        ld (dataOffLo),hl
        ld (dataOffHi),de
        or a
        ret
.io_fail
        ld a,7
        scf
        ret
.bad_local
        ld a,8
        scf
        ret


; ================================================================
; stream_stored: copy a stored member out a page at a time. Sending it
; through the same write services the inflate path uses means a stored
; member has no size limit either.
; Carry set on failure with A = the service failure code.
; ================================================================
stream_stored
        ld hl,0
        ld (flushedLo),hl       ; the byte counter show_written reports
        ld (flushedHi),hl
        ld hl,(entryCompLo)
        ld (srcRemLo),hl
        ld hl,(entryCompHi)
        ld (srcRemHi),hl
        ld hl,(dataOffLo)
        ld (srcOffLo),hl
        ld hl,(dataOffHi)
        ld (srcOffHi),hl
        call show_written
.loop
        ld hl,(srcRemLo)
        ld de,(srcRemHi)
        ld a,h
        or l
        or d
        or e
        jr z,.done
        ld a,d
        or e
        jr nz,.full
        ld de,FLUSH_SIZE
        or a
        sbc hl,de
        jr nc,.full
        ld hl,(srcRemLo)        ; the tail is shorter than a page
        jr .have
.full
        ld hl,FLUSH_SIZE
.have
        ld (inChunk),hl

        xor a
        call data_page_no       ; stage it through the first data page
        ld hl,0
        ld de,(inChunk)
        call read_source
        ret c
        ld de,0
        ld bc,(inChunk)
        call call_write_chunk
        ret c

        ld hl,(srcOffLo)
        ld de,(srcOffHi)
        ld bc,(inChunk)
        call add32_16
        ld (srcOffLo),hl
        ld (srcOffHi),de
        ld hl,(srcRemLo)
        ld de,(srcRemHi)
        ld bc,(inChunk)
        call sub32_16
        ld (srcRemLo),hl
        ld (srcRemHi),de
        ld hl,(flushedLo)
        ld de,(flushedHi)
        ld bc,(inChunk)
        call add32_16
        ld (flushedLo),hl
        ld (flushedHi),de
        call show_written
        call poll_abort
        jp nc,.loop
        ld a,IERR_ABORT
        scf
        ret
.done
        or a
        ret


; ================================================================
; restore_central_dir: writing a member out runs through the pages the
; directory copy lives in, so it has to be read back before the listing
; or an export-all run can carry on.
; ================================================================
restore_central_dir
        call read_central_dir
        jp scan_entries


; ================================================================
; do_extract_all: write out every stored member. Directories, encrypted
; and deflated members are counted as skipped rather than treated as a
; failure, because a normal archive is full of them and stopping on the
; first one would export nothing at all.
; ================================================================
do_extract_all
        xor a
        ld (bulkIdx),a
        ld (okCount),a
        ld (skipCount),a
.loop
        ld a,(bulkIdx)
        ld b,a
        ld a,(totalFiles)
        cp b
        jr z,.done
        jr c,.done

        ld a,(bulkIdx)
        call read_entry_fields
        ld a,(entryKind)
        cp KIND_PACKED
        jr z,.take
        or a
        jr nz,.skip
.take
        ld a,(bulkIdx)
        call do_extract_entry
        ld a,(abortFlag)
        or a
        jr nz,.stopped
        ld a,(extractStatus)
        or a
        jr nz,.failed
        ld hl,okCount
        inc (hl)
        jr .next
.skip
        ld hl,skipCount
        inc (hl)
.next
        ld hl,bulkIdx
        inc (hl)
        jr .loop

.done
        call clear_debug_area
        ld de,strAllExported
        ld hl,42*256+CONTENT_ROW
        ld a,ATTR_PACKED
        call call_print
        ld a,(okCount)
        ld l,a
        ld h,0
        ld de,0
        call make_decimal32
        ld de,numBuf+5
        ld hl,53*256+CONTENT_ROW
        ld a,ATTR_PACKED
        call call_print

        ld de,strAllSkipped
        ld hl,42*256+(CONTENT_ROW+1)
        ld a,ATTR_PACKED
        call call_print
        ld a,(skipCount)
        ld l,a
        ld h,0
        ld de,0
        call make_decimal32
        ld de,numBuf+5
        ld hl,53*256+(CONTENT_ROW+1)
        ld a,ATTR_PACKED
        jp call_print

.stopped
        ld de,strAborted
        ld hl,42*256+CONTENT_ROW
        ld a,ATTR_PACKED
        jp call_print

.failed
        call fail_code_char
        ld (strExportAllFailCode),a
        call clear_debug_area
        ld de,strExportAllFail
        ld hl,42*256+CONTENT_ROW
        ld a,ATTR_PACKED
        jp call_print


; ================================================================
; build_extract_name: A = entry index -> extractName, an 8.3 name built
; from the basename. ZIP names are long, mixed case and carry a path,
; while the extract service takes 16 bytes, so the name has to be cut
; down and sanitised.
; ================================================================
build_extract_name
        ld (nameIdx),a
        call clear_extract_name
        ld a,(isDir)
        or a
        ret nz              ; directories are never written out

        ld a,(baseLen)
        or a
        jp z,build_num_name
        ld (stemLen),a
        xor a
        ld (extLen),a

        ; find the last dot, ignoring one in first position
        ld a,(baseLen)
        ld b,a
.dot
        ld a,b
        dec a
        or a
        jr z,.no_dot
        ld (scanIdx),a
        ld e,a
        ld d,0
        ld hl,(baseOff)
        add hl,de
        push bc
        call read_cd_byte
        pop bc
        cp "."
        jr z,.found_dot
        djnz .dot
        jr .no_dot
.found_dot
        ld a,(scanIdx)
        ld (stemLen),a
        ld c,a
        ld a,(baseLen)
        sub c
        dec a
        ld (extLen),a

.no_dot
        ld hl,extractName
        ld (dstPtr),hl
        ld hl,(baseOff)
        ld (walkPtr),hl
        ld a,(stemLen)
        ld b,a
        ld a,8
        cp b
        jr nc,.stem_ok
        ld b,a
.stem_ok
        ld a,b
        or a
        jp z,build_num_name
        call copy_sanitised

        ld a,(extLen)
        or a
        jr z,.done
        ld hl,(dstPtr)
        ld (hl),"."
        inc hl
        ld (dstPtr),hl
        ld a,(stemLen)
        inc a               ; step over the stem and the dot
        ld e,a
        ld d,0
        ld hl,(baseOff)
        add hl,de
        ld (walkPtr),hl
        ld a,(extLen)
        ld b,a
        ld a,3
        cp b
        jr nc,.ext_ok
        ld b,a
.ext_ok
        call copy_sanitised
.done
        ld hl,(dstPtr)
        ld (hl),0
        ret


; copy B characters from walkPtr to dstPtr, sanitising as they go
copy_sanitised
        ld a,b
        or a
        ret z
.loop
        push bc
        ld hl,(walkPtr)
        call read_cd_byte
        call sanitize_filename_char
        ld hl,(dstPtr)
        ld (hl),a
        inc hl
        ld (dstPtr),hl
        ld hl,(walkPtr)
        inc hl
        ld (walkPtr),hl
        pop bc
        djnz .loop
        ret


; a name made only of unusable bytes falls back to FILEnnn.BIN
build_num_name
        ld hl,strNumName
        ld de,extractName
        ld bc,12
        ldir
        ld a,(nameIdx)
        inc a
        ld l,a
        ld h,0
        ld de,0
        call make_decimal32
        ld hl,numBuf+5
        ld de,extractName+4
        ld b,3
.digit
        ld a,(hl)
        cp " "
        jr nz,.store
        ld a,"0"
.store
        ld (de),a
        inc hl
        inc de
        djnz .digit
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
        cp "A"
        jr c,.maybe_digit
        cp "Z"+1
        ret c
.maybe_digit
        cp "0"
        jr c,.maybe_lower
        cp "9"+1
        ret c
.maybe_lower
        cp "a"
        jr c,.punct
        cp "z"+1
        ret c
.punct
        cp "_"
        ret z
        cp "-"
        ret z
        ld a,"_"
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
        cp CONTENT_ROW+6
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
.row
        push bc
        push hl
        ld b,76
.col
        ld a," "
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
.l
        ld a," "
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

; write B chars from HL at DE, space padded when the string ends early
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
.pad
        ld a," "
        ld (de),a
        inc de
        ld a,(curAttr)
        ld (de),a
        inc de
        djnz .pad
        ret

; write HL as a right justified 3 digit number
write_dec3
        push de
        ld de,0
        call make_decimal32
        pop de
        ld hl,numBuf+5
        ld b,3
        jp write_padded

; write the 4 byte value at HL as 8 right justified digits
write_num32
        push de
        call num32_to_buf
        pop de
        ld hl,numBuf
        ld b,8
        jp write_padded


; ================================================================
; INFLATE
;
; A canonical Huffman decoder in the shape of zlib's puff: for each code
; length, count[] says how many codes have it and symbol[] lists them in
; code order, so decoding walks the lengths one bit at a time instead of
; needing a lookup table that would not fit here.
;
; Output goes straight into the data pages, and the same bytes serve as
; the LZ77 window, so a back reference is just a read from earlier in
; the output. Only one 8K slot can be mapped at a time, so both ends of
; a match copy go through map_window, which skips the remap whenever
; source and destination happen to share a page - which they usually do.
; ================================================================

; ================================================================
; run_inflate: decompress the current member into the output window.
; find_data_offset must have run first. Carry set on failure, with A
; already turned into the code the export line shows.
; ================================================================
run_inflate
        ld hl,(dataOffLo)
        ld (srcOffLo),hl
        ld hl,(dataOffHi)
        ld (srcOffHi),hl
        ld hl,(entryCompLo)
        ld (srcRemLo),hl
        ld hl,(entryCompHi)
        ld (srcRemHi),hl

        xor a
        ld (inflErr),a
        ld (bitCnt),a
        ld a,255
        ld (mappedPage),a       ; nothing mapped yet
        ld hl,0
        ld (inLeft),hl
        ld (outTotalLo),hl
        ld (outTotalHi),hl
        ld (flushedLo),hl
        ld (flushedHi),hl
        ld hl,inBuf
        ld (inPtr),hl
        ld hl,$FFFF
        ld (crcLo),hl
        ld (crcHi),hl
        ; the directory length is the budget: out_byte counts it down and
        ; stops a corrupt stream producing for ever
        ld hl,(entryUncLo)
        ld (outLeftLo),hl
        ld hl,(entryUncHi)
        ld (outLeftHi),hl
        call show_written       ; on screen from the outset, not only
                                ; once the first page has been flushed

        call inflate_blocks
        ld a,(inflErr)
        or a
        jr nz,.failed
        call flush_tail
        ld a,(inflErr)
        or a
        jr nz,.failed

        ; the budget has to come out exactly, and the CRC has to match:
        ; together they turn "it produced something" into "it produced
        ; the right thing"
        ld hl,(outLeftLo)
        ld de,(outLeftHi)
        ld a,h
        or l
        or d
        or e
        ld a,IERR_OVER
        jr nz,.failed

        ld hl,(crcLo)
        ld de,(entryCrcLo)
        ld a,l
        cpl
        cp e
        jr nz,.crc_bad
        ld a,h
        cpl
        cp d
        jr nz,.crc_bad
        ld hl,(crcHi)
        ld de,(entryCrcHi)
        ld a,l
        cpl
        cp e
        jr nz,.crc_bad
        ld a,h
        cpl
        cp d
        jr nz,.crc_bad
        or a
        ret
.crc_bad
        ld a,IERR_CRC
.failed
        add a,9                 ; reported as A onwards, apart from the
        scf                     ; numeric codes the extract path uses
        ret


; ================================================================
; inflate_blocks: walk the deflate stream one block at a time
; ================================================================
inflate_blocks
.loop
        call get_bit
        ld a,0
        adc a,a
        ld (lastBlock),a
        ld b,2
        call get_bits
        ld a,(inflErr)
        or a
        ret nz

        ld a,l
        or a
        jr z,.stored
        cp 1
        jr z,.fixed
        cp 2
        jr z,.dynamic
        ld a,IERR_BLOCK
        ld (inflErr),a
        ret

.fixed
        call build_fixed
        jr .codes
.dynamic
        call build_dynamic
.codes
        ld a,(inflErr)
        or a
        ret nz
        call inflate_codes
        jr .block_done
.stored
        call stored_block
.block_done
        ld a,(inflErr)
        or a
        ret nz
        ld a,(lastBlock)
        or a
        jr z,.loop
        ret


; ================================================================
; stored_block: restarts at a byte boundary and carries its length
; twice, the second time inverted.
; ================================================================
stored_block
        xor a
        ld (bitCnt),a           ; drop the rest of the current byte
        ; each byte goes straight to memory: next_in_byte may refill the
        ; input buffer, and nothing survives that in a register
        call next_in_byte
        ld (blockLen),a
        call next_in_byte
        ld (blockLen+1),a
        call next_in_byte
        ld (blockNLen),a
        call next_in_byte
        ld (blockNLen+1),a
        ld a,(inflErr)
        or a
        ret nz
        ld hl,blockNLen
        ld a,(blockLen)
        cpl
        cp (hl)
        jr nz,.bad
        inc hl
        ld a,(blockLen+1)
        cpl
        cp (hl)
        jr nz,.bad
.copy
        ld hl,(blockLen)
        ld a,h
        or l
        ret z
        dec hl
        ld (blockLen),hl
        call next_in_byte
        call out_byte
        ld a,(inflErr)
        or a
        ret nz
        jr .copy
.bad
        ld a,IERR_LEN
        ld (inflErr),a
        ret


; ================================================================
; inflate_codes: the literal/length and distance codes of one block
; ================================================================
inflate_codes
.loop
        ld hl,litCount
        ld de,litSymbol
        call decode
        ld a,(inflErr)
        or a
        ret nz

        ld a,h
        or a
        jr nz,.not_literal
        ld a,l
        call out_byte
        ld a,(inflErr)
        or a
        ret nz
        jr .loop

.not_literal
        ld de,256
        or a
        sbc hl,de
        ret z                   ; symbol 256 ends the block
        dec hl                  ; HL = symbol - 257
        ld a,h
        or a
        jr nz,.bad_symbol
        ld a,l
        cp 29
        jr nc,.bad_symbol
        ld (lenIdx),a

        ld l,a
        ld h,0
        ld de,lenExtra
        add hl,de
        ld b,(hl)
        call get_bits
        ld (extraVal),hl
        ld a,(lenIdx)
        ld l,a
        ld h,0
        add hl,hl
        ld de,lenBase
        add hl,de
        ld e,(hl)
        inc hl
        ld d,(hl)
        ld hl,(extraVal)
        add hl,de
        ld (matchLen),hl

        ld hl,distCount
        ld de,distSymbol
        call decode
        ld a,(inflErr)
        or a
        ret nz
        ld a,h
        or a
        jr nz,.bad_dsym
        ld a,l
        cp NDIST
        jr nc,.bad_dsym
        ld (distIdx),a

        ld l,a
        ld h,0
        ld de,distExtra
        add hl,de
        ld b,(hl)
        call get_bits
        ld (extraVal),hl
        ld a,(distIdx)
        ld l,a
        ld h,0
        add hl,hl
        ld de,distBase
        add hl,de
        ld e,(hl)
        inc hl
        ld d,(hl)
        ld hl,(extraVal)
        add hl,de
        ex de,hl                ; DE = distance
        ld bc,(matchLen)
        call copy_match
        ld a,(inflErr)
        or a
        ret nz
        jp .loop

.bad_symbol
        ld a,IERR_SYMBOL
        ld (inflErr),a
        ret
.bad_dsym
        ld a,IERR_DSYM
        ld (inflErr),a
        ret


; ================================================================
; build_fixed: the fixed tables are the same in every stream, so they
; are built from their code lengths. The loop that fills them in costs
; far less room than 318 entries of ready made table.
; ================================================================
build_fixed
        ld hl,lengths
        ld b,144
        ld a,8
        call fill_lengths
        ld b,112
        ld a,9
        call fill_lengths
        ld b,24
        ld a,7
        call fill_lengths
        ld b,8
        ld a,8
        call fill_lengths
        ld hl,litCount
        ld de,litSymbol
        ld bc,NSYMS
        call construct

        ld hl,lengths
        ld b,NDIST
        ld a,5
        call fill_lengths
        ld hl,distCount
        ld de,distSymbol
        ld bc,NDIST
        jp construct


; fill B bytes of A from HL onwards, leaving HL past them
fill_lengths
        ld (hl),a
        inc hl
        djnz fill_lengths
        ret


; ================================================================
; build_dynamic: read the block's own code lengths and build both
; tables from them.
; ================================================================
build_dynamic
        ld b,5
        call get_bits
        ld de,257
        add hl,de
        ld (nLen),hl
        ld b,5
        call get_bits
        inc hl
        ld (nDist),hl
        ld b,4
        call get_bits
        ld de,4
        add hl,de
        ld (nCLen),hl
        ld a,(inflErr)
        or a
        ret nz

        ld hl,(nLen)
        ld de,NSYMS+1
        or a
        sbc hl,de
        jp nc,.bad
        ld hl,(nDist)
        ld de,NDIST+1
        or a
        sbc hl,de
        jp nc,.bad
        ld hl,(nLen)
        ld de,(nDist)
        add hl,de
        ld (nTotal),hl

        ; the code length code lengths arrive in a scrambled fixed order
        ld hl,lengths
        ld b,19
        xor a
        call fill_lengths
        ld a,(nCLen)
        ld b,a
        ld ix,clOrder
.order
        push bc
        ld b,3
        call get_bits
        ld a,l
        ld e,(ix+0)
        inc ix
        ld d,0
        ld hl,lengths
        add hl,de
        ld (hl),a
        pop bc
        djnz .order

        ; that code is only used to read the real lengths, so it can be
        ; built over the literal table, which is rebuilt straight after
        ld hl,litCount
        ld de,litSymbol
        ld bc,19
        call construct
        ld a,(inflErr)
        or a
        ret nz

        ld hl,0
        ld (lenIx),hl
.read
        ld hl,(lenIx)
        ld de,(nTotal)
        or a
        sbc hl,de
        jr nc,.built

        ld hl,litCount
        ld de,litSymbol
        call decode
        ld a,(inflErr)
        or a
        ret nz
        ld a,h
        or a
        jp nz,.bad
        ld a,l
        cp 16
        jr nc,.repeat
        call store_length
        jr .check

.repeat
        cp 16
        jr nz,.zeros_short
        ld hl,(lenIx)           ; 16: repeat the previous length 3-6 times
        ld a,h
        or l
        jr z,.bad               ; nothing to repeat yet
        dec hl
        ld de,lengths
        add hl,de
        ld a,(hl)
        ld (repVal),a
        ld b,2
        call get_bits
        ld de,3
        jr .do_repeat
.zeros_short
        cp 17
        jr nz,.zeros_long
        xor a                   ; 17: 3-10 zero lengths
        ld (repVal),a
        ld b,3
        call get_bits
        ld de,3
        jr .do_repeat
.zeros_long
        cp 18
        jr nz,.bad
        xor a                   ; 18: 11-138 zero lengths
        ld (repVal),a
        ld b,7
        call get_bits
        ld de,11
.do_repeat
        add hl,de
        ld b,l                  ; a repeat never runs past 138
.rep_loop
        push bc
        ld a,(repVal)
        call store_length
        pop bc
        ld a,(inflErr)
        or a
        ret nz
        djnz .rep_loop
.check
        ld a,(inflErr)
        or a
        ret nz
        jp .read

.built
        ld hl,litCount
        ld de,litSymbol
        ld bc,(nLen)
        call construct
        ld a,(inflErr)
        or a
        ret nz
        ; move the distance lengths to the front so construct can always
        ; read them from the start of the array
        ld hl,lengths
        ld de,(nLen)
        add hl,de
        ld de,lengths
        ld bc,(nDist)
        ldir
        ld hl,distCount
        ld de,distSymbol
        ld bc,(nDist)
        jp construct
.bad
        ld a,IERR_HDR
        ld (inflErr),a
        ret


; store_length: A = code length, appended at lenIx
store_length
        ld c,a
        ld hl,(lenIx)
        ld de,(nTotal)
        or a
        sbc hl,de
        jr c,.room
        ld a,IERR_LENS
        ld (inflErr),a
        ret
.room
        ld hl,(lenIx)
        ld de,lengths
        add hl,de
        ld (hl),c
        ld hl,(lenIx)
        inc hl
        ld (lenIx),hl
        ret


; ================================================================
; construct: HL = count table, DE = symbol table, BC = symbol count.
; The code lengths come from `lengths`. An over-subscribed code is
; rejected, because it would let decode index past the symbol table.
; ================================================================
construct
        ld (conCount),hl
        ld (conSym),de
        ld (conN),bc

        ld hl,(conCount)
        ld b,(MAXBITS+1)*2
.zero
        ld (hl),0
        inc hl
        djnz .zero

        ld hl,lengths
        ld bc,(conN)
.count
        ld a,(hl)
        push hl
        ld l,a
        ld h,0
        add hl,hl
        ld de,(conCount)
        add hl,de
        inc (hl)
        jr nz,.no_carry
        inc hl
        inc (hl)
.no_carry
        pop hl
        inc hl
        dec bc
        ld a,b
        or c
        jr nz,.count

        ld hl,1
        ld c,1
        ld b,MAXBITS
.left
        add hl,hl
        push hl
        ld l,c
        ld h,0
        add hl,hl
        ld de,(conCount)
        add hl,de
        ld e,(hl)
        inc hl
        ld d,(hl)
        pop hl
        or a
        sbc hl,de
        jr c,.over
        inc c
        djnz .left

        ld hl,0
        ld (offsArr+2),hl       ; offs[1] = 0
        ld c,1
        ld b,MAXBITS-1
.offs
        ld l,c
        ld h,0
        add hl,hl
        ld de,offsArr
        add hl,de
        ld e,(hl)
        inc hl
        ld d,(hl)               ; DE = offs[len]
        push de
        ld l,c
        ld h,0
        add hl,hl
        ld de,(conCount)
        add hl,de
        ld e,(hl)
        inc hl
        ld d,(hl)               ; DE = count[len]
        pop hl
        add hl,de
        ex de,hl                ; DE = offs[len] + count[len]
        ld a,c
        inc a
        ld l,a
        ld h,0
        add hl,hl
        push de
        ld de,offsArr
        add hl,de
        pop de
        ld (hl),e
        inc hl
        ld (hl),d
        inc c
        djnz .offs

        ld hl,lengths
        ld (conPtr),hl
        ld bc,0
.fill
        ld hl,(conPtr)
        ld a,(hl)
        or a
        jr z,.next
        ld l,a
        ld h,0
        add hl,hl
        ld de,offsArr
        add hl,de               ; HL = &offs[len]
        ld e,(hl)
        inc hl
        ld d,(hl)
        inc de
        ld (hl),d
        dec hl
        ld (hl),e               ; offs[len]++
        dec de                  ; DE = the slot just claimed
        ex de,hl
        add hl,hl
        ld de,(conSym)
        add hl,de
        ld (hl),c
        inc hl
        ld (hl),b
.next
        ld hl,(conPtr)
        inc hl
        ld (conPtr),hl
        inc bc
        ld hl,(conN)
        or a
        sbc hl,bc
        jr nz,.fill
        ret
.over
        ld a,IERR_SPACE
        ld (inflErr),a
        ret


; ================================================================
; decode: HL = count table, DE = symbol table -> HL = symbol.
; Walks the code lengths a bit at a time; at each length the codes of
; that length occupy a contiguous run, so the symbol is a plain index.
; ================================================================
decode
        ld (decCount),hl
        ld (decSym),de
        ld hl,0
        ld (decCode),hl
        ld (decFirst),hl
        ld (decIndex),hl
        ld c,1
        ld b,MAXBITS
.loop
        call get_bit
        ld hl,(decCode)
        jr nc,.no_bit
        inc l                   ; bit 0 is clear, so inc sets it
.no_bit
        ld (decCode),hl

        ld l,c
        ld h,0
        add hl,hl
        ld de,(decCount)
        add hl,de
        ld e,(hl)
        inc hl
        ld d,(hl)
        ld (decCnt),de          ; DE = count[len]

        ld hl,(decCode)
        or a
        sbc hl,de
        jr c,.hit               ; code < count: certainly this length
        ld de,(decFirst)
        or a
        sbc hl,de
        jr c,.hit

        ld hl,(decIndex)
        ld de,(decCnt)
        add hl,de
        ld (decIndex),hl
        ld hl,(decFirst)
        add hl,de
        add hl,hl
        ld (decFirst),hl
        ld hl,(decCode)
        add hl,hl
        ld (decCode),hl
        inc c
        djnz .loop

        ld a,IERR_CODE
        ld (inflErr),a
        ld hl,0
        ret
.hit
        ld hl,(decCode)
        ld de,(decFirst)
        or a
        sbc hl,de
        ld de,(decIndex)
        add hl,de
        add hl,hl
        ld de,(decSym)
        add hl,de
        ld e,(hl)
        inc hl
        ld d,(hl)
        ex de,hl
        ret


; ================================================================
; Bit and byte input
; ================================================================

; get_bit -> carry = the bit. Clobbers A only.
get_bit
        ld a,(bitCnt)
        or a
        jr nz,.have
        push hl
        call next_in_byte
        pop hl
        ld (bitBuf),a
        ld a,8
.have
        dec a
        ld (bitCnt),a
        ld a,(bitBuf)
        srl a
        ld (bitBuf),a
        ret


; get_bits: B = how many bits (0-16), least significant first -> HL
get_bits
        ld hl,0
        ld a,b
        or a
        ret z
        ld c,b
.loop
        call get_bit
        rr h
        rr l                    ; the bits pile up at the top of HL
        djnz .loop
        ld a,16
        sub c
        ret z
        ld b,a
.norm
        srl h
        rr l
        djnz .norm
        ret


; next_in_byte -> A. Clobbers HL only: a refill goes all the way out to
; a host service, which is free with the registers, so BC, DE and IX are
; saved here rather than at every call site.
; Sets inflErr when the member runs out early.
next_in_byte
        ld hl,(inLeft)
        ld a,h
        or l
        jr nz,.take
        push bc
        push de
        push ix
        call refill_input
        pop ix
        pop de
        pop bc
        ld hl,(inLeft)
        ld a,h
        or l
        jr nz,.take
        ld a,(inflErr)
        or a
        jr nz,.done             ; keep the first failure
        ld a,IERR_EOF
        ld (inflErr),a
.done
        xor a
        ret
.take
        dec hl
        ld (inLeft),hl
        ld hl,(inPtr)
        ld a,(hl)
        inc hl
        ld (inPtr),hl
        ret


; refill_input: pull the next chunk of compressed data into inBuf
refill_input
        ld hl,(srcRemLo)
        ld de,(srcRemHi)
        ld a,h
        or l
        or d
        or e
        ret z                   ; nothing left in this member

        ld a,d
        or e
        jr nz,.full
        ld de,IN_BUF_SIZE
        or a
        sbc hl,de
        jr nc,.full
        ld hl,(srcRemLo)        ; the tail is shorter than the buffer
        jr .have
.full
        ld hl,IN_BUF_SIZE
.have
        ld (inChunk),hl

        ; staged through a spare data page rather than read straight
        ; into inBuf: DOS pages the destination bank in while it works,
        ; and that bank is the one this code is executing from
        ld a,IN_PAGE_IX
        call data_page_no
        ld hl,0
        ld de,(inChunk)
        call read_source
        jr nc,.ok
        ld a,IERR_READ
        ld (inflErr),a
        ret
.ok
        ld a,IN_PAGE_IX
        call data_page_no
        ld a,c
        nextreg $57,a
        ld hl,$E000
        ld de,inBuf
        ld bc,(inChunk)
        ldir
        ld hl,inBuf
        ld (inPtr),hl
        ld hl,(inChunk)
        ld (inLeft),hl
        ld hl,(srcOffLo)
        ld de,(srcOffHi)
        ld bc,(inChunk)
        call add32_16
        ld (srcOffLo),hl
        ld (srcOffHi),de
        ld hl,(srcRemLo)
        ld de,(srcRemHi)
        ld bc,(inChunk)
        call sub32_16
        ld (srcRemLo),hl
        ld (srcRemHi),de
        ld a,255
        ld (mappedPage),a       ; the read paged our own bank in and out
        ret


; ================================================================
; Output window
; ================================================================

; map_window: HL = window offset -> HL = address, with the page holding
; it mapped at $E000. The remap is skipped when the page is already
; there, which is what keeps a match copy cheap. Clobbers A, DE.
map_window
        ld a,h
        rlca
        rlca
        rlca
        and 7                   ; A = offset / 8192
        ld e,a
        ld a,(mappedPage)
        cp e
        jr z,.ready
        ld a,e
        ld (mappedPage),a
        push hl
        ld l,a
        ld h,0
        ld de,(dataPagesPtr)
        add hl,de
        ld a,(hl)
        nextreg $57,a
        pop hl
.ready
        ld a,h
        and $1F
        or $E0
        ld h,a
        ret


; out_byte: append A to the output ring, flushing a page to the file
; whenever one has filled. The directory says how long the file is, so
; outLeft counts down and stops a runaway stream producing for ever.
out_byte
        ld c,a
        ld hl,(outLeftLo)
        ld de,(outLeftHi)
        ld a,h
        or l
        or d
        or e
        jr nz,.room
        ld a,IERR_OVER
        ld (inflErr),a
        ret
.room
        ld hl,(outTotalLo)
        ld a,h
        and WIN_MASK_HI
        ld h,a
        call map_window
        ld (hl),c
        ld a,c
        call crc_byte           ; done here, while the byte is still in C

        ld hl,(outLeftLo)
        ld de,(outLeftHi)
        ld bc,1
        call sub32_16
        ld (outLeftLo),hl
        ld (outLeftHi),de
        ld hl,(outTotalLo)
        ld de,(outTotalHi)
        ld bc,1
        call add32_16
        ld (outTotalLo),hl
        ld (outTotalHi),de

        ; a whole page of new output means one page can go to the file
        ld hl,(outTotalLo)
        ld de,(flushedLo)
        or a
        sbc hl,de               ; the gap never exceeds one page
        ld de,FLUSH_SIZE
        or a
        sbc hl,de
        ret c
        ld hl,FLUSH_SIZE
        jp flush_window


; flush_window: hand HL bytes of the ring, starting at the flush point,
; to the open output file. A big member takes long enough that this is
; also where the byte counter is refreshed and BREAK is looked for.
flush_window
        ld (flushLen),hl
        ld hl,(flushedLo)
        ld a,h
        and WIN_MASK_HI
        ld h,a
        ex de,hl                ; DE = offset inside the data pages
        ld bc,(flushLen)
        call call_write_chunk
        jr c,.failed
        ld hl,(flushedLo)
        ld de,(flushedHi)
        ld bc,(flushLen)
        call add32_16
        ld (flushedLo),hl
        ld (flushedHi),de
        call show_written
        call poll_abort
        ld a,255
        ld (mappedPage),a       ; a host service may have moved MMU7
        ret nc
        ld a,IERR_ABORT
        ld (inflErr),a
        ret
.failed
        ld a,IERR_WRITE
        ld (inflErr),a
        ret


; show_written: bytes handed to the file so far, and compressed bytes
; still unread. On its own row, so a failure message cannot cover it up:
; together the two numbers say how far a failed export actually got.
show_written
        ld de,strWroteLbl
        ld hl,42*256+(CONTENT_ROW+4)
        ld a,ATTR_PACKED
        call call_print
        ld hl,flushedLo
        call num32_to_buf
        ld de,numBuf
        ld hl,44*256+(CONTENT_ROW+4)
        ld a,ATTR_PACKED
        call call_print
        ld de,strLeftLbl
        ld hl,53*256+(CONTENT_ROW+4)
        ld a,ATTR_PACKED
        call call_print
        ld hl,srcRemLo
        call num32_to_buf
        ld de,numBuf
        ld hl,55*256+(CONTENT_ROW+4)
        ld a,ATTR_PACKED
        jp call_print


; poll_abort: carry set when BREAK has been pressed. Checked once per
; flushed page, which is often enough to feel responsive and rare
; enough to cost nothing.
poll_abort
        call call_input
        cp 1
        jr z,.stop
        or a
        ret
.stop
        ld (abortFlag),a        ; A is 1 here
        scf
        ret


; flush_tail: push out whatever is left after the last full page
flush_tail
        ld hl,(outTotalLo)
        ld de,(flushedLo)
        or a
        sbc hl,de
        ld a,h
        or l
        ret z
        jp flush_window


; crc_byte: fold A into the running CRC32. The bitwise form needs no
; table, and the time it costs is small next to the decoding itself.
crc_byte
        ld hl,(crcLo)
        xor l
        ld l,a
        ld de,(crcHi)
        ld b,8
.loop
        srl d
        rr e
        rr h
        rr l
        jr nc,.next
        ld a,d
        xor $ED
        ld d,a
        ld a,e
        xor $B8
        ld e,a
        ld a,h
        xor $83
        ld h,a
        ld a,l
        xor $20
        ld l,a
.next
        djnz .loop
        ld (crcLo),hl
        ld (crcHi),de
        ret


; copy_match: BC = length, DE = distance back into the window
copy_match
        ; the reference must not reach back before the start of the file
        ld hl,(outTotalHi)
        ld a,h
        or l
        jr nz,.in_range         ; past 64K of output it never can
        ld hl,(outTotalLo)
        or a
        sbc hl,de
        jr c,.bad_dist
.in_range
        ld hl,(outTotalLo)
        or a
        sbc hl,de
        ld a,h
        and WIN_MASK_HI
        ld h,a
        ld (matchSrc),hl        ; ring offset of the first byte
.loop
        ld a,b
        or c
        ret z
        push bc
        ld hl,(matchSrc)
        call map_window
        ld a,(hl)
        push af
        ld hl,(matchSrc)
        inc hl
        ld a,h
        and WIN_MASK_HI
        ld h,a
        ld (matchSrc),hl
        pop af
        call out_byte
        pop bc
        ld a,(inflErr)
        or a
        ret nz
        dec bc
        jr .loop
.bad_dist
        ld a,IERR_DIST
        ld (inflErr),a
        ret


; ================================================================
; Deflate constants
; ================================================================
lenBase
        defw 3,4,5,6,7,8,9,10,11,13,15,17,19,23,27,31,35,43,51,59
        defw 67,83,99,115,131,163,195,227,258
lenExtra
        defb 0,0,0,0,0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3
        defb 4,4,4,4,5,5,5,5,0
distBase
        defw 1,2,3,4,5,7,9,13,17,25,33,49,65,97,129,193,257,385,513,769
        defw 1025,1537,2049,3073,4097,6145,8193,12289,16385,24577
distExtra
        defb 0,0,0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8
        defb 9,9,10,10,11,11,12,12,13,13
clOrder
        defb 16,17,18,0,8,7,9,6,10,5,11,4,12,3,13,2,14,1,15


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
        ld l,(ix+SERVICE_READ_AT)
        ld h,(ix+SERVICE_READ_AT+1)
        ld (call_read_at+1),hl
        ld l,(ix+SERVICE_WRITE_OPEN)
        ld h,(ix+SERVICE_WRITE_OPEN+1)
        ld (call_write_open+1),hl
        ld l,(ix+SERVICE_WRITE_CHUNK)
        ld h,(ix+SERVICE_WRITE_CHUNK+1)
        ld (call_write_chunk+1),hl
        ld l,(ix+SERVICE_WRITE_CLOSE)
        ld h,(ix+SERVICE_WRITE_CLOSE+1)
        ld (call_write_close+1),hl
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

; call_read_at: C=page, HL=offset in page, DE=count
call_read_at
        call 0
        ret

; call_write_open: HL=output file name
call_write_open
        call 0
        ret

; call_write_chunk: DE=offset in the data pages, BC=count
call_write_chunk
        call 0
        ret

call_write_close
        call 0
        ret


; ================================================================
; Variables
; ================================================================
ctxPtr          defw 0
svcPtr          defw 0
dataPagesPtr    defw 0
fileSizeLo      defw 0
fileSizeHi      defw 0

zipError        defb 0
zipEntries      defw 0      ; entry count the EOCD claims
cdSizeLo        defw 0
cdSizeHi        defw 0
cdOffLo         defw 0
cdOffHi         defw 0
cdLen           defw 0      ; how much of the directory we actually hold
cdTrunc         defb 0
cdPageIx        defb 0
cdChunkOff      defw 0
cdChunkLen      defw 0
cdPtr           defw 0
skipName        defw 0
skipExtra       defw 0
scanLen         defw 0
srcOffLo        defw 0
srcOffHi        defw 0
dwLow           defw 0

totalFiles      defb 0
topEntry        defb 0
curEntry        defb 0
curAttr         defb ATTR_NORMAL
renderIdx       defb 0
renderRow       defb 0
screenPos       defw 0

entryBase       defw 0
entryFlags      defw 0
entryMethod     defw 0
entryCrcLo      defw 0
entryCrcHi      defw 0
entryCompLo     defw 0
entryCompHi     defw 0
entryUncLo      defw 0
entryUncHi      defw 0
entryLocalLo    defw 0
entryLocalHi    defw 0
entryKind       defb 0
nameShort       defb 0
baseOff         defw 0
baseLen         defb 0
isDir           defb 0
scanIdx         defb 0
stemLen         defb 0
extLen          defb 0
walkPtr         defw 0
dstPtr          defw 0
dataOffLo       defw 0
dataOffHi       defw 0

numBuf          defs 9      ; 8 digits + null
progressBuf     defs 8      ; "nnn/nnn" + null
nameBuf         defs DISP_NAME+2
nameIdx         defb 0
extractName     defs 16     ; output filename, NUL terminated
extractIdx      defb 0
extractStatus   defb 0
bulkIdx         defb 0
okCount         defb 0
skipCount       defb 0

; ================================================================
; Strings
; ================================================================
strZip          defb "ZIP: ",0
strFiles        defb "files",0
strColHdr       defb " ## Name                Size   Packed M",0
strTrunc        defb "LISTING INCOMPLETE",0
strHelp         defb " BREAK=exit / stop export  Up/Dn  PgUp/PgDn  e:export  CAPS+e:all",0
strEmpty        defb "Archive is empty.",0
strErrNoEocd    defb "Not a readable ZIP archive.",0
strErrZip64     defb "ZIP64 archives are not supported.",0
strErrRead      defb "Cannot read the archive.",0
strKindStored   defb "STORED  ",0
strKindDir      defb "DIR     ",0
strKindCrypt    defb "CRYPTED ",0
strKindPacked   defb "DEFLATE ",0
strKindMethod   defb "METHOD? ",0
strNumName      defb "FILE000.BIN",0
; result strings are padded to cover the whole "WROTE: nnnnnnnn" line
; they replace, otherwise stale digits stay behind
strExportOK     defb "OK             ",0
strExportFail   defb "FAIL "
strExportFailCode defb "?         ",0
strWorking      defb "WORKING...     ",0
strWroteLbl     defb "W:",0
strLeftLbl      defb "R:",0
strAborted      defb "STOPPED        ",0
strBusyAll      defb "EXPORTING ALL FILES - PLEASE WAIT...",0
strBusyOne      defb "EXPORTING - PLEASE WAIT...",0
strFileLabel    defb "FILE:",0
strExportName   defb "NAME:",0
strExportLen    defb "LEN:",0
strExportType   defb "TYPE:",0
strAllExported  defb "EXPORTED:",0
strAllSkipped   defb "SKIPPED:",0
strExportAllFail defb "ALL FAIL "
strExportAllFailCode defb "?",0
strDebugBlank   defb "                                ",0

; ---- inflate state ----
inflErr         defb 0
lastBlock       defb 0
blockLen        defw 0
blockNLen       defw 0
bitBuf          defb 0
bitCnt          defb 0
inPtr           defw 0
inLeft          defw 0
inChunk         defw 0
srcRemLo        defw 0
srcRemHi        defw 0
outTotalLo      defw 0
outTotalHi      defw 0
outLeftLo       defw 0
outLeftHi       defw 0
flushedLo       defw 0
flushedHi       defw 0
flushLen        defw 0
opCode          defb 0
abortFlag       defb 0
mappedPage      defb 255
matchSrc        defw 0
matchLen        defw 0
extraVal        defw 0
lenIdx          defb 0
distIdx         defb 0
crcLo           defw 0
crcHi           defw 0
nLen            defw 0
nDist           defw 0
nCLen           defw 0
nTotal          defw 0
lenIx           defw 0
repVal          defb 0
conCount        defw 0
conSym          defw 0
conN            defw 0
conPtr          defw 0
decCount        defw 0
decSym          defw 0
decCode         defw 0
decFirst        defw 0
decIndex        defw 0
decCnt          defw 0

; ---- inflate tables ----
; count[] and symbol[] per alphabet, the code lengths they are built
; from, and the running offsets construct needs while sorting symbols.
litCount        defs (MAXBITS+1)*2
litSymbol       defs NSYMS*2
distCount       defs (MAXBITS+1)*2
distSymbol      defs NDIST*2
offsArr         defs (MAXBITS+1)*2
lengths         defs NLENGTHS
inBuf           defs IN_BUF_SIZE

; ================================================================
; Entry offset table: where each directory record starts inside the
; copy held in the data pages
; ================================================================
entryOffsets    defs MAX_ENTRIES*2

plugin_end
        assert plugin_end - plugin_start <= VIEW_PLUGIN_BIG_SIZE
        SAVEBIN "plugin/zip.ccp", VIEW_PLUGIN_ADDRESS, VIEW_PLUGIN_BIG_SIZE
