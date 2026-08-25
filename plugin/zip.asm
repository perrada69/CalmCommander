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
EOCD_SCAN       equ 8192        ; how much of the tail is searched for the EOCD
EOCD_MIN        equ 22          ; an EOCD without a comment
LOCAL_HDR       equ 30
DISP_NAME       equ 15          ; width of the name column

; entryKind values
KIND_STORED     equ 0           ; ready to be written out
KIND_DIR        equ 1
KIND_CRYPT      equ 2
KIND_PACKED     equ 3
KIND_BIG        equ 4           ; over 64KB: one extract call cannot span it

; zipError values
ZERR_NONE       equ 0
ZERR_NOEOCD     equ 1
ZERR_ZIP64      equ 2
ZERR_READ       equ 3


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
        call show_busy_one
        call do_extract_current
        call restore_help
        jp .wait_release

; ---- extract every stored entry ----
.extract_all
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
; cursor. Only stored members can be written out at the moment; deflate
; needs an inflate engine, which is the next step for this plugin.
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
        or l
        ld a,KIND_PACKED
        jr nz,.set
        ld hl,(entryCompHi)
        ld a,h
        or l
        ld a,KIND_BIG
        jr nz,.set
        xor a               ; stored and under 64KB: extractable
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
        ld de,strKindBig
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
        or a
        jp nz,.unsupported

        ld de,strWorking
        ld hl,42*256+(CONTENT_ROW+4)
        ld a,ATTR_PACKED
        call call_print

        ld hl,(entryLocalLo)
        ld (srcOffLo),hl
        ld hl,(entryLocalHi)
        ld (srcOffHi),hl
        call scratch_page_no
        ld hl,0
        ld de,LOCAL_HDR
        call read_source
        jp c,.io_fail
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

        ld ix,(ctxPtr)
        ld a,$FF
        ld (ix+VIEWCTX_P3DOS_TYPE),a    ; a ZIP member is a raw file
        ld hl,(dataOffHi)
        ld (ix+VIEWCTX_EXTRACT_OFHI),l
        ld (ix+VIEWCTX_EXTRACT_OFHI+1),h
        ld hl,extractName
        ld de,(dataOffLo)
        ld bc,(entryCompLo)
        call call_extract_seek
        jr c,.fail
        xor a
        ld (extractStatus),a
        ld de,strExportOK
        jr .report
.fail
        ld (extractStatus),a
        jr .fail_code
.io_fail
        ld a,7
        ld (extractStatus),a
        jr .fail_code
.bad_local
        ld a,8
        ld (extractStatus),a
.fail_code
        add a,"0"
        ld (strExportFailCode),a
        ld de,strExportFail
        jr .report
.unsupported
        ld a,255
        ld (extractStatus),a
        call kind_text
.report
        ld hl,42*256+(CONTENT_ROW+4)
        ld a,ATTR_PACKED
        jp call_print


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
        or a
        jr nz,.skip

        ld a,(bulkIdx)
        call do_extract_entry
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

.failed
        add a,"0"
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
        ld l,(ix+SERVICE_READ_AT)
        ld h,(ix+SERVICE_READ_AT+1)
        ld (call_read_at+1),hl
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

; call_read_at: C=page, HL=offset in page, DE=count
call_read_at
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
strHelp         defb " BREAK=exit  Up/Dn  PgUp/PgDn  e:export  CAPS+e:export all stored",0
strEmpty        defb "Archive is empty.",0
strErrNoEocd    defb "Not a readable ZIP archive.",0
strErrZip64     defb "ZIP64 archives are not supported.",0
strErrRead      defb "Cannot read the archive.",0
strKindStored   defb "STORED  ",0
strKindDir      defb "DIR     ",0
strKindCrypt    defb "CRYPTED ",0
strKindPacked   defb "DEFLATE ",0
strKindBig      defb "TOO BIG ",0
strNumName      defb "FILE000.BIN",0
; result strings are padded so they cover the "WORKING..." they replace
strExportOK     defb "OK        ",0
strExportFail   defb "FAIL "
strExportFailCode defb "?    ",0
strWorking      defb "WORKING...",0
strBusyAll      defb "EXPORTING STORED FILES - PLEASE WAIT...",0
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

; ================================================================
; Entry offset table: where each directory record starts inside the
; copy held in the data pages
; ================================================================
entryOffsets    defs MAX_ENTRIES*2

plugin_end
        assert plugin_end - plugin_start <= VIEW_PLUGIN_BIG_SIZE
        SAVEBIN "plugin/zip.ccp", VIEW_PLUGIN_ADDRESS, VIEW_PLUGIN_BIG_SIZE
