        DEVICE ZXSPECTRUMNEXT
        org BOOKMARK_PLUGIN_ADDRESS

        include "bookmarks_api.i.asm"

F_OPEN      equ $9A
F_CLOSE     equ $9B
F_READ      equ $9D
F_WRITE     equ $9E
F_SEEK      equ $9F
F_OPENDIR   equ $A3
F_READDIR   equ $A4

MODE_READ_EXIST  equ $01
MODE_WRITE_CREATE equ $0A
MODE_LFN_DIR     equ $10
SEEK_SET         equ 0

PLUGIN_STACK     equ $DFFE
WORK_AREA        equ $E000
RECORD_BUFFER    equ WORK_AREA
LINE_BUFFER      equ RECORD_BUFFER+BOOKMARK_RECORD_SIZE
NAME_BUFFER      equ LINE_BUFFER+72
LFN_PATH_BUFFER  equ $E1A0
TRAVERSE_PATH    equ $E2A8
COMPONENT_BUFFER equ $E3B0
SHORT_ENTRY      equ $E3C0
LFN_ENTRY        equ $E420

BOX_X            equ 4
BOX_Y            equ 3
BOX_WIDTH        equ 72
BOX_HEIGHT       equ 25
LIST_X           equ BOX_X+2
LIST_Y           equ BOX_Y+4
VISIBLE_ROWS     equ 16
LIST_WIDTH       equ BOX_WIDTH-4

ADD_X            equ 8
ADD_Y            equ 8
ADD_WIDTH        equ 64
ADD_HEIGHT       equ 10
NAME_INPUT_X     equ ADD_X+18
NAME_INPUT_Y     equ ADD_Y+7
NAME_INPUT_WIDTH equ BOOKMARK_NAME_SIZE-1

plugin_start
        ld (ctxPtr),hl
        ld (svcPtr),de
        ld (savedSp),sp
        ld sp,PLUGIN_STACK
        call patch_services
        ld ix,(ctxPtr)
        xor a
        ld (ix+BOOKMARKCTX_RESULT),a
        ld (ix+BOOKMARKCTX_ERROR),a
        ld a,(ix+BOOKMARKCTX_ABI)
        cp BOOKMARK_ABI
        jr nz,.bad_abi
        ld a,(ix+BOOKMARKCTX_MODE)
        cp BOOKMARK_MODE_ADD
        call z,bookmark_add
        ld ix,(ctxPtr)
        ld a,(ix+BOOKMARKCTX_MODE)
        cp BOOKMARK_MODE_LIST
        call z,bookmark_list
.done
        ld sp,(savedSp)
        ret
.bad_abi
        ld a,$7f
        ld (ix+BOOKMARKCTX_ERROR),a
        jr .done


; -----------------------------------------------------------------------------
; Add the active panel path under a short user supplied name.
; -----------------------------------------------------------------------------
bookmark_add
        call draw_add_box
        ld b,ADD_X+2
        ld c,ADD_Y+1
        ld hl,titleAdd
        ld a,16
        call plot_string
        ld b,ADD_X+2
        ld c,ADD_Y+3
        ld hl,promptPath
        ld a,16
        call plot_string
        call prepare_current_path_line
        ld b,ADD_X+2
        ld c,ADD_Y+4
        ld hl,LINE_BUFFER
        ld a,16
        call plot_string
        ld b,ADD_X+2
        ld c,ADD_Y+7
        ld hl,promptName
        ld a,16
        call plot_string
        call draw_name_input_field
        ld b,ADD_X+2
        ld c,ADD_Y+ADD_HEIGHT-2
        ld hl,addHint
        ld a,16
        call plot_string
        call edit_name
        ret c
        call migrate_bookmark_file
        jp c,file_error

        ld hl,RECORD_BUFFER
        ld de,RECORD_BUFFER+1
        ld bc,BOOKMARK_RECORD_SIZE-1
        xor a
        ld (hl),a
        ldir
        ld hl,NAME_BUFFER
        ld de,RECORD_BUFFER
        ld bc,BOOKMARK_NAME_SIZE
        ldir
        ld ix,(ctxPtr)
        ld a,(ix+BOOKMARKCTX_DRIVE)
        ld (RECORD_BUFFER+BOOKMARK_NAME_SIZE),a
        ld l,(ix+BOOKMARKCTX_PATH)
        ld h,(ix+BOOKMARKCTX_PATH+1)
        ld de,RECORD_BUFFER+BOOKMARK_NAME_SIZE+1
        ld bc,BOOKMARK_PATH_SIZE
        ldir

        xor a
        ld ix,bookmarkFile
        ld b,MODE_WRITE_CREATE
        rst $08
        db F_OPEN
        jr c,file_error
        ld (fileHandle),a

        ld bc,$ffff
        ld de,$ffff
        ld ixl,SEEK_SET
        rst $08
        db F_SEEK
        jp c,.close_error
        ld a,b
        or c
        jr nz,.limit
        ex de,hl
        ld de,BOOKMARK_RECORD_SIZE*BOOKMARK_MAX_COUNT
        or a
        sbc hl,de
        jr nc,.limit

        ld a,(fileHandle)
        ld ix,RECORD_BUFFER
        ld bc,BOOKMARK_RECORD_SIZE
        rst $08
        db F_WRITE
        jr c,.close_error
        ld a,(fileHandle)
        rst $08
        db F_CLOSE
        ret

.limit
        ld a,(fileHandle)
        rst $08
        db F_CLOSE
        ld hl,msgLimit
        jp show_message
.close_error
        push af
        ld a,(fileHandle)
        rst $08
        db F_CLOSE
        pop af
file_error
        ld ix,(ctxPtr)
        ld (ix+BOOKMARKCTX_ERROR),a
        ld hl,msgFileError
        jp show_message


; Upgrade the original 12-character records in place. Work from the final
; record backwards so extending a record never overwrites one not read yet.
migrate_bookmark_file
        xor a
        ld ix,bookmarkFile
        ld b,MODE_READ_EXIST
        rst $08
        db F_OPEN
        jr nc,.opened
        or a                                      ; a missing file needs no migration
        ret
.opened
        ld (fileHandle),a
        ld bc,$ffff
        ld de,$ffff
        ld ixl,SEEK_SET
        rst $08
        db F_SEEK
        jp c,migration_close_error
        ld a,b
        or c
        jp nz,migration_close_ok                  ; outside the supported <64K file size
        ld (migrationSize),de
        ld a,(fileHandle)
        rst $08
        db F_CLOSE
        ret c

        ld hl,(migrationSize)
        ld de,BOOKMARK_RECORD_SIZE
        call size_is_multiple
        ret z                                     ; already current
        ld hl,(migrationSize)
        ld de,BOOKMARK_OLD_RECORD_SIZE
        call size_is_multiple
        jp nz,.bad_format

        ld hl,(migrationSize)
        xor a
        ld (migrationCount),a
.count_old
        ld a,h
        or l
        jr z,.count_ready
        ld de,BOOKMARK_OLD_RECORD_SIZE
        or a
        sbc hl,de
        jp c,.bad_format
        ld a,(migrationCount)
        inc a
        ld (migrationCount),a
        jr .count_old
.count_ready
        ld a,(migrationCount)
        or a
        ret z
        ld a,(migrationCount)
        dec a
        ld (migrationIndex),a
.record_loop
        ld a,(migrationIndex)
        call migration_read_old_record
        ret c

        ld hl,RECORD_BUFFER+BOOKMARK_OLD_RECORD_SIZE-1
        ld de,RECORD_BUFFER+BOOKMARK_RECORD_SIZE-1
        ld bc,BOOKMARK_OLD_RECORD_SIZE-BOOKMARK_OLD_NAME_SIZE
        lddr
        ld hl,RECORD_BUFFER+BOOKMARK_OLD_NAME_SIZE
        ld de,RECORD_BUFFER+BOOKMARK_OLD_NAME_SIZE+1
        ld bc,BOOKMARK_NAME_SIZE-BOOKMARK_OLD_NAME_SIZE-1
        xor a
        ld (hl),a
        ldir

        ld a,(migrationIndex)
        call migration_write_new_record
        ret c
        ld a,(migrationIndex)
        or a
        ret z
        dec a
        ld (migrationIndex),a
        jr .record_loop
.bad_format
        ld a,$ff
        scf
        ret


; NextZXOS does not accept the old combined read/write open mode here. Open
; the file separately for every read and write; backwards processing still
; guarantees that extending a record cannot overwrite unread source data.
migration_read_old_record
        call old_record_offset
        ld (migrationOffset),hl
        xor a
        ld ix,bookmarkFile
        ld b,MODE_READ_EXIST
        rst $08
        db F_OPEN
        ret c
        ld (fileHandle),a
        ld hl,(migrationOffset)
        call migration_seek
        jp c,migration_close_error
        ld a,(fileHandle)
        ld ix,RECORD_BUFFER
        ld bc,BOOKMARK_OLD_RECORD_SIZE
        rst $08
        db F_READ
        jp c,migration_close_error
        ld a,b
        or c
        jr nz,migration_short_io
        jp migration_close_ok


migration_write_new_record
        call record_offset
        ld (migrationOffset),hl
        xor a
        ld ix,bookmarkFile
        ld b,MODE_WRITE_CREATE
        rst $08
        db F_OPEN
        ret c
        ld (fileHandle),a
        ld hl,(migrationOffset)
        call migration_seek
        jp c,migration_close_error
        ld a,(fileHandle)
        ld ix,RECORD_BUFFER
        ld bc,BOOKMARK_RECORD_SIZE
        rst $08
        db F_WRITE
        jp c,migration_close_error
        ld a,b
        or c
        jr nz,migration_short_io
        jp migration_close_ok


migration_short_io
        ld a,$ff
migration_close_error
        push af
        ld a,(fileHandle)
        rst $08
        db F_CLOSE
        pop af
        scf
        ret
migration_close_ok
        ld a,(fileHandle)
        rst $08
        db F_CLOSE
        ret


; Z when HL is an exact multiple of DE.
size_is_multiple
.loop
        ld a,h
        or l
        ret z
        or a
        sbc hl,de
        jr nc,.loop
        ld a,1
        or a
        ret


old_record_offset
        ld hl,0
        or a
        ret z
        ld b,a
.loop
        ld de,BOOKMARK_OLD_RECORD_SIZE
        add hl,de
        djnz .loop
        ret


migration_seek
        ex de,hl
        ld bc,0
        ld a,(fileHandle)
        ld ixl,SEEK_SET
        rst $08
        db F_SEEK
        ret


; -----------------------------------------------------------------------------
; Open, count and browse the fixed-size records.
; -----------------------------------------------------------------------------
bookmark_list
        call migrate_bookmark_file
        jp c,file_error
        xor a
        ld ix,bookmarkFile
        ld b,MODE_READ_EXIST
        rst $08
        db F_OPEN
        jp c,.empty
        ld (fileHandle),a
        ld bc,$ffff
        ld de,$ffff
        ld ixl,SEEK_SET
        rst $08
        db F_SEEK
        jp c,.close_error

        ld a,b
        or c
        jr z,.small_file
        ld a,BOOKMARK_MAX_COUNT
        jr .count_ready
.small_file
        ex de,hl
        xor a
.count_loop
        cp BOOKMARK_MAX_COUNT
        jr z,.count_ready
        push af
        ld de,BOOKMARK_RECORD_SIZE
        or a
        sbc hl,de
        jr c,.count_short
        pop af
        inc a
        jr .count_loop
.count_short
        pop af
.count_ready
        ld (recordCount),a
        or a
        jr z,.close_empty
        xor a
        ld (cursor),a
        ld (topIndex),a
        call draw_list_screen

.input
        call read_key
        cp 1
        jr z,.cancel
        cp 13
        jr z,.select
        cp 11
        jr z,.up
        cp 10
        jr z,.down
        cp 8
        jr z,.page_up
        cp 9
        jr z,.page_down
        jr .input
.up
        call cursor_up
        call draw_rows
        jr .input
.down
        call cursor_down
        call draw_rows
        jr .input
.page_up
        ld b,VISIBLE_ROWS
.page_up_loop
        call cursor_up
        djnz .page_up_loop
        call draw_rows
        jr .input
.page_down
        ld b,VISIBLE_ROWS
.page_down_loop
        push bc
        call cursor_down
        pop bc
        djnz .page_down_loop
        call draw_rows
        jr .input

.select
        ld a,(cursor)
        call read_record
        jr c,.cancel
        ld ix,(ctxPtr)
        ld a,(RECORD_BUFFER+BOOKMARK_NAME_SIZE)
        ld (ix+BOOKMARKCTX_RESULT_DRIVE),a
        ld e,(ix+BOOKMARKCTX_RESULT_PATH)
        ld d,(ix+BOOKMARKCTX_RESULT_PATH+1)
        ld hl,RECORD_BUFFER+BOOKMARK_NAME_SIZE+1
        ld bc,BOOKMARK_PATH_SIZE
        ldir
        ld a,1
        ld (ix+BOOKMARKCTX_RESULT),a
.cancel
        ld a,(fileHandle)
        rst $08
        db F_CLOSE
        ret

.close_empty
        ld a,(fileHandle)
        rst $08
        db F_CLOSE
.empty
        ld hl,msgEmpty
        jp show_message
.close_error
        push af
        ld a,(fileHandle)
        rst $08
        db F_CLOSE
        pop af
        jp file_error


read_record
        call record_offset
        ex de,hl
        ld bc,0
        ld a,(fileHandle)
        ld ixl,SEEK_SET
        rst $08
        db F_SEEK
        ret c
        ld a,(fileHandle)
        ld ix,RECORD_BUFFER
        ld bc,BOOKMARK_RECORD_SIZE
        rst $08
        db F_READ
        ret c
        ld a,b
        or c
        ret z
        or a
        ret

record_offset
        ld hl,0
        or a
        ret z
        ld b,a
.loop
        ld de,BOOKMARK_RECORD_SIZE
        add hl,de
        djnz .loop
        ret


cursor_up
        ld a,(cursor)
        or a
        ret z
        dec a
        ld (cursor),a
        ld hl,topIndex
        cp (hl)
        ret nc
        ld (hl),a
        ret

cursor_down
        ld a,(recordCount)
        ld b,a
        ld a,(cursor)
        inc a
        cp b
        ret nc
        ld (cursor),a
        ld b,a
        ld a,(topIndex)
        add a,VISIBLE_ROWS
        cp b
        jr z,.scroll
        jr c,.scroll
        ret
.scroll
        ld a,b
        sub VISIBLE_ROWS-1
        ld (topIndex),a
        ret


; -----------------------------------------------------------------------------
; UI through the host ABI, so borders, attributes and text match Calm Commander.
; -----------------------------------------------------------------------------
draw_box
        ld hl,BOX_X*256+BOX_Y
        ld bc,BOX_WIDTH*256+BOX_HEIGHT
        ld a,16
        jp call_window

draw_add_box
        ld hl,ADD_X*256+ADD_Y
        ld bc,ADD_WIDTH*256+ADD_HEIGHT
        ld a,16
        jp call_window


; Paint the whole editable width independently of the current text. This
; keeps all 24 available character cells visible, including unused cells.
draw_name_input_field
        ld hl,LINE_BUFFER
        ld de,LINE_BUFFER+1
        ld bc,NAME_INPUT_WIDTH-1
        ld (hl),' '
        ldir
        xor a
        ld (LINE_BUFFER+NAME_INPUT_WIDTH),a
        ld b,NAME_INPUT_X
        ld c,NAME_INPUT_Y
        ld hl,LINE_BUFFER
        ld a,80
        jp plot_string

draw_list_screen
        call draw_box
        ld b,BOX_X+2
        ld c,BOX_Y+1
        ld hl,titleList
        ld a,16
        call plot_string
        ld b,LIST_X
        ld c,BOX_Y+3
        ld hl,listHeader
        ld a,16
        call plot_string
        ld b,LIST_X
        ld c,BOX_Y+BOX_HEIGHT-2
        ld hl,listHint
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
        ld hl,recordCount
        cp (hl)
        jr nc,.plot
        call read_record
        jr c,.plot

        ld a,(cursor)
        ld b,a
        ld a,(drawIndex)
        cp b
        jr nz,.name
        ld a,'>'
        ld (LINE_BUFFER),a
.name
        ld hl,RECORD_BUFFER
        ld de,LINE_BUFFER+2
        ld b,24
        call copy_field
        ld hl,RECORD_BUFFER+BOOKMARK_NAME_SIZE+1
        ld a,(RECORD_BUFFER+BOOKMARK_NAME_SIZE)
        call resolve_lfn_path
        ld hl,LFN_PATH_BUFFER
        ld de,LINE_BUFFER+27
        ld b,LIST_WIDTH-27
        call copy_field
.plot
        ld b,LIST_X
        ld a,(visibleRow)
        add a,LIST_Y
        ld c,a
        ld hl,LINE_BUFFER
        ld a,16
        jp plot_string

copy_field
        ld a,(hl)
        or a
        ret z
        cp 255
        ret z
        ld (de),a
        inc hl
        inc de
        djnz copy_field
        ret


prepare_current_path_line
        ld ix,(ctxPtr)
        ld a,(ix+BOOKMARKCTX_DRIVE)
        ld l,(ix+BOOKMARKCTX_PATH)
        ld h,(ix+BOOKMARKCTX_PATH+1)
        ld a,(ix+BOOKMARKCTX_DRIVE)
        call resolve_lfn_path
        ld hl,LFN_PATH_BUFFER
        ld de,LINE_BUFFER
        ld b,ADD_WIDTH-4
        call copy_field
        xor a
        ld (de),a
        ret


; Convert a DOS 8.3 directory path to its display-only LFN form.  Navigation
; deliberately keeps using the original stored path.  For each component the
; parent directory is opened in short and LFN modes; NextZXOS returns the same
; directory entries in the same order, allowing the short alias to be paired
; with its long name without changing Calm Commander's current directory.
; IN: A=drive letter, HL=zero/255 terminated 8.3 path.
; OUT: LFN_PATH_BUFFER=zero terminated LFN path (falls back per component).
resolve_lfn_path
        ld (resolveDrive),a
        ld (sourcePtr),hl
        ld (LFN_PATH_BUFFER),a
        ld (TRAVERSE_PATH),a
        ld a,':'
        ld (LFN_PATH_BUFFER+1),a
        ld (TRAVERSE_PATH+1),a
        ld a,'/'
        ld (LFN_PATH_BUFFER+2),a
        ld (TRAVERSE_PATH+2),a
        xor a
        ld (LFN_PATH_BUFFER+3),a
        ld (TRAVERSE_PATH+3),a
        ld hl,LFN_PATH_BUFFER+3
        ld (lfnOutPtr),hl
        ld hl,TRAVERSE_PATH+3
        ld (traverseOutPtr),hl

        ld hl,(sourcePtr)
        ld a,(resolveDrive)
        cp (hl)
        jr nz,.skip_slashes
        inc hl
        ld a,(hl)
        cp ':'
        jr nz,.undo_drive_skip
        inc hl
        jr .save_source
.undo_drive_skip
        dec hl
.save_source
        ld (sourcePtr),hl
.skip_slashes
        ld hl,(sourcePtr)
        ld a,(hl)
        cp '/'
        jr z,.consume_slash
        cp 92
        jr nz,.component
.consume_slash
        inc hl
        ld (sourcePtr),hl
        jr .skip_slashes

.component
        or a
        ret z
        cp 255
        ret z
        ld de,COMPONENT_BUFFER
        ld b,12
.copy_component
        ld a,(hl)
        or a
        jr z,.component_ready
        cp 255
        jr z,.component_ready
        cp '/'
        jr z,.component_ready
        cp 92
        jr z,.component_ready
        ld (de),a
        inc de
        inc hl
        djnz .copy_component
.component_ready
        xor a
        ld (de),a
        ld (sourcePtr),hl
        call resolve_component
        jr c,.fallback
        ld hl,LFN_ENTRY+1
        jr .append_lfn
.fallback
        ld hl,COMPONENT_BUFFER
.append_lfn
        call append_lfn_string
        ld a,'/'
        call append_lfn_char
        ld hl,COMPONENT_BUFFER
        call append_traverse_string
        ld a,'/'
        call append_traverse_char
        jr .skip_slashes


; Resolve COMPONENT_BUFFER in TRAVERSE_PATH by reading short/LFN entries in
; lockstep. Carry set means that the component could not be resolved.
resolve_component
        ld ix,TRAVERSE_PATH
        xor a
        ld b,a
        rst $08
        db F_OPENDIR
        ret c
        ld (shortHandle),a
        ld ix,TRAVERSE_PATH
        ld b,MODE_LFN_DIR
        xor a
        rst $08
        db F_OPENDIR
        jr c,.close_short_fail
        ld (lfnHandle),a
.next
        ld ix,SHORT_ENTRY
        ld a,(shortHandle)
        rst $08
        db F_READDIR
        jr c,.close_both_fail
        or a
        jr z,.close_both_fail
        ld ix,LFN_ENTRY
        ld a,(lfnHandle)
        rst $08
        db F_READDIR
        jr c,.close_both_fail
        or a
        jr z,.close_both_fail
        ld hl,COMPONENT_BUFFER
        ld de,SHORT_ENTRY+1
        call equal_filename_ci
        jr nz,.next
        call close_resolve_handles
        or a
        ret
.close_both_fail
        call close_resolve_handles
        scf
        ret
.close_short_fail
        push af
        ld a,(shortHandle)
        rst $08
        db F_CLOSE
        pop af
        scf
        ret

close_resolve_handles
        ld a,(lfnHandle)
        rst $08
        db F_CLOSE
        ld a,(shortHandle)
        rst $08
        db F_CLOSE
        ret


; Z when the two zero-terminated filenames match, ASCII case-insensitively.
equal_filename_ci
.loop
        ld a,(de)
        call upper_ascii
        ld c,a
        ld a,(hl)
        call upper_ascii
        cp c
        ret nz
        or a
        ret z
        inc hl
        inc de
        jr .loop

upper_ascii
        cp 'a'
        ret c
        cp 'z'+1
        ret nc
        sub 32
        ret


append_lfn_string
        ld de,(lfnOutPtr)
.loop
        ld a,(hl)
        or a
        jr z,.done
        cp 255
        jr z,.done
        call append_lfn_char_de
        inc hl
        jr .loop
.done
        ld (lfnOutPtr),de
        ret

append_lfn_char
        ld de,(lfnOutPtr)
        call append_lfn_char_de
        ld (lfnOutPtr),de
        ret

append_lfn_char_de
        push hl
        ld hl,LFN_PATH_BUFFER+BOOKMARK_PATH_SIZE-1
        or a
        sbc hl,de
        pop hl
        ret z
        ret c
        ld (de),a
        inc de
        xor a
        ld (de),a
        ret

append_traverse_string
        ld de,(traverseOutPtr)
.loop
        ld a,(hl)
        or a
        jr z,.done
        ld (de),a
        inc de
        inc hl
        jr .loop
.done
        ld (traverseOutPtr),de
        ret

append_traverse_char
        ld de,(traverseOutPtr)
        ld (de),a
        inc de
        xor a
        ld (de),a
        ld (traverseOutPtr),de
        ret


edit_name
        ld hl,NAME_BUFFER
        ld de,NAME_BUFFER+1
        ld bc,BOOKMARK_NAME_SIZE-1
        ld (hl),' '
        ldir
        xor a
        ld (NAME_BUFFER+BOOKMARK_NAME_SIZE-1),a
        ld (namePos),a
.redraw
        ld a,(namePos)
        cp BOOKMARK_NAME_SIZE-1
        jr nc,.draw_full
        ld e,a
        ld d,0
        ld hl,NAME_BUFFER
        add hl,de
        ld (hl),'_'
        push hl
        ld b,NAME_INPUT_X
        ld c,NAME_INPUT_Y
        ld hl,NAME_BUFFER
        ld a,80
        call plot_string
        pop hl
        ld (hl),' '
        jr .read
.draw_full
        ld b,NAME_INPUT_X
        ld c,NAME_INPUT_Y
        ld hl,NAME_BUFFER
        ld a,80
        call plot_string
.read
        call read_key
        cp 1
        jr z,.cancel
        cp 13
        jr z,.accept
        cp 12
        jr z,.backspace
        cp 32
        jr c,.read
        cp 127
        jr nc,.read
        ld b,a
        ld a,(namePos)
        cp BOOKMARK_NAME_SIZE-1
        jr nc,.read
        ld e,a
        ld d,0
        ld hl,NAME_BUFFER
        add hl,de
        ld (hl),b
        inc a
        ld (namePos),a
        jr .redraw
.backspace
        ld a,(namePos)
        or a
        jr z,.read
        dec a
        ld (namePos),a
        jr .redraw
.accept
        ld a,(namePos)
        or a
        jr z,.cancel
        ld e,a
        ld d,0
        ld hl,NAME_BUFFER
        add hl,de
        xor a
        ld (hl),a
        or a
        ret
.cancel
        scf
        ret


show_message
        push hl
        call draw_add_box
        pop hl
        ld b,ADD_X+2
        ld c,ADD_Y+3
        ld a,16
        call plot_string
        ld b,ADD_X+2
        ld c,ADD_Y+ADD_HEIGHT-2
        ld hl,msgContinue
        ld a,16
        call plot_string
        call read_key
        ret


; IN: B=x, C=y, HL=zero/255 terminated text, A=attribute.
plot_string
        ex de,hl
        ld h,b
        ld l,c
        jp call_print


patch_services
        ld ix,(svcPtr)
        ld l,(ix+BOOKMARK_SERVICE_PRINT)
        ld h,(ix+BOOKMARK_SERVICE_PRINT+1)
        ld (call_print+1),hl
        ld l,(ix+BOOKMARK_SERVICE_WINDOW)
        ld h,(ix+BOOKMARK_SERVICE_WINDOW+1)
        ld (call_window+1),hl
        ret

call_print
        jp 0

call_window
        jp 0


; Busy-style keyboard scanner copied locally so the plugin needs no resident
; service table. read_key waits for a full release between key presses and
; then gives CAPS/SYMBOL combinations two frames to settle. Without that
; delay, the first matrix scan could see the ordinary key just before the
; modifier and e.g. CAPS+0 was incorrectly returned as character "0".
read_key
.released
        call keyscan
        ld a,e
        inc a
        jr nz,.released
.pressed
        call keyscan
        ld a,e
        inc a
        jr z,.pressed
        ei
        ld b,2
.settle
        halt
        djnz .settle
        call keyscan
        ld a,e
        inc a
        jr z,.pressed
        ld a,d
        ld hl,symtab
        cp $18
        jr z,.table
        ld hl,capstab
        cp $27
        jr z,.table
        ld hl,normtab
.table
        ld d,0
        add hl,de
        ld a,(hl)
        or a
        jr z,.pressed
        ret

keyscan
        ld l,47
        ld de,65535
        ld bc,65278
.line
        in a,(c)
        cpl
        and 31
        jr z,.done
        ld h,a
        ld a,l
.three
        inc d
        ret nz
.bits
        sub 8
        srl h
        jr nc,.bits
        ld d,e
        ld e,a
        jr nz,.three
.done
        dec l
        rlc b
        jr c,.line
        ld a,d
        inc a
        ret z
        cp 40
        ret z
        cp 25
        ret z
        ld a,e
        ld e,d
        ld d,a
        cp 24
        ret

symtab db "*^[&%>}/",",-]'$<{?",".+",127,"($",200,"/ ",0,"=;)@",201,"|:",32,13,34,"_!",199,"~",0
capstab db "BHY",10,8,"TGV","NJU",11,5,"RFC","MKI",9,4,"EDX",2,"LO",15,6,"WSZ",1,13,"P",12,7,"QA"
normtab db "bhy65tgv","nju74rfc","mki83edx",0,"lo92wsz",32,13,"p01qa",0

bookmarkFile defb "c:/sys/bookmark.cfg",0
titleAdd    defb " Add bookmark",0
titleList   defb " Bookmarks",0
promptPath  defb "Directory (LFN):",0
promptName  defb "Bookmark name:",0
addHint     defb "ENTER save  BREAK cancel",0
listHeader  defb "  Name                     LFN path",0
listHint    defb "UP/DOWN move  LEFT/RIGHT page  ENTER jump  BREAK cancel",0
msgEmpty    defb "No bookmarks yet.",0
msgFileError defb "Cannot access c:/sys/bookmark.cfg.",0
msgLimit    defb "Bookmark limit reached (200).",0
msgContinue defb "Press any key",0

ctxPtr      defw 0
svcPtr      defw 0
savedSp     defw 0
fileHandle  defb 0
shortHandle defb 0
lfnHandle   defb 0
resolveDrive defb 0
sourcePtr   defw 0
lfnOutPtr   defw 0
traverseOutPtr defw 0
recordCount defb 0
cursor      defb 0
topIndex    defb 0
visibleRow  defb 0
drawIndex   defb 0
namePos     defb 0
migrationSize defw 0
migrationCount defb 0
migrationIndex defb 0
migrationOffset defw 0

plugin_end
        assert plugin_end - plugin_start <= BOOKMARK_PLUGIN_SIZE
        SAVEBIN "plugin/bookmarks.ccp", BOOKMARK_PLUGIN_ADDRESS, BOOKMARK_PLUGIN_SIZE
