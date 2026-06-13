        DEVICE ZXSPECTRUMNEXT
        org SYSCOPY_PLUGIN_ADDRESS

        include "syscopy_api.i.asm"

F_OPEN      equ $9A
F_CLOSE     equ $9B
F_READ      equ $9D
F_WRITE     equ $9E
F_OPENDIR   equ $A3
F_READDIR   equ $A4
F_TELLDIR   equ $A5
F_SEEKDIR   equ $A6
F_MKDIR     equ $AA
F_RMDIR     equ $AB
F_UNLINK    equ $AD

MODE_READ_EXIST  equ $01
MODE_WRITE_TRUNC equ $0E
MODE_LFN_DIR     equ $10
ATTR_DIR         equ $10

MAX_DEPTH        equ 11
PLUGIN_STACK     equ $DFFE

STAGE_ROOT_PATH  equ $11
STAGE_NESTED_DST equ $12
STAGE_COPY_MKDIR equ $20
STAGE_COPY_DIR   equ $21
STAGE_COPY_LFN   equ $22
STAGE_READ_DIR   equ $23
STAGE_READ_LFN   equ $24
STAGE_CHILD_PATH equ $25
STAGE_SAVE_POS   equ $26
STAGE_REOPEN_DIR equ $27
STAGE_REOPEN_LFN equ $28
STAGE_SRC_OPEN   equ $30
STAGE_DST_OPEN   equ $31
STAGE_FILE_READ  equ $32
STAGE_FILE_WRITE equ $33

SRC_STACK        equ SYSCOPY_WORK_ADDRESS          ; 12 * 256 bytes
DST_STACK        equ SYSCOPY_WORK_ADDRESS + $0C00  ; 12 * 256 bytes
DIR_ENTRY        equ SYSCOPY_WORK_ADDRESS + $1800  ; 512 bytes
LFN_ENTRY        equ SYSCOPY_WORK_ADDRESS + $1A00  ; 512 bytes
COPY_BUFFER      equ DIR_ENTRY                     ; file copy can reuse dir-entry space
COPY_BUFFER_LEN  equ 2048
STATUS_LINE_LEN  equ 45

plugin_start
        ld (ctxPtr),hl
        ld (svcPtr),de
        ld (savedSp),sp
        ld sp,PLUGIN_STACK
        call patch_services
        xor a
        ld (failStage),a

        call copy_root_paths
        jr c,.failed
        ld ix,(ctxPtr)
        ld a,(ix+SYSCOPYCTX_MODE)
        cp 2
        jr nz,.copy_or_move
        call prepare_total_count
        ld de,deletePhaseTxt
        call print_counter_status
        ld a,0
        call delete_dir
        jr c,.failed
        jr .success

.copy_or_move
        call reject_nested_destination
        jr c,.failed
        call prepare_total_count
        ld de,copyPhaseTxt
        call print_counter_status
        ld a,0
        call copy_dir
        jr c,.failed

        ld ix,(ctxPtr)
        ld a,(ix+SYSCOPYCTX_MODE)
        or a
        jr z,.success
        call prepare_total_count
        ld de,deletePhaseTxt
        call print_counter_status
        ld a,0
        call delete_dir
        jr c,.failed

.success
        ld ix,(ctxPtr)
        xor a
        ld (ix+SYSCOPYCTX_RESULT),a
        ld sp,(savedSp)
        xor a
        ret

.failed
        ld ix,(ctxPtr)
        ld (ix+SYSCOPYCTX_ERROR),a
        ld a,(failStage)
        ld (ix+SYSCOPYCTX_STAGE),a
        ld a,1
        ld (ix+SYSCOPYCTX_RESULT),a
        ld sp,(savedSp)
        ret


copy_root_paths
        ld a,STAGE_ROOT_PATH
        ld (failStage),a
        ld ix,(ctxPtr)
        ld l,(ix+SYSCOPYCTX_SRC_PATH)
        ld h,(ix+SYSCOPYCTX_SRC_PATH+1)
        ld de,SRC_STACK
        ld c,(ix+SYSCOPYCTX_NAME)
        ld b,(ix+SYSCOPYCTX_NAME+1)
        call build_path
        ret c

        ld ix,(ctxPtr)
        ld l,(ix+SYSCOPYCTX_DST_PATH)
        ld h,(ix+SYSCOPYCTX_DST_PATH+1)
        ld de,DST_STACK
        ld c,(ix+SYSCOPYCTX_LFN_NAME)
        ld b,(ix+SYSCOPYCTX_LFN_NAME+1)
        call build_path
        ret


reject_nested_destination
        ld hl,SRC_STACK
        ld de,DST_STACK
.compare
        ld a,(hl)
        or a
        jr z,.src_done
        ld b,a
        ld a,(de)
        cp b
        jr nz,.ok
        inc hl
        inc de
        jr .compare
.src_done
        ld a,(de)
        or a
        jr z,.bad
        cp "/"
        jr z,.bad
        cp 92
        jr z,.bad
.ok
        xor a
        ret
.bad
        ld a,STAGE_NESTED_DST
        ld (failStage),a
        ld a,$7e
        scf
        ret


; A = depth. Uses SRC_STACK/DST_STACK path buffers for this depth.
copy_dir
        cp MAX_DEPTH+1
        jp nc,.depth_fail
        ld (curDepth),a

        call get_dst_path
        ld a,STAGE_COPY_MKDIR
        ld (failStage),a
        call esx_mkdir_ignore
        call inc_dir_count
        call inc_file_count
        ld de,copyPhaseTxt
        call print_counter_status

        ld a,(curDepth)
        call get_src_path
        push hl
        pop ix
        xor a
        ld b,MODE_LFN_DIR
        ld a,STAGE_COPY_LFN
        ld (failStage),a
        xor a
        rst $08
        db F_OPENDIR
        ret c
        ld (lfnHandle),a

.next
        ld a,(lfnHandle)
        ld ix,LFN_ENTRY
        ld a,STAGE_READ_LFN
        ld (failStage),a
        ld a,(lfnHandle)
        rst $08
        db F_READDIR
        jr c,.read_fail
        or a
        jr z,.done

        call skip_dot_lfn_entry
        jr z,.next

        ld a,(curDepth)
        cp MAX_DEPTH
        jr nc,.next
        ld a,STAGE_CHILD_PATH
        ld (failStage),a
        call build_child_paths
        jp c,.read_fail

        ld a,(LFN_ENTRY)
        and ATTR_DIR
        jr z,.file

        ld a,(lfnHandle)
        push af
        ld a,(curDepth)
        ld c,a
        push bc
        inc a
        call copy_dir
        ld e,a
        pop bc
        ld a,c
        ld (curDepth),a
        pop af
        ld (lfnHandle),a
        ld a,e
        jr c,.dir_fail
        jr .next

.dir_fail
        scf
        ret

.file
        call inc_file_count
        ld de,copyPhaseTxt
        call print_counter_status
        call copy_child_file
        jr c,.read_fail
        jp .next

.done
        ld a,(lfnHandle)
        rst $08
        db F_CLOSE
        xor a
        ret

.read_fail
        push af
        ld a,(lfnHandle)
        rst $08
        db F_CLOSE
        pop af
        scf
        ret

.depth_fail
        ld a,$7f
        ld (failStage),a
        ld a,$7f
        scf
        ret


store_bcde_at_hl
        ld (hl),c
        inc hl
        ld (hl),b
        inc hl
        ret


store_de_at_hl
        ld (hl),e
        inc hl
        ld (hl),d
        ret


load_bcde_from_hl
        ld c,(hl)
        inc hl
        ld b,(hl)
        inc hl
        ld e,(hl)
        inc hl
        ld d,(hl)
        ret


get_dir_pos_slot
        call depth_to_offset4
        ld hl,dirPosStack
        add hl,de
        ret


get_lfn_pos_slot
        call depth_to_offset4
        ld hl,lfnPosStack
        add hl,de
        ret


depth_to_offset4
        ld e,a
        ld d,0
        sla e
        rl d
        sla e
        rl d
        ret


build_child_paths
        ld a,(curDepth)
        call get_src_path
        push hl
        ld a,(curDepth)
        inc a
        call get_src_path
        ex de,hl
        pop hl
        ld bc,LFN_ENTRY+1
        call build_path
        ret c

        ld a,(curDepth)
        call get_dst_path
        push hl
        ld a,(curDepth)
        inc a
        call get_dst_path
        ex de,hl
        pop hl
        ld bc,LFN_ENTRY+1
        call build_path
        ret


build_child_src_path
        ld a,(curDepth)
        call get_src_path
        push hl
        ld a,(curDepth)
        inc a
        call get_src_path
        ex de,hl
        pop hl
        ld bc,DIR_ENTRY+1
        call build_path
        ret


copy_child_file
        call init_file_progress
        ld a,(curDepth)
        inc a
        call get_src_path
        push hl
        pop ix
        xor a
        ld b,MODE_READ_EXIST
        ld a,STAGE_SRC_OPEN
        ld (failStage),a
        xor a
        rst $08
        db F_OPEN
        ret c
        ld (srcHandle),a

        ld a,(curDepth)
        inc a
        call get_dst_path
        push hl
        pop ix
        xor a
        ld b,MODE_WRITE_TRUNC
        ld a,STAGE_DST_OPEN
        ld (failStage),a
        xor a
        rst $08
        db F_OPEN
        jr c,.dst_fail
        ld (dstHandle),a

.loop
        ld a,(srcHandle)
        ld ix,COPY_BUFFER
        ld bc,COPY_BUFFER_LEN
        ld a,STAGE_FILE_READ
        ld (failStage),a
        ld a,(srcHandle)
        rst $08
        db F_READ
        jr c,.copy_fail
        ld a,b
        or c
        jr z,.done
        ld (lastReadLen),bc

        ld a,(dstHandle)
        ld ix,COPY_BUFFER
        ld a,STAGE_FILE_WRITE
        ld (failStage),a
        ld a,(dstHandle)
        rst $08
        db F_WRITE
        jr c,.copy_fail
        call add_copied_bytes
        call print_file_progress
        call call_cancel
        cp 1
        jr z,.cancel
        jr .loop

.done
        ld a,(dstHandle)
        rst $08
        db F_CLOSE
        ld a,(srcHandle)
        rst $08
        db F_CLOSE
        xor a
        ret

.copy_fail
        push af
        ld a,(dstHandle)
        rst $08
        db F_CLOSE
        pop af
.dst_fail
        push af
        ld a,(srcHandle)
        rst $08
        db F_CLOSE
        pop af
        scf
        ret

.cancel
        ld a,(dstHandle)
        rst $08
        db F_CLOSE
        ld a,(srcHandle)
        rst $08
        db F_CLOSE
        ld a,(curDepth)
        inc a
        call get_dst_path
        push hl
        pop ix
        rst $08
        db F_UNLINK
        ld a,$7c
        scf
        ret


; A = depth. Deletes files first, then directories bottom-up.
delete_dir
        cp MAX_DEPTH+1
        jp nc,.depth_fail
        ld (curDepth),a

        call get_src_path
        push hl
        pop ix
        xor a
        ld b,a
        rst $08
        db F_OPENDIR
        ret c
        ld (delHandle),a

.next
        ld a,(delHandle)
        ld ix,DIR_ENTRY
        rst $08
        db F_READDIR
        jr c,.read_fail
        or a
        jr z,.done

        call skip_dot_entry
        jr z,.next

        ld a,(curDepth)
        cp MAX_DEPTH
        jr nc,.depth_fail_open
        call build_child_src_path

        ld a,(DIR_ENTRY)
        and ATTR_DIR
        jr z,.file

        ld a,(curDepth)
        push af
        ld a,(delHandle)
        push af
        ld a,(curDepth)
        inc a
        call delete_dir
        jr c,.dir_fail
        pop af
        ld (delHandle),a
        pop af
        ld (curDepth),a
        jr .next

.dir_fail
        ld e,a
        pop af
        ld (delHandle),a
        pop af
        ld (curDepth),a
        ld a,e
        jr .read_fail

.file
        call inc_file_count
        ld de,deletePhaseTxt
        call print_counter_status
        ld a,(curDepth)
        inc a
        call get_src_path
        push hl
        pop ix
        xor a
        rst $08
        db F_UNLINK
        jr c,.read_fail
        jr .next

.done
        ld a,(delHandle)
        rst $08
        db F_CLOSE
        jr c,.close_fail

        ld a,(curDepth)
        call get_src_path
        push hl
        pop ix
        xor a
        rst $08
        db F_RMDIR
        ret c
        call inc_file_count
        ld de,deletePhaseTxt
        call print_counter_status
        xor a
        ret

.depth_fail_open
        ld a,$7f
.read_fail
        push af
        ld a,(delHandle)
        rst $08
        db F_CLOSE
        pop af
        scf
        ret

.close_fail
        scf
        ret

.depth_fail
        ld a,$7f
        scf
        ret


; HL = parent path, DE = output path, BC = child name.
build_path
        ld (pathChildPtr),bc
        ld a,255
        ld (pathLeft),a
        xor a
        ld (lastChar),a
.copy_parent
        ld a,(hl)
        or a
        jr z,.parent_done
        cp 255
        jr z,.parent_done
        call put_path_char
        ret c
        inc hl
        jr .copy_parent

.parent_done
        ld a,(lastChar)
        cp "/"
        jr z,.copy_name
        cp 92
        jr z,.copy_name
        cp ":"
        jr z,.copy_name
        ld a,"/"
        call put_path_char
        ret c

.copy_name
        ld hl,(pathChildPtr)
.name_loop
        ld a,(hl)
        or a
        jr z,.name_zero
        cp 255
        jr z,.name_end
        call put_path_char
        ret c
        inc hl
        jr .name_loop
.name_zero
        ld (de),a
        or a
        ret
.name_end
        xor a
        ld (de),a
        or a
        ret


put_path_char
        ld c,a
        ld a,(pathLeft)
        or a
        jr z,.overflow
        dec a
        ld (pathLeft),a
        ld a,c
        ld (de),a
        ld (lastChar),a
        inc de
        or a
        ret
.overflow
        ld a,$7d
        scf
        ret


skip_dot_entry
        ld a,(DIR_ENTRY+1)
        cp "."
        jr nz,.not_dot
        ld a,(DIR_ENTRY+2)
        or a
        ret z
        cp "."
        jr nz,.not_dot
        ld a,(DIR_ENTRY+3)
        or a
        ret
.not_dot
        ld a,1
        or a
        ret


skip_dot_lfn_entry
        ld a,(LFN_ENTRY+1)
        cp "."
        jr nz,.not_dot
        ld a,(LFN_ENTRY+2)
        or a
        ret z
        cp "."
        jr nz,.not_dot
        ld a,(LFN_ENTRY+3)
        or a
        ret
.not_dot
        ld a,1
        or a
        ret


get_src_path
        add a,$E0
        ld h,a
        ld l,0
        ret


get_dst_path
        add a,$EC
        ld h,a
        ld l,0
        ret


esx_mkdir_ignore
        push hl
        pop ix
        xor a
        rst $08
        db F_MKDIR
        xor a
        ret


inc_file_count
        ld ix,(ctxPtr)
        inc (ix+SYSCOPYCTX_FILES_LO)
        ret nz
        inc (ix+SYSCOPYCTX_FILES_LO+1)
        ret nz
        inc (ix+SYSCOPYCTX_FILES_HI)
        ret nz
        inc (ix+SYSCOPYCTX_FILES_HI+1)
        ret


inc_dir_count
        ld ix,(ctxPtr)
        inc (ix+SYSCOPYCTX_DIRS_LO)
        ret nz
        inc (ix+SYSCOPYCTX_DIRS_LO+1)
        ret nz
        inc (ix+SYSCOPYCTX_DIRS_HI)
        ret nz
        inc (ix+SYSCOPYCTX_DIRS_HI+1)
        ret


prepare_total_count
        ld hl,0
        ld (totalFiles),hl
        ld ix,(ctxPtr)
        ld (ix+SYSCOPYCTX_FILES_LO),l
        ld (ix+SYSCOPYCTX_FILES_LO+1),h
        ld (ix+SYSCOPYCTX_FILES_HI),l
        ld (ix+SYSCOPYCTX_FILES_HI+1),h
        ld a,0
        call count_dir
        ld ix,(ctxPtr)
        ld hl,0
        ld (ix+SYSCOPYCTX_FILES_LO),l
        ld (ix+SYSCOPYCTX_FILES_LO+1),h
        ld (ix+SYSCOPYCTX_FILES_HI),l
        ld (ix+SYSCOPYCTX_FILES_HI+1),h
        ret


; A = depth. Counts files in SRC_STACK paths.
count_dir
        cp MAX_DEPTH+1
        jp nc,.depth_fail
        ld (curDepth),a

        call get_src_path
        push hl
        pop ix
        xor a
        ld b,a
        rst $08
        db F_OPENDIR
        ret c
        ld (countHandle),a
        call inc_total_count

.next
        ld a,(countHandle)
        ld ix,DIR_ENTRY
        rst $08
        db F_READDIR
        jr c,.read_fail
        or a
        jr z,.done

        call skip_dot_entry
        jr z,.next

        ld a,(curDepth)
        cp MAX_DEPTH
        jr nc,.next
        call build_child_src_path

        ld a,(DIR_ENTRY)
        and ATTR_DIR
        jr z,.file

        ld a,(curDepth)
        push af
        ld a,(countHandle)
        push af
        ld a,(curDepth)
        inc a
        call count_dir
        jr c,.dir_fail
        pop af
        ld (countHandle),a
        pop af
        ld (curDepth),a
        jr .next

.dir_fail
        ld e,a
        pop af
        ld (countHandle),a
        pop af
        ld (curDepth),a
        ld a,e
        jr .read_fail

.file
        call inc_total_count
        jr .next

.done
        ld a,(countHandle)
        rst $08
        db F_CLOSE
        xor a
        ret

.read_fail
        push af
        ld a,(countHandle)
        rst $08
        db F_CLOSE
        pop af
        scf
        ret

.depth_fail
        ld a,$7f
        scf
        ret


inc_total_count
        ld hl,(totalFiles)
        inc hl
        ld (totalFiles),hl
        ret


patch_services
        ld ix,(svcPtr)
        ld l,(ix+SYSCOPY_SERVICE_PRINT)
        ld h,(ix+SYSCOPY_SERVICE_PRINT+1)
        ld (call_print+1),hl
        ld l,(ix+SYSCOPY_SERVICE_CANCEL)
        ld h,(ix+SYSCOPY_SERVICE_CANCEL+1)
        ld (call_cancel+1),hl
        ret


print_status
        push af
        push bc
        push de
        push hl
        ld hl,11*256+13
        ld a,16
        call call_print
        pop hl
        pop de
        pop bc
        pop af
        ret


print_counter_status
        push af
        push bc
        push de
        push hl
        push ix

        ld hl,statusLine
        ld b,STATUS_LINE_LEN
.clear
        ld (hl),32
        inc hl
        djnz .clear
        xor a
        ld (hl),a

        ld hl,statusLine
.copy_text
        ld a,(de)
        or a
        jr z,.text_done
        ld (hl),a
        inc hl
        inc de
        jr .copy_text
.text_done
        call append_short_root_name
        ld (hl),32
        inc hl
        ld (hl),"("
        inc hl
        push hl

        ld ix,(ctxPtr)
        ld l,(ix+SYSCOPYCTX_FILES_LO)
        ld h,(ix+SYSCOPYCTX_FILES_LO+1)
        pop de
        call write_dec3
        ld a,"/"
        ld (de),a
        inc de
        ld hl,(totalFiles)
        call write_dec3
        ld a,")"
        ld (de),a
        inc de

        ld hl,56*256+14
        ld a,16
        ld de,blankNameTxt
        call call_print
        ld hl,11*256+13
        ld a,16
        ld de,statusLine
        call call_print
        pop ix
        pop hl
        pop de
        pop bc
        pop af
        ret


init_file_progress
        ld hl,0
        ld (fileCopiedBytes),hl
        ld (fileCopiedBytes+2),hl
        call find_lfn_entry_size_ptr
        ld de,fileTotalBytes
        ld bc,4
        ldir
        ld hl,LFN_ENTRY+1
        ld de,fileNameText
        ld b,20
.name
        ld a,(hl)
        or a
        jr z,.name_done
        cp 255
        jr z,.name_done
        ld (de),a
        inc hl
        inc de
        djnz .name
.name_done
        xor a
        ld (de),a
        jp print_file_progress


add_copied_bytes
        ld bc,(lastReadLen)
        ld hl,(fileCopiedBytes)
        add hl,bc
        ld (fileCopiedBytes),hl
        ret nc
        ld hl,(fileCopiedBytes+2)
        inc hl
        ld (fileCopiedBytes+2),hl
        ret


print_file_progress
        push af
        push bc
        push de
        push hl
        push ix

        ld hl,fileStatusLine
        ld b,STATUS_LINE_LEN
.clear
        ld (hl),32
        inc hl
        djnz .clear
        xor a
        ld (hl),a

        ld hl,fileNameText
        ld de,fileStatusLine
        ld b,20
.name
        ld a,(hl)
        or a
        jr z,.name_done
        cp 255
        jr z,.name_done
        ld (de),a
        inc hl
        inc de
        djnz .name
.name_done
        ld a,32
        ld (de),a
        inc de
        ld a,"["
        ld (de),a
        inc de

        push de
        ld hl,fileCopiedBytes
        call bytes_to_kb_capped
        pop de
        call write_dec3
        ld a,"k"
        ld (de),a
        inc de
        ld a,"B"
        ld (de),a
        inc de
        ld a,"/"
        ld (de),a
        inc de

        push de
        ld hl,fileTotalBytes
        call bytes_to_kb_capped
        pop de
        call write_dec3
        ld a,"k"
        ld (de),a
        inc de
        ld a,"B"
        ld (de),a
        inc de
        ld a,"]"
        ld (de),a

        ld hl,11*256+14
        ld a,16
        ld de,fileStatusLine
        call call_print
        pop ix
        pop hl
        pop de
        pop bc
        pop af
        ret


find_lfn_entry_size_ptr
        ld hl,LFN_ENTRY+1
.scan
        ld a,(hl)
        inc hl
        or a
        jr nz,.scan
        inc hl
        inc hl
        inc hl
        inc hl
        ret


; HL points to 32-bit byte count. Returns HL = kB capped to 999.
bytes_to_kb_capped
        push ix
        push hl
        pop ix
        ld a,(ix+1)
        srl a
        srl a
        ld l,a
        ld b,(ix+2)
        ld a,b
        and 3
        rrca
        rrca
        or l
        ld l,a
        ld a,b
        srl a
        srl a
        ld h,a
        ld a,(ix+3)
        or a
        jr nz,.cap
        ld de,1000
        or a
        sbc hl,de
        jr nc,.cap
        add hl,de
        pop ix
        ret
.cap
        ld hl,999
        pop ix
        ret


; HL=destination. Appends root directory name, max 10 chars + "...".
; Returns HL advanced to the new destination.
append_short_root_name
        push ix
        ld ix,(ctxPtr)
        ld e,(ix+SYSCOPYCTX_LFN_NAME)
        ld d,(ix+SYSCOPYCTX_LFN_NAME+1)
        ex de,hl
        ld b,10
.copy
        ld a,(hl)
        or a
        jr z,.done
        cp 255
        jr z,.done
        ld (de),a
        inc hl
        inc de
        djnz .copy
        ld a,(hl)
        or a
        jr z,.done
        cp 255
        jr z,.done
        ld a,"."
        ld (de),a
        inc de
        ld (de),a
        inc de
        ld (de),a
        inc de
.done
        ex de,hl
        pop ix
        ret


; HL=value, DE=destination. Writes 3 decimal digits, advances DE.
write_dec3
        push de
        ld de,1000
        or a
        sbc hl,de
        jr c,.under_1000
        ld hl,999
        jr .capped
.under_1000
        add hl,de
.capped
        pop de
        ld b,"0"-1
.hundreds
        inc b
        ld a,l
        sub 100
        ld l,a
        ld a,h
        sbc a,0
        ld h,a
        jr nc,.hundreds
        ld a,l
        add a,100
        ld l,a
        ld a,h
        adc a,0
        ld h,a
        ld a,b
        ld (de),a
        inc de

        ld b,"0"-1
.tens
        inc b
        ld a,l
        sub 10
        ld l,a
        ld a,h
        sbc a,0
        ld h,a
        jr nc,.tens
        ld a,l
        add a,10
        ld l,a
        ld a,h
        adc a,0
        ld h,a
        ld a,b
        ld (de),a
        inc de

        ld a,l
        add a,"0"
        ld (de),a
        inc de
        ret


call_print
        jp 0


call_cancel
        jp 0


ctxPtr      defw 0
svcPtr      defw 0
savedSp     defw 0
curDepth    defb 0
dirHandle   defb 0
lfnHandle   defb 0
delHandle   defb 0
srcHandle   defb 0
dstHandle   defb 0
lastChar    defb 0
countHandle defb 0
totalFiles  defw 0
pathChildPtr defw 0
pathLeft    defb 0
failStage   defb 0
lastReadLen defw 0
fileCopiedBytes defd 0
fileTotalBytes defd 0
fileNameText defs 21
dirPosStack defs (MAX_DEPTH+1)*4
lfnPosStack defs (MAX_DEPTH+1)*4

copyPhaseTxt   defb "Copying: ",0
deletePhaseTxt defb "Deleting: ",0
statusLine     defs 56
fileStatusLine defs 56
blankNameTxt   defb "BREAK = cancel ",0

plugin_end
        assert plugin_end - plugin_start <= SYSCOPY_PLUGIN_SIZE
        SAVEBIN "plugin/syscopy.ccp", SYSCOPY_PLUGIN_ADDRESS, SYSCOPY_PLUGIN_SIZE
