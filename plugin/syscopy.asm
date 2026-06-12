        DEVICE ZXSPECTRUMNEXT
        org SYSCOPY_PLUGIN_ADDRESS

        include "syscopy_api.i.asm"

F_OPEN      equ $9A
F_CLOSE     equ $9B
F_READ      equ $9D
F_WRITE     equ $9E
F_OPENDIR   equ $A3
F_READDIR   equ $A4
F_MKDIR     equ $AA

MODE_READ_EXIST  equ $01
MODE_WRITE_TRUNC equ $0E
MODE_LFN_DIR     equ $10
ATTR_DIR         equ $10

MAX_DEPTH        equ 11
PLUGIN_STACK     equ $DFFE

SRC_STACK        equ SYSCOPY_WORK_ADDRESS          ; 12 * 256 bytes
DST_STACK        equ SYSCOPY_WORK_ADDRESS + $0C00  ; 12 * 256 bytes
DIR_ENTRY        equ SYSCOPY_WORK_ADDRESS + $1800  ; 512 bytes
COPY_BUFFER      equ SYSCOPY_WORK_ADDRESS + $1A00
COPY_BUFFER_LEN  equ 1024

plugin_start
        ld (ctxPtr),hl
        ld (savedSp),sp
        ld sp,PLUGIN_STACK

        call copy_root_paths
        ld a,0
        call copy_dir
        jr c,.failed

        ld ix,(ctxPtr)
        xor a
        ld (ix+SYSCOPYCTX_RESULT),a
        ld sp,(savedSp)
        xor a
        ret

.failed
        ld ix,(ctxPtr)
        ld (ix+SYSCOPYCTX_ERROR),a
        ld a,1
        ld (ix+SYSCOPYCTX_RESULT),a
        ld sp,(savedSp)
        ret


copy_root_paths
        ld ix,(ctxPtr)
        ld l,(ix+SYSCOPYCTX_SRC_PATH)
        ld h,(ix+SYSCOPYCTX_SRC_PATH+1)
        ld de,SRC_STACK
        ld c,(ix+SYSCOPYCTX_NAME)
        ld b,(ix+SYSCOPYCTX_NAME+1)
        call build_path

        ld ix,(ctxPtr)
        ld l,(ix+SYSCOPYCTX_DST_PATH)
        ld h,(ix+SYSCOPYCTX_DST_PATH+1)
        ld de,DST_STACK
        ld c,(ix+SYSCOPYCTX_NAME)
        ld b,(ix+SYSCOPYCTX_NAME+1)
        call build_path
        ret


; A = depth. Uses SRC_STACK/DST_STACK path buffers for this depth.
copy_dir
        cp MAX_DEPTH+1
        jp nc,.depth_fail
        ld (curDepth),a

        call get_dst_path
        call esx_mkdir_ignore

        ld a,(curDepth)
        call get_src_path
        push hl
        pop ix
        xor a
        ld b,MODE_LFN_DIR
        rst $08
        db F_OPENDIR
        ret c
        ld (dirHandle),a

.next
        ld a,(dirHandle)
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
        call build_child_paths

        ld a,(DIR_ENTRY)
        and ATTR_DIR
        jr z,.file

        call inc_dir_count
        ld a,(curDepth)
        push af
        ld a,(dirHandle)
        push af
        ld a,(curDepth)
        inc a
        call copy_dir
        jr c,.dir_fail
        pop af
        ld (dirHandle),a
        pop af
        ld (curDepth),a
        jr .next

.dir_fail
        ld e,a
        pop af
        ld (dirHandle),a
        pop af
        ld (curDepth),a
        ld a,e
        jr .read_fail

.file
        call inc_file_count
        call copy_child_file
        jr c,.read_fail
        jr .next

.done
        ld a,(dirHandle)
        rst $08
        db F_CLOSE
        xor a
        ret

.read_fail
        push af
        ld a,(dirHandle)
        rst $08
        db F_CLOSE
        pop af
        scf
        ret

.depth_fail
        ld a,$7f
        scf
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
        ld bc,DIR_ENTRY+1
        call build_path

        ld a,(curDepth)
        call get_dst_path
        push hl
        ld a,(curDepth)
        inc a
        call get_dst_path
        ex de,hl
        pop hl
        ld bc,DIR_ENTRY+1
        call build_path
        ret


copy_child_file
        ld a,(curDepth)
        inc a
        call get_src_path
        push hl
        pop ix
        xor a
        ld b,MODE_READ_EXIST
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
        rst $08
        db F_OPEN
        jr c,.dst_fail
        ld (dstHandle),a

.loop
        ld a,(srcHandle)
        ld ix,COPY_BUFFER
        ld bc,COPY_BUFFER_LEN
        rst $08
        db F_READ
        jr c,.copy_fail
        ld a,b
        or c
        jr z,.done

        ld a,(dstHandle)
        ld ix,COPY_BUFFER
        rst $08
        db F_WRITE
        jr c,.copy_fail
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


; HL = parent path, DE = output path, BC = child name.
build_path
        xor a
        ld (lastChar),a
.copy_parent
        ld a,(hl)
        or a
        jr z,.parent_done
        cp 255
        jr z,.parent_done
        ld (de),a
        ld (lastChar),a
        inc hl
        inc de
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
        ld (de),a
        inc de

.copy_name
        ld h,b
        ld l,c
.name_loop
        ld a,(hl)
        cp 255
        jr z,.name_end
        ld (de),a
        inc hl
        inc de
        or a
        ret z
        jr .name_loop
.name_end
        xor a
        ld (de),a
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


ctxPtr      defw 0
savedSp     defw 0
curDepth    defb 0
dirHandle   defb 0
srcHandle   defb 0
dstHandle   defb 0
lastChar    defb 0

plugin_end
        assert plugin_end - plugin_start <= SYSCOPY_PLUGIN_SIZE
        SAVEBIN "plugin/syscopy.ccp", SYSCOPY_PLUGIN_ADDRESS, SYSCOPY_PLUGIN_SIZE
