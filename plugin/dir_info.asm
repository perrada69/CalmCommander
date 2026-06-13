        DEVICE ZXSPECTRUMNEXT
        org DIRINFO_PLUGIN_ADDRESS

        include "dir_info_api.i.asm"

F_CLOSE     equ $9B
F_OPENDIR   equ $A3
F_READDIR   equ $A4

MODE_LFN_DIR equ $10
ATTR_DIR     equ $10
MAX_DEPTH    equ 11
PLUGIN_STACK equ $DFFE

ROOT_PATH    equ DIRINFO_WORK_ADDRESS              ; 12 * 256-byte path slots
DIR_ENTRY    equ DIRINFO_WORK_ADDRESS + $0C00      ; keep clear of depth path slots

STAGE_ROOT   equ $10
STAGE_OPEN   equ $20
STAGE_READ   equ $21
STAGE_CHILD  equ $22

plugin_start
        ld (ctxPtr),hl
        ld (svcPtr),de
        ld (savedSp),sp
        ld sp,PLUGIN_STACK
        call patch_services

        call build_root_path
        jr c,.failed
        xor a
        call count_dir
        jr c,.failed

        ld ix,(ctxPtr)
        xor a
        ld (ix+DIRINFOCTX_RESULT),a
        ld sp,(savedSp)
        xor a
        ret

.failed
        ld ix,(ctxPtr)
        ld (ix+DIRINFOCTX_ERROR),a
        ld a,(failStage)
        ld (ix+DIRINFOCTX_STAGE),a
        ld a,1
        ld (ix+DIRINFOCTX_RESULT),a
        ld sp,(savedSp)
        ret


build_root_path
        ld a,STAGE_ROOT
        ld (failStage),a
        ld ix,(ctxPtr)
        ld l,(ix+DIRINFOCTX_SRC_PATH)
        ld h,(ix+DIRINFOCTX_SRC_PATH+1)
        ld e,(ix+DIRINFOCTX_NAME)
        ld d,(ix+DIRINFOCTX_NAME+1)
        ld (pathChildPtr),de
        ld de,ROOT_PATH
        jp build_path


; A = depth. Counts all items below ROOT_PATH.
count_dir
        cp MAX_DEPTH+1
        jp nc,.depth_fail
        ld (curDepth),a

        call get_path_for_depth
        push hl
        pop ix
        xor a
        ld b,MODE_LFN_DIR
        ld a,STAGE_OPEN
        ld (failStage),a
        xor a
        rst $08
        db F_OPENDIR
        ret c
        ld (dirHandle),a
        ld a,(curDepth)
        call clear_dir_index

.next
        ld ix,DIR_ENTRY
        ld a,STAGE_READ
        ld (failStage),a
        ld a,(dirHandle)
        rst $08
        db F_READDIR
        jr c,.read_fail
        or a
        jr z,.done
        ld a,(curDepth)
        call inc_dir_index

        call skip_dot_entry
        jr z,.next

        call inc_item_count

        ld a,(DIR_ENTRY)
        and ATTR_DIR
        jr nz,.dir

        call add_entry_size
        jr .next

.dir
        ld a,(curDepth)
        cp MAX_DEPTH
        jr nc,.next
        ld a,STAGE_CHILD
        ld (failStage),a
        call build_child_path
        jr c,.read_fail

        ld a,(curDepth)
        push af
        call save_close_dir_pos
        jr c,.restore_fail
        ld a,(curDepth)
        inc a
        call count_dir
        pop af
        ld (curDepth),a
        jr c,.child_fail
        call reopen_dir_pos
        jr c,.child_fail
        jr .next

.child_fail
        scf
        ret

.restore_fail
        pop af
        ld (curDepth),a
        jr .read_fail

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


build_child_path
        ld a,(curDepth)
        call get_path_for_depth
        push hl
        ld a,(curDepth)
        inc a
        call get_path_for_depth
        ex de,hl
        pop hl
        ld bc,DIR_ENTRY+1
        ld (pathChildPtr),bc
        jp build_path


; HL = parent path, DE = output path, pathChildPtr = child name.
build_path
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


get_path_for_depth
        add a,$E0
        ld h,a
        ld l,0
        ret


depth_to_offset4
        ld e,a
        ld d,0
        sla e
        rl d
        sla e
        rl d
        ret


get_dir_pos_slot
        call depth_to_offset4
        ld hl,dirPosStack
        add hl,de
        ret


save_close_dir_pos
        ld a,(dirHandle)
        rst $08
        db F_CLOSE
        ret


reopen_dir_pos
        ld a,(curDepth)
        call get_path_for_depth
        push hl
        pop ix
        xor a
        ld b,MODE_LFN_DIR
        rst $08
        db F_OPENDIR
        ret c
        ld (dirHandle),a
        ld a,(curDepth)
        call get_dir_pos_slot
        ld a,(dirHandle)
        call skip_saved_entries
        ret nc
        push af
        ld a,(dirHandle)
        rst $08
        db F_CLOSE
        pop af
        ret


clear_dir_index
        call get_dir_pos_slot
        ld (hl),0
        inc hl
        ld (hl),0
        ret


inc_dir_index
        call get_dir_pos_slot
        inc (hl)
        ret nz
        inc hl
        inc (hl)
        ret


; A=handle, HL=stored 16-bit entry count. Uses DIR_ENTRY as scratch buffer.
skip_saved_entries
        ld (skipHandle),a
        ld c,(hl)
        inc hl
        ld b,(hl)
        ld (skipCount),bc
.loop
        ld bc,(skipCount)
        ld a,b
        or c
        ret z
        ld ix,DIR_ENTRY
        ld a,(skipHandle)
        rst $08
        db F_READDIR
        ret c
        or a
        jr nz,.read_one
        ld a,$7e
        scf
        ret
.read_one
        ld bc,(skipCount)
        dec bc
        ld (skipCount),bc
        jr .loop


inc_item_count
        ld ix,(ctxPtr)
        inc (ix+DIRINFOCTX_ITEMS_LO)
        ret nz
        inc (ix+DIRINFOCTX_ITEMS_LO+1)
        ret nz
        inc (ix+DIRINFOCTX_ITEMS_HI)
        ret nz
        inc (ix+DIRINFOCTX_ITEMS_HI+1)
        ret


add_entry_size
        call find_entry_size_ptr
        ld ix,(ctxPtr)
        ld a,(hl)
        add a,(ix+DIRINFOCTX_SIZE_LO)
        ld (ix+DIRINFOCTX_SIZE_LO),a
        inc hl
        ld a,(hl)
        adc a,(ix+DIRINFOCTX_SIZE_LO+1)
        ld (ix+DIRINFOCTX_SIZE_LO+1),a
        inc hl
        ld a,(hl)
        adc a,(ix+DIRINFOCTX_SIZE_HI)
        ld (ix+DIRINFOCTX_SIZE_HI),a
        inc hl
        ld a,(hl)
        adc a,(ix+DIRINFOCTX_SIZE_HI+1)
        ld (ix+DIRINFOCTX_SIZE_HI+1),a
        ret


find_entry_size_ptr
        ld hl,DIR_ENTRY+1
.scan
        ld a,(hl)
        inc hl
        or a
        jr nz,.scan
        inc hl          ; timestamp low
        inc hl          ; timestamp high
        inc hl          ; datestamp low
        inc hl          ; datestamp high
        ret


patch_services
        ld ix,(svcPtr)
        ld l,(ix+DIRINFO_SERVICE_PRINT)
        ld h,(ix+DIRINFO_SERVICE_PRINT+1)
        ld (call_print+1),hl
        ret

call_print
        jp 0

ctxPtr       defw 0
svcPtr       defw 0
savedSp      defw 0
curDepth     defb 0
dirHandle    defb 0
lastChar     defb 0
pathChildPtr defw 0
pathLeft     defb 0
failStage    defb 0
skipHandle   defb 0
skipCount    defw 0
dirPosStack  defs (MAX_DEPTH+1)*4

plugin_end
        assert plugin_end - plugin_start <= DIRINFO_PLUGIN_SIZE
        SAVEBIN "plugin/dir_info.ccp", DIRINFO_PLUGIN_ADDRESS, DIRINFO_PLUGIN_SIZE
