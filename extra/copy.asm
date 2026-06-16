; -----------------------------------------------------------------------------
; .copy - standalone NextZXOS dot command
; -----------------------------------------------------------------------------
; Copies or moves files/directories using the esxDOS-compatible API.
; This file was split from the Calm Commander syscopy plugin, but runs as a
; normal dot command at $2000. Because dot commands execute in a special DOS
; environment, pointers passed to RST $08 use HL, not IX.
; -----------------------------------------------------------------------------

        DEVICE ZXSPECTRUMNEXT
        org $2000

        include "../plugin/syscopy_api.i.asm"

; esxDOS / NextZXOS hooks used by this command.
F_OPEN      equ $9A
F_CLOSE     equ $9B
F_READ      equ $9D
F_WRITE     equ $9E
F_OPENDIR   equ $A3
F_READDIR   equ $A4
F_MKDIR     equ $AA
F_RMDIR     equ $AB
F_UNLINK    equ $AD
F_RENAME    equ $B0
M_ERRH      equ $95

; NextReg access for temporary 28 MHz turbo during actual copy/delete work.
TBBLUE_REGISTER_SELECT_P_243B equ $243B
TBBLUE_REGISTER_ACCESS_P_253B equ $253B
TURBO_CONTROL_NR_07 equ $07

; File modes and FAT attributes.
MODE_READ_EXIST  equ $01
MODE_WRITE_TRUNC equ $0E
MODE_LFN_DIR     equ $10
ATTR_DIR         equ $10

MAX_DEPTH        equ 11
PLUGIN_STACK     equ $BFFE
SYS_COPY_WORK_PAGE equ 99

; Stage values are printed with raw esxDOS errors. They are intentionally coarse:
; they make field debugging possible without keeping verbose debug output on.
STAGE_ROOT_PATH  equ $11
STAGE_NESTED_DST equ $12
STAGE_DST_PARENT equ $13
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

; Runtime work area. Path stacks hold one 256-byte path per recursion level.
; The copy buffer is deliberately above DIR_ENTRY/LFN_ENTRY so file I/O cannot
; overwrite the directory entry currently being processed.
SRC_STACK        equ $C000                         ; 12 * 256 bytes
DST_STACK        equ $CC00                         ; 12 * 256 bytes
DIR_ENTRY        equ $D800                         ; 512 bytes
LFN_ENTRY        equ $DA00                         ; 512 bytes
COPY_BUFFER      equ $DC00                         ; 8 KiB copy buffer above DIR/LFN entries
COPY_BUFFER_LEN  equ 8192
STATUS_LINE_LEN  equ 45
SCREEN_LINE_LEN  equ 32
DEBUG_OUTPUT     equ 0

MAIN
        ; Entry: HL points to the dot-command argument line. Preserve the caller
        ; stack and switch to our private stack before doing ROM/DOS calls.
        ld (argPtr),hl
        ld (savedSp),sp
        ld sp,PLUGIN_STACK
        ; Save MMU slots 6+7 and map dedicated work pages so we never touch BASIC RAM.
        ld a,$56
        call read_nextreg
        ld (savedMmu6),a
        ld a,$57
        call read_nextreg
        ld (savedMmu7),a
        nextreg $56,SYS_COPY_WORK_PAGE
        nextreg $57,SYS_COPY_WORK_PAGE+1
        call install_error_handler
        call clear_screen
        call clear_options
        ld hl,(argPtr)
        call debug_print_args
        ld hl,(argPtr)
        call parse_args
        call debug_print_parsed
        jp c,show_help_or_error
        ld a,(helpFlag)
        or a
        jp nz,show_help_ok

        ; Both source and destination are mandatory. The parser accepts either
        ; `.copy source destiny` or `.copy -s source -d destiny`.
        ld a,(srcPath)
        or a
        jp z,show_usage_error
        ld a,(dstPath)
        or a
        jp z,show_usage_error

        ld hl,srcPath
        call trim_trailing_slash
        call setup_context
        ld hl,msgDbgSetup
        call debug_print_msg
        ld hl,dotCtx
        ld (ctxPtr),hl
        xor a
        ld (failStage),a
        ld (cpFiles),a
        ld (cpFiles+1),a
        ld (cpDirs),a
        ld (cpDirs+1),a
        ; Print the screen before probing destination paths. Some missing-drive
        ; failures in emulators may enter ROM error handling; at least the user
        ; sees the command context if that happens.
        call print_header
        call validate_dest_parent
        jp c,failed

        ; Directory detection is done by attempting to open the source as a
        ; directory with LFN mode. Failure means "treat it as a single file".
        ld hl,msgDbgBeforeSource
        call debug_print_msg
        ld a,STAGE_COPY_LFN
        ld (failStage),a
        call is_source_directory
        jr nc,.source_is_dir
        push af
        ld hl,msgDbgSourceFile
        call debug_print_msg
        ld hl,msgDbgOpenErr
        call debug_print_msg
        pop af
        call debug_print_hex8
        call debug_print_nl
        call set_turbo_28
        jp copy_one_file
.source_is_dir
        ld hl,msgDbgSourceDir
        call debug_print_msg

        ; For directory copies both root paths are prebuilt into the depth-0
        ; path stack. Children are then assembled from the current directory
        ; entry names while recursing.
        ld hl,msgDbgRoot
        call debug_print_msg
        call copy_root_paths
        jp c,failed
        ld hl,msgDbgRootOk
        call debug_print_msg
        call set_turbo_28
        ld ix,(ctxPtr)
        ld a,(ix+SYSCOPYCTX_MODE)
        cp 2
        jr nz,.copy_or_move
        call prepare_total_count
        ld de,deletePhaseTxt
        call print_counter_status
        ld a,0
        call delete_dir
        jp c,failed
        jp success

.copy_or_move
        ; Avoid copying a directory into itself or into one of its descendants.
        call reject_nested_destination
        jp c,failed
        call prepare_total_count
        ld de,copyPhaseTxt
        call print_counter_status
        ld a,0
        call copy_dir
        jp c,failed

        ld ix,(ctxPtr)
        ld a,(ix+SYSCOPYCTX_MODE)
        or a
        jr z,success
        call prepare_total_count
        ld de,deletePhaseTxt
        call print_counter_status
        ld a,0
        call delete_dir
        jp c,failed

success
        ld ix,(ctxPtr)
        xor a
        ld (ix+SYSCOPYCTX_RESULT),a
        call restore_mmu
        ld sp,(savedSp)
        call clear_copy_display
        call print_copy_result
        call clear_error_handler
        call restore_turbo
        ld bc,0
        xor a
        ret

failed
        ld ix,(ctxPtr)
        ld (ix+SYSCOPYCTX_ERROR),a
        ld a,(failStage)
        ld (ix+SYSCOPYCTX_STAGE),a
        ld a,1
        ld (ix+SYSCOPYCTX_RESULT),a
        call restore_mmu
        ld sp,(savedSp)
        ld a,(dotCtx+SYSCOPYCTX_ERROR)
        cp $7c
        jp z,cancelled
        ld a,(dotCtx+SYSCOPYCTX_STAGE)
        cp STAGE_DST_PARENT
        jp z,dest_parent_missing
        ld hl,0*256+20
        ld a,16
        ld de,msgError
        call call_print
        ld a,(dotCtx+SYSCOPYCTX_ERROR)
        call print_hex8
        ld hl,msgStage
        call print_msg
        ld a,(dotCtx+SYSCOPYCTX_STAGE)
        call print_hex8
        call clear_error_handler
        call restore_turbo
        ld bc,0
        xor a
        ret

dest_parent_missing
        ld hl,0*256+20
        ld a,16
        ld de,msgDestMissing
        call call_print
        call clear_error_handler
        call restore_turbo
        ld bc,0
        xor a
        ret

cancelled
        call clear_copy_display
        ld hl,0*256+20
        ld a,16
        ld de,msgCancelled
        call call_print
        call print_copy_result_r21
        call clear_error_handler
        call restore_turbo
        ld bc,0
        xor a
        ret

show_help_or_error
show_usage_error
        call restore_mmu
        ld sp,(savedSp)
        ld hl,msgUsageError
        call print_msg
show_help_ok
        call restore_mmu
        ld sp,(savedSp)
        ld hl,msgHelp
        call print_msg
        call clear_error_handler
        call restore_turbo
        ld bc,0
        xor a
        ret


copy_one_file
        call make_file_destination
        ld a,(moveFlag)
        or a
        jr z,.copy

        ld hl,srcPath
        ld de,dstFullPath
        ld a,"*"
        rst $08
        db F_RENAME
        jr c,.copy
        ld hl,(cpFiles)
        inc hl
        ld (cpFiles),hl
        jp success

.copy
        call confirm_file_overwrite
        jp c,failed
        or a
        jp z,success

        ld hl,srcPath
        ld a,"*"
        ld b,MODE_READ_EXIST
        rst $08
        db F_OPEN
        jp c,failed
        ld (srcHandle),a

        ld hl,dstFullPath
        ld a,"*"
        ld b,MODE_WRITE_TRUNC
        rst $08
        db F_OPEN
        jr c,.dst_fail
        ld (dstHandle),a

.loop
        call call_cancel
        cp 1
        jp z,.cancel
        ld a,(srcHandle)
        ld hl,COPY_BUFFER
        ld bc,COPY_BUFFER_LEN
        rst $08
        db F_READ
        jr c,.copy_fail
        ld a,b
        or c
        jr z,.done
        ld (lastReadLen),bc

        call call_cancel
        cp 1
        jp z,.cancel
        ld a,(dstHandle)
        ld hl,COPY_BUFFER
        ld bc,(lastReadLen)
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
        ld hl,(cpFiles)
        inc hl
        ld (cpFiles),hl
        ld a,(moveFlag)
        or a
        jp z,success
        ld hl,srcPath
        ld a,"*"
        rst $08
        db F_UNLINK
        jp c,failed
        jp success

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
        jp failed

.cancel
        ld a,(dstHandle)
        rst $08
        db F_CLOSE
        ld a,(srcHandle)
        rst $08
        db F_CLOSE
        ld hl,dstFullPath
        ld a,"*"
        rst $08
        db F_UNLINK
        ld a,$7c
        jp failed


clear_options
        xor a
        ld (moveFlag),a
        ld (yesFlag),a
        ld (helpFlag),a
        ld (posIndex),a
        ld hl,srcPath
        ld b,7
.clear_paths
        push bc
        ld (hl),0
        ld de,hl
        inc de
        ld bc,255
        ldir
        ex de,hl
        pop bc
        djnz .clear_paths
        ret


parse_args
.next
        call skip_spaces
        ld a,(hl)
        or a
        ret z
        cp 13
        ret z
        cp "-"
        jr z,.switch
        call store_positional
        ret c
        jr .next

.switch
        inc hl
        ld a,(hl)
        and $DF
        inc hl
        cp "H"
        jr z,.help
        cp "M"
        jr z,.move
        cp "Y"
        jr z,.yes
        cp "S"
        jr z,.source
        cp "D"
        jr z,.dest
        scf
        ret
.help
        ld a,1
        ld (helpFlag),a
        or a
        ret
.move
        ld a,1
        ld (moveFlag),a
        jr .next
.yes
        ld a,1
        ld (yesFlag),a
        jr .next
.source
        call skip_separators
        ld de,srcPath
        call extract_token
        ret c
        jr .next
.dest
        call skip_separators
        ld de,dstPath
        call extract_token
        ret c
        jr .next


store_positional
        ld a,(posIndex)
        or a
        jr z,.first
        cp 1
        jr z,.dst
        scf
        ret
.first
        call is_command_name_token
        jr c,.skip_command_name
.src
        ld a,1
        ld (posIndex),a
        ld de,srcPath
        jp extract_token
.skip_command_name
        call skip_token
        call skip_spaces
        jp store_positional
.dst
        inc a
        ld (posIndex),a
        ld de,dstPath
        jp extract_token


is_command_name_token
        push hl
        ld a,(hl)
        cp "."
        jr nz,.maybe_copy
        inc hl
.maybe_copy
        ld a,(hl)
        and $DF
        cp "C"
        jr nz,.no
        inc hl
        ld a,(hl)
        and $DF
        cp "O"
        jr nz,.no
        inc hl
        ld a,(hl)
        and $DF
        cp "P"
        jr nz,.no
        inc hl
        ld a,(hl)
        and $DF
        cp "Y"
        jr nz,.no
        inc hl
        ld a,(hl)
        or a
        jr z,.yes
        cp 13
        jr z,.yes
        cp " "
        jr z,.yes
        cp ":"
        jr z,.yes
.no
        pop hl
        or a
        ret
.yes
        pop hl
        scf
        ret


skip_token
        ld a,(hl)
        or a
        ret z
        cp 13
        ret z
        cp " "
        ret z
        inc hl
        jr skip_token


skip_spaces
        ld a,(hl)
        cp " "
        jr nz,.done
        inc hl
        jr skip_spaces
.done
        ret


skip_separators
        call skip_spaces
        ld a,(hl)
        cp "="
        jr z,.skip_one
        cp ":"
        ret nz
.skip_one
        inc hl
        jp skip_spaces


extract_token
        ld b,255
        ld a,(hl)
        cp 34
        jr z,.quoted
.plain
        ld a,(hl)
        or a
        jr z,.done
        cp 13
        jr z,.done
        cp " "
        jr z,.done
        cp $A5
        jr nc,.done
        ld (de),a
        inc de
        inc hl
        djnz .plain
        jr .done
.quoted
        inc hl
.qloop
        ld a,(hl)
        or a
        jr z,.done
        cp 13
        jr z,.done
        cp 34
        jr z,.qdone
        ld (de),a
        inc de
        inc hl
        djnz .qloop
        jr .done
.qdone
        inc hl
.done
        xor a
        ld (de),a
        or a
        ret


setup_context
        ; Fill the small syscopy-compatible context block. The rest of this file
        ; still uses that structure because most code was shared with the
        ; Calm Commander system-copy plugin.
        ld hl,msgDbgSetupStart
        call debug_print_msg
        ld a,SYSCOPY_ABI
        ld (dotCtx+SYSCOPYCTX_ABI),a
        ld a,(moveFlag)
        ld (dotCtx+SYSCOPYCTX_MODE),a
        ld hl,srcParent
        ld (dotCtx+SYSCOPYCTX_SRC_PATH),hl
        ld hl,dstParent
        ld (dotCtx+SYSCOPYCTX_DST_PATH),hl
        ld hl,srcName
        ld (dotCtx+SYSCOPYCTX_NAME),hl
        ld hl,dstName
        ld (dotCtx+SYSCOPYCTX_LFN_NAME),hl
        xor a
        ld (dotCtx+SYSCOPYCTX_RESULT),a
        ld (dotCtx+SYSCOPYCTX_ERROR),a
        ld (dotCtx+SYSCOPYCTX_STAGE),a
        ld hl,msgDbgSplitSrc
        call debug_print_msg
        call split_source_path
        ld hl,msgDbgSplitDst
        call debug_print_msg
        call split_dest_path
        ret


split_source_path
        ; Split `srcPath` into parent directory and leaf name. If there is no
        ; slash/drive separator, parent becomes ".".
        ld hl,srcPath
        ld de,srcParent
        ld bc,srcName
        jp split_path


split_dest_path
        ; Destination semantics:
        ; - existing directory, or path ending in slash: append source name
        ; - otherwise: treat destination as the final file/directory name
        ld hl,dstPath
        call path_ends_with_slash
        jr z,.directory_target
        call is_dst_directory
        jr nc,.directory_target
        ld hl,dstPath
        ld de,dstParent
        ld bc,dstName
        jp split_path
.directory_target
        ld hl,dstPath
        ld de,dstParent
        call copy_string
        ld hl,srcName
        ld de,dstName
        jp copy_string


is_source_directory
        ; NC means source can be opened as a directory. C means it is a file or
        ; does not exist; the caller then tries the file-copy path.
        ld hl,srcPath
        ld a,"*"
        ld b,MODE_LFN_DIR
        rst $08
        db F_OPENDIR
        ret c
        rst $08
        db F_CLOSE
        xor a
        ret


is_dst_directory
        ; Used only to decide how to interpret the destination argument.
        ; Non-existing paths are not errors here.
        ld hl,dstPath
        ld a,"*"
        ld b,MODE_LFN_DIR
        rst $08
        db F_OPENDIR
        ret c
        rst $08
        db F_CLOSE
        xor a
        ret


validate_dest_parent
        ; Validate only the parent part of the destination. Example:
        ;   d:/backup/foo -> checks d:/backup
        ; This catches missing paths before the command starts creating files.
        ; A dot-command ROM error handler is installed because some systems report
        ; missing drives as BASIC errors instead of ordinary esxDOS carry errors.
        ld a,STAGE_DST_PARENT
        ld (failStage),a
        ld hl,dstParent
        ld a,"*"
        ld b,MODE_LFN_DIR
        rst $08
        db F_OPENDIR
        ret c
        rst $08
        db F_CLOSE
        xor a
        ret


make_file_destination
        ; Single-file copy builds the final destination filename here. Directory
        ; copy uses copy_root_paths/build_child_paths instead.
        ld hl,dstPath
        call path_ends_with_slash
        jr z,.append_name
        call is_dst_directory
        jr nc,.append_name
        ld hl,dstPath
        ld de,dstFullPath
        jp copy_string
.append_name
        ld hl,dstPath
        ld de,dstFullPath
        ld bc,srcName
        jp build_path


confirm_file_overwrite
        ; Standalone dot command policy: without -y, existing files are skipped.
        ; We do not ask interactively, because there is no Calm Commander UI here.
        ld hl,dstFullPath
        ld a,"*"
        ld b,MODE_READ_EXIST
        rst $08
        db F_OPEN
        jr c,.copy
        rst $08
        db F_CLOSE
        ld a,(yesFlag)
        or a
        jr nz,.copy
        ld hl,0*256+10
        ld a,16
        ld de,msgSkip
        call call_print
        xor a
        ret
.copy
        ld a,1
        or a
        ret


path_ends_with_slash
        ; Returns Z if the last character is '/' or '\'. Empty strings also
        ; return Z, but callers never pass empty destination paths.
        ld a,(hl)
        or a
        ret z
.scan
        ld b,a
        inc hl
        ld a,(hl)
        or a
        jr nz,.scan
        ld a,b
        cp "/"
        ret z
        cp 92
        ret


trim_trailing_slash
        ; Normalize source paths so `dot/` and `dot` are treated the same.
        ld de,0
.scan
        ld a,(hl)
        or a
        jr z,.done
        push hl
        pop de
        inc hl
        jr .scan
.done
        ld a,d
        or e
        ret z
        ld a,(de)
        cp "/"
        jr z,.trim
        cp 92
        ret nz
.trim
        xor a
        ld (de),a
        ret


split_path
        ; Generic splitter. The last slash, backslash, or drive colon separates
        ; parent from leaf. For `d:/foo`, parent is `d:/` and name is `foo`.
        ld (splitSrc),hl
        ld (splitParent),de
        ld (splitName),bc
        ld de,0
        ld (lastSep),de
.scan
        ld a,(hl)
        or a
        jr z,.scan_done
        cp "/"
        jr z,.mark
        cp 92
        jr z,.mark
        cp ":"
        jr nz,.next_char
.mark
        push hl
        pop de
        ld (lastSep),de
.next_char
        inc hl
        jr .scan
.scan_done
        ld de,(lastSep)
        ld a,d
        or e
        jr z,.no_parent
        ld hl,(lastSep)
        ld de,(splitSrc)
        or a
        sbc hl,de
        inc hl
        push hl
        pop bc
        ld de,(splitParent)
        ld hl,(splitSrc)
        ldir
        xor a
        ld (de),a
        ld hl,(lastSep)
        inc hl
        ld de,(splitName)
        jp copy_string
.no_parent
        ld hl,dotTxt
        ld de,(splitParent)
        call copy_string
        ld hl,(splitSrc)
        ld de,(splitName)
        jp copy_string


copy_string
        ld a,(hl)
        ld (de),a
        or a
        ret z
        inc hl
        inc de
        jr copy_string


copy_root_paths
        ; Build depth-0 absolute/relative working paths:
        ;   SRC_STACK[0] = srcParent/srcName
        ;   DST_STACK[0] = dstParent/dstName
        ; Further recursion writes paths into the next 256-byte slots.
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
        ; Reject copying a directory into itself or below itself. This is a
        ; simple prefix check on normalized root paths.
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
        ; Directory copy is depth-first. Each level opens the source directory in
        ; LFN mode, creates the matching destination directory, then processes
        ; entries one by one. LFN entry format:
        ;   byte 0 = MSDOS attributes, byte 1.. = zero-terminated long name.
        cp MAX_DEPTH+1
        jp nc,.depth_fail
        ld (curDepth),a

        call get_dst_path
        ld a,STAGE_COPY_MKDIR
        ld (failStage),a
        call esx_mkdir_ignore
        ld hl,(cpDirs)
        inc hl
        ld (cpDirs),hl
        call inc_dir_count
        call inc_file_count
        ld de,copyPhaseTxt
        call print_counter_status

        ld a,(curDepth)
        call get_src_path
        xor a
        ld b,MODE_LFN_DIR
        ld a,STAGE_COPY_LFN
        ld (failStage),a
        ld a,"*"
        rst $08
        db F_OPENDIR
        ret c
        ld (lfnHandle),a
        ld a,(curDepth)
        call clear_lfn_index

.next
        call call_cancel
        cp 1
        jp z,.cancel
        ld a,(lfnHandle)
        ld hl,LFN_ENTRY
        ld a,STAGE_READ_LFN
        ld (failStage),a
        ld a,(lfnHandle)
        rst $08
        db F_READDIR
        jr c,.read_fail
        or a
        jp z,.done
        ld a,(curDepth)
        call inc_lfn_index

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

        ; NextZXOS directory handles cannot safely stay open across recursion on
        ; all setups. Store our current entry index, close the handle, recurse,
        ; then reopen and skip back to the saved position.
        ld a,(curDepth)
        push af
        call save_close_lfn_pos
        jr c,.restore_fail_lfn
        ld a,(curDepth)
        inc a
        call copy_dir
        ld (childResult),a
        ld a,0
        jr nc,.copy_child_ok
        inc a
.copy_child_ok
        ld (childCarry),a
        pop af
        ld (curDepth),a
        ld a,(childCarry)
        or a
        ld a,(childResult)
        jr nz,.dir_fail
        call reopen_lfn_pos
        jr c,.dir_fail
        jr .next

.restore_fail_lfn
        pop af
        ld (curDepth),a
        jr .read_fail

.dir_fail
        scf
        ret

.file
        ; For files, child source/destination paths were already built from the
        ; current LFN entry. Existing destination files may be skipped unless -y.
        call inc_file_count
        ld de,copyPhaseTxt
        call print_counter_status
        call copy_child_file
        jr c,.read_fail
        ld hl,(cpFiles)
        inc hl
        ld (cpFiles),hl
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

.cancel
        ld a,(lfnHandle)
        rst $08
        db F_CLOSE
        ld a,$7c
        scf
        ret

.depth_fail
        ld a,$7f
        ld (failStage),a
        ld a,$7f
        scf
        ret


clear_dir_index
        call get_dir_pos_slot
        ld (hl),0
        inc hl
        ld (hl),0
        ret


clear_lfn_index
        call get_lfn_pos_slot
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


dec_dir_index
        call get_dir_pos_slot
        ld a,(hl)
        or a
        jr nz,.dec_low
        inc hl
        dec (hl)
        dec hl
.dec_low
        dec (hl)
        ret


inc_lfn_index
        call get_lfn_pos_slot
        inc (hl)
        ret nz
        inc hl
        inc (hl)
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


save_close_lfn_pos
        ; Save/close the current LFN directory handle before recursing.
        ld a,STAGE_SAVE_POS
        ld (failStage),a
        ld a,(lfnHandle)
        rst $08
        db F_CLOSE
        ret


reopen_lfn_pos
        ; Reopen current directory and skip entries until we reach the saved
        ; position. This avoids keeping nested directory handles alive.
        ld a,STAGE_REOPEN_LFN
        ld (failStage),a
        ld a,(curDepth)
        call get_src_path
        ld a,"*"
        ld b,MODE_LFN_DIR
        rst $08
        db F_OPENDIR
        ret c
        ld (lfnHandle),a
        ld a,(curDepth)
        call get_lfn_pos_slot
        ld a,(lfnHandle)
        ld de,LFN_ENTRY
        call skip_saved_entries
        ret nc
        push af
        ld a,(lfnHandle)
        rst $08
        db F_CLOSE
        pop af
        ret


save_close_del_pos
        ld a,(delHandle)
        rst $08
        db F_CLOSE
        ret


reopen_del_pos
        ld a,(curDepth)
        call get_src_path
        ld b,0
        ld a,"*"
        rst $08
        db F_OPENDIR
        ret c
        ld (delHandle),a
        ld a,(curDepth)
        call get_dir_pos_slot
        ld a,(delHandle)
        ld de,DIR_ENTRY
        call skip_saved_entries
        ret nc
        push af
        ld a,(delHandle)
        rst $08
        db F_CLOSE
        pop af
        ret


save_close_count_pos
        ld a,(countHandle)
        rst $08
        db F_CLOSE
        ret


reopen_count_pos
        ld a,(curDepth)
        call get_src_path
        ld b,0
        ld a,"*"
        rst $08
        db F_OPENDIR
        ret c
        ld (countHandle),a
        ld a,(curDepth)
        call get_dir_pos_slot
        ld a,(countHandle)
        ld de,DIR_ENTRY
        call skip_saved_entries
        ret nc
        push af
        ld a,(countHandle)
        rst $08
        db F_CLOSE
        pop af
        ret


; A=handle, DE=entry buffer, HL=stored 16-bit entry count.
skip_saved_entries
        ; Used by directory delete/count/copy after reopening a directory.
        ; Reads and discards exactly the saved number of entries.
        ld (skipHandle),a
        ld (skipBuffer),de
        ld c,(hl)
        inc hl
        ld b,(hl)
        ld (skipCount),bc
.loop
        ld bc,(skipCount)
        ld a,b
        or c
        ret z
        ld hl,(skipBuffer)
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


build_child_paths
        ; Build next-depth source and destination paths from the current LFN name.
        ; The child name begins at LFN_ENTRY+1.
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
        ; Copy a single file while inside a directory traversal. Reads into the
        ; 8 KiB buffer at COPY_BUFFER, then writes exactly `lastReadLen` bytes.
        ; BREAK is checked between read/write operations, never in the middle of
        ; a DOS call.
        call init_file_progress
        call confirm_child_overwrite
        ret c
        or a
        ret z
        ld a,(curDepth)
        inc a
        call get_src_path
        xor a
        ld b,MODE_READ_EXIST
        ld a,STAGE_SRC_OPEN
        ld (failStage),a
        ld a,"*"
        rst $08
        db F_OPEN
        ret c
        ld (srcHandle),a

        ld a,(curDepth)
        inc a
        call get_dst_path
        xor a
        ld b,MODE_WRITE_TRUNC
        ld a,STAGE_DST_OPEN
        ld (failStage),a
        ld a,"*"
        rst $08
        db F_OPEN
        jr c,.dst_fail
        ld (dstHandle),a

.loop
        call call_cancel
        cp 1
        jp z,.cancel
        ld a,(srcHandle)
        ld hl,COPY_BUFFER
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

        call call_cancel
        cp 1
        jp z,.cancel
        ld a,(dstHandle)
        ld hl,COPY_BUFFER
        ld a,STAGE_FILE_WRITE
        ld (failStage),a
        ld bc,(lastReadLen)
        ld a,(dstHandle)
        rst $08
        db F_WRITE
        jr c,.copy_fail
        call add_copied_bytes
        call print_file_progress
        call call_cancel
        cp 1
        jp z,.cancel
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
        ld a,"*"
        rst $08
        db F_UNLINK
        ld a,$7c
        scf
        ret


; Returns NC,A=1 to copy, NC,A=0 to skip, C,A=$7c to cancel.
confirm_child_overwrite
        ld a,(curDepth)
        inc a
        call get_dst_path
        ld a,"*"
        ld b,MODE_READ_EXIST
        rst $08
        db F_OPEN
        jr c,.copy
        rst $08
        db F_CLOSE
        ld hl,LFN_ENTRY+1
        call call_overwrite
        cp 1
        jr z,.cancel
        cp "n"
        jr z,.skip
        cp 2
        jr z,.copy
        cp 13
        jr z,.copy
.skip
        xor a
        ret
.copy
        ld a,1
        or a
        ret
.cancel
        ld a,$7c
        scf
        ret


; A = depth. Deletes files first, then directories bottom-up.
delete_dir
        cp MAX_DEPTH+1
        jp nc,.depth_fail
        ld (curDepth),a

        call get_src_path
        ld b,0
        ld a,"*"
        rst $08
        db F_OPENDIR
        ret c
        ld (delHandle),a
        ld a,(curDepth)
        call clear_dir_index

.next
        call call_cancel
        cp 1
        jp z,.cancel
        ld a,(delHandle)
        ld hl,DIR_ENTRY
        rst $08
        db F_READDIR
        jp c,.read_fail
        or a
        jp z,.done
        ld a,(curDepth)
        call inc_dir_index

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
        call save_close_del_pos
        jr c,.restore_fail_del
        ld a,(curDepth)
        inc a
        call delete_dir
        pop af
        ld (curDepth),a
        jr c,.dir_fail
        ld a,(curDepth)
        call dec_dir_index
        call reopen_del_pos
        jr c,.dir_fail
        jr .next

.dir_fail
        scf
        ret

.restore_fail_del
        pop af
        ld (curDepth),a
        jr .read_fail

.file
        call inc_file_count
        ld de,deletePhaseTxt
        call print_counter_status
        ld a,(curDepth)
        inc a
        call get_src_path
        ld a,"*"
        rst $08
        db F_UNLINK
        jr c,.read_fail
        ld a,(curDepth)
        call dec_dir_index
        jr .next

.done
        ld a,(delHandle)
        rst $08
        db F_CLOSE
        jr c,.close_fail

        ld a,(curDepth)
        call get_src_path
        ld a,"*"
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

.cancel
        ld a,(delHandle)
        rst $08
        db F_CLOSE
        ld a,$7c
        scf
        ret

.depth_fail
        ld a,$7f
        scf
        ret


; HL = parent path, DE = output path, BC = child name.
build_path
        ; HL=parent, DE=destination buffer, BC=child name.
        ; Inserts one slash between parent and child unless parent already ends
        ; in '/', '\' or ':' (drive root such as "d:").
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
        ; Shared bounds-checked writer for build_path. Leaves carry set on
        ; overflow; callers convert that to a normal command failure.
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
        ; Skip "." and ".." in short-name directory entries.
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
        ; Skip "." and ".." in LFN directory entries.
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
        ; A=depth -> HL points to SRC_STACK + depth*256.
        add a,$C0
        ld h,a
        ld l,0
        ret


get_dst_path
        ; A=depth -> HL points to DST_STACK + depth*256.
        add a,$CC
        ld h,a
        ld l,0
        ret


esx_mkdir_ignore
        ; Create the destination directory for the current depth. Failure is
        ; ignored here because "already exists" is expected during copies.
        ld a,"*"
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
        ; Pre-count entries for progress display. This is a second directory
        ; traversal before the real copy/delete work.
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
        ; Short-name traversal is enough for counting. Actual copy uses LFN mode
        ; so long child names are preserved.
        cp MAX_DEPTH+1
        jp nc,.depth_fail
        ld (curDepth),a

        call get_src_path
        ld b,0
        ld a,"*"
        rst $08
        db F_OPENDIR
        ret c
        ld (countHandle),a
        ld a,(curDepth)
        call clear_dir_index
        call inc_total_count

.next
        call call_cancel
        cp 1
        jr z,.cancel
        ld a,(countHandle)
        ld hl,DIR_ENTRY
        rst $08
        db F_READDIR
        jr c,.read_fail
        or a
        jr z,.done
        ld a,(curDepth)
        call inc_dir_index

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
        call save_close_count_pos
        jr c,.restore_fail_count
        ld a,(curDepth)
        inc a
        call count_dir
        pop af
        ld (curDepth),a
        jr c,.dir_fail
        call reopen_count_pos
        jr c,.dir_fail
        jr .next

.dir_fail
        scf
        ret

.restore_fail_count
        pop af
        ld (curDepth),a
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

.cancel
        ld a,(countHandle)
        rst $08
        db F_CLOSE
        ld a,$7c
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
        ld l,(ix+SYSCOPY_SERVICE_OVERWRITE)
        ld h,(ix+SYSCOPY_SERVICE_OVERWRITE+1)
        ld (call_overwrite+1),hl
        ret


print_header
        ld hl,0*256+0
        ld a,16
        ld de,msgTitle
        call call_print
        ld hl,0*256+2
        ld a,16
        ld de,msgSource
        call call_print
        ld hl,8*256+2
        ld a,16
        ld de,srcPath
        call call_print
        ld hl,0*256+3
        ld a,16
        ld de,msgDestiny
        call call_print
        ld hl,9*256+3
        ld a,16
        ld de,dstPath
        call call_print
        ld hl,0*256+5
        ld a,16
        ld de,blankNameTxt
        call call_print
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
        jr .print

        ld ix,(ctxPtr)
        ld l,(ix+SYSCOPYCTX_FILES_LO)
        ld h,(ix+SYSCOPYCTX_FILES_LO+1)
        ld a,h
        or l
        jr z,.print
        ld a,h
        or a
        jr nz,.check_total
        ld a,l
        cp 1
        jr z,.print
.check_total
        ld de,(totalFiles)
        or a
        sbc hl,de
        jr z,.print
        pop ix
        pop hl
        pop de
        pop bc
        pop af
        ret

.print
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
        call write_dec16
        ld a,"/"
        ld (de),a
        inc de
        ld hl,(totalFiles)
        call write_dec16
        ld a,")"
        ld (de),a
        inc de

        ld hl,0*256+7
        ld a,16
        ld de,statusLine
        call call_print
        pop ix
        pop hl
        pop de
        pop bc
        pop af
        ret


clear_copy_display
        push af
        push de
        push hl
        ld hl,0*256+7
        ld a,16
        ld de,msgBlank
        call call_print
        ld hl,0*256+8
        ld a,16
        ld de,msgBlank
        call call_print
        pop hl
        pop de
        pop af
        ret


print_copy_result
        ld hl,0*256+20
        jr print_copy_result_core

print_copy_result_r21
        ld hl,0*256+21

print_copy_result_core
        push af
        push bc
        push de
        push hl
        ld de,statusLine
        ld hl,msgCopied
.cp1    ld a,(hl)
        or a
        jr z,.cp1done
        ld (de),a
        inc hl
        inc de
        jr .cp1
.cp1done
        ld hl,(cpFiles)
        call write_dec16
        ld hl,msgFiles
.cp2    ld a,(hl)
        or a
        jr z,.cp2done
        ld (de),a
        inc hl
        inc de
        jr .cp2
.cp2done
        ld hl,(cpDirs)
        call write_dec16
        ld hl,msgDirs
.cp3    ld a,(hl)
        or a
        jr z,.cp3done
        ld (de),a
        inc hl
        inc de
        jr .cp3
.cp3done
        xor a
        ld (de),a
        pop hl
        ld a,16
        ld de,statusLine
        call call_print
        pop de
        pop bc
        pop af
        ret


init_file_progress
        ld hl,0
        ld (fileCopiedBytes),hl
        ld (fileCopiedBytes+2),hl
        ld hl,$FFFF
        ld (lastPrintedKb),hl
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

        ld hl,fileCopiedBytes
        call bytes_to_kb
        ld de,(lastPrintedKb)
        or a
        sbc hl,de
        jr z,.skip_print
        add hl,de
        ld (lastPrintedKb),hl

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
        call bytes_to_kb
        pop de
        call write_dec16
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
        call bytes_to_kb
        pop de
        call write_dec16
        ld a,"k"
        ld (de),a
        inc de
        ld a,"B"
        ld (de),a
        inc de
        ld a,"]"
        ld (de),a

        ld hl,0*256+8
        ld a,16
        ld de,fileStatusLine
        call call_print
        jr .done
.skip_print
.done
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


; HL points to 32-bit byte count. Returns HL = count / 1024 in kB.
bytes_to_kb
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
        jr z,.done
        ld hl,65535
.done
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


; HL=value, DE=destination. Writes unsigned decimal without leading spaces.
write_dec16
        xor a
        ld (decStarted),a
        ld bc,10000
        call write_dec_digit
        ld bc,1000
        call write_dec_digit
        ld bc,100
        call write_dec_digit
        ld bc,10
        call write_dec_digit
        ld a,l
        add a,"0"
        ld (de),a
        inc de
        ret


write_dec_digit
        ld a,"0"-1
.count
        inc a
        or a
        sbc hl,bc
        jr nc,.count
        add hl,bc
        cp "0"
        jr nz,.emit
        push af
        ld a,(decStarted)
        or a
        jr nz,.emit_popped
        pop af
        ret
.emit_popped
        pop af
.emit
        ld (de),a
        inc de
        ld a,1
        ld (decStarted),a
        ret


call_print
        jp dot_print


call_cancel
        jp dot_cancel


call_overwrite
        jp dot_overwrite


dot_print
        push af
        push bc
        push de
        push hl
        call set_cursor
        ex de,hl
        call print_msg_count
        ld a,b
        cp SCREEN_LINE_LEN
        jr nc,.done
        ld a,SCREEN_LINE_LEN
        sub b
        ld b,a
.pad
        ld a,b
        or a
        jr z,.done
        ld a,32
        rst 16
        djnz .pad
.done
        pop hl
        pop de
        pop bc
        pop af
        ret


dot_cancel
        ; BREAK = CAPS SHIFT + SPACE on the Spectrum keyboard matrix.
        ld bc,$FEFE
        in a,(c)
        bit 0,a
        jr nz,.no
        ld bc,$7FFE
        in a,(c)
        bit 0,a
        jr nz,.no
        ld a,1
        ret
.no
        xor a
        ret


dot_overwrite
        ; Standalone command overwrite policy:
        ;   -y -> copy/overwrite
        ;   otherwise -> skip and show the skipped filename
        ld a,(yesFlag)
        or a
        jr nz,.copy
        push hl
        ld de,msgSkip
        ld hl,0*256+10
        ld a,16
        call call_print
        pop hl
        ld de,hl
        ld hl,0*256+11
        ld a,16
        call call_print
        xor a
        ret
.copy
        ld a,2
        ret


print_msg
        ld a,(hl)
        or a
        ret z
        cp 255
        ret z
        rst 16
        inc hl
        jr print_msg


; Prints zero/255 terminated text and returns B=printed character count.
print_msg_count
        ld b,0
.loop
        ld a,b
        cp SCREEN_LINE_LEN
        ret nc
        ld a,(hl)
        or a
        ret z
        cp 255
        ret z
        rst 16
        inc b
        inc hl
        jr .loop


print_nl
        ld a,13
        rst 16
        ret


print_hex8
        push af
        rrca
        rrca
        rrca
        rrca
        call print_hex_nibble
        pop af
print_hex_nibble
        and $0F
        add a,"0"
        cp "9"+1
        jr c,.ok
        add a,7
.ok
        rst 16
        ret


clear_screen
        ; Clear bitmap and attributes directly with LDIR. This avoids ROM CLS
        ; side effects and scroll prompts.
        ld hl,16384
        ld de,16385
        ld bc,6143
        ld (hl),l
        ldir
        ld hl,22528
        ld de,22529
        ld bc,767
        ld (hl),56
        ldir
        ret


install_error_handler
        ; Some ROM3 errors (for example invalid/missing drive on certain
        ; emulators) do not return through carry. M_ERRH lets us route those
        ; back through the normal failure path instead of dropping to BASIC.
        ld hl,rom_error_handler
        rst $08
        db M_ERRH
        ret


clear_error_handler
        ld hl,0
        rst $08
        db M_ERRH
        ret


rom_error_handler
        ; Entered by NextZXOS after a ROM/BASIC error. A contains BASIC error-1,
        ; but for this command any such error during path probing is presented
        ; as a missing destination path.
        ld a,STAGE_DST_PARENT
        ld (failStage),a
        ld a,$7d
        jp failed


set_turbo_28
        ; Copy/delete runs at 28 MHz. The previous turbo setting is restored
        ; before returning to NextZXOS/BASIC.
        push af
        push bc
        ld a,TURBO_CONTROL_NR_07
        call read_nextreg
        ld (savedTurbo),a
        ld a,1
        ld (turboEnabled),a
        ld bc,TBBLUE_REGISTER_SELECT_P_243B
        ld a,TURBO_CONTROL_NR_07
        out (c),a
        ld bc,TBBLUE_REGISTER_ACCESS_P_253B
        ld a,3
        out (c),a
        pop bc
        pop af
        ret


restore_mmu
        push af
        push bc
        ld bc,TBBLUE_REGISTER_SELECT_P_243B
        ld a,$56
        out (c),a
        ld bc,TBBLUE_REGISTER_ACCESS_P_253B
        ld a,(savedMmu6)
        out (c),a
        ld bc,TBBLUE_REGISTER_SELECT_P_243B
        ld a,$57
        out (c),a
        ld bc,TBBLUE_REGISTER_ACCESS_P_253B
        ld a,(savedMmu7)
        out (c),a
        pop bc
        pop af
        ret


restore_turbo
        push af
        push bc
        ld a,(turboEnabled)
        or a
        jr z,.done
        xor a
        ld (turboEnabled),a
        ld bc,TBBLUE_REGISTER_SELECT_P_243B
        ld a,TURBO_CONTROL_NR_07
        out (c),a
        ld bc,TBBLUE_REGISTER_ACCESS_P_253B
        ld a,(savedTurbo)
        out (c),a
.done
        pop bc
        pop af
        ret


read_nextreg
        push bc
        ld bc,TBBLUE_REGISTER_SELECT_P_243B
        out (c),a
        ld bc,TBBLUE_REGISTER_ACCESS_P_253B
        in a,(c)
        pop bc
        ret


; HL uses the Calm Commander convention: H=X, L=Y.
set_cursor
        push af
        ld a,22
        rst 16
        ld a,l
        rst 16
        ld a,h
        rst 16
        pop af
        ret


debug_print_msg
        ld a,DEBUG_OUTPUT
        or a
        ret z
        jp print_msg


debug_print_nl
        ld a,DEBUG_OUTPUT
        or a
        ret z
        jp print_nl


debug_print_hex8
        push af
        ld a,DEBUG_OUTPUT
        or a
        jr nz,.enabled
        pop af
        ret
.enabled
        pop af
        jp print_hex8


debug_print_args
        ld a,DEBUG_OUTPUT
        or a
        ret z
        push hl
        ld hl,msgDbgArgs
        call print_msg
        pop hl
        call print_arg_string
        call print_nl
        ret


debug_print_parsed
        ld a,DEBUG_OUTPUT
        or a
        ret z
        push af
        push hl
        ld hl,msgDbgSrc
        call print_msg
        ld hl,srcPath
        call print_msg
        call print_nl
        ld hl,msgDbgDst
        call print_msg
        ld hl,dstPath
        call print_msg
        call print_nl
        ld hl,msgDbgPos
        call print_msg
        ld a,(posIndex)
        call print_hex8
        call print_nl
        pop hl
        pop af
        ret


print_arg_string
        ld b,96
.loop
        ld a,(hl)
        or a
        ret z
        cp 13
        ret z
        cp $A5
        ret nc
        cp 32
        jr nc,.print
        ld a,"."
.print
        rst $10
        inc hl
        djnz .loop
        ret


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
childResult defb 0
childCarry  defb 0
skipHandle  defb 0
skipBuffer  defw 0
skipCount   defw 0
fileCopiedBytes defd 0
fileTotalBytes defd 0
lastPrintedKb defw 0
savedTurbo  defb 0
turboEnabled defb 0
savedMmu6   defb 0
savedMmu7   defb 0
cpFiles     defw 0
cpDirs      defw 0
fileNameText defs 21
decStarted defb 0
dirPosStack defs (MAX_DEPTH+1)*4
lfnPosStack defs (MAX_DEPTH+1)*4

argPtr      defw 0
splitSrc    defw 0
splitParent defw 0
splitName   defw 0
lastSep     defw 0
moveFlag    defb 0
yesFlag     defb 0
helpFlag    defb 0
posIndex    defb 0

dotServices
        defw dot_print
        defw dot_cancel
        defw dot_overwrite

dotCtx      defs SYSCOPYCTX_SIZE

dotTxt      defb ".",0
msgDone     defb "Done.",0
msgError    defb "copy: esxDOS error $",0
msgStage    defb " stg $",0
msgDestMissing defb "copy: destiny path not found",0
msgCancelled defb "Cancelled.",0
msgSkip     defb "Skipping existing:   ",0
msgBlank    defb "                                ",0
msgCopied   defb "Copied: ",0
msgFiles    defb " files, ",0
msgDirs     defb " dirs",0
msgUsageError defb "copy: missing or bad arguments",13,0
msgTitle    defb ".copy - by Shrek/MB Maniax 2026",0
msgSource   defb "Source:",0
msgDestiny  defb "Destiny:",0
msgDbgArgs  defb 13,"DBG args: ",0
msgDbgSrc   defb "DBG src : ",0
msgDbgDst   defb "DBG dst : ",0
msgDbgPos   defb "DBG pos : $",0
msgDbgSetup defb "DBG setup",13,0
msgDbgSetupStart defb "DBG setup start",13,0
msgDbgSplitSrc defb "DBG split src",13,0
msgDbgSplitDst defb "DBG split dst",13,0
msgDbgSourceFile defb "DBG source=file",13,0
msgDbgOpenErr defb "DBG opendir err $",0
msgDbgSourceDir defb "DBG source=dir",13,0
msgDbgBeforeSource defb "DBG before source",13,0
msgDbgRoot  defb "DBG root paths",13,0
msgDbgRootOk defb "DBG root ok",13,0
msgHelp
        defb ".copy - by Shrek/MB Maniax 2026",13
        defb 13
        defb "Usage:",13
        defb "  .copy -s source -d destiny",13
        defb "  .copy source destiny",13
        defb "Options:",13
        defb "  -m  move after successful copy",13
        defb "  -y  overwrite existing files",13
        defb 13
        defb "If destiny ends with / or is an existing"
        defb "directory, source name is appended.",13
        defb 13
        defb "Examples:",13
        defb "  .copy zaloha backup",13
        defb "  .copy -s ",34,"my dir",34," -d ",34,"d:/my backup",34,13
        defb 13
        defb "Work buffer: $DC00/56320, 8192b",13

copyPhaseTxt   defb "Copying: ",0
deletePhaseTxt defb "Deleting: ",0
statusLine     defs 56
fileStatusLine defs 56
blankNameTxt   defb "BREAK = cancel ",0

srcPath     defs 256
dstPath     defs 256
srcParent   defs 256
dstParent   defs 256
srcName     defs 256
dstName     defs 256
dstFullPath defs 256

plugin_end
        assert plugin_end - MAIN <= 8192
        SAVEBIN "extra/copy", MAIN, plugin_end - MAIN
        END MAIN
