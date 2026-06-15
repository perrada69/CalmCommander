; -----------------------------------------------------------------------------
; .dirinfo - standalone NextZXOS dot command
; -----------------------------------------------------------------------------
; Shows recursive information about a file or directory. The command is intended
; as a small companion to Calm Commander: it accepts quoted paths, uses LFN
; directory reads, can be cancelled with BREAK, and avoids ROM CLS/scroll prompts.
; -----------------------------------------------------------------------------

        DEVICE ZXSPECTRUMNEXT
        org $2000

F_CLOSE     equ $9B
F_OPENDIR   equ $A3
F_READDIR   equ $A4
F_STAT      equ $AC
M_ERRH      equ $95

MODE_LFN_DIR equ $10
ATTR_DIR     equ $10
MAX_DEPTH    equ 11
PLUGIN_STACK equ $BFFE

TBBLUE_REGISTER_SELECT_P_243B equ $243B
TBBLUE_REGISTER_ACCESS_P_253B equ $253B
TURBO_CONTROL_NR_07 equ $07

PATH_STACK   equ $C000                 ; 12 * 256 byte path slots
DIR_ENTRY    equ $D000                 ; 512 byte F_READDIR buffer
STAT_BUFFER  equ $D300                 ; 16 byte F_STAT buffer
STATUS_LINE_LEN equ 45
SCREEN_LINE_LEN equ 32

STAGE_OPEN   equ $20
STAGE_READ   equ $21
STAGE_CHILD  equ $22
STAGE_STAT   equ $30

MAIN
        ld (argPtr),hl
        ld (savedSp),sp
        ld sp,PLUGIN_STACK
        call install_error_handler
        call clear_screen
        call clear_state
        ld hl,(argPtr)
        call parse_args
        jp c,show_usage_error
        ld a,(helpFlag)
        or a
        jp nz,show_help_ok

        ld a,(targetPath)
        or a
        jr nz,.have_target
        ld hl,dotPath
        ld de,targetPath
        call copy_string
.have_target
        call trim_trailing_slash
        call print_header
        call set_turbo_28

        call is_target_directory
        jr nc,.is_dir
        call stat_target
        jp c,failed
        call prepare_file_result
        call print_file_result
        jp success

.is_dir
        ld a,0
        call copy_root_path
        ld a,0
        call count_dir
        jp c,failed
        call clear_scan_display
        call print_dir_result
        jp success


success
        ld sp,(savedSp)
        call clear_error_handler
        call restore_turbo
        ld bc,0
        xor a
        ret


failed
        ld (lastError),a
        ld sp,(savedSp)
        call clear_error_handler
        call restore_turbo
        ld hl,0*256+20
        ld de,msgError
        call print_at
        ld a,(lastError)
        call print_hex8
        ld hl,msgStage
        call print_msg
        ld a,(failStage)
        call print_hex8
        ld bc,0
        xor a
        ret


cancelled
        ld sp,(savedSp)
        call clear_error_handler
        call restore_turbo
        ld hl,0*256+20
        ld de,msgCancelled
        call print_at
        ld bc,0
        xor a
        ret


show_usage_error
        call print_header_title
        ld hl,msgBadArgs
        call print_msg
        call print_nl
        call show_help_body
        call clear_error_handler
        ld sp,(savedSp)
        ld bc,0
        xor a
        ret


show_help_ok
        call print_header_title
        call show_help_body
        call clear_error_handler
        ld sp,(savedSp)
        ld bc,0
        xor a
        ret


; -----------------------------------------------------------------------------
; Argument parser
; -----------------------------------------------------------------------------

clear_state
        xor a
        ld (helpFlag),a
        ld (posIndex),a
        ld (failStage),a
        ld (targetPath),a
        ld hl,0
        ld (fileCount),hl
        ld (fileCount+2),hl
        ld (dirCount),hl
        ld (dirCount+2),hl
        ld (totalBytes),hl
        ld (totalBytes+2),hl
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
        cp "S"
        jr z,.source
        scf
        ret
.help
        ld a,1
        ld (helpFlag),a
        or a
        ret
.source
        call skip_separators
        ld de,targetPath
        call extract_token
        ret c
        jr .next


store_positional
        ld a,(posIndex)
        or a
        jr z,.first
        scf
        ret
.first
        call is_command_name_token
        jr c,.skip_command_name
        ld a,1
        ld (posIndex),a
        ld de,targetPath
        jp extract_token
.skip_command_name
        call skip_token
        call skip_spaces
        jp store_positional


is_command_name_token
        push hl
        ld a,(hl)
        cp "."
        jr nz,.maybe
        inc hl
.maybe
        ld de,cmdName
.loop
        ld a,(de)
        or a
        jr z,.end_name
        ld c,a
        ld a,(hl)
        and $DF
        cp c
        jr nz,.no
        inc hl
        inc de
        jr .loop
.end_name
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


trim_trailing_slash
        ld hl,targetPath
        ld de,targetPath
.scan
        ld a,(hl)
        or a
        jr z,.found_end
        inc hl
        jr .scan
.found_end
        or a
        sbc hl,de
        ret z
        add hl,de
        dec hl
        ld a,(hl)
        cp "/"
        jr z,.maybe_trim
        cp 92
        ret nz
.maybe_trim
        dec hl
        ld a,(hl)
        cp ":"
        ret z
        inc hl
        xor a
        ld (hl),a
        ret


copy_string
        ld a,(hl)
        ld (de),a
        inc hl
        inc de
        or a
        jr nz,copy_string
        ret


; -----------------------------------------------------------------------------
; File/directory scanning
; -----------------------------------------------------------------------------

stat_target
        ld a,STAGE_STAT
        ld (failStage),a
        ld hl,targetPath
        ld de,STAT_BUFFER
        ld a,"*"
        rst $08
        db F_STAT
        ret


is_target_directory
        ld a,STAGE_OPEN
        ld (failStage),a
        ld hl,targetPath
        ld a,"*"
        ld b,MODE_LFN_DIR
        rst $08
        db F_OPENDIR
        ret c
        rst $08
        db F_CLOSE
        xor a
        ret


prepare_file_result
        ld hl,STAT_BUFFER+7
        ld de,totalBytes
        ld bc,4
        ldir
        ld hl,1
        ld (fileCount),hl
        xor a
        ret


copy_root_path
        call get_path_for_depth
        ex de,hl
        ld hl,targetPath
        jp copy_string


; A = depth.
count_dir
        cp MAX_DEPTH+1
        jp nc,.depth_fail
        ld (curDepth),a

        call print_current_dir
        call get_path_for_depth
        ld b,MODE_LFN_DIR
        ld a,STAGE_OPEN
        ld (failStage),a
        ld a,"*"
        rst $08
        db F_OPENDIR
        ret c
        ld (dirHandle),a
        ld a,(curDepth)
        call clear_dir_index

.next
        call check_cancel
        cp 1
        jp z,.cancel
        call print_read_marker
        ld hl,DIR_ENTRY
        ld a,STAGE_READ
        ld (failStage),a
        ld a,(dirHandle)
        rst $08
        db F_READDIR
        jp c,.read_fail
        or a
        jr z,.done
        ld a,(curDepth)
        call inc_dir_index
        call skip_dot_lfn_entry
        jr z,.next
        call print_current_item

        ld a,(curDepth)
        cp MAX_DEPTH
        jr nc,.count_only
        ld a,STAGE_CHILD
        ld (failStage),a
        call build_child_path
        jr c,.read_fail
.count_only
        ld a,(DIR_ENTRY)
        and ATTR_DIR
        jr nz,.dir
        call inc_file_count
        call add_entry_size
        jr .next

.dir
        call inc_dir_count
        ld a,(curDepth)
        cp MAX_DEPTH
        jr nc,.next
        push af
        call save_close_dir_pos
        jr c,.restore_fail
        ld a,(curDepth)
        inc a
        call count_dir
        ld (childResult),a
        ld a,0
        jr nc,.child_ok
        inc a
.child_ok
        ld (childCarry),a
        pop af
        ld (curDepth),a
        ld a,(childCarry)
        or a
        ld a,(childResult)
        jr nz,.dir_fail
        ld a,(curDepth)
        call reopen_dir_pos
        jr c,.dir_fail
        jp .next

.done
        ld a,(dirHandle)
        rst $08
        db F_CLOSE
        xor a
        ret

.cancel
        ld a,(dirHandle)
        rst $08
        db F_CLOSE
        ld a,$7c
        jp cancelled

.restore_fail
        pop af
        ld (curDepth),a
.read_fail
        push af
        ld a,(dirHandle)
        rst $08
        db F_CLOSE
        pop af
        scf
        ret

.dir_fail
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
        jp build_path


; HL = parent, DE = output, BC = child name.
build_path
        ld (namePtr),bc          ; save name pointer before put_path_char corrupts C
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
        ld hl,(namePtr)          ; restore name pointer (C was corrupted by put_path_char)
.name_loop
        ld a,(hl)
        or a
        jr z,.name_done
        cp 255
        jr z,.name_done
        call put_path_char
        ret c
        inc hl
        jr .name_loop
.name_done
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


skip_dot_lfn_entry
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
        add a,$C0
        ld h,a
        ld l,0
        ret


depth_to_offset2
        ld e,a
        ld d,0
        sla e
        rl d
        ret


get_dir_pos_slot
        call depth_to_offset2
        ld hl,dirPosStack
        add hl,de
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


save_close_dir_pos
        ld a,(dirHandle)
        rst $08
        db F_CLOSE
        ret


reopen_dir_pos
        call get_path_for_depth
        ld b,MODE_LFN_DIR
        ld a,"*"
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


; A=handle, HL=stored 16-bit entry count. Uses DIR_ENTRY as scratch.
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
        ld hl,DIR_ENTRY
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


inc_file_count
        ld hl,fileCount
        jp inc_u32_at_hl


inc_dir_count
        ld hl,dirCount
        jp inc_u32_at_hl


inc_u32_at_hl
        inc (hl)
        ret nz
        inc hl
        inc (hl)
        ret nz
        inc hl
        inc (hl)
        ret nz
        inc hl
        inc (hl)
        ret


add_entry_size
        ; Size field offset in LFN-mode READDIR buffer is TBD.
        ; F_STAT corrupts the open dir handle position, so skip for now.
        ret


; -----------------------------------------------------------------------------
; Output
; -----------------------------------------------------------------------------

print_header_title
        call clear_screen
        ld hl,msgTitle
        call print_msg
        call print_nl
        call print_nl
        ret


print_header
        ld hl,0*256+0
        ld de,msgTitle
        call print_at
        ld hl,0*256+2
        ld de,msgTarget
        call print_at
        ld hl,8*256+2
        ld de,targetPath
        call print_at
        ld hl,0*256+4
        ld de,msgBreak
        call print_at
        ret


print_current_dir
        push af
        push bc
        push de
        push hl
        ld hl,statusLine
        ld b,STATUS_LINE_LEN
.clear
        ld (hl),32
        inc hl
        djnz .clear
        xor a
        ld (hl),a
        ld hl,statusLine
        ld de,msgScanning
.copy_text
        ld a,(de)
        or a
        jr z,.path
        ld (hl),a
        inc hl
        inc de
        jr .copy_text
.path
        ld a,(curDepth)
        call get_path_for_depth
        ld de,hl
        ld hl,statusLine+10
        ld b,22
.path_loop
        ld a,(de)
        or a
        jr z,.done
        ld (hl),a
        inc de
        inc hl
        djnz .path_loop
.done
        ld hl,0*256+6
        ld de,statusLine
        call print_at
        pop hl
        pop de
        pop bc
        pop af
        ret


print_read_marker
        push af
        push bc
        push de
        push hl
        ld hl,0*256+7
        ld de,msgReading
        call print_at
        pop hl
        pop de
        pop bc
        pop af
        ret


print_current_item
        push af
        push bc
        push de
        push hl
        ld hl,statusLine
        ld b,STATUS_LINE_LEN
.clear
        ld (hl),32
        inc hl
        djnz .clear
        xor a
        ld (hl),a
        ld hl,statusLine
        ld de,msgItem
.copy_text
        ld a,(de)
        or a
        jr z,.name
        ld (hl),a
        inc hl
        inc de
        jr .copy_text
.name
        ld de,DIR_ENTRY+1
        ld b,24
.name_loop
        ld a,(de)
        or a
        jr z,.done
        ld (hl),a
        inc de
        inc hl
        djnz .name_loop
.done
        ld hl,0*256+8
        ld de,statusLine
        call print_at
        pop hl
        pop de
        pop bc
        pop af
        ret


print_file_result
        ld hl,0*256+7
        ld de,msgTypeFile
        call print_at
        ld hl,0*256+9
        call set_cursor
        ld hl,msgSize
        call print_msg
        ld hl,totalBytes
        call print_u32
        ld hl,msgBytes
        call print_msg
        ld hl,0*256+10
        call set_cursor
        ld hl,msgAttr
        call print_msg
        ld a,(STAT_BUFFER+2)
        call print_hex8
        ret


clear_scan_display
        ld hl,0*256+6
        ld de,msgEmpty
        call print_at
        ld hl,0*256+8
        ld de,msgEmpty
        jp print_at


print_dir_result
        ld hl,0*256+7
        ld de,msgTypeDir
        call print_at
        ld hl,0*256+9
        call set_cursor
        ld hl,msgFiles
        call print_msg
        ld hl,fileCount
        call print_u32
        ld hl,0*256+10
        call set_cursor
        ld hl,msgDirs
        call print_msg
        ld hl,dirCount
        call print_u32
        ld hl,0*256+11
        call set_cursor
        ld hl,msgSize
        call print_msg
        ld hl,totalBytes
        call print_u32
        ld hl,msgBytes
        call print_msg
        ret


show_help_body
        ld hl,msgHelp
        jp print_msg


print_at
        push hl
        push de
        call set_cursor
        pop hl
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
        ret


set_cursor
        ld b,h
        ld c,l
        ld a,22
        rst 16
        ld a,c
        rst 16
        ld a,b
        rst 16
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


; HL points to little-endian 32-bit value.
print_u32
        ld de,numWork
        ld bc,4
        ldir
        ld hl,numWork
        call is_u32_zero
        jr nz,.convert
        ld a,"0"
        rst 16
        ret
.convert
        ld hl,decBufEnd
        ld (decPtr),hl
.loop
        call div_u32_by_10
        ld hl,(decPtr)
        dec hl
        ld (decPtr),hl
        ld a,(remByte)
        add a,"0"
        ld (hl),a
        ld hl,numWork
        call is_u32_zero
        jr nz,.loop
        ld hl,(decPtr)
        jp print_msg


is_u32_zero
        ld a,(hl)
        inc hl
        or (hl)
        inc hl
        or (hl)
        inc hl
        or (hl)
        ret


div_u32_by_10
        xor a
        ld (remByte),a
        ld b,32
.loop
        ld hl,numWork
        sla (hl)
        inc hl
        rl (hl)
        inc hl
        rl (hl)
        inc hl
        rl (hl)
        ld a,(remByte)
        rla
        ld (remByte),a
        cp 10
        jr c,.next
        sub 10
        ld (remByte),a
        ld hl,numWork
        set 0,(hl)
.next
        djnz .loop
        ret


; -----------------------------------------------------------------------------
; System helpers
; -----------------------------------------------------------------------------

check_cancel
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


clear_screen
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
        ld a,$7d
        jp failed


set_turbo_28
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


argPtr       defw 0
savedSp      defw 0
helpFlag     defb 0
posIndex     defb 0
curDepth     defb 0
dirHandle    defb 0
skipHandle   defb 0
skipCount    defw 0
lastChar     defb 0
pathLeft     defb 0
failStage    defb 0
lastError    defb 0
childResult  defb 0
childCarry   defb 0
savedTurbo   defb 0
turboEnabled defb 0
remByte      defb 0
decPtr       defw 0
namePtr      defw 0

fileCount    defs 4
dirCount     defs 4
totalBytes   defs 4
numWork      defs 4
dirPosStack  defs (MAX_DEPTH+1)*2
targetPath   defs 256
statusLine   defs STATUS_LINE_LEN+1
decBuf       defs 11
decBufEnd    defb 0

cmdName      defb "DIRINFO",0
dotPath      defb ".",0
msgEmpty     defb 0
msgTitle     defb ".dirinfo - by Shrek/MB Maniax",0
msgTarget    defb "Target: ",0
msgBreak     defb "BREAK = cancel",0
msgScanning  defb "Scanning: ",0
msgReading   defb "Reading directory...",0
msgItem      defb "Item: ",0
msgTypeFile  defb "Type: file",0
msgTypeDir   defb "Type: directory",0
msgFiles     defb "Files: ",0
msgDirs      defb "Dirs: ",0
msgSize      defb "Size: ",0
msgBytes     defb " bytes",0
msgAttr      defb "Attributes: $",0
msgError     defb "dirinfo: esxDOS error $",0
msgStage     defb " stg $",0
msgCancelled defb "Cancelled.",0
msgBadArgs   defb "dirinfo: missing or bad arguments",0
msgHelp      defb "DIRINFO 0.1 - directory info",13
             defb "Usage:",13
             defb "  .dirinfo [path]",13
             defb "  .dirinfo -s path",13
             defb "Options:",13
             defb "  -s  source/target path",13
             defb "  -h  show this help",13
             defb "Examples:",13
             defb "  .dirinfo dot",13
             defb "  .dirinfo ",34,"my dir",34,13
             defb "  .dirinfo c:/dot",13,0

plugin_end
        assert plugin_end - MAIN <= 8192
        SAVEBIN "extra/dirinfo", MAIN, plugin_end - MAIN
