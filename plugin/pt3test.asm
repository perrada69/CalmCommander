        DEVICE ZXSPECTRUMNEXT
        org VIEW_PLUGIN_ADDRESS

        include "plugin_api.i.asm"

PT3_LINEAR_ADDR equ $4000
PT3_STACK       equ $DFFE
INPUT_ARM_FRAMES equ 25

; VTPL.START references songdata. The test plugin maps the first
; two loaded data pages as one linear 16K block at $4000.
songdata equ PT3_LINEAR_ADDR

plugin_start
        ld (ctxPtr),hl
        ld (svcPtr),de
        call patch_services

        ld (savedSp),sp
        ld sp,PT3_STACK

        ld a,$52
        call read_nextreg
        ld (savedMmu2),a
        ld a,$53
        call read_nextreg
        ld (savedMmu3),a
        ld a,$54
        call read_nextreg
        ld (savedMmu4),a
        ld a,$55
        call read_nextreg
        ld (savedMmu5),a

        call prepare_pt3_info
        call show_player_screen
        call map_pt3_data
        call setup_pt3_mode

        ld a,(pt3Setup)
        ld (VTPL.SETUP),a
        ld hl,PT3_LINEAR_ADDR
        ld de,(secondModuleAddr)
        call VTPL.INIT
        call show_pt3_mode
        ei

.wait_release
        call raw_control
        or a
        jr nz,.wait_release
        call input_arm_delay

.play_loop
        call restore_mmu
        ei
        halt
        di
        call call_input
        cp 1
        jr z,.exit
        cp 2
        jr z,.next
        call map_pt3_data
        call VTPL.PLAY
        ld a,(VTPL.SETUP)
        and %11000000
        jr nz,.next
        call show_volume_meters
        ; VTPL sets SETUP bit 7 when the module loop point is passed.
        ; In looped playback mode this is informational, not a stop request.
        jr .play_loop

.exit
        call VTPL.MUTE
        call restore_mmu
        call wait_stop_release
        di
        ld sp,(savedSp)
        xor a
        ret

.next
        call VTPL.MUTE
        call restore_mmu
        call wait_stop_release
        di
        ld sp,(savedSp)
        ld a,1
        ret


map_pt3_data
        ld ix,(ctxPtr)
        ld l,(ix+VIEWCTX_DATA_PAGES)
        ld h,(ix+VIEWCTX_DATA_PAGES+1)
        ld a,(hl)
        nextreg $52,a
        ld a,(ix+VIEWCTX_PAGE_COUNT)
        cp 2
        jr c,.restore_3
        inc hl
        ld a,(hl)
        nextreg $53,a
        ld a,(ix+VIEWCTX_PAGE_COUNT)
        cp 3
        jr c,.restore_4
        inc hl
        ld a,(hl)
        nextreg $54,a
        ld a,(ix+VIEWCTX_PAGE_COUNT)
        cp 4
        jr c,.restore_5
        inc hl
        ld a,(hl)
        nextreg $55,a
        ret
.restore_3
        ld a,(savedMmu3)
        nextreg $53,a
        jr .restore_4
.restore_4
        ld a,(savedMmu4)
        nextreg $54,a
.restore_5
        ld a,(savedMmu5)
        nextreg $55,a
        ret


restore_mmu
        ld a,(savedMmu2)
        nextreg $52,a
        ld a,(savedMmu3)
        nextreg $53,a
        ld a,(savedMmu4)
        nextreg $54,a
        ld a,(savedMmu5)
        nextreg $55,a
        ret


setup_pt3_mode
        ld a,%00100001          ; PT3, ABC, no loop, AlCo TS autodetect
        ld (pt3Setup),a
        ld hl,0
        ld (secondModuleAddr),hl
        ld ix,(ctxPtr)
        ld a,(ix+VIEWCTX_SIZE_HI)
        or (ix+VIEWCTX_SIZE_HI+1)
        ret nz
        ld l,(ix+VIEWCTX_SIZE_LO)
        ld h,(ix+VIEWCTX_SIZE_LO+1)
        ld a,h
        cp $80
        ret nc
        ld a,h
        or l
        ret z
        dec hl
        ld de,PT3_LINEAR_ADDR
        add hl,de
        ld a,(hl)
        cp "S"
        ret nz
        dec hl
        ld a,(hl)
        cp "T"
        ret nz
        dec hl
        ld a,(hl)
        cp "2"
        ret nz
        dec hl
        ld a,(hl)
        cp "0"
        ret nz
        ld de,8
        or a
        sbc hl,de
        ld e,(hl)
        inc hl
        ld d,(hl)
        ld a,d
        or e
        ret z
        ld ix,(ctxPtr)
        ld l,(ix+VIEWCTX_SIZE_LO)
        ld h,(ix+VIEWCTX_SIZE_LO+1)
        or a
        sbc hl,de
        ret c
        ret z
        ld a,h
        or a
        jr nz,.check_pt3
        ld a,l
        cp 105
        ret c
.check_pt3
        ld hl,PT3_LINEAR_ADDR+100
        add hl,de
        ld a,(hl)
        or a
        ret z
        cp $40
        ret nc
        inc hl
        ld a,(hl)
        or a
        ret z
        inc hl
        cp (hl)
        ret c
.second_ok
        ld hl,PT3_LINEAR_ADDR
        add hl,de
        ld (secondModuleAddr),hl
        ld a,%00010001          ; Two PT3 modules TS, ABC, no loop
        ld (pt3Setup),a
        ret


show_player_screen
        ld hl,0 * 256 + 5
        ld bc,78 * 256 + 20
        ld a,16
        call call_window
        ld hl,title
        call print_de_at_2_1
        ld ix,(ctxPtr)
        ld e,(ix+VIEWCTX_FILENAME)
        ld d,(ix+VIEWCTX_FILENAME+1)
        ld hl,15*256+6
        ld a,16
        call call_print
        ld de,infoText
        ld hl,2*256+9
        ld a,16
        call call_print
        ld de,pt3TitleLabel
        ld hl,2*256+11
        ld a,16
        call call_print
        ld de,pt3Title
        ld hl,9*256+11
        ld a,16
        call call_print
        ld de,pt3AuthorLabel
        ld hl,2*256+13
        ld a,16
        call call_print
        ld de,pt3Author
        ld hl,10*256+13
        ld a,16
        call call_print
        ld de,controlsText
        ld hl,2*256+24
        ld a,32
        call call_print
        ld de,nextText
        ld hl,66*256+24
        ld a,32
        call call_print
        ld de,metersHeaderText
        ld hl,2*256+16
        ld a,16
        call call_print
        ld de,meterRowAText
        ld hl,2*256+18
        ld a,16
        call call_print
        ld de,meterRowBText
        ld hl,2*256+19
        ld a,16
        call call_print
        ld de,meterRowCText
        ld hl,2*256+20
        ld a,16
        call call_print
        ret


prepare_pt3_info
        ld hl,VIEW_DATA_ADDRESS+30
        ld de,pt3Title
        call copy_pt3_text
        ld hl,VIEW_DATA_ADDRESS+60
        ld de,pt3Author
        call copy_pt3_text
        ret


copy_pt3_text
        ld b,30
.copy
        ld a,(hl)
        ld (de),a
        inc hl
        inc de
        djnz .copy
        xor a
        ld (de),a
        ret


show_pt3_mode
        call restore_mmu
        ld de,modeSingleText
        ld a,(VTPL.is_ts)
        or a
        jr z,.print
        ld de,modeTsText
.print
        ld hl,2*256+9
        ld a,16
        call call_print
        ret


show_volume_meters
        call restore_mmu
        ld a,(VTPL.VARS1+VTPL.VRS.AYREGS+VTPL.AmplA)
        and $0f
        ld b,a
        ld a,(VTPL.is_ts)
        or a
        jr z,.meter_a
        ld a,(VTPL.VARS2+VTPL.VRS.AYREGS+VTPL.AmplA)
        and $0f
        add a,b
        jr .print_a
.meter_a
        ld a,b
.print_a
        ld hl,8*256+18
        call print_meter
        ld a,(VTPL.VARS1+VTPL.VRS.AYREGS+VTPL.AmplB)
        and $0f
        ld b,a
        ld a,(VTPL.is_ts)
        or a
        jr z,.meter_b
        ld a,(VTPL.VARS2+VTPL.VRS.AYREGS+VTPL.AmplB)
        and $0f
        add a,b
        jr .print_b
.meter_b
        ld a,b
.print_b
        ld hl,8*256+19
        call print_meter
        ld a,(VTPL.VARS1+VTPL.VRS.AYREGS+VTPL.AmplC)
        and $0f
        ld b,a
        ld a,(VTPL.is_ts)
        or a
        jr z,.meter_c
        ld a,(VTPL.VARS2+VTPL.VRS.AYREGS+VTPL.AmplC)
        and $0f
        add a,b
        jr .print_c
.meter_c
        ld a,b
.print_c
        ld hl,8*256+20
        call print_meter
        jp map_pt3_data


print_meter
        ld b,a
        ld de,meterBuffer
        ld a,"["
        ld (de),a
        inc de
        ld c,30
.loop
        ld a,b
        or a
        jr z,.space
        ld a,"#"
        dec b
        jr .store
.space
        ld a,"."
.store
        ld (de),a
        inc de
        dec c
        jr nz,.loop
        ld a,"]"
        ld (de),a
        inc de
        xor a
        ld (de),a
        ld de,meterBuffer
        ld a,16
        jp call_print


print_de_at_2_1
        ex de,hl
        ld hl,2*256+6
        ld a,16
call_print
        call 0
        ret

call_window
        call 0
        ret

patch_services
        ld ix,(svcPtr)
        ld l,(ix+SERVICE_PRINT)
        ld h,(ix+SERVICE_PRINT+1)
        ld (call_print+1),hl
        ld l,(ix+SERVICE_WINDOW)
        ld h,(ix+SERVICE_WINDOW+1)
        ld (call_window+1),hl
        ld l,(ix+SERVICE_INPUT_NOWAIT)
        ld h,(ix+SERVICE_INPUT_NOWAIT+1)
        ld (call_input+1),hl
        ret


input_arm_delay
        ld b,INPUT_ARM_FRAMES
.wait
        halt
        djnz .wait
        ret


wait_stop_release
        call raw_control
        or a
        jr nz,wait_stop_release
        ei
        ld b,INPUT_ARM_FRAMES
.wait
        halt
        djnz .wait
        ret


call_input
        call 0
        ret


raw_control
        ld bc,$bffe
        in a,(c)
        cpl
        and $01
        jr z,.space
        ld a,1
        ret
.space
        ld bc,$7ffe
        in a,(c)
        cpl
        and $01
        ret z
        ld a,2
        ret

read_nextreg
        ld bc,$243B
        out (c),a
        inc b
        in a,(c)
        ret

ctxPtr       defw 0
svcPtr       defw 0
savedSp      defw 0
savedMmu2    defb 0
savedMmu3    defb 0
savedMmu4    defb 0
savedMmu5    defb 0
secondModuleAddr defw 0
pt3Setup    defb 0
title        defb "PT3:",0
infoText
modeSingleText defb "M:PT3 [AY] YM [ABC] ACB",0
modeTsText   defb "M:PT3TS [AY] YM [ABC] ACB",0
pt3TitleLabel defb "Title:",0
pt3AuthorLabel defb "Author:",0
metersHeaderText defb "Meters:",0
meterRowAText defb "A",0
meterRowBText defb "B",0
meterRowCText defb "C",0
controlsText defb "ENTER stop",0
nextText     defb "SPACE next",0
pt3Title     defs 31
pt3Author    defs 31
meterBuffer  defs 33

        include "ptx_vtpl.i.asm"

plugin_end
        assert plugin_end - plugin_start <= VIEW_PLUGIN_SIZE
        SAVEBIN "plugin/PT3_PT3-Player.CCP", VIEW_PLUGIN_ADDRESS, VIEW_PLUGIN_SIZE
