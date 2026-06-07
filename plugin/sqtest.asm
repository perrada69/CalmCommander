        DEVICE ZXSPECTRUMNEXT
        org VIEW_PLUGIN_ADDRESS

        include "plugin_api.i.asm"

MUSIC_LINEAR_ADDR equ $4000
MUSIC_STACK       equ $DFFE
INPUT_ARM_FRAMES equ 25

songdata equ MUSIC_LINEAR_ADDR
BUFFER equ MUSIC_LINEAR_ADDR
RELOC_BASE equ BUFFER+2
I_SAMPLES equ RELOC_BASE
I_ORNAMENTS equ I_SAMPLES+2
I_PATTERNS equ I_ORNAMENTS+2
I_POSITIONS equ I_PATTERNS+2
I_REPEAT equ I_POSITIONS+2
RELOC_ENDPTR equ I_REPEAT+2

plugin_start
        ld (ctxPtr),hl
        ld (svcPtr),de
        call patch_services

        ld (savedSp),sp
        ld sp,MUSIC_STACK

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
        call SQ_INIT
        call show_music_mode
        ei

.wait_release
        call raw_control
        or a
        jr nz,.wait_release
        call input_arm_delay

.play_loop
        halt
        call raw_input
        cp 1
        jr z,.exit
        cp 2
        jr z,.next
        call SQ_PLAY
        ld a,(SQ_STATUS)
        bit 7,a
        jr nz,.next
        call show_volume_meters
        jr .play_loop

.exit
        call SQ_STOP
        call wait_stop_release
        di
        call restore_mmu
        ld sp,(savedSp)
        xor a
        ret

.next
        call SQ_STOP
        call wait_stop_release
        di
        call restore_mmu
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
        ld de,stopText
        ld hl,2*256+24
        ld a,32
        call call_print
        ld de,nextText
        ld hl,63*256+24
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
        ld hl,sqtInfoText
        ld de,pt3Title
        call copy_zero_text
        ld hl,emptyText
        ld de,pt3Author
        call copy_zero_text
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


copy_zero_text
        ld a,(hl)
        ld (de),a
        inc hl
        inc de
        or a
        jr nz,copy_zero_text
        ret


show_music_mode
        call restore_mmu
        ld de,modeSingleText
.print
        ld hl,2*256+9
        ld a,16
        call call_print
        jp map_pt3_data


show_volume_meters
        call restore_mmu
        ld a,(SQ_AY_AMPS)
        and $0f
        ld hl,8*256+18
        call print_meter
        ld a,(SQ_AY_AMPS+1)
        and $0f
        ld hl,8*256+19
        call print_meter
        ld a,(SQ_AY_AMPS+2)
        and $0f
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
        ld b,INPUT_ARM_FRAMES
.wait
        halt
        djnz .wait
        ret


raw_input
        call call_input
        or a
        ret nz
        call raw_control
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
title        defb "SQT:",0
infoText     defb "M:SQT [AY] YM [ABC] ACB",0
modeSingleText defb "M:SQT [AY] YM [ABC] ACB",0
modeTsText   defb "M:SQT [AY] YM [ABC] ACB",0
pt3TitleLabel defb "Info:",0
pt3AuthorLabel defb "More:",0
metersHeaderText defb "Meters:",0
meterRowAText defb "A",0
meterRowBText defb "B",0
meterRowCText defb "C",0
stopText     defb "[ ENTER stop ]",0
nextText     defb "[ SPACE next ]",0
sqtInfoText  defb "SQ Tracker",0
emptyText    defb 0
pt3Title     defs 31
pt3Author    defs 31
meterBuffer  defs 33

        include "sqt_player.i.asm"

plugin_end
        assert plugin_end - plugin_start <= VIEW_PLUGIN_SIZE
        SAVEBIN "plugin/sqtest.ccp", VIEW_PLUGIN_ADDRESS, VIEW_PLUGIN_SIZE
