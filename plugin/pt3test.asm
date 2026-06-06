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

        call show_player_screen
        call capture_mouse_idle
        call map_pt3_data

        xor a
        ld (VTPL.SETUP),a       ; PT3, ABC, loop forever
        ld hl,PT3_LINEAR_ADDR
        ld de,0
        call VTPL.INIT
        ei

.wait_release
        call raw_keyboard
        or a
        jr nz,.wait_release
        call input_arm_delay

.play_loop
        halt
        call raw_input
        or a
        jr nz,.stop
        call VTPL.PLAY
        ; VTPL sets SETUP bit 7 when the module loop point is passed.
        ; In looped playback mode this is informational, not a stop request.
        jr .play_loop

.stop
        call VTPL.MUTE
        call wait_stop_release
        di
        call restore_mmu
        ld sp,(savedSp)
        ret


map_pt3_data
        ld ix,(ctxPtr)
        ld l,(ix+VIEWCTX_DATA_PAGES)
        ld h,(ix+VIEWCTX_DATA_PAGES+1)
        ld (dataPagesPtr),hl
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
        ld hl,0 * 256 + 0
        ld bc,78 * 256 + 30
        ld a,16
        call call_window
        ld hl,title
        call print_de_at_2_1
        ld ix,(ctxPtr)
        ld e,(ix+VIEWCTX_FILENAME)
        ld d,(ix+VIEWCTX_FILENAME+1)
        ld hl,15*256+1
        ld a,16
        call call_print
        ld de,infoText
        ld hl,2*256+4
        ld a,16
        call call_print
        ld de,stopText
        ld hl,2*256+31
        ld a,32
        call call_print
        ret


print_de_at_2_1
        ex de,hl
        ld hl,2*256+1
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
        call capture_mouse_idle
        ret


wait_stop_release
        call raw_keyboard
        or a
        jr nz,wait_stop_release
        ld b,INPUT_ARM_FRAMES
.wait
        halt
        djnz .wait
        ret


raw_input
        ; Keep CC mouse handling alive, but do not use its key result
        ; for closing yet; the PT3 test player exits only on SPACE.
        call call_input
        call raw_keyboard
        ret


call_input
        call 0
        ret


raw_mouse_click
        call read_mouse_buttons
        ld b,a
        ld a,(idleMouseButtons)
        cp b
        ret z
        ld a,1
        ret


capture_mouse_idle
        call read_mouse_buttons
        ld (idleMouseButtons),a
        ret


read_mouse_buttons
        ld bc,$fadf
        in a,(c)
        and 3
        ret


raw_keyboard
        ; For the first PT3 test build use SPACE only. Generic "any key"
        ; detection is too sensitive while returning from CC.
        ld bc,$7ffe
        in a,(c)
        cpl
        and $01
        ret z
        ld a,1
        ret


raw_key_row
        in a,(c)
        cpl
        and $1f
        ret z
        ld a,1
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
idleMouseButtons defb 0
dataPagesPtr defw 0
title        defb "PT3:",0
infoText     defb "Test PT3 plugin - data mapped at $4000-$BFFF",0
stopText     defb "SPACE stops playback",0

        include "ptx_vtpl.i.asm"

plugin_end
        assert plugin_end - plugin_start <= VIEW_PLUGIN_SIZE
        SAVEBIN "plugin/pt3test.ccp", VIEW_PLUGIN_ADDRESS, VIEW_PLUGIN_SIZE
