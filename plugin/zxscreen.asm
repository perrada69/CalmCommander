        DEVICE ZXSPECTRUMNEXT
        org VIEW_PLUGIN_ADDRESS

        include "plugin_api.i.asm"

NEXTREG_SEL      equ $243B
NEXTREG_DAT      equ $253B

NR_LAYER2_PAGE   equ $12
NR_LAYER2_XOFF   equ $16
NR_LAYER2_YOFF   equ $17
NR_CLIP_LAYER2   equ $18
NR_CLIP_CTRL     equ $1C
NR_PALETTE_IDX   equ $40
NR_PALETTE_CTRL  equ $43
NR_PALETTE_VAL9  equ $44
NR_MMU2          equ $52
NR_MMU3          equ $53
NR_MMU7          equ $57
NR_DISPLAY_CTRL1 equ $69
NR_LAYER2_CTRL   equ $70

PALCTRL_L2_1     equ %00010000
SCR_L2_BANK      equ 49

plugin_start
        ld (ctxPtr),hl
        ld (svcPtr),de
        call patch_services
        di
        call save_state
        call set_scr_l2_bank
        call l2_init_256
        call l2_reset_view
        call upload_zx_palette

        ld (savedSp),sp
        ld sp,pluginStackTop
        call convert_scr_to_l2
        call restore_mmu23
        ld sp,(savedSp)

        call draw_controls
        call l2_show
        ei
        call wait_command
        ld (pluginResult),a
        di
        call l2_hide
        call restore_state
        ei
        ld a,(pluginResult)
        ret

patch_services
        ld ix,(svcPtr)
        ld l,(ix+SERVICE_PRINT)
        ld h,(ix+SERVICE_PRINT+1)
        ld (call_print+1),hl
        ld l,(ix+SERVICE_INPUT_NOWAIT)
        ld h,(ix+SERVICE_INPUT_NOWAIT+1)
        ld (call_input+1),hl
        ret

call_print
        call 0
        ret

call_input
        call 0
        ret

save_state
        ld a,NR_MMU2
        call read_nextreg
        ld (savedMmu2),a
        ld a,NR_MMU3
        call read_nextreg
        ld (savedMmu3),a
        ld a,NR_MMU7
        call read_nextreg
        ld (savedMmu7),a
        ld a,NR_DISPLAY_CTRL1
        call read_nextreg
        ld (savedDisplay),a
        ld a,NR_PALETTE_CTRL
        call read_nextreg
        ld (savedPalCtrl),a
        ld a,NR_LAYER2_PAGE
        call read_nextreg
        ld (savedLayer2Page),a
        ret

restore_state
        ld a,(savedMmu7)
        nextreg NR_MMU7,a
        ld a,(savedDisplay)
        nextreg NR_DISPLAY_CTRL1,a
        ld a,(savedPalCtrl)
        nextreg NR_PALETTE_CTRL,a
        ld a,(savedLayer2Page)
        nextreg NR_LAYER2_PAGE,a
        ret

restore_mmu23
        ld a,(savedMmu2)
        nextreg NR_MMU2,a
        ld a,(savedMmu3)
        nextreg NR_MMU3,a
        ret

read_nextreg
        ld bc,NEXTREG_SEL
        out (c),a
        inc b
        in a,(c)
        ret

set_scr_l2_bank
        ld a,SCR_L2_BANK
        nextreg NR_LAYER2_PAGE,a
        ret

convert_scr_to_l2
        xor a
        ld (yCounter),a
.row_loop
        call map_dest_row
        call calc_scr_row_ptr
        ld (srcPtr),hl
        call calc_attr_row_ptr
        push hl
        pop ix
        ld hl,(srcPtr)
        ld de,(destPtr)
        ld b,32
.byte_loop
        push bc
        ld a,(ix+0)
        inc ix
        call set_attr_colors
        ld c,(hl)
        inc hl
        ld b,8
.pixel_loop
        sla c
        jr c,.ink
        ld a,(paperColor)
        jr .store_pixel
.ink
        ld a,(inkColor)
.store_pixel
        ld (de),a
        inc de
        djnz .pixel_loop
        pop bc
        djnz .byte_loop

        ld hl,yCounter
        inc (hl)
        ld a,(hl)
        cp 192
        jr nz,.row_loop
        ret

map_dest_row
        ld a,(yCounter)
        and $C0
        rlca
        rlca
        ld b,a
        ld a,SCR_L2_BANK
        add a,b
        add a,a
        nextreg NR_MMU2,a
        inc a
        nextreg NR_MMU3,a

        ld a,(yCounter)
        and $3F
        add a,$40
        ld h,a
        ld l,0
        ld (destPtr),hl
        ret

calc_scr_row_ptr
        ld a,(yCounter)
        ld e,a
        and 7
        ld h,a
        ld l,0

        ld a,e
        and $38
        rlca
        rlca
        ld l,a

        ld a,e
        and $C0
        rrca
        rrca
        rrca
        add a,h
        ld h,a

        ld de,VIEW_DATA_ADDRESS
        add hl,de
        ret

calc_attr_row_ptr
        ld a,(yCounter)
        srl a
        srl a
        srl a
        ld l,a
        ld h,0
        add hl,hl
        add hl,hl
        add hl,hl
        add hl,hl
        add hl,hl
        ld de,VIEW_DATA_ADDRESS+6144
        add hl,de
        ret

set_attr_colors
        ld (attrByte),a
        and 7
        ld (inkColor),a
        ld a,(attrByte)
        and $38
        rrca
        rrca
        rrca
        ld (paperColor),a
        ld a,(attrByte)
        bit 6,a
        ret z
        ld a,(inkColor)
        add a,8
        ld (inkColor),a
        ld a,(paperColor)
        add a,8
        ld (paperColor),a
        ret

upload_zx_palette
        ld a,PALCTRL_L2_1
        nextreg NR_PALETTE_CTRL,a
        ld a,NR_PALETTE_IDX
        ld bc,NEXTREG_SEL
        out (c),a
        inc b
        xor a
        out (c),a

        ld bc,NEXTREG_SEL
        ld a,NR_PALETTE_VAL9
        out (c),a
        ld hl,zxPalette
        ld bc,NEXTREG_DAT
        ld d,16
.loop
        ld a,(hl)
        inc hl
        out (c),a
        xor a
        out (c),a
        dec d
        jr nz,.loop
        ret

l2_init_256
        xor a
        nextreg NR_LAYER2_CTRL,a
        ret

l2_reset_view
        xor a
        nextreg NR_LAYER2_XOFF,a
        nextreg NR_LAYER2_YOFF,a

        ld a,15
        nextreg NR_CLIP_CTRL,a

        ld bc,NEXTREG_SEL
        ld a,NR_CLIP_LAYER2
        out (c),a
        inc b
        xor a
        out (c),a
        ld a,255
        out (c),a
        xor a
        out (c),a
        ld a,175
        out (c),a
        ret

l2_show
        ld a,NR_DISPLAY_CTRL1
        call read_nextreg
        or %10000000
        nextreg NR_DISPLAY_CTRL1,a
        ret

l2_hide
        ld a,NR_DISPLAY_CTRL1
        call read_nextreg
        and %01111111
        nextreg NR_DISPLAY_CTRL1,a
        ret

draw_controls
        ld hl,12*256+27
        ld a,144
        ld de,enterText
        call call_print
        ld hl,52*256+27
        ld a,144
        ld de,spaceText
        call call_print
        ret

wait_command
.release
        call call_input
        or a
        jr nz,.release
.press
        call call_input
        or a
        jr z,.press
        cp 13
        jr z,.stop
        cp 1
        jr z,.stop
        cp " "
        jr z,.next
        cp 2
        jr z,.next
        jr .press
.stop
        call wait_release
        xor a
        ret
.next
        call wait_release
        ld a,1
        ret

wait_release
        call call_input
        or a
        jr nz,wait_release
        ret

ctxPtr       defw 0
svcPtr       defw 0
savedMmu2    defb 0
savedMmu3    defb 0
savedMmu7    defb 0
savedDisplay defb 0
savedPalCtrl defb 0
savedLayer2Page defb 0
savedSp      defw 0
pluginResult defb 0
yCounter    defb 0
srcPtr      defw 0
destPtr     defw 0
attrByte    defb 0
inkColor    defb 0
paperColor  defb 0

pluginStack  defs 192
pluginStackTop equ $

enterText    defb "[ ENTER close ]",0
spaceText    defb "[ SPACE next ]",0

zxPalette
        defb $00,$02,$A0,$A2,$14,$16,$B4,$B6
        defb $00,$03,$E0,$E3,$1C,$1F,$FC,$FF

plugin_end
        assert plugin_end - plugin_start <= VIEW_PLUGIN_SIZE
        SAVEBIN "plugin/zxscreen.ccp", VIEW_PLUGIN_ADDRESS, VIEW_PLUGIN_SIZE
