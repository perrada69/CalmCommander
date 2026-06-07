        DEVICE ZXSPECTRUMNEXT
        org VIEW_PLUGIN_ADDRESS

        include "plugin_api.i.asm"

NEXTREG_SEL      equ $243B
NEXTREG_DAT      equ $253B

NR_LAYER2_PAGE   equ $12
NR_TRANSPARENCY  equ $14
NR_LAYER2_XOFF   equ $16
NR_LAYER2_YOFF   equ $17
NR_CLIP_LAYER2   equ $18
NR_CLIP_CTRL     equ $1C
NR_PALETTE_IDX   equ $40
NR_PALETTE_CTRL  equ $43
NR_PALETTE_VAL9  equ $44
NR_FALLBACK_COL  equ $4A
NR_MMU2          equ $52
NR_MMU3          equ $53
NR_MMU7          equ $57
NR_DISPLAY_CTRL1 equ $69
NR_LAYER2_CTRL   equ $70

PALCTRL_L2_1     equ %00010000
NXI_L2_BANK      equ 49
SIZE_IMAGE       equ 49152
SIZE_PALETTE     equ 512
SRC_PAGE_SIZE    equ 8192

plugin_start
        ld (ctxPtr),hl
        ld (svcPtr),de
        call patch_services

        call validate_size
        jp c,exit_bad

        call save_state
        call set_nxi_l2_bank
        call l2_init_256
        call l2_reset_view
        call set_safe_transparency

        ld a,(hasPalette)
        or a
        jr z,.no_palette
        call upload_palette
        call init_source_after_palette
        jr .load_pixels
.no_palette
        call select_l2_palette_1
        call init_source_at_start

.load_pixels
        ld (savedSp),sp
        ld sp,pluginStackTop
        ld a,(l2BaseBank)
        call copy_16k_to_l2
        ld a,(l2BaseBank)
        inc a
        call copy_16k_to_l2
        ld a,(l2BaseBank)
        add a,2
        call copy_16k_to_l2

        call restore_mmu23
        ld sp,(savedSp)
        call l2_show
        call wait_key
        call l2_hide
        call select_l2_palette_1
        call restore_state
exit_ok
        xor a
        ret

exit_bad
        scf
        ret

validate_size
        ld ix,(ctxPtr)
        ld a,(ix+VIEWCTX_SIZE_HI)
        or (ix+VIEWCTX_SIZE_HI+1)
        jr nz,.bad
        ld a,(ix+VIEWCTX_SIZE_LO)
        or a
        jr nz,.bad
        ld a,(ix+VIEWCTX_SIZE_LO+1)
        cp $C0
        jr z,.image_only
        cp $C2
        jr z,.with_palette
.bad
        scf
        ret
.image_only
        xor a
        ld (hasPalette),a
        ret
.with_palette
        ld a,1
        ld (hasPalette),a
        xor a
        ret

patch_services
        ld ix,(svcPtr)
        ld l,(ix+SERVICE_INPUT_NOWAIT)
        ld h,(ix+SERVICE_INPUT_NOWAIT+1)
        ld (call_input+1),hl
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

set_nxi_l2_bank
        ld a,NXI_L2_BANK
        ld (l2BaseBank),a
        nextreg NR_LAYER2_PAGE,a
        ret

init_source_at_start
        xor a
        ld (srcPageIndex),a
        ld hl,0
        ld (srcOffset),hl
        ld hl,SRC_PAGE_SIZE
        ld (srcRemain),hl
        ret

init_source_after_palette
        xor a
        ld (srcPageIndex),a
        ld hl,SIZE_PALETTE
        ld (srcOffset),hl
        ld hl,SRC_PAGE_SIZE-SIZE_PALETTE
        ld (srcRemain),hl
        ret

copy_16k_to_l2
        add a,a
        ld (destPageLo),a
        inc a
        ld (destPageHi),a
        ld hl,$4000
        ld (destPtr),hl
        ld hl,16384
        ld (destRemain),hl

.loop
        ld hl,(destRemain)
        ld a,h
        or l
        ret z

        call map_current_source_page
        ld a,(destPageLo)
        nextreg NR_MMU2,a
        ld a,(destPageHi)
        nextreg NR_MMU3,a

        call set_copy_len
        ld hl,$E000
        ld de,(srcOffset)
        add hl,de
        ld de,(destPtr)
        ld bc,(copyLen)
        ldir

        ld hl,(destPtr)
        ld bc,(copyLen)
        add hl,bc
        ld (destPtr),hl

        ld hl,(destRemain)
        ld bc,(copyLen)
        or a
        sbc hl,bc
        ld (destRemain),hl

        ld hl,(srcOffset)
        ld bc,(copyLen)
        add hl,bc
        ld (srcOffset),hl

        ld hl,(srcRemain)
        ld bc,(copyLen)
        or a
        sbc hl,bc
        ld (srcRemain),hl

        ld hl,(destRemain)
        ld a,h
        or l
        ret z

        ld hl,(srcRemain)
        ld a,h
        or l
        jr nz,.loop

        ld hl,srcPageIndex
        inc (hl)
        ld hl,0
        ld (srcOffset),hl
        ld hl,SRC_PAGE_SIZE
        ld (srcRemain),hl
        jr .loop

set_copy_len
        ld hl,(destRemain)
        ld de,(srcRemain)
        or a
        sbc hl,de
        jr c,.use_dest
        ld hl,(srcRemain)
        jr .store
.use_dest
        ld hl,(destRemain)
.store
        ld (copyLen),hl
        ret

map_current_source_page
        ld ix,(ctxPtr)
        ld l,(ix+VIEWCTX_DATA_PAGES)
        ld h,(ix+VIEWCTX_DATA_PAGES+1)
        ld a,(srcPageIndex)
        add a,l
        ld l,a
        jr nc,.ok
        inc h
.ok
        ld a,(hl)
        nextreg NR_MMU7,a
        ret

upload_palette
        xor a
        ld (srcPageIndex),a
        call map_current_source_page

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
        ld hl,$E000
        ld bc,NEXTREG_DAT
        ld d,0
.loop
        ld a,(hl)
        inc hl
        out (c),a
        ld a,(hl)
        inc hl
        and 1
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

        ld a,1
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
        ld a,191
        out (c),a
        ret

set_safe_transparency
        ld a,$55
        nextreg NR_TRANSPARENCY,a
        xor a
        nextreg NR_FALLBACK_COL,a
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

select_l2_palette_1
        ld a,PALCTRL_L2_1
        nextreg NR_PALETTE_CTRL,a
        ret

wait_key
.release
        call call_input
        or a
        jr nz,.release
.press
        call call_input
        or a
        jr z,.press
        ret

ctxPtr       defw 0
svcPtr       defw 0
savedMmu2    defb 0
savedMmu3    defb 0
savedMmu7    defb 0
savedDisplay defb 0
savedPalCtrl defb 0
savedLayer2Page defb 0
l2BaseBank   defb 0
hasPalette   defb 0
srcPageIndex defb 0
srcOffset    defw 0
srcRemain    defw 0
destPageLo   defb 0
destPageHi   defb 0
destPtr      defw 0
destRemain   defw 0
copyLen      defw 0
savedSp      defw 0
pluginStack  defs 256
pluginStackTop equ $

plugin_end
        assert plugin_end - plugin_start <= VIEW_PLUGIN_SIZE
        SAVEBIN "plugin/nxi.ccp", VIEW_PLUGIN_ADDRESS, VIEW_PLUGIN_SIZE
