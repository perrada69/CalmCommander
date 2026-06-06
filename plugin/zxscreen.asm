        DEVICE ZXSPECTRUMNEXT
        org VIEW_PLUGIN_ADDRESS

        include "plugin_api.i.asm"

plugin_start
        ld (svcPtr),de
        call patch_services

        call call_layer0

        ld hl,VIEW_DATA_ADDRESS
        ld de,16384
        ld bc,6912
        ldir

.wait
        call raw_break_pressed
        jr nz,.wait
        ret

call_layer0
        call 0
        ret

; BREAK = CAPS SHIFT + SPACE. This avoids CC INKEY, because INKEY
; updates mouse/time/sprites and corrupts the ULA preview screen.
raw_break_pressed
        ld bc,$FEFE
        in a,(c)
        bit 0,a
        jr nz,.not_break
        ld bc,$7FFE
        in a,(c)
        bit 0,a
        jr nz,.not_break
        xor a
        inc a
        ret
.not_break
        xor a
        ret

patch_services
        ld ix,(svcPtr)
        ld l,(ix+SERVICE_LAYER0)
        ld h,(ix+SERVICE_LAYER0+1)
        ld (call_layer0+1),hl
        ret

svcPtr      defw 0

plugin_end
        assert plugin_end - plugin_start <= VIEW_PLUGIN_SIZE
        SAVEBIN "plugin/zxscreen.ccp", VIEW_PLUGIN_ADDRESS, VIEW_PLUGIN_SIZE
