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

.wait_release
        call call_input
        or a
        jr nz,.wait_release

.wait
        call call_input
        or a
        jr z,.wait
        ret

call_input
        call 0
        ret

call_layer0
        call 0
        ret

patch_services
        ld ix,(svcPtr)
        ld l,(ix+SERVICE_INPUT_NOWAIT)
        ld h,(ix+SERVICE_INPUT_NOWAIT+1)
        ld (call_input+1),hl
        ld l,(ix+SERVICE_LAYER0)
        ld h,(ix+SERVICE_LAYER0+1)
        ld (call_layer0+1),hl
        ret

svcPtr      defw 0

plugin_end
        assert plugin_end - plugin_start <= VIEW_PLUGIN_SIZE
        SAVEBIN "plugin/zxscreen.ccp", VIEW_PLUGIN_ADDRESS, VIEW_PLUGIN_SIZE
