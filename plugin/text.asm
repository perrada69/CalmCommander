        DEVICE ZXSPECTRUMNEXT
        org VIEW_PLUGIN_ADDRESS

        include "plugin_api.i.asm"

plugin_start
        ld (ctxPtr),hl
        ld (svcPtr),de
        call patch_services

        ld hl,0 * 256 + 0
        ld bc,80 * 256 + 32
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

        ld de,breakText
        ld hl,56*256+31
        ld a,32
        call call_print

        ld hl,VIEW_DATA_ADDRESS
        ld de,$4000+160*3+2
        ld (lineStart),de
        xor a
        ld (textCol),a
        ld (textRow),a

        ld ix,(ctxPtr)
        ld c,(ix+VIEWCTX_READ_LEN)
        ld b,(ix+VIEWCTX_READ_LEN+1)

.loop
        ld a,b
        or c
        jr z,.wait
        ld a,(textRow)
        cp 28
        jr nc,.wait

        ld a,(hl)
        inc hl
        dec bc
        cp 13
        jr z,.loop
        cp 10
        jr z,.newline
        cp 9
        jr nz,.not_tab
        ld a,32
.not_tab
        cp 32
        jr nc,.char_ok
        ld a,"."
.char_ok
        ld (tmpChar),a
        ld a,(textCol)
        cp 78
        call z,newline
        ld a,(textRow)
        cp 28
        jr nc,.wait
        ld a,(tmpChar)
        ld (de),a
        inc de
        ld a,16
        ld (de),a
        inc de
        ld a,(textCol)
        inc a
        ld (textCol),a
        jr .loop

.newline
        call newline
        jr .loop

.wait
        call call_inkey
        cp 1
        jr nz,.wait
        ret

newline
        push hl
        ld hl,(lineStart)
        ld de,160
        add hl,de
        ld (lineStart),hl
        ex de,hl
        xor a
        ld (textCol),a
        ld hl,textRow
        inc (hl)
        pop hl
        ret

print_de_at_2_1
        ex de,hl
        ld hl,2*256+1
        ld a,16
call_print
        call 0
        ret

call_inkey
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
        ld l,(ix+SERVICE_INKEY)
        ld h,(ix+SERVICE_INKEY+1)
        ld (call_inkey+1),hl
        ld l,(ix+SERVICE_WINDOW)
        ld h,(ix+SERVICE_WINDOW+1)
        ld (call_window+1),hl
        ret

ctxPtr      defw 0
svcPtr      defw 0
lineStart   defw 0
textCol     defb 0
textRow     defb 0
tmpChar     defb 0
title       defb "VIEW:",0
breakText   defb "BREAK: close this window",0

plugin_end
        assert plugin_end - plugin_start <= VIEW_PLUGIN_SIZE
        SAVEBIN "plugin/text.ccp", VIEW_PLUGIN_ADDRESS, VIEW_PLUGIN_SIZE
