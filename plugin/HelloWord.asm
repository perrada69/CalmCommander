        DEVICE ZXSPECTRUMNEXT
        org VIEW_PLUGIN_ADDRESS

        include "plugin_api.i.asm"

; ------------------------------------------------------------
; HelloWord.ccp
; ------------------------------------------------------------
; This is a deliberately small and well-commented viewer plugin
; for Calm Commander. It demonstrates the basic plugin contract:
;
;   HL = pointer to PluginContext
;   DE = pointer to Calm Commander service table
;
; The plugin draws a player-like window, prints information from
; the context, waits for STOP or NEXT, and returns:
;
;   A = 0  stop/close viewer
;   A = 1  close viewer and ask Calm Commander to move to next file
; ------------------------------------------------------------

INPUT_ARM_FRAMES equ 25
PLUGIN_STACK     equ $DFFE

plugin_start
        ld (ctxPtr),hl
        ld (svcPtr),de
        call patch_services

        ld (savedSp),sp
        ld sp,PLUGIN_STACK

        call draw_demo_window
        call wait_key_release
        call input_arm_delay

.input_loop
        halt
        call read_input
        cp 1
        jr z,.stop
        cp 2
        jr z,.next
        jr .input_loop

.stop
        call wait_key_release
        ld sp,(savedSp)
        xor a
        ret

.next
        call wait_key_release
        ld sp,(savedSp)
        ld a,1
        ret


; Draw the complete plugin screen. The coordinates use the same
; convention as the host print/window services: H = column, L = row.
draw_demo_window
        ld hl,0*256+5
        ld bc,78*256+20
        ld a,16
        call call_window

        ld de,titleText
        ld hl,2*256+6
        ld a,16
        call call_print

        ld ix,(ctxPtr)
        ld e,(ix+VIEWCTX_FILENAME)
        ld d,(ix+VIEWCTX_FILENAME+1)
        ld hl,20*256+6
        ld a,16
        call call_print

        ld de,helloText
        ld hl,34*256+14
        ld a,16
        call call_print

        call print_context_info

        ld de,stopText
        ld hl,2*256+24
        ld a,32
        call call_print

        ld de,nextText
        ld hl,63*256+24
        ld a,32
        call call_print
        ret


; Print every useful value currently provided by PluginContext.
; This makes the file useful as a reference when writing new plugins.
print_context_info
        ld de,abiLabel
        ld hl,2*256+8
        call print_label
        ld ix,(ctxPtr)
        ld a,(ix+VIEWCTX_ABI)
        call print_hex8_at_cursor

        ld de,typeLabel
        ld hl,15*256+8
        call print_label
        ld a,(ix+VIEWCTX_TYPE)
        call print_hex8_at_cursor

        ld de,sizeLabel
        ld hl,2*256+10
        call print_label
        ld l,(ix+VIEWCTX_SIZE_HI)
        ld h,(ix+VIEWCTX_SIZE_HI+1)
        call print_hex16_at_cursor
        ld l,(ix+VIEWCTX_SIZE_LO)
        ld h,(ix+VIEWCTX_SIZE_LO+1)
        call print_hex16_at_cursor

        ld de,readLabel
        ld hl,2*256+11
        call print_label
        ld l,(ix+VIEWCTX_READ_LEN)
        ld h,(ix+VIEWCTX_READ_LEN+1)
        call print_hex16_at_cursor

        ld de,dataPageLabel
        ld hl,2*256+12
        call print_label
        ld a,(ix+VIEWCTX_DATA_PAGE)
        call print_hex8_at_cursor

        ld de,dataAddrLabel
        ld hl,24*256+12
        call print_label
        ld l,(ix+VIEWCTX_DATA_ADDR)
        ld h,(ix+VIEWCTX_DATA_ADDR+1)
        call print_hex16_at_cursor

        ld de,pageCountLabel
        ld hl,2*256+16
        call print_label
        ld a,(ix+VIEWCTX_PAGE_COUNT)
        call print_hex8_at_cursor

        ld de,pagesLabel
        ld hl,2*256+17
        call print_label
        call print_data_pages

        ld de,pagesPtrLabel
        ld hl,2*256+19
        call print_label
        ld l,(ix+VIEWCTX_DATA_PAGES)
        ld h,(ix+VIEWCTX_DATA_PAGES+1)
        call print_hex16_at_cursor

        ld de,servicesLabel
        ld hl,2*256+20
        call print_label
        ld l,(ix+VIEWCTX_SERVICES)
        ld h,(ix+VIEWCTX_SERVICES+1)
        call print_hex16_at_cursor
        ret


; Print up to eight MMU page numbers that contain the loaded file.
; Calm Commander maps each page at VIEW_DATA_ADDRESS ($E000) when
; the plugin needs to inspect that part of the file.
print_data_pages
        ld ix,(ctxPtr)
        ld b,(ix+VIEWCTX_PAGE_COUNT)
        ld a,b
        or a
        jr z,.none
        ld a,b
        cp 8
        jr c,.count_ok
        ld b,8
.count_ok
        ld l,(ix+VIEWCTX_DATA_PAGES)
        ld h,(ix+VIEWCTX_DATA_PAGES+1)
.loop
        push bc
        ld a,(hl)
        push hl
        call print_hex8_at_cursor
        ld de,spaceText
        call print_at_cursor
        pop hl
        inc hl
        pop bc
        djnz .loop
        ret
.none
        ld de,noneText
        jp print_at_cursor


; Print a label and remember the next free column as the cursor.
print_label
        call string_length
        push hl
        push bc
        ld a,16
        call call_print
        pop bc
        pop hl
        ld a,h
        add a,b
        ld h,a
        ld (cursorPos),hl
        ret


; Print a zero-terminated string at cursorPos and update cursorPos.
print_at_cursor
        ld hl,(cursorPos)
        call string_length
        push hl
        push bc
        ld a,16
        call call_print
        pop bc
        pop hl
        ld a,h
        add a,b
        ld h,a
        ld (cursorPos),hl
        ret


; Return the length of the zero-terminated string in DE in B.
; DE is preserved so the caller can immediately print the string.
string_length
        push de
        ld b,0
.loop
        ld a,(de)
        or a
        jr z,.done
        inc b
        inc de
        jr .loop
.done
        pop de
        ret


; Print A as two hexadecimal digits at cursorPos.
print_hex8_at_cursor
        push af
        rrca
        rrca
        rrca
        rrca
        call nibble_to_ascii
        ld (hexBuffer),a
        pop af
        call nibble_to_ascii
        ld (hexBuffer+1),a
        xor a
        ld (hexBuffer+2),a
        ld de,hexBuffer
        jp print_at_cursor


; Print HL as four hexadecimal digits, high byte first.
print_hex16_at_cursor
        ld a,h
        call print_hex8_at_cursor
        ld a,l
        jp print_hex8_at_cursor


; Convert the low nibble in A to ASCII hex.
nibble_to_ascii
        and $0f
        add a,"0"
        cp "9"+1
        ret c
        add a,7
        ret


; Patch local CALL placeholders with service addresses supplied by
; Calm Commander. Keeping wrappers local makes the rest of the plugin
; read like normal code.
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


call_print
        call 0
        ret

call_window
        call 0
        ret

call_input
        call 0
        ret


; The host input service returns 1 for STOP and 2 for NEXT for this
; plugin type. raw_control is a tiny fallback so Enter and Space also
; work while the original launch key is still being released.
read_input
        call call_input
        or a
        ret nz
        call raw_control
        ret


wait_key_release
        call raw_control
        or a
        jr nz,wait_key_release
        ret


input_arm_delay
        ld b,INPUT_ARM_FRAMES
.wait
        halt
        djnz .wait
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


ctxPtr            defw 0
svcPtr            defw 0
savedSp           defw 0
cursorPos         defw 0
hexBuffer         defs 3

titleText         defb "HelloWord demo:",0
helloText         defb "Hello Word",0
abiLabel          defb "ABI:$",0
typeLabel         defb "TYPE:$",0
sizeLabel         defb "SIZE:$",0
readLabel         defb "FIRST READ:$",0
dataPageLabel     defb "DATA PAGE:$",0
dataAddrLabel     defb "DATA ADDR:$",0
pageCountLabel    defb "PAGE COUNT:$",0
pagesLabel        defb "FILE PAGES:$",0
pagesPtrLabel     defb "PAGES PTR:$",0
servicesLabel     defb "SERVICES PTR:$",0
spaceText         defb " ",0
noneText          defb "none",0
stopText          defb "[ ENTER stop ]",0
nextText          defb "[ SPACE next ]",0

plugin_end
        assert plugin_end - plugin_start <= VIEW_PLUGIN_SIZE
        SAVEBIN "plugin/HelloWord.ccp", VIEW_PLUGIN_ADDRESS, VIEW_PLUGIN_SIZE
