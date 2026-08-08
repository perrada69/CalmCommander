        DEVICE ZXSPECTRUMNEXT
        org SETTINGS_PLUGIN_ADDRESS

        include "settings_api.i.asm"

PLUGIN_STACK       equ $DFFE
BACKUP_PALETTE     equ $E000
BACKUP_KEYS        equ $E020
LINE_BUFFER        equ $E080
KEY_NAME_BUFFER    equ $E100

BOX_X              equ 4
BOX_Y              equ 3
BOX_WIDTH          equ 72
BOX_HEIGHT         equ 25
LIST_X             equ BOX_X+2
LIST_Y             equ BOX_Y+6
VISIBLE_ROWS       equ 15
LIST_WIDTH         equ BOX_WIDTH-4

plugin_start
        ld (ctxPtr),hl
        ld (svcPtr),de
        ld (savedSp),sp
        ld sp,PLUGIN_STACK
        ld ix,(ctxPtr)
        xor a
        ld (ix+SETTINGSCTX_RESULT),a
        ld (ix+SETTINGSCTX_ERROR),a
        ld a,(ix+SETTINGSCTX_ABI)
        cp SETTINGS_ABI
        jp nz,.bad_abi
        call patch_services
        call backup_config
        xor a
        ld (currentTab),a
        ld (currentIndex),a
        ld (topIndex),a
        call draw_screen

.input
        call read_key
        cp 1
        jp z,.cancel
        cp 13
        jr z,.enter
        cp 11
        jr z,.up
        cp 10
        jr z,.down
        cp 8
        jr z,.left
        cp 9
        jr z,.right
        cp "s"
        jr z,.save
        cp "S"
        jr z,.save
        jr .input
.up
        call move_up
        call draw_rows
        jr .input
.down
        call move_down
        call draw_rows
        jr .input
.left
        ld a,(currentTab)
        or a
        jr z,.input
        xor a
        jr .set_tab
.right
        ld a,(currentTab)
        or a
        jr nz,.input
        ld a,1
.set_tab
        ld (currentTab),a
        xor a
        ld (currentIndex),a
        ld (topIndex),a
        call draw_screen
        jp .input
.enter
        ld a,(currentTab)
        or a
        jr nz,.capture
        call colour_up
        call draw_screen
        jr .input
.capture
        call capture_key
        call draw_screen
        jr .input
.save
        ld ix,(ctxPtr)
        ld a,1
        ld (ix+SETTINGSCTX_RESULT),a
        jr .done
.cancel
        call restore_config
        jr .done
.bad_abi
        ld a,$7f
        ld (ix+SETTINGSCTX_ERROR),a
.done
        ld sp,(savedSp)
        ret


backup_config
        ld ix,(ctxPtr)
        ld l,(ix+SETTINGSCTX_PALETTE)
        ld h,(ix+SETTINGSCTX_PALETTE+1)
        ld de,BACKUP_PALETTE
        ld bc,SETTINGS_PALETTE_COUNT
        ldir
        ld l,(ix+SETTINGSCTX_KEYS)
        ld h,(ix+SETTINGSCTX_KEYS+1)
        ld de,BACKUP_KEYS
        ld bc,SETTINGS_ACTION_COUNT
        ldir
        ret

restore_config
        ld ix,(ctxPtr)
        ld e,(ix+SETTINGSCTX_PALETTE)
        ld d,(ix+SETTINGSCTX_PALETTE+1)
        ld hl,BACKUP_PALETTE
        ld bc,SETTINGS_PALETTE_COUNT
        ldir
        ld e,(ix+SETTINGSCTX_KEYS)
        ld d,(ix+SETTINGSCTX_KEYS+1)
        ld hl,BACKUP_KEYS
        ld bc,SETTINGS_ACTION_COUNT
        ldir
        ret


draw_screen
        ld hl,BOX_X*256+BOX_Y
        ld bc,BOX_WIDTH*256+BOX_HEIGHT
        ld a,16
        call call_window
        ld b,BOX_X+2
        ld c,BOX_Y+1
        ld hl,titleText
        ld a,16
        call plot_string
        ld b,BOX_X+2
        ld c,BOX_Y+3
        ld hl,coloursTabIdle
        ld a,16
        call plot_string
        ld b,BOX_X+18
        ld c,BOX_Y+3
        ld hl,keysTabIdle
        ld a,16
        call plot_string
        ld a,(currentTab)
        or a
        ld b,BOX_X+2
        ld hl,coloursTabActive
        jr z,.active_tab
        ld b,BOX_X+18
        ld hl,keysTabActive
.active_tab
        ld c,BOX_Y+3
        ld a,64
        call plot_string
        ld b,LIST_X
        ld c,BOX_Y+5
        ld hl,colourHeader
        ld a,(currentTab)
        or a
        jr z,.header
        ld hl,keyHeader
.header
        ld a,16
        call plot_string
        ld b,LIST_X
        ld c,BOX_Y+BOX_HEIGHT-2
        ld hl,hintText
        ld a,16
        call plot_string
        jp draw_rows

draw_rows
        xor a
        ld (visibleRow),a
.loop
        call draw_one_row
        ld a,(visibleRow)
        inc a
        ld (visibleRow),a
        cp VISIBLE_ROWS
        jr nz,.loop
        ret

draw_one_row
        ld hl,LINE_BUFFER
        ld de,LINE_BUFFER+1
        ld bc,LIST_WIDTH-1
        ld (hl),' '
        ldir
        xor a
        ld (LINE_BUFFER+LIST_WIDTH),a
        ld a,(visibleRow)
        ld hl,topIndex
        add a,(hl)
        ld (drawIndex),a
        ld b,a
        call current_count
        cp b
        jr z,.plot
        jr c,.plot
        ld a,(currentIndex)
        cp b
        jr nz,.content
        ld a,'>'
        ld (LINE_BUFFER),a
.content
        ld a,(currentTab)
        or a
        call z,prepare_colour_row
        ld a,(currentTab)
        or a
        call nz,prepare_key_row
.plot
        ld b,LIST_X
        ld a,(visibleRow)
        add a,LIST_Y
        ld c,a
        ld hl,LINE_BUFFER
        ld a,16
        ld d,a
        ld a,(currentTab)
        or a
        jr nz,.normal_attr
        ld a,(drawIndex)
        cp SETTINGS_STYLE_COUNT
        jr nc,.normal_attr
        add a,a
        add a,a
        add a,a
        add a,a
        jr .print
.normal_attr
        ld a,d
.print
        jp plot_string

prepare_colour_row
        ld a,(drawIndex)
        add a,a
        ld e,a
        ld d,0
        ld hl,styleNameTable
        add hl,de
        ld e,(hl)
        inc hl
        ld d,(hl)
        ex de,hl
        ld de,LINE_BUFFER+2
        ld b,24
        call copy_field
        ld hl,paletteText
        ld de,LINE_BUFFER+30
        ld b,8
        call copy_field
        call draw_palette_value
        rrca
        rrca
        rrca
        rrca
        and $0f
        call hex_digit
        ld (LINE_BUFFER+38),a
        ret

prepare_key_row
        ld a,(drawIndex)
        add a,a
        ld e,a
        ld d,0
        ld hl,actionNameTable
        add hl,de
        ld e,(hl)
        inc hl
        ld d,(hl)
        ex de,hl
        ld de,LINE_BUFFER+2
        ld b,30
        call copy_field
        call draw_key_value
        call format_key_name_entry
        ld hl,KEY_NAME_BUFFER
        ld de,LINE_BUFFER+36
        ld b,28
        jp copy_field


move_up
        ld a,(currentIndex)
        or a
        ret z
        dec a
        ld (currentIndex),a
        ld hl,topIndex
        cp (hl)
        ret nc
        ld (hl),a
        ret

move_down
        call current_count
        ld b,a
        ld a,(currentIndex)
        inc a
        cp b
        ret nc
        ld (currentIndex),a
        ld b,a
        ld a,(topIndex)
        add a,VISIBLE_ROWS
        cp b
        ret nz
        ld a,b
        sub VISIBLE_ROWS-1
        ld (topIndex),a
        ret

current_count
        ld a,(currentTab)
        or a
        ld a,SETTINGS_STYLE_COUNT
        ret z
        ld a,SETTINGS_ACTION_COUNT
        ret


selected_palette_ptr
        ld ix,(ctxPtr)
        ld l,(ix+SETTINGSCTX_PALETTE)
        ld h,(ix+SETTINGSCTX_PALETTE+1)
        ld a,(currentIndex)
        ld e,a
        ld d,0
        add hl,de
        ret

draw_palette_value
        ld ix,(ctxPtr)
        ld l,(ix+SETTINGSCTX_PALETTE)
        ld h,(ix+SETTINGSCTX_PALETTE+1)
        ld a,(drawIndex)
        ld e,a
        ld d,0
        add hl,de
        ld a,(hl)
        ret

colour_up
        call selected_palette_ptr
        ld a,(hl)
        add a,16
        ld (hl),a
        ret

colour_down
        call selected_palette_ptr
        ld a,(hl)
        sub 16
        ld (hl),a
        ret


selected_key_ptr
        ld ix,(ctxPtr)
        ld l,(ix+SETTINGSCTX_KEYS)
        ld h,(ix+SETTINGSCTX_KEYS+1)
        ld a,(currentIndex)
        ld e,a
        ld d,0
        add hl,de
        ret

draw_key_value
        ld ix,(ctxPtr)
        ld l,(ix+SETTINGSCTX_KEYS)
        ld h,(ix+SETTINGSCTX_KEYS+1)
        ld a,(drawIndex)
        ld e,a
        ld d,0
        add hl,de
        ld a,(hl)
        ret

capture_key
        ld b,LIST_X
        ld c,BOX_Y+BOX_HEIGHT-2
        ld hl,captureText
        ld a,48
        call plot_string
.read
        call read_key
        cp 1
        ret z
        ld (capturedKey),a
        call selected_key_ptr
        ld (selectedKeyPtr),hl
        ld a,(capturedKey)
        cp (hl)
        ret z
        call key_already_used
        jr c,.conflict
        ld hl,(selectedKeyPtr)
        ld a,(capturedKey)
        ld (hl),a
        ret
.conflict
        ld b,LIST_X
        ld c,BOX_Y+BOX_HEIGHT-2
        ld hl,keyConflictText
        ld a,48
        call plot_string
        ld a,(capturedKey)
        call format_key_name_entry
        ld b,LIST_X+23
        ld c,BOX_Y+BOX_HEIGHT-2
        ld hl,KEY_NAME_BUFFER
        ld a,48
        call plot_string
        jr .read


; Carry is set when capturedKey is already assigned to another action.
key_already_used
        ld ix,(ctxPtr)
        ld l,(ix+SETTINGSCTX_KEYS)
        ld h,(ix+SETTINGSCTX_KEYS+1)
        ld de,(selectedKeyPtr)
        ld b,SETTINGS_ACTION_COUNT
.scan
        ld a,h
        cp d
        jr nz,.compare
        ld a,l
        cp e
        jr z,.next
.compare
        ld a,(capturedKey)
        cp (hl)
        jr z,.used
.next
        inc hl
        djnz .scan
        or a
        ret
.used
        scf
        ret


; A=INKEY code -> readable zero-terminated key name.
format_key_name
        ld de,KEY_NAME_BUFFER
        cp 1
        ld hl,keyBreak
        jp z,.copy_known
        cp 4
        ld hl,keyCs3
        jp z,.copy_known
        cp 5
        ld hl,keyCs4
        jp z,.copy_known
        cp 6
        ld hl,keyCs2
        jp z,.copy_known
        cp 7
        ld hl,keyCs1
        jp z,.copy_known
        cp 8
        ld hl,keyCs5
        jp z,.copy_known
        cp 9
        ld hl,keyCs8
        jp z,.copy_known
        cp 10
        ld hl,keyCs6
        jp z,.copy_known
        cp 11
        ld hl,keyCs7
        jp z,.copy_known
        cp 12
        ld hl,keyDelete
        jp z,.copy_known
        cp 13
        ld hl,keyEnter
        jp z,.copy_known
        cp 15
        ld hl,keyCs9
        jp z,.copy_known
        cp 32
        ld hl,keySpace
        jp z,.copy_known
        cp 127
        ld hl,keySsI
        jp z,.copy_known
        cp 199
        ld hl,keySsQ
        jp z,.copy_known
        cp 200
        ld hl,keySsE
        jp z,.copy_known
        cp 201
        ld hl,keySsW
        jp z,.copy_known
        cp 'A'
        jr c,.plain_or_code
        cp 'Z'+1
        jr nc,.plain_or_code
        ld hl,keyCapsPrefix
        call copy_zero_string
        dec de
        ld a,(formattedKey)
        ld (de),a
        inc de
        xor a
        ld (de),a
        ret
.plain_or_code
        cp 32
        jr c,.code
        cp 127
        jr nc,.code
        cp 'a'
        jr c,.store_plain
        cp 'z'+1
        jr nc,.store_plain
        sub 32
.store_plain
        ld (de),a
        inc de
        xor a
        ld (de),a
        ret
.code
        push af
        ld a,'$'
        ld (de),a
        inc de
        pop af
        push af
        rrca
        rrca
        rrca
        rrca
        call hex_digit
        ld (de),a
        inc de
        pop af
        call hex_digit
        ld (de),a
        inc de
        xor a
        ld (de),a
        ret
.copy_known
        jp copy_zero_string

; Preserve the original character for the CAPS+ branch above.
; This tiny entry wrapper avoids keeping it live across string copying.
format_key_name_entry
        ld (formattedKey),a
        jp format_key_name

copy_zero_string
        ld a,(hl)
        ld (de),a
        inc hl
        inc de
        or a
        jr nz,copy_zero_string
        ret

hex_digit
        and $0f
        add a,'0'
        cp '9'+1
        ret c
        add a,'A'-'9'-1
        ret

copy_field
        ld a,(hl)
        or a
        ret z
        ld (de),a
        inc hl
        inc de
        djnz copy_field
        ret


; IN: B=x, C=y, HL=text, A=legacy palette attribute.
plot_string
        ex de,hl
        ld h,b
        ld l,c
        jp call_print

patch_services
        ld ix,(svcPtr)
        ld l,(ix+SETTINGS_SERVICE_PRINT)
        ld h,(ix+SETTINGS_SERVICE_PRINT+1)
        ld (call_print+1),hl
        ld l,(ix+SETTINGS_SERVICE_WINDOW)
        ld h,(ix+SETTINGS_SERVICE_WINDOW+1)
        ld (call_window+1),hl
        ld l,(ix+SETTINGS_SERVICE_KEYSCAN)
        ld h,(ix+SETTINGS_SERVICE_KEYSCAN+1)
        ld (call_keyscan+1),hl
        ld l,(ix+SETTINGS_SERVICE_SYMTAB)
        ld h,(ix+SETTINGS_SERVICE_SYMTAB+1)
        ld (symTablePtr),hl
        ld l,(ix+SETTINGS_SERVICE_CAPSTAB)
        ld h,(ix+SETTINGS_SERVICE_CAPSTAB+1)
        ld (capsTablePtr),hl
        ld l,(ix+SETTINGS_SERVICE_NORMTAB)
        ld h,(ix+SETTINGS_SERVICE_NORMTAB+1)
        ld (normTablePtr),hl
        ret

call_print
        jp 0
call_window
        jp 0
call_keyscan
        jp 0

; Use Calm Commander's resident KEYSCAN and its resident decoder tables.
; The full INKEY entry also services the mouse and can call banked UI code,
; so the plugin uses the safe keyboard core exposed by the ABI instead.
read_key
        ei
        ld b,2
.delay
        halt
        djnz .delay
        call call_keyscan
        ld a,e
        inc a
        jr z,read_key
        ld a,d
        ld hl,(symTablePtr)
        cp $18
        jr z,.decode
        ld hl,(capsTablePtr)
        cp $27
        jr z,.decode
        ld hl,(normTablePtr)
.decode
        ld d,0
        add hl,de
        ld a,(hl)
        or a
        jr z,read_key
        push af
.release
        ei
        halt
        call call_keyscan
        ld a,e
        inc a
        jr nz,.release
        pop af
        ret


titleText        defb " Settings",0
coloursTabIdle   defb "  Colours  ",0
coloursTabActive defb "[ Colours ]",0
keysTabIdle      defb "  Keys  ",0
keysTabActive    defb "[ Keys ]",0
colourHeader     defb "  Interface style              Palette",0
keyHeader        defb "  Action                              Shortcut",0
hintText         defb "LEFT/RIGHT tab  UP/DOWN move  ENTER change/edit  S save",0
captureText      defb "Press new shortcut (BREAK cancels capture)                         ",0
keyConflictText  defb "Shortcut already used:                                               ",0
paletteText      defb "Palette ",0

styleNameTable
        defw styleNormal,styleDialog,styleCursor,styleButton,styleMenuSelect
        defw styleMarked,styleMarkedCursor,styleDirectory,styleExecutable
styleNameTableEnd
        assert styleNameTableEnd-styleNameTable = SETTINGS_STYLE_COUNT*2
styleNormal       defb "Normal files",0
styleDialog       defb "Dialogs and title",0
styleCursor       defb "Panel cursor",0
styleButton       defb "Buttons and prompts",0
styleMenuSelect   defb "Menu selection",0
styleMarked       defb "Marked files",0
styleMarkedCursor defb "Marked cursor",0
styleDirectory    defb "Directories",0
styleExecutable   defb "Executable files",0

actionNameTable
        defw actSysInfo,actDown,actUp,actPageDown,actPageUp,actSwitch,actEnter,actDelete
        defw actParent,actRename,actMenu,actCopy,actMove,actMark,actMkdir,actDriveL
        defw actDriveR,actSelect,actInvert,actDeselect,actSearch,actLeftPanel
        defw actRightPanel,actView,actEdit,actPlugins,actBookmarkAdd,actBookmarkList
        defw actHelp,actAttr,actFileInfo,actQuit,actSettings
actionNameTableEnd
        assert actionNameTableEnd-actionNameTable = SETTINGS_ACTION_COUNT*2
actSysInfo      defb "About Calm Commander",0
actDown         defb "Cursor down",0
actUp           defb "Cursor up",0
actPageDown     defb "Page down",0
actPageUp       defb "Page up",0
actSwitch       defb "Switch panel",0
actEnter        defb "Open / Enter",0
actDelete       defb "Delete",0
actParent       defb "Parent directory",0
actRename       defb "Rename",0
actMenu         defb "Menu",0
actCopy         defb "Copy",0
actMove         defb "Move",0
actMark         defb "Mark file",0
actMkdir        defb "Create directory",0
actDriveL       defb "Left drive",0
actDriveR       defb "Right drive",0
actSelect       defb "Select by mask",0
actInvert       defb "Invert selection",0
actDeselect     defb "Deselect by mask",0
actSearch       defb "Search",0
actLeftPanel    defb "Activate left panel",0
actRightPanel   defb "Activate right panel",0
actView         defb "View file",0
actEdit         defb "Edit file",0
actPlugins      defb "Plugin menu",0
actBookmarkAdd  defb "Add bookmark",0
actBookmarkList defb "Show bookmarks",0
actHelp         defb "Help",0
actAttr         defb "Change attributes",0
actFileInfo     defb "File info",0
actQuit         defb "Quit",0
actSettings     defb "Settings",0

keyBreak       defb "BREAK",0
keyCs1         defb "CAPS+1",0
keyCs2         defb "CAPS+2",0
keyCs3         defb "CAPS+3",0
keyCs4         defb "CAPS+4",0
keyCs5         defb "CAPS+5",0
keyCs6         defb "CAPS+6",0
keyCs7         defb "CAPS+7",0
keyCs8         defb "CAPS+8",0
keyCs9         defb "CAPS+9",0
keyDelete      defb "DELETE",0
keyEnter       defb "ENTER",0
keySpace       defb "SPACE",0
keySsI         defb "SS+I",0
keySsQ         defb "SS+Q",0
keySsE         defb "SS+E",0
keySsW         defb "SS+W",0
keyCapsPrefix  defb "CAPS+",0

ctxPtr         defw 0
svcPtr         defw 0
savedSp        defw 0
currentTab     defb 0
currentIndex   defb 0
topIndex       defb 0
visibleRow     defb 0
drawIndex      defb 0
capturedKey    defb 0
selectedKeyPtr defw 0
formattedKey   defb 0
symTablePtr    defw 0
capsTablePtr   defw 0
normTablePtr   defw 0

plugin_end
        assert plugin_end-plugin_start <= SETTINGS_PLUGIN_SIZE
        SAVEBIN "plugin/settings.ccp",SETTINGS_PLUGIN_ADDRESS,SETTINGS_PLUGIN_SIZE
