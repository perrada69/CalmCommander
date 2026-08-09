SETTINGS_PLUGIN_ADDRESS equ $C000
SETTINGS_PLUGIN_SIZE    equ 8192
SETTINGS_ABI            equ 5

SETTINGSCTX_ABI         equ 0
SETTINGSCTX_PALETTE     equ 1
SETTINGSCTX_KEYS        equ 3
SETTINGSCTX_RESULT      equ 5
SETTINGSCTX_ERROR       equ 6
SETTINGSCTX_SCHEME      equ 7
SETTINGSCTX_SIZE        equ 9

SETTINGS_SERVICE_PRINT  equ 0
SETTINGS_SERVICE_WINDOW equ 2
SETTINGS_SERVICE_KEYSCAN equ 6
SETTINGS_SERVICE_SYMTAB  equ 8
SETTINGS_SERVICE_CAPSTAB equ 10
SETTINGS_SERVICE_NORMTAB equ 12

SETTINGS_PALETTE_COUNT  equ 16
SETTINGS_STYLE_COUNT    equ 16
SETTINGS_COLOUR_PRIMARY_BYTES equ SETTINGS_STYLE_COUNT*2
SETTINGS_COLOUR_BLUE_BYTES equ 4
SETTINGS_COLOUR_BYTES   equ SETTINGS_COLOUR_PRIMARY_BYTES+SETTINGS_COLOUR_BLUE_BYTES
SETTINGS_ACTION_COUNT   equ 33

SETTINGS_SCHEME_DEFAULT equ 0
SETTINGS_SCHEME_LIGHT   equ 1
SETTINGS_SCHEME_DARK    equ 2
SETTINGS_SCHEME_CUSTOM  equ 3
SETTINGS_SCHEME_COUNT   equ 3
SETTINGS_COLOUR_VERSION equ 1

; Per style: background RRRGGGBB followed by foreground RRRGGGBB. The final
; four bytes hold blue bit 0 for all 32 colours (background first).
        MACRO EMIT_SETTINGS_DEFAULT_COLOURS
        db %000'000'10,%110'110'11
        db %000'000'01,%111'111'11
        db %000'100'11,%111'111'11
        db %111'111'11,%000'000'00
        db %011'101'00,%000'000'00
        db %110'100'00,%000'000'00
        db %100'011'00,%000'000'00
        db %000'000'10,%000'111'11
        db %000'000'10,%010'111'00
        db %011'000'00,%101'101'11
        db %000'000'01,%111'111'00
        db %000'000'01,%000'111'00
        db %000'000'01,%000'111'11
        db %000'000'01,%000'111'00
        db %000'000'01,%111'111'00
        db %000'000'01,%111'000'00
        db $7a,$81,$00,$02
        ENDM

ACT_SYSINFO             equ 1
ACT_DOWN                equ 2
ACT_UP                  equ 3
ACT_PAGE_DOWN           equ 4
ACT_PAGE_UP             equ 5
ACT_SWITCH_PANEL        equ 6
ACT_ENTER               equ 7
ACT_DELETE              equ 8
ACT_PARENT              equ 9
ACT_RENAME              equ 10
ACT_MENU                equ 11
ACT_COPY                equ 12
ACT_MOVE                equ 13
ACT_MARK                equ 14
ACT_MKDIR               equ 15
ACT_DRIVE_LEFT          equ 16
ACT_DRIVE_RIGHT         equ 17
ACT_SELECT_MASK         equ 18
ACT_INVERT_MARKS        equ 19
ACT_DESELECT_MASK       equ 20
ACT_SEARCH              equ 21
ACT_LEFT_PANEL          equ 22
ACT_RIGHT_PANEL         equ 23
ACT_VIEW                equ 24
ACT_EDIT                equ 25
ACT_PLUGIN_MENU         equ 26
ACT_BOOKMARK_ADD        equ 27
ACT_BOOKMARK_LIST       equ 28
ACT_HELP                equ 29
ACT_CHANGE_ATTR         equ 30
ACT_FILE_INFO           equ 31
ACT_QUIT                equ 32
ACT_SETTINGS            equ 33
