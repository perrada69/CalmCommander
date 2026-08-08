BOOKMARK_PLUGIN_ADDRESS equ 49152
; One complete MMU6 slot: $C000-$DFFF.
BOOKMARK_PLUGIN_SIZE    equ 8192

BOOKMARK_ABI            equ 2
BOOKMARK_MODE_ADD       equ 1
BOOKMARK_MODE_LIST      equ 2

; The host deliberately reuses sysCopyContext while the bookmark plugin runs.
BOOKMARKCTX_ABI         equ 0
BOOKMARKCTX_MODE        equ 1
BOOKMARKCTX_PATH        equ 2
BOOKMARKCTX_DRIVE       equ 4
BOOKMARKCTX_RESULT      equ 5
BOOKMARKCTX_ERROR       equ 6
BOOKMARKCTX_RESULT_DRIVE equ 7
BOOKMARKCTX_RESULT_PATH equ 8
BOOKMARKCTX_SIZE        equ 10

BOOKMARK_PATH_SIZE      equ 264
BOOKMARK_OLD_NAME_SIZE  equ 13
BOOKMARK_OLD_RECORD_SIZE equ BOOKMARK_OLD_NAME_SIZE+1+BOOKMARK_PATH_SIZE
BOOKMARK_NAME_SIZE      equ 25       ; 24 characters plus terminator
BOOKMARK_RECORD_SIZE    equ BOOKMARK_NAME_SIZE+1+BOOKMARK_PATH_SIZE
BOOKMARK_MAX_COUNT      equ 200

; DE points to this host service table on entry. Calls use the native Calm
; Commander conventions: PRINT (HL=XY, DE=text, A=attribute) and WINDOW
; (HL=XY, BC=width/height, A=attribute).
BOOKMARK_SERVICE_PRINT  equ 0
BOOKMARK_SERVICE_WINDOW equ 2
