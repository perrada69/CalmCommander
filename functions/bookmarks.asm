; Thin resident host. Dialogs, keyboard handling and file I/O stay in
; bookmarks.ccp; sysCopyContext and LFNNAME are safe scratch space here.

        include "plugin/bookmarks_api.i.asm"

bookmarks_add
        ld a,BOOKMARK_MODE_ADD
        jr bookmarks_run

bookmarks_list
        ld a,BOOKMARK_MODE_LIST

bookmarks_run
        ld (sysCopyContext+BOOKMARKCTX_MODE),a
        ld a,BOOKMARK_ABI
        ld (sysCopyContext+BOOKMARKCTX_ABI),a
        call syscopy_get_active_path
        ld (sysCopyContext+BOOKMARKCTX_PATH),hl
        ld hl,actdisc
        call ROZHOD
        ld a,(hl)
        ld (sysCopyContext+BOOKMARKCTX_DRIVE),a
        ld hl,LFNNAME
        ld (sysCopyContext+BOOKMARKCTX_RESULT_PATH),hl
        xor a
        ld (sysCopyContext+BOOKMARKCTX_RESULT),a

        call savescr
        ld hl,bookmarksPluginName
        ld (syscopyLoadName+1),hl
        ld hl,BOOKMARK_PLUGIN_SIZE
        ld (syscopyLoadSize+1),hl
        call syscopy_load_plugin
        jr c,.restore
        call syscopy_call_plugin
.restore
        ld hl,sysCopyPluginName
        ld (syscopyLoadName+1),hl
        ld hl,SYSCOPY_PLUGIN_SIZE
        ld (syscopyLoadSize+1),hl
        call loadscr

        ld a,(sysCopyContext+BOOKMARKCTX_RESULT)
        or a
        jp z,loop0

        ld hl,actdisc
        call ROZHOD
        ld a,(sysCopyContext+BOOKMARKCTX_RESULT_DRIVE)
        ld (hl),a
        call syscopy_get_active_path
        ex de,hl
        ld hl,LFNNAME
        ld bc,BOOKMARK_PATH_SIZE
        ldir
        call reload_panels_after_cancel
        call dospage
        call zapisCfg
        call basicpage
        jp loop0

bookmarksPluginName defb "bookmarks.ccp",255
