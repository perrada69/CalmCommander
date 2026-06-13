; ============================================================
; Directory info plugin host.
; Loads c:/CalmCommander/plugin/dir_info.ccp and displays the
; result in the same style as File info.
; ============================================================

        include "plugin/dir_info_api.i.asm"

DIR_INFO_PLUGIN_BANK   equ 41
DIR_INFO_PLUGIN_PAGE   equ DIR_INFO_PLUGIN_BANK*2
DIR_INFO_WORK_PAGE     equ 98

dirInfoContext equ sysCopyContext
dirInfoActive  equ sysCopyContext+DIRINFOCTX_ABI

dirInfoPluginName defb "dir_info.ccp",255
