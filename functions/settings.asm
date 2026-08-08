; Thin resident host for the 8KB Settings plugin. The live palette/key tables
; stay in Cfg so startup loading, Settings, dispatch and Help all see the same
; values; all editor UI and capture logic live outside resident RAM.

settings_open
        ld a,SETTINGS_ABI
        ld (sysCopyContext+SETTINGSCTX_ABI),a
        ld hl,cfgPaletteMap
        ld (sysCopyContext+SETTINGSCTX_PALETTE),hl
        ld hl,cfgKeyBindings
        ld (sysCopyContext+SETTINGSCTX_KEYS),hl
        xor a
        ld (sysCopyContext+SETTINGSCTX_RESULT),a
        ld (sysCopyContext+SETTINGSCTX_ERROR),a

        call savescr
        ld hl,settingsPluginName
        ld (syscopyLoadName+1),hl
        ld hl,SETTINGS_PLUGIN_SIZE
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

        ld a,(sysCopyContext+SETTINGSCTX_RESULT)
        or a
        jp z,loop0
        call settings_save_config
        call kresli
        call obnov_okna
        call freespace
        jp loop0
