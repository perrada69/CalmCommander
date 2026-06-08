; ============================================================
; Viewer plugin discovery.
; Runs from S1, because it temporarily maps a scratch page at
; $A000 for DOS catalog. The runtime tables live with viewer.asm
; in S3 and are accessed after NOBUFF83 maps page 5 back.
; ============================================================

view_discover_plugins
        di
        call NOBUFF83
        ld hl,0
        ld (viewPluginCount),hl
        ld (viewDbgCatalogCount),hl

        call dospage
        ld hl,viewPluginDir
        xor a
        call $01b1

        call BUFF83
        ld hl,catbuff
        ld de,catbuff+1
        ld bc,1024
        xor a
        ld (hl),a
        ldir
        ld de,catbuff
        ld b,pocetpolozek
        ld c,%101
        ld hl,stardstar
        call dos_catalog
        ld (savehl),hl
        push bc
        call basicpage
        pop bc
        push bc
        call NOBUFF83
        ld a,b
        ld (viewDbgCatalogCount),a
        call BUFF83
        pop bc

        ld a,b
        or a
        jr z,.done_mapped
        dec a
        jr z,.done_mapped
        ld b,a
        ld hl,catbuff+13
.scan_loop
        push bc
        push hl
        ld de,bufftmp
        ld bc,13
        ldir
        call dospage
        ld de,bufftmp
        ld hl,stardstar
        ld ix,(savehl)
        ld bc,LFNNAME
        call $01b7
        call basicpage
        call NOBUFF83
        ld hl,viewDbgScanCount
        inc (hl)

        call view_try_add_plugin_lfn

        pop hl
        ld de,13
        add hl,de
        call BUFF83
        pop bc
        djnz .scan_loop

.done_mapped
        call NOBUFF83
.done
        ret
