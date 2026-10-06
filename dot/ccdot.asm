; =============================================================================
; .cc - Calm Commander jako dot command pro NextZXOS
; =============================================================================
; Soubor c:/dot/cc se sklada ze tri casti:
;   0..8191  tento zavadec (NextZXOS ho nahraje na $2000 do DivMMC RAM)
;   dale     extra banka CC (build/dot/ccd_xb.bin, patri do stranky dot_xb_page)
;   dale     hlavni kod CC (build/dot/ccd.bin, patri na S1..E2)
; Po nahrani prvnich 8K nechava NextZXOS soubor otevreny hned za nimi.
;
; CC je psany pro beh z BASICu (cc.bas): ma pevne stranky a pise do celych
; bank 5, 2 a 0. Zavadec proto:
;   1. zarezervuje pevne stranky CC a alokuje stranky na zalohu (IDE_BANK),
;   2. zalohuje vsechno, co CC prepise (stranky v MMU2-MMU7 a banku 3),
;   3. docte CC ze sveho souboru a zavola ho pres RST $18 - DivMMC je tim
;      odpojena a CC bezi stejne jako po RANDOMIZE USR z cc.bas,
;   4. po Quit obnovi Next registry a pamet, uvolni stranky a vrati se do
;      BASICu, jako by se nic nestalo; pri spusteni programu jen uvolni
;      stranky a skoci do CC pres RST $20 - teprve mimo dot command smi CC
;      zavolat M_EXECCMD (nexload/run).
;
; Sestaveni (z korene projektu, viz compile.bat):
;   sjasmplus -DCC_DOT cc.asm --exp=build/dot/ccd.exp
;   sjasmplus dot/ccdot.asm --raw=build/dot/cc
; =============================================================================

        OPT --zxnext

        ; S1, E2, dot_entry, dot_launch, dot_xb_page, EXTRA_BANK_END, DOT_EXIT_LAUNCH
        INCLUDE "../build/dot/ccd.exp"

M_DOSVERSION    equ $88
M_GETHANDLE     equ $8D
M_P3DOS         equ $94
F_CLOSE         equ $9B
F_READ          equ $9D
IDE_BANK        equ $01BD

BANK_TOTAL      equ $0000               ; H=0 ZX pamet, L=duvod pro IDE_BANK
BANK_ALLOC      equ $0001
BANK_RESERVE    equ $0002
BANK_FREE       equ $0003

XB_LEN          equ EXTRA_BANK_END - $E000
MAIN_LEN        equ E2 - S1

BACKUP_COUNT    equ 8                   ; stranky v MMU2..MMU7 + banka 3 (6,7)
CALL_STACK      equ $0000               ; RST $18 se vola se zasobnikem na konci
                                        ; stranky 1 - tu CC nikdy nepise
BORDCR          equ $5C48
VARS            equ $5C4B
PROG            equ $5C53
E_LINE          equ $5C59
WORKSP          equ $5C61
MAKE_ROOM       equ $1655               ; ROM3: HL = misto, BC = pocet bajtu
INJECT_MAX      equ 160                 ; prikaz ke spusteni: [delka][priznaky] + tokeny

        org $2000

dot_start
        ld (entrySp),sp
        ld (entryIx),ix
        ld (entryBc),bc                 ; BC = prikazova radka (bez tecky)
        exx
        ld (entryHlAlt),hl              ; H'L' patri BASICu, vratime ho pri odchodu
        exx

        ld a,h                          ; HL = argumenty (0 = zadne)
        or l
        call nz,check_args
        jp c,show_usage

        rst $08
        db M_DOSVERSION
        jr c,.notNext
        or a                            ; A=0 jen v rezimu NextZXOS
        jr nz,.notNext
        ld a,b
        cp 'N'
        jr z,.isNext
.notNext
        ld hl,msgNotNext
        jp exit_error

.isNext
        call read_source_pages
        call save_nextregs

        call grab_pages                 ; M_P3DOS - jeste na zasobniku BASICu
        jp c,exit_error                 ; HL = zprava

        ; Od ted pracujeme se zasobnikem v okne dot commandu: MMU6/MMU7
        ; poslouzi jako okna a zasobnik BASICu muze lezet prave v nich.
        di
        ld sp,dotStackTop
        ei

        call backup_memory
        call load_cc
        jr nc,.loaded
        call restore_memory
        ld sp,(entrySp)
        call free_pages
        ld hl,msgLoadError
        jp exit_error

.loaded
        di
        ld sp,CALL_STACK
        ei
        rst $18
        defw dot_entry                  ; CC se vraci az pri Quit nebo spusteni

        ld a,(dot_exit_code)            ; v RAM CC, MMU6 je zpet na strance 0
        cp DOT_EXIT_LAUNCH
        jr z,.launch
        cp DOT_EXIT_BASIC
        jr nz,.quit
        ; Prikaz BASICu ke spusteni je v pameti CC - schovat ho, nez ji
        ; prepise obnova pameti BASICu.
        ld hl,dot_cmd_buf
        ld de,injectBuf
        ld bc,INJECT_MAX
        ldir
        ld a,1
        ld (injectPending),a

        ; Quit: vsechno vratit, jak bylo
.quit
        di
        ld sp,dotStackTop
        ld iy,$5C3A
        call restore_nextregs
        ld a,(injectPending)            ; TAP a snapshoty jako v BASIC verzi CC
        or a                            ; na 3,5 MHz (bit 0 priznaku od CC)
        jr z,.speedOk
        ld a,(injectBuf+1)
        rrca
        jr nc,.speedOk
        nextreg $07,0
.speedOk
        call restore_memory
        ld sp,(entrySp)                 ; pamet je zpet, zasobnik BASICu taky
        call free_pages                 ; posledni M_P3DOS srovna strankovani
        call restore_ula                ; border a MMU6/7, $7FFD uz ne (viz tam)
        ld a,(injectPending)            ; spusteni BAS/TAP/snapshotu: vloz prikaz
        or a                            ; do radku za .cc, BASIC ho pak provede
        call nz,inject_cmd
        jp c,exit_error                 ; HL = zprava
        call restore_regs
        ei
        xor a                           ; Fc=0: dot command skoncil v poradku
        ret

.launch
        ; Spousteny program dostane celou pamet; CC je porad v RAM.
        call free_pages                 ; zasobnik je porad CALL_STACK
        ld sp,(entrySp)                 ; zasobnik BASICu jako pri zadani .cc - pod
                                        ; nim je volno pro .run a jeho LOAD
        ld hl,dot_launch
        rst $20                         ; ukonci dot command a skoc na HL

; -----------------------------------------------------------------------------
; Chybovy navrat: HL = zprava (posledni znak s bitem 7)
; -----------------------------------------------------------------------------
exit_error
        ld sp,(entrySp)
        call restore_regs
        xor a
        scf
        ret

; -----------------------------------------------------------------------------
; Vlozi prikaz z injectBuf ([delka][tokeny]) do radku, ze ktereho byl .cc
; zavolan, hned za prikaz .cc. BASIC po navratu z dot commandu pokracuje
; zbytkem radku, takze ho provede, jako by ho napsal uzivatel.
; Radek napsany primo lezi v E_LINE. Z bezuciho programu se vklada do jeho
; radku a opravi se delka v hlavicce radku - spousteny program ten stavajici
; stejne nahradi.
; Vola ROM pres RST $18: zasobnik uz musi byt zasobnik BASICu.
; Fc=1, HL = zprava pri chybe.
; -----------------------------------------------------------------------------
inject_cmd
        ld hl,0
        ld (injectLenAt),hl             ; 0 = E_LINE, jinak adresa delky radku
        ld hl,(entryBc)
        ld de,(E_LINE)
        or a
        sbc hl,de
        jr c,.tryProg                   ; pred E_LINE
        ld hl,(entryBc)
        ld de,(WORKSP)
        or a
        sbc hl,de
        jr c,.scanStart                 ; E_LINE <= radek < WORKSP: zadany primo
.tryProg
        ld hl,(entryBc)
        ld de,(PROG)
        or a
        sbc hl,de
        jr c,.notFound                  ; ani v programu
        ld hl,(PROG)                    ; najdi radek programu, ve kterem .cc lezi
.line
        ex de,hl                        ; DE = zacatek radku
        ld hl,(VARS)
        or a
        sbc hl,de
        jr c,.notFound                  ; za koncem programu
        jr z,.notFound
        ld h,d
        ld l,e
        inc hl
        inc hl                          ; HL -> delka radku
        push hl
        ld c,(hl)
        inc hl
        ld b,(hl)
        inc hl
        add hl,bc                       ; HL = dalsi radek
        ex de,hl
        ld hl,(entryBc)
        or a
        sbc hl,de                       ; radek .cc - dalsi radek
        pop bc                          ; BC = adresa delky
        ex de,hl                        ; HL = dalsi radek (priznaky zustavaji)
        jr nc,.line
        ld (injectLenAt),bc
.scanStart
        ld hl,(entryBc)
        ld c,0                          ; C bit 0 = uvnitr uvozovek
.scan                                   ; konec .cc: ':' mimo uvozovky, $0D, 0
        ld a,(hl)
        or a
        jr z,.found
        cp $0D
        jr z,.found
        cp '"'
        jr nz,.notQuote
        ld a,c
        xor 1
        ld c,a
        jr .next
.notQuote
        bit 0,c
        jr nz,.next
        cp ':'
        jr z,.found
        cp $0E                          ; skryte cislo: 5 bajtu, muze v nich byt
        jr nz,.next                     ; cokoli vcetne ':' a $0D
        inc hl
        inc hl
        inc hl
        inc hl
        inc hl
.next
        inc hl
        jr .scan
.found
        ld (injectAt),hl
        ld a,(injectBuf)
        ld c,a
        ld b,0
        rst $18                         ; MAKE-ROOM: misto o BC bajtech pred (HL),
        defw MAKE_ROOM                  ; posune pamet BASICu a opravi ukazatele
        ld hl,injectBuf+2               ; [delka][priznaky][tokeny]
        ld de,(injectAt)
        ld a,(injectBuf)
        ld c,a
        ld b,0
        ldir
        ld hl,(injectLenAt)             ; radek programu: delka += vlozeno
        ld a,h
        or l
        ret z                           ; E_LINE (Fc=0 po "or l")
        ld a,(injectBuf)
        add a,(hl)
        ld (hl),a
        inc hl
        ld a,0
        adc a,(hl)
        ld (hl),a
        or a
        ret
.notFound
        ld hl,msgNotDirect
        scf
        ret

; H'L' a IX tak, jak je dot command dostal (HL zustava)
restore_regs
        exx
        ld hl,(entryHlAlt)
        exx
        ld ix,(entryIx)
        ret

; -----------------------------------------------------------------------------
; Argumenty: zadne nebo -h. Cokoli jineho vypise napovedu (Fc=1).
; -----------------------------------------------------------------------------
check_args
        ld a,(hl)
        cp ' '
        jr nz,.first
        inc hl
        jr check_args
.first
        or a
        ret z
        cp $0D
        ret z
        cp ':'
        ret z
        scf
        ret

show_usage
        ld hl,msgUsage
.loop
        ld a,(hl)
        or a
        jr z,.done
        rst $10
        inc hl
        jr .loop
.done
        call restore_regs
        xor a
        ret

; -----------------------------------------------------------------------------
; Next registry, ktere CC meni (displej, vrstvy, tilemap, turbo)
; -----------------------------------------------------------------------------
nextregList
        db $07, $14, $15, $2F, $30, $31, $4A, $4B, $4C
        db $68, $69, $6B, $6C, $6E, $6F, $43
NEXTREG_COUNT   equ $ - nextregList

read_nextreg                            ; A = registr -> A = hodnota
        push bc
        ld bc,$243B
        out (c),a
        inc b
        in a,(c)
        pop bc
        ret

save_nextregs
        ld hl,nextregList
        ld de,nextregValues
        ld b,NEXTREG_COUNT
.loop
        ld a,(hl)
        call read_nextreg
        ld (de),a
        inc hl
        inc de
        djnz .loop
        ret

restore_nextregs
        ld hl,nextregList
        ld de,nextregValues
        ld b,NEXTREG_COUNT
.loop
        push bc
        ld a,(hl)
        ld bc,$243B
        out (c),a
        inc b
        ld a,(de)
        out (c),a
        pop bc
        inc hl
        inc de
        djnz .loop
        ret

; Border podle obnovene BORDCR a MMU6/MMU7 jako pri startu.
; Port $7FFD sem NEPATRI: kazde M_P3DOS na konci samo nastavi strankovani,
; jak ho NextZXOS uvnitr dot commandu potrebuje, a BANKM mu neodpovida.
; Zapis BANKM do $7FFD po M_P3DOS (free_pages) NextZXOS po navratu z dotu
; zasekne - overeno testy .cc 4 / 7 / 9.
restore_ula
        ld a,(BORDCR)
        rrca
        rrca
        rrca
        and 7
        out ($FE),a
        ld a,(sourcePages+4)
        nextreg $56,a
        ld a,(sourcePages+5)
        nextreg $57,a
        ret

; -----------------------------------------------------------------------------
; Stranky, ktere zalohujeme: co je v MMU2..MMU7 a banka 3 (CC tam kopiruje)
; -----------------------------------------------------------------------------
read_source_pages
        ld hl,sourcePages
        ld a,$52
.loop
        push af
        call read_nextreg
        ld (hl),a
        inc hl
        pop af
        inc a
        cp $58
        jr nz,.loop
        ld (hl),6
        inc hl
        ld (hl),7
        ret

; -----------------------------------------------------------------------------
; IDE_BANK pres M_P3DOS. HL = typ/duvod, E = stranka -> Fc=0 ok, E = vysledek
; Vola se jen se zasobnikem mimo okno dot commandu.
; -----------------------------------------------------------------------------
ide_bank
        exx
        ld de,IDE_BANK
        ld c,7
        rst $08
        db M_P3DOS
        ccf                             ; +3DOS hlasi uspech s Fc=1
        ret

; -----------------------------------------------------------------------------
; Zarezervuje pevne stranky CC a alokuje BACKUP_COUNT stranek na zalohu.
; Fc=1 -> HL = zprava, nic nezustane zabrane.
; -----------------------------------------------------------------------------
; Pevne stranky CC: LFN levy panel od 24, pravy od 60; buffl/buffr/savescr
; 74/76/78; viewer 81-97 (plugin 82); extra banka 90; pracovni 98, 99;
; Layer 2 pluginu NXI/SCR v 16K bankach 49-51 = stranky 98-103.
fixedRanges
        db 24, 44
        db 60, 103
        db $FF

grab_pages
        xor a
        ld (ownedCount),a
        ld hl,BANK_TOTAL
        call ide_bank
        jr c,.noMemory
        ld a,e
        ld (totalPages),a

        ld hl,fixedRanges
.range
        ld a,(hl)
        cp $FF
        jr z,.backups
        ld b,a                          ; B = prvni stranka
        inc hl
        ld c,(hl)                       ; C = posledni stranka
        inc hl
        push hl
.page
        ld a,(totalPages)
        dec a
        cp b
        jr c,.rangeDone                 ; stranka uz v tomhle stroji neni
        push bc
        ld e,b
        ld hl,BANK_RESERVE
        call ide_bank
        pop bc
        jr c,.inUse
        ld a,b
        call own_page
        ld a,b
        cp c
        jr z,.rangeDone
        inc b
        jr .page
.rangeDone
        pop hl
        jr .range

.inUse
        pop hl
        ld a,b
        ld hl,msgInUseNum
        call put_decimal
        call free_pages
        ld hl,msgInUse
        scf
        ret

.backups
        ld de,backupPages
        ld b,BACKUP_COUNT
.alloc
        push bc
        push de
        ld hl,BANK_ALLOC
        call ide_bank
        ld a,e
        pop de
        pop bc
        jr c,.noMemoryFree
        ld (de),a
        inc de
        call own_page
        djnz .alloc
        or a
        ret

.noMemoryFree
        call free_pages
.noMemory
        ld hl,msgNoMemory
        scf
        ret

own_page                                ; A = stranka do seznamu zabranych
        push de
        push hl
        ld hl,ownedCount
        ld e,(hl)
        inc (hl)
        ld d,0
        ld hl,ownedPages
        add hl,de
        ld (hl),a
        pop hl
        pop de
        ret

free_pages
        ld a,(ownedCount)
        or a
        ret z
        ld b,a
        ld hl,ownedPages
.loop
        push bc
        push hl
        ld e,(hl)
        ld hl,BANK_FREE
        call ide_bank
        pop hl
        pop bc
        inc hl
        djnz .loop
        xor a
        ld (ownedCount),a
        ret

; A = 0..255 -> tri cislice na (HL)
put_decimal
        ld c,100
        call .digit
        ld c,10
        call .digit
        add a,'0'
        ld (hl),a
        ret
.digit
        ld b,'0'-1
.sub
        inc b
        sub c
        jr nc,.sub
        add a,c
        ld (hl),b
        inc hl
        ret

; -----------------------------------------------------------------------------
; Zaloha / obnova pameti: HL = seznam zdrojovych stranek, DE = cilovych.
; Kopiruje pres MMU6 (zdroj) a MMU7 (cil). Zasobnik musi byt v okne dotu.
; -----------------------------------------------------------------------------
backup_memory
        ld hl,sourcePages
        ld de,backupPages
        jr copy_pages

restore_memory
        ld hl,backupPages
        ld de,sourcePages

copy_pages
        di
        ld a,$07
        call read_nextreg
        ld (copyTurbo),a
        nextreg $07,3                   ; 28 MHz, at je to hned
        ld b,BACKUP_COUNT
.loop
        ld a,(hl)
        nextreg $56,a
        ld a,(de)
        nextreg $57,a
        push bc
        push hl
        push de
        ld hl,$C000
        ld de,$E000
        ld bc,$2000
        ldir
        pop de
        pop hl
        pop bc
        inc hl
        inc de
        djnz .loop
        ld a,(sourcePages+4)
        nextreg $56,a
        ld a,(sourcePages+5)
        nextreg $57,a
        ld a,(copyTurbo)
        nextreg $07,a
        ei
        ret

; -----------------------------------------------------------------------------
; Docte CC z vlastniho souboru. Fc=1 pri chybe. Soubor vzdy zavre.
; -----------------------------------------------------------------------------
load_cc
        rst $08
        db M_GETHANDLE
        ret c
        ld (dotHandle),a

        nextreg $57,dot_xb_page         ; extra banka rovnou do sve stranky
        ld hl,$E000
        ld bc,XB_LEN
        call read_part
        ld a,(sourcePages+5)
        nextreg $57,a
        jr c,.close

        ld hl,S1
        ld bc,MAIN_LEN
        call read_part
.close
        push af
        ld a,(dotHandle)
        rst $08
        db F_CLOSE
        pop af
        ret

read_part                               ; HL = kam, BC = kolik -> Fc=1 chyba
        push bc
        ld a,(dotHandle)
        rst $08
        db F_READ
        pop hl
        ret c
        or a
        sbc hl,bc                       ; precteno vsechno?
        ret z
        scf
        ret

; -----------------------------------------------------------------------------
; Texty a promenne
; -----------------------------------------------------------------------------
msgUsage
        db "Calm Commander", 13
        db "File manager for ZX Spectrum Next", 13, 13
        db "Usage: .cc", 13
        db "Plugins: c:/sys/cc/*.ccp", 13, 0
msgNotNext      db "Requires NextZXO", 'S'|$80
msgNoMemory     db "Not enough memor", 'y'|$80
msgLoadError    db "CC load erro", 'r'|$80
msgNotDirect    db "Start from command lin", 'e'|$80
msgInUse        db "Page "
msgInUseNum     db "000 in us", 'e'|$80

entrySp         dw 0
entryIx         dw 0
entryBc         dw 0
entryHlAlt      dw 0
injectPending   db 0
injectAt        dw 0
injectLenAt     dw 0                    ; adresa delky radku programu, 0 = E_LINE
injectBuf       ds INJECT_MAX
dotHandle       db 0
copyTurbo       db 0
totalPages      db 0
ownedCount      db 0
ownedPages      ds 100
sourcePages     ds BACKUP_COUNT
backupPages     ds BACKUP_COUNT
nextregValues   ds NEXTREG_COUNT

                ds 256
dotStackTop

        ASSERT S1 = $7100
        ASSERT $ <= $4000
        DISPLAY "Zavadec .cc: ",/A,$ - $2000," z 8192 B"

        ; NextZXOS nahraje jen prvnich 8K, zbytek si dot cte sam
        BLOCK $4000 - $, 0
        INCBIN "../build/dot/ccd_xb.bin"
        INCBIN "../build/dot/ccd.bin"
