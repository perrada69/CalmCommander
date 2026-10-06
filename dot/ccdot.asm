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
BANKM           equ $5B5C

        org $2000

dot_start
        ld (entrySp),sp
        ld (entryIx),ix
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

        ; Quit: vsechno vratit, jak bylo
        di
        ld sp,dotStackTop
        ld iy,$5C3A
        call restore_nextregs
        call restore_memory
        ld sp,(entrySp)                 ; pamet je zpet, zasobnik BASICu taky
        call free_pages
        call restore_ula
        call restore_regs
        ei
        xor a                           ; Fc=0: dot command skoncil v poradku
        ret

.launch
        ; Spousteny program dostane celou pamet; CC je porad v RAM.
        call free_pages                 ; zasobnik je porad CALL_STACK
        ld sp,S1                        ; zasobnik CC, stejne jako po CLEAR 28927
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

; Border a $7FFD podle obnovenych systemovych promennych.
restore_ula
        ld a,(BORDCR)
        rrca
        rrca
        rrca
        and 7
        out ($FE),a
        ld a,(BANKM)
        ld bc,$7FFD
        out (c),a                       ; prepise i MMU6/MMU7 podle banky...
        ld a,(sourcePages+4)
        nextreg $56,a                   ; ...tak je vratime presne
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
msgInUse        db "Page "
msgInUseNum     db "000 in us", 'e'|$80

entrySp         dw 0
entryIx         dw 0
entryHlAlt      dw 0
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
