; ============================================================
; Konstanty / nastavení bufferu pro kopírování
; ============================================================
LENGHT_BUFFER    equ 6*1024      ; velikost bloku pro čtení/zápis (6 KB na iteraci)
PAGE_BUFF        equ 3           ; stránka RAM (bank/page), do které se čte/zapisuje (používá READ/WRITE)

; ============================================================
; createfile
; Vytvoří cílový soubor podle LFNNAME v aktuálním cílovém okně.
; Postup:
;  1) PROHOD → přepne aktivní panel (typicky aby se pracovalo s cílovým oknem)
;  2) dospage + $01B1 → nastav aktuální adresář podle pathl
;  3) DOS call 0106h → create file (handle B=2) v režimu exclusive WRITE
;  4) znovu PROHOD → vrať se do původního okna a obnov DOS kontext
; Pozn.: 0106h se tady používá jak pro create, tak později pro open – dle API Next/DOS.
; ============================================================
createfile
CR
        call PROHOD                                ; přepni na druhé okno (cílový panel)

        call dospage                               ; přepni do DOS paměťové stránky (kvůli DOS rutinám)
        ld hl,pathl
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a                                    ; HL = pointer na path string pro aktuální okno

        xor a
        call $01b1                                 ; DOS: změň adresář / nastav cestu (podle pathl)

        ; Parametry pro DOS create:
        ; B = číslo souboru/handle (2) – u tebe "soubor číslo 1" (pravděpodobně handle 2 = cílový)
        ; C = mód (2 = exclusive WRITE)
        ; D/E = další parametry (zde D=2, E=4) – typicky atributy/flags dle DOS API
        ; HL = pointer na LFN (LFNNAME)
        ld b,2                                     ; handle cílového souboru (u tebe "soubor číslo 1")
        ld c,2                                     ; exclusive WRITE
        ld d,2
        ld e,4
        ld hl,LFNNAME
        call 0106h                                 ; DOS: create file

        ; vrať kontext zpátky (původní okno)
        call PROHOD
        call dospage
        ld hl,pathl
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        xor a
        call $01b1                                 ; DOS: nastav cestu pro původní okno
        call basicpage                             ; zpět do základní stránky
        ret


; ============================================================
; closefile
; Zavře soubor podle handle v registrech BC (B = handle? C = ??? dle DOS API).
; Volající předává BC už nastavené (proto push/pop).
; ============================================================
closefile
        push bc
        call dospage
        pop bc
        call 0109h                                 ; DOS: close file (dle API)
        call basicpage
        ret


konecread defb 0                                    ; flag: 1 = poslední blok byl přečten (konec souboru)


; ============================================================
; readfile
; Zkopíruje obsah ze zdroje (handle B=0) do cíle (handle B=2) po blocích.
; Čte do RAM na adrese 49152 (0xC000) na stránce PAGE_BUFF a pak zapisuje.
;
; Důležité:
;  - velikost souboru je držena v LFNNAME+261..+264 (32-bit: low word HL, high word DE)
;  - rutina si průběžně ZMENŠUJE tento 32-bit “remaining size” o LENGHT_BUFFER
;  - pocetbytu (self-mod) drží délku aktuálního bloku pro READ/WRITE
;  - konecread = 1 označí poslední iteraci
;  - souběžně aktualizuje progress (PROGPROM2 + PROGRES2) přes CPPX2/CPPX4
; ============================================================
readfile
READ
        xor a
        ld (konecread),a                           ; začínáme → ještě nejsme na konci

        call dospage
        call NOBUFF83                              ; zřejmě vypne/odpojí 8.3 bufferování (aby nekolidovalo s I/O)

read0
        ; ---- progress tick (PROGPROM2++)
        ld   hl,(PROGPROM2)
        inc  hl
        ld   (PROGPROM2),hl
CPPX2   ld   de,0                                  ; (self-mod) prah pro progress
        or   a
        sbc  hl,de
        jr   c,CPPNE2
CPPX4   ld   b,1                                  ; (self-mod) krok / šířka
        ld   (PROGPROM2),hl
        call PROGRES2
CPPNE2

        ; ====================================================
        ; DELKA: rozhodni kolik bajtů teď číst/zapsat
        ; ====================================================
DELKA
        ; remaining size (32-bit) je uložená v LFNNAME+261..+264:
        ;   HL = low 16
        ;   DE = high 16
        ld hl,(LFNNAME+261)
        ld de,(LFNNAME+261+2)

        ; pokud high word != 0 → soubor je > 64 KB, vždy čti plný blok (6 KB)
        ld a,d
        or e
        jr z,bit16

        ; --- 32-bit případ: odečti LENGHT_BUFFER z DE:HL a nastav pocetbytu=LENGHT_BUFFER
        push hl
        ld hl,LENGHT_BUFFER
        ld (pocetbytu+1),hl                        ; self-mod operand: DE = počet bajtů pro READ/WRITE
        pop hl

        ld bc,LENGHT_BUFFER
        call sub32                                 ; DE:HL = DE:HL - BC (32-bit odečet)
        jr dalcti

bit16
        ; --- 16-bit případ (high=0): ověř, jestli remaining <= LENGHT_BUFFER
        ld bc,LENGHT_BUFFER
        or a
        sbc hl,bc
        add hl,bc                                  ; HL zpátky, jen test carry / zero
        jr c,posledniread
        jr z,posledniread

        ; remaining > LENGHT_BUFFER → odečti blok a nastav pocetbytu=LENGHT_BUFFER
        or a
        sbc hl,bc
        push hl
        ld hl,LENGHT_BUFFER
        ld (pocetbytu+1),hl
        pop hl
        jr dalcti

posledniread
        ; remaining <= LENGHT_BUFFER → poslední iterace
        ld a,1
        ld (konecread),a
        ld (pocetbytu+1),hl                        ; DE = remaining (v self-mod operandu) – čteme už jen zbytek

dalcti
        ; ulož nový remaining size zpátky do LFNNAME+261..+264
        ld (LFNNAME+261),hl
        ld (LFNNAME+261+2),de

        ; ====================================================
        ; READ: zdroj (handle B=0) → RAM stránka PAGE_BUFF, adresa 49152
        ; ====================================================
        ld b,0                                     ; handle zdrojového souboru
        ld c,PAGE_BUFF                             ; page/bank pro transfer
pocetbytu ld de,0                                  ; (self-mod) DE = počet bajtů
        ld hl,49152                                ; cílová adresa v RAM
        call 0112h                                 ; DOS: READ (handle B do bufferu HL, délka DE, page C)

        ; ====================================================
        ; WRITE: RAM → cíl (handle B=2) ve stejné page/adrese
        ; ====================================================
        ld c,PAGE_BUFF
        ld b,2                                     ; handle cílového souboru
        ld de,(pocetbytu+1)                        ; délka stejného bloku
        ld hl,49152
        call 0115h                                 ; DOS: WRITE

        ; další iterace, pokud nebyl konec
        ld a,(konecread)
        or a
        jr z,read0

konec
        call basicpage
        ret


; ============================================================
; openfile
; Otevře zdrojový soubor (handle B=0) pro čtení:
;  - vstupně HL ukazuje na položku v seznamu (záznam okna); používá find83/FINDLFN
;  - vykopíruje LFNNAME do bfname a vypíše název v UI
;  - spočítá něco z TMP83+11 (velikost?) a nastaví progress PROGRES2
;  - připraví TMP83: odstraní stavové bity (bit7 v každém znaku jména),
;    nastaví 255 terminátory na koncích 8.3 i LFN jména
;  - DOS open: 0106h s C=1 (exclusive READ), HL=TMP83 (8.3 entry)
; ============================================================
openfile
        push hl                                    ; uložit pointer na položku

        inc hl                                     ; obvykle pointer na data záznamu (přeskoč délku/flag)
;       call BUFF83
        call find83                                 ; načti 8.3 entry do TMP83
        pop hl
        push hl
        call FINDLFN                                ; načti dlouhé jméno do LFNNAME (+ metadata)

        ; zkopíruj jméno do bfname a ukaž v dialogu
        ld hl,LFNNAME
        ld de,bfname
        ld bc,45
        ldir

        ld hl,11*256+13
        ld a,16
        ld de,bfname
        call print

        ; ----------------------------------------------------
        ; Příprava progressu: bere hodnotu z TMP83+11
        ; (pravděpodobně velikost souboru / počet sektorů / počet bloků)
        ; ----------------------------------------------------
;       call BUFF83
        ld hl,(TMP83+11)                           ; tady TMP83+11..12 jako 16-bit číslo
ZDE
        ld c,6
        call deleno                                ; dělení HL / 6? (výsledek dle tvé deleno rutiny)
        inc hl                                     ; +1, aby progress nebyl nulový

of0
        ex de,hl
        call PROVYP                                 ; nastav parametry progressu podle DE
        add a,a
        ld   (CPPX2+1),hl
        ld   (CPPX4+1),a
        ld   hl,0
        ld   (PROGPROM2),hl
        ld   hl,$4000+160*14+23
        ld (PROGS2+1),hl
        call clearpr                                ; vymaž progress grafiku

        pop hl                                      ; obnov pointer na položku
LF
        call FINDLFN                                ; znovu načti LFN (redundantní, ale zachováno)

        ; ---- terminátory a normalizace názvů:
        ld hl,LFNNAME+255
        ld (hl),255                                 ; LFNNAME ukončeno 255 (tvůj string terminátor)

        ; v TMP83 vynuluj bit 7 každého bajtu jména (odstranění příznaků/diakritiky/selected markeru)
        ld b,11
        ld hl,TMP83
CCCA
        res 7,(hl)
        inc hl
        djnz CCCA

        ; najdi poslední znak 8.3 jména v TMP83 (odzadu) a za něj dej 255 terminátor
        ld hl,TMP83+10
achng2  ld a,(hl)
        cp 32                                      ; space = výplň
        jr nz,azap
        dec hl
        jr achng2
azap
        ld a,255
        inc hl
        ld (hl),a                                  ; terminátor za poslední znak

        ; totéž pro LFNNAME (odzadu) – terminátor za poslední ne-space
        ld hl,LFNNAME+254
achnga2 ld a,(hl)
        cp 32
        jr nz,azapa
        dec hl
        jr achnga2
azapa
        ld a,255
        inc hl
        ld (hl),a

        ; ----------------------------------------------------
        ; DOS OPEN (zdroj): handle B=0, C=1 (exclusive READ)
        ; HL = TMP83 (8.3 entry / nebo struktura pro DOS)
        ; ----------------------------------------------------
        call dospage
        ld b,0                                     ; handle zdroje
        ld c,1                                     ; exclusive READ
        ld e,2
        ld hl,TMP83
        call 0106h                                 ; DOS: open file

        call basicpage
        ret


; ============================================================
; sub32
; Odečte 16-bit BC od 32-bit hodnoty v DE:HL.
; Vstup: DE:HL, BC
; Výstup: DE:HL = DE:HL - BC
; Používá carry z SBC HL,BC a při borrow sníží DE o 1.
; ============================================================
; DEHL - BC = DEHL
sub32
        xor a
        sbc hl,bc
        ret nc
        dec de
        ret


size    defs 4                                     ; rezervace 4 bajtů (pravděpodobně pro 32-bit size jinde)
