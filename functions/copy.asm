; ------------------------------------------------------------
; no_copy_move
; Zobrazí hlášku, že kopírování/přesun není možný (typicky: cílová cesta neexistuje,
; nebo zdroj/cíl jsou stejné apod.) a čeká na stisk klávesy.
; ------------------------------------------------------------
no_copy_move
        call savescr                               ; uložit aktuální obrazovku (pod dialog)
        ld hl,8 * 256 + 10                         ; pozice okna (X=8, Y=10)  (typicky HL = X*256 + Y)
        ld bc,60 * 256 + 3                         ; rozměr okna (šířka=60, výška=3)
        ld a,16                                    ; atribut/barva dialogu
        call window

        ld hl,11*256+11                            ; text řádek 1
        ld a,16
        ld de,nocopy
        call print

        ld hl,11*256+12                            ; text řádek 2
        ld a,16
        ld de,nocopy2
        call print

        ld hl,44*256+13                            ; "Break"/"Cancel" nápověda
        ld a,32
        ld de,breaktxt
        call print

nocopy0
        xor a
        ld (TLACITKO),a                            ; reset stavu myšího tlačítka
        call INKEY                                 ; čekej na klávesu / událost
        cp 1
        jp z,infoend                               ; ESC/Break (dle mapování INKEY) → ukončit info dialog
        jp nocopy0                                 ; jinak stále čekej


; ------------------------------------------------------------
; move / copy
; Nastaví příznak operace:
;   ismove=1 → MOVE (přesun)
;   ismove=0 → COPY (kopírování)
; Poté pokračuje společným kódem contmov.
; ------------------------------------------------------------
move
        ld a,1
        ld (ismove),a                              ; režim: přesun
        jr contmov

CCCC                                             ; (label pro debug / alternativní vstupy)
copy
        xor a
        ld (ismove),a                              ; režim: kopírování

; ------------------------------------------------------------
; contmov
; Připraví PATHLEFT/PATHRIGHT (ořízne na C-string nulou místo 255 terminátoru),
; ověří, že operace dává smysl (specific_search),
; a rozhodne, zda se kopíruje 1 soubor nebo více vybraných.
; ------------------------------------------------------------
contmov
        ; PATHLEFT: najdi 255 terminátor a přepiš na 0 (nulový string)
        ld hl,PATHLEFT
        ld a,255
        ld bc,261
        cpir                                       ; hledej A (255) v rozsahu 261 bajtů, HL skončí ZA nalezeným
        ld (hl),0                                  ; udělej z toho 0-terminated

        ; PATHRIGHT: totéž
        ld hl,PATHRIGHT
        ld a,255
        ld bc,261
        cpir
        ld (hl),0

        ; porovnej PATHLEFT a PATHRIGHT (nebo jiná specifická kontrola cíl/zdroj)
        ld hl,PATHLEFT
        ld de,PATHRIGHT
        ld a,0
COPY    call specific_search
        jp z,no_copy_move                           ; pokud jsou stejné/neplatné → dialog "no_copy_move"

        ; zjisti počet vybraných souborů (numsel)
        ld hl,numsel
        call ROZHOD2                                 ; HL = ukazatel na numsel (pro aktivní okno)
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a                                       ; HL = 16bit hodnota (počet vybraných?)
        or h
        jp nz,morecopy                               ; pokud != 0 → kopíruje/přesouvá více souborů

        ; ----------------------------------------------------
        ; Jediný soubor: potvrzovací dialog "Copy/Move file?"
        ; ----------------------------------------------------
        call savescr
        ld hl,10 * 256 + 10
        ld bc,60 * 256 + 5
        ld a,16
        call window

        ld hl,11*256+11                              ; řádek titulku
        ld de,onecopytxt
        ld a,(ismove)
        or a
        jr z,$+4
        ld de,onemovetxt                              ; pokud ismove=1 → text pro přesun
        ld a,16
        call print

        ; získej index kurzoru v levém okně (POSKURZL)
        ld hl,POSKURZL
        call ROZHOD
        ld a,(hl)
        ld l,a
        ld h,0                                       ; HL = index položky

        ; zjisti adresu položky v seznamu STARTWINL + index
        push hl
        ld hl,STARTWINL
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a                                       ; HL = base pointer seznamu (start okna)
        ex de,hl                                     ; DE = base
        pop hl                                       ; HL = index
        add hl,de                                    ; HL = pointer na záznam položky
        push hl
        inc hl
        call find83                                   ; načti 8.3 záznam do TMP83 / apod.
        pop hl
        call FINDLFN                                  ; načti LFN do LFNNAME

        ; zkopíruj jméno do bfname pro zobrazení v dialogu
        ld hl,LFNNAME
        ld de,bfname
        ld bc,45
        ldir

        ; vypiš vybraný název souboru
        ld hl,22*256+11
        ld a,16
        ld de,bfname
        call print

        ; vykresli tlačítka YES / NO (texty)
        ld hl,60*256+15
        ld a,48
        ld de,yestxt
        call print

        ld hl,60*256+14
        ld a,16
        ld de,notxt
        call print

; ------------------------------------------------------------
; copywait
; Čeká na potvrzení:
;  - klávesa 1/ESC → copyend (zrušit)
;  - Enter        → copycont (pokračovat)
;  - myš: klik na buttonYes/buttonNo → continue/end
; ------------------------------------------------------------
copywait
        xor a
        ld (TLACITKO),a
        call INKEY
        cp 1
        jp z,copyend                                 ; zrušit
        cp 13
        jr z,copycont                                ; Enter = potvrdit

        ld a,(TLACITKO)
        bit 1,a
        jr z,copywait                                ; pokud není klik, čekej dál

        ; klik myší: ověř, jestli byl zásah do buttonYes / buttonNo
        ld hl,buttonYes
        call CONTROL_CLICK
        jr nc,copycont                               ; NC = zásah → potvrdit

        ld hl,buttonNo
        call CONTROL_CLICK
        jp nc,copyend                                ; NC = zásah → zrušit

        jr copywait                                  ; jinak klik mimo → čekej dál


; ------------------------------------------------------------
; copycont
; Provede samotné copy/move (single file):
;  - UI "please wait"
;  - načte 8.3 záznam a zjistí, zda je to adresář (bit7 ve TMP83+7)
;  - pokud adresář → nekopiruj_adresar
;  - jinak otestuje existenci cílového souboru (isfile)
;  - provede otevření, vytvoření, kopírování (readfile)
;  - pokud MOVE, pak po kopii smaže zdroj (DOS call $0124 nad TMP83)
;  - po COPY obnoví okna a refresh dir listing
; ------------------------------------------------------------
copycont
        ; malý UI symbol do atributové obrazovky (pravděpodobně "progress")
        ld   hl,$4000+160*14+23 + 1 + 62
        ld (hl),"|"

        ; přepiš NO na "please wait" a vymaž YES řádek
        ld hl,60*256+14
        ld a,16
        ld de,pleasewait
        call print

        ld hl,60*256+15
        ld a,16
        ld de,spaces
        call print

        ; znovu najdi vybraný soubor podle POSKURZL
        ld hl,POSKURZL
        call ROZHOD
        ld a,(hl)
        ld l,a
        ld h,0

        push hl
        ld hl,STARTWINL
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        ex de,hl
        pop hl
        add hl,de                                    ; HL = pointer na položku
        push hl
        inc hl
        call find83                                   ; vyplní TMP83 (8.3 entry + atributy)
        ;call BUFF83

        ; test "adresář?" – v TMP83+7 bit 7 (zřejmě atributy) nebo "stavové bity"
        ld hl,TMP83
        ld de,7
        add hl,de
        bit 7,(hl)
        pop hl
        jp nz,nekopiruj_adresar                       ; pokud je to adresář → nelze kopírovat tímto způsobem

        push hl
        ; otestuj, jestli cílový soubor už existuje (isfile může vyvolat overwrite dialog)
        ld de,norr                                    ; DE = adresa kam pokračovat pokud existuje (návratový hook)
        ld bc,nalezeno_isfile                          ; BC = kam skočit při shodě (v isfile je to self-modifying)
        call isfile
        pop hl

        ; vlastní kopie:
        push hl
        call openfile                                  ; otevři zdroj
        call createfile                                ; vytvoř cíl
        call readfile                                  ; přenes data
        ld b,0
        call closefile                                 ; zavři zdroj?
        ld b,2
        call closefile                                 ; zavři cíl?
norr    pop hl

        ; pokud MOVE, po kopii smaž zdroj (DOS unlink/delete)
        ld a,(ismove)
        or a
        jr z,nenimove
        inc hl                                         ; posun na správné místo záznamu?
        call find83                                    ; načti záznam pro delete
        ld b,11
        ld hl,TMP83
CCCAC20
        res 7,(hl)                                     ; vynuluj bit7 v každém bajtu jména (odstraní "stavové bity")
        inc hl
        djnz CCCAC20
        ld a,$ff
        ld (TMP83+11),a                                ; terminátor / marker pro DOS rutinu

        call dospage
        ld hl,TMP83
        call $0124                                     ; DOS rutina: delete/unlink položku podle 8.3 entry (pravděpodobně)
        call basicpage
        jp mmorekonec                                   ; pokračuj do konce multi-copy (sdílený konec)

; ------------------------------------------------------------
; nenimove
; Po COPY (ne MOVE): obnov okna + refresh adresář
; ------------------------------------------------------------
nenimove
        call obnov_okna

        ; refresh adresáře pro aktivní okno
        call dospage
        ld hl,pathl
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        xor a
        call $01b1                                      ; DOS: změna/načtení dir (dle implementace)
        call basicpage

        jp loop0


; ------------------------------------------------------------
; obnov_okna
; Kompletní refresh obou panelů (levý i pravý):
;  - smaže/vykreslí okna prázdně
;  - obnoví jeden panel, načte dir
;  - prohodí OKNO a udělá totéž pro druhý panel
; ------------------------------------------------------------
obnov_okna
        call prekresli_prazdne_okna
        call PROHOD
        call obnov_jedno_okno
        call GETDIR

        call PROHOD
        call obnov_jedno_okno
        call GETDIR

        ld a,32
        call writecur
        ret


; ------------------------------------------------------------
; prekresli_prazdne_okna
; Vykreslí dvě prázdná okna (levý panel a pravý panel) barvou A=0.
; ------------------------------------------------------------
prekresli_prazdne_okna
        ld hl,0 * 256 + 1
        ld bc,38 * 256 + 27
        ld a,0
        call window

        ld hl,40*256 + 1
        ld bc,38 * 256 + 27
        ld a,0
        call window
        ret


; ------------------------------------------------------------
; obnov_jedno_okno
; Resetuje proměnné okna (ALLFILES, POSKURZL), přepne do DOS stránky,
; načte adresář dle pathl, vykreslí okno a znovu načte obsah.
; ------------------------------------------------------------
obnov_jedno_okno
        ld hl,ALLFILES
        call ROZHOD2
        xor a
        ld (hl),a
        inc hl
        ld (hl),a                                      ; ALLFILES = 0 (žádné položky / reset)

        ld hl,POSKURZL
        call ROZHOD
        xor a
        ld (hl),a                                      ; kurzor na první položku

        call dospage
        ld hl,pathl
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        xor a
        call $01b1                                      ; DOS: reload dir pro pathl
        call basicpage

        ; vykreslení okna na pozici pozicel
        ld hl,pozicel
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a

        ld bc,38 * 256 + 27
        ld a,0
        call draw.window

        call reload_dir                                 ; interní refresh seznamu položek

        ; nastav root/adr a zobraz okno
        ld hl,adrl
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        ld (adrs+1),hl
        call getroot_reload
        call showwin
        ret


; ------------------------------------------------------------
; copyend
; Zrušení akce: vynuluje numsel (výběr), obnoví obrazovku a návrat do loop0.
; ------------------------------------------------------------
copyend
        ld hl,numsel
        call ROZHOD2
        xor a
        ld (hl),a
        inc hl
        ld (hl),a

        call loadscr
        jp loop0


; ------------------------------------------------------------
; nekopiruj_adresar
; Dialog: "You cannot copy the directory." + "Press any key"
; poté přesměruje do copyend.
; ------------------------------------------------------------
nodirtxt        defb "You cannot copy the directory.",0
pressanykeytxt  defb "Press any key to continue.",0

nekopiruj_adresar
        ld hl,10 * 256 + 10
        ld bc,60 * 256 + 5
        ld a,16
        call window

        ld hl,11*256+11
        ld a,16
        ld de,nodirtxt
        call print

        ld hl,11*256+15
        ld a,48
        ld de,pressanykeytxt
        call print

        xor a
        ld (TLACITKO),a
        call INKEY
        jp copyend


; ============================================================
; Více souborů: morecopy
; - zobrazí dialog "Copy files?" / "Move files?"
; - vypíše počet vybraných (numsel) a čeká na Enter/ESC
; - poté iteruje přes položky v ALLFILES, které mají nastavený příznak "selected"
; - kopíruje je a zobrazuje progress
; ============================================================
morecopytxt      defb "Copy     files?",0
moremovetxt      defb "Move     files?",0

moredeletetxt    defb "Delete     files?",0
runtxt           defb "Can you run this file?",0
unsuptxt         defb "Unsuported file!",0

nowcopy          defb "Copy     file from     files",0

actcount         defw 0

morecopy
        ld hl,0
        ld (actcount),hl                            ; počitadlo zkopírovaných
        call savescr

        ld hl,10 * 256 + 10
        ld bc,60 * 256 + 5
        ld a,16
        call window

        ld hl,11*256+11
        ld de,morecopytxt
        ld a,(ismove)
        or a
        jr z,$+4
        ld de,moremovetxt                            ; podle ismove zvol text
        ld a,16
        call print

        ; načti numsel (počet vybraných) a připrav progress bar (PROVYP/PROGRES)
        ld hl,numsel
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        push hl

        ex de,hl                                     ; DE = numsel (16bit)
        call PROVYP                                  ; nastav parametry progressu podle počtu
        add a,a
        ld   (CPPX1+1),hl
        ld   (CPPX3+1),a
        ld   hl,0
        ld   (PROGPROM),hl

        ; vypiš numsel (DECIMAL3) do dialogu
        ld hl,NUMBUF
        ld de,NUMBUF+1
        ld bc,5
        ld a,32
        ld (hl),a
        ldir
        ld hl,NUMBUF
        xor a
        ld (NUMBUF+3),a
        ld (numadr+1),hl
        pop hl
        call DECIMAL3

        ld hl,16*256+11
        ld a,16
        ld de,NUMBUF
        call print

        ; tlačítka YES/NO (bez myši, jen klávesy)
        ld hl,60*256+15
        ld a,48
        ld de,yestxt
        call print

        ld hl,60*256+14
        ld a,16
        ld de,notxt
        call print

acopywait
        xor a
        ld (TLACITKO),a
        call INKEY
        cp 1
        jp z,copyend                                  ; ESC → konec
        cp 13
        jr z,acopycont                                ; Enter → start
        jr acopywait


; ------------------------------------------------------------
; acopycont
; Vlastní cyklus multi-copy/move:
;  - ukáže "please wait"
;  - nastaví 2 progress značky
;  - iteruje přes ALLFILES a pro každou selected položku:
;     * pokud je to adresář → přeskočí (NODIR)
;     * pokud existuje v cíli → isfile (overwrite dialog)
;     * kopíruje open/create/read/close
;     * u MOVE smaže zdroj
;     * aktualizuje progress (PROGRES)
; ------------------------------------------------------------
acopycont
        ld hl,60*256+14
        ld a,16
        ld de,pleasewait
        call print

        ld hl,60*256+15
        ld a,16
        ld de,spaces
        call print

        ; progress znaky do atributové oblasti
        ld   hl,$4000+160*15+23 + 1 + 62
        ld (hl),"|"
        ld   hl,$4000+160*14+23 + 1 + 62
        ld (hl),"|"

        ld hl,0
MMM
moredalsi
        push hl
        ld (cislo_souboru+1),hl                       ; self-mod: index aktuální položky pro FINDLFN níže
        call find83                                    ; načti TMP83 pro položku index HL

        ld hl,TMP83
        bit 7,(hl)
        jp z,nekopirovat                               ; pokud není selected (nebo není platný flag) → přeskoč

        push hl

        ; status text: "Copy X file from Y files"
        ld hl,11*256+11
        ld a,16
        ld de,nowcopy
        call print

        ; actcount++
        ld hl,(actcount)
        inc hl
        ld (actcount),hl

        ; vypiš actcount
        push hl
        ld hl,NUMBUF
        ld de,NUMBUF+1
        ld bc,5
        ld a,32
        ld (hl),a
        ldir
        ld hl,NUMBUF
        xor a
        ld (NUMBUF+3),a
        ld (numadr+1),hl
        pop hl
        call DECIMAL3

        ld hl,16*256+11
        ld a,16
        ld de,NUMBUF
        call print

        ; vypiš numsel (celkem)
        ld hl,numsel
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        push hl
        ld hl,NUMBUF
        ld de,NUMBUF+1
        ld bc,5
        ld a,32
        ld (hl),a
        ldir
        ld hl,NUMBUF
        xor a
        ld (NUMBUF+3),a
        ld (numadr+1),hl
        pop hl
        call DECIMAL3

        ld hl,30*256+11
        ld a,16
        ld de,NUMBUF
        call print

        ; zruš označení (selected) pro aktuální položku
        pop hl
        res 7,(hl)

        ; test "je to adresář?" přes TMP83+7 bit7 (tvoje konvence)
        ld de,7
        add hl,de
        bit 7,(hl)
        jr nz,NODIR                                  ; pokud adresář → přeskoč
        pop hl

        ; připrav HL tak, aby ukazoval na entry (často -1 kvůli formátu tabulky)
        push hl
        dec hl
        push hl

        ; isfile: zjisti cílový LFN a ošetři overwrite
cislo_souboru
        ld hl,0
        dec hl
        call FINDLFN

        ld de,norr2
        ld bc,nalezeno_isfile
        call isfile

        pop hl
        push hl

        ; kopie souboru
        call openfile
        call createfile
        call readfile
        ld b,0
        call closefile
        ld b,2
        call closefile

norr2
        pop hl

        ; pokud MOVE, smaž zdroj stejně jako u single-copy
        ld a,(ismove)
        or a
        jp z,nenimove11
        inc hl
        call find83
        ld b,11
        ld hl,TMP83
CCCAC21
        res 7,(hl)
        inc hl
        djnz CCCAC21
        ld a,$ff
        ld (TMP83+11),a

        call dospage
        ld hl,TMP83
        call $0124
        call basicpage

nenimove11

; ---- progress bar update (PROGPROM vs CPPX1/CPPX3)
NODIR   ld   hl,(PROGPROM)
        inc  hl
        ld   (PROGPROM),hl
CPPX1    ld   de,0
        or   a
        sbc  hl,de
        jr   c,CPPNE1
CPPX3    ld   b,1
        ld   (PROGPROM),hl
        call PROGRES
CPPNE1

; ---- pokračuj na další položku
nekopirovat
        ld hl,ALLFILES
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld d,(hl)
        ld e,a                                      ; DE = počet položek? (ALLFILES)
        pop hl                                      ; HL = index aktuální položky

        ; test: jsme na konci?
        or a
        sbc hl,de
        add hl,de
        jr z,morekonec
        inc hl
        jp moredalsi

morekonec
        ; po dokončení:
        ld a,(ismove)
        or a
        jr z,nenimove2
        jp mmorekonec                                 ; MOVE konec (někde jinde)

nenimove2
        ; po COPY: obnov okna, reload dir, reset numsel, update freespace
        call obnov_okna

        call dospage
        ld hl,pathl
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        xor a
        call $01b1
        call basicpage

        ld hl,numsel
        call ROZHOD2
        xor a
        ld (hl),a
        inc hl
        ld (hl),a

        call freespace
        jp loop0


; ============================================================
; isfile
; Testuje, jestli soubor (LFNNAME) už existuje v cílovém okně.
; Pokud ano, vyvolá "nalezeno_isfile" (overwrite dialog) a podle volby:
;   - YES → pokračuje a vrátí se (Z = true pro skok přes iskam)
;   - NO  → přes self-mod isfilee skočí na adresu v DE (typicky norr/norr2)
; Pozn.: rutina přepíná OKNO (xor 16), aby hledala v druhém panelu.
; ============================================================
isfile
        ld (isfilee+1),de                            ; kam skočit, když uživatel odmítne overwrite (self-mod JP)
        ld (iskam+1),bc                              ; kam skočit při shodě (self-mod JP z iskam)

        ; ořízni LFNNAME: najdi poslední ne-space a dej 255 terminátor
        ld hl,LFNNAME+260
is0     ld a,(hl)
        dec hl
        cp 32
        jr z,is0
        inc hl
        inc hl
        ld (hl),255
        inc hl
        xor a
        ld (hl),a

        ; kopie LFNNAME → LFNNAME2 (buffer pro porovnání)
        ld hl,LFNNAME
        ld de,LFNNAME2
        ld bc,270
        ldir

        ; přepni aktivní okno (hledáme v cíli)
        ld a,(OKNO)
        xor 16
        ld (OKNO),a

        ; iteruj seznam ALLFILES v cílovém panelu
        ld hl,ALLFILES
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld b,(hl)
        ld c,a                                      ; BC = počet/ukazatel? (dle tvé struktury)

isfile0
        push bc
        ld h,b
        ld l,c
        push hl
        inc hl
        call find83
        pop hl
        dec hl
        call FINDLFN                                  ; LFNNAME naplněn názvem aktuální položky

        ; ořízni LFNNAME stejně (terminátor 255)
        ld hl,LFNNAME+260
is01    ld a,(hl)
        dec hl
        cp 32
        jr z,is01
        inc hl
        inc hl
        ld (hl),255
        inc hl
        xor a
        ld (hl),a

        ; porovnej LFNNAME vs LFNNAME2
        ld hl,LFNNAME
        ld de,LFNNAME2
        ld a,0
        call specific_search
        pop bc

; Adresa kam se skače když soubor je stejný
iskam   jp z,nalezeno_isfile                           ; shoda → overwrite dialog

        dec bc
        ld a,b
        or c
        jr nz,isfile0                                  ; další položka

        ; nebyla shoda → nastav A!=0 (aby Z nebylo) a pokračuj ven
        ld a,1
        or a
        jp cont_isfile0


; ============================================================
; nalezeno_isfile
; Dialog "file exists" + informace o obou souborech (datum, čas, size)
; a dotaz na overwrite. Výsledek:
;   - YES/Enter → cont_isfile (pokračuj kopírováním)
;   - NO/ESC    → norewrite (skoč přes isfilee na návrat)
; ============================================================
nalezeno_isfile
        call savescr
        ld hl,10 * 256 + 10
        ld bc,60 * 256 + 11
        ld a,16
        call window

        ; (zobraz datum/cas/size obou souborů - LFNNAME vs LFNNAME2)
        ld hl,11*256+17
        ld a,16
        ld de,origin
        call print

        ld hl,11*256+18
        ld a,16
        ld de,newfile
        call print

        ld de,(LFNNAME+261+4)
        ld hl,21*256+17
        call showdate
        ld hl,32*256+17
        ld de,(LFNNAME+261+6)
        call showtime

        ld de,(LFNNAME2+261+4)
        ld hl,21*256+18
        call showdate
        ld hl,32*256+18
        ld de,(LFNNAME2+261+6)
        call showtime

        ; size labels
        ld hl,42*256+17
        ld a,16
        ld de,sizetxt
        call print
        ld hl,42*256+18
        ld a,16
        ld de,sizetxt
        call print

        ; size hodnoty (DEC32)
        ld hl,49*256+17
        ld (dec32pos+1),hl
        ld hl,(LFNNAME+261)
        ld de,(LFNNAME+261+2)
        ld b,10
        ld a,16
        ld (decink+1),a
        call DEC32
        ld a,0
        ld (decink+1),a

        ld hl,60*256+17
        ld a,16
        ld de,bytestxt
        call print

        ld hl,49*256+18
        ld (dec32pos+1),hl
        ld hl,(LFNNAME2+261)
        ld de,(LFNNAME2+261+2)
        ld b,10
        ld a,16
        ld (decink+1),a
        call DEC32
        ld a,0
        ld (decink+1),a

        ld hl,60*256+18
        ld a,16
        ld de,bytestxt
        call print

        ; hlavní text
        ld hl,11*256+11
        ld a,16
        ld de,file_exists_txt
        call print

        ld hl,11*256+13
        ld a,16
        ld de,overwrite_txt
        call print

        ld hl,11*256+15
        ld a,16
        ld de,namefile
        call print

        ; připrav bfname = název existujícího souboru (LFNNAME2)
        ld hl,LFNNAME2
        ld de,bfname
        ld bc,35
        ldir

        ; najdi 255 terminátor a přepiš na space kvůli tisku
        ld hl,bfname
        ld bc,35
        ld a,255
        cpir
        dec hl
        ld (hl),32

TTT
        ; nulový terminátor pro tisk
        ld hl,bfname+37
        xor a
        ld (hl),a

        ; vypiš jméno
        ld hl,25*256+15
        ld a,16
        ld de,bfname
        call print

        ; YES/NO
        ld hl,60*256+21
        ld a,48
        ld de,yestxt
        call print

        ld hl,60*256+20
        ld a,16
        ld de,notxt
        call print

.wait
        xor a
        ld (TLACITKO),a
        call INKEY
        cp 1
        jp z,norewrite                                ; ESC → odmítnout overwrite
        cp 13
        jr z,cont_isfile                               ; Enter → souhlas overwrite
        jr .wait

cont_isfile
        call loadscr
cont_isfile0
        ; vrať LFNNAME2 → LFNNAME (zachovej nalezený soubor jako aktuální kontext)
        ld hl,LFNNAME2
        ld de,LFNNAME
        ld bc,270
        ldir

        ; přepni okno zpátky
        ld a,(OKNO)
        xor 16
        ld (OKNO),a
        ret

norewrite
        pop af                                         ; zahoď návratovou adresu CALL isfile (ukončíme vyšší úroveň toku)
        ld a,(OKNO)
        xor 16
        ld (OKNO),a                                    ; přepni okno zpátky

        call loadscr
isfilee  jp 0                                          ; self-mod: skok na adresu z DE (norr/norr2 apod.)
