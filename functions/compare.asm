;==============================================================================
;  Porovnání adresářů (ZX Spectrum Next / Z80N)
;
;  Funkce:
;    - Porovná obsah dvou panelů (levý vs pravý adresář).
;    - Pokud najde stejný soubor v obou panelech, „označí“ ho v obou seznamech
;      (typicky resetem bitu 7 u příznaku v záznamu 8.3 / BUFF83).
;
;  Princip (stručně):
;    1) Nejdřív se v obou panelech zruší staré označení (compare.deselect).
;    2) Pro každý soubor v prvním panelu se načte jeho LFN jméno (FINDLFN)
;       a uloží se do LFNNAME2.
;    3) Přepne se do druhého panelu a projdou se všechny soubory:
;         - načti LFN jméno (LFNNAME)
;         - porovnej LFNNAME vs LFNNAME2 přes specific_search
;         - když sedí, označ soubor v obou panelech (self-modifying HL konstanty)
;    4) Po skončení se obnoví obrazovka a oba panely se reloadnou a vykreslí.
;
;  Poznámky:
;    - Kód používá self-modifying instrukce:
;        ld (primary_window+1),hl  -> přepisuje operand "ld hl,0" v primary_window
;        ld (second_window+1),hl   -> přepisuje operand "ld hl,0" v second_window
;      aby rutiny primary_window/second_window věděly, který záznam označit.
;
;    - LFNNAME/LFNNAME2 jsou buffery pro dlouhá jména (LFN), vypadá to na délku
;      ~270 bajtů. Konec se tady „ručně“ ukončuje sentinel hodnotou 255 a nulou.
;
;==============================================================================


;------------------------------------------------------------------------------
; compare_dirs
;------------------------------------------------------------------------------
; Hlavní vstup: porovná adresáře v levém a pravém panelu a označí shody.
;
; Kroky:
;   - Uloží obrazovku, zruší předchozí označení v obou oknech.
;   - Vypíše malé okno "please wait".
;   - Projde soubory v aktivním okně (aktuální panel):
;       * načte 8.3 info (find83) a LFN (FINDLFN) do LFNNAME
;       * LFNNAME zkopíruje do LFNNAME2 (to je "hledaný vzor")
;       * přepne do druhého okna a zavolá other_window, která prohledá druhý panel
;         a když najde shodu, označí oba záznamy.
;   - Po skončení obnoví obrazovku a vynutí reload obou panelů.
;
compare_dirs
        call savescr                 ; uloží aktuální obrazovku (UI překryv)
        call compare.deselect        ; zruší označení v aktuálním panelu
        call prohod_okno             ; přepni panel (OKNO ^= 16)
        call compare.deselect        ; zruší označení v druhém panelu
        call prohod_okno             ; vrať se zpět do původního

        ; UI: malé okno + text "please wait"
        ld hl,10 * 256 + 10          ; (Y,X) horní levý roh okna
        ld bc,60 * 256 + 1           ; (šířka, výška) nebo (X2,Y2) dle tvého window()
        ld a,16                      ; atribut / barva
        call window

        ld hl,11*256+11              ; pozice textu v okně
        ld a,16                      ; atribut textu
        ld de,please_wait            ; string
        call print


        ; Získání ukazatele na seznam souborů v aktivním panelu:
        ; ALLFILES -> přes ROZHOD2 vybere správnou banku/offset dle OKNO
        ld hl,ALLFILES
        call ROZHOD2
        ld a,(hl)                    ; low byte pointeru
        inc hl
        ld h,(hl)                    ; high byte pointeru
        ld l,a                       ; HL = adresa "poslední záznam?" / iterátor

;--- smyčka: pro každý soubor v primárním panelu ------------------------------
comp0   push hl
        ld (primary_window+1),hl     ; SMC: uloží pozici záznamu pro pozdější označení
                                     ; (přepíše operand v "primary_window: ld hl,0")

        call find83                  ; načte/namapuje 8.3 záznam (interní buffer BUFF83)
        pop hl
        push hl
        dec hl                       ; posun na položku pro FINDLFN (typicky předchozí/metadata)
        call FINDLFN                 ; vyplní LFNNAME dlouhým jménem aktuálního souboru

        call prohod_okno             ; přepni na druhý panel

        ; Přesun LFNNAME -> LFNNAME2:
        ; LFNNAME2 teď drží jméno souboru z prvního panelu, které budeme hledat
        ; v druhém panelu.
        ld hl,LFNNAME
        ld de,LFNNAME2
        ld bc,270
        ldir

        call other_window            ; projde soubory v tomto (druhém) panelu a hledá shodu

        call prohod_okno             ; vrať se do původního panelu

        pop hl
        dec hl                       ; další položka (iterace směrem dolů)
        ld a,l
        or h
        jp nz,comp0                  ; dokud HL != 0, pokračuj

        call invert_body             ; UI: pravděpodobně zruší/obnoví invert v těle seznamu
        call loadscr                 ; obnoví původní obrazovku (zruší "please wait")

        ; Po porovnání se oba panely znovu načtou a vykreslí:
        ; (u tebe to vypadá, že getroot_reload bere adresu v self-modifying "adrs+1")
        ld hl,adrl
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        ld (adrs+1),hl
        call getroot_reload

        call showwin                 ; vykresli panel
        call prohod_okno
        call invert_body             ; UI efekt (možná zvýraznění aktivního okna)

        ; Druhý panel:
        ld hl,adrl
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        ld (adrs+1),hl
        call getroot_reload
        call showwin
        call prohod_okno

        ; UI finále:
        ld a,32
        call writecur                ; nastav kurzor (pravděpodobně "space" / vypnutí kurzoru)
        call zobraz_nadpis           ; obnov header
        jp loop0                     ; návrat do hlavní smyčky UI


;------------------------------------------------------------------------------
; prohod_okno
;------------------------------------------------------------------------------
; Přepne aktivní panel (OKNO xor 16).
;  - Hodnota 16 typicky značí "pravý panel" vs "levý panel" (bitové pole).
PROO
prohod_okno
        ld a,(OKNO)
        xor 16
        ld (OKNO),a
        ret


;------------------------------------------------------------------------------
; other_window
;------------------------------------------------------------------------------
; Projde všechny soubory v aktuálním (tedy „druhém“) panelu a porovná každý
; s LFNNAME2 (jméno z prvního panelu).
;
; Pokud porovnej_dva_lfn vrátí Z (shoda), skočí se do second_window, kde se:
;  - označí aktuální záznam v druhém panelu
;  - přepne panel a označí odpovídající záznam v prvním panelu (uložený přes SMC)
;
other_window
        ld hl,ALLFILES
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a                       ; HL = iterátor přes záznamy v aktuálním panelu

other0  push hl
        ld (second_window+1),hl      ; SMC: uloží pozici záznamu v druhém panelu
        call find83                  ; načti 8.3 (a nastav interní buffery)
        pop hl
        push hl
        dec hl
        call FINDLFN                 ; načti LFN do LFNNAME

        call porovnej_dva_lfn        ; porovná LFNNAME vs LFNNAME2 (Z = shoda)
        pop hl

        jr z,second_window           ; když shoda, označ oba záznamy

        dec hl                       ; další položka
        ld a,l
        or h
        jp nz,other0                 ; dokud HL != 0
        ret


;------------------------------------------------------------------------------
; second_window / primary_window
;------------------------------------------------------------------------------
; Rutiny, které označí soubor na pozici uložené přes self-modifying operand.
;
; Pozn.: "oznac_soubor_dle_pozice_v_hl" očekává HL = pozice záznamu v seznamu.
;        Tady se HL nastavuje tím, že se přepíše operand instrukce "ld hl,0".
;
second_window
        ld hl,0                      ; SMC operand = pozice záznamu v druhém panelu
        call oznac_soubor_dle_pozice_v_hl
        call prohod_okno             ; přepni na první panel

primary_window
        ld hl,0                      ; SMC operand = pozice záznamu v prvním panelu
        call oznac_soubor_dle_pozice_v_hl
        call prohod_okno             ; vrať se do druhého panelu (obnov původní stav)
        ret


;------------------------------------------------------------------------------
; porovnej_dva_lfn
;------------------------------------------------------------------------------
; Připraví oba LFN buffery na porovnání a zavolá specific_search.
;
; Co přesně dělá příprava:
;   - Jde od konce (offset +260) směrem dozadu a přeskočí trailing mezery (0x20).
;   - Za poslední "ne-mezeru" vloží hodnotu 255 (sentinel/terminátor).
;   - Za to vloží 0 (pravděpodobně extra ukončení / bezpečnost pro vyhledávání).
;
; Pak zavolá:
;   specific_search(LFNNAME, LFNNAME2, A=0)
;   a vrací příznaky (Z = shoda) dál volajícímu.
;
porovnej_dva_lfn
        ;--- uprav konec LFNNAME ------------------------------------------------
        ld hl,LFNNAME+260
por0    ld a,(hl)
        dec hl
        cp 32
        jr z,por0                    ; přeskakuj mezery na konci
        inc hl                       ; vrať se na poslední znak (nebo o 1 zpět dle struktury)
        inc hl
        ld (hl),255                  ; sentinel za poslední znak
        inc hl
        xor a
        ld (hl),a                    ; ještě 0 za sentinel (dvojité ukončení)

        ;--- uprav konec LFNNAME2 -----------------------------------------------
        ld hl,LFNNAME2+260
por1    ld a,(hl)
        dec hl
        cp 32
        jr z,por1
        inc hl
        inc hl
        ld (hl),255
        inc hl
        xor a
        ld (hl),a

        ;--- porovnání přes vyhledávání -----------------------------------------
        ld hl,LFNNAME
        ld de,LFNNAME2
        ld a,0
        call specific_search          ; očekává se: Z=1 pokud match (nebo dle implementace)
        ret



;==============================================================================
;  Modul compare
;==============================================================================
        module compare

;------------------------------------------------------------------------------
; compare.deselect
;------------------------------------------------------------------------------
; Zruší označení (výběr/shody) u všech souborů v aktuálním panelu.
;
; Jak to funguje:
;   - Projde všechny položky v ALLFILES (přes ROZHOD2 dle OKNO).
;   - Pro každou položku:
;       * načte 8.3 záznam (BUFF83 + find83)
;       * načte LFN (FINDLFN) do LFNNAME
;       * připraví dva řetězce pro search:
;           - v RAM na adrese 23296 je zřejmě pracovní buffer pro "hledaný text"
;           - do (23296) se patrně BUFF83/find83 uloží i 8.3 jméno, nebo se to
;             používá jako "search key"
;           - LFNNAME je druhý řetězec
;         Oba řetězce se ukončí sentinel 255 za posledním znakem (ořez trailing spaces).
;       * search(de=23296, hl=LFNNAME) -> nastaví (foundfile) pokud našel.
;       * Pokud našel (NZ=0?), vezme (foundfile), zavolá BUFF83 a udělá res 7,(hl)
;         => odstraní "označeno" (bit 7 flagu).
;       * Následně nulováním dvou bajtů někde u numsel (asi počitadlo vybraných)
;         resetuje počty výběrů pro panel.
deselect
        ld a,255
        ld (23296),a                 ; inicializace/terminátor pracovního bufferu

        ld hl,ALLFILES
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a                       ; HL = iterátor přes položky v aktuálním panelu

;--- smyčka přes všechny soubory ----------------------------------------------
dfind0   push hl
        call BUFF83                  ; připrav/namapuj 8.3 buffer pro aktuální položku
        call find83                  ; načti 8.3 data
        pop hl
        push hl
        dec hl
        call FINDLFN                 ; načti LFN do LFNNAME

        ;--- ukonči pracovní buffer 23296 sentinel 255 za posledním znakem -------
        ld hl,23296 + 59             ; "najdi poslední znak" v pracovním bufferu
dfind1
        dec hl
        ld a,(hl)
        cp 32
        jr z,dfind1                  ; přeskakuj trailing mezery
        inc hl
        inc hl
        ld a,255
        ld (hl),a                    ; sentinel za poslední znak

        ;--- ukonči LFNNAME sentinelem 255 ---------------------------------------
        ld hl,LFNNAME + 261          ; "najdi poslední znak" v LFNNAME
dfind2
        dec hl
        ld a,(hl)
        cp 32
        jr z,dfind2                  ; přeskakuj trailing mezery
        inc hl
        ld a,255
        ld (hl),a                    ; sentinel

        ;--- vyhledej / spáruj a případně zruš flag --------------------------------
        ld de,23296
        ld hl,LFNNAME
        call search                  ; výsledek: NZ => neshoda / nenalezeno (dle použití níže)

        jr nz,dnesouhlasi            ; pokud nenalezeno, nic neruš

        ; Nalezeno: v (foundfile) je pozice záznamu -> zruš označení (bit 7)
        ld hl,(foundfile)
        call BUFF83
        res 7,(hl)                   ; zruš příznak výběru/shody u souboru

        ; Reset počitadla vybraných (numsel) v aktuálním panelu:
        ld hl,numsel
        call ROZHOD2                 ; vybere správnou instanci pro panel
        ld (dadresasel+1),hl         ; SMC: kam zapisujeme nulu
        ld a,(hl)                    ; vyzvedni pointer/adr (2B)
        inc hl
        ld h,(hl)
        ld l,a
        inc hl                       ; hl = ? (pravděpodobně ukazatel na 2 bajty počitadla)
        ld a,l
        ld (dzvys+1),a               ; SMC: nízký bajt adresy pro zápis
        ld a,h
        ld (dzvys2+1),a              ; SMC: vysoký bajt adresy pro zápis

dadresasel   ld hl,0
dzvys        ld (hl),0               ; vynuluj low byte počitadla
        inc hl
dzvys2       ld (hl),0               ; vynuluj high byte počitadla

dnesouhlasi
        pop hl
        dec hl
        ld a,l
        or h
        jp nz,dfind0                 ; další položka

dfind_end
        ret

        endmodule
