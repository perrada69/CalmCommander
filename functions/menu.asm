
; -----------------------------------------------------------------------------
; Flag: informace, zda je zobrazené menu
;   0 ... menu není zobrazené
;   1 ... menu je zobrazené
; -----------------------------------------------------------------------------
zobrazeneMenu
        defb 0


; -----------------------------------------------------------------------------
; menu
; Vstup do menu režimu:
;   - nastaví zobrazeneMenu=1
;   - vypíše text (pravděpodobně status / debug) přes PRINT
;   - uloží aktuální screen (savescr) pro pozdější návrat (loadscr)
;   - vykreslí menu (show_menu)
;   - přejde do hlavní smyčky obsluhy vstupu (menu01)
; -----------------------------------------------------------------------------
menu
        ld a,1
        ld (zobrazeneMenu),a

        ld hl,0*256+0           ; HL = pozice tisku (X=0,Y=0) 
        ld de,text              ; DE = ukazatel na text
        ld a,16                 
        call print

        call gettime            
        call savescr            

        call show_menu          ; vykresli menu (položky, rámeček, kurzor)

; -----------------------------------------------------------------------------
; menu01
; Hlavní smyčka menu:
;   - vyčistí stav tlačítka myši (TLACITKO)
;   - přečte klávesu (INKEY) => návrat v A
;   - zpracuje ESC/Exit, šipky, Enter
;   - pokud není klávesa, kontroluje kliknutí levým tlačítkem myši
; -----------------------------------------------------------------------------
menu01
        xor a
        ld (TLACITKO),a         ; vynuluj stav kliknutí (před novým čtením)
        call INKEY              ; A = kód klávesy (1=ESC?, 10/11=down/up, 8/9=left/right, 13=enter)

        cp 1
        jp z,menu_exit          ; ESC / Exit

        cp 10
        jp z,menudown
        cp 11
        jp z,menuup
        cp 9
        jp z,menu_right
        cp 8
        jp z,menu_left
        cp 13
        jp z,menuenter          ; Enter -> akce položky

        ; žádná "známá" klávesa => zkus myš
        ld a,(TLACITKO)
        bit 1,a                 ; test na levé tlačítko (bit0=pravé)
        jp nz,tlacitko_menu      ; pokud levé zmáčknuto, řeš klik v menu
        jr menu01                ; jinak čekej dál


zjistiJestliMyskaNeniVHorniCastiMenu
        ld a,(zobrazeneMenu)
        or a
        ret z                   ; pokud menu není zobrazené, nic nedělej
	call GetHotXRangeInTopBands
	cp 255
	ret z		;pokud nejsi, vrat se
	ld b,a
	ld a,(nummenu)
	cp b
	ret z

	ld a,b
	ld (nummenu),a

        call loadscr
        call gettime
        call savescr

        ld a,1
        ld (curmeny+1),a         

        call show_menu
        ld a,64
        call writecurmenu        ; vykresli kurzor (např. šipku) v novém menu

        xor a
        ld (menucur),a           ; index položky = 0	
        ret

; -----------------------------------------------------------------------------
; menuenter_pred
; Pomocná větev pro kliknutí:
;   - nejdřív obnoví screen pod menu (loadscr)
;   - pak skočí do menuenter (provedení akce)
; -----------------------------------------------------------------------------
menuenter_pred
        call loadscr
        jp menuenter


; -----------------------------------------------------------------------------
; tlacitko_menu
; Obsluha kliknutí myší v menu:
;   - ZjistiJestliJsemVMenu nastaví/počítá bounding box aktivního menu
;     a volá CONTROL => návrat Carry=0 (NC) = trefil oblast
;   - pokud trefil, jde na menuenter_pred (nejdřív loadscr, pak akce)
; -----------------------------------------------------------------------------
tlacitko_menu
        call ZjistiJestliJsemVMenu
        jp nc,menuenter_pred     ; NC = trefil oblast -> proveď akci položky

        ;xor a
        ;ld (prvniYsouradniceMenu),a   ; nastav 1. variantu Y offsetu = 0
        ;call ZjistiJestliJsemVMenu

        ld a,8
        ld (prvniYsouradniceMenu),a   
        jp nc,menu01            
           
; -----------------------------------------------------------------------------
; menu_exit
; Zavření menu:
;   - obnoví screen (loadscr)
;   - nastaví zobrazeneMenu=0
;   - vykreslí nadpis do VRAM od #4000 (znaky + atribut 16 střídavě)
;   - návrat do hlavní smyčky programu (loop0)
; -----------------------------------------------------------------------------
menu_exit
        call loadscr
        xor a
        ld (zobrazeneMenu),a

        ld hl,nadpis
        ld de,#4000
        ld bc,80                 ; počet znaků nadpisu (80 bajtů)

menu001
        ld a,(hl)                ; znak
        ld (de),a
        inc de
        ld a,16                  ; atribut (barva)
        ld (de),a
        inc de
        inc hl
        dec bc
        ld a,c
        or b
        jr nz,menu001

        jp loop0                 ; pokračuj v hlavním programu


; -----------------------------------------------------------------------------
; zobraz_nadpis
; Samostatná rutina pro vykreslení nadpisu (stejná logika jako v menu_exit),
; jen se na konci vrací RET.
; -----------------------------------------------------------------------------
zobraz_nadpis
        ld hl,nadpis
        ld de,#4000
        ld bc,80

nadpis111
        ld a,(hl)
        ld (de),a
        inc de
        ld a,16
        ld (de),a
        inc de
        inc hl
        dec bc
        ld a,c
        or b
        jr nz,nadpis111
        ret


; -----------------------------------------------------------------------------
; menu_right
; Přepnutí na další "hlavní" menu (doprava):
;   - pokud už jsme na posledním (nummenu==4), nic nedělej
;   - jinak nummenu++
;   - obnov screen, uloží nový podklad a znovu vykresli menu
;   - nastav kurzor menu (curmeny=1) + reset vybrané položky (menucur=0)
; -----------------------------------------------------------------------------
menu_right
        ld a,(nummenu)
        cp 4
        jp z,menu01              ; už poslední menu -> nic

        inc a
        ld (nummenu),a

        call loadscr
        call gettime
        call savescr

        ld a,1
        ld (curmeny+1),a         ; self-mod? nebo proměnná s číslem řádku kurzoru

        call show_menu
        ld a,64
        call writecurmenu        

        xor a
        ld (menucur),a           ; index položky = 0

        jp menu01


; -----------------------------------------------------------------------------
; menu_left
; Přepnutí na předchozí "hlavní" menu (doleva):
;   - pokud jsme na prvním (nummenu==0), nic nedělej
;   - jinak nummenu--
;   - stejné kroky jako menu_right
; -----------------------------------------------------------------------------
menu_left
        ld a,(nummenu)
        or a
        jp z,menu01              ; už první menu

        dec a
        ld (nummenu),a

        call loadscr
        call gettime
        call savescr

        ld a,1
        ld (curmeny+1),a

        call show_menu
        ld a,64
        call writecurmenu

        xor a
        ld (menucur),a

        jp menu01


; -----------------------------------------------------------------------------
; menudown
; Pohyb kurzoru dolů v aktuálním menu:
;   - pokud je kurzor už na posledním řádku (curmeny == pocet_radku), nic
;   - jinak:
;       * smaž starý kurzor (writecurmenu s A=48)
;       * curmeny++
;       * vykresli nový kurzor (A=64)
;       * menucur++ (interní index vybrané položky)
; -----------------------------------------------------------------------------
menudown
        ld hl,curmeny+1
        ld a,(hl)
        ld hl,pocet_radku+1
        cp (hl)
        jp z,menu01              ; už dole -> nic

        ld a,48
        call writecurmenu        ; smaž starý kurzor / přepiš mezerou?

        ld hl,curmeny+1
        ld a,(hl)
        inc a
        ld (curmeny+1),a

        ld a,64
        call writecurmenu        ; vykresli nový kurzor

        ld hl,menucur
        inc (hl)                 ; posuň index položky

        jp menu01


; -----------------------------------------------------------------------------
; menuup
; Pohyb kurzoru nahoru v aktuálním menu:
;   - pokud je kurzor na prvním řádku (curmeny==1), nic
;   - jinak analogicky jako menudown:
;       * smaž starý kurzor
;       * curmeny--
;       * vykresli nový kurzor
;       * menucur--
; -----------------------------------------------------------------------------
menuup
        ld hl,curmeny+1
        ld a,(hl)
        cp 1
        jp z,menu01              ; už nahoře -> nic

        ld a,48
        call writecurmenu        ; smaž starý kurzor

        ld hl,curmeny+1
        ld a,(hl)
        dec a
        ld (curmeny+1),a

        ld a,64
        call writecurmenu        ; vykresli nový kurzor

        ld hl,menucur
        dec (hl)                 ; posuň index položky

        jp menu01


; -----------------------------------------------------------------------------
; ZjistiJestliJsemVMenu
;
; Rutina zjistí parametry aktuálně aktivního menu a připraví oblast,
; ve které se menu nachází, pro další zpracování (např. detekci kurzoru).
;
; Postup:
;   1) Podle indexu menu (nummenu) zjistí počet položek menu
;      z tabulky menulenght.
;   2) Z tabulky menupos načte X souřadnici menu.
;   3) Spočítá horizontální rozsah menu (levý a pravý okraj).
;   4) Podle počtu položek spočítá vertikální rozsah menu
;      (výška menu = počet položek × 8).
;   5) Připraví hodnoty pro rutinu CONTROL, která následně testuje,
;      zda se kurzor nachází v oblasti menu.
;
; Výstupy ukládá do:
;   xovaSouradniceMenu  - levý okraj menu
;   konecXoveSouradnice - pravý okraj menu
;   rohAktivnihoMenu    - spodní hranice / výška menu
;
; Používá self-modifying kód pro předání počtu položek.
;
; Volá:
;   CONTROL
;
; Registry: A, DE, HL modifikovány.
; -----------------------------------------------------------------------------
ZjistiJestliJsemVMenu
        ld a,(nummenu)          ; A = číslo aktuálního menu (index)
        ld e,a                  ; E = index menu
        ld d,0                  ; DE = index (16-bit), D=0

        ld hl,menulenght        ; HL = base adresa tabulky délek menu
        add hl,de               ; HL = menulenght + index
        ld a,(hl)               ; A = menulenght[index]  (pravděpodobně "počet položek - 1" nebo podobně)
        inc a                   ; A = počet položek aktivního menu (dorovnání +1)

        ld (pocetPolozekMenu + 1),a ; self-modifying: přepíše operand u "ld e,0" níže na aktuální počet položek

        ld hl,menupos           ; HL = base tabulky X pozic menu
        add hl,de               ; HL = menupos + index
        ld a,(hl)               ; A = menupos[index] = X souřadnice menu (asi v pixelech nebo sloupcích)
        or a                    ; test A==0 ?
        jr z,nultaPolozka       ; pokud X=0, neskákej do posunů (nech 0)

        dec a                   ; jinak X -= 2  (posun vlevo)
        dec a

nultaPolozka                    ; zde se definuje obdélník aktivního menu (X rozsah)
        ld (xovaSouradniceMenu),a   ; uloží levý okraj menu

        ld b,36                 ; šířka menu (nebo delta do pravého okraje) = 36
        add a,b                 ; A = X + 36
        ld (konecXoveSouradnice),a  ; uloží pravý okraj menu

pocetPolozekMenu
        ld e,0                  ; !!! operand "0" je před chvílí přepsán -> E = počet položek
        ld d,8                  ; D = 8 (výška jedné položky v pixelech / řádcích)
        mul d,e                 ; DE = D*E (na klasickém Z80 není MUL, tohle je Z80N / makro / assembler rozšíření)
                                ; výsledkem chceš mít v E celkovou výšku menu (8 * počet položek)

        ld a,e                  ; A = výška (v 8px krocích) v E
        dec a                   ; A -= 2 (zase korekce: "roh" obdélníku trochu nahoru)
        dec a
        ld (rohAktivnihoMenu),a ; uloží "roh" (pravděpodobně Y rozsah / výšku / spodní okraj)

        ld hl,xovaSouradniceMenu ; HL ukazuje na strukturu/parametry obdélníku (minimálně X start)
        call CONTROL            

        ret


; -----------------------------------------------------------------------------
; GetHotXRangeInTopBands
;
; Test myši podle COORD (X,Y):
;  - Y pásma: 0..8  OR  prvniYsouradniceMenu..prvniYsouradniceMenu+8
;  - X pásma: 0..15, 16..30, 31..45, 46..55, 56..70
;
; Výstup:
;   NC + A=0..4  = trefil, A je index pásma
;   C  + A=255   = netrefil
;
; Mění: A, B, E
; -----------------------------------------------------------------------------
GetHotXRangeInTopBands

        ; ---------
        ; Y test #1: 0..8
        ; ---------
        ld a,(COORD+1)          ; A = Y
        cp 9                    ; Y < 9  <=> Y <= 8
        jr nc,.NO_HIT
.Y_OK
        ; ---------
        ; X pásma -> A = 0..4
        ; ---------
        ld a,(COORD+0)          ; A = X

        cp 16                   ; 0..15
        jr c,.RET0

        cp 31                   ; 16..30
        jr c,.RET1

        cp 46                   ; 31..45
        jr c,.RET2

        cp 56                   ; 46..55
        jr c,.RET3

        cp 71                   ; 56..70
        jr c,.RET4

.NO_HIT
        ld a,255
        scf                     ; C=1 netrefil
        ret

.RET0    xor a                  ; A=0
         or a                   ; C=0
         ret
.RET1    ld a,1
         or a
         ret
.RET2    ld a,2
         or a
         ret
.RET3    ld a,3
         or a
         ret
.RET4    ld a,4
         or a
         ret


; =============================================================================
; podbarviPodlePoziceMysky
; "Hover" logika: pokud je menu zobrazené a myš je uvnitř jeho oblasti,
; tak se podle Y souřadnice myši přepne vybraná položka (kurzor) a podbarví se.
;
; Používá:
;   - zobrazeneMenu: 0/1 (menu aktivní)
;   - ZjistiJestliJsemVMenu: připraví bounding box + volá CONTROL
;     Výstup: C = mimo menu, NC = uvnitř menu
;   - COORD+1: Y souřadnice myši
;   - deleno8: vydělí Y / 8 (výsledek v D) => řádek menu
;   - writecurmenu: vykreslení kurzoru/podbarvení (A určuje "barvu"/znak)
;
; Efekt:
;   - smaže staré zvýraznění (A=48)
;   - nastaví curmeny a menucur podle myši
;   - vykreslí nové zvýraznění (A=64)
; =============================================================================
podbarviPodlePoziceMysky

        ld a,(zobrazeneMenu)
        or a
        ret z                   ; pokud menu není zobrazené, nic nedělej

        call ZjistiJestliJsemVMenu
        ret c                   ; Carry=1 => myš není v menu oblasti

JsemVMenu
        ; --- výpočet položky v menu podle Y souřadnice myši ---
        ld a,(COORD+1)          ; A = Y souřadnice myši
        ld d,a                  ; D = Y (vstup pro deleno8)
        ld e,8                  ; dělitel = 8 (výška 1 položky)
        call deleno8            ; D = Y/8 (výsledný řádek)

        push de                 ; uchovej D/E (D=řádek), protože writecurmenu si je může změnit

        ; smaž staré zvýraznění / kurzor
        ld a,48
        call writecurmenu

        pop de                  ; vrať D = řádek

        ; nastav nový řádek kurzoru (pozor: curmeny+1 je SMC operand)
        ld a,d
        ld (curmeny+1),a        ; řádek kurzoru = Y/8

        ; menucur je index položky od 0, zatímco curmeny je typicky od 1
        dec a
        ld (menucur),a

        ; vykresli nové zvýraznění / kurzor
        ld a,64
        call writecurmenu

        ret


; =============================================================================
; Proměnné / parametry pro bounding box menu (čte CONTROL / ZjistiJestliJsemVMenu)
; Pozn.: názvy naznačují:
;   xovaSouradniceMenu     = levý okraj
;   prvniYsouradniceMenu   = horní okraj (offset, default 8)
;   konecXoveSouradnice    = pravý okraj
;   rohAktivnihoMenu       = spodní hrana / výška / "roh" (dle tvé logiky)
; =============================================================================
XOVA
xovaSouradniceMenu
        defb 0
prvniYsouradniceMenu
        defb 8
konecXoveSouradnice
        defb 0
rohAktivnihoMenu
        defb 0


; =============================================================================
; show_menu
; Vykreslí aktuální menu (podle nummenu):
;   1) Z tabulky menulenght uloží počet řádků do SMC (pocet_radku+1)
;   2) Z tabulky menupos uloží X pozici do SMC (menux+1), s korekcí -2
;   3) Vybere správný blok menu textů v menuitems a uloží jeho adresu
;      do SMC (adresa_polozky+1)
;   4) Předkreslí "pruh" kurzoru / pozadí (s A=48)
;   5) Smyčkou volá print pro jednotlivé položky (dokud nenarazí na 255)
;   6) Nakonec vykreslí aktivní kurzor (A=64)
;
; Důležité: je tu hodně self-modifying míst:
;   - pocet_radku
;   - menux
;   - adresa_polozky
;   - ymenu/xmenu (parametry pro print)
; =============================================================================
show_menu
SAS
        ; --- zjisti délku menu (počet řádků) ---
        ld a,(nummenu)
        ld e,a
        ld d,0
        ld hl,menulenght
        add hl,de
        ld a,(hl)
        ld (pocet_radku+1),a    ; SMC: operand u "ld b,0"

        ; --- zjisti X pozici menu ---
        ld hl,menupos
        add hl,de
        ld a,(hl)
        or a
        jr z,sm1
        dec a
        dec a                   ; korekce X o -2
sm1     ld (menux+1),a          ; SMC: operand u "ld hl,$4000+2"

        ; --- výpočet offsetu do tabulky menuitems: 2 * nummenu ---
        ld b,e
        xor a
sm0     add a,2
        djnz sm0

        ; --- HL = ukazatel na adresu definice konkrétního menu ---
        ld e,a
        ld d,0
        ld hl,menuitems
        add hl,de               ; HL = &menuitems[2*nummenu]
        ld a,(hl)               ; low byte adresy menu definice
        inc hl
        ld h,(hl)               ; high byte
        ld l,a                  ; HL = adresa definice menu (texty + pointery)
        ld (adresa_polozky+1),hl ; SMC: operand u "ld de,0" (start menu dat)

        ; --- připrav základní adresu v obrazovce pro kreslení menu lišty ---
menux    ld hl,$4000+2          ; SMC: HL = VRAM start + X offset
        ld (curmen+1),hl        ; ulož base pro writecurmenu
        inc hl

        ; Předkreslení "pruhu" (A=48) na několika pozicích
        ld a,48
        ld (hl),a

        inc hl : inc hl
        ld (hl),a
        inc hl : inc hl
        ld (hl),a
        inc hl : inc hl
        ld (hl),a

        inc hl : inc hl
        ld (hl),a

        inc hl : inc hl
        ld (hl),a

        inc hl : inc hl
        ld (hl),a

        ld h,a
        ld l,1

        ld a,16                 ; atribut/barva pro PRINT?

adresa_polozky
        ld de,0                 ; SMC: DE = adresa menu definice (text/pointer)

        ; startovní řádek menu = 1
        ld a,1
        ld (ymenu+1),a

        ; xmenu je v "atributových" jednotkách => menux / 2
        ld a,(menux+1)
        rra                     ; divide by 2 (atributy)
        ld (xmenu+1),a

pocet_radku
        ld b,0                  ; SMC: B = počet položek (řádků)

ymenu    ld l,1                 ; SMC: L = y
xmenu    ld h,0                 ; SMC: H = x

        ; vykresli položku (PRINT typicky bere: HL=(x,y), DE=text, A=atribut)
        ld a,48                
        call print

        ld a,(de)
        cp 255
        ret z                   ; 255 = konec menu definice

        inc de
        inc de
        inc de

        ; y++ (další řádek)
        ld hl,ymenu+1
        inc (hl)

        djnz ymenu             

        ; po vykreslení menu nastav první zvýraznění/kurzor
        ld a,64
        call writecurmenu

        ret
SASS


; =============================================================================
; writecurmenu
; Vykreslí (nebo smaže) kurzor/podbarvení řádku menu.
;
; Vstup:
;   A = "barva" / znak / atribut, který se zapisuje (typicky 64=aktivní, 48=mazání)
;   curmen  (SMC) = base adresa v obrazovce (#4000 + X offset)
;   curmeny (SMC operand) = řádek (E) v menu, 1..N
;
; Princip:
;   HL = curmen + (curmeny * 160) + 1
;   a pak se 18× v rozestupu 2 bajty zapisuje hodnota A (atributový pruh)
; =============================================================================
writecurmenu
        ld (curmencolor+1),a    ; SMC: operand u "ld a,16"

curmen   ld hl,0                ; SMC: base adresa menu v obrazovce
curmeny  ld e,1                 ; SMC: E = řádek kurzoru
        ld d,160
        mul d,e                 ; DE = 160 * E
        add hl,de               ; HL = base + rowOffset
        inc hl                 

curmencolor
        ld a,16                 ; SMC: sem se přepíše A (48/64)
        ld (hl),a               ; zapiš první bod pruhu
        ld b,18
curmen0
        inc hl : inc hl         ; přeskoč na další atribut (každé 2 bajty)
        ld (hl),a
        djnz curmen0
        ret


; =============================================================================
; menuenter / MENUENTER
; Provede akci vybrané položky menu:
;   1) Najde adresu definice menu (menuitems + 2*nummenu)
;   2) Najde v něm položku podle menucur:
;        položka = menucur+1 (proto inc a)
;        offset = 22 * položka (proto d=22) - pak -2 (dec de, dec de)
;      (Tj. očekávám fixní délku záznamu položky v menu definici!)
;   3) Z definice položky načte handler adresu (defw) a skočí na ni.
;   4) Před skokem schová menu:
;        - zobrazeneMenu=0
;        - loadscr (obnoví původní screen)
; =============================================================================
MENUENTER
menuenter
        ; --- vyber správnou definici menu podle nummenu ---
        ld a,(nummenu)
        ld e,a
        ld d,2
        mul d,e                 ; DE = 2 * nummenu
        ld hl,menuitems
        add hl,de               ; HL = &menuitems[2*nummenu]
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a                  ; HL = adresa menu definice

        push hl                 ; uložíme si base adresu menu definice

        ; --- spočítej offset na vybranou položku ---
        ld a,(menucur)
        inc a                   ; položky asi čísluješ od 1
        ld e,a
        ld d,22
        mul d,e                 ; DE = 22 * (menucur+1)
        dec de
        dec de                  ; korekce -2 (aby to sedlo na defw handler)

        pop hl                  ; HL = base menu definice
        add hl,de               ; HL = ukazatel na handler adresu v definici položky
        push hl

        ; --- zavři menu + obnov screen ---
        xor a
        ld (zobrazeneMenu),a
        call loadscr

        ; --- načti handler adresu a skoč na něj ---
        pop hl                  ; HL ukazuje na defw handler (low, high)
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a                  ; HL = handler
        jp (hl)


; =============================================================================
; Data: titulky, tabulky a definice menu
; =============================================================================
text        defb " LEFT  | FILE  | UTILS | RIGHT | QUIT                ",0
nadpis      defb " Calm Commander " : VERSION : defb " (Development version)                                                   ",0

menupos     defb 0, 18, 34, 50, 66

nummenu     defb 0              ; index hlavního menu (0..4)

; tabulka pointerů na definice jednotlivých menu
menuitems   defw menuleft, menufile, menuutil, menuright, menuquit

; počet položek v každém menu (pozor: používám to jako počet řádků pro smyčku)
menulenght  defb 4, 6, 3, 4, 2

; index vybrané položky v aktuálním menu (0-based)
menucur     defb 0


; =============================================================================
; Formát definice menu:
;   defb "TEXT...",0
;   defw handler
;   ...
;   defb 255   ; terminátor menu
;
; Pozn.: V menuenter se počítá s FIXNÍ velikostí záznamu 22 bajtů na položku.
; To je důležité: texty musí mít přesně danou délku/padding, jinak se offsety rozjedou.
; =============================================================================

menuleft
        defb " SELECT FILES   (+)",0
        defw select_files_left
        defb " DESEL. FILES   (-)",0
        defw deselect_files_left
        defb " INVERT SELECT  (*)",0
        defw invert_select_files_left
        defb " CHANGE DRIVE(CS+1)",0
        defw newdisc_left
        defb 255

menufile
        defb " COPY           (5)",0
        defw copy
        defb " MOVE           (6)",0
        defw move
        defb " DELETE         (8)",0
        defw delete
        defb " RENAME         (9)",0
        defw RENAME
        defb " FILE INFO      (I)",0
        defw info_file
        defb " CHANGE ATTR.   (C)",0
        defw CHNG_ATTR
        defb 255

menuright
        defb " SELECT FILES   (+)",0
        defw select_files_right
        defb " DESEL. FILES   (-)",0
        defw deselect_files_right
        defb " INVERT SELECT  (*)",0
        defw invert_select_files_right
        defb " CHANGE DRIVE(CS+2)",0
        defw newdisc_right
        defb 255

menuutil
        defb " COMPARE DIRS      ",0
        defw compare_dirs
        defb " HELP           (H)",0
        defw help
        defb " ABOUT CC    (SS+I)",0
        defw info
        defb " SORT DIRECTORY    ",0
        defw notnow
        defb 255

menuquit
        defb " EXIT MENU  (BREAK)",0
        defw menu_exit
        defb " QUIT CCommander   ",0
        defw quit
        defb 255
