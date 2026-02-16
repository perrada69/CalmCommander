            ; ------------------------------------------------------------
            ; Souborový manažer pro ZX Spectrum Next
            ; ------------------------------------------------------------
            ; Autor: Shrek/MB Maniax
            ; Pomoc: ped75g
            ; ------------------------------------------------------------

            DEVICE ZXSPECTRUMNEXT
            OPT reset --zxnext --syntax=abfw
            slot 4

            MACRO VERSION : defb "0.7a" : ENDM

            DEFINE DISP_ADDRESS     $2000
            DEFINE SP_ADDRESS       $3D00
            OPT --zxnext=cspect

            ; ORG pro kód aplikace (záměrně posunuté o 256+256)
            DEFINE ORG_ADDRESS      $7000 + 128 + 128

            ; Testovací stránka RAM (komentář říká poslední page v emulátoru)
            DEFINE TEST_CODE_PAGE   223

            ; Tilemap/tilegfx adresy (tilemap 80*32 = 2560B)
            DEFINE TILE_MAP_ADR     $4000
            DEFINE TILE_GFX_ADR     $6000         ; $5400 bylo zřejmě dřív

                                                  ; Jméno CFG souboru je definováno někde jinde (dspedge.*)
            DEFINE CFG_FILENAME     dspedge.defaultCfgFileName

            ; ------------------------------------------------------------
            ; Struktury – konfigurace “okrajů” a UI (použité jinde v programu)
            ; ------------------------------------------------------------

            STRUCT S_MARGINS                      ; pixely okrajů 0..31 (-1 = nedefinováno)
L           BYTE    -1                            ; left
R           BYTE    -1                            ; right
T           BYTE    -1                            ; top
B           BYTE    -1                            ; bottom
            ENDS

            STRUCT S_UI_DEFINITIONS
labelDot    WORD    0                             ; adresa kam zapsat “tečku” stavu (UI)
cellAdr     WORD    0                             ; adresa buňky velké tabulky na obrazovce
nextMode    BYTE    0
keyword     WORD    0                             ; adresa keywordu pro CFG
            ENDS

            STRUCT S_MODE_EDGES
cur         S_MARGINS                             ; aktuální marginy (MUSÍ být první 4 byty)
orig        S_MARGINS                             ; původní hodnoty z CFG
ui          S_UI_DEFINITIONS                      ; UI konfig pro tento režim

modified    BYTE    0                             ; příznak změny módu
leftT       BYTE    0                             ; počet plných dlaždic vlevo
rightT      BYTE    0                             ; počet plných dlaždic vpravo
midT        BYTE    0                             ; počet “polovičních” top/bottom tiles (bez rohů)

                                                  ; masky pro překreslení tile grafiky (Green/Background), pořadí je důležité
maskLeftG   BYTE    0
maskLeftB   BYTE    0
maskRightG  BYTE    0
maskRightB  BYTE    0
maskTopG    BYTE    0
maskTopB    BYTE    0
maskBottomG BYTE    0
maskBottomB BYTE    0
            ENDS

            STRUCT S_STATE
timingIsUnlocked    BYTE    0
edge                BYTE    0                     ; 0 left,1 top,2 right,3 bottom (mapuje na znaky)
lastCtrlKey         BYTE    0                     ; save/reload/quit/... při potvrzovacích dialozích
debounceKey         BYTE    0
modified            BYTE    0                     ; změna v libovolném módu
noFileFound         BYTE    0
esxErrorNo          BYTE    1
argsPtr             WORD    0
            ENDS

            STRUCT S_PRESERVE
            ; Ukládání hodnot NextRegů (každý WORD: [nr][value]) – používáno jinde
turbo_07            WORD    TURBO_CONTROL_NR_07
spr_ctrl_15         WORD    SPRITE_CONTROL_NR_15
transp_fallback_4A  WORD    TRANSPARENCY_FALLBACK_COL_NR_4A
tile_transp_4C      WORD    TILEMAP_TRANSPARENCY_I_NR_4C
ula_ctrl_68         WORD    ULA_CONTROL_NR_68
display_ctrl_69     WORD    DISPLAY_CONTROL_NR_69
tile_ctrl_6B        WORD    TILEMAP_CONTROL_NR_6B
tile_def_attr_6C    WORD    TILEMAP_DEFAULT_ATTR_NR_6C
tile_map_adr_6E     WORD    TILEMAP_BASE_ADR_NR_6E
tile_gfx_adr_6F     WORD    TILEMAP_GFX_ADR_NR_6F
tile_xofs_msb_2F    WORD    TILEMAP_XOFFSET_MSB_NR_2F
tile_xofs_lsb_30    WORD    TILEMAP_XOFFSET_LSB_NR_30
tile_yofs_31        WORD    TILEMAP_YOFFSET_NR_31
pal_ctrl_43         WORD    PALETTE_CONTROL_NR_43
pal_idx_40          WORD    PALETTE_INDEX_NR_40
mmu2_52             WORD    MMU2_4000_NR_52
mmu3_53             WORD    MMU3_6000_NR_53
            ; záměrně se NEuchovává clip window a tile palette a další “substates”
            ENDS

            ; ------------------------------------------------------------
            ; Konstanty UI / klávesnice
            ; ------------------------------------------------------------

KEY_DEBOUNCE_WAIT   EQU     8

CHAR_DOT_RED        EQU     25
CHAR_DOT_YELLOW     EQU     26
CHAR_DOT_GREEN      EQU     27

CHAR_ARROW_L        EQU     28
CHAR_ARROW_T        EQU     29
CHAR_ARROW_R        EQU     30
CHAR_ARROW_B        EQU     31

            ; další konstanty (HW/API) jsou v include
            INCLUDE "constants.i.asm"


            ; ------------------------------------------------------------
            ; ESXDOS funkce (rst $08 služby) – zde jen symboly
            ; ------------------------------------------------------------

M_DOSVERSION                    equ $88
M_GETSETDRV                     equ $89
M_GETHANDLE                     equ $8D
M_GETERR                        equ $93
F_OPEN                          equ $9A
F_CLOSE                         equ $9B
F_READ                          equ $9D
F_WRITE                         equ $9E
F_SEEK                          equ $9F
F_FGETPOS                       equ $A0
F_UNLINK                        equ $AD
F_RENAME                        equ $B0
FA_READ                         equ $01


; ------------------------------------------------------------
; Helper makra
; ------------------------------------------------------------

ESXDOS      MACRO service?
            ; ESXDOS volání přes RST $08, HL se zkopíruje do IX (ESXDOS ABI)
            push hl
            pop ix
            rst $08
            db service?
            ENDM

NEXTREG2A   MACRO nextreg?
            ; přečti NextReg číslo nextreg? do A (přes ReadNextReg2A)
            ld a,nextreg?
            call ReadNextReg2A
            ENDM

CSP_BREAK   MACRO
            ; breakpoint jen v TESTING buildu
            IFDEF TESTING
                break
            ENDIF
            ENDM

DET                                               ; (neznámý symbol/makro – nechávám bez spekulace)


                                                  ; ------------------------------------------------------------
                                                  ; Start programu a některé RAM oblasti
                                                  ; ------------------------------------------------------------

            org ORG_ADDRESS
S1          jp START

CHARS       equ  15616-256

mystak      equ  24575                            ; ad-hoc adresa pro stack (komentář: pod 4000h, pod BFE0h)
staksto     equ  24575                            ; místo pro BASIC stack pointer

port1       equ  #7FFD                            ; port pro stránkování (klasický 128k port)
catbuff     equ  #A000                            ; buffer pro katalog (pro DOS)
dos_catalog equ  #011E                            ; DOS rutina (externí) – není zde rozebráno


                                                  ; ------------------------------------------------------------
                                                  ; ReadNextReg2A
                                                  ; ------------------------------------------------------------
                                                  ; Vstup:
                                                  ; A = číslo NextReg (index)
                                                  ; Výstup:
                                                  ; A = přečtená hodnota NextReg
                                                  ; Pozn:
                                                  ; Funkce mění aktuálně vybraný NextReg na portu (out na 243B/253B).
                                                  ; ------------------------------------------------------------
ReadNextReg2A:
            push    bc
            ld      bc,#243B                      ; TBBlue register select port
            out     (c),a                         ; zvol NextReg index
            inc     b                             ; BC = #253B (register access port)
            in      a,(c)                         ; načti hodnotu zvoleného NextReg
            pop     bc
            ret


            ; ------------------------------------------------------------
            ; START – inicializace aplikace a načtení prostředí
            ; ------------------------------------------------------------
START
            ld (savesp+1),sp                      ; self-modify: uloží původní SP pro pozdější návrat
                                                  ; ld sp,$a000 - 2            ; (zakomentováno) alternativní nastavení stacku

                                                  ; kopie části systémových proměnných do vlastní oblasti (sysvars)
                                                  ; (23296 = 0x5B00 – typicky oblast ZX sysvars; přesný význam závisí na projektu)
xc

            ld hl,23296
            ld de,sysvars
            ld bc,500
            ldir

            ; nastavení počáteční pozice kurzoru/myši (externí rutina KOREKCE)
            ld hl,50*256+50
            ld (lastCoordMouse),hl

            call KOREKCE                          ; externí: nastaví počáteční souřadnice

            call dospage                          ; externí: přepnutí stránkování/ROM pro DOS funkce?
            call createCfg                        ; externí: vytvoření/načtení CFG

                                                  ; inicializace “staré” hodnoty kolečka myši z portu $FADF (horní nibble)
            ld bc,$fadf
            in a,(c)
            and  $F0
            rrca
            rrca
            rrca
            rrca
            ld (wheelOld),a                       ; uložená pozice kolečka (0..15)

                                                  ; volání rutiny na adrese $01BD (externí)
                                                  ; Zde se z E bere výsledek jako “počet stránek” (pocetstranek)
            ld l,0
            ld h,0
            call $01bd                            ; externí (neodhadovat)
            ld a,e
            ld (pocetstranek),a


            ; ------------------------------------------------------------
            ; Skenování dostupných disků/drive písmen (A..?)
            ; ------------------------------------------------------------
disc        ld l,"A"                              ; self-modify: zde se bude postupně měnit písmeno
            cp "M"
            jr z,dalsi                            ; pokud "M" -> konec? (logika závisí na externích voláních)

            ld bc,bufferdisc
            call $00F7                            ; externí: zřejmě dotaz na drive? (neodhadovat)
                                                  ; //jr nc,neskenuj
            jr z,dalsi                            ; pokud Z, tak “dalsi” (podle volání $00F7)

DSCDET
dscdet      ld hl,discdetail
            ld (hl),a                             ; uloží nějaký byte detailu disku (vrácený v A)
            inc hl
            ld (dscdet+1),hl                      ; posune ukazatel pro další zápis (self-modify)

setdisc     ld hl,listdisc
            ld a,(disc+1)                         ; aktuální písmeno drive z operand části instrukce ld l,"A"
            ld (hl),a                             ; uloží písmeno do seznamu disků
            inc hl
            ld (setdisc+1),hl                     ; posune pointer pro další uložení (self-modify)

            ld hl,pocetdisku
            inc (hl)                              ; inkrement počtu disků v seznamu

dalsi
            ld a,(disc+1)                         ; aktuální drive písmeno
            cp "P"
            jr z,neskenuj                         ; pokud P, přestaň skenovat (limit)
            inc a
            ld (disc+1),a                         ; další písmeno drive
            jr disc

NES
neskenuj


            ; ------------------------------------------------------------
            ; Inicializace grafiky/spritů/GUI
            ; ------------------------------------------------------------
            call basicpage                        ; externí: přepnutí “basic” stránkování?

                                                  ; načti sprite grafiku “sipka” do sprite systému (externí LoadSprites)
            ld hl,sipka
            ld bc,16*16*1
            ld a,0
            call LoadSprites                      ; externí

            call VSE_NASTAV                       ; externí: globální nastavení aplikace/GUI?

            

            ;call showSprite                       ; externí (v předchozím kontextu: aktualizace kurzoru)


                                                  ; ------------------------------------------------------------
                                                  ; menu0 – kopírování bloků (není zde kontext dat v HL/DE/BC)
                                                  ; ------------------------------------------------------------
menu0
            ; kopíruj byte z (HL) do (DE), a za něj zapiš atribut 16
            ; BC určuje délku (počítadlo)
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
            jr nz,menu0

            call kresli                           ; externí: vykreslí hlavní obrazovku


                                                  ; ------------------------------------------------------------
                                                  ; Nastavení aktivních disků pro levé/pravé okno podle PATHLEFT/PATHRIGHT
                                                  ; ------------------------------------------------------------
            call dospage                          ; externí

                                                  ; ld a,255
                                                  ; call $012d                 ; externí: zjisti disk? (zakomentováno)

            ld a,(PATHLEFT)
            ld (actdisc),a

            ld a,(PATHRIGHT)
            ld (actdisc+1),a


            ; ------------------------------------------------------------
            ; Načtení obsahu levého a pravého panelu (volání externích rutin)
            ; ------------------------------------------------------------
            call basicpage
            call dospage

            ; získání pointeru na pathl přes ROZHOD2 (externí)
            ld hl,pathl
            call ROZHOD2                          ; externí: zřejmě vrací HL=ukazatel (u tebe přes mezikrok)
            ld a,(hl)
            inc hl
            ld h,(hl)
            ld l,a

            xor a
            call $01b1                            ; externí (neodhadovat)

            call reload_dir                       ; externí: načtení diru

                                                  ; nastav adresu pro následné vykreslování okna (self-modify)
            ld hl,$4000+2
            ld (adrs+1),hl

            call getroot                          ; externí
            call showwin                          ; externí
            call PROHOD                           ; externí: přepnutí aktivního okna?

                                                  ; zopakuj pro druhý panel (pravý)
            call dospage
            ld hl,pathl
            call ROZHOD2                          ; externí
            ld a,(hl)
            inc hl
            ld h,(hl)
            ld l,a

            xor a
            call $01b1                            ; externí

            call reload_dir                       ; externí

            ld hl,$4000+2+80
            ld (adrs+1),hl
            call getroot_reload                   ; externí
            call showwin                          ; externí
            call PROHOD                           ; externí

            call dospage
            ld hl,pathl
            call ROZHOD2                          ; externí
            ld a,(hl)
            inc hl
            ld h,(hl)
            ld l,a

            xor a
            call $01b1                            ; externí

            call basicpage

            ; zvýraznění kurzoru? (externí writecur)
            ld a,32
            call writecur                         ; externí
            ld a,16                               ; (připraveno pro další použití)

            call freespace                        ; lokální rutina níže: vykreslí “Free: … kB” pro oba panely


                                                  ; ------------------------------------------------------------
                                                  ; Zobrazení “emptydir” v obou oknech (přepínáním OKNO xor 16)
                                                  ; ------------------------------------------------------------
            ld hl,emptypos
            call ROZHOD2                          ; externí: vrátí ukazatel na pozici
            ld a,(hl)
            inc hl
            ld h,(hl)
            ld l,a

            ld de,emptydir
            xor a
            call print                            ; externí: tisk textu

            ld a,(OKNO)
            xor 16
            ld (OKNO),a                           ; přepnutí aktivního okna (bit 4?)

                                                  ; vytiskni znovu do druhého okna
            ld hl,emptypos
            call ROZHOD2                          ; externí
            ld a,(hl)
            inc hl
            ld h,(hl)
            ld l,a

            ld de,emptydir
            xor a
            call print

stop
            ; následná sekvence: přepínání okna a GETDIR
            ; (volání externích rutin – bez odhadů)
            call PROHOD
            call GETDIR                           ; externí
            call PROHOD
            call dospage

            ld hl,pathl
            call ROZHOD2                          ; externí
            ld a,(hl)
            inc hl
            ld h,(hl)
            ld l,a

            xor a
            call $01b1                            ; externí
            call basicpage

            call GETDIR                           ; externí
            call PROHOD
            call dospage

            ld hl,pathl
            call ROZHOD2                          ; externí
            ld a,(hl)
            inc hl
            ld h,(hl)
            ld l,a

            xor a
            call $01b1                            ; externí
            call basicpage


            ; ------------------------------------------------------------
            ; Hlavní smyčka UI
            ; ------------------------------------------------------------
L0
loop0
            ; nastav pozici pro progress bar (self-modify PROGS+1)
            ld   hl,$4000+160*15+23
            ld (PROGS+1),hl

            ; na začátku smyčky se tlačítka myši vynulují (latch/akumulace se začne znovu)
            xor a
            ld (TLACITKO),a

            call gettime                          ; lokální rutina: vypíše čas/datum nebo prázdno

                                                  ; nextreg $56/$55 – bez kontextu jen pozn.: zápis do Next registrů (externí efekt/řízení)
            nextreg $56,0
            nextreg $55,20


            ; vymazání/obnovení textu “Selected” (?) v obou oknech
            ld hl,1*256+30
            ld a,0
            ld de,seltxt
            call print                            ; externí

            ld hl,41*256+30
            ld a,0
            ld de,seltxt
            call print                            ; externí


                                                  ; zobraz numsel (levé okno)
            ld hl,(numsel)
            call NUM                              ; externí: převod čísla do NUMBUF
            ld hl,11*256+30
            ld a,0
            ld de,NUMBUF
            call print                            ; externí

                                                  ; zobraz numsel+2 (pravé okno)
            ld hl,(numsel+2)
            call NUM
            ld hl,51*256+30
            ld a,0
            ld de,NUMBUF
            call print

            ; vypiš oddělovače "/" na pevné adresy v obrazovce
            ld a,"/"
            ld ($4000+30*160+32),a
            ld a,"/"
            ld ($4000+30*160+112),a

            ; zobraz ALLFILES (levé okno)
            ld hl,(ALLFILES)
            call NUM
            ld hl,17*256+30
            ld a,0
            ld (NUMBUF+5),a                       ; ruční ukončení / zkrácení čísla (formát)
            ld de,NUMBUF
            call print

            ; zobraz ALLFILES+2 (pravé okno)
            ld hl,(ALLFILES + 2)
            call NUM
            ld hl,57*256+30
            ld a,0
            ld (NUMBUF+5),a
            ld de,NUMBUF
            call print



            call NOBUFF83                         ; externí: pravděpodobně práce s 8.3 bufferem (neodhadovat)
            call INKEY                            ; externí: načtení klávesy do A
            ld (klavesa),a                        ; uložit poslední klávesu

                                                  ; dispatch kláves – mapování na akce (externí rutiny)
            cp ''
            jp z,info

            cp 10
            jp z,down

            cp 11
            jp z,up

            cp 9
            jp z,rightcur

            cp 8
            jp z,leftcur

            cp 4                                  ; “true video”
            jp z,changewin

            cp 13
            jp z,enter

            cp "8"
            jp z,delete

            cp "9"
            jp z,RENAME

            cp "0"
            jp z,menu

            cp "5"
            jp z,copy

            cp "6"
            jp z,move

            cp 32
            jp z,select

            cp "7"
            jp z,MKDIR

            cp 7
            jp z,newdisc_left

            cp 6
            jp z,newdisc_right

            cp "+"
            jp z,select_files

            cp "*"
            jp z,invert_select_files

            cp "-"
            jp z,deselect

            cp "1"
            jp z,leftwin

            cp "2"
            jp z,rightwin

            cp "h"
            jp z,help

            cp "c"
            jp z,CHNG_ATTR

            cp "i"
            jp z,info_file

            cp 199
            jp z,quit

            call MOUSE
            ; ------------------------------------------------------------
            ; Myš: test levého tlačítka + kolečko (v cspect poznámka, že kolečko nefunguje)
            ; TLACITKO je "latched" (ORované) jinde v kódu (showSprite/MOUSE dle tvých úryvků)
            ; ------------------------------------------------------------
            ld a,(TLACITKO)
            bit 1,a                               ; levé tlačítko je bit1 (bit0 = pravé)
            jp nz,LEVE_TLACITKO                   ; obsluha levého kliknutí


            ld a,(CONTRB)
            bit 0,a                               ; pravé tlačítko je bit1 (bit0 = pravé)
            jp nz,PRAVE_TLACITKO                   ; obsluha levého kliknutí




                                                  ; ------------------------------------------------------------
                                                  ; Kolečko myši: wheelOld je 0..15, čte se z horního nibblu portu $FADF
                                                  ; Logika řeší i přetečení 15->0 a 0->15 (hraniční stavy)
                                                  ; ------------------------------------------------------------
            ld a,(wheelOld)
            cp 15
            jr z,hranicniPatnact
            or a
            jr z,hranicniNula

            ld e,a                                ; E = stará poloha kolečka
            call nactiWheelMysky                  ; načti aktuální polohu kolečka do A
            ld (wheelOld),a                       ; ulož novou polohu
            cp e
            jp z,loop0                            ; beze změny -> další iterace smyčky

            jp c,leftcur                          ; pokud A < E: směr jedním směrem (např. nahoru)
            jp rightcur                           ; pokud A > E: opačný směr


                                                  ; ------------------------------------------------------------
                                                  ; nactiWheelMysky
                                                  ; ------------------------------------------------------------
                                                  ; Čte port $FADF, bere horní nibble (po 4× RRCA) => A=0..15.
                                                  ; ------------------------------------------------------------
nactiWheelMysky
            ld bc,$fadf
            in a,(c)

            and  $F0
            rrca
            rrca
            rrca
            rrca
            ret

TLACITKO    defb 0                                ; latched stav tlačítek myši (bit0=pravé, bit1=levé, ...)

                                                  ; ------------------------------------------------------------
                                                  ; Hraniční případy kolečka:
                                                  ; - když je stará hodnota 15, řeší se přechod 15->0 i “o krok zpět”
                                                  ; - když je stará hodnota 0, řeší se přechod 0->15 i “o krok vpřed”
                                                  ; ------------------------------------------------------------
hranicniPatnact
            ld e,a
            call nactiWheelMysky
            ld (wheelOld),a
            cp 0
            jp z,rightcur                         ; 15 -> 0 (přetečení) vyhodnoceno jako směr "rightcur"
            cp 14
            jp z,leftcur                          ; 15 -> 14 směr "leftcur"
            jp loop0

hranicniNula
            ld e,a
            call nactiWheelMysky
            ld (wheelOld),a
            cp 15
            jp z,leftcur                          ; 0 -> 15 (přetečení) vyhodnoceno jako "leftcur"
            cp 1
            jp z,rightcur                         ; 0 -> 1 směr "rightcur"
            jp loop0

wheelOld    defb 0                                ; poslední načtená “pozice” kolečka (0..15)


                                                  ; ------------------------------------------------------------
                                                  ; freespace
                                                  ; ------------------------------------------------------------
                                                  ; UI rutina: vypíše text "Free:" do obou panelů a pak z DOSu zjistí volné místo
                                                  ; (přes externí volání na $0121) a vypíše v kB.
                                                  ; ------------------------------------------------------------
freespace
            ; text "Free:" do levého panelu
            ld hl,24*256 + 30
            ld de,freetxt
            ld a,0
            call print                            ; externí

                                                  ; text "Free:" do pravého panelu
            ld hl,64*256 + 30
            ld de,freetxt
            ld a,0
            call print

            call dospage                          ; externí: přepni prostředí pro DOS volání

                                                  ; levý panel: připrav pozici pro DEC32 výpis (self-modify dec32pos+1)
            ld hl,29*256 + 30
            ld (dec32pos+1),hl

            ld a,(actdisc)
            call $121                             ; externí: vrací volné místo? (B,C?) – neodhadovat
            ld h,b
            ld l,c
            ex de,hl                              ; DE = hodnota pro DEC32
            ld b,8
            call DEC32                            ; externí: převod 32bit hodnoty na text? (neodhadovat)

                                                  ; přidej "kB"
            ld hl,37*256 + 30
            ld de,kb
            ld a,0
            call print

            ; pravý panel: totéž
            ld hl,69*256 + 30
            ld (dec32pos+1),hl

            ld a,(actdisc)
            call $121                             ; externí
            ld h,b
            ld l,c
            ex de,hl
            ld b,8
            call DEC32                            ; externí

            ld hl,77*256 + 30
            ld de,kb
            ld a,0
            call print

            call basicpage                        ; externí: zpět do “basic” prostředí?
            ret

freetxt     defb "Free:",0
kb          defb "kB",0


; ------------------------------------------------------------
; setleftwin / setrightwin
; ------------------------------------------------------------
; Přepnutí aktivního panelu (OKNO) + UI kurzoru (writecur),
; a zavolání externích rutin pro nastavení aktivní cesty (ROZHOD2, $01B1).
; ------------------------------------------------------------
setleftwin
            ld a,0
            call writecur                         ; externí: vymaž/starý kurzor?

            ld a,3
            ld (OKNO),a                           ; OKNO = 3 (konkrétní význam neznám, ale odpovídá “levému”)

            ld a,32
            call writecur                         ; externí: nastav nový kurzor?

            call dospage

            ld hl,pathl
            call ROZHOD2                          ; externí: vrátí ukazatel
            ld a,(hl)
            inc hl
            ld h,(hl)
            ld l,a

            xor a
            call $01b1                            ; externí: nastavení cesty/drive?
            call basicpage
            ret


setrightwin
            ld a,0
            call writecur

            ld a,$13
            ld (OKNO),a                           ; OKNO = $13 (konkrétní význam neznám, odpovídá “pravému”)

            ld a,32
            call writecur

            call dospage

            ld hl,pathl
            call ROZHOD2                          ; externí
            ld a,(hl)
            inc hl
            ld h,(hl)
            ld l,a

            xor a
            call $01b1                            ; externí
            call basicpage
            ret


            ; ------------------------------------------------------------
            ; Wrappery: akce pro levý/pravý panel → nastav okno a skoč do společné rutiny
            ; ------------------------------------------------------------
select_files_left
            call setleftwin
            jp select_files                       ; externí

deselect_files_left
            call setleftwin
            jp deselect                           ; externí

select_files_right
            call setrightwin
            jp select_files                       ; externí

invert_select_files_left
            call setleftwin
            jp invert_select_files                ; externí

invert_select_files_right
            call setrightwin
            jp invert_select_files                ; externí

deselect_files_right
            call setrightwin
            jp deselect                           ; externí


                                                  ; ------------------------------------------------------------
                                                  ; newdisc_left/right + leftwin/rightwin + lw/rw
                                                  ; ------------------------------------------------------------
                                                  ; Opakující se blok: nastav aktivní okno, nastav cestu přes DOS,
                                                  ; pak buď skoč do changedrive/loop0 nebo se vrať (lw/rw).
                                                  ; (Volání externích rutin se neodhadují.)
                                                  ; ------------------------------------------------------------
newdisc_left
            ld a,0
            call writecur
            ld a,3
            ld (OKNO),a
            ld a,32
            call writecur

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
            jp changedrive                        ; externí


leftwin
            ld a,0
            call writecur
            ld a,3
            ld (OKNO),a
            ld a,32
            call writecur

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
            jp loop0


lw
            ; stejná logika jako leftwin, ale končí RET (použito jako podprogram)
            ld a,0
            call writecur
            ld a,3
            ld (OKNO),a
            ld a,32
            call writecur

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
            ret


rightwin
            ld a,0
            call writecur
            ld a,$13
            ld (OKNO),a
            ld a,32
            call writecur

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
            jp loop0


rw
            ; stejná logika jako rightwin, ale končí RET
            ld a,0
            call writecur
            ld a,$13
            ld (OKNO),a
            ld a,32
            call writecur

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
            ret


newdisc_right
            ld a,0
            call writecur
            ld a,$13
            ld (OKNO),a
            ld a,32
            call writecur

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
            jp changedrive                        ; externí


                                                  ; ------------------------------------------------------------
                                                  ; Konfigurace a proměnné pro progress bar
                                                  ; ------------------------------------------------------------
souboru_na_radek    equ 26

PROGPROM    defw 0
PROGPROM2   defw 0


; ------------------------------------------------------------
; PROVYP – výpočet pro progress (podle komentáře autora)
; ------------------------------------------------------------
; Vstup:
; DE = počet (např. souborů, bloků…)
; Výstup:
; HL a A (podle tvého komentáře: HL na CPPX1, A na CPPX3)
; Pozn:
; V téhle ukázce nejsou CPPX1/CPPX3, jen algoritmus výpočtu.
; ------------------------------------------------------------
PROVYP
            ld   h,d
            ld   l,e
            ld   a,d
            or   a
            jr   nz,PROV3
            ld   a,e
            cp   9
            jr   c,PROV4

PROV3
            ; zde se opakovaně posouvá HL a A – vypadá to jako aproximace dělení (škálování)
            xor  a
            srl  h
            rr   l
            rra
            srl  h
            rr   l
            rra
            srl  h
            rr   l
            rra
            srl  h
            rr   l
            rra
            or   a

            ld   a,1
            ret  z
            inc  hl
            ret

PROV4
            ; malé hodnoty (1..8) se mapují tabulkou TABPRO
            ld   hl,TABPRO
            dec  de
            add  hl,de
            ld   a,(hl)
            ld   hl,1
            ret

TABPRO      db 16,8,5,4,3,2,2,2


; ------------------------------------------------------------
; PRGRS1/PROGRES – vykreslení “teploměru” (progress bar)
; ------------------------------------------------------------
; B = počet segmentů; zapisuje se do obrazovky po 2 bajtech (char+attr nebo tile+attr)
; ------------------------------------------------------------
PRGRS1
            ld   b,1

PROGRES
            push hl

PROGS        ld   hl,$4000+160*15+25              ; self-modify: aktuální pozice bar kreslení
            ld   a,36                             ; barva segmentu progressu

PROGRSM1
            ld   (hl),a                           ; zapiš hodnotu (barva/atribut)
            inc  hl
            inc  hl                               ; krok o 2 (zřejmě přeskočí “znak”/“atribut” dle layoutu)
            djnz PROGRSM1

            ld   (PROGS+1),hl                     ; posuň výchozí pozici pro další volání
            pop  hl
            ret


            ; Druhý progress bar (jiný řádek)
PROGRES2
            push hl

PROGS2       ld   hl,$4000+160*14+25
            ld   a,36

PROGRSM12
            ld   (hl),a
            inc  hl
            inc  hl
            djnz PROGRSM12

            ld   (PROGS2+1),hl
            pop  hl
            ret


            ; ------------------------------------------------------------
            ; clearpr – vymazání progress baru (vypíše hodnotu 16 do 40 segmentů)
            ; ------------------------------------------------------------
clearpr
            ld   hl,$4000+160*14+25
            ld   a,16
            ld   b,40
clearpr2
            ld (hl),a
            inc hl
            inc hl
            djnz clearpr2
            ret


            ; ------------------------------------------------------------
            ; Buffery pro seznam disků
            ; ------------------------------------------------------------
bufferdisc      defs 18
listdisc        defs 15
pocetdisku      defb 0
pocetstranek    defb 0

rtcpresent      defb 0


; ------------------------------------------------------------
; gettime – načti čas/datum z DOSu a vypiš do UI
; ------------------------------------------------------------
; Volá externí DOS rutinu na $01CC, výsledky ukládá do dostime/dosdate
; a formátuje je do textu přes NUM + print.
; Pokud není RTC dostupné / volání selže, vypíše prázdný řádek.
; ------------------------------------------------------------
gettime
            call dospage
            call $01cc                            ; externí: načti čas+datum, DE=time, BC=date
            ld (dostime),de
            ld (dosdate),bc

            push af
            call basicpage
            pop af

            jp nc,notimeend                       ; pokud “NC” => bez času (dle autora)

            ld a,1
            ld (rtcpresent),a

            ; Následuje dekódování času z registrů D/E do HH:MM
            ; (bez přesného formátu zdroje nebudu tvrdit víc, jen že se tu skládají čísla)
            ld   a,d
            ld   b,e
            srl  a
            rr   b
            srl  a
            rr   b
            srl  a
            rr   b
            srl  b
            srl  b
            push bc

            ; tisk “hodiny” (část NUMBUF+3)
            ld l,a
            ld h,0
            call NUM
            ld hl,63*256+0
            ld a,16
            ld de,NUMBUF+3
            call print

            ; tisk ":" mezi hodinami a minutami
            ld hl,65*256+0
            ld a,16
            ld de,dvojt
            call print

            pop  af                               ; zde se zřejmě bere druhá složka času (minuty) z uloženého BC

            ld l,a
            ld h,0
            call NUM
            xor a
            ld (NUMBUF+3+2),a                     ; ukončení/format
            ld hl,66*256+0
            ld a,16
            ld de,NUMBUF+3
            call print


            ; Dekódování data z dosdate (DE) – den/měsíc/rok
            ld de,(dosdate)
            ld a,e
            and 31                                ; den v měsíci (mask 5 bitů)
            push de

            ; tisk dne
            ld l,a
            ld h,0
            call NUM
            ld hl,69*256+0
            ld a,16
            ld de,NUMBUF+3
            call print

            ; tisk "."
            ld hl,71*256+0
            ld a,16
            ld de,tecka
            call print

            pop de

            ; výpočet měsíce (bitové přesuny; přesný formát datum wordu je typicky MS-DOS FAT)
            ld a,e
            ld b,d
            srl b
            push bc
            rra
            rra
            rra
            rra
            rra
            and 15

            ; tisk měsíce
            ld l,a
            ld h,0
            call NUM
            ld hl,72*256+0
            ld a,16
            ld de,NUMBUF+3
            call print

            ; tisk "."
            ld hl,74*256+0
            ld a,16
            ld de,tecka
            call print

            pop af

            ; výpočet roku: (A + 1980) – odpovídá DOS/FAT datumu (rok od 1980)
            ld l,a
            ld h,0
            ld de,1980
            add hl,de
            call NUM
            call smaznuly                         ; externí: odstranění nul? (neodhadovat detail)

                                                  ; tisk roku (NUMBUF+1, ořez délky)
            ld hl,75*256+0
            ld de,NUMBUF+1
            xor a
            ld (NUMBUF+1+4),a
            ld a,16
            call print

timeend
            ret


            ; když není čas dostupný, vymaže řádek a nastaví rtcpresent=0
notimeend
            xor a
            ld (rtcpresent),a
            ld hl,63*256+0
            ld a,16
            ld de,notimetxt
            call print
            ret

notimetxt   defb "                ",0

; proměnné pro čas/datum
istime      defb 0
den         defb 0
mesic       defb 0
rok         defb 0
dvojt       defb ":",0

hodiny      defb 0
minuty      defb 0
dostime     defw 0
dosdate     defw 0

; ------------------------------------------------------------
; unsup
; ------------------------------------------------------------
; Zobrazí okno "unsupported" pro soubory s nepodporovanou příponou
; a čeká na Enter nebo klik do tlačítka (CONTROL_CLICK).
; ------------------------------------------------------------
unsup
        call savescr                              ; externí: ulož obrazovku/VRAM stav pro pozdější obnovu

                                                  ; vykresli okno (pozice+velikost) – konkrétní parametry závisí na rutině window
        ld hl,10 * 256 + 10                       ; HL = (y<<8) + x
        ld bc,60 * 256 + 5                        ; BC = (šířka/…?) + (výška/…?) – dle implementace window
        ld a,144                                   ; barva/atribut okna
        call window                               ; externí: vykresli okno

                                                  ; text "Unsupported" (nebo podobně) – tisk do okna
        ld hl,12*256+11
        ld a,144
        ld de,unsuptxt
        call print                                ; externí: tisk textu

                                                  ; další řádek hlášky (norun)
        ld hl,12*256+13
        ld a,144
        ld de,norun
        call print

        ; "Continue / press enter" atp. (conttxt), zvýrazněná barva 48
        ld hl,54*256+15
        ld a,16
        ld de,conttxt
        call print

enterwait
        ; v tomhle čekání se vždy vynuluje latched stav myši
        xor a
        ld (TLACITKO),a

        call INKEY                                ; externí: načti klávesu do A
        cp 13
        jp z,enterno2                             ; Enter => zavři okno (obnov screen) a návrat do loop0

                                                  ; klik na potvrzovací tlačítko v okně
                                                  ; CONTROL_CLICK: externí, podle použití CF=1/0 indikace trefení
        ld hl,buttonYes2
        call CONTROL_CLICK
        jp nc,enterno2                            ; pokud klik na tlačítko => ukonči okno

        jr enterwait


        ; ------------------------------------------------------------
        ; enter
        ; ------------------------------------------------------------
        ; Akce po Enter na souboru:
        ; - spočítá index vybrané položky v panelu (POSKURZL + STARTWINL)
        ; - přes BUFF83/find83/FINDLFN získá jméno souboru a metadata
        ; - rozliší adresář vs soubor
        ; - pro soubor připraví řetězec cmd2 = filename a podle přípony volí RUN_*
        ; - nepodporované přípony -> unsup
        ; ------------------------------------------------------------
enter
        ; získej ukazatel na proměnnou POSKURZL podle aktivního okna (ROZHOD)
        ld hl,POSKURZL
        call ROZHOD                               ; externí: zřejmě zvolí levý/pravý panel -> vrací HL na správnou proměnnou
        ld a,(hl)
        ld l,a
        ld h,0                                    ; HL = pozice kurzoru v okně (0..)

                                                  ; HL (pos) si uložíme, a spočítáme absolutní index = STARTWINL + pos
        push hl
        ld hl,STARTWINL
        call ROZHOD2                              ; externí: vrací HL na WORD proměnnou pro aktivní okno
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a                                    ; HL = STARTWIN? (word)

        ex de,hl                                  ; DE = STARTWIN
        pop hl                                    ; HL = pos
        add hl,de                                 ; HL = STARTWIN + pos  (index v listu)
        push hl
        inc hl                                    ; v řadě míst je “+1”, typicky přeskočení hlavičky / 1-based index
        call BUFF83                               ; externí: připrav 8.3 buffer pro položku index HL
        call find83                               ; externí: vyhledej/vyplň TMP83 dle indexu
        pop hl

        ; vypln je self-modify (operand) – tady se 2× přepisuje, nakonec zůstane $20
        ; (pravděpodobně nastavení výplně / paddingu u textu při práci s LFN)
        xor a
        ld (vypln+1),a
        ld a,$20
        ld (vypln+1),a

        call FINDLFN                              ; externí: získej LFN jméno do LFNNAME (podle použití níže)

                                                  ; test na “directory” bit v TMP83
        ld ix,TMP83
        bit 7,(ix+7)
        jp nz,enter_directory                     ; pokud je to adresář -> skok do obsluhy adresáře (není v úryvku)

                                                  ; ------------------------------------------------------------
                                                  ; Příprava cmd2/cmd3 bufferů (vymazání)
                                                  ; ------------------------------------------------------------
        push hl
        push de

        ; vynuluj cmd2 (100B) – připravíme si buffer pro příkaz/jméno
        ld hl,cmd2
        ld de,cmd2+1
        ld bc,99
        xor a
        ld (hl),a
        ldir

        pop de
        pop hl

        ; vynuluj name (50B) – lokální buffer (použití mimo úryvek)
        ld hl,name
        ld de,name+1
        xor a
        ld (hl),a
        ld bc,50
        ldir


        ; ------------------------------------------------------------
        ; RUN – převeď LFNNAME do nulou ukončeného řetězce, spočítej délku,
        ; zkopíruj do cmd2 a pak zkus přípony.
        ; ------------------------------------------------------------
RUN
        ; LFNNAME+59: hledá se konec jména v pevné šířce 60 znaků
        ld hl,LFNNAME+59

run2
        ; přeskoč trailing mezery (0x20) od konce
        ld a,(hl)
        dec hl
        cp 32
        jr z,run2

        ; po nalezení posledního ne-space se posune na skutečný konec a doplní terminátor 0
        inc hl
        inc hl
        ld a,0
        ld (hl),a                                 ; nulový terminátor

                                                  ; spočítej délku: HL - LFNNAME
        ld de,LFNNAME
        or a
        sbc hl,de                                 ; HL = délka (včetně terminátoru? dle předchozích posunů)
        ld b,h
        ld c,l

xxx
        ; uložit délku do operandu delkaNazvu+1 (self-modify)
        ld h,b
        ld l,c
        ld (delkaNazvu + 1),hl                    ; později se používá jako BC pro kopii jména do BASICu

                                                  ; zkopíruj jméno do cmd2 (buffer pro další testy)
        ld hl,LFNNAME
        ld de,cmd2

        push hl
        push bc
        ldir                                      ; kopie BC bajtů (délka) z LFNNAME -> cmd2
        pop bc
        pop hl

        ; zkopíruj zbytek (?) do cmd3 (cmd3 je další buffer 100B)
        ; Pozn.: zde se pokračuje s HL tam kde skončil předchozí LDIR (tj. za jménem)
        ld de,cmd3
        ldir


        ; ------------------------------------------------------------
        ; 1) whitelist podporovaných přípon – pokud žádná nesedí => unsup
        ; pripony: porovnává poslední 3 znaky před koncem řetězce
        ; ------------------------------------------------------------
        ld hl,cmd2
        ld de,ext_nex
        call pripony
        jp z,rrrun

        ld hl,cmd2
        ld de,ext_NEX
        call pripony
        jp z,rrrun

        ld hl,cmd2
        ld de,ext_tap
        call pripony
        jp z,rrrun

        ld hl,cmd2
        ld de,ext_TAP
        call pripony
        jp z,rrrun

        ld hl,cmd2
        ld de,ext_z80
        call pripony
        jp z,rrrun

        ld hl,cmd2
        ld de,ext_Z80
        call pripony
        jp z,rrrun

        ld hl,cmd2
        ld de,ext_snx
        call pripony
        jp z,rrrun

        ld hl,cmd2
        ld de,ext_sna
        call pripony
        jp z,rrrun

        ld hl,cmd2
        ld de,ext_SNX
        call pripony
        jp z,rrrun

        ld hl,cmd2
        ld de,ext_SNA
        call pripony
        jp z,rrrun

        ld hl,cmd2
        ld de,ext_bas
        call pripony
        jp z,rrrun

        ld hl,cmd2
        ld de,ext_BAS
        call pripony
        jp z,rrrun

        ; pokud žádná přípona neodpovídá -> nepodporováno
        jp unsup


        ; ------------------------------------------------------------
        ; rrrun – druhá fáze: dle přípony zvol konkrétní RUN_* rutinu
        ; ------------------------------------------------------------
rrrun
        ; přepnutí prostředí (externí)
        call basicpage

        ; NEX
        ld hl,cmd2
        ld de,ext_nex
        call pripony
        jp z,RUN_NEX_FILE

        ld hl,cmd2
        ld de,ext_NEX
        call pripony
        jp z,RUN_NEX_FILE

        ; TAP
        ld hl,cmd2
        ld de,ext_tap
        call pripony
        jp z,RUN_TAP

        ld hl,cmd2
        ld de,ext_TAP
        call pripony
        jp z,RUN_TAP

        ; snapshoty
        ld hl,cmd2
        ld de,ext_z80
        call pripony
        jp z,RUN_SNAP

        ld hl,cmd2
        ld de,ext_Z80
        call pripony
        jp z,RUN_SNAP

        ld hl,cmd2
        ld de,ext_snx
        call pripony
        jp z,RUN_SNAP

        ld hl,cmd2
        ld de,ext_sna
        call pripony
        jp z,RUN_SNAP

        ld hl,cmd2
        ld de,ext_SNX
        call pripony
        jp z,RUN_SNAP

        ld hl,cmd2
        ld de,ext_SNA
        call pripony
        jp z,RUN_SNAP

        ; BASIC
        ld hl,cmd2
        ld de,ext_BAS
        call pripony
        jp z,RUN_BAS

        ld hl,cmd2
        ld de,ext_bas
        call pripony
        jp z,RUN_BAS

        ; fallback -> nic nespouštěj
        jp loop0


        ; ------------------------------------------------------------
        ; RUN_BAS
        ; ------------------------------------------------------------
        ; Připraví BASIC environment a vloží do BASIC řádku příkaz pro LOAD/RUN
        ; (používá systémové proměnné v oblasti 23296 a BASIC programovou oblast okolo $5Dxx)
        ; ------------------------------------------------------------
RUN_BAS
        call potvrd                               ; lokální: confirm dialog (Yes/No)
        call dospage                              ; externí
        call zapisCfg                             ; externí: uložit konfiguraci
        call basicpage                            ; externí
        call layer0                               ; lokální: nastav vrstvy/grafiku na “BASIC mód”

                                                  ; obnov sysvars zpět do 23296 (před spuštěním BASICu)
        ld de,23296
        ld hl,sysvars
        ld bc,500
        ldir

        ; příprava některých BASIC systémových registrů (typické pro návrat do BASICu)
        ld   iy,23610
        ld   hl,10072
        exx
        im   1
        ld   a,63
        ld   i,a
        ld   a,16
        ld   bc,32765
        out  (c),a

bass
        ; najdi konec řetězce v cmd2 (hledá 0 v max 100B)
        ld hl,cmd2
        ld bc,100
        ld a,0
        cpir

        ; hl ukazuje 1 za nalezeným 0 -> vrátit se na poslední pozici a nahradit 0 uvozovkou
        dec hl
        ld a,$22
        ld (hl),a

        ; spočti délku řetězce v cmd2
        ld de,cmd2
        or a
        sbc hl,de                                 ; HL = délka
        ld b,h
        ld c,l

        push hl

        ; před cmd2 připrav token (0xEF) a uvozovku (0x22)
        ; (cmd2-2 a cmd2-1 jsou dva bajty před bufferem cmd2 -> musí být alokované!)
        ld a,$ef
        ld (cmd2-2),a
        ld a,$22
        ld (cmd2-1),a

        ; zkopíruj připravený token+uvozovku+řetězec do BASIC oblasti $5D23
        ; (BC se zvětší o 2 kvůli těm dvěma prefix bajtům)
        inc bc
        inc bc
        ld hl,cmd2-2
        ld de,$5d23
        ldir

        ; doplň koncovou uvozovku a CR (0x0D) do BASIC řádku
        ex de,hl
        ld a,$22
        ld (hl),a
        inc hl
        ld a,$d
        ld (hl),a

        pop hl
        ld (delkaRadku),hl                        ; uloží délku řádku pro BASIC (adresa je equ níže)
        call spravneStranky


                                                  ; “vyremování” tokenu SPECTRUM v BASIC hlavičce
                                                  ; (0xEA = REM token)
        ld a,$ea
        ld ($5d1d),a
        ret

delkaRadku  equ $5d21


; ------------------------------------------------------------
; RUN_SNAP
; ------------------------------------------------------------
; Připrav prostředí a spustí ESXDOS "dot command" přes rst $08, service $8F
; (přesnou funkci $8F nepopisuju – je to externí ESXDOS služba)
; ------------------------------------------------------------
RUN_SNAP
        call potvrd
        call dospage
        call zapisCfg
        call basicpage
        call layer0

        ; obnov sysvars
        ld de,23296
        ld hl,sysvars
        ld bc,500
        ldir

        ; inicializace systému před návratem do loaderu/ROMu
        ld   iy,23610
        ld   hl,10072
        exx
        im   1
        ld   a,63
        ld   i,a
        ld   a,16
        ld   bc,32765
        out  (c),a

        ; připrav příkaz “run     ” do cmd (kopíruje tapein -> cmd)
        ld hl,tapein
        ld de,cmd
        ld bc,cmd-tapein
        ldir

        ; vypni turbo
        nextreg TURBO_CONTROL_NR_07,0
        call spravneStranky

        ; zavolej ESXDOS službu $8F s parametry v IX=cmd
        ld ix,cmd
        rst $08
        defb $8f
        ret


        ; ------------------------------------------------------------
        ; layer0
        ; ------------------------------------------------------------
        ; Nastaví grafický režim: tilemap vyp, vyčistí ULA screen, nastaví priority spritů.
        ; (Používá NextRegy, logika je zde jasná z instrukcí.)
        ; ------------------------------------------------------------
layer0
        nextreg PALETTE_CONTROL_NR_43,%0'000'0000
        nextreg TILEMAP_BASE_ADR_NR_6E,44
        nextreg TILEMAP_GFX_ADR_NR_6F,12
        nextreg $6B, 0                            ; TILEMAP_CONTROL? (dle konstant bývá tile ctrl)
        nextreg $68, 0                            ; ULA_CONTROL? (dle konstant bývá ula ctrl)

                                                  ; vyčisti ULA bitmapu+atributy (16384..)
        ld hl,16384
        ld de,16385
        ld bc,6143
        xor a
        ld (hl),a
        ldir

        ; sprite priority / enable (komentář: layer priority: USL)
        nextreg SPRITE_CONTROL_NR_15,%01100010
        ret


        ; ------------------------------------------------------------
        ; Datové konstanty používané pro RUN_TAP / BASIC injekci
        ; ------------------------------------------------------------
cestaSys        defb "c:/nextzxos/",$ff

loadTap         defb $22, ":", $ef,$22,$22,$d
loadTapLen      equ $-loadTap

loadTapNext
        defb $22, ":", $ef,$22,"t:",$22,":",$ef,$22,$22,$d
loadTapNextLen  equ $-loadTapNext

; BASIC tokenizovaný text (bez rozboru tokenů – jsou to pevná data)
taptxt  defb $fd,$36,$35,$33,$36,$37,$0E,$0,$0,$57,$ff,$00,$3a,$ef,$22,$74,$61,$70,$6c,$6f,$61,$64,$2E,$62,$61,$73,$22,$3A,$66,$24,$3D
        defb $22
taptxt2 defb $22,$3A,$61,$64,$6A,$3D,$30,$0E,$00,$00,$00,$00,$00,$3A,$ec,$31,$0e,$00,$00,$01,$00,$00,$0d
taptxt3

uvozovkyVBasicu  equ $5d58                        ; adresa v BASIC oblasti pro uvozovky/řetězec


                                                  ; ------------------------------------------------------------
                                                  ; RUN_TAP
                                                  ; ------------------------------------------------------------
                                                  ; Připraví BASIC řádek pro LOAD a nastaví některé systémové hodnoty,
                                                  ; řeší odlišné chování v režimu “Next” (cursorComp==3).
                                                  ; ------------------------------------------------------------
RUN_TAP
        call potvrd                               ; confirm dialog
        call vyberPocitace                        ; externí: volba “počítače/konfigurace” (neodhadovat)
        call layer0

st
        call dospage
        call zapisCfg
        call basicpage

        ; obnov sysvars
        ld de,23296
        ld hl,sysvars
        ld bc,500
        ldir

        ; zkopíruj filename (cmd2) do BASIC oblasti uvozovek
        ld hl,cmd2
        ld de,uvozovkyVBasicu
delkaNazvu
        ld bc,0                                   ; self-modify: délka jména z RUN části
        ldir

        ; doplň loadTap makro za filename
        ld hl,loadTap
        ld bc,loadTapLen
        ldir

        ; spočítej délku BASIC řádku a ulož do $5D21
        ld de,loadTapLen
        ld hl,(delkaNazvu+1)
        add hl,de
        ld de,9
        add hl,de
        ld ($5d21),hl

savesp   ld sp,0                                  ; self-modify: obnov původní SP uložený na STARTu

                                                  ; vyber hodnotu z tabComp podle cursorComp a ulož do $5D4B (BASIC sysvar)
        ld a,(cursorComp)
        ld e,a
        ld d,0
        ld hl,tabComp
        add hl,de
        ld a,(hl)
        ld ($5d4B),a

        ; další inicializace systému před návratem do BASIC
        ld   iy,23610
        ld   hl,10072
        exx
        di
        ld   a,63
        ld   i,a
        ld   a,16
        ld   bc,32765
        out  (c),a
        ei
        im   1

        ; pokud není zvolen “Next config” (cursorComp != 3), končíme
        ld a,(cursorComp)
        cp 3
        ret nz

        ; pro Next config: vyremuj token SPECTRUM
        ld a,$ea
        ld ($5d1d),a

        ; znovu překopíruj filename a použij loadTapNext variantu (s "t:")
        ld hl,cmd2
        ld de,uvozovkyVBasicu
        ld bc,(delkaNazvu+1)
        ldir

        ld hl,loadTapNext
        ld bc,loadTapNextLen
        ldir

        ; přepočet délky řádku (delkaRadku)
        ld de,loadTapNextLen
        ld hl,(delkaNazvu+1)
        add hl,de
        ld de,9
        add hl,de
        ld (delkaRadku),hl

        ; znovu nastav $5D4B podle tabComp
        ld a,(cursorComp)
        ld e,a
        ld d,0
        ld hl,tabComp
        add hl,de
        ld a,(hl)
        ld ($5d4B),a
        call spravneStranky

        ret


        ; ------------------------------------------------------------
        ; RUN_NEX_FILE
        ; ------------------------------------------------------------
        ; Confirm + ulož cfg + obnov sysvars + zavolej ESXDOS $8F s IX=cmd
        ; ------------------------------------------------------------
RUN_NEX_FILE

        call potvrd
        call layer0
        call dospage
        call zapisCfg
        call basicpage

        ; obnov sysvars
        ld de,23296
        ld hl,sysvars
        ld bc,500
        ldir
        call layer0                               ; znovu (pravděpodobně jistota stavu vrstvy)

                                                  ; ESXDOS dot-command / loader přes $8F (externí)

        call spravneStranky
        ld ix,cmd
        rst $08
        defb $8f

        jp loop0

dfdf
ClearAllSprites:
        nextreg $34, 0          ; Začni od slotu 0
    ld b, 128               ; 128 spritů
.loop:
    nextreg $35, 0          ; X low = 0
    nextreg $36, 0          ; Y = 0
    nextreg $37, 0          ; Attr 2: Bit 7 = 0 (VYPNUTO), X high = 0
    nextreg $38, 0          ; Attr 3: Všechny flagy pryč
    ; Registr $34 se po zápisu do $38 automaticky inkrementuje na další slot!
    djnz .loop
    call ClearSpritePatterns
    ret
ClearSpritePatterns:
    nextreg $3b, 0          ; Vyber vzor (Pattern) 0
    ld bc, $005b            ; Port $5B (Sprite Pattern System)
    ld hl, 128 * 256        ; 128 vzorů * 256 bytů
.clearLoop:
    xor a
    out (c), a              ; Pošli nulu
    dec hl
    ld a, h
    or l
    jr nz, .clearLoop
    ret
spravneStranky
        ld a,$0e
        nextreg $56,a
        ld a,$0f
        nextreg $57,a

        call ClearAllSprites
        ret
        ; ------------------------------------------------------------
        ; potvrd
        ; ------------------------------------------------------------
        ; Potvrzovací dialog (Yes/No):
        ; - kreslí okno, text, tlačítka
        ; - čeká na Enter nebo klik myší
        ; - pokud No/ESC-like (klávesa==1) -> enterno -> obnov obrazovku a návrat loop0
        ; ------------------------------------------------------------
potvrd
        call savescr                              ; externí: ulož obrazovku

        ld hl,10 * 256 + 10
        ld bc,60 * 256 + 5
        ld a,16
        call window                               ; externí

        ld hl,11*256+11
        ld a,16
        ld de,runtxt
        call print

        ; tlačítko YES (barva 48)
        ld hl,60*256+15
        ld a,48
        ld de,yestxt
        call print

        ; tlačítko NO (barva 16)
        ld hl,60*256+14
        ld a,16
        ld de,notxt
        call print

enterwait2
        xor a
        ld (TLACITKO),a                           ; vynuluj latched tlačítka (čekací smyčka)
        call INKEY                                ; externí: klávesa do A

        cp 1
        jp z,enterno                              ; klávesa 1 => zruš akci (obnov obrazovku, návrat loop0)

        cp 13
        jr z,enterw2                              ; Enter => potvrď (RET z potvrd)

                                                  ; čekej dokud není levé tlačítko (bit1) “aktivní”
        ld a,(TLACITKO)
        bit 1,a
        jr z,enterwait2

        ; ověř, že klik padl do oblasti YES, jinak test NO
        ld hl,buttonYes
        call CONTROL_CLICK
        ret nc                                    ; klik na YES => potvrdit (CF=1?) => návrat z potvrd

        ld hl,buttonNo
        call CONTROL_CLICK
        jr nc,enterno                             ; klik na NO => zrušit

        jr enterwait2

enterw2
        ret

        ; definice obdélníků tlačítek (souřadnice pro CONTROL_CLICK)
buttonYes  defb 120,120
           defb 140,128

buttonYes2 defb 108,120
           defb 138,128

buttonNo   defb 120,112
           defb 140,120


           ; ------------------------------------------------------------
           ; enterno/enterno2
           ; ------------------------------------------------------------
           ; Zrušení akce: obnoví uloženou obrazovku a skočí do loop0.
           ; Pozn.: enterno má "pop hl" – musí odpovídat konkrétnímu call-stacku volajícího.
           ; ------------------------------------------------------------
enterno
        pop hl                                    ; uvolnění návratové adresy/parametru (jen tam kde to sedí!)
enterno2
        call loadscr                              ; externí: obnov obrazovku
        jp loop0


        ; ------------------------------------------------------------
        ; pripony
        ; ------------------------------------------------------------
        ; Porovnání přípony souboru.
        ; Vstup:
        ; HL = adresa nulou ukončeného názvu (cmd2)
        ; DE = adresa řetězce přípony (např ".nex") – v datech je 4 znaky včetně tečky
        ; Výstup:
        ; Z = shoda (poslední 3 znaky názvu odpovídají posledním 3 znakům přípony)

        ; Poznámka:
        ; Rutina hledá konec názvu (0) přes CPIR, pak jde o 2 bajty zpět a porovnává 3 znaky pozpátku.
        ; Tečka se netestuje – testují se jen 3 písmena.
        ; ------------------------------------------------------------
        ; HL ... adresa nazvu
        ; DE ... pripona
pripony
        push de

        ld bc,50
hledej
        ld a,0
        cpir                                      ; najdi terminátor 0 (max 50B)

        pop de
        dec hl
        dec hl                                    ; vrať se na poslední znak názvu (před 0)

        ld bc,3
        ex de,hl                                  ; HL = přípona, DE = konec názvu

        add hl,bc                                 ; HL -> třetí znak přípony od konce (např 'x' v ".nex")
        ld a,(de)
        cp (hl)
        ret nz

        dec hl
        dec de
        ld a,(de)
        cp (hl)
        ret nz

        dec hl
        dec de
        ld a,(de)
        cp (hl)
        ret nz

        dec hl
        dec de
        ld a,(de)
        cp (hl)
        ret


        ; ------------------------------------------------------------
        ; Přípony
        ; ------------------------------------------------------------
ext_nex defb ".nex"
ext_NEX defb ".NEX"

ext_tap defb ".tap"
ext_TAP defb ".TAP"

ext_z80 defb ".z80"
ext_Z80 defb ".Z80"

ext_snx defb ".snx"
ext_SNX defb ".SNX"

ext_sna defb ".sna"
ext_SNA defb ".SNA"

ext_bas defb ".bas"
ext_BAS defb ".BAS"


; ------------------------------------------------------------
; Buffery/příkazy pro ESXDOS/BASIC
; ------------------------------------------------------------
cmdload defb $ef                                  ; BASIC token LOAD (další bajty připravuje kód výše)

cmd3    defs 100                                  ; pomocný buffer

tapein  defb "run     "                           ; text pro příkaz (7 znaků + mezery)
cmd     defb "nexload "                           ; text pro příkaz (8 znaků včetně mezery)
cmd2    defs 100                                  ; hlavní buffer pro filename / command

enter_directory
        call dospage

        ld hl,actdisc
        call ROZHOD
        ld a,(hl)
        call $012d                                ; změna disku

        call basicpage
        ld b,11
        ld hl,TMP83
CCC                                               ; vynuluj všechny stavové bity v názvu (7.)
        res 7,(hl)
        inc hl
        djnz CCC
        ld hl,TMP83+10                            ; najdi poslední znak názvu souboru/adresare
chng2	ld a,(hl)
        cp 32
        jr nz,zap
        dec hl
        jr chng2
zap		ld a,255
        inc hl
        ld (hl),a
        call dospage
        xor a                                     ; change path
        ld hl,TMP83
AAAA
        call $01b1                                ; změň adresář

        call basicpage
        ld hl,ALLFILES
        call ROZHOD2
        xor a
        ld (hl),a
        inc hl
        ld (hl),a
        ld hl,POSKURZL
        call ROZHOD
        xor a
        ld (hl),a

        call reload_dir

        ld hl,pozicel
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a

        ld bc,38 * 256 + 27
        ld a,0
        call draw.window

        ld hl,pathl
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        ld de,3
        add hl,de
        ld a,(hl)
        cp 255
        jr z,root
        ld a,1
        ld (star+1),a
        jr rcont
root	xor a
        ld (star+1),a
rcont	ld hl,adrl
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a

        ld (adrs+1),hl

        ld hl,POSKURZL
        call ROZHOD
        xor a
        ld (hl),a

        ld hl,ALLPOSL
        call ROZHOD2
        xor a
        ld (hl),a
        inc hl
        ld (hl),a
star	ld hl,1
        call getroot

        ld hl,pathl
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        ld de,3
        add hl,de
        ld a,(hl)
        cp 255
        jr z,snula
        ld hl,1
        jr scont
snula	ld hl,0
scont

        ld hl,ALLPOSL
        call ROZHOD2
        ld (hl),0
        ld hl,STARTWINL
        call ROZHOD2
        push hl
        call getroot_reload
        pop de
        ex de,hl

        ld (hl),e
        inc hl
        ld (hl),d
        call showwin
        ld a,32
        call writecur
        call GETDIR
        call dospage
        call zapisCfg
        call basicpage
        jp loop0


getroot
        ld hl,pathl
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        ld de,3
        add hl,de
        ld a,(hl)
        cp 255
        jr z,mroot

        ld hl,STARTWINL
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        jr mmm0
mroot
        ld hl,STARTWINL
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        dec hl
mmm0
        ret

NUMBUF	defb "        ",0
NUM
        push hl
        ld hl,NUMBUF
        ld de,NUMBUF+1
        ld bc,5
        ld a,32
        ld (hl),a
        ldir
        ld hl,NUMBUF
        ld (numadr+1),hl
        pop hl
DECIMAL5 ld de,10000                              ; řád desetitisíců
         call DIGIT                               ; počet desetitisíců
DECIMAL4 ld de,1000                               ; řád tisíců
         call DIGIT                               ; a jeho počet
DECIMAL3 ld de,100                                ; řád stovek
         call DIGIT                               ; počet
DECIMAL2 ld de,10                                 ; desítky
         call DIGIT                               ; počet
DECIMAL1 ld de,1                                  ; jednotky
DIGIT 	 ld a,"0"-1                                ; do A kód znaku 0 bez jedné
DIGIT2 	 inc a                                    ; přičti jedničku
         or a                                     ; vynuluj CARRY Flag
         sbc hl,de                                ; pokusně odečti řád
         jr nc,DIGIT2                             ; pokud není výsledek záporný opakuj
         add hl,de                                ; přičti řád zpátky
         cp "9"+1                                 ; testuj znaky 0 až 9
         jr c,DIGIT3                              ; odskoč pokud platí
         add a,"A"-"9"-1                          ; oprava na A až F pro hexa čísla
DIGIT3   push 	hl
numadr	 ld hl,0
         ld (hl),a
         inc hl
         ld (numadr+1),hl
         pop hl
         ret

BUFF     defs   11
NUMB      ds 	11                                  ; temp pro vypis cisel
DEC32	 push iy
         ld c,32
         call D32B
DEC32SP  ld   de,NUMB
dec32pos ld   hl,1*256+1
decink	 ld a,0
         call  print
         pop iy
         ret

D32B     xor  a
         ld   iy,NUMB
         push de
         push bc
         ld   de,BUFF
         ld   b,10
DCC1     ld   (de),a
         inc  de
         djnz DCC1
         pop  bc
         pop  de
         push bc
         ld   b,$20
DCC2     add  hl,hl
         ex   de,hl
         adc  hl,hl
         ex   de,hl
         push bc
         push de
         ld   bc,$0A0A
         ld   de,BUFF
DCC3     ld   a,(de)
         adc  a,a
         cp   c
         jr   c,DCC4
         sub  c
DCC4     ld   (de),a
         ccf
         inc  de
         djnz DCC3
         pop  de
         pop  bc
         djnz DCC2
         ld   d,b
         pop  bc
         ld   e,b
         ld   hl,BUFF-1
         add  hl,de
         dec  b
         jr   z,DCC8
DCC5     ld   a,(hl)
         or   a
         jr   nz,DCC6
         ld   a,c
         db $11
DCC6     ld   c,'0'
DCC7     or   c
         dec  hl
         or   a
         jr   z,DCC9
         ld   (iy+0),a
         inc  iy
DCC9     djnz DCC5
DCC8     ld   a,(hl)
         or   '0'
         ld   (iy+0),a
         inc  iy
         ret

getroot_reload
        ld hl,pathl
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        ld de,3
        add hl,de
        ld a,(hl)
        cp 255
        jr z,amroot
        ld hl,1
        ret
amroot
        ld hl,0
        ret


DIRTMP  defb "C:",255

getdir
        ld hl,actdisc
        call ROZHOD
        ld a,(hl)
        ld (DIRTMP),a

        ld hl,pathl
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        ex de,hl
        push de
        ld hl,DIRTMP


        ld bc,4
        ldir

        call dospage

        pop hl
        ld a,1
        call $01b1
        call basicpage
        ret


pozicel	defw $01
pozice2 defw 40* 256 + 1

adrl	defw $4000+2
adrr 	defw $4000+2+80

PROHOD
        ld a,(OKNO)
        xor 16
        ld (OKNO),a

        call dospage
        ld hl,actdisc
        call ROZHOD
        ld a,(hl)
        call $012d
        call basicpage                            ; změn aktualni disk
        ret


changewin
CHANGEWIN

        ld a,0
        call writecur
        call PROHOD
        ld a,32
        call writecur

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
        jp loop0

bufscr equ $e000
; Uloží obrazovku do banky 19 ZX Next.
savescr
        nextreg $57,19                            ; Stránka na uložení VideoRam
        ld hl,16384
        ld de,bufscr
        ld bc,32*160
        ldir
        nextreg $57,1                             ; Nastránkuj zpátky
        ret



        ; Obnovení obrazovky z 19 stárnky ZX Next.
loadscr
        nextreg $57,19
        ld hl,bufscr
        ld de,16384
        ld bc,32*160
        ldir
        nextreg $57,1
        ret


downall	defw 0, 0
KL
klavesa	defb 0

leftpos	defw	0

; Test jestli je soubor označený nebo ne
; Z - není označený
; NZ - je označený
CHECKSEL
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
        add hl,de
        inc hl
        call BUFF83
        call find83
        ld hl,TMP83
        bit 7,(hl)
        ld a,(curcolor+1)
        jr z,neni_oznacen
        or a
        ld a,80
        ret z
        ld a,96
        ret


neni_oznacen
        or a
        ld a,0
        ret z
        ld a,32
        ret

        ; Nakreslí kurzor
writecur
        ld (curcolor+1),a
        call CHECKSEL
        ld (curcolor+1),a
        ld hl,KURZL
        call ROZHOD2

        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        inc hl
        push hl
        ld hl,POSKURZL
        call ROZHOD
        ld a,(hl)
        ld e,a
        ld d,160
        mul d,e

        pop hl
        add hl,de

curcolor ld a,32
        ld b,38
wr0		ld (hl),a
        inc hl
        inc hl
        djnz wr0
        ret

ROZHOD   ld   a,(OKNO)
         bit  4,a
         ret  z
         inc  hl
         ret

ROZHOD2  ld   a,(OKNO)
         bit  4,a
         ret  z
         inc  hl
         inc  hl
         ret







         ; Násobení HL x B
         ; Vysledek HL
mull		ld d,l                                      ; vynásob spodní byty
            ld e,b
            mul d,e                               ; vysledek je v de
            ex de,hl                              ; vysledek je v hl
            ld e,b                                ; násobitel do e
            mul d,e                               ; vynásob
            ld a,e                                ; do akumulátoru hoď výsledek (spodní byte)
            add a,h
            ld h,a                                ; konečný výsledek je v HL
            ret

            ; Najde podle pozice souboru nazev 8.3, se kterým dále pracujeme
            ; HL ... pozice
find83
        push hl
        ld hl,pathl
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a

        ld de,2
        add hl,de
        ld a,(hl)

        pop hl
        cp 255
        jr nz,find830
        dec hl
find830
        ld b,13
        call mull
        ld de,$a000
        add hl,de

        call BUFF83
        ld (foundfile),hl
        ld de,TMP83
        ld bc,13
        ldir
        call NOBUFF83
        ret

foundfile	defw 0
TMP83		ds 13

clickMouse

        xor a
        ld   (aLAST_KEY+1),a
        ret

INKEY 	call gettime
        call podbarviPodlePoziceMysky
        call zjistiJestliMyskaNeniVHorniCastiMenu
        call MOUSE
        ld b,a                                    ; uložit aktuální stav tlačítek

                                                  ; kontrola změny tlačítek oproti uloženému stavu
        ld a,(TLACITKO)
        xor b
        jr z,nemenTlacitka                               ; žádná změna → pokračuj

                                                  ; při změně uložíme OR stav
                                                  ; tlačítko zůstane "aktivní", dokud se ručně nevynuluje
        ld a,(CONTRB)
        or b
        ld (TLACITKO),a
nemenTlacitka        
        ld hl,(COORD)
        ld de,(lastCoordMouse)
        or a
        sbc hl,de
        jr z,nekresliMysku      ;pokud nebyl zadny pohyb nekresly kurzor mysky
        call showSprite
nekresliMysku
        ld hl,(COORD)
        ld (lastCoordMouse),hl

        ld a,(wheelOld)

        ld e,a
        call nactiWheelMysky
        xor e
        jr nz,clickMouse

        ld a,(TLACITKO)
        or a
        jr nz,clickMouse

        xor  a
        ld   (aLAST_KEY+1),a
        ei

        ld b,2
CEKEJ	halt
        djnz CEKEJ
ahl0
         call KEYSCAN

         ld   a,e
         inc  a
         jr   z,INKEY
         ld   a,d
         ld   hl,SYMTAB
         cp   $18
         jr   z,aHLSM2
         ld   hl,CAPSTAB
         cp   $27
         jr   z,aHLSM2
         ld   hl,NORMTAB
aHLSM2    ld   d,0
         add  hl,de
         ld   a,(hl)
         or   a
         jr   z,INKEY

aLAST_KEY ld   b,0
         cp   b
         jr   z,aSEDI_KEY

         ld   b,3
aLOOP_LST halt
         djnz aLOOP_LST
aSEDI_KEY
         ld   (aLAST_KEY+1),a
        push af
        call beepk
        pop af
        ret

        ; KeyScan od Busyho z MRSu
KEYSCAN  ld   l,47                                ; testovani klavesnice
         ld   de,65535
         ld   bc,65278
KEYLINE  in   a,(c)
         cpl
         and  31
         jr   z,KEYDONE
         ld   h,a
         ld   a,l
KEY3KEYS inc  d
         ret  nz
KEYBITS  sub  8
         srl  h
         jr   nc,KEYBITS
         ld   d,e
         ld   e,a
         jr   nz,KEY3KEYS
KEYDONE  dec  l
         rlc  b
         jr   c,KEYLINE
         ld   a,d
         inc  a
         ret  z
         cp   40
         ret  z
         cp   25
         ret  z
         ld   a,e
         ld   e,d
         ld   d,a
         cp   24
         ret
keysound db 0                                     ; key sound 0= yes,1= no, klavesnicove echo
SYMTAB   db "*^[&%>}/"
         db ",-]'$<{?"
         db ".+($"
         db 200
         db '/',' '
         db 0
         db "=;)@"
         db 201
         db "|:"
         db 32,13,34
         db "_!"
         db 199
         db "~",0

CAPSTAB  db "BHY"
         db 10,8
         db "TGV"
         db "NJU"
         db 11,5
         db "RFC"
         db "MKI"
         db 9,4
         db "EDX"
         db 2
         db "LO"
         db 15,6
         db "WSZ"
         db 1,13,"P"
         db 12,7
         db "QA"

NORMTAB  db "bhy65tgv"
         db "nju74rfc"
         db "mki83edx"
         db 0
         db "lo92wsz"
         db 32,13
         db "p01qa"
         db 0

beepk	ld a,(keysound)                             ; Busyho nahradni rutina,kratsi
        or a
        ret nz
        ld a,(BORDER)
        ld e,a
        ld b,$10
        add a,b
        ; ld a,$10+border
        out ($fe),a
        ld b,$1c
beepk1	djnz beepk1
        ld a,$08
        add a,e
        ; ld a,$08+border
        out ($fe),a
        ret
BORDER   db 1                                     ; okraj

offset	equ 3

; vstup:
; HL .... XY
; DE .... TEXT zakonceny 0 bytem
; A ..... atribut palety
print  ld (paleta + 1),a
        push de
        ld d,l
        ld e,160
        mul d,e
        ld a,h
        add a,a
        ld l,a
        ld h,0
        add hl,de
        ld de,$4000
        add hl,de
        pop de
print0
        ld a,(de)
        or a
        ret z
        ld (hl),a

paleta	ld a,0
        inc hl
        inc de
        ld (hl),a
        inc hl
        jr print0

window
        ld (atr1+1),a
        ld (atr2+1),a
        ld (atr3+1),a
        ld (atr4+1),a
        ld (atr5+1),a
        ld (atr6+1),a
        ld (atr7+1),a
        ld (atr8+1),a
        ld (atr9+1),a

        ld e,l
        ld d,160
        mul d,e
        ld a,h
        add a,a
        ld l,a
        ld h,0
        add hl,de
        ld de,#4000
        add hl,de                                 ; adresa v tilemode
window0	push hl

        ld a,18
        ld (hl),a
        inc hl
atr1	ld (hl),0
        ld a,b
        ld (w5+1),a
        ld a,16
        inc hl


w2		ld (hl),a
        inc hl
atr2	ld (hl),0
        inc hl
        djnz w2
        ld a,19
        ld (hl),a
        inc hl

atr3	ld (hl),0
        ld de,160-1
        add hl,de

        ld (w3+1),hl                              ; uloz adresu
        pop hl
        ld de,160
        add hl,de
        ld (w4+1),hl

w3		ld hl,0
        ld a,23
        ld (hl),a
        inc hl
atr4	ld (hl),0

        ld de,160-1
        add hl,de
        ld (w3+1),hl
w4		ld hl,0                                       ; leva cast
        ld a,22
        ld (hl),a
        inc hl
atr5	ld (hl),0
        push hl
        inc hl
        ld a,(w5+1)
        ld b,a
cisti	ld 	(hl),0
        inc hl
atr6	ld (hl),0
        inc hl
        djnz cisti

        pop hl
        ld de,160-1
        add hl,de
        ld (w4+1),hl
        dec c
        ld a,c
        or a
        jr nz,w3

        ld a,21
        ld (hl),a
        inc hl
atr7	ld (hl),0
w5 		ld b,0
        ld a,17
w6		inc hl
        ld (hl),a
        inc hl
atr8	ld (hl),0
        djnz w6
        inc hl
        ld a,20
        ld (hl),a
        inc hl
atr9	ld (hl),0
        ret


        module draw
window
UZZ
        ld (atr1+1),a
        ld (atr2+1),a
        ld (atr3+1),a
        ld (atr4+1),a
        ld (atr5+1),a
        ld (atr6+1),a
        ld (atr7+1),a
        ld (atr8+1),a
        ld (atr9+1),a
        ld (atr200+1),a


        ld e,l
        ld d,160
        mul d,e
        ld a,h
        add a,a
        ld l,a
        ld h,0
        add hl,de
        ld de,#4000
        add hl,de                                 ; adresa v tilemode
window0	push hl

        ld a,18
        ld (hl),a
        inc hl
atr1	ld (hl),0
        ld a,b

        ld (w5+1),a

        rra                                       ; vyděl dvěma
        ld b,12                                   ; odečti 10
        or a
        sbc a,b
        ld b,a
        ld (w20+1),a

        ld a,16
        inc hl


w2		ld (hl),a
        inc hl
atr2	ld (hl),0
        inc hl
        djnz w2

        ld a,"["
        ld (hl),a
        inc hl
        ld a,(atr200+1)
        ld (hl),a
        inc hl


        ld b,21
writmes
        ld a," "
        ld (hl),a
        inc hl
        ld a,(atr200+1)
        ld (hl),a
        inc hl
        djnz writmes
        ld a,"]"
        ld (hl),a
        inc hl
        ld a,(atr200+1)
        ld (hl),a
        inc hl

        ld a,16
        ld (hl),a
        inc hl
        ld a,(atr200+1)
        ld (hl),a
        inc hl
        ld a,16
w20		ld b,0

w2000	ld (hl),a
        inc hl
atr200	ld (hl),0
        inc hl
        djnz w2000



        ld a,19
        ld (hl),a
        inc hl

atr3	ld (hl),0
        ld de,160-1
        add hl,de

        ld (w3+1),hl                              ; uloz adresu
        pop hl
        ld de,160
        add hl,de
        ld (w4+1),hl

w3		ld hl,0
        ld a,23
        ld (hl),a
        inc hl
atr4	ld (hl),0

        ld de,160-1
        add hl,de
        ld (w3+1),hl
w4		ld hl,0                                       ; leva cast
        ld a,22
        ld (hl),a
        inc hl
atr5	ld (hl),0
        push hl
        inc hl
        ld a,(w5+1)
        ld b,a
cisti	ld 	(hl),0
        inc hl
atr6	ld (hl),0
        inc hl
        djnz cisti

        pop hl
        ld de,160-1
        add hl,de
        ld (w4+1),hl
        dec c
        ld a,c
        or a
        jr nz,w3

        ld a,21
        ld (hl),a
        inc hl
atr7	ld (hl),0
w5 		ld b,0
        ld a,17
w6		inc hl
        ld (hl),a
        inc hl
atr8	ld (hl),0
        djnz w6
        inc hl
        ld a,20
        ld (hl),a
        inc hl
atr9	ld (hl),0
        ret

        endmodule

        ; ------------------------------------------------------------
        ; reload_dir
        ; ------------------------------------------------------------
        ; Načte katalog aktuálního disku/adresáře do RAM bufferu catbuff,
        ; aktualizuje počty souborů (ALLFILES) a připraví pomocné struktury:
        ; - vynuluje počty (ALLFILES, numsel, virtmem)
        ; - vyčistí pracovní oblast $A000 (pravděpodobně pro 8.3 cache / TMP83 práci)
        ; - zavolá DOS katalog rutinu (dos_catalog) opakovaně (po blocích) a skládá výsledky
        ; - po dokončení:
        ; * uloží návratovou hodnotu do dosret
        ; * nastaví STARTWIN dle root/non-root (path +3 == 255)
        ; * zavolá getAllLFN (naplnění LFN bufferů) a getdir (aktualizace cesty)
        ; * uloží downall (nějaký “all/scroll” údaj pro aktivní okno)
        ; ------------------------------------------------------------
reload_dir

        di                                        ; během práce s bankováním/ROM (dále se přepíná dospage/basicpage)

        ld hl,catbuff
        ld (Count11+1),hl                         ; self-modify: Count11 bude ukazovat na catbuff (pro CountMemory)

                                                  ; ALLFILES (word) pro aktivní okno = 0
        ld hl,ALLFILES
        call ROZHOD2
        xor a
        ld (hl),a
        inc hl
        ld (hl),a

        ; numsel (word) pro aktivní okno = 0
        ld hl,numsel
        call ROZHOD2
        xor a
        ld (hl),a
        inc hl
        ld (hl),a

        ; virtmem = 0 (počítá, kolik “virtuálních” bloků katalogu už bylo nataženo)
        ld (virtmem),a

        ; Přepni stránku dat pro aktivní okno (NextReg $55) a vyčisti oblast $A000..$A400
        call BUFF83
        ld hl,#a000
        ld de,#a001
        ld bc,1024
        xor a
        ld (hl),a
        ldir

        call dospage                              ; přepnout na “DOS stránku” (ROM/RAM mapping přes port #7FFD)


                                                  ; --------------------------------------------------------
                                                  ; Příprava catbuff pro katalog
                                                  ; --------------------------------------------------------
        ld hl,catbuff                             ; buffer pro DOS katalog
        ld de,catbuff+1
        ld bc,1024                                ; poznámka v kódu: 64*13+13=845 pro +3DOS, tady se alokuje 1024
        ld (hl),0
        ldir                                      ; vyčistí buffer (aspoň první položka bude 0)

        ld de,catbuff                             ; DE = kam DOS rutiny zapisují položky

aNextDirItem
        ld b,pocetpolozek                         ; max počet položek v jednom volání dos_catalog (konstanta / equ jinde)
        ld c,%101                                 ; flagy pro dos_catalog (komentář: “include system files”)
        ld hl,stardstar                           ; "*.*" + $FF terminátor
        call dos_catalog                          ; externí (ROM/DOS): naplní katalog do bufferu (DE), vrací stav v registrech


                                                  ; ------------------------------------------------------------
                                                  ; NEXT0: zpracování výsledku dos_catalog
                                                  ; ------------------------------------------------------------
NEXT0
        ld (savehl),hl                            ; ukládá registry vrácené DOSem (použije se v getAllLFN)
        ld (saveix),ix

        ld a,b
        cp pocetpolozek                           ; pokud B == pocetpolozek, buffer byl plný (=> možná další “page” katalogu)
        push af

        push hl
        push de
        push bc

        ; Aktualizace ALLFILES? + (B-1) ? – tady se počítá “celkový počet položek”
        ; Pozor: přesnou sémantiku B (kolik položek DOS vrátil) nechávám bez spekulace,
        ; ale je zřejmé, že se z něj odvozuje index do ALLFILES a zároveň dirNum.
        ld hl,ALLFILES
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a                                    ; HL = ALLFILES (word)

        ld e,b
        ld d,0
        add hl,de
        dec hl
        push hl

        ; ALLFILESL je tabulka dvou pointerů (pro L/R okno) – ukládá se do ní HL
        ld hl,ALLFILESL
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        ld (N0+1),hl                              ; self-modify: kam se uloží hodnota

        pop hl
N0      ld (0),hl                                 ; self-modify store: *(ALLFILESL[okno]) = HL

                                                  ; dirNum = dirNum + B - 1
        ld hl,(dirNum)
        add hl,de
        dec hl
        ld (dirNum),hl

        pop bc
        pop de
        pop hl
        pop af

        jr c,acont                                ; pokud B == pocetpolozek (carry po CP), skonči smyčku (nebo přepni režim) – dle logiky níže

        ld a,b
        or a
        jr z,acont                                ; pokud B == 0, nic nepřišlo => konec

        ld a,(virtmem)
        cp 2
        jr z,acont                                ; limit virtmem na 2 (další stránky se už neberou)

        call CountMemory                          ; posune Count11+1 na konec posledního bloku v catbuff
        ex de,hl                                  ; DE = ukazatel na další místo (podle CountMemory)
        ld hl,virtmem
        inc (hl)                                  ; virtmem++

        jr aNextDirItem                           ; načti další blok katalogu

acont
        push af

        ; zde se pracuje s dirNum a “dosret” pro NextBASIC (původní kód stylově)
        ld hl,(dirNum)

        pop hl
        ld (dosret),hl                            ; uloží návratovou hodnotu (viditelné z NextBASIC dle komentáře v pův. kódu)

        ld c,b                                    ; BC = počet souborů? (low byte = B)
        ld b,0

        di                                        ; bude se přepínat přes #7FFD

        push bc
        call basicpage                            ; návrat na “základní stránku” (ZXOS/NextBASIC mapování)
        pop bc
        dec bc                                    ; úprava počtu (pravděpodobně korekce o 1 položku – bez spekulace proč)

        call BUFF83                               ; přepni datovou stránku pro okno zpět (NextReg $55)

                                                  ; Přeskočí první položku (catbuff+13), pak načítá LFN pro všechny položky
        ld hl,catbuff+13
askon
        call getAllLFN                            ; interní: prochází catbuff a pro každou položku ukládá LFN do “paged bufferu”
        call getdir                               ; interní/external mix: aktualizace cesty (nemám celý include, neodhaduju)

                                                  ; downall = ALLFILES-1 (pro aktivní okno)
        ld hl,(ALLFILES)
        dec hl

        push hl
        ld hl,downall
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        pop de
        ex de,hl
        ld (hl),e
        inc hl
        ld (hl),d

        ; root detekce: path + 3 == 255
        ld hl,pathl
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        ld de,3
        add hl,de
        ld a,(hl)
        cp 255
        jr z,jetoroot
        ld a,1
        jr nenitoroot
jetoroot
        xor a
nenitoroot
        ld (pocatek+1),a                          ; self-modify: operand v “ld (hl),0” níže bude 0 nebo 1

                                                  ; STARTWIN (word) pro aktivní okno nastav podle root/non-root
        ld hl,STARTWINL
        call ROZHOD2
pocatek ld (hl),0                                 ; self-modify: nastav STARTWIN low byte
        call NOBUFF83                             ; vrátí stránkování NextReg $55 do defaultu

        ret


        ; ------------------------------------------------------------
        ; dospage / basicpage
        ; ------------------------------------------------------------
        ; Přepínání bankování přes port #7FFD (bankm je “shadow” poslední hodnoty).
        ; - dospage: reset bit4, OR 7  (mapování pro DOS rutiny)
        ; - basicpage: set bit4, AND #F8 (mapování pro “základ”/ZXOS)
        ; ------------------------------------------------------------
dospage
        ld bc,port1
        ld a,(bankm)
        res 4,a
        or 7
        ld (bankm),a
        out (c),a
        ret

basicpage                                         ; nastránkuje základní stránku ZXOS
        ld bc,port1
        ld a,(bankm)
        set 4,a
        and #F8
        ld (bankm),a
        out (c),a
        ret


        ; ------------------------------------------------------------
        ; getAllLFN
        ; ------------------------------------------------------------
        ; Projde položky v catbuff (13B záznamy) a pro každou položku:
        ; - zavolá DOS funkci $01B7 pro získání LFN (long file name)
        ; - uloží LFN + metadata (délka, datum, čas – ukládá se do LFNNAME+261..)
        ; - zkopíruje maxlen bajtů do “paged” paměti na stránkách (NextReg $57), začátek #E000
        ; - při zaplnění stránky posune Page+1 a resetuje InBuff na #E000
        ; ------------------------------------------------------------
DETT
discdetail      defs 30

lfnpage defb 24,60                                ; stránky (pro L/R okno) – ROZHOD vybírá byte

getAllLFN
        ld hl,0
        ld (numLoop),hl                           ; počítadlo zpracovaných položek

        ld hl,#e000
        ld (InBuff+1),hl                          ; self-modify: kam v aktuální stránce ukládat

                                                  ; zvol stránku pro LFN buffer podle aktivního okna
        ld hl,lfnpage
        call ROZHOD
        ld a,(hl)
        ld (Page+1),a                             ; self-modify: nextreg $57,Page

                                                  ; načti první 13B záznam z catbuff+0x0D do bufftmp (startovní položka)
        call BUFF83
        ld hl,catbuff+#d
        ld de,bufftmp
        ld bc,13
        ldir
        call NOBUFF83

        ld hl,catbuff+#d
        ld (tmpname),hl                           ; ukazatel na aktuální 13B záznam v catbuff

                                                  ; BC = ALLFILES (word) pro aktivní okno; pokud 0 => nic nedělej
        ld hl,ALLFILES
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld b,(hl)
        ld c,a
        or b
        ret z

LFN1
        push bc

        ; numLoop++ (pouze statistika/ladění)
        ld hl,(numLoop)
        inc hl
        ld (numLoop),hl

lfnpos  ld de,18560+24                            ; pozice ve VRAM (nejspíš ladění / nepoužité v tomto úryvku)

        di                                        ; při DOS volání musí být správně nastránkovaná ROM

        call dospage

BFN
BufferName
        ld de,bufftmp                             ; 8.3 jméno (z catbuff) pro dotaz na LFN
        ld hl,stardstar                           ; (vstupní parametr DOSu – přesný význam bez spekulace)
        ld ix,(savehl)                            ; DOS “context” z předchozího katalogu
        ld bc,LFNNAME                             ; buffer pro výstup LFN
        call $01b7                                ; externí: zjisti LFN

                                                  ; uloží “metadata” vedle LFNNAME (offsety 261..)
        ld (LFNNAME + 261),ix                     ; délka? / pomocný návrat z DOSu (bez spekulace přesného významu)
        ld (LFNNAME + 261 + 2),hl
        ld (LFNNAME + 261 + 4),bc                 ; datum
        ld (LFNNAME + 261 + 6),de                 ; čas

        call basicpage

Page    ld a,24
        nextreg $57,a                             ; nastránkuj stránku pro uložení LFN bloků

InBuff  ld de,#e000                               ; self-modify: cílová adresa v aktuální stránce
        ld hl,LFNNAME                             ; zdroj: LFN buffer
        ld bc,maxlen                              ; kolik bajtů ukládat z LFNNAME
        ldir

        ; posuň se na další 13B položku v catbuff a zkopíruj ji do bufftmp
        call BUFF83
        ld hl,(tmpname)
        ld de,13
        add hl,de
        ld (tmpname),hl

        ld de,bufftmp
        ld bc,13
        ldir
        call NOBUFF83

        ; obnov stránkování $57 a posuň InBuff o maxlen
        ld a,(Page+1)
        nextreg $57,a

        ld hl,(InBuff+1)
        ld de,maxlen
        add hl,de
        ld (InBuff+1),hl

        ; kontrola, jestli se InBuff nevejde (porovnání s #FFFF-261)
        ld de,#FFFF-261
        or a
        sbc hl,de
        jr c,contin                               ; pokud je ještě místo, pokračuj

                                                  ; přeteklo: další stránka a reset InBuff na #E000
        ld hl,Page+1
        inc (hl)
        ld hl,#e000
        ld (InBuff+1),hl

contin
        pop bc
        dec bc
        ld a,b
        or c
        jp nz,LFN1

AAA
        nextreg $57,1                             ; vrať stránku 1 (programová data)
        ret


        ; ------------------------------------------------------------
        ; deleno8 / deleno
        ; ------------------------------------------------------------
        ; Pomocné dělení:
        ; - deleno8: D/E -> D=quotient, A=remainder (8 bit)
        ; - deleno : HL/C -> HL=quotient, A=remainder (16 bit)
        ; ------------------------------------------------------------
        ; Input: D = Dividend, E = Divisor, A = 0
        ; Output: D = Quotient, A = Remainder
deleno8
        xor a
        ld b,8
de8     sla d
        rla
        cp e
        jr c,$+4
        sub e
        inc d
        djnz de8
        ret

        ; Input: HL = Dividend, C = Divisor, A = 0
        ; Output: HL = Quotient, A = Remainder
deleno
        xor a
        ld b,16
de
        add hl,hl
        rla
        cp c
        jr c,de1
        sub c
        inc l
de1     djnz de
        ret


addrlfn dw 0


; ------------------------------------------------------------
; BUFF83 / NOBUFF83
; ------------------------------------------------------------
; Přepíná NextReg $55 na stránku dat podle aktivního okna (buffl/buffr),
; a vrací zpět na default (5).
; ------------------------------------------------------------
setspace
        ld (hl),32
        ret

buffl   defb 20
buffr   defb 22

BUFF83
        push hl
        push de
        push af
        ld hl,buffl
        call ROZHOD
        ld a,(hl)
        nextreg $55,a                             ; nastránkuj data pro správné okno
        pop af
        pop de
        pop hl
        ret

NOBUFF83
        nextreg $55,5
        ret


        ; ------------------------------------------------------------
        ; CountMemory
        ; ------------------------------------------------------------
        ; Posune “Count11+1” (self-modify ukazatel) na poslední položku v catbuff bloku:
        ; hl = catbuff + 13*(pocetpolozek-1)
        ; Používá se při iterativním načítání katalogu po blocích.
        ; ------------------------------------------------------------
CountMemory
        ld de,13*(pocetpolozek-1)
Count11  ld hl,catbuff                            ; self-modify: base adresa se průběžně posouvá
        add hl,de
        ld (Count11+1),hl
        ret


changedrivetxt defb "Select drive:",0
selecttxt     defb "ENTER = select",0


; ------------------------------------------------------------
; enterdrv
; ------------------------------------------------------------
; Potvrzení vybraného disku (posdrv) pro aktivní okno:
; - přepne disk přes $012D
; - uloží písmeno do actdisc (L/R dle OKNO)
; - aktualizuje dir a znovu načte katalog + překreslení okna
; ------------------------------------------------------------
enterdrv
        call dospage

HNH
        ; A = listdisc[posdrv]
        ld a,(posdrv)
        ld e,a
        ld d,0
        ld hl,listdisc
        add hl,de
        ld a,(hl)
        push af

        call $012d                                ; externí: změna disku

                                                  ; načti ukazatel path pro aktivní okno (bez dalšího použití v tomto úryvku)
        ld hl,pathl
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a

        ; actdisc (L/R) = vybraný disk
        ld hl,actdisc
        call ROZHOD
        pop af
        ld (hl),a

        call basicpage
        call getdir                               ; aktualizace cesty (externí/include)

                                                  ; reset ALLFILES a POSKURZ pro aktivní okno
        ld hl,ALLFILES
        call ROZHOD2
        xor a
        ld (hl),a
        inc hl
        ld (hl),a

        ld hl,POSKURZL
        call ROZHOD
        xor a
        ld (hl),a

RLD     call reload_dir

        ; vykresli rám okna
        ld hl,pozicel
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a

        ld bc,38 * 256 + 27
        ld a,0
        call window

        ; root detekce podle path+3 (255) a připrav “astar” self-modify pro getroot/showwin
        ld hl,pathl
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        ld de,3
        add hl,de
        ld a,(hl)
        cp 255
        jr z,aroot
        ld a,1
        ld (astar+1),a
        jr arcont
aroot   xor a
        ld (astar+1),a
arcont
        ; nastav adrs+1 pro showwin podle panelu
        ld hl,adrl
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        ld (adrs+1),hl

astar   ld hl,1                                   ; self-modify 0/1

                                                  ; pokud ALLFILES==0, návrat do loop0
        push hl
        ld hl,ALLFILES
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        or h
        pop hl
        jp z,loop0

        ; vykresli listing + kurzor
        call showwin                              ; externí
        ld a,32
        call writecur                             ; interní

                                                  ; reset ALLPOS/STARTWIN podle root
        ld hl,pathl
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        ld de,3
        add hl,de
        ld a,(hl)
        cp 255
        jr z,asnula
        ld hl,1
        jr ascont
asnula  ld hl,0
ascont

        ld hl,ALLPOSL
        call ROZHOD2
        ld (hl),0

        ld hl,STARTWINL
        call ROZHOD2
        push hl
        call getroot_reload                       ; interní
        pop de
        ex de,hl

        ld (hl),e
        inc hl
        ld (hl),d

        ; redraw + další UI
        call showwin
        ld a,32
        call writecur
        call GETDIR                               ; externí/include
        call zobraz_nadpis                        ; externí/include
        jp loop0


ramdisc defb " (ramdisc)",0
image   defb " (disc image)  ",0
sdcard  defb " (SD card)",0


; ------------------------------------------------------------
; writecurdrv
; ------------------------------------------------------------
; Vykreslí “kurzor” v menu výběru disku podle posdrv.
; - cíl (kkam+1) se volí podle aktivního panelu
; - barva/atribut se bere z A (chngcol+1 self-modify)
; ------------------------------------------------------------
writecurdrv
        push af

        ; podle lftw (L/R?) zvol základní adresu v obrazovce
        ld hl,lftw
        call ROZHOD
        ld a,(hl)
        or a
        jr z,levy_panel

        ld hl,$4000 + 160*8 + 17 + 78
        ld (kkam +1),hl
        jr pravy_panel

levy_panel
        ld hl,$4000 + 160*8 + 15
        ld (kkam +1),hl

pravy_panel
        pop af
        ld (chngcol+1),a                          ; self-modify: barva/atribut kurzoru

                                                  ; posuň se o (posdrv * 160) řádků
        ld a,(posdrv)
        ld e,a
        ld d,160
        mul d,e

kkam    ld hl,0                                   ; self-modify: base adresa
        add hl,de

        ; vykresli 15 znaků “atributu” (krok po 2 B)
        ld b,15
wrcurdrv
chngcol ld (hl),64
        inc hl
        inc hl
        djnz wrcurdrv

        ret


        ; ------------------------------------------------------------
        ; chng_save
        ; ------------------------------------------------------------
        ; Uloží změnu atributů souboru (R/O, SYS, ARCH) – z TMP83+7..:
        ; - sestaví masku v E podle stavů v TMP83
        ; - přepíše extension (TMP83+8..+10) do záznamu v katalogu (foundfile+8)
        ; - vyčistí bit7 v názvu (TMP83[0..10])
        ; - nastaví terminátor 255 a zavolá ROM rutinu 0148h (externí) pro zápis atributů
        ; ------------------------------------------------------------
chng_save
        call dospage

        ld d,00000111b
        ld e,0
        ld ix,TMP83+7

        bit 7,(ix+1)
        call z,clr_ro
        bit 7,(ix+2)
        call z,clr_sys
        bit 7,(ix+3)
        call z,clr_arch

        push de

        ; přepiš 3B extension v katalogovém záznamu (foundfile+8)
        ld hl,(foundfile)
        call BUFF83
        ld de,8
        add hl,de
        ex de,hl
        ld hl,TMP83+8
        ld bc,3
        ldir
        call NOBUFF83

        ; očisti bit7 u názvu (prvních 11 znaků)
        ld hl,TMP83
        ld b,11
c_save
        res 7,(hl)
        inc hl
        djnz c_save

        ; terminátor pro DOS/ROM rutiny pracující s 8.3
        ld hl,TMP83
        ld a,255
        ld (TMP83+11),a

        pop de
        call 0148h                                ; externí: změna atributů (přesný význam registrů neodhadovat)

        call basicpage
        call loadscr
        jp loop0

clr_ro
        set 2,e
        ret
clr_sys
        set 1,e
        ret
clr_arch
        set 0,e
        ret


        ; ------------------------------------------------------------
        ; zapisCfg
        ; ------------------------------------------------------------
        ; Zapíše konfigurační soubor cc.cfg do c:/sys
        ; - přepne cestu na pathCfg
        ; - create file (0106h)
        ; - write blok Cfg o délce DelkaCfg (115h)
        ; - close (0109h)
        ; - vrátí cestu zpět dle pathl aktivního okna
        ; Pozn.: konkrétní ESXDOS/ROM API význam 0106h/0112h/0115h beru jako externí.
        ; ------------------------------------------------------------
zapisCfg
        ld hl,pathCfg
        xor a
        call $01b1                                ; externí: nastavení cesty

        ld b,1
        ld c,3                                    ; exclusive WRITE
        ld d,1
        ld e,1
        ld hl,nameCfg
        call 0106h                                ; externí: create file

        ld c,PAGE_BUFF
        ld b,1
        ld de,DelkaCfg
        ld hl,Cfg
        call 115h                                 ; externí: WRITE

        ld b,1
        call $0109                                ; externí: close

                                                  ; vrať cestu podle aktuálního panelu
        ld hl,pathl
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a

        xor a
        call $01b1
        ret


        ; ------------------------------------------------------------
        ; createCfg
        ; ------------------------------------------------------------
        ; Zajistí existenci cc.cfg v c:/sys:
        ; - vytvoří soubor (0106h)
        ; - pokud vytvoření uspěje -> zapíše default Cfg
        ; - pokud soubor už existuje -> načte ho do Cfg (0112h)
        ; - zavře soubor a končí
        ; ------------------------------------------------------------
createCfg
        call dospage

        ld hl,pathCfg
        xor a
        call $01b1                                ; externí: nastavení cesty

        ld b,1
        ld c,3
        ld d,1
        ld e,1
        ld hl,nameCfg
        call 0106h                                ; externí: create file

        jr nz,NactiKonfiguraci                    ; pokud create “nevyšlo” (NZ), jde se číst existující (bez spekulace přesné podmínky)

VytvorKonfiguraci
        ld c,PAGE_BUFF
        ld b,1
        ld de,DelkaCfg
        ld hl,Cfg
        call 115h                                 ; externí: WRITE

zavriSoubor
        ld b,1
        call $0109                                ; externí: close
        ret

NactiKonfiguraci
        ld b,1
        ld c,PAGE_BUFF
        ld de,DelkaCfg
        ld hl,Cfg
        call 0112h                                ; externí: READ

        jr zavriSoubor


        ; ------------------------------------------------------------
        ; Data / proměnné používané výše
        ; ------------------------------------------------------------
pathCfg  defb "c:/sys",255
nameCfg  defb "cc.cfg",255
posdrv   defb 0

ALLFILESL defw ALLFILES, ALLFILES2                ; tabulka pointerů na “ALLFILES word” pro levý/pravý panel

name     defs 60

ALLFILES  defw 0
ALLFILES2 defw 0
ALLFILESR defw 0

LFNNAME   defs 275                                ; buffer pro LFN + metadata (používá se i offset 261..)
LFNNAME2  defs 275                                ; pomocný buffer (porovnávání jinde)
tmpname   ds 2
bufftmp   ds 15

M_P3DOS   equ $94
savehl    defw 0
saveix    defw 0

bankm     defb 0                                  ; “shadow” poslední hodnoty posílané na #7FFD

stardstar:
        defb "*.*",#FF

dosret:
        defw 0

numLoop  defw 0
FILES    defb 0
dirNum   defw 0

        include "functions/texts.asm"
        include "functions/rename.asm"
        include "functions/getdir.asm"
        include "functions/delete.asm"
        include "functions/file.asm"

FILEBUFF



; *****************************************************************************************
; *****************************************************************************************
; *****************************************************************************************
; *****************************************************************************************
; *****************************************************************************************
; *****************************************************************************************
; *****************************************************************************************
; *****************************************************************************************
; *****************************************************************************************
; *****************************************************************************************
; *****************************************************************************************
; *****************************************************************************************
; *****************************************************************************************
; *****************************************************************************************
; *****************************************************************************************
; *****************************************************************************************
; *****************************************************************************************
; *****************************************************************************************
; *****************************************************************************************
; *****************************************************************************************
; *****************************************************************************************
; *****************************************************************************************
; *****************************************************************************************
; *****************************************************************************************

E1

        org $a000
S3
        include "functions/copy.asm"
        include "functions/createdir.asm"
        include "kmouse/driver.a80"
        include "kmouse/akce.a80"
        include "functions/compare.asm"
        include "functions/menu.asm"
        include "functions/input.asm"
tilemapFont_char24:
        include "tilemap_font_8x6.i.asm"
E3
        org 49152
S2
oknoVyber	defb	64,32
            defb	100,96
vyberPocitace
        ld hl,30 * 256 + 5
        ld bc,20 * 256 + 15
        ld a,16
        call window

        ld hl,32*256+6
        ld a,16
        ld de,vyberTxt
        call print

        ld hl,32*256+8
        ld a,16
        ld de,txt128
        call print

        ld hl,32*256+9
        ld a,16
        ld de,txt48
        call print

        ld hl,32*256+10
        ld a,16
        ld de,Pentagontxt
        call print

        ld hl,32*256+11
        ld a,16
        ld de,NextTxt
        call print
        ld a,64
        call kreslicurcomp

comp00
        xor a
        ld (TLACITKO),a
        call InkeyNoWait
        cp 10
        jp z,compdown

        cp 11
        jp z,compup
        cp 1
        jr z,cancelComp
        cp 13
        jp z,entercomp

        ld hl,oknoVyber
        call CONTROL
        jr nc,vOkne

        jp comp00

vOkne

        ld a,(COORD+1)
        ld d,a
        ld e,8
        call deleno8
        ld a,-8
        add a,d
        push af
        ld d,a


        ld a,16
        call kreslicurcomp

        pop af
        ld (cursorComp),a
        ld a,64
        call kreslicurcomp

        ld a,(TLACITKO)
        bit 1,a
        jr nz,entercomp
        jr comp00

v0
        di
        halt
        jr v0
        ret
entercomp
        ld a,(cursorComp)
        ld e,a
        ld d,0
        ld hl,tabComp
        add hl,de
        ld a,(hl)

        nextreg TURBO_CONTROL_NR_07,0
        nextreg MACHINE_TYPE_NR_03,a
        ld a,(cursorComp)
        cp 2
        jr z,pentagon
        NEXTREG2A $8
        res 6,a
        nextreg $8,a
        ret

pentagon
        NEXTREG2A $8
        set 6,a
        nextreg $8,a

        ret

tabComp
        defb  $A0                                 ; 128k/2
        defb  $90                                 ; 48k
        defb  $C0                                 ; Pentagon
        defb  $B0                                 ; +2A/+3/Next

cancelComp
        pop hl
        pop hl
        call loadscr

        jp loop0
compup
        ld a,(cursorComp)
        cp 0
        jp z,comp00

        ld a,16
        call kreslicurcomp
        ld a,(cursorComp)
        dec a
        ld (cursorComp),a
        ld a,64
        call kreslicurcomp

        jp comp00

compdown
        ld a,(cursorComp)
        cp 3
        jp z,comp00

        ld a,16
        call kreslicurcomp
        ld a,(cursorComp)
        inc a
        ld (cursorComp),a
        ld a,64
        call kreslicurcomp

        jp comp00



kreslicurcomp
        ld (clrComp+1),a
        ld a,(cursorComp)
        ld e,a
        ld d,160
        mul d,e
        ld hl,(adr_cur)
        add hl,de
clrComp ld a,32
        ld b,18
wr00	ld (hl),a
        inc hl
        inc hl
        djnz wr00
        ret
adr_cur defw $4002+160*8 + 63                     ; adresa prvni polozky ve vyberu



vyberTxt defb "Select computer:",0
txt128	defb "128k/+2",0
txt48	defb	"48k",0
Pentagontxt	defb	"Pentagon",0
NextTxt		defb "+2A/+3/Next",0

tilemapFont:    ds   16*32

ConvertRomCharTo4bpp:
        push    bc
        ld      bc,$08FF
.lineLoop:
        ld      a,(hl)
        inc     hl
        push    hl
        call    .convert8pixels
        pop     hl
        djnz    .lineLoop
        pop     bc
        ret
.convert8pixels:
        call    .convert4pixels
.convert4pixels:
        call    .convert2pixels
.convert2pixels:
        rlca
        rlca
        push    af
        and     3
        ld      hl,.pixelTable
        add     hl,a
        ldi
        pop     af
        ret
.pixelTable:
       DB      $00, $03, $30, $33

VSE_NASTAV
        ld a,3
        ld (OKNO),a
        nextreg MMU3_6000_NR_53,5*2+1
        ld      hl,TILE_GFX_ADR
        ld      de,TILE_GFX_ADR+1
        ld      bc,16*32-1
        ld      (hl),0
        ldir
        ld hl,$6000
        ld de,$6001
        ld bc,32*16
        ld (hl),l
        ldir
        ld		de,$6000 + 32*32

        ; convert ROM font to 4bpp tiles by code
        ld      hl,MEM_ROM_CHARS_3C00 + 32*8
        ld      b,128-' '
.RomCharsLoop
        call    ConvertRomCharTo4bpp
        djnz    .RomCharsLoop

        ld hl,specialchar
        ld de,$6000+32*16
        ld bc,specialchar2-specialchar

        ldir

        nextreg TURBO_CONTROL_NR_07,3             ; 28Mhz mode
        nextreg SPRITE_CONTROL_NR_15,%01100011    ; layer priority: USL
        nextreg TRANSPARENCY_FALLBACK_COL_NR_4A,0 ; black transparency fallback color
        nextreg TILEMAP_TRANSPARENCY_I_NR_4C,$0F
        nextreg ULA_CONTROL_NR_68,$80             ; disable ULA layer
        nextreg DISPLAY_CONTROL_NR_69,0           ; layer2 off, bank 5, timex=0
        nextreg TILEMAP_CONTROL_NR_6B,%1100'0011    ; 80x32x2, 4bpp, pal0, 512tile-mode, force tile-over-ULA
        nextreg TILEMAP_DEFAULT_ATTR_NR_6C,$00    ; no pal offset, no mirror/rot, 0 bit8
        nextreg TILEMAP_BASE_ADR_NR_6E,high TILE_MAP_ADR
        nextreg TILEMAP_GFX_ADR_NR_6F,high TILE_GFX_ADR

        nextreg CLIP_TILEMAP_NR_1B,0
        nextreg CLIP_TILEMAP_NR_1B,159
        nextreg CLIP_TILEMAP_NR_1B,0
        nextreg CLIP_TILEMAP_NR_1B,255
        nextreg TILEMAP_XOFFSET_MSB_NR_2F,0
        nextreg TILEMAP_XOFFSET_LSB_NR_30,0
        nextreg TILEMAP_YOFFSET_NR_31,0


        ; set tilemap palette
        nextreg PALETTE_CONTROL_NR_43,%0'011'0000 ; tilemap pal0
        nextreg PALETTE_INDEX_NR_40,0

        ld      hl,tilemapPalette
        ld      bc,tilemapPalette_SZ
.setPalLoop:
        ld      a,(hl)
        inc     hl
        nextreg PALETTE_VALUE_9BIT_NR_44,a

        ld a,b
        or c
        dec bc
        jr nz,.setPalLoop
        ld hl,$4000
        ld de,$4001
        ld bc,80*32*2
        ld (hl),0
        ldir

        ld hl,nadpis
        ld de,#4000
        ld bc,80
        ret

tilemapPalette:
                db  %000'000'11,1                 ; 0 modra(paper)					0
                db  %100'100'10,1                 ; 1 light grey (25% ink)
                db  %010'010'01,1                 ; 2 dark grey (75% ink)
                db  %101'101'11,0                 ; 0 white-blueish (ink)
                db  %110'001'00,1                 ; 4 red
                db  %111'110'00,1                 ; 5 yellow
                db  %000'100'00,0                 ; 6 green
                ds 18
                db  %000'000'00,0                 ; 0 modra (paper)					16
                db  %100'100'10,1                 ; 1 light grey (25% ink)
                db  %010'010'01,1                 ; 2 dark grey (75% ink)
                db  %101'101'11,0                 ; 0 white-blueish (ink)
                db  %110'001'00,1                 ; 4 red
                db  %111'110'00,1                 ; 5 yellow
                db  %000'110'00,0                 ; 6 green
                ds 18
                db  %000'011'10,1                 ; 0 modra (paper)					32
                db  %100'100'10,1                 ; 1 light grey (25% ink)
                db  %010'010'01,1                 ; 2 dark grey (75% ink)
                db  %101'101'11,0                 ; 0 white-blueish (ink)
                db  %110'001'00,1                 ; 4 red
                db  %111'110'00,1                 ; 5 yellow
                db  %000'100'00,0                 ; 6 green
                ds 18
                db  %111'111'11,1                 ; 0 modra (paper)					48
                db  %100'100'10,1                 ; 1 light grey (25% ink)
                db  %010'010'01,1                 ; 2 dark grey (75% ink)
                db  %000'000'000,0                ; 0 white-blueish (ink)
                db  %110'001'00,1                 ; 4 red
                db  %111'110'00,1                 ; 5 yellow
                db  %000'100'00,0                 ; 6 green
                ds 18
                db  %011'101'00,1                 ; 0 zluta (paper)					64
                db  %100'100'10,1                 ; 1 light grey (25% ink)
                db  %010'010'01,1                 ; 2 dark grey (75% ink)
                db  %000'000'00,0                 ; 0 white-blueish (ink)
                db  %110'001'00,1                 ; 4 red
                db  %111'110'00,1                 ; 5 yellow
                db  %000'100'00,0                 ; 6 green
                ds 18
                db  %100'100'10,0                 ; 0 zluta (paper)					80
                db  %100'100'10,1                 ; 1 light grey (25% ink)
                db  %010'010'01,1                 ; 2 dark grey (75% ink)
                db  %000'000'00,0                 ; 0 white-blueish (ink)
                db  %110'001'00,1                 ; 4 red
                db  %111'110'00,1                 ; 5 yellow
                db  %000'100'00,0                 ; 6 green
                ds 18
                db  %101'101'10,1                 ; 0 zluta (paper)					96
                db  %100'100'10,1                 ; 1 light grey (25% ink)
                db  %010'010'01,1                 ; 2 dark grey (75% ink)
                db  %000'000'00,0                 ; 0 white-blueish (ink)
                db  %110'001'00,1                 ; 4 red
                db  %111'110'00,1                 ; 5 yellow
                db  %000'100'00,0                 ; 6 green
                ds 18

                db  %000'000'11,1                 ; 0 modra(paper)					112 - barva adresare
                db  %100'100'10,1                 ; 1 light grey (25% ink)
                db  %010'010'01,1                 ; 2 dark grey (75% ink)
                db  %111'111'00,0                 ; 0 white-blueish (ink)
                db  %110'001'00,1                 ; 4 red
                db  %111'110'00,1                 ; 5 yellow
                db  %000'100'00,0                 ; 6 green
                ds 18

                db  %000'000'11,1                 ; 0 modra(paper)					128 - spustitelné soubory
                db  %100'111'00,1                 ; 1 light grey (25% ink)
                db  %010'010'01,1                 ; 2 dark grey (75% ink)
                db  %000'111'00,1                 ; 0 white-blueish (ink)
                db  %110'001'00,1                 ; 4 red
                db  %111'110'00,1                 ; 5 yellow
                db  %000'100'00,0                 ; 6 green
                ds 18

                db  %111'000'00,0                 ; 0 modra (paper)					144 - 
                db  %100'111'00,1                 ; 1 light grey (25% ink)
                db  %010'010'01,1                 ; 2 dark grey (75% ink)
                db  %101'101'11,0                 ; 0 white-blueish (ink)
                db  %110'001'00,1                 ; 4 red
                db  %111'110'00,1                 ; 5 yellow
                db  %000'110'00,0                 ; 6 green
                ds 18


tilemapPalette_SZ:  EQU $ - tilemapPalette

lftw	defb 0
        defb 1




changedrive
        call savescr
        call NOBUFF83
        ld hl,lftw
        call ROZHOD
        ld a,(hl)
        or a
        jr nz,chngright
        ld hl, 4 * 256 + 5
        ld bc,30 * 256 + 17
        ld a,16
        call window

        ld hl,5*256+6
        ld a,16
        ld de,changedrivetxt
        call print

        ld hl,20*256+22
        ld a,16
        ld de,selecttxt
        call print
        ld hl,24*256+21
        ld a,16
        ld de,notxt
        call print

        ld a,(pocetdisku)
        ld b,a
        ld de,listdisc
        exx
        ld hl,discdetail
        exx
        ld hl,$4000 + 160*8 + 14
        jr chngdrv0
chngright
        ld hl,44 * 256 + 5
        ld bc,30 * 256 + 17
        ld a,16
        call window

        ld hl,45*256+6
        ld a,16
        ld de,changedrivetxt
        call print

        ld hl,60*256+22
        ld a,16
        ld de,selecttxt
        call print
        ld hl,64*256+21
        ld a,16
        ld de,notxt
        call print

        ld a,(pocetdisku)
        ld b,a
        ld de,listdisc
        exx
        ld hl,discdetail
        exx
        ld hl,$4000 + 160*8 + 16 + 78

chngdrv0
        ld a,(de)
        ld (hl),a
        inc hl
        ld (hl),16
        inc hl
        ld (hl),":"
        inc hl
        ld (hl),16
        inc hl
        push hl

        exx
        ld a,(hl)
        inc hl
        exx
        cp 4
        pop hl
        push hl
        push de
        push af
        call z,showramdisc
        pop af
        push af
        cp 255
        call z,showimagedisc
        pop af
        cp 5
        call z,showsdcard
        cp 6
        call z,showsdcard
        pop de
        pop hl
        push de
        ld de,160-4
        add hl,de
        pop de
        inc de
        djnz chngdrv0
        ld a,64
        call writecurdrv
chng0
        xor a
        ld (TLACITKO),a
        call INKEY
        cp 10
        jr z,curchngdown

        cp 11
        jr z,curchngup
        cp 1
        jr z,cancel
        cp 13
        jp z,enterdrv
        jp chng0
cancel	call loadscr
        call zobraz_nadpis
        jp loop0
curchngup
        ld a,(posdrv)
        cp 0
        jp z,chng0

        ld a,16
        call writecurdrv
        ld a,(posdrv)
        dec a
        ld (posdrv),a
        ld a,64
        call writecurdrv

        jp chng0


curchngdown
        ld a,(posdrv)
        inc a
        ld hl,pocetdisku
        cp (hl)
        jr z,chng0

        ld a,16
        call writecurdrv
        ld a,(posdrv)
        inc a
        ld (posdrv),a
        ld a,64
        call writecurdrv
        jp chng0

showramdisc
        ld de,ramdisc
        call showtyp
        xor a
        ret
showimagedisc
        ld de,image
        call showtyp
        xor a
        ret

showsdcard
        ld de,sdcard
        call showtyp
        xor a
        ret

showtyp
        ld a,(de)
        or a
        ret z
shwtyp0	ld (hl),a
        inc hl
        ld (hl),16
        inc hl
        inc de
        jr showtyp


down
        ld hl,ALLFILES
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        or h
        jp z,loop0



        ld hl,POSKURZL
        call ROZHOD
        ld a,(hl)
        cp 26
        jr z,down0
        push af

        ld hl,STARTWINL
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        pop af
        ld e,a
        ld d,0
        add hl,de
        push hl
        ld hl,ALLFILES

        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        dec hl
        pop de
        or a
        sbc hl,de
        jp z,loop0

        ld a,0
        call writecur

        ld hl,POSKURZL
        call ROZHOD

        inc (hl)
        ld hl,ALLPOSL
        call ROZHOD2
        push hl
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        pop de
        ex de,hl
        inc de
        ld (hl),e
        inc hl
        ld (hl),d


        ld hl,adrl
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        ld (adrs+1),hl

        call getroot

        call showwin
        ld a,32
        call writecur
        jp loop0

down0
        push af

        ld hl,STARTWINL
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        pop af
        ld e,a
        ld d,0
        add hl,de
        push hl
        ld hl,ALLFILES

        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        dec hl
        pop de
        or a
        sbc hl,de
        jp z,loop0

        ld hl,ALLPOSL
        call ROZHOD2
        push hl
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a

        push hl
        ld hl,downall
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        pop de
        ex de,hl

        or a
        sbc hl,de
        add hl,de
        pop de
        jp z,loop0

        ex de,hl
        inc de
        ld (hl),e
        inc hl
        ld (hl),d

        ld hl,STARTWINL
        call ROZHOD2
        push hl
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        pop de

        ex de,hl
        inc de
        ld (hl),e
        inc hl
        ld (hl),d

        ld hl,adrl
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        ld (adrs+1),hl

        call getroot

        call showwin
        ld a,32
        call writecur
        jp loop0


up
        ld hl,ALLFILES
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        or h
        jp z,loop0

        ld hl,POSKURZL
        call ROZHOD
        ld a,(hl)
        or a
        jr z,up0

        ld a,0
        call writecur
        ld hl,POSKURZL
        call ROZHOD
        dec (hl)


        ld hl,ALLPOSL
        call ROZHOD2
        push hl
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        pop de
        ex de,hl
        dec de
        ld (hl),e
        inc hl
        ld (hl),d


        ld hl,adrl
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        ld (adrs+1),hl

        call getroot


        call showwin
        ld a,32
        call writecur
        jp loop0

up0
UP0

        ld hl,pathl
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        ld de,3
        add hl,de
        ld a,(hl)
        cp 255
        jr z,upzero
        ld a,1
        ld (kolik+1),a
        jr opcont
upzero	xor a
        ld (kolik+1),a

opcont
        ld hl,STARTWINL
        call ROZHOD2
        push hl
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
kolik	ld de,0
        or a
        ld (usavehl+1),hl
        sbc hl,de
        ; add hl,de
        pop de
        ld a,l
        or h
        jp z,loop0
usavehl	ld hl,0
        ; or a
        ; sbc hl,bc
        ex de,hl
        dec de
        ld (hl),e
        inc hl
        ld (hl),d


        ld hl,adrl
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        ld (adrs+1),hl

        call getroot

        call showwin
        ld a,32
        call writecur


        jp loop0

        ; znova vykresli okna
showwin
        ld hl,ALLFILES
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        or h
        ret z


        ld hl,STARTWINL
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a

        ld a,2
        ld (ypos+1),a                             ; vynuluj Y pozici

        push hl
        ld hl,ALLFILES
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld b,(hl)
        ld c,a

        ld h,b
        ld l,c
        push hl

        ld hl,pathl
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a

        ld de,3
        add hl,de
        ld a,(hl)
        cp 255
        jr z,sw0
        dec hl
        ld a,(hl)

        cp 255
        jr z,ssw0
        ld hl,1

        dec bc
        jr sw0
ssw0	pop de
        pop hl
        dec hl
        push hl
        push de
sw0

        pop hl
        ld de,28
        or a
        sbc hl,de
        jr c,SSS
        ld bc,27


SSS		pop hl
showloop
        push hl
        push bc

        inc hl

        call find83
        call BUFF83


        ld hl,(foundfile)
        bit 7,(hl)                                ; testuj jestli je soubor označený
        jr z,nonselect
        ld a,80
        ld (inkcolor+1),a
        jr isselect
nonselect
        ld a,0
        ld (inkcolor+1),a
isselect
        ld hl,(TMP83+11)
        ld (velikost+1),hl
        pop bc
        pop hl
        push hl
        push bc

        push hl
        push af

        ; obarveni adresaru
        ld hl,TMP83 +7
        bit 7,(hl)
        jr z,neni_to_adresar
        ld a,(inkcolor+1)
        cp 80
        jr z,neni_to_adresar
        ld a,112
        ld (inkcolor+1),a


neni_to_adresar

        pop af
        pop hl

XXXX	call FINDLFN
        call basicpage
        ld hl,LFNNAME + 230
NajdiPrvniNemezeru
        ld a,(hl)
        cp $20
        jr nz,TUU
        dec hl
        jr NajdiPrvniNemezeru

TUU		inc hl
        xor a
        ld (hl),a
        ld hl,LFNNAME
        ld de,ext_tap
        call pripony
        jp z,obarvi_spustitelny_soubor

        ld hl,LFNNAME
        ld de,ext_TAP
        call pripony
        jp z,obarvi_spustitelny_soubor

        ld hl,LFNNAME
        ld de,ext_nex
        call pripony
        jp z,obarvi_spustitelny_soubor

        ld hl,LFNNAME
        ld de,ext_NEX
        call pripony
        jp z,obarvi_spustitelny_soubor

        ld hl,LFNNAME
        ld de,ext_sna
        call pripony
        jp z,obarvi_spustitelny_soubor

        ld hl,LFNNAME
        ld de,ext_SNA
        call pripony
        jp z,obarvi_spustitelny_soubor

        ld hl,LFNNAME
        ld de,ext_snx
        call pripony
        jp z,obarvi_spustitelny_soubor

        ld hl,LFNNAME
        ld de,ext_SNX
        call pripony
        jp z,obarvi_spustitelny_soubor

        ld hl,LFNNAME
        ld de,ext_z80
        call pripony
        jp z,obarvi_spustitelny_soubor

        ld hl,LFNNAME
        ld de,ext_Z80
        call pripony
        jp z,obarvi_spustitelny_soubor

        ld hl,LFNNAME
        ld de,ext_bas
        call pripony
        jp z,obarvi_spustitelny_soubor

        ld hl,LFNNAME
        ld de,ext_BAS
        call pripony
        jp z,obarvi_spustitelny_soubor


        jr SSSS
obarvi_spustitelny_soubor

        ld hl,(foundfile)
        bit 7,(hl)                                ; testuj jestli je soubor označený
        jr nz,SSSS

        ld a,128
        ld (inkcolor+1),a
SSSS

ypos	ld e,2
        ld d,80 * 2
        mul d,e
adrs	ld hl,$4000 + 2

        add hl,de
        ex de,hl
        ld hl,LFNNAME

        ld bc,31
shw0	ld a,(hl)
        cp 255
        jr z,shw01
        ld (de),a
        inc de
        inc hl
inkcolor ld a,0
        ld (de),a
        inc de
        dec bc
        ld a,b
        or c
        jr nz,shw0

shw01
        ld hl,TMP83 +7
        bit 7,(hl)
        jr z,shw20

dir		ld hl,dirext
        ld bc,5+2
shw1	ld a,(hl)

        ld (de),a
        inc de
        inc hl
        ld a,(inkcolor+1)
        ld (de),a
        inc de
        dec bc
        ld a,b
        or c
        jr nz,shw1
        jr shw3


shw20
velikost ld hl,0


        push de
        call NUM
        ld hl,NUMBUF

        ld b,5
vel1	ld a,(hl)
        cp "0"
        jr nz,vel2
        ld (hl)," "
        inc hl
        djnz vel1
vel2	pop de
        ld hl,NUMBUF
shw2	ld b,5
sh		ld a,(hl)
        ld (de),a
        ld a,(inkcolor+1)
        inc de
        ld (de),a
        inc de
        inc hl
        djnz sh

        ld a,"k"
        ld (de),a
        inc de
        ld a,(inkcolor+1)
        ld (de),a
        inc de
        ld a,"B"
        ld (de),a
        inc de
        ld a,(inkcolor+1)
        ld (de),a
shw3	pop bc
        pop hl
        inc hl

        ld a,(ypos+1)
        inc a
        ld (ypos+1),a
        dec bc
        ld a,b
        or c
        jp nz,showloop

        call NOBUFF83
        ret


leftcur
        ld hl,ALLFILES
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        or h
        jp z,loop0
        ld hl,POSKURZL
        call ROZHOD
        ld (smcur+1),hl
        ld a,(hl)
        or a
        jr z,leftcur0
        ld a,0

        push hl
        call writecur
        pop hl
        xor a
        ld (hl),a
        ld a,32
        call writecur
        jp loop0


leftcur0
LFT0
        ld hl,0
        push hl
        ld hl,pathl
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        ld de,3
        add hl,de
        ld a,(hl)
        cp 255
        pop hl
        jr z,leftcur1
        inc hl
leftcur1
        ld (leftcur_porovnej + 1),hl


        ld hl,POSKURZL
        call ROZHOD
        ld a,(hl)
        ld l,a
        ld h,0

        push hl
        ld hl,STARTWINL
        call ROZHOD2
        ld (pocatekleft+1),hl
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a

        ex de,hl
        pop hl
        add hl,de
        inc hl                                    ; aktualni pozice v HL
        ld (aktpos+1),hl

leftcur_porovnej
        ld de,0
        or a
        sbc hl,de
        add hl,de
        jp z,loop0                                ; odskoc pokud jsi na prvni pozici
        ld de,28
        or a
        sbc hl,de
        add hl,de
        jr c,mensi_nez_28
        ld de,28
aktpos	ld hl,0
        or a
        sbc hl,de
        ex de,hl
        jr pocatekleft
mensi_nez_28

        ld de,(leftcur_porovnej + 1)
pocatekleft
        ld hl,0
        ld (hl),e
        inc hl
        ld (hl),d
        ; vykresli znova okno
        ld hl,adrl
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        ld (adrs+1),hl

        call getroot

        call showwin
        ld a,32
        call writecur
        jp loop0

RGHT
rightcur
        ld hl,ALLFILES
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        or h
        jp z,loop0
        ld hl,POSKURZL
        call ROZHOD
        ld (smcur+1),hl
        ld a,(hl)
        cp 26
        jp z,rightcur0                            ; zobraz další stránku

        ld hl,ALLFILES
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld d,(hl)
        ld e,a
        dec de
        push de
        ld hl,pathl
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        ld de,3
        add hl,de
        ld a,(hl)
        cp 255
        pop de
        jr z,aasw0

        dec de
aasw0
        ld hl,26
        or a
        sbc hl,de
        add hl,de
        jr c,posledniradek
        ld a,e
        ld (kamcur+1),a
        jr smcur
posledniradek
        ld a,26
        ld (kamcur+1),a
smcur	ld hl,0
        ld a,0
        call writecur
krcur	ld hl,(smcur+1)
kamcur	ld (hl),26
        ld a,32
        call writecur

        jp loop0

rightcur0

        ld hl,STARTWINL
        call ROZHOD2
        ld (rightsedi+1),hl                       ; ulož adresu okna, které se bude vykreslovat
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        ld (AKT+1),hl
        inc hl

        ld de,27 +27
        add hl,de
        push hl
        ; HL ... číslo souboru na kterém stojí kurzor + 26 (stránka)
        ld hl,ALLFILES
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld d,(hl)
        ld e,a
        ld (MAXR+1),de
        dec de
        dec de
        pop hl
        ; DE ... počet všech souborů v aktuálním okně

        or a
        ex de,hl
        sbc hl,de
        add hl,de

        jr c,right12                              ; když se nesmí odstránkovat celých 26 souborů
MAXR	ld hl,0
AKT		ld hl,0
        ld de,27
        or a
        add hl,de

        add hl,de
        push hl

        ld hl,pathl
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        ld de,3
        add hl,de
        ld a,(hl)
        cp 255
        pop hl
        jr z,asw0

        dec hl
asw0

        ; dec hl
        ld de,27
        or a
        sbc hl,de
        jr	rightsedi
right12 ld hl,(MAXR+1)
        ld de,27
        or a
        sbc hl,de

rightsedi
          ld (0),hl


          ; vykresli znova okno
        ld hl,adrl
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        ld (adrs+1),hl
SED
        call getroot

        call showwin
        ld a,32
        call writecur
        jp loop0

        ; Označí soubor dle HL
OZNA
oznac_soubor_dle_pozice_v_hl
        call BUFF83
        call find83
        call BUFF83
        ld hl,(foundfile)
        push hl
        ld de,ban1
        ld a,0
        call specific_search
        pop hl
        jr z,odeselect_file
        push hl
        ld de,ban2
        ld a,0
        call specific_search
        pop hl
        jr z,odeselect_file
        bit 7,(hl)
        jr z,oselect_file
        res 7,(hl)

        ld hl,numsel
        call ROZHOD2
        push hl
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        pop de
        dec hl
        ex de,hl
        ld (hl),e
        inc hl
        ld (hl),d

        jr odeselect_file

oselect_file
        set 7,(hl)                                ; označ soubor
        ld hl,numsel
        call ROZHOD2
        push hl
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        pop de
        inc hl
        ex de,hl
        ld (hl),e
        inc hl
        ld (hl),d

odeselect_file
        call NOBUFF83
        ret

numsel	defw 0,0
seltxt defb "Selected: ",0
SES
select
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
        add hl,de
        inc hl                                    ; v HL máme číslo souboru
        call BUFF83
        call find83
        call BUFF83
        ld hl,(foundfile)
        push hl
        ld de,ban1
        ld a,0
        call specific_search
        pop hl
        jp z,down
        push hl
        ld de,ban2
        ld a,0
        call specific_search
        pop hl
        jp z,down
        bit 7,(hl)
        jr z,select_file
        res 7,(hl)

        ld hl,numsel
        call ROZHOD2
        push hl
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        pop de
        dec hl
        ex de,hl
        ld (hl),e
        inc hl
        ld (hl),d

        jr deselect_file
select_file
        set 7,(hl)                                ; označ soubor
        ld hl,numsel
        call ROZHOD2
        push hl
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        pop de
        inc hl
        ex de,hl
        ld (hl),e
        inc hl
        ld (hl),d

deselect_file

selcont
        ld hl,adrl
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        ld (adrs+1),hl

        call getroot

        call showwin
        ld a,32
        call writecur
        ld a,(rightMouse)
        or a
        jp z,down
        xor a
        ld (rightMouse),a
        jp loop0

rightMouse      db 0

quittxt	defb "You want realy quit from Calm Commander?",0
emul    defb "Sorry, you use emulator... ;) Reset not works.",0

quit
        call savescr
        ld hl,10 * 256 + 10
        ld bc,60 * 256 + 5
        ld a,16
        call window

        ld hl,11*256+11
        ld a,16
        ld de,quittxt
        call print

        ld hl,60*256+15
        ld a,48
        ld de,yestxt
        call print

        ld hl,60*256+14
        ld a,16
        ld de,notxt
        call print

quit0
        xor a
        ld (TLACITKO),a
        call INKEY
        cp 1
        jp z,infoend
        cp 13
        jp z,softreset
        jp quit0

softreset
        nextreg 2,1

        ld hl,10 * 256 + 10
        ld bc,60 * 256 + 5
        ld a,16
        call window

        ld hl,11*256+11
        ld a,16
        ld de,emul
        call print

        ld hl,11*256+15
        ld a,16
        ld de,pressanykeytxt
        call print

        xor a
        ld (TLACITKO),a
        call INKEY
        call loadscr
        jp loop0


CHNG_ATTR
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
        add hl,de
        push hl
        inc hl
        call BUFF83
        call find83
        pop hl
        call FINDLFN

        call BUFF83
        ld hl,(foundfile)
        ld de,ban1
        ld a,0
        call specific_search
        jp z,loop0
        ld hl,(foundfile)
        ld de,ban2
        ld a,0
        call specific_search
        jp z,loop0


        call savescr

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
        add hl,de
        push hl
        inc hl
        call BUFF83
        call find83
        pop hl
        call FINDLFN
        ld hl,8 * 256 + 10
        ld bc,60 * 256 + 10
        ld a,16
        call window

        ld hl,11*256+11
        ld a,16
        ld de,attr_nadpis
        call print

        ld hl,11*256+13
        ld a,16
        ld de,readonlytxt
        call print

        ld hl,11*256+14
        ld a,16
        ld de,systemfiletxt
        call print

        ld hl,11*256+15
        ld a,16
        ld de,archivedtxt
        call print

        xor a
        ld hl,LFNNAME+44
        ld (hl),a
        ld hl,24*256+17
        ld a,16
        ld de,LFNNAME
        call print

        ld hl,11*256+17
        ld a,16
        ld de,namefile
        call print

        ld hl,55*256+20
        ld a,48
        ld de,savetxt
        call print

        ld hl,55*256+19
        ld a,16
        ld de,notxt
        call print
        ; vyhodnocení

chng00	call showattr
        xor a
        ld (TLACITKO),a
        call INKEY
        cp 1
        jr z,chng_end
        cp "r"
        jp z,switch_ro
        cp "s"
        jp z,switch_sys
        cp "a"
        jp z,switch_arch
        cp 13
        jp z,chng_save
        jr chng00
chng_end
        call loadscr
        jp loop0


switch_sys
        ld ix,TMP83+7
        bit 7,(ix+2)
        jr z,set_sys
        res 7,(ix+2)
        jp chng00
set_sys  set 7,(ix+2)
        jp chng00

switch_arch
        ld ix,TMP83+7
        bit 7,(ix+3)
        jp z,set_arch
        res 7,(ix+3)
        jp chng00
set_arch set 7,(ix+3)
        jp chng00

switch_ro
        ld ix,TMP83+7
        bit 7,(ix+1)
        jr z,set_ro
        res 7,(ix+1)
        jp chng00
set_ro  set 7,(ix+1)
        jp chng00

showattr
        ld ix,TMP83+7

        bit 7,(ix+1)
        jr z,ro_null
        ld a,27
        jr readonly
ro_null	ld a,25

readonly
        ld hl,$4000 + 160 * 13 + 50
        ld (hl),a
        inc hl
        ld a,16
        ld (hl),a

        bit 7,(ix+2)
        jr z,sys_null
        ld a,27
        jr system
sys_null	ld a,25

system
        ld hl,$4000 + 160 * 14 + 50
        ld (hl),a
        inc hl
        ld a,16
        ld (hl),a


        bit 7,(ix+3)
        jr z,arch_null
        ld a,27
        jr archive
arch_null	ld a,25

archive
        ld hl,$4000 + 160 * 15 + 50
        ld (hl),a
        inc hl
        ld a,16
        ld (hl),a
        ret

showattr_info
        ld ix,TMP83+7

        bit 7,(ix+1)
        jr z,ro_nulla
        ld a,27
        jr readonlya
ro_nulla	ld a,25

readonlya
        ld hl,$4000 + 160 * 17 + 128
        ld (hl),a
        inc hl
        ld a,16
        ld (hl),a

        bit 7,(ix+2)
        jr z,sys_nulla
        ld a,27
        jr systema
sys_nulla	ld a,25

systema
        ld hl,$4000 + 160 * 18 + 128
        ld (hl),a
        inc hl
        ld a,16
        ld (hl),a


        bit 7,(ix+3)
        jr z,arch_nulla
        ld a,27
        jr archivea
arch_nulla	ld a,25

archivea
        ld hl,$4000 + 160 * 19 + 128
        ld (hl),a
        inc hl
        ld a,16
        ld (hl),a
        ret

namefile	defb "Name of file:",0
attr_nadpis	defb "Edit file attribute",0
readonlytxt	defb "[R]ead only",0
systemfiletxt defb "[S]ystem file",0
archivedtxt defb "[A]rchived",0

readonlytxt2	defb "Read only",0
systemfiletxt2 defb "System file",0
archivedtxt2 defb "Archived",0

; ============================================================
; info_file
; Zobrazí dialog s informacemi o právě označeném souboru:
;  - název (LFNNAME, zkrácený pro zobrazení)
;  - datum/čas (MS-DOS formát uložený v metadatech LFNNAME+261+4/+6)
;  - velikost (32-bit z LFNNAME+261..)
;  - atributy (read-only/system/archive) přes showattr_info
;
; Na začátku navíc blokuje “ban1/ban2” soubory (porovnání foundfile vs ban texty).
;
; Pozn.: V kódu je hodně opakování “získej aktuální záznam → BUFF83/find83 → FINDLFN”.
; ============================================================
info_file
        ; ---- HL = index aktuální položky (kurzor v levém panelu)
        ld hl,POSKURZL
        call ROZHOD
        ld a,(hl)                                  ; A = index položky v okně
        ld l,a
        ld h,0                                     ; HL = index (16-bit)

        ; ---- HL = pointer na položku v seznamu: STARTWINL + index
        push hl
        ld hl,STARTWINL
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a                                     ; HL = base pointer seznamu
        ex de,hl                                   ; DE = base
        pop hl                                     ; HL = index
        add hl,de                                  ; HL = base + index

        ; ---- načti 8.3/LFN data pro danou položku
        push hl
        inc hl
        call BUFF83                                ; typicky připraví buffer/kontext pro find83
        call find83                                ; načte 8.3 entry do TMP83 (a/nebo foundfile)
        pop hl
        call FINDLFN                               ; načte dlouhé jméno do LFNNAME (+ metadata)

        ; ---- ochrana: některé soubory se nesmí “info” zobrazit (ban list)
        call BUFF83                                ; zřejmě nastaví (foundfile) na aktuální jméno
        ld hl,(foundfile)
        ld de,ban1
        ld a,0
        call specific_search
        jp z,loop0                                 ; pokud shoda s ban1 → nic nezobrazuj

        ld hl,(foundfile)
        ld de,ban2
        ld a,0
        call specific_search
        jp z,loop0                                 ; pokud shoda s ban2 → nic nezobrazuj

        ; ----------------------------------------------------
        ; Uložit obrazovku, protože budeme kreslit dialog
        ; ----------------------------------------------------
        call savescr

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
        add hl,de
        push hl
        inc hl
        call BUFF83
        call find83
        pop hl
        call FINDLFN

        ; ---- vykresli okno dialogu
        ld hl,8 * 256 + 10                         ; pozice dialogu
        ld bc,60 * 256 + 10                        ; rozměry
        ld a,16                                    ; atribut okna
        call window

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
        add hl,de
        push hl
        inc hl
        call BUFF83
        call find83
        pop hl
        call FINDLFN

        ; (tady jsou zbytkové LD hl/bc/a bez CALL window – historický pozůstatek?)
        ld hl,8 * 256 + 10
        ld bc,60 * 256 + 10
        ld a,16


        ; ---- Nadpis dialogu
        ld hl,11*256+11
        ld a,16
        ld de,fileinfonadpis
        call print

        ; ---- Zkrácení LFNNAME pro výpis (nulový terminátor na pozici +44)
        xor a
        ld hl,LFNNAME+44
        ld (hl),a                                  ; ukonči string, aby byl max 44 znaků

        ; ---- Vypiš název souboru
        ld hl,25*256+13
        ld a,16
        ld de,LFNNAME
        call print

        ; ---- Popisek "Name:" (nebo podobně; namefile je text)
        ld hl,11*256+13
        ld a,16
        ld de,namefile
        call print

        ; ---- Popisky pro datum/čas
        ld de,filedate
        ld hl,11*256+15
        ld a,16
        call print

        ld de,filetime
        ld hl,11*256+16
        ld a,16
        call print

        ; ---- Vypiš datum a čas z metadat (MS-DOS formát)
        ; datum: (LFNNAME+261+4)  čas: (LFNNAME+261+6)
        ld de,(LFNNAME+261+4)
        ld hl,17*256+15
        call showdate

        ld hl,17*256+16
        ld de,(LFNNAME+261+6)
        call showtime

        ; ---- Sekce atributů (System/Readonly/Archive) – samotné hodnoty řeší showattr_info
        ld hl,51*256+15
        ld a,16
        ld de,sysatrtxt
        call print

        ld hl,51*256+17
        ld a,16
        ld de,readonlytxt2
        call print

        ld hl,51*256+18
        ld a,16
        ld de,systemfiletxt2
        call print

        ld hl,51*256+19
        ld a,16
        ld de,archivedtxt2
        call print

        ; ---- Velikost
        ld hl,11*256+18
        ld a,16
        ld de,sizetxt
        call print

        ; DEC32: vytiskne 32-bit číslo (velikost) na pozici HL (přes self-mod dec32pos)
        ld hl,16*256+18
        ld (dec32pos+1),hl                          ; kam DEC32 vypíše
        ld hl,(LFNNAME+261)                          ; low word size
        ld de,(LFNNAME+261+2)                        ; high word size
        ld b,10                                     ; počet číslic (nebo šířka pole)
        ld a,16                                     ; ink/atribut pro výpis
        ld (decink+1),a
        call DEC32
        ld a,0
        ld (decink+1),a                             ; vrať default ink

        ld hl,27*256+18
        ld a,16
        ld de,bytestxt
        call print

        ; ---- nápověda “press any key”
        ld hl,11*256+20
        ld a,16
        ld de,pressanykeytxt
        call print

        ; ---- vykresli konkrétní hodnoty atributů (read-only/system/archive) podle TMP83/LFN metadata
        call showattr_info

        ; ---- čekej na klávesu a vrať obrazovku
        xor a
        ld (TLACITKO),a
        call INKEY
        call loadscr
        jp loop0


; ============================================================
; Texty
; ============================================================
sizetxt  defb "Size:",0
bytestxt defb "bytes",0


; ============================================================
; showdate
; Vypíše datum z MS-DOS formátu (DE) na pozici HL.
; MS-DOS date (16-bit):
;   bits 0-4   = day   (1-31)
;   bits 5-8   = month (1-12)
;   bits 9-15  = year  (od 1980)
;
; Používá self-mod adresy shdt1..shdt5 pro konkrétní pozice tisku:
;  dd.mm.yyyy
; ============================================================
; DE - datum (MSDOS)
; HL - pozice (X*256+Y)
showdate
        ; uloží pozice pro jednotlivé části na základě HL
        ld (shdt1+1),hl                            ; pozice dne
        inc h
        inc h
        ld (shdt2+1),hl                            ; pozice první tečky
        inc h
        ld (shdt3+1),hl                            ; pozice měsíce
        inc h
        inc h
        ld (shdt4+1),hl                            ; pozice druhé tečky
        inc h
        ld (shdt5+1),hl                            ; pozice roku

        ; ---- day = E & 31
        ld a,e
        and 31
        push de

        ld l,a
        ld h,0
        call NUM                                   ; převede HL na ASCII do NUMBUF

shdt1   ld hl,17*256+15                            ; (self-mod) skutečná cílová pozice dne
        ld a,16
        ld de,NUMBUF+3                             ; typicky poslední 2 číslice
        call print

shdt2   ld hl,19*256+15                            ; (self-mod) tečka
        ld a,16
        ld de,tecka
        call print

        pop de

        ; ---- month = (DE >> 5) & 15
        ; poskládá z D/E posunem doprava (5 bitů)
        ld a,e
        ld b,d
        srl b
        push bc
        rra
        rra
        rra
        rra
        rra
        and 15

        ld l,a
        ld h,0
        call NUM

shdt3   ld hl,20*256+15
        ld a,16
        ld de,NUMBUF+3
        call print

shdt4   ld hl,22*256+15
        ld a,16
        ld de,tecka
        call print

        ; ---- year = 1980 + (DE >> 9)
        pop af                                      ; A = horní část po posunech (rok offset)
        ld l,a
        ld h,0
        ld de,1980
        add hl,de
        call NUM
        call smaznuly                               ; odstraní vedoucí nuly (nahradí mezerou)

shdt5   ld hl,23*256+15
        ld a,16
        ld de,NUMBUF+1                             ; rok typicky 4 znaky
        call print
        ret


; ============================================================
; showtime
; Vypíše čas z MS-DOS formátu (DE) na pozici HL.
; MS-DOS time (16-bit):
;   bits 0-4   = seconds/2  (0-29)  -> *2 = 0..58
;   bits 5-10  = minutes    (0-59)
;   bits 11-15 = hours      (0-23)
;
; Vypíše hh:mm:ss
; ============================================================
; DE - čas ve formátu MSDOS
; HL - pozice
showtime
        ; pozice pro jednotlivé části (self-mod shtm1..shtm5)
        ld (shtm1+1),hl                            ; hodiny
        inc h
        inc h
        ld (shtm2+1),hl                            ; dvojtečka
        inc h
        ld (shtm3+1),hl                            ; minuty
        inc h
        inc h
        ld (shtm4+1),hl                            ; druhá dvojtečka
        inc h
        ld (shtm5+1),hl                            ; vteřiny

        ; ---- seconds = (E & 0x1F) * 2
        ld a,e
        and 00011111b
        add a,a
        ld (vteriny+1),a                           ; self-mod: operand pro "ld l,0" níže

        ; ---- hours/minutes: posuny doprava pro získání hodin (A) a minut (AF)
        ; Výsledkem:
        ;   A = hours
        ;   AF (po pop) = minutes (z B po posunech)
        ld a,d
        ld b,e
        srl a
        rr  b
        srl a
        rr  b
        srl a
        rr  b                                      ; po 3 posunech máme A≈(time>>3) a B≈(time low)
        srl b
        srl b                                      ; doladění na minutes (>>5)
        push bc

        ; ---- print hours
        ld l,a
        ld h,0
        call NUM
shtm1   ld hl,17*256+16
        ld a,16
        ld de,NUMBUF+3
        call print

shtm2   ld hl,19*256+16
        ld a,16
        ld de,dvojtecka
        call print

        ; ---- print minutes (z uloženého B)
        pop af                                      ; A = minutes
        ld l,a
        ld h,0
        call NUM
shtm3   ld hl,20*256+16
        ld a,16
        ld de,NUMBUF+3
        call print

shtm4   ld hl,19*256+16
        ld a,16
        ld de,dvojtecka
        call print

        ; ---- print seconds (operand je self-mod vteriny)
vteriny ld l,0                                     ; sem se dosadí seconds
        ld h,0
        call NUM
shtm5   ld hl,20*256+16
        ld a,16
        ld de,NUMBUF+3
        call print

        ret


; ============================================================
; smaznuly
; Odstraní vedoucí nuly v NUMBUF tím, že je nahradí mezerou.
; Končí na první nenulové číslici.
; ============================================================
smaznuly
        ld hl,NUMBUF
        ld b,5
snuly   ld a,(hl)
        cp "0"
        ret nz
        ld (hl)," "
        inc hl
        djnz snuly
        ret


; ============================================================
; help
; Zobrazí vícestránkový help dialog (18 řádků vysoký),
; vypíše řádky help1..help17 a čeká na ESC/Break (INKEY==1).
; ============================================================
help
        call savescr
        ld hl,8 * 256 + 4
        ld bc,60 * 256 + 18
        ld a,16
        call window

        ; postupně vypiš help texty
        ld hl,11*256+5
        ld a,16
        ld de,help1
        call print

        ld hl,11*256+7
        ld a,16
        ld de,help2
        call print

        ld hl,11*256+8
        ld a,16
        ld de,help3
        call print

        ld hl,11*256+9
        ld a,16
        ld de,help4
        call print

        ld hl,11*256+10
        ld a,16
        ld de,help5
        call print

        ld hl,11*256+11
        ld a,16
        ld de,help6
        call print

        ld hl,11*256+12
        ld a,16
        ld de,help7
        call print

        ld hl,11*256+13
        ld a,16
        ld de,help8
        call print

        ld hl,11*256+14
        ld a,16
        ld de,help9
        call print

        ld hl,11*256+15
        ld a,16
        ld de,help10
        call print

        ld hl,11*256+16
        ld a,16
        ld de,help11
        call print

        ld hl,11*256+17
        ld a,16
        ld de,help12
        call print

        ld hl,11*256+18
        ld a,16
        ld de,help13
        call print

        ld hl,11*256+19
        ld a,16
        ld de,help14
        call print

        ld hl,11*256+20
        ld a,16
        ld de,help15
        call print

        ld hl,11*256+21
        ld a,16
        ld de,help16
        call print

        ld hl,11*256+22
        ld a,16
        ld de,help17
        call print

help0
        xor a
        ld (TLACITKO),a
        call INKEY
        cp 1
        jp z,infoend                                ; ESC/Break → konec helpu (obvykle loadscr uvnitř infoend)
        jr help0


; ============================================================
; notnow
; Univerzální dialog: “This feature is not yet implemented.”
; Po stisku klávesy obnoví původní obrazovku + znovu překreslí nadpis
; (ruční kopie nadpisu do #4000 s atributem 16).
; ============================================================
notimplemented defb "This feature is not yet implemented.",0

notnow
        call savescr
        ld hl,8 * 256 + 10
        ld bc,60 * 256 + 3
        ld a,16
        call window

        ld hl,11*256+11
        ld a,16
        ld de,notimplemented
        call print

        ld hl,42*256+13
        ld a,32
        ld de,pressanykeytxt
        call print

        xor a
        ld (TLACITKO),a
        call INKEY
        call loadscr

        ; ručně překresli nadpis do obrazovky (nadpis → #4000),
        ; vždy znak + atribut 16 (ink/paper)
        ld hl,nadpis
        ld de,#4000
        ld bc,80
not0
        ld a,(hl)
        ld (de),a                                   ; znak
        inc de
        ld a,16
        ld (de),a                                   ; atribut
        inc de
        inc hl
        dec bc
        ld a,c
        or b
        jr nz,not0

        jp loop0



info
        call savescr
        ld hl,8 * 256 + 10
        ld bc,60 * 256 + 10
        ld a,16
        call window

        ld hl,11*256+11
        ld a,16
        ld de,calmcommander
        call print

        ld hl,11*256+13
        ld a,16
        ld de,info1txt
        call print

        ld hl,11*256+14
        ld a,16
        ld de,info6txt
        call print

        ld a,(pocetstranek)
        ld e,a
        ld d,8
        mul d,e
        ex de,hl
        call NUM
        ld hl,30*256+14
        ld a,16
        ld de,NUMBUF+1

        call print

        ld hl,34*256+14
        ld a,16
        ld de,kB
        call print

        ld hl,11*256+15
        ld a,16
        ld de,rtc
        call print

        ld de,presenttxt
        ld a,(rtcpresent)
        or a
        jr nz,rtcje
        ld de,notpresenttxt
rtcje

        ld hl,16*256+15
        ld a,16

        call print



        ld hl,11*256+17
        ld a,16
        ld de,info2txt
        call print

        ld hl,11*256+18
        ld a,16
        ld de,info3txt
        call print

        ld hl,11*256+19
        ld a,16
        ld de,info5txt
        call print


        ld hl,45*256+20
        ld a,32
        ld de,breaktxt
        call print

info0
        xor a
        ld (TLACITKO),a
        call INKEY
        cp 1
        jp z,infoend
        jp info0
infoend call loadscr
        jp loop0

kresli
        call prekresli_prazdne_okna
        ld a,0
        ld de,bottom
        ld hl,0*256+31
        call print

        ld hl,2*256+31
        ld a,32
        ld de,left_txt
        call print

        ld hl,10*256+31
        ld a,32
        ld de,right_txt
        call print

        ld hl,19*256+31
        ld a,32
        ld de,view_txt
        call print

        ld hl,27*256+31
        ld a,32
        ld de,edit_txt
        call print

        ld hl,35*256+31
        ld a,32
        ld de,copy_txt
        call print

        ld hl,43*256+31
        ld a,32
        ld de,move_txt
        call print

        ld hl,51*256+31
        ld a,32
        ld de,mkdir_txt
        call print

        ld hl,60*256+31
        ld a,32
        ld de,delete_txt
        call print

        ld hl,70*256+31
        ld a,32
        ld de,menu_txt
        call print

        ret


FINDLFN
        push hl
        ld hl,pathl
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a

        ld de,2
        add hl,de
        ld a,(hl)

        pop hl
        cp 255
        jr nz,findlfn830
        inc hl
findlfn830
        push hl
        ld hl,lfnpage
        call ROZHOD
        ld a,(hl)
        ld (lfnroot+1),a

        ld hl,LFNNAME
        ld de,LFNNAME+1
        ld bc,maxlen
vypln	ld a,32
        ld (hl),a
        ldir

        pop hl
        or a
        ld de,30
        sbc hl,de
        add hl,de
        jr c,prvni
        ld c,30
        call deleno
        jr oddeleno
prvni	ld a,l
        ld l,0

oddeleno	push af
lfnroot	ld a,24
        add a,l
            nextreg $57,a
            pop bc
            ld de,maxlen
            ld a,b
            or a
            ld hl,$e000
            jr z,prvnizaznam
lll
            add hl,de
            djnz lll

prvnizaznam
            ld (addrlfn),hl
            ld de,LFNNAME
            ld bc,maxlen
popop
            ld a,(hl)
            cp 255
            jr z,kon
            ld (de),a
            inc hl
            inc de
            dec bc
            ld a,b
            or c
            jr nz,popop

kon
            ld hl,(addrlfn)
            ld de,261
            add hl,de
            ld de,LFNNAME+261
            ld bc,8
            ldir                                  ; prenes velikost souboru
            ld hl,LFNNAME
            ld b,40
F22222
            ld a,(hl)
            cp 255
            call z,setspace
            inc hl
            djnz F22222
lfnend		ld hl,LFNNAME

lfnat		ld de,20672+2
            ld hl,LFNNAME
            ret




sysatrtxt	defb "System attributes:",0
fileinfonadpis
            defb "File/directory informations:",0
filedate	defb "Date:",0
filetime	defb "Time:",0
tecka		defb ". ",0
dvojtecka 	defb ":",0
DISC


        include "functions/search.asm"
        include "functions/selected.asm"



LoadSprites
            ld bc,SPRITE_STATUS_SLOT_SELECT_P_303B
            out (c),a
            ld (.dmaSource),hl
            ld (.dmaLenght),bc
            ld hl, .dmaProgram
            ld b, .dmaProgramLength
            ld c,$6B
            otir

            nextreg $19,0
            nextreg $19,255
            nextreg $19,0
            nextreg $19,255

            ret

.dmaProgram db %10000011
            db %01111101

.dmaSource	dw 0
.dmaLenght	dw 0
            db %00010100
            db %00101000
            db %10101101
            dw $005B
            db %10000010
            db %11001111
            db %10000111

.dmaProgramLength = $ - .dmaProgram

; ------------------------------------------------------------
; showSprite
; ------------------------------------------------------------
; Rutina:
; - vypíše aktuální souřadnice myši (debug)
; - zavolá driver myši (MOUSE)
; - aktualizuje stav tlačítek
; - nastaví sprite kurzoru na nové souřadnice

; MOUSE vrací:
; HL = nové souřadnice
; A  = stav tlačítek

; Používá self-modifying code pro zápis operandů NextReg.
; ------------------------------------------------------------

lastCoordMouse  defw 0;

showSprite
            ; ochrana registrů používaných uvnitř rutiny
            push af
            push bc
            push hl
            push de


            ; ------------------------------------------------------------
            ; DEBUG: výpis X souřadnice
            ; ------------------------------------------------------------

        ld a,(CONTRB)                              ; načti aktuální X myši
        ld l,a
        ld h,0                                    ; HL = číslo pro převod na text
        call NUM                                  ; převeď číslo do NUMBUF

        ld hl,41*256+31                           ; pozice na obrazovce (y,x)
        ld a,16                                   ; šířka pole pro tisk
        ld de,NUMBUF                              ; buffer s číslem
        ;call print                                ; tisk čísla


                                                  ; ------------------------------------------------------------
                                                  ; DEBUG: výpis Y souřadnice
                                                  ; ------------------------------------------------------------

        ld a,(COORD+1)                            ; načti aktuální Y
        ld l,a
        ld h,0
        call NUM

        ld hl,50*256+31                           ; jiný řádek na obrazovce
        ld a,16
        ld de,NUMBUF
        ;call print


        ; ------------------------------------------------------------
        ; Načtení nové pozice a tlačítek z driveru
        ; ------------------------------------------------------------

        call MOUSE                                ; HL = nové souřadnice, A = tlačítka

        ld b,a                                    ; uložit aktuální stav tlačítek

                                                  ; kontrola změny tlačítek oproti uloženému stavu
        ld a,(TLACITKO)
        xor b
        jr z,shwSpr                               ; žádná změna → pokračuj

                                                  ; při změně uložíme OR stav
                                                  ; tlačítko zůstane "aktivní", dokud se ručně nevynuluje
        ld a,(CONTRB)
        or b
        ld (TLACITKO),a

shwSpr

        ; ------------------------------------------------------------
        ; Přenos nové pozice do self-modifying instrukcí
        ; ------------------------------------------------------------
        ; Hodnota operandů instrukcí níže se změní na nové X,Y

        ld a,l
        ld (Xcoordinate + 1),a                    ; operand LD D,xx

        ld a,h
        ld (Ycoordinate + 1),a                    ; operand LD A,xx


                                                  ; ------------------------------------------------------------
                                                  ; Nastavení sprite registrů (Next)
                                                  ; ------------------------------------------------------------

        nextreg $34,0                             ; výběr sprite slotu (sprite 0)


                                                  ; ------------------------------------------------------------
                                                  ; Výpočet X pozice sprite
                                                  ; ------------------------------------------------------------
                                                  ; X se násobí 2 (přepočet jednotek / subpixel režim)

Xcoordinate  
        ld d,0                               ; zde bude dosazena X souřadnice
        ld e,2
        mul d,e                               ; DE = X * 2

        ld a,e                                ; dolní byte výsledku
        ld (mouseX+3),a                       ; uložit do operandu nextreg


                                                  ; úprava horní části X + flagů
        ld a,(moreX + 3)
        res 0,a                                   ; zruš bit 0
        or d                                      ; přidej horní byte X
shwSpr_EnableBit:        
        set 7,a                                   ; sprite enable flag
        ld (moreX + 3),a
nemenX


; ------------------------------------------------------------
; Nastavení Y pozice sprite
; ------------------------------------------------------------

Ycoordinate  
        ld a,0
        ld (mouseY+3),a                       ; dosadí operand nextreg


                                                  ; ------------------------------------------------------------
                                                  ; Zápis souřadnic sprite do Next registrů
                                                  ; ------------------------------------------------------------

mouseX      nextreg $35,10                        ; X low
mouseY      nextreg $36,80                        ; Y
moreX       nextreg $37,%00001000                 ; X high + flags
            nextreg $38,%10000000                 ; další sprite flags


            pop de
            pop hl
            pop bc
            pop af
            ret

PRAVE_TLACITKO
        ld hl,menuSouradnice
        call CONTROL
        jp nc,menu                                ; pokud CONTROL vrátil NC = klik v oblasti horního řádku → otevři menu

        ld hl,leveOkno
        call CONTROL
        jp nc,prave2                               ; klik spadl do levého okna → obsluha levého okna

        ld hl,praveOkno
        call CONTROL
        jp nc,prave3                               ; klik spadl do pravého okna → obsluha pravého okna

        jp loop0                                  ; klik mimo definované oblasti → nic, zpět do hlavní smyčky

; ------------------------------------------------------------
; LEVE_TLACITKO
; Obsluha kliknutí levým tlačítkem myši.
; Testuje, kam uživatel kliknul:
;  - horní řádek (menu)
;  - levé okno (seznam souborů vlevo)
;  - pravé okno (seznam souborů vpravo)
; Podle toho provede odpovídající akci.
; ------------------------------------------------------------
LEVE_TLACITKO

        ld hl,menuSouradnice
        call CONTROL
        jp nc,menu                                ; pokud CONTROL vrátil NC = klik v oblasti horního řádku → otevři menu

        ld hl,leveOkno
        call CONTROL
        jp nc,leve2                               ; klik spadl do levého okna → obsluha levého okna

        ld hl,praveOkno
        call CONTROL
        jp nc,leve3                               ; klik spadl do pravého okna → obsluha pravého okna

        jp loop0                                  ; klik mimo definované oblasti → nic, zpět do hlavní smyčky

leve2
        ; ---- Levé okno: připraví kontext levého panelu a spočítá, na jakou položku se kliklo
        call lw                                   ; přepni/aktivuj levé okno (např. nastav aktivní panel, barvy, kurzor, apod.)
        call vypoctiClick                         ; přepočítej Y souřadnici myši na index položky v seznamu + ošetři doubleclick



        jp loop0

leve3
        ; ---- Pravé okno: analogicky pro pravý panel
        call rw                                   ; přepni/aktivuj pravé okno
        call vypoctiClick                         ; stejná logika výběru položky


        jp loop0


prave2
        ; ---- Levé okno: připraví kontext levého panelu a spočítá, na jakou položku se kliklo
        call lw                                   ; přepni/aktivuj levé okno (např. nastav aktivní panel, barvy, kurzor, apod.)
        call vypoctiClickRight                         ; přepočítej Y souřadnici myši na index položky v seznamu + ošetři doubleclick



        jp loop0

prave3
        ; ---- Pravé okno: analogicky pro pravý panel
        call rw                                   ; přepni/aktivuj pravé okno
        call vypoctiClickRight                         ; stejná logika výběru položky


        jp loop0


vypoctiClickRight
        ld a,(COORD+1)                            ; A = Y souřadnice myši (v pixelech nebo v jednotkách co vrací MOUSE)
        ld d,a                                    ; D = Y (před dělením)
        ld e,8
        call deleno8                              ; D = Y / 8  (převod na řádek znakové mřížky / řádek seznamu)
        dec d
        dec d                                     ; posun o 2 řádky nahoru (typicky kvůli hlavičce/okrajům okna)

        push de                                   ; ulož DE (E=8, D=řádek) – budeme ho brzy potřebovat znovu

        ld hl,ALLFILES
        call ROZHOD2                              ; HL = ukazatel na strukturu/tabulku aktivního panelu (varianta "2")
        pop de                                    ; obnov D = vypočtený řádek položky

        push de                                   ; znovu ulož D (budeme ho ještě používat)
        ld e,d
        ld d,0                                    ; DE = index položky (16-bit), E = číslo řádku/položky, D=0

        ; Následující blok vytahuje z (HL) pointer/délku struktury.
        ; Typicky: (HL)=low, (HL+1)=high → HL = adresa/počet položek apod.
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a                                    ; HL = 16bit hodnota ze struktury (např. počet položek, konec seznamu, apod.)

        ; Ověření rozsahu: odečítá DE od HL a sleduje carry.
        ; Tady to vypadá jako "je index (DE) uvnitř rozsahu?" – pokud je mimo, vrátí se s C=1.
        dec hl
        dec hl                                    ; korekce HL (např. ukazuje na konec-2, nebo počet-2)
        or a                                      ; vynuluje carry před SBC
        sbc hl,de                                 ; HL = HL - DE
        pop de                                    ; obnov původní DE (D=řádek)
        ret c                                     ; pokud borrow/carry → index mimo platný rozsah → nic nevybírej

        push de                                   ; index je platný → uložíme ještě jednou D (číslo souboru/řádku)

        ld hl,adrl
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        ld (adrs+1),hl

        call getroot

        call showwin

        ld hl,POSKURZL
        call ROZHOD                               ; HL = adresa proměnné pro "pozici kurzoru" v aktuálním panelu

        pop de                                    ; obnov D = index položky (v komentáři níže: "v D máme číslo souboru")

        ld a,d
        ld (hl),a                                 ; uloží vybranou položku jako aktuální (kurzor na soubor)

        ld a,32
        call writecur                             ; zřejmě vrať viditelný kurzor / nastav znak kurzoru (32=mezera?) dle implementace
        ld hl,rightMouse
        inc (hl)

pauzaR
        call MOUSE                                ; načti aktuální stav myši (souřadnice do COORD apod.)
        call showSprite                           ; překresli sprite kurzoru myši

        ld bc,KEMPSTON_MOUSE_B_P_FADF
        in a,(c)                                  ; A = stav tlačítek z KEMPSTON mouse portu

        bit 0,a                                   ; test tlačítka (v tomto kódu je "bit 1" = levé nebo konkrétní tlačítko dle mapování)
        jr z,pauzaR                                ; pokud bit=0 → stále nedošlo k (požadované změně) → čekej dál

        jp select

; ------------------------------------------------------------
; vypoctiClick
; Z Y souřadnice myši (COORD+1) spočítá index řádku/položky v seznamu.
; Potom:
;  - ověří, jestli je klik v rozsahu existujících souborů (ALLFILES)
;  - nastaví pozici kurzoru (POSKURZL) na vybraný soubor
;  - čeká na uvolnění tlačítka
;  - a následně krátce testuje doubleclick:
;      * pokud doubleclick na stejné položce → skok na "enter"
;      * pokud klik pokračoval jinam → znovu obsluha LEVE_TLACITKO
; ------------------------------------------------------------
vypoctiClick
        ld a,(COORD+1)                            ; A = Y souřadnice myši (v pixelech nebo v jednotkách co vrací MOUSE)
        ld d,a                                    ; D = Y (před dělením)
        ld e,8
        call deleno8                              ; D = Y / 8  (převod na řádek znakové mřížky / řádek seznamu)
        dec d
        dec d                                     ; posun o 2 řádky nahoru (typicky kvůli hlavičce/okrajům okna)

        push de                                   ; ulož DE (E=8, D=řádek) – budeme ho brzy potřebovat znovu

        ld hl,ALLFILES
        call ROZHOD2                              ; HL = ukazatel na strukturu/tabulku aktivního panelu (varianta "2")
        pop de                                    ; obnov D = vypočtený řádek položky

        push de                                   ; znovu ulož D (budeme ho ještě používat)
        ld e,d
        ld d,0                                    ; DE = index položky (16-bit), E = číslo řádku/položky, D=0

        ; Následující blok vytahuje z (HL) pointer/délku struktury.
        ; Typicky: (HL)=low, (HL+1)=high → HL = adresa/počet položek apod.
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a                                    ; HL = 16bit hodnota ze struktury (např. počet položek, konec seznamu, apod.)

        ; Ověření rozsahu: odečítá DE od HL a sleduje carry.
        ; Tady to vypadá jako "je index (DE) uvnitř rozsahu?" – pokud je mimo, vrátí se s C=1.
        dec hl
        dec hl                                    ; korekce HL (např. ukazuje na konec-2, nebo počet-2)
        or a                                      ; vynuluje carry před SBC
        sbc hl,de                                 ; HL = HL - DE
        pop de                                    ; obnov původní DE (D=řádek)
        ret c                                     ; pokud borrow/carry → index mimo platný rozsah → nic nevybírej

        push de                                   ; index je platný → uložíme ještě jednou D (číslo souboru/řádku)

        ld hl,adrl
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        ld (adrs+1),hl

        call getroot

        call showwin

        ld hl,POSKURZL
        call ROZHOD                               ; HL = adresa proměnné pro "pozici kurzoru" v aktuálním panelu

        pop de                                    ; obnov D = index položky (v komentáři níže: "v D máme číslo souboru")

        ld a,d
        ld (hl),a                                 ; uloží vybranou položku jako aktuální (kurzor na soubor)

        ld a,32
        call writecur                             ; zřejmě vrať viditelný kurzor / nastav znak kurzoru (32=mezera?) dle implementace

        ; --------------------------------------------------------
        ; Čekání na uvolnění tlačítka:
        ; smyčka pauza opakovaně čte stav myši, dokud bit 1 není 1.
        ; (tj. dokud tlačítko stále drží, čeká)
        ; --------------------------------------------------------
        ld b,25                                   ; (nepoužité kvůli zakomentovanému DJNZ, ale původně zřejmě timeout)
pauza
        call MOUSE                                ; načti aktuální stav myši (souřadnice do COORD apod.)
        call showSprite                           ; překresli sprite kurzoru myši

        ld bc,KEMPSTON_MOUSE_B_P_FADF
        in a,(c)                                  ; A = stav tlačítek z KEMPSTON mouse portu

        bit 1,a                                   ; test tlačítka (v tomto kódu je "bit 1" = levé nebo konkrétní tlačítko dle mapování)
        jr z,pauza                                ; pokud bit=0 → stále nedošlo k (požadované změně) → čekej dál
dvoj2    ei                                       ; po uvolnění tlačítka povol přerušení (pokud byly vypnuté)

        ; --------------------------------------------------------
        ; Ošetření doubleclicku:
        ; krátké okno (B=50), ve kterém testuje další stisk téhož tlačítka.
        ; Pokud se do limitu stisk objeví, jde se do overDvojKlik.
        ; --------------------------------------------------------
        ld b,50
dvojKlik
        halt                                      ; čekej na přerušení (časování / zpomalení smyčky)
        push bc

        call showSprite                           ; udržuj kurzor myši živý (překreslení)

        ld bc,KEMPSTON_MOUSE_B_P_FADF
        in a,(c)
        bit 1,a                                   ; znovu test tlačítka
        push af
        call MOUSE                                ; aktualizuj COORD (aby šlo zjistit, kam případný doubleclick míří)
        pop af
        pop bc
        jp z,overDvojKlik                         ; pokud bit signalizuje "klik" → řeš doubleclick

        djnz dvojKlik                             ; jinak ubírej časové okno
        ret                                       ; timeout → nebyl doubleclick, hotovo

odskocZnovaKlik
        ; sem se jde, když druhý klik nebyl na stejné položce
        ; (tj. uživatel kliknul jinam → zpracuj jako nový klik)
        pop hl                                    ; vyrovnání zásobníku (typicky návratová adresa / něco, co si výše tlačilo)
        xor a
        ld (TLACITKO),a                           ; reset indikace tlačítka
        jp LEVE_TLACITKO                          ; zpracuj to znovu jako nový klik (rekurze přes JP)

odskocEnter
        ; sem se jde, když byl doubleclick na stejné položce
        pop hl                                    ; vyrovnání zásobníku
        xor a
        ld (TLACITKO),a                           ; reset tlačítka
xx      jp enter                                  ; provede "enter" akci nad vybranou položkou (otevřít soubor/adresář)

; ------------------------------------------------------------
; overDvojKlik
; Vyhodnocení: byl druhý klik na stejné položce jako první?
; Porovná řádek z aktuální Y souřadnice s uloženou pozicí kurzoru POSKURZL.
; Pokud sedí → enter, jinak → obsluha nového kliku.
; ------------------------------------------------------------
overDvojKlik
        ld d,h                                    ; POZOR: H zde musí obsahovat něco smysluplného (pravděpodobně Y z MOUSE?)
                                                  ; (z kontextu: po call MOUSE se často vrací souřadnice v registrech)
        ld e,8
        call deleno8                              ; D = (aktuální Y)/8
        dec d
        dec d                                     ; stejný posun jako u prvního výpočtu

        push de
        ld hl,POSKURZL
        call ROZHOD                               ; HL = adresa uložené pozice kurzoru pro aktivní panel
        ld a,(hl)                                 ; A = původně vybraná položka (index)
        pop de

        xor d                                     ; porovná A s D: když jsou stejné → výsledek 0
        jr z,odskocEnter                          ; stejné → doubleclick na stejné položce → ENTER
        jp odskocZnovaKlik                        ; různé → považuj jako nový klik


; ------------------------------------------------------------
; InkeyNoWait
; Nečekající čtení klávesy + průběžná obsluha myši.
; - nejdřív refresh času a sprite myši
; - pokud je TLACITKO != 0, přesměruje na clickMouse (myší událost má prioritu)
; - jinak provede KEYSCAN a mapuje scancode přes tabulky:
;     SYMTAB / CAPSTAB / NORMTAB dle D (modifier)
; - vrací A = znak, nebo Z pokud nic
; ------------------------------------------------------------
InkeyNoWait
        call gettime                              ; aktualizace času (pro timeouty, UI, atd.)


        call MOUSE
        ld b,a                                    ; uložit aktuální stav tlačítek

                                                  ; kontrola změny tlačítek oproti uloženému stavu
        ld a,(TLACITKO)
        xor b
        jr z,nemenTlacitka2                               ; žádná změna → pokračuj

                                                  ; při změně uložíme OR stav
                                                  ; tlačítko zůstane "aktivní", dokud se ručně nevynuluje
        ld a,(CONTRB)
        or b
        ld (TLACITKO),a
nemenTlacitka2        


        ld hl,(COORD)
        ld de,(lastCoordMouse)
        or a
        sbc hl,de
        jr z,zadnyPohybMysky


        call showSprite                           ; udržuj myš viditelnou
zadnyPohybMysky
        call MOUSE
        ld hl,(COORD)
        ld (lastCoordMouse),hl
        ld a,(TLACITKO)
        or a
        jp nz,clickMouse                          ; pokud je evidovaný klik myši → obsluž ho místo kláves

        xor  a
        ld   (aLAST_KEY+1),a                      ; vynuluj "last key" (self-modifying operand v aLAST_KEY)
        ei                                        ; povol přerušení

        ; malý delay, aby KEYSCAN nebyl volán extrémně často (a současně běžely HALT timingy)
        ld b,2
CEKEJd  halt
        djnz CEKEJd

ahl0d   call KEYSCAN                              ; nasnímání klávesnice; vrací kód v DE (D=mod, E=key index), podle implementace

        ld   a,e
        inc  a
        ret z                                     ; pokud E = $FF (po inc → 0), pravděpodobně "žádná klávesa"

        ld   a,d                                  ; A = stav modifikátorů (caps/symbol shift apod.)
        ld   hl,SYMTAB
        cp   $18
        jr   z,aHLSM2s                            ; pokud D==$18 → symbol tabulka

        ld   hl,CAPSTAB
        cp   $27
        jr   z,aHLSM2s                            ; pokud D==$27 → caps tabulka

        ld   hl,NORMTAB                           ; jinak normální tabulka

aHLSM2s
        ld   d,0                                  ; DE = index do tabulky znaků (D=0, E=key)
        add  hl,de                                ; HL = tabulka + index
        ld   a,(hl)                               ; A = výsledný znak
        or   a
        ret z                                     ; pokud tabulka vrací 0 → nic/ignoruj
        jp aLAST_KEY                              ; pokračuj do rutiny zpracování/filtrace opakování klávesy


sysvars 	defs 500
sipka	incbin "sipka.spr"

last:
E2
             SAVEBIN "cc1.bin",S1,E1-S1
             SAVEBIN "cc2.bin",S2,E2-S2
            SAVEBIN "cc.bin", S1, E2-S1

            DISPLAY "Volne misto v prvni casti:",/A,S3 - E1
            DISPLAY "Volne misto v druhe casti:",/A,S2 - E3
            DISPLAY "Volne misto v treti casti:",/A,57344 - E2
            DISPLAY "Volne misto v treti casti:",/A,E2 - 57344

              CSPECTMAP player.map
              ; savenex open "CalmCommander.nex",START,ORG_ADDRESS-2
              ; savenex core 2,0,0
              ; savenex auto
              ; savenex close