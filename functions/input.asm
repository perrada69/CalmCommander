;		hx = délka vstupu
;		hl = adresa na obrazovce 
;       a = barva palety

INPUT 	ei
		ld (INPOS+1),hl     ;ulož adresu začátku pro další použití
        ld (INCOL+1),a      ;ulož barvu
		ld hl,23296         ;do HL adresa editační oblasti
		ld b,hx             ;do B délka editační oblasti
IN1 	ld (hl),32          ;a nyní celou editační
		inc hl              ;zónu vyplníme mezerami
		djnz IN1            ;na konec editační zóny
INZERO	ld (hl),b           ;přijde 0
		res 5,(iy+1)        ;signál není stisknuta klávesa
		xor a               ;nastav kurzor
		ld (CURSOR+1),a     ;na začátek editační zóny
IN2 	ld b,hx             ;nyní celou editační zónu
INPOS 	ld hl,0             ;vytiskneme, nastav
		ld (PPOS+1),hl      ;tiskovou pozici
		ld hl,23296         ;začínáme od začátku
CURSOR 	ld c,0              ;do C polohu kurzoru
IN3 	ld a,l              ;testuj spodní byte adresy
		cp c                ;v případe rovnosti
		ld a,">"            ;dej do A kód kurzoru
		call z,CHAR         ;a vytiskni ho
		ld a,(hl)           ;vytiskni znak
		call CHAR           ;z editační zóny
		inc hl              ;a posun se pro další
		djnz IN3            ;opakuj se všemi znaky
		ld a,l              ;kurzor také může
		cp c                ;být až za posledním
		ld a,"<"            ;znakem, pak bude
		call z,CHAR         ;na řádku vypadat jinak
		call INKEY          ;přečti si kód klávesy
		cp 1                ;testuj EDIT (Caps Shift + 1)
		ret z               ;a případně se vrať zpátky
		cp 13               ;testuj ENTER a případné odskoč
		ret z               ;jp z,INPCLEAR ;na smazání řádku z obrazovky
		
		ld hl,IN2           ;na zásobník ulož adresu IN2, sem
		push hl             ;se bude nyní program vracet
		ld hl,CURSOR+1      ;do HL vlož adresu pozice kurzoru
		cp 8                ;testuj kurzor doleva
		jr z,CURSLEFT       ;odskoč
		cp 9                ;kurzor doprava
		jr z,CURSRGHT
		cp 12               ;delete (správně BACKSPACE)
		jr z,BCKSPACE
		cp 199              ;znak <= (funkce DELETE)
		jr z,DELETE 
		cp 32               ;nyní zbývají
		ret c               ;obyčejné znaky,
		cp 128              ;odfiltruj
		ret nc              ;netisknutelné znaky
		ex af,af'           ;a kód přesuň do A'
		ld a,(hl)           ;testuj, zda není kurzor
		cp hx               ;na konci řádku,
		ret nc              ;když ano, tak se vrať
		inc (hl)            ;posuň kurzor doprava
		ld l,(hl)           ;do HL vlož adresu,
		dec l               ;na kterou bude znak
		ld h,23296/256      ;uložen
INS 	ld a,(hl)           ;přečti původní znak
		or a                ;a testuj konec řádku
		ret z               ;případně se vrať
		ex af,af'           ;přehoď původní a nový znak
		ld (hl),a           ;a nový zapiš, pro další znak
		inc hl              ;bude novým znakem předchozí
		jr INS              ;znak, opakuj posun znaku až do konce
CURSLEFT ld a,(hl)          ;přečti polohu kurzoru
		or a                ;a v případe, že je na levém okraji
		ret z               ;tak se vrať a nic nedělej
		dec (hl)            ;posuň kurzor doleva a vrať se na IN2
		ret
CURSRGHT ld a,(hl)          ;přečti polohu kurzoru
		cp hx               ;a když je na konci řádku
		ret nc              ;tak se vrať
		inc (hl)            ;jinak posuň kurzor doprava a vrať se
		ret                 ;vrať se na IN2
DELETE 	ld a,(hl)           ;na konci
		cp hx               ;editační zóny
		ret z               ;DELETE nepracuje
		inc a               ;jinak uprav polohu
		jr BCK2             ;a pokračuj společnou částí
BCKSPACE ld a,(hl)          ;BACKSPACE naopak
		or a                ;nepracuje na začátku
		ret z               ;editační zóny
		dec (hl)            ;posuň kurzor vlevo
BCK2 	ld l,a              ;společná část,
		ld h,23296/256      ;která zajišťuje
		ld e,l              ;přesunutí následujících
		ld d,h              ;znaků na uvolněné místo
		dec e               ;po smazaném znaku 

DEL2 	ld a,(hl)           ;vlastní přesun
		ldi                 ;je prováděn instrukcí
		or a                ;LDI dokud není přenesena 0,
		jr nz,DEL2          ;která signalizuje konec zóny
		ex de,hl            ;na poslední pozici,
		dec hl              ;která se nyní uvolnila,
		ld (hl)," "         ;je zapsána mezera
		ret                 ;návrat na IN2

CHAR    push hl
PPOS    ld hl,0
        ld (hl),a
        inc hl
INCOL   ld a,0        
        ld (hl),a
        inc hl
        ld (PPOS+1),hl
        pop hl
        ret