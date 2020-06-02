
SELDATA  equ  $A000

;poloha bitu, který se bude xorovat.
TABBITS  db 1,2,4,8,16,32,64,128

;Zjistí konkrétní bit a nastaví masku pro XOR 
;Vstup: HL = číslo souboru
;Po zavolání SELTED pak stačí udělat jen:
;         ex   de,hl
;         xor  (hl)
;         ld   (hl),a
SELTED  dec hl
        ld   de,SELDATA
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
        rrca 
        rrca 
        rrca 
        rrca 
        rrca 
        add  hl,de
        ex   de,hl
        ld   hl,TABBITS
        add  a,l
        ld   l,a
        jr   nc,SELC2
        inc  h
SELC2   ld   a,(hl)
        ret 

;najde první označenou položku
GETSELF ld hl,$a000         ;adresa prního
        ld de,0             ;vynuluj  počítadlo
        
first1  dec de
        ld a,(hl)      
        ld b,8
first0  inc de
        rrca
        jr c,nalezeno
first2  djnz first0
        inc de
first3  inc hl
        jr first1
nalezeno
        ld (GETSELN+1),hl
        ld (getselde+1),de
        ex de,hl
        ld c,a
        ld a,b
        ld (selnb+1),a
        ld a,c
        ld (selna+1),a
        ret
;najde další označenou položku

GETSELN ld hl,$a000
getselde ld de,0

selnb   ld b,8
selna   ld a,0
        jr first2

;Nastránkuje stránku se select daty od adresy $A000
GETSELPAGE
        nextreg $55,19
        ret

INITSEL 
        ld a,1
        ld (selnb+1),a
        ld hl,$a000-1
        ld (GETSELN+1),hl
        ld hl,$0
        ld (getselde+1),hl
        ld a,($a000)
        ld (selna+1),a
        ret

; 		call GETSELPAGE
; 		ld hl,$a000
; 		ld de,$a001
; 		ld bc,8*1024
; 		xor a
; 		ld (hl),a
; 		ldir

; 		ld hl,$134
; 		call SELTED
; 		ex de,hl
; 		xor (hl)
; 		ld (hl),a

; 		call INITSEL

; III		
; 		call GETSELN
; 		call GETSELN
; 		call GETSELN
; 		call GETSELN
; 		call GETSELN
