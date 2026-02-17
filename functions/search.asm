
; ------------------------------------------------------------
; wildcard_match_ci
;   '*' = libovolná posloupnost znaků (včetně prázdné)
;   '?' = jeden libovolný znak
;   case-insensitive ASCII
;   terminátor: 0 nebo 255
;
; IN : DE = pattern (maska)
;      HL = text (filename)
; OUT: ZF=1 match
;      ZF=0 no match
; TRASH: AF, BC, DE, HL, IX
; ------------------------------------------------------------
search
wildcard_match_ci:

        ; IX = pattern resume po '*'
        ; BC = text resume pro '*'
        ld ix,0
        ld bc,0

.loop:
        ld a,(de)
        call .is_end
        jr z,.pattern_end

        cp '*'
        jr z,.star

        ld a,(hl)
        call .is_end
        jr z,.text_end_need_only_stars

        ld a,(de)
        cp '?'
        jr z,.match_one

        ; case-insensitive compare (SAFE: nešahá na D/E pointer)
        push bc                ; zachovej resume pointer
        ld a,(de)
        call to_upper
        ld b,a                 ; B = upper(pattern char)

        ld a,(hl)
        call to_upper
        cp b                   ; compare upper(text char) vs upper(pattern)
        pop bc
        jr z,.match_one

        ; mismatch
        ld a,ixl
        or ixh
        jr z,.fail

        ; backtrack přes '*'
        push ix
        pop de                 ; DE = pattern resume (za '*')

        inc bc                 ; posuň text resume o 1
        push bc
        pop hl                 ; HL = nový text resume
        jr .loop

.match_one:
        inc de
        inc hl
        jr .loop

.star:
        inc de                 ; přeskoč '*'
        push de
        pop ix                 ; IX = pattern resume (za '*')
        push hl
        pop bc                 ; BC = text resume (odtud může '*' začít matchovat)
        jr .loop

.pattern_end:
        ; pattern skončil -> match jen když text taky skončil
        ld a,(hl)
        call .is_end
        jr z,.success

        ; jinak povol backtrack, jen pokud bylo '*'
        ld a,ixl
        or ixh
        jr z,.fail

        push ix
        pop de

        inc bc
        push bc
        pop hl
        jr .loop

.text_end_need_only_stars:
        ; text skončil -> pattern může obsahovat už jen '*'
.skip_stars:
        ld a,(de)
        call .is_end
        jr z,.success
        cp '*'
        jr nz,.fail
        inc de
        jr .skip_stars

.success:
        xor a                  ; ZF=1
        ret

.fail:
        or 1                   ; ZF=0
        ret

; ------------------------------------------------------------
; A = znak
; Z pokud 0 nebo 255
.is_end:
        cp 0
        ret z
        cp 255
        ret

; ------------------------------------------------------------
; A -> uppercase pokud 'a'..'z'
to_upper:
        cp 'a'
        ret c
        cp 'z'+1
        ret nc
        sub 32
        ret



;Hledávní se specifickým koncem (default je 255)
;Vstup:
;Vstup:
;         DE .... vstup hledaneho retezce (ukončený bytem 255)
;         HL .... vstup textu,kde budeme hledat (ukončený bytem 255)
;		  A ..... ukončující byte
; Výstup:
;         Z ... nalezeno
;         NZ .. nenalezeno

specific_search
			push ix
			call search
			pop ix
			ret


deselect
        call savescr

		ld hl,10 * 256 + 10
		ld bc,60 * 256 + 10
		ld a,16
		call window

		ld hl,11*256+11
		ld a,16
		ld de,DESELTXT
		call print

		ld hl,11*256+13
		ld a,16
		ld de,SELTXT2
		call print

		ld hl,56*256+20
		ld a,48
		ld de,searchtxt
		call print		

		ld hl,60*256+19
		ld a,16
		ld de,notxt
		call print		


;		ld hx,59
;		ld hl,$4000 + 15*160 + 11*2
;		ld a,80
;		call INPUT


		ld hx,59
		ld hl,$4000 + 15*160 + 11*2
		ld a,80

		ld (INPOS+1),hl     ;ulož adresu začátku pro další použití
        ld (INCOL+1),a      ;ulož barvu

		ld hl,23296         ;do HL adresa editační oblasti
		ld b,hx             ;do B délka editační oblasti
		push bc
RIN1aa 	ld (hl),32          ;a nyní celou editační
		inc hl              ;zónu vyplníme mezerami
		djnz RIN1aa         ;na konec editační zóny
		ld a,"*"
		ld (23296),a
		pop bc

		res 5,(iy+1)        ;signál není stisknuta klávesa
		xor a

		ld a,1
        ld (CURSOR+1),a
        call IN2



		
		cp 1                    ;testuj BREAK
		jp z,dfind_end

        ld hl,ALLFILES
		call ROZHOD2
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a
;        dec hl
dfind0   push hl


		call BUFF83
		call find83
		pop hl
        push hl
        dec hl
		call FINDLFN

		ld hl,23296 + 59		;najdi poslední znak
dfind1	
		dec hl
		ld a,(hl)
		cp 32
		jr z,dfind1
		inc hl
		ld a,255
		ld (hl),a

		ld hl,LFNNAME + 261		;najdi poslední znak
dfind2	
		dec hl
		ld a,(hl)
		cp 32
		jr z,dfind2
		inc hl
		ld a,255
		ld (hl),a


        ld de,23296
        ld hl,LFNNAME
        call search
        jr nz,dnesouhlasi
        ld hl,(foundfile)
        call BUFF83
        res 7,(hl)

        ld hl,numsel
    	call ROZHOD2
        ld (adresasel+1),hl
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        inc hl
        ld a,l
        ld (dzvys+1),a
        ld a,h
        ld (dzvys2+1),a
dadresasel   ld hl,0
dzvys    ld (hl),0
        inc hl
dzvys2   ld (hl),0        

dnesouhlasi
        pop hl
        dec hl
        ld a,l
        or h
        jp nz,dfind0
		
dfind_end
        call loadscr
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


invert_body
        ld hl,ALLFILES
		call ROZHOD2
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a

idfind0   push hl


		call BUFF83
		call find83
		pop hl
        push hl
        dec hl
		call FINDLFN

		ld hl,LFNNAME + 261		;najdi poslední znak
idfind2	
		dec hl
		ld a,(hl)
		cp 32
		jr z,idfind2
		inc hl
		ld a,255
		ld (hl),a
		inc hl
		xor a
		ld (hl),a
ID  
		ld hl,LFNNAME
		ld de,banlfn1
		ld a,0
		call specific_search
		jp z,idnesouhlasi
		ld hl,LFNNAME
		ld de,banlfn2
		ld a,0
		call specific_search
		jp z,idnesouhlasi
  
  
        ld hl,(foundfile)
  
  
  
        call BUFF83
		ld a,(hl)
		xor 10000000b
		ld (hl),a

        ld hl,numsel
    	call ROZHOD2
        ld (adresasel+1),hl
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        inc hl
        ld a,l
        ld (dzvys+1),a
        ld a,h
        ld (dzvys2+1),a
idadresasel   ld hl,0
idzvys  ld (hl),0
        inc hl
idzvys2 ld (hl),0        

idnesouhlasi
        pop hl
        dec hl
        ld a,l
        or h
        jp nz,idfind0
		call NOBUFF83
		ret

invert_select_files
		call invert_body


idfind_end
        ;call loadscr
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

SELTXT		defb "Search and select files.",0
SELTXT2		defb "Please insert part of name:",0
DESELTXT	defb "Search and deselect files.",0


select_files
        call savescr

		ld hl,10 * 256 + 10
		ld bc,60 * 256 + 10
		ld a,16
		call window

		ld hl,11*256+11
		ld a,16
		ld de,SELTXT
		call print

		ld hl,11*256+13
		ld a,16
		ld de,SELTXT2
		call print

		ld hl,56*256+20
		ld a,48
		ld de,searchtxt
		call print		

		ld hl,60*256+19
		ld a,16
		ld de,notxt
		call print		


;		ld hx,59
;		ld hl,$4000 + 15*160 + 11*2
;		ld a,80
;		call INPUT


		ld hx,59
		ld hl,$4000 + 15*160 + 11*2
		ld a,80

		ld (INPOS+1),hl     ;ulož adresu začátku pro další použití
        ld (INCOL+1),a      ;ulož barvu

		ld hl,23296         ;do HL adresa editační oblasti
		ld b,hx             ;do B délka editační oblasti
		push bc
RIN1a 	ld (hl),32          ;a nyní celou editační
		inc hl              ;zónu vyplníme mezerami
		djnz RIN1a            ;na konec editační zóny
		ld a,"*"
		ld (23296),a
		pop bc

		res 5,(iy+1)        ;signál není stisknuta klávesa
		xor a

		ld a,1
        ld (CURSOR+1),a
        call IN2

		cp 1                    ;testuj BREAK
		jp z,find_end

        ld hl,ALLFILES
		call ROZHOD2
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a
;        dec hl
find0   push hl


		call BUFF83
		call find83
		call BUFF83

		ld hl,(foundfile)
		ld de,ban1
		ld a,0
		call specific_search
		jp z,nesouhlasi
		ld hl,(foundfile)
		ld de,ban2
		ld a,0
		call specific_search
		jp z,nesouhlasi
		
		pop hl

        push hl
        dec hl
		call FINDLFN

		ld hl,23296 + 59		;najdi poslední znak
find1	
		dec hl
		ld a,(hl)
		cp 32
		jr z,find1
		inc hl
		ld a,255
		ld (hl),a

		ld hl,LFNNAME + 261		;najdi poslední znak
find2	
		dec hl
		ld a,(hl)
		cp 32
		jr z,find2
		inc hl
		ld a,255
		ld (hl),a


        ld de,23296
        ld hl,LFNNAME
        call search
aaas    jr nz,nesouhlasi
        ld hl,(foundfile)
        call BUFF83
        set 7,(hl)

        ld hl,numsel
    	call ROZHOD2
        ld (adresasel+1),hl
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        inc hl
        ld a,l
        ld (zvys+1),a
        ld a,h
        ld (zvys2+1),a
adresasel   ld hl,0
zvys    ld (hl),0
        inc hl
zvys2   ld (hl),0        

nesouhlasi
        pop hl
        dec hl
        ld a,l
        or h
        jp nz,find0




find_end
        call loadscr
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
		



			