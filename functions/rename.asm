
RENAMETXT	defb "Rename",0
RENAMETXT2	defb "Please insert new name:",0

RENAME
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

		ld hl,10 * 256 + 10
		ld bc,60 * 256 + 10
		ld a,16
		call window

		ld hl,11*256+11
		ld a,16
		ld de,RENAMETXT
		call print

		ld hl,11*256+13
		ld a,16
		ld de,RENAMETXT2
		call print

		ld hl,56*256+20
		ld a,48
		ld de,renametxt
		call print		

		ld hl,60*256+19
		ld a,16
		ld de,notxt
		call print		


		ld hx,59
		ld hl,$4000 + 15*160 + 11*2
		ld a,80

	    ei
		ld (INPOS+1),hl     ;ulož adresu začátku pro další použití
        ld (INCOL+1),a      ;ulož barvu
		ld hl,23296         ;do HL adresa editační oblasti
		ld b,hx             ;do B délka editační oblasti
RIN1 	ld (hl),32          ;a nyní celou editační
		inc hl              ;zónu vyplníme mezerami
		djnz RIN1            ;na konec editační zóny
        push bc
        ld hl,LFNNAME
        ld de,23296
        ld bc,58
        ldir
        ld hl,LFNNAME+59

RIN2    ld a,(hl)
        dec hl
        cp 32
        jr z,RIN2
        inc hl
        ld de,LFNNAME
        or a
        sbc hl,de
        ld a,l
        inc a
        ld (CURSOR+1),a

        pop bc
		ld (hl),b           ;přijde 0
		res 5,(iy+1)        ;signál není stisknuta klávesa
		xor a
        call IN2
		cp 1                    ;testuj BREAK
		jp z,end_rename
		ld hl,23296 + 59		;najdi poslední znak
rename1
		dec hl
		ld a,(hl)
		cp 32
		jr z,rename1
		inc hl
		ld a,255
		ld (hl),a

		ld b,11
		ld hl,TMP83
rename0							;vynuluj všechny stavové bity v názvu (7.)
		res 7,(hl)
		inc hl
		djnz rename0
		ld a,$ff
		ld (TMP83+11),a



REN3
		call dospage
		ld a,2
		ld hl,TMP83
        ld de,23296
		call $0127
		call basicpage
		;call loadscr

;---------------- KONEC --------------------
KONEC_OPERACE	
	    ld hl,0
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
		;call loadscr	

		ld hl,pozicel
		call ROZHOD2
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a
		
		ld bc,38 * 256 + 27
		ld a,0
		call window
		
		call reload_dir
	
		ld hl,adrl
		call ROZHOD2
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a
		ld (adrs+1),hl
		call getroot_reload
		call showwin
		ld a,32
		call writecur


		call PROHOD

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

		ld hl,0
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


		ld hl,pozicel
		call ROZHOD2
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a
		
		ld bc,38 * 256 + 27
		ld a,0
		call window
		call reload_dir
	
		ld hl,adrl
		call ROZHOD2
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a
		ld (adrs+1),hl
		call getroot_reload
		call showwin
		call PROHOD

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

end_rename
		call loadscr
		jp loop0