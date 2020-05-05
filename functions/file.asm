createfile

		ld a,(OKNO)
		xor 16
		ld (OKNO),a


		call dospage
		
		ld hl,pathl
		call ROZHOD2
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a
		
		xor a
		call $01b1

		ld b,2 				;soubor číslo 1
		ld c,2				;exclusive WRITE
		ld d,2
		ld e,4
		ld hl,LFNNAME
		
		call 0106h			;create file



		ld a,(OKNO)
		xor 16
		ld (OKNO),a
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

closefile
		push bc
		call dospage
		pop bc
		call 0109h 
		call basicpage
		ret

readfile
READ
		call dospage
		call NOBUFF83
read0	


		 ld   hl,(PROGPROM2)
         inc  hl
         ld   (PROGPROM2),hl		
CPPX2    ld   de,0
         or   a
         sbc  hl,de
         jr   c,CPPNE2
CPPX4    ld   b,1
         ld   (PROGPROM2),hl
         call PROGRES2
CPPNE2


		ld b,0
		ld c,3
		ld de,blocklenght				;počet bytu
		ld hl,FILEBUFF
		call 0112h
		jr nc, konec

		ld c,3
		ld b,2
		ld de,blocklenght
		ld hl,FILEBUFF
		call 115h


		jr read0
konec

		ld c,3
		ld b,2
		ld de,blocklenght
		ld hl,FILEBUFF
		call 115h

		call basicpage
		ret



openfile
		
		push hl
		
		inc hl
		
		call BUFF83
		call find83
		pop hl
		push hl
		call FINDLFN
		
		ld hl,LFNNAME
		ld de,bfname
		ld bc,45
		ldir
		
		ld hl,11*256+15
		ld a,16
		ld de,bfname
		call print				
		
		
		call BUFF83
		ld hl,(TMP83+11)
ZDE
		ld c,6
		call deleno

		inc hl
of0
		ex de,hl
		call PROVYP
		add a,a
		ld   (CPPX2+1),hl
        ld   (CPPX4+1),a
        ld   hl,0
        ld   (PROGPROM2),hl
		ld   hl,$4000+160*14+23
		ld (PROGS2+1),hl
		call clearpr
		
		
		pop hl
LF		
		call FINDLFN
						;na adrese TMP83 je adresa
		ld hl,LFNNAME+255
		ld (hl),255
		ld b,11
		ld hl,TMP83
CCCA							;vynuluj všechny stavové bity v názvu (7.)
		res 7,(hl)
		inc hl
		djnz CCCA
		ld hl,TMP83+10		;najdi poslední znak názvu souboru/adresare
achng2	ld a,(hl)
		cp 32
		jr nz,azap
		dec hl
		jr achng2
azap		ld a,255
		inc hl
		ld (hl),a

		ld hl,LFNNAME+254		;najdi poslední znak LFN souboru/adresare
achnga2	ld a,(hl)
		cp 32
		jr nz,azapa
		dec hl
		jr achnga2
azapa	ld a,255
		inc hl
		ld (hl),a

							

		call dospage
		ld b,0 				;soubor číslo 0
		ld c,1				;exclusive READ
		ld e,2
		ld hl,TMP83
		
		
		call 0106h			;open file

		call basicpage
		ret
