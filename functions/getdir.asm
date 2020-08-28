
GETDIR  
		ld hl,DIRTMP
		ld de,modstart
		ld bc,4
		ldir

        call dospage
              
		ld a,1				;get path
		ld hl,modstart
		call $01b1
		jr c,ok1
    	ld a,2
		out (254),a

ok1		call basicpage

		ld hl,modstart
		ld bc,100
		ld a,#ff
		cpir

		dec hl
		dec hl
		dec hl
		
		push hl
		ld hl,DIRBUFF
		ld de,DIRBUFF+1
		ld bc,13
		ld a,32
		ld (hl),a
		ldir
		pop hl
		
		
pr1		ld a,(hl)
		cp "/"
		jr z,pr0
		cp ":"
		jp z,getdirroot

		dec hl
		jr pr1
		
		
pr0		inc hl

		ld de,DIRBUFF
		ld b,8
presun
		ld a,(hl)
		cp "/"
		jr z,prk
		ld (de),a
		inc de
		inc hl
		djnz presun
		
prk		

		ld hl,DIRBUFF
		ld bc,7
		ld a,"."
		cpir
		jr nz,PRK2
		push hl
		ld de,DIRBUFF+8
		ld bc,4
		ldir
PRK0		
		pop hl				;adresa s teckou v HL
		dec hl
		push hl
		ex de,hl			;je v DE
		ld hl,DIRBUFF+7		;az kam
		or a				
		sbc hl,de
		push hl
		pop bc				;počet bytu
		pop hl
		ld d,h
		ld e,l
		inc de
		ld a,32
		ld (hl),a
		ldir
		
PRK2
		xor a
		ld hl,DIRBUFF+11
		ld (hl),a
		inc hl
		ld (hl),a
		inc hl
		ld (hl),a
		inc hl
		ld (hl),a
		ld hl,DIRBUFF+7
		set 7,(hl)

        call dospage

		xor a 		
		ld hl,parrent
		call $01b1				;skoc do nadrizeneho adresare

		nextreg $55,95
		ld hl,$a000
		ld (FF+1),hl
		xor a
		ld (virtmem),a
        
FF		ld de,$a000
        ld b,pocetpolozek 
        ld hl,stardstar   
		ld c,%101         
		call dos_catalog  
		ld a,pocetpolozek
		xor b
		jr nz,PRK4
		ld hl,(FF+1)
		ld de,pocetpolozek*13
		add hl,de
		ld (FF+1),hl
		ld hl,virtmem
		inc (hl)
		ld a,(hl)
		xor 3
		jr z,PRK4
		jr FF
PRK4		
		ld de,DIRBUFF			;jmeno souboru
		ld hl,stardstar
		ld ix,(savehl)
		ld bc,LFNNAME
		call	$01b7  
						;skoc zpatky do adresare
		

        ld b,11
		ld hl,DIRBUFF
prk00							;vynuluj všechny stavové bity v názvu (7.)
		res 7,(hl)
		inc hl
		djnz prk00

		ld a,255
		ld (DIRBUFF+11),a
		ld hl,DIRBUFF
		xor a	
		call $01b1
        call basicpage
PRK5		
        ld hl,LFNNAME
        ld bc,20
        ld a,255
        cpir
        dec hl

        xor a
		ld (hl),a
        ld (LFNNAME+19),a


		ld hl,dirpos
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        xor a
		ld de,LFNNAME
		call print
		call NOBUFF83
		ret


getdirroot	
    	ld a,"/"
		ld (LFNNAME),a
		xor a
		ld (LFNNAME+1),a
		


		ld hl,dirpos
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        xor a
		ld de,LFNNAME
		call print

		call NOBUFF83
		ret

dirpos      defw 10*256+1
            defw 50*256+1
emptypos    defw 8*256+1
            defw 48*256+1

emptydir    defb "[                     ]",0
parrent 	defb 	"..",#ff
DIRBUFF	    defs 15
modstart    defs 100
lfndir		defb "Dir:", 0
NAZEVADRESARE	ds 20	
