CRDIRTXT	defb "Create directory.",0
CRDIRTXT2	defb "Please insert name of the new directory:",0

MKDIR	

		call savescr

		ld hl,10 * 256 + 10
		ld bc,60 * 256 + 10
		ld a,16
		call window

		ld hl,11*256+11
		ld a,16
		ld de,CRDIRTXT
		call print

		ld hl,11*256+13
		ld a,16
		ld de,CRDIRTXT2
		call print

		ld hl,56*256+20
		ld a,48
		ld de,createtxt
		call print		

		ld hl,60*256+19
		ld a,16
		ld de,notxt
		call print		


		ld hx,59
		ld hl,$4000 + 15*160 + 11*2
		ld a,80
		call INPUT
		cp 1                    ;testuj BREAK
		jp z,end_mkdir
		ld hl,23296 + 59		;najdi poslední znak
mkdir1	
		dec hl
		ld a,(hl)
		cp 32
		jr z,mkdir1
		inc hl
		ld a,255
		ld (hl),a
MK
		call dospage
		ld a,2
		ld hl,23296		
		call $01b1
		call basicpage
		;call loadscr

;---------------- KONEC --------------------	
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
		jp loop0

end_mkdir
		call loadscr
		jp loop0