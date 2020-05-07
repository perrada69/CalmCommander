moredelete


		call savescr

		ld hl,10 * 256 + 10
		ld bc,60 * 256 + 5
		ld a,16
		call window

		ld hl,11*256+11
		ld a,16
		ld de,moredeletetxt
		call print		




		ld hl,numsel
		call ROZHOD2

		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a
		push hl
		
		ex de,hl

		call PROVYP
		add a,a
		ld   (CPPX1+1),hl
        ld   (CPPX3+1),a
        ld   hl,0
        ld   (PROGPROM),hl
		
		ld hl,NUMBUF
		ld de,NUMBUF+1
		ld bc,5
		ld a,32
		ld (hl),a
		ldir
		ld hl,NUMBUF
		xor a
		ld (NUMBUF+3),a
		ld (numadr+1),hl
		pop hl
		call DECIMAL3
		ld hl,18*256+11
		ld a,16
		ld de,NUMBUF
		call print


		ld hl,60*256+15
		ld a,48
		ld de,yestxt
		call print		


		

		ld hl,60*256+14
		ld a,16
		ld de,notxt
		call print		
mdeletewait		
		call INKEY
		cp 1
		jp z,copyend
		cp 13
		jr z,mdeletecont
		jr mdeletewait
		
mdeletecont 

		ld hl,numsel
		call ROZHOD2

		ld a,(hl)
		inc hl
		ld d,(hl)
		ld e,a

		call PROVYP
		add a,a
		ld   (DPPX1+1),hl
        ld   (DPPX3+1),a
        ld   hl,0
        ld   (PROGPROM),hl
		ld   hl,$4000+160*13+23 + 1 + 62
		ld (hl),"|"
		ld hl,0
		
deletenext
		push hl
		call find83
		call BUFF83					
		ld hl,(foundfile)
		bit 7,(hl)	
		jp z,nemazat
		res 7,(hl)


		ld ix,TMP83				;zjisti jestli se jedná o adresář
		bit 7,(ix+7)
		jr nz,isdirdelm
		xor a
		ld (smazdirm+1),a
		jr dcm
isdirdelm
		ld a,1
		ld (smazdirm+1),a
dcm

		ld b,11
		ld hl,TMP83
ACCCAC							;vynuluj všechny stavové bity v názvu (7.)
		res 7,(hl)
		inc hl
		djnz ACCCAC
		ld a,$ff
		ld (TMP83+11),a
		
		call dospage
		
smazdirm	ld a,0
		or a
		jr z,nenidirm
		xor a
		ld (vymaz_vse_v_adresari+1),a

		ld a,3
		ld hl,TMP83
		call $01b1
		call nc,vymaz_vse_v_adresari
		jr pokrm
nenidirm
	
		ld hl,TMP83
		call $0124

pokrm
		
		call basicpage
;------------------------

		ld   hl,(PROGPROM)
        inc  hl
        ld   (PROGPROM),hl		
DPPX1    ld   de,0
         or   a
         sbc  hl,de
         jr   c,DPPNE1
DPPX3    ld   b,1
         ld   (PROGPROM),hl
         call PROGRES
DPPNE1

nemazat

		ld hl,ALLFILES
		call ROZHOD2
		ld a,(hl)
		inc hl
		ld d,(hl)
		ld e,a
		pop hl
		or a
		sbc hl,de
		add hl,de
		jp z,mmorekonec
		inc hl
		jp deletenext

mmorekonec

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
		call loadscr	

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


;---------------------------	
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



delete
DA
		ld hl,numsel
		call ROZHOD2
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a
		or h
		jp nz,moredelete
		
		call savescr
		ld hl,10 * 256 + 10
		ld bc,60 * 256 + 5
		ld a,16
		call window

		ld hl,11*256+11
		ld a,16
		ld de,onedeletetxt
		call print		
		
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
		call find83
		pop hl
		call FINDLFN
		
		ld hl,LFNNAME
		ld de,bfname
		ld bc,35
		ldir
		
		ld hl,34*256+11
		ld a,16
		ld de,bfname
		call print		

		ld hl,60*256+15
		ld a,48
		ld de,yestxt
		call print		

		ld hl,60*256+14
		ld a,16
		ld de,notxt
		call print		
deletewait		
		call INKEY
		cp 1
		jp z,copyend
		cp 13
		jr z,deletecont
		jr deletewait
		
deletecont 
DT
		ld ix,TMP83
		bit 7,(ix+7)
		jr nz,isdirdel
		xor a
		ld (smazdir+1),a
		jr dc
isdirdel
		ld a,1
		ld (smazdir+1),a
dc		ld b,11
		ld hl,TMP83
CCCAC							;vynuluj všechny stavové bity v názvu (7.)
		res 7,(hl)
		inc hl
		djnz CCCAC
		ld a,$ff
		ld (TMP83+11),a
		
		call dospage
smazdir	ld a,0
		or a
		jr z,nenidir
		
		xor a
		ld (vymaz_vse_v_adresari+1),a

		ld a,3
		ld hl,TMP83
ED		call $01b1
		call nc,vymaz_vse_v_adresari
		jr pokr
nenidir
	
		ld hl,TMP83
		call $0124

pokr	
		call basicpage
;------------------------
KON

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
		
		call reload_dir
		call loadscr	

		ld hl,pozicel
		call ROZHOD2
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a
		
		ld bc,38 * 256 + 27
		ld a,0
		call window


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
;---------------------------		
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

errordel	defb "Directory is not empty. Delete?",0

vymaz_vse_v_adresari	
		ld a,0
		or a
		jr nz,smaz
		ld a,1
		ld (vymaz_vse_v_adresari+1),a
	
		call savescr
		ld hl,10 * 256 + 10
		ld bc,60 * 256 + 5
		ld a,16
		call window

		ld hl,11*256+11
		ld a,16
		ld de,errordel
		call print		

		ld hl,60*256+15
		ld a,48
		ld de,yestxt
		call print		

		ld hl,60*256+14
		ld a,16
		ld de,notxt
		call print		
edeletewait		
		call INKEY
		cp 1
		jp z,ecopyend
		cp 13
		jr z,smaz
		jr edeletewait
smaz
		ld hl,60*256+14
		ld a,16
		ld de,pleasewait
		call print		
		

		ld hl,60*256+15
		ld a,16
		ld de,spaces
		call print	



		call dospage



		xor a 			;change path
		ld hl,TMP83
		call $01b1		;změň adresář

		ld hl,vse
		call $0124

		xor a 			;change path
		ld hl,parrentdir
		call $01b1		;změň adresář


		ld a,3			;zkus znova smazat
		ld hl,TMP83
		call $01b1		;změň adresář

ecopyend	call loadscr
		ret
vse			defb "*.*",255
parrentdir 	defb "..",255