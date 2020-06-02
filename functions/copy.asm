onecopytxt 	defb "Copy file: ",0
onemovetxt	defb "Move file: ",0
onedeletetxt 	defb "Delete file/directory: ",0
yestxt		defb "ENTER = yes",0
yesall		defb "SPACE = yes to all directory",0
yesallsp	defb "                            ",0

createtxt	defb "ENTER = create",0
renametxt	defb "ENTER = rename",0
searchtxt	defb "ENTER = search",0
savetxt		defb "ENTER = save ",0
notxt 		defb "BREAK = no",0
spaces 		defb "           ",0
pleasewait	defb "Please wait",0
bfname		defs 45
			defb 0

ismove	defb 0

nocopy	defb "I'm sorry, but you can't copy or move files to the same",0
nocopy2 defb "directory.",0

no_copy_move
	
		call savescr
		ld hl,8 * 256 + 10
		ld bc,60 * 256 + 3
		ld a,16
		call window

		ld hl,11*256+11
		ld a,16
		ld de,nocopy
		call print
		ld hl,11*256+12
		ld a,16
		ld de,nocopy2
		call print

		ld hl,44*256+13
		ld a,32
		ld de,breaktxt
		call print

nocopy0
		call INKEY
		cp 1
		jp z,infoend
		jp nocopy0

move 	ld a,1
		ld (ismove),a
		jr contmov

copy 	xor a
		ld (ismove),a

contmov
		ld hl,PATHLEFT
		ld a,255
		ld bc,261
		cpir
		ld (hl),0

		ld hl,PATHRIGHT
		ld a,255
		ld bc,261
		cpir
		ld (hl),0

		ld hl,PATHLEFT
		ld de,PATHRIGHT
		ld a,0
COPY		call specific_search
		jp z,no_copy_move

		ld hl,numsel
		call ROZHOD2
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a
		or h
		jp nz,morecopy
		
		call savescr
		ld hl,10 * 256 + 10
		ld bc,60 * 256 + 5
		ld a,16
		call window

		ld hl,11*256+11
		
		ld de,onecopytxt
		ld a,(ismove)
		or a
		jr z,$+4
		ld de,onemovetxt
		ld a,16
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
		ld bc,45
		ldir
		
		ld hl,22*256+11
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
copywait		
		call INKEY
		cp 1
		jp z,copyend
		cp 13
		jr z,copycont
		jr copywait
		
copycont 

		ld   hl,$4000+160*14+23 + 1 + 62
		ld (hl),"|"

		ld hl,60*256+14
		ld a,16
		ld de,pleasewait
		call print		

		ld hl,60*256+15
		ld a,16
		ld de,spaces
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
		
		call BUFF83			
		
		ld hl,(foundfile)
		ld de,7
		add hl,de
		bit 7,(hl)
		pop hl
		jp nz,nekopiruj_adresar
		
		
		push hl
		;otestovat, jestli se tam již ten soubor nachází
		call isfile
IS		pop hl
		push hl
		call openfile
		call createfile
		call readfile		
		ld b,0
		call closefile
		ld b,2
		call closefile
		pop hl
		ld a,(ismove)
		or a
		jr z,nenimove
		inc hl
		
		call BUFF83
		call find83
		ld b,11
		ld hl,TMP83
CCCAC20							;vynuluj všechny stavové bity v názvu (7.)
		res 7,(hl)
		inc hl
		djnz CCCAC20
		ld a,$ff
		ld (TMP83+11),a
		
		call dospage
		
		ld hl,TMP83
		call $0124
		
		call basicpage
		jp mmorekonec



nenimove
		call PROHOD
		
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
		
copyend	
			ld hl,numsel
			call ROZHOD2
			xor a
			ld (hl),a
			inc hl
			ld (hl),a
		
		call loadscr
		jp loop0


nodirtxt	defb "You cannot copy the directory.",0
pressanykeytxt	defb "Press any key to continue.",0

nekopiruj_adresar

		ld hl,10 * 256 + 10
		ld bc,60 * 256 + 5
		ld a,16
		call window

		ld hl,11*256+11
		ld a,16
		ld de,nodirtxt
		call print



		ld hl,11*256+15
		ld a,48
		ld de,pressanykeytxt
		call print


		call INKEY
		jp copyend


morecopytxt	defb "Copy     files?",0
moremovetxt	defb "Move     files?",0

moredeletetxt	defb "Delete     files?",0

nowcopy	defb "Copy     file from     files",0
actcount defw 0
morecopy
		ld hl,0
		ld (actcount),hl
		call savescr

		ld hl,10 * 256 + 10
		ld bc,60 * 256 + 5
		ld a,16
		call window

		ld hl,11*256+11

		ld de,morecopytxt
		ld a,(ismove)

		or a
		jr z,$+4
		ld de,moremovetxt

		ld a,16
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
		ld hl,16*256+11
		ld a,16
		ld de,NUMBUF
		call print

		
;*****seem
		ld hl,60*256+15
		ld a,48
		ld de,yestxt
		call print		
		

		ld hl,60*256+14
		ld a,16
		ld de,notxt
		call print		
acopywait		
		call INKEY
		cp 1
		jp z,copyend
		cp 13
		jr z,acopycont
		jr acopywait
		
acopycont 

		ld hl,60*256+14
		ld a,16
		ld de,pleasewait
		call print		
		

		ld hl,60*256+15
		ld a,16
		ld de,spaces
		call print	

		ld   hl,$4000+160*15+23 + 1 + 62
		ld (hl),"|"
		ld   hl,$4000+160*14+23 + 1 + 62
		ld (hl),"|"

		ld hl,0

moredalsi push hl
		
		call find83
		call BUFF83					
		ld hl,(foundfile)
		bit 7,(hl)	
		jp z,nekopirovat
		push hl

		ld hl,11*256+11
		ld a,16
		ld de,nowcopy
		call print		

		ld hl,(actcount)
		inc hl
		ld 	(actcount),hl
		push hl
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
		ld hl,16*256+11
		ld a,16
		ld de,NUMBUF
		call print



		ld hl,numsel
		call ROZHOD2

		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a
		push hl
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
		ld hl,30*256+11
		ld a,16
		ld de,NUMBUF
		call print		
		
		pop hl
		res 7,(hl)			;zrus oznaceni
		ld de,7
		add hl,de
		bit 7,(hl)
		jr nz,NODIR		;preskoč adresar
		pop hl
		push hl
		dec hl
		push hl
		call openfile
		call createfile
		call readfile		
		ld b,0
		call closefile
		ld b,2
		call closefile
		pop hl
		ld a,(ismove)
		or a
		jp z,nenimove11
		inc hl

		call BUFF83
		call find83
		ld b,11
		ld hl,TMP83
CCCAC21							;vynuluj všechny stavové bity v názvu (7.)
		res 7,(hl)
		inc hl
		djnz CCCAC21
		ld a,$ff
		ld (TMP83+11),a
		
		call dospage
		
		ld hl,TMP83
		call $0124
		
		call basicpage

nenimove11

NODIR   ld   hl,(PROGPROM)
        inc  hl
        ld   (PROGPROM),hl		
CPPX1    ld   de,0
         or   a
         sbc  hl,de
         jr   c,CPPNE1
CPPX3    ld   b,1
         ld   (PROGPROM),hl
         call PROGRES
CPPNE1

nekopirovat
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
		jr z,morekonec
		inc hl
		jp moredalsi
morekonec

		ld a,(ismove)
		or a
		jr z,nenimove2
		jp mmorekonec

nenimove2
		call PROHOD
		
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



		ld hl,numsel
		call ROZHOD2
		xor a
		ld (hl),a
		inc hl
		ld (hl),a





		call freespace
		jp loop0



isfile
ISFILE
		ld hl,LFNNAME+260
is0		ld a,(hl)

		dec hl
		cp 32
		jr z,is0
		inc hl
		inc hl
		ld (hl),255		

		inc hl
		xor a
		ld (hl),a
		ld hl,LFNNAME
		ld de,LFNNAME2
		ld bc,261
		ldir
		ld a,(OKNO)
		xor 16
		ld (OKNO),a

		ld hl,ALLFILES
		call ROZHOD2
		ld a,(hl)
		inc hl
		ld b,(hl)
		ld c,a
isfile0
		push bc
		ld h,b
		ld l,c
		push hl
        inc hl
		call BUFF83
		call find83
        pop hl
		dec hl
        call FINDLFN

		ld hl,LFNNAME+260
is01	ld a,(hl)

		dec hl
		cp 32
		jr z,is01
		inc hl
		inc hl
		ld (hl),255		
		inc hl
		xor a
		ld (hl),a
		
		ld hl,LFNNAME
		ld de,LFNNAME2
		ld a,0
		call specific_search
		pop bc
		jp z,nalezeno_isfile

		dec bc
		ld a,b
		or c
		jr nz,isfile0
		ld a,1
		or a
nalezeno_isfile
		push af

		ld hl,LFNNAME2	;vrat zpatky nalezeny soubor
		ld de,LFNNAME
		ld bc,261
		ldir

						;přepni okno zpátky
		ld a,(OKNO)
		xor 16
		ld (OKNO),a
		pop af
		ret

		