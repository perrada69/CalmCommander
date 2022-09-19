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
		xor a
		ld (TLACITKO),a
		call INKEY
		cp 1
		jp z,infoend
		jp nocopy0

move 	ld a,1
		ld (ismove),a
		jr contmov
CCCC
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
COPY	call specific_search
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
		xor a
		ld (TLACITKO),a
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
		
		;call BUFF83			
		
		ld hl,TMP83
		ld de,7
		add hl,de
		bit 7,(hl)
		pop hl
		jp nz,nekopiruj_adresar
		
		
		push hl
		;otestovat, jestli se tam již ten soubor nachází
		ld de,norr
		ld bc,nalezeno_isfile
		call isfile
		pop hl
		push hl
		call openfile
		call createfile
		call readfile		
		ld b,0
		call closefile
		ld b,2
		call closefile
norr	pop hl
		ld a,(ismove)
		or a
		jr z,nenimove
		inc hl
		
		;call BUFF83
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

NN

nenimove
		call obnov_okna


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


obnov_okna
		call prekresli_prazdne_okna
		call PROHOD
		call obnov_jedno_okno
		call GETDIR

		call PROHOD
		call obnov_jedno_okno
		call GETDIR
		ld a,32
		call writecur
		ret	

prekresli_prazdne_okna
		ld hl,0 * 256 + 1
		ld bc,38 * 256 + 27
		ld a,0
		call window
		
		
		
		ld hl,40* 256 + 1
		;	  delka      vyska
		ld bc,38 * 256 + 27
		ld a,0
		call window

		ret

obnov_jedno_okno
		
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
		
		ld hl,pozicel
		call ROZHOD2
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a
		
		ld bc,38 * 256 + 27
		ld a,0
		call draw.window

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
		
		ret

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

		xor a
		ld (TLACITKO),a
		call INKEY
		jp copyend


morecopytxt	defb "Copy     files?",0
moremovetxt	defb "Move     files?",0

moredeletetxt	defb "Delete     files?",0

runtxt	defb "Can you run this file?",0

unsuptxt	defb "Unsuported file!",0

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


		ld hl,60*256+15
		ld a,48
		ld de,yestxt
		call print		
		

		ld hl,60*256+14
		ld a,16
		ld de,notxt
		call print		
acopywait		
		xor a
		ld (TLACITKO),a
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
MMM
moredalsi push hl
		ld (cislo_souboru+1),hl
		call find83
		;call BUFF83					
		ld hl,TMP83
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

		;otestovat, jestli se tam již ten soubor nachází

cislo_souboru
		ld hl,0		
		dec hl
		call FINDLFN
		ld de,norr2
		ld bc,nalezeno_isfile
		call isfile
		;call BUFF83
		pop hl
		push hl

		call openfile
		call createfile
		call readfile		
		ld b,0
		call closefile
		ld b,2
		call closefile
norr2	;call BUFF83
		pop hl
		ld a,(ismove)
		or a
		jp z,nenimove11
		inc hl

		;call BUFF83
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
		
		call obnov_okna


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
		ld (isfilee+1),de
		ld (iskam+1),bc
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
		ld bc,270
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
		;call BUFF83
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

;Adresa kam se skače když soubor je stejný
iskam	jp z,nalezeno_isfile

		dec bc
		ld a,b
		or c
		jr nz,isfile0
		ld a,1
		or a
		jp cont_isfile0



nalezeno_isfile
		call savescr
		ld hl,10 * 256 + 10
		ld bc,60 * 256 + 11
		ld a,16
		call window

;zobraz datum a cas u obou souboru

		ld hl,11*256+17
		ld a,16
		ld de,origin
		call print	

		ld hl,11*256+18
		ld a,16
		ld de,newfile
		call print	

		ld de,(LFNNAME+261+4)
		ld hl,21*256+17
		call showdate
		ld hl,32*256+17
		ld de,(LFNNAME+261+6)
		call showtime

		ld de,(LFNNAME2+261+4)
		ld hl,21*256+18
		call showdate
		ld hl,32*256+18
		ld de,(LFNNAME2+261+6)
		call showtime


		ld hl,42*256+17
		ld a,16
		ld de,sizetxt
		call print
		ld hl,42*256+18
		ld a,16
		ld de,sizetxt
		call print

		ld hl,49*256+17
		ld (dec32pos+1),hl
		ld hl,(LFNNAME+261)
		ld de,(LFNNAME+261+2)
		ld b,10
		ld a,16
		ld (decink+1),a
		call DEC32
		ld a,0
		ld (decink+1),a

		ld hl,60*256+17
		ld a,16
		ld de,bytestxt
		call print


		ld hl,49*256+18
		ld (dec32pos+1),hl
		ld hl,(LFNNAME2+261)
		ld de,(LFNNAME2+261+2)
		ld b,10
		ld a,16
		ld (decink+1),a
		call DEC32
		ld a,0
		ld (decink+1),a

		ld hl,60*256+18
		ld a,16
		ld de,bytestxt
		call print

;***************************************
		ld hl,11*256+11
		ld a,16
		ld de,file_exists_txt
		call print		
		
		ld hl,11*256+13
		ld a,16
		ld de,overwrite_txt
		call print		

		ld hl,11*256+15
		ld a,16
		ld de,namefile
		call print		

		
		ld hl,LFNNAME2
		ld de,bfname
		ld bc,35
		ldir

		ld hl,bfname
		ld bc,35
		ld a,255
		cpir
		dec hl
		ld (hl),32
TTT
		ld hl,bfname+37
		xor a
		ld (hl),a
		ld hl,25*256+15
		ld a,16
		ld de,bfname
		call print		

		ld hl,60*256+21
		ld a,48
		ld de,yestxt
		call print		

		ld hl,60*256+20
		ld a,16
		ld de,notxt
		call print		
.wait		
		xor a
		ld (TLACITKO),a
		call INKEY
		cp 1
		jp z,norewrite
		cp 13
		jr z,cont_isfile
		jr .wait

cont_isfile call loadscr
cont_isfile0
		ld hl,LFNNAME2	;vrat zpatky nalezeny soubor
		ld de,LFNNAME
		ld bc,270
		ldir
						;přepni okno zpátky
		ld a,(OKNO)
		xor 16
		ld (OKNO),a

		ret
norewrite
		pop af			;zrus ze  zasobníku návratovou adresu z CALL
						;přepni okno zpátky
		ld a,(OKNO)
		xor 16
		ld (OKNO),a

		call loadscr
isfilee	jp 0

		