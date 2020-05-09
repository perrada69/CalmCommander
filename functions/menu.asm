menu	
		ld hl,text
		ld de,#4000
		ld bc,80
menu000		
		ld a,(hl)
		ld (de),a
		inc de
		ld a,16
		ld (de),a
		inc de
		inc hl
		dec bc
		ld a,c
		or b
		jr nz,menu000

		call savescr
		
		call show_menu
menu01	

		call INKEY

		cp 1
		jp z,menu_exit
		
		cp 10
		jr z,menudown
		cp 11
		jp z,menuup
		cp 9
		jr z,menu_right
		cp 8
		jr z,menu_left
		jr menu01			
		
menu_exit 
		call loadscr

		ld hl,nadpis
		ld de,#4000
		ld bc,80
menu001		
		ld a,(hl)
		ld (de),a
		inc de
		ld a,16
		ld (de),a
		inc de
		inc hl
		dec bc
		ld a,c
		or b
		jr nz,menu001

		jp loop0

menu_right
		ld a,(nummenu)
		cp 4
		jp z,menu01

		inc a
		ld (nummenu),a
		call loadscr
		ld a,1
		ld (curmeny+1),a
		
		call show_menu
		ld a,64
		call writecurmenu
		jp menu01

menu_left
		ld a,(nummenu)
		or a
		jp z,menu01

		dec a
		ld (nummenu),a
		call loadscr
		ld a,1
		ld (curmeny+1),a
		
		call show_menu
		ld a,64
		call writecurmenu
		jp menu01


menudown
		ld hl,curmeny+1
		ld a,(hl)
		ld hl,pocet_radku+1
		cp (hl)
		jp z,menu01
				
		
		ld a,48
		call writecurmenu
		ld hl,curmeny+1
		ld a,(hl)
		
		inc a
		ld (curmeny+1),a
		ld a,64
		call writecurmenu
		jp menu01

menuup 	ld hl,curmeny+1
		ld a,(hl)
		cp 1
		jp z,menu01
				
		
		ld a,48
		call writecurmenu
		ld hl,curmeny+1
		ld a,(hl)
		
		dec a
		ld (curmeny+1),a
		ld a,64
		call writecurmenu
		jp menu01
		

show_menu
SAS
		ld a,(nummenu)				;zjisteni delky menu
		ld e,a
		ld d,0
		ld hl,menulenght
		add hl,de
		ld a,(hl)
		ld (pocet_radku+1),a
		
		ld hl,menupos				;zjisteni X souradnice
		add hl,de
		ld a,(hl)
		or a
		jr z,sm1
		dec a
		dec a
sm1		ld (menux+1),a
		
		ld b,e
		xor a
sm0		add a,2
		djnz sm0
		
		ld e,a
		ld d,0
		ld hl,menuitems
		add hl,de
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a
		ld (adresa_polozky+1),hl		;uloz definované menu

menux		ld hl,$4000+2
			ld (curmen+1),hl
			inc hl
			ld a,48
			ld (hl),a

			inc hl : inc hl
			ld (hl),a

			inc hl : inc hl
			ld (hl),a

			inc hl : inc hl
			ld (hl),a

			inc hl : inc hl
			ld (hl),a

			inc hl : inc hl
			ld (hl),a
			
			inc hl : inc hl
			ld (hl),a

			
			
			ld h,a
			ld l,1
	
		ld a,16
		
adresa_polozky	ld de,0


		ld a,1
		ld (ymenu+1),a
		ld a,(menux+1)
		rra				;vydel dvema - atributy
		ld (xmenu +1),a
pocet_radku ld b,0
ymenu	ld l,1
xmenu	ld h,0
		ld a,48
		call print
		ld a,(de)
		cp 255
		ret z
		inc de
		inc de
		inc de
		ld hl,ymenu+1
		inc (hl)
		djnz ymenu

		ld a,64
		call writecurmenu
		
		ret
SASS
writecurmenu
			ld (curmencolor+1),a

curmen		ld hl,0
curmeny		ld b,1			
			ld de,160
curmen1		
			add hl,de
			djnz curmen1

			inc hl
curmencolor	ld a,16
			ld (hl),a
			ld b,16
curmen0		inc hl : inc hl
			ld (hl),a
			djnz curmen0
			ret



text 		defb " LEFT  | FILE  | UTILS | RIGHT | QUIT                                                       ",0
nadpis 		defb " Calm Commander 0.1 (Development version)                                                   ",0
menupos		defb 0, 18, 34, 50,66

nummenu		defb 0				;jakou položku zobrazit v menu
menuitems	defw menuleft, menufile, menuutil, menuright, menuquit
menulenght	defb 3, 5, 2, 2, 1	;počet položek v daném menu
menucur 	defb 0
menuleft	defb " RELOAD DIR      ",0
			defw 0
			defb " CHANGE DISC     ",0
			defw 0
			defb " MENU ITEM       ",0
			defw 0
			defb 255
menufile    defb " COPY            ",0
			defw 0
			defb " MOVE            ",0
			defw 0
			defb " MENU ITEM       ",0
			defw 0
			defb " MENU ITEM 1     ",0
			defw 0
			defb " MENU ITEM 2     ",0
			defw 0
			defb 255

menuright	defb " RELOAD DIR      ",0
			defw 0
			defb " MENU ITEM       ",0
			defw 0
			defb 255
menuutil 	defb " SORT DIRECTORY  ",0
			defw 0
			defb " MENU ITEM 1     ",0
			defw 0
			defb " MENU ITEM 2     ",0
			defw 0
			defb 255
menuquit	defb " QUIT CCommander ",0
			defw 0