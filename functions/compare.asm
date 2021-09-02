

;   Porovnání adresářů
;
;   Porovnává dva adresáře - v levém a pravém panelu. Společné soubory se označují.

compare_dirs
        call savescr
		call compare.deselect
		call prohod_okno
		call compare.deselect
		call prohod_okno
        ld hl,10 * 256 + 10
		ld bc,60 * 256 + 1
		ld a,16
		call window

		ld hl,11*256+11
		ld a,16
		ld de,please_wait
		call print



        ld hl,ALLFILES
		call ROZHOD2
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a

comp0   push hl
        ld (primary_window+1),hl
        
		call find83
		pop hl
        push hl
        dec hl
		call FINDLFN

        call prohod_okno

        ;Presun soubor do LFNNAME2 a hned skoc na porovnavani se soubory v druhém okně
        ld hl,LFNNAME
        ld de,LFNNAME2
        ld bc,270
        ldir

        call other_window
       
        call prohod_okno

        pop hl
        dec hl
        ld a,l
        or h
        jp nz,comp0
        call invert_body

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
        call prohod_okno
        call invert_body

        ld hl,adrl
		call ROZHOD2
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a
		ld (adrs+1),hl
		call getroot_reload
		call showwin	     
        call prohod_okno

        ld a,32
		call writecur
		call zobraz_nadpis
        jp loop0

PROO
prohod_okno
        ld a,(OKNO)
        xor 16
        ld (OKNO),a
        ret

other_window
        ld hl,ALLFILES
		call ROZHOD2
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a

other0  push hl
        ld (second_window+1),hl
		call find83
		pop hl
        push hl
        dec hl
		call FINDLFN

        call porovnej_dva_lfn
        pop hl

        jr z,second_window
		dec hl
        ld a,l
        or h
        jp nz,other0
        ret

second_window
        ld hl,0
        call oznac_soubor_dle_pozice_v_hl
        call prohod_okno

primary_window
        ld hl,0
        call oznac_soubor_dle_pozice_v_hl
        call prohod_okno

        ret        


porovnej_dva_lfn
        ld hl,LFNNAME+260
por0	ld a,(hl)

		dec hl
		cp 32
		jr z,por0
		inc hl
		inc hl
		ld (hl),255		

		inc hl
		xor a
		ld (hl),a


        ld hl,LFNNAME2+260
por1	ld a,(hl)

		dec hl
		cp 32
		jr z,por1
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
        ret


		module compare
deselect
		
		;jp dfind_end
		ld a,255
		ld (23296),a
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
        ld (dadresasel+1),hl
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
dzvys   ld (hl),0
        inc hl
dzvys2  ld (hl),0        

dnesouhlasi
        pop hl
        dec hl
        ld a,l
        or h
        jp nz,dfind0
		
dfind_end
       ret     
 


		endmodule