

; Vstup:
;         DE .... vstup hledaneho retezce (ukončený bytem 255)
;         HL .... vstup textu,kde budeme hledat (ukončený bytem 255)
; Výstup:
;         Z ... nalezeno
;         C .. nenalezeno
search		ld (search0+1),de	
search1		
			
			ld a,(de)
			cp "a"
			jr c,search_next
			cp "z"
			jr nc,search_next
			or a
			sbc a,32
			ld (de),a
search_next
			ld a,(hl)
			cp "a"
			jr c,search_next2
			cp "z"
			jr nc,search_next2
			or a
			sbc a,32
			ld (hl),a
search_next2			
			
			ld a,(de)
			cp 255
			ret z		        ;konec, nasli jsme retezec

			ld a,(hl)
			cp 255
			jr z,konec_textu
					
			ld a,(de)           ;porovnání
			xor (hl)

			jr z,search_jo
search0		ld de,0						
			inc hl
			jr search1
search_jo	inc hl
			inc de
			jr search1

konec_textu ld a,1		        ;nastav nz - nic jsme nenasli
			or a
			ret

SELTXT	defb "Search and select files.",0
SELTXT2	defb "Please insert part of name:",0
find_file
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


		ld hx,59
		ld hl,$4000 + 15*160 + 11*2
		ld a,80
		call INPUT
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

FIND
		call BUFF83
		call find83
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
        jr nz,nesouhlasi
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