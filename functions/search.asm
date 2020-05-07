

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