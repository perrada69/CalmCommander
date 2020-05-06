
SELDATA  equ  49152

;Označí konkrétní bit - identifikace označeného souboru
;Vstup: HL = číslo souboru

SELTED  ld   de,SELDATA
        xor  a
        srl  h
        rr   l
        rra  
        srl  h
        rr   l
        rra  
        srl  h
        rr   l
        rra  
        rrca 
        rrca 
        rrca 
        rrca 
        rrca 
        add  hl,de
        ex   de,hl
        ld   hl,TABBITS
        add  a,l
        ld   l,a
        jr   nc,SELC2
        inc  h
SELC2   ld   a,(de)
        and  (hl)
        ret 

GETSELF ld   hl,numsel	;najde prvni oznacenou polozku, v pripade
        call ROZHOD2	;ze neni oznacena, zadny soubor (adresar)
        ld   a,(hl)		;tak to vyhodi pozici kurzoru
        inc  hl			;vysledek je v registru ´hl´
        or   (hl)
        ld   hl,0
        jr   nz,GETSELN3
        ld   hl,KURZL
        call ROZHOD2
        ld   a,(hl)
        inc  hl
        ld   h,(hl)
        ld   l,a
        or   h
        ret  

GETSELN3 
        ld   hl,dirl
        call rozhod
        ld   a,(hl)
        ld   hl,0
        or   a
        jr   nz,GETSELN
        ld   hl,-1
GETSELN inc  hl			;najde dalsi polozku, ktera je oznacena, v hl
		push de			;musi byt posledni nalezena a vysledek je zase 
        ld   de,65535	;v registru ´hl´
        or   a
        sbc  hl,de
        add  hl,de
        pop  de
        ret  z
        push hl
        call SELTED
        pop  hl
        jp   z,GETSELN
        ret  


TABBITS  db 1,2,4,8,16,32,64
         db 128         