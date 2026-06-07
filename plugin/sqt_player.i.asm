SQ_STATUS:	db	2

SQ_INIT: 
		ld	de,(RELOC_BASE)		; nacitame prvy offset, z ktoreho
		ld	hl,BUFFER+10		; po odpocitani skutocnej adresy dat
		xor	a			; bez 10 bajtov ziskavame offset,
		sbc	hl,de			; ktorym musime relokovat vsetky
		push	hl			; absolutne ukazovatele v datach muziky
		ld	de,(RELOC_ENDPTR)
		add	hl,de
		pop	bc
RELOCATOR:	dec	hl
		ld	d,(hl)
		dec	hl
		ld	e,(hl)
		ex	de,hl
		add	hl,bc
		ex	de,hl
		ld	(hl),e
		inc	hl
		ld	(hl),d
		dec	hl
		ld	a,l
		sub	RELOC_BASE % 256
		jr	nz,RELOCATOR
 	    ld      (CHKFADEOUT+1),a
        ld      (GLOBVOL+1),a
		dec	a
		ld	(SQ_REST+1),a		; resetujem detektor konca skladby
		ld a,2
		ld (SQ_STATUS),a
		ld	a,8			; inicializacia prehravaca
		ld	(CHNZ1),a
		ld	(CHNZ2),a
		ld	(CHNZ3),a
		ld	bc,$0101
		call	SQ_REND1
CHN_INIT: 	ld	hl,(I_POSITIONS)
		ld	ix,CHNZ1
		call	SQ_I9
		call	SQ_I
		call	SQ_I
SQ_STOP:	ld	de,#073F		; AY registre 7-12 nastav na uplne ticho
CHN_INILOOP:	call	OUT1
		ld	e,0
		inc	d
		ld	a,d
		cp	12
		jr	nz,CHN_INILOOP
		ret

SQ_DEADEND:	inc	(hl)			; prehravanie skoncilo, SQ_PLAY je mrtve
		jr	SQ_STOP

SQ_PLAY:	ld	hl,SQ_SYS
		dec	(hl)
		jr	nz,CHKFADEOUT
SQ_PLAYSPEED:	ld	(hl),0
		inc	hl
		dec	(hl)
		ld	a,(hl)
		or	a
		call	z,SQ_REST
		cp	4
		call	c,SQ_I

SQ_PP		ld	ix,CHNZ1
		ld	c,36
		call	SQ_P
		ld	ix,CHNZ2
		ld	c,18
		call	SQ_P
		ld	ix,CHNZ3
		ld	c,9
		call	SQ_P

CHKFADEOUT:	ld	a,0			; kontrola, ci fadeoutujeme
		or	a
		jr	z,SQ_C
		ld	a,(SQ_STATUS)
		bit	7,a
		jr	nz,SQ_DEADEND

COUNTFADEOUT:	ld	a,0
		dec	a
		ld	(COUNTFADEOUT+1),a
		jr	nz,GLOBVOL
		ld	a,(GLOBVOL+1)
		inc	a
		cp	16
		jr	z,RESETFADEOUT
		ld	(GLOBVOL+1),a
DIVFADEOUT:	ld	a,0
		srl	a
		ld	l,a
		srl	l
		add	a,l
		jr	nz,DIVFADEOUT1
		inc	a
DIVFADEOUT1:	ld	(COUNTFADEOUT+1),a
		ld	(DIVFADEOUT+1),a

GLOBVOL:	ld	a,0			; global attenuation
		ld	(CHNZ1+11),a		; pre vsetky kanaly
		ld	(CHNZ2+11),a
		ld	(CHNZ3+11),a
		bit	3,a			; dosiahla hodnota attenuation
		jr	z,SQ_C			; cislo vacsie ako 8?
		ld	hl,CHNZ1		; tak je nutne upravit sq-flagy,
		ld	de,CHZL 		; aby sa prestali prehravat hw obalky
		ld	c,3			; vo vsetkych troch kanaloch
GLOBVOL1:	res	0,(hl)			; vynulovanim nulteho bytu
		add	hl,de
		dec	c
		jr	nz,GLOBVOL1

SQ_C:		xor	a			; vynuluje mixer register
		ld	l,a
		ld	h,a
		ld	(SQ_N+1),hl
		ld	ix,CHNZ1		; postupne prechadza kanalmi
		call	SQ_R
		call	SQ_R
		call	SQ_R

SQ_N:		ld	bc,0			; nastavenie mixer registra
		ld	a,b			; B = sumove generatory pre ABC
		rla				; C = tonove generatory pre ABC
		rla
		rla
		or	c
		cpl				; potom sa bity komplementuju
SQ_OFF: 	or	0
		ld	e,a
		ld	d,7			; a posielaju na register 7 AY
OUT1:		push	af
		ld	a,d
		cp	8
		jr	c,OUT1_REAL
		cp	11
		jr	nc,OUT1_REAL
		sub	8
		ld	hl,SQ_AY_AMPS
		add	a,l
		ld	l,a
		jr	nc,OUT1_STORE
		inc	h
OUT1_STORE:	ld	(hl),e
OUT1_REAL:	pop	af
		ld	bc,$FFFD
		out	(c),d
		ld	b,$BF
		out	(c),e
		ret

RESETFADEOUT:	ld	hl,-1
		ld	(SQ_SYS),hl
		ld	hl,SQ_STATUS
		set	7,(hl)
		xor	a
		db	1 ; ld bc,NN namiesto ld a,N

ENABLEFADE:	ld	a,48
FORCEFADE:	ld	hl,CHKFADEOUT+1
		inc	(hl)
		ld	(COUNTFADEOUT+1),a
		ld	(DIVFADEOUT+1),a
		cpl
		ld	(SQ_REST+1),a
		ret

SQ_I:		ld	hl,0
SQ_I1:		ld	ix,CHNZ1
SQ_I9:		ld	a,(hl)
		or	a
		jr	nz,SQ_I3
		ld	(SQ_REST+1),a		; oznac, ze pri najblizsom patterne
		ld	hl,(I_REPEAT)		; budeme testovat fadeout alebo noloop
SQ_I3:		ld	b,(hl)
		rl	b
		res	5,(ix+0)
		jr	nc,SQ_I4
		set	5,(ix+0)
SQ_I4:		inc	hl
		ld	a,(hl)
		and	15
		ld	(ix+26),a
		ld	a,(hl)
		and	240
		rra
		rra
		rra
		rra
		cp	9
		jr	c,ZBR
		sub	9
		cpl
ZBR:		ld	(ix+24),a
		inc	hl
		ld	(SQ_I+1),hl
		ld	l,b
		ld	h,0
		ld	de,(I_PATTERNS)
		add	hl,de
		ld	e,(hl)
		inc	hl
		ld	d,(hl)
		inc	de
		ld	(ix+22),e
		ld	(ix+23),d
		ld	de,CHZL
		add	ix,de
		ld	(SQ_I1+2),ix
		ret

SQ_NOFADE:	call	RESETFADEOUT
		pop	af
		ret

SQ_REST:	ld	a,-1
		or	a
		jr	nz,SQ_REST0
		ld	a,(SQ_STATUS)
		rrca			; bit 0. nastaveny = koniec
		jr	c,SQ_NOFADE
		rrca			; bit 1. nastaveny = fadeout
		jr	nc,SQ_REST0
		call	ENABLEFADE
SQ_REST0:	ld	a,(CHNZ1+26)
		ld	(CHNZ1+11),a
		ld	a,(CHNZ2+26)
		ld	(CHNZ2+11),a
		ld	a,(CHNZ3+26)
		ld	(CHNZ3+11),a
		ld	hl,(CHNZ1+22)
		dec	hl
		ld	b,(hl)
		inc	hl
		ld	(CHNZ1+18),hl
		ld	hl,(CHNZ2+22)
		ld	(CHNZ2+18),hl
		ld	hl,(CHNZ3+22)
		ld	(CHNZ3+18),hl
		ld	hl,(CHNZ1+24)
		ld	(CHNZ1+20),hl
		ld	hl,(CHNZ2+24)
		ld	(CHNZ2+20),hl
		ld	hl,(CHNZ3+24)
		ld	(CHNZ3+20),hl
		ld	hl,(SQ_I+1)
		ld	c,(hl)
		inc	hl
		ld	(SQ_I+1),hl
		ld	hl,CHNZ1
		ld	(SQ_I1+2),hl
		ld	a,3
		ld	d,0
SQ_REST1:	res	4,(hl)
		bit	5,(hl)
		jr	z,SQ_REST2
		set	4,(hl)
SQ_REST2:	ld	e,21
		add	hl,de
		ld	(hl),d
		ld	e,CHZL-21
		add	hl,de
		dec	a
		jr	nz,SQ_REST1
SQ_REND1:	ld	(SQ_SYS),bc
		ld	a,c
SQ_REND2:	ld	(SQ_PLAYSPEED+1),a
		ld	a,b
		ret

SQ_P:		ld	a,(ix+21)
		or	a
		jr	z,Y01
		dec	(ix+21)
		bit	7,(ix+0)
		jr	nz,Y33
		ret

Y01:		ld	e,(ix+18)
		ld	d,(ix+19)
		set	6,(ix+0)
		res	7,(ix+0)
		ld	a,(de)
		inc	de
		bit	7,a
		jr	z,Y02

Y05:		ld	(ix+18),e
		ld	(ix+19),d
		ld	b,a
		bit	6,a
		jr	z,Y60

		dec	de
		ld	(ix+27),e
		ld	(ix+28),d
Y34:		and	31
		jp	SQ_SMP

Y60:		bit	5,a
		jr	nz,Y06

		and	15
		bit	4,b
		jr	z,Y07
		neg
Y07:		add	a,(ix+12)
		ld	(ix+12),a
Y33:		ld	e,(ix+27)
		ld	d,(ix+28)
		res	6,(ix+0)
		ld	a,(de)
		bit	7,a
		jr	nz,Y34
		inc	de
		jp	SMP_ORN

Y06:		and	15
		ld	(ix+21),a
		bit	4,b
		ret	z
		or	a
		jr	z,Y33
		set	7,(ix+0)
		jr	Y33

Y02:		cp	96
		jp	c,Y03
		sub	96
		cp	15
		jr	c,Y04

		ld	hl,SQ_OFF+1
		ld	b,a
		ld	a,(hl)
		or	c
		ld	(hl),a
		set	3,(ix+0)
		ld	a,b
		sub	15
		jp	z,Z26

Y04:		dec	a
		ex	de,hl
		ld	c,(hl)
		inc	hl
		bit	6,(ix+0)
		jr	z,Y69
		ld	(ix+18),l
		ld	(ix+19),h
		res	6,(ix+0)
Y69:		cp	8
		jr	c,Z38
		set	0,(ix+0)
		ld	l,c
		ld	e,a
		ld	d,13
		call	OUT1
		ld	d,11
		ld	e,l
		jp	OUT1

Z38:		cp	6			; channel volume set
		jr	nc,Z36
		bit	4,(ix+0)
		ret	z
		or	a
		jr	nz,Z31
		ld	a,c
SQ_V:		and	15
		ld	(ix+11),a
		ret

Z31:		dec	a			; channel volume slide
		jr	nz,Z32
		ld	a,c
		add	a,(ix+11)
		jr	SQ_V

Z32:		dec	a			; global volume set
		jr	nz,Z33
		ld	a,c
		ld	(CHNZ1+11),a
		ld	(CHNZ2+11),a
		ld	(CHNZ3+11),a
		ret

Z33:		dec	a			; global volume slide
		jr	nz,Z34
		ld	b,3
		ld	de,CHZL
		ld	hl,CHNZ1+11
Z33_2:		ld	a,(hl)
		add	a,c
		and	15
		ld	(hl),a
		add	hl,de
		djnz	Z33_2
		ret

Z34:		ld	hl,SQ_SYS		; speed set
		dec	a
		jr	nz,Z35
		ld	a,c
SQ_S:		and	31
		jr	nz,SQ_Z
		ld	a,32
SQ_Z:		ld	(hl),a
		jp	SQ_REND2

Z35:		ld	a,(hl)			; speed slide
		add	a,c
		jr	SQ_S

Z36:		sub	6
		ld	b,0
		ld	a,c
		ld	c,b
		jr	nz,Z37
		dec	b
		neg
Z37:		set	2,(ix+0)
		ld	(ix+13),c
		ld	(ix+14),c
		ld	(ix+15),a
		ld	(ix+16),b
		ret

Y03:		ld	(ix+12),a
		dec	de
		ld	(ix+27),e
		ld	(ix+28),d
		inc	de
		call	SMP_ORN
		bit	6,(ix+0)
		ret	z
Z26:		ld	(ix+18),e
		ld	(ix+19),d
		ret

SMP_ORN:	ld	a,(de)
		inc	de
		bit	7,a
		jr	z,SMP_ORN9
		ld	b,a
		rra
		and	31
		call	nz,SQ_SMP
		bit	6,b
		ret	z
		ld	a,(de)
		and	240
		rr	b
		rra
		rra
		rra
		srl	a
		call	nz,SQ_ORN
		ld	a,(de)
		inc	de
		and	15
		ret	z
SMP_ORN9:	jp	Y04

SQ_SMP: 	push	bc
		add	a,a
		ld	c,a
		ld	b,0
		ld	a,(ix+0)
		and	%11110000
		ld	(ix+0),a
		ld	hl,(I_SAMPLES)
		add	hl,bc
		ld	c,(hl)
		inc	hl
		ld	b,(hl)
		push	ix
		pop	hl
		inc	hl
		ld	(hl),c
		inc	hl
		ld	(hl),b
		inc	bc
		inc	bc
		inc	hl
		ld	(hl),c
		inc	hl
		ld	(hl),b
		inc	hl
		ld	(hl),32
		inc	hl
		ld	(SQ_NXT+1),hl
		pop	bc
		ld	hl,SQ_OFF+1
		ld	a,(hl)
		or	c
		xor	c
		ld	(hl),a
		ret

SQ_ORN: 	add	a,a
		ld	c,a
		ld	b,0
		ld	hl,(I_ORNAMENTS)
		add	hl,bc
		ld	c,(hl)
		inc	hl
		ld	b,(hl)
SQ_NXT: 	ld	hl,0
		ld	(hl),c
		inc	hl
		ld	(hl),b
		inc	hl
		inc	bc
		inc	bc
		ld	(hl),c
		inc	hl
		ld	(hl),b
		inc	hl
		ld	(hl),32
		set	1,(ix+0)
		ret

OUT2:		ld	hl,SQ_N+1		; podla carry nastavi v SQ_N
		rl	(hl)			; tonovy generator, sumovy off
		inc	hl			; potom vytiahne cislo kanalu
		rl	(hl)
		ld	a,(ix+17)
		add	a,8			; pripocita k nemu 8
		out	(c),a			; tj. volume registre AY
		ld	b,$BF
		out	(c),e			; a posle hodnotu v E
		jp	SQ_ZCH

SQ_R:		ld	l,(ix+3)
		ld	h,(ix+4)
		ld	bc,$FFFD
		ld	d,(ix+0)
		ld	e,0
		bit	3,d			; ak sa nema nic hrat, umlcat
		jr	nz,OUT2
		ld	a,(hl)
		and	15
		jp	nz,SQ_R1
		bit	0,d			; ak sa ma hrat obalka, nastav
		jr	z,SQ_R2
		ld	e,16
		jp	SQ_R2

SQ_R1:		sub	(ix+11) 		; od hlasitosti z sa odpocita
		jr	c,SQ_R2 		; global volume nastavenie
		ld	e,a

SQ_R2:		ld	a,(ix+17)		; vytiahne sa cislo kanalu
		or	a
		jr	z,SQ_R2_A
		dec	a
		jr	z,SQ_R2_B
		ld	a,e
		ld	(SQ_AY_AMPS+2),a
		ld	a,10
		jr	SQ_R2_OUT
SQ_R2_B:	ld	a,e
		ld	(SQ_AY_AMPS+1),a
		ld	a,9
		jr	SQ_R2_OUT
SQ_R2_A:	ld	a,e
		ld	(SQ_AY_AMPS),a
		ld	a,8
SQ_R2_OUT:	out	(c),a			; na registre hlasitosi AY
		ld	b,$BF
		out	(c),e
		ld	a,(hl)
		inc	hl
		and	240			; vytiahujem sumove data
		rra
		rra
		rra
		ld	d,6
		ld	e,(hl)
		rl	e
		bit	5,(hl)			; zistujem, ci budeme sumiet
		jr	z,SQ_ZNN
		adc	a,0
		ld	b,$FF
		out	(c),d
		ld	b,$BF
		out	(c),a
SQ_ZNN:		ld	a,e
		rla
		ex	de,hl
		ld	hl,SQ_N+1		; nastavime v SQ_N stavy oboch
		rl	(hl)			; generatorov (sum/ton) pre ch.
		inc	hl
		rla
		rl	(hl)
		ex	de,hl
		ld	a,(hl)			; vypocitavanie frekvencie...
		and	31
		ld	d,a
		inc	hl
		ld	e,(hl)
		inc	hl
		push	de
		ld	d,0
		dec	(ix+5)
		jp	nz,FQ_2
		ld	l,(ix+1)
		ld	h,(ix+2)
		ld	a,(hl)
		inc	hl
		cp	32
		ld	c,(hl)
		inc	hl
		jr	nz,FQ_1
		set	3,(ix+0)
		res	1,(ix+0)
FQ_1:		ld	b,a
		add	a,a
		add	a,b
		ld	e,a
		add	hl,de
		ld	(ix+5),c
FQ_2:		ld	(ix+3),l
		ld	(ix+4),h
		ld	a,(ix+12)
		bit	1,(ix+0)
		jr	z,FQ_5
		ld	l,(ix+8)
		ld	h,(ix+9)
		add	a,(hl)
		inc	hl
		dec	(ix+10)
		jp	nz,FQ_4
		ex	af,af'
		ld	l,(ix+6)
		ld	h,(ix+7)
		ld	a,(hl)
		inc	hl
		cp	32
		ld	e,b
		jr	z,FQ_3
		ld	c,(hl)
		ld	e,a
FQ_3:		inc	hl
		add	hl,de
		ld	(ix+10),c
		ex	af,af'
FQ_4:		ld	(ix+8),l
		ld	(ix+9),h
FQ_5:		add	a,(ix+20)
		cp	45
		jr	nc,FQ_6
		add	a,a
		ld	e,a
		ld	hl,FRQ2
		add	hl,de
		ld	d,(hl)
		inc	hl
		jp	FQ_7

FQ_6:		ld	hl,FRQ1-45
		ld	e,a
		add	hl,de
FQ_7:		ld	e,(hl)
		ex	de,hl
		pop	de			; ...frekvenciu mame,
		bit	4,d			; bude este fine-tuning?
		res	4,d
		jr	z,FQ_9
		add	hl,de
		db	1 ; ld bc,NN namiesto sbc hl,de
FQ_9:		sbc	hl,de
		bit	2,(ix+0)
		jr	z,OUT9
		ld	c,(ix+13)
		ld	b,(ix+14)
		add	hl,bc
		ex	de,hl
		ld	l,(ix+15)
		ld	h,(ix+16)
		add	hl,bc
		ld	(ix+13),l
		ld	(ix+14),h
		ex	de,hl

OUT9:		ld	a,(ix+17)		; vytiahi cislo kanalu
		add	a,a			; vynasob dvoma
		ld	bc,$FFFD		; a naprogramuj freq tonu
		out	(c),a
		ld	b,$BF
		out	(c),l
		inc	a
		ld	b,$FF
		out	(c),a
		ld	b,$BF
		out	(c),h
SQ_ZCH: 	ld	de,CHZL 		; prejdi s IX na dalsi kanal
		add	ix,de
		ret

FRQ2:		db	13,93,12,156
		db	11,231,11,60
		db	10,155,10,2,9,115
		db	8,235,8,107,7,242
		db	7,128,7,20,6,174
		db	6,78,5,244,5,158
		db	5,79,5,1,4,185
		db	4,117,4,53,3,249
		db	3,192,3,138,3,87
		db	3,39,2,250,2,207
		db	2,167,2,129,2,93
		db	2,59,2,27,1,252
		db	1,224,1,197,1,172
		db	1,148,1,125,1,104
		db	1,83,1,64
		db	1,46,1,29,1,13

FRQ1:		db	254,240,226,214
		db	202,190,180,170
		db	160,151,143,135
		db	127,120,113,107
		db	101,95,90,85,80
		db	76,71,67,64,60,57
		db	53,50,48,45,42,40
		db	38,36,34,32,30,28
		db	27,25,24,22,21,20
		db	19,18,17,16,15,14

CHNZ1:	db	0
		dw	0,0,0,0,0,0,0,0
		dw	2,0,0,0,0,0
CHNZ2:	db	0
		dw	0,0,0,0,0,0,0,0
		dw	1,0,0,0,0,0
CHNZ3:	db	0
		dw	0,0,0,0,0,0,0,0
		dw	0,0,0,0,0,0

SQ_SYS: 	dw	#0101

CHZL:		equ	CHNZ2-CHNZ1	; offset medzi kanalmi
SQ_AY_AMPS:	db	0,0,0

