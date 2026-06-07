		module stc

music_init:	ld	hl,songdata
music_init0:	ld	a,(hl)
		ld	(speed),a
		ld	(module_addr+1),hl
		inc	hl
		call	add_offset
		ld	a,(de)
		inc	de
		inc	a
		ld	(song_length),a
		ld	(positions),de
		call	add_offset
		ld	(ornaments),de
		push	de
		call	add_offset
		ld	(patterns),de
		ld	hl,HEADLEN
		call	module_addr
		ex	de,hl
		ld	(samples),hl
		ld	hl,empty_pattern
		ld	(chn1_pattptr),hl
		inc	hl
		ld	de,CHN1+1
		ld	bc,VARSLEN
		ld	(hl),b
		ldir
		pop	hl
		ld	bc,ORNALEN
		xor	a
		call	scan_until
		ld	(apply_volenv+1),a
		ld	(chkfadeout+1),a
		dec	a
		ld	(CHN1+CHP.SmpCnt),a
		ld	(CHN2+CHP.SmpCnt),a
		ld	(CHN3+CHP.SmpCnt),a
		ld	a,1
		ld	(tempo_cnt),a
		inc	hl
		ld	(CHN1+CHP.OrnPtr),hl
		ld	(CHN2+CHP.OrnPtr),hl
		ld	(CHN3+CHP.OrnPtr),hl
		ld	hl,music_setup
		res	7,(hl)
		jp	music_mute

scan_until:	cp	(hl)
		ret	z
		add	hl,bc
		jr	scan_until

add_offset:	ld	e,(hl)
		inc	hl
		ld	d,(hl)
		inc	hl
		ex	de,hl
module_addr:	ld	bc,0
		add	hl,bc
		ex	de,hl
		ret

new_position:	ld	a,(current_pos)
		ld	c,a
		ld	hl,song_length
		cp	(hl)
		jr	c,.continue
		ld	hl,music_setup
		bit	1,(hl)
		call	nz,enablefade
		bit	0,(hl)
		jp	nz,noloop
		xor	a
		ld	c,a
.continue:	inc	a
		ld	(current_pos),a
		ld	l,c
		ld	h,0
		add	hl,hl
		ld	de,(positions)
		add	hl,de
		ld	c,(hl)
		inc	hl
		ld	a,(hl)
		ld	(get_frequency+1),a
		ld	a,c
		ld	hl,(patterns)
		ld	bc,PATTLEN
		call	scan_until
		inc	hl
		call	add_offset
		ld	(chn1_pattptr),de
		call	add_offset
		ld	(chn2_pattptr),de
		call	add_offset
		ld	(chn3_pattptr),de
		ret

adv_patt_step:	dec	(ix+CHP.PatStpCnt)
		ret	p
		ld	a,(ix+CHP.PatStep)
		ld	(ix+CHP.PatStpCnt),a
		ret

;------------------------------------------------------------------------------
music_play:	ld	a,(tempo_cnt)
		dec	a
		ld	(tempo_cnt),a
		jr	nz,playback
		ld	a,(speed)
		ld	(tempo_cnt),a

.chn1:		ld	ix,CHN1
		call	adv_patt_step
		jp	p,.chn2
		ld	hl,(chn1_pattptr)
		ld	a,(hl)
		inc	a
		call	z,new_position
		ld	hl,(chn1_pattptr)
		call	read_pattern
		ld	(chn1_pattptr),hl

.chn2:		ld	ix,CHN2
		call	adv_patt_step
		jp	p,.chn3
		ld	hl,(chn2_pattptr)
		call	read_pattern
		ld	(chn2_pattptr),hl

.chn3:		ld	ix,CHN3
		call	adv_patt_step
		jp	p,playback
		ld	hl,(chn3_pattptr)
		call	read_pattern
		ld	(chn3_pattptr),hl

;------------------------------------------------------------------------------
playback:	call	chkfadeout
		ld	ix,CHN1
		call	adv_sample
		ld	a,c
		ld	(sample_index+1),a
		call	read_sample
		ld	a,c
		or	b
		rrca
		ld	(AYREGS+AR.Mixer),a
		ld	a,(ix+CHP.SmpCnt)
		inc	a
		jr	z,.chn2
		call	set_noise
		call	get_tone
		ld	(AYREGS+AR.TonA),hl

.chn2:		ld	hl,AYREGS+AR.AmplA
		call	apply_volenv
		ld	ix,CHN2
		call	adv_sample
		ld	a,(ix+CHP.SmpCnt)
		inc	a
		jr	z,.chn3
		ld	a,c
		ld	(sample_index+1),a
		call	read_sample
		ld	a,(AYREGS+AR.Mixer)
		or	c
		or	b
		ld	(AYREGS+AR.Mixer),a
		call	set_noise
		call	get_tone
		ld	(AYREGS+AR.TonB),hl

.chn3:		ld	hl,AYREGS+AR.AmplB
		call	apply_volenv
		ld	ix,CHN3
		call	adv_sample
		ld	a,(ix+CHP.SmpCnt)
		inc	a
		jr	z,.finish
		ld	a,c
		ld	(sample_index+1),a
		call	read_sample
		ld	a,(AYREGS+AR.Mixer)
		rlc	c
		rlc	b
		or	b
		or	c
		ld	(AYREGS+AR.Mixer),a
		call	set_noise
		call	get_tone
		ld	(AYREGS+AR.TonC),hl

.finish:	ld	hl,AYREGS+AR.AmplC
		call	apply_volenv

outay:		ld	hl,AYREGS+AR.EnvSh
		xor	a
		or	(hl)
		ld	a,AR.EnvSh
		jr	nz,.fillregs
		sub	3
		dec	hl
		dec	hl
		dec	hl
.fillregs:	ld	bc,#FFFD
.loop:		out	(c),a
		res	6,b
		outd
		set	6,b
		dec	a
		ret	m
		jr	.loop

music_mute:	ld	hl,AYREGS
		ld	de,AYREGS+1
		ld	bc,AR.Mixer
		ld	(hl),b
		ldir
		dec	(hl)
		ld	l,low AYREGS
		ld	c,AR.EnvSh-AR.Mixer-1
		ldir
		ex	hl,de
		ld	a,AR.EnvSh
		jr	outay.fillregs

c_note:		ld	(ix+CHP.Note),a
		ld	(ix+CHP.SmpIndex),0
		ld	(ix+CHP.SmpCnt),#20
c_empty:	inc	hl
		ret

c_sample:	sub	#60
		push	hl
		ld	bc,SAMPLEN
		ld	hl,(samples)
		call	scan_until
		inc	hl
		ld	(ix+CHP.SmpPtr),l
		ld	(ix+CHP.SmpPtr+1),h
		pop	hl
		inc	hl
		jr	read_pattern

c_rest:		inc	hl
c_norept:	ld	(ix+CHP.SmpCnt),-1
		ret

read_pattern:	ld	a,(hl)
		cp	#60		; note
		jr	c,c_note
		cp	#70		; sample
		jr	c,c_sample
		cp	#80		; ornament
		jr	c,c_ornament
		jr	z,c_rest	; rest
		cp	#81		; empty
		jr	z,c_empty
		cp	#82		; ornament off
		jr	z,c_ornoff
		cp	#8F		; envelope
		jr	c,c_envelope
		sub	#A1		; speed
		ld	(ix+CHP.PatStpCnt),a
		ld	(ix+CHP.PatStep),a
		inc	hl
		jr	read_pattern

c_ornoff:	xor	a
		db	1 ; ld bc,* (skip next instruction)
c_ornament:	sub	#70
c_ornament0:	push	hl
		ld	bc,ORNALEN
		ld	hl,(ornaments)
		call	scan_until
		inc	hl
		ld	(ix+CHP.OrnPtr),l
		ld	(ix+CHP.OrnPtr+1),h
		ld	(ix+CHP.EnvState),0
		pop	hl
		inc	hl
		jr	read_pattern

c_envelope:	sub	#80
		ld	(AYREGS+AR.EnvSh),a
		inc	hl
		ld	a,(hl)
		inc	hl
		ld	(AYREGS+AR.Env),a
		ld	(ix+CHP.EnvState),1
		push	hl
		xor	a
		ld	bc,ORNALEN
		ld	hl,(ornaments)
		call	scan_until
		inc	hl
		ld	(ix+CHP.OrnPtr),l
		ld	(ix+CHP.OrnPtr+1),h
		pop	hl
		jr	read_pattern

adv_sample:	ld	a,(ix+CHP.SmpCnt)
		inc	a
		ret	z
		dec	a
		dec	a
		ld	(ix+CHP.SmpCnt),a
		push	af
		ld	a,(ix+CHP.SmpIndex)
		ld	c,a
		inc	a
		and	#1F
		ld	(ix+CHP.SmpIndex),a
		pop	af
		ret	nz
		ld	e,(ix+CHP.SmpPtr)
		ld	d,(ix+CHP.SmpPtr+1)
		ld	hl,#60	; offset to sample metadata
		add	hl,de
		ld	a,(hl)
		dec	a
		jp	m,c_norept
		ld	c,a	; repeat sample
		inc	a
		and	#1F
		ld	(ix+CHP.SmpIndex),a
		inc	hl
		ld	a,(hl)
		inc	a
		ld	(ix+CHP.SmpCnt),a
		ret

read_sample:	ld	d,0
		ld	e,a
		add	a,a
		add	a,e
		ld	e,a
		ld	l,(ix+CHP.SmpPtr)
		ld	h,(ix+CHP.SmpPtr+1)
		add	hl,de
		inc	hl
		ld	a,(hl)
		bit	7,a
		ld	c,#10
		jr	nz,.no_noise
		ld	c,d
.no_noise:	bit	6,a
		ld	b,2
		jr	nz,.no_tone
		ld	b,d
.no_tone:	inc	hl
		ld	e,(hl)
		dec	hl
		dec	hl
		ld	d,(hl)
		ld	l,a
		and	#1F
		ld	h,a
		ld	a,d
		push	af
		and	#F0
		rrca
		rrca
		rrca
		rrca
		ld	d,a
		pop	af
		and	#0F
		bit	5,l
		ld	l,a
		ret	z
		set	4,d
		ret

set_noise:	ld	a,c
		or	a
		ret	nz
		ld	a,h
		ld	(AYREGS+AR.Noise),a
		ret

apply_volenv:	ld	e,0		; GLOBAL ATTENUATION
		sub	e
		jr	nc,.set
		xor	a
.set		ld	(hl),a		; if value of attenuation is higher
		bit	3,a		; then 8 we will stop also envelopes...
		ret	z

		ld	a,(ix+CHP.SmpCnt)
		inc	a
		ret	z
		ld	a,(ix+CHP.EnvState)
		or	a
		ret	z
		dec	a
		jr	nz,.resetshape
		ld	(ix+CHP.EnvState),2
		jr	.setenv

.resetshape:	xor	a
		ld	(AYREGS+AR.EnvSh),a
.setenv:	set	4,(hl)
		ret

get_tone:	ld	a,l
		push	af
		push	de
		ld	l,(ix+CHP.OrnPtr)
		ld	h,(ix+CHP.OrnPtr+1)
sample_index:	ld	de,0
		add	hl,de
		ld	a,(ix+CHP.Note)
		add	a,(hl)

get_frequency:	add	a,0
		add	a,a
		ld	e,a
		ld	d,0
		ld	hl,freqtable
		add	hl,de
		ld	e,(hl)
		inc	hl
		ld	d,(hl)
		ex	de,hl
		pop	de
		pop	af
		bit	4,d
		jr	z,.negative
		res	4,d
		add	hl,de
		ret

.negative:	and	a
		sbc	hl,de
		ret

;------------------------------------------------------------------------------
resetfadeout:	ld	hl,music_setup
noloop:		set	7,(hl)
		call	music_mute
deadend:	pop	hl
		xor	a
		ld	(tempo_cnt),a
		dec	a
		ld	(CHN1+CHP.SmpCnt),a
		ld	(CHN2+CHP.SmpCnt),a
		ld	(CHN3+CHP.SmpCnt),a
		ld	(chkfadeout+1),a
		ret

enablefade:	ld	a,48
forcefade:	ld	(chkfadeout+1),a
		ld	(countfadeout+1),a
		ld	(divfadeout+1),a

chkfadeout:	ld	a,0
		or	a
		ret	z
		ld	a,(music_setup)
		rlca
		jr	c,deadend
countfadeout:	ld	a,0
		dec	a
		ld	(countfadeout+1),a
		ret	nz
		ld	a,(apply_volenv+1)
		inc	a
		cp	16
		jr	z,resetfadeout
		ld	(apply_volenv+1),a
divfadeout:	ld	a,0
		srl	a
		ld	e,a
		srl	e
		add	a,e
		jr	nz,divfadeout1
		inc	a
divfadeout1:	ld	(countfadeout+1),a
		ld	(divfadeout+1),a
		ret

;------------------------------------------------------------------------------
positions:	dw	0
ornaments:	dw	0
patterns:	dw	0
samples:	dw	0
speed:		db	0
tempo_cnt:	db	0
song_length:	db	0

chn1_pattptr:	dw	empty_pattern
chn2_pattptr:	dw	empty_pattern
chn3_pattptr:	dw	empty_pattern
empty_pattern:	db	#FF

VARS
CHN1:		ds	10
CHN2:		ds	10
CHN3:		ds	10

current_pos:	db	0
@VARSLEN = $ - VARS

AYREGS:		ds	14

;------------------------------------------------------------------------------
freqtable:	dw	#0EF8, #0E10, #0D60, #0C80, #0BD8, #0B28, #0A88, #09F0
		dw	#0960, #08E0, #0858, #07E0, #077C, #0708, #06B0, #0640
		dw	#05EC, #0594, #0544, #04F8, #04B0, #0470, #042C, #03F0
		dw	#03BE, #0384, #0358, #0320, #02F6, #02CA, #02A2, #027C
		dw	#0258, #0238, #0216, #01F8, #01DF, #01C2, #01AC, #0190
		dw	#017B, #0165, #0151, #013E, #012C, #011C, #010B, #00FC
		dw	#00EF, #00E1, #00D6, #00C8, #00BD, #00B2, #00A8, #009F
		dw	#0096, #008E, #0085, #007E, #0077, #0070, #006B, #0064
		dw	#005E, #0059, #0054, #004F, #004B, #0047, #0042, #003F
		dw	#003B, #0038, #0035, #0032, #002F, #002C, #002A, #0027
		dw	#0025, #0023, #0021, #001F, #001D, #001C, #001A, #0019
		dw	#0017, #0016, #0015, #0013, #0012, #0011, #0010, #000F


;------------------------------------------------------------------------------
	struct AR	; AY Registers structure
TonA		word	; 0
TonB		word	; 2
TonC		word	; 4
Noise		byte	; 6
Mixer		byte	; 7
AmplA		byte	; 8
AmplB		byte	; 9
AmplC		byte	; 10
Env		word	; 11
EnvSh		byte	; 13
	ends

	struct CHP	; channel parameters structure
EnvState	byte
PatStep		byte
SmpIndex	byte
Note		byte
PatStpCnt	byte
SmpPtr		word
OrnPtr		word
SmpCnt		byte
	ends

@HEADLEN = 27	; file header length
@SAMPLEN = 99	; single sample length
@ORNALEN = 33	; single ornament length
@PATTLEN = 7	; single pattern record length


; bit.0 no loop, bit.1 fadeout after loop, bit.7 set when finished
music_setup:	db	1


			endmodule
