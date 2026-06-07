			module stp
@NULL = 0
;------------------------------------------------------------------------------
music_init:		ld	hl,songdata
music_init0:	ld	(module_addr+1),hl
		ld	a,#FC
		ld	(ChA.smppos+1),a
		ld	(ChB.smppos+1),a
		ld	(ChC.smppos+1),a
		ld	a,(hl)
		inc	hl
		ld	(speed+1),a
		call	add_offset.de
		ld	a,(hl)
		ld	(song_length+1),a
		inc	hl
		ld	a,(hl)
		ld	(loop_position+1),a
		inc	hl
		ld	(positions+1),hl
		call	add_offset
		ld	(patterns+1),hl
		push	hl
		call	add_offset
		ld	(ChA.ornoffs+1),hl
		ld	(ChB.ornoffs+1),hl
		ld	(ChC.ornoffs+1),hl
		call	add_offset
		ld	(ChA.smpoffs+1),hl
		ld	(ChB.smpoffs+1),hl
		ld	(ChC.smpoffs+1),hl
		ex	de,hl
		ld	a,(hl)
		ld	(hl),0
		pop	hl
		or	a
		jr	z,.noreloc

.relocator:	call	add_offset.de
		ex	de,hl
		dec	hl
		ld	(hl),d
		dec	hl
		ld	(hl),e
		inc	hl
		inc	hl
		dec	a
		jr	nz,.relocator

.noreloc:	ld	hl,empty_pattern
		ld	c,3
		ld	(ChA.datptr+1),hl
		ld	(ChB.datptr+1),hl
		ld	(ChC.datptr+1),hl
		ld	h,a
		ld	l,a
		ld	(ChA.Xportamnt+1),hl
		ld	(ChB.Xportamnt+1),hl
		ld	(ChA.Xpitch+1),hl
		ld	(ChB.Xpitch+1),hl
		ld	(ChC.playback+2),a

		ld	(chkfadeout+1),a
		ld	(apply_volume+1),a
		ld	(ChA.attn+1),a
		ld	(ChB.attn+1),a
		ld	(ChC.attn+1),a
		dec	a
		ld	(current_pos+1),a
		ld	hl,music_setup
		res	7,(hl)

music_mute:	xor	a
		ld	(AYREG_EnvSh+1),a
		ld	hl,AYREG_TonA
		ld	b,#0D
.loopregs:	ld	(hl),a
		inc	hl
		djnz	.loopregs
		ld	a,c
		ld	(tempocnt_0),a
		ld	bc,#FFFD
		ld	a,#0C
		out	(c),a
		xor	a
		ld	b,#BF
		out	(c),a
		jp	outay

add_offset	ex	de,hl
.de		ld	e,(hl)
		inc	hl
		ld	d,(hl)
		inc	hl
		ex	de,hl
module_addr:	ld	bc,0
		add	hl,bc
		ret

; ---------------------------------------------------------------------------
resetfadeout:	ld	hl,music_setup
noloop:		set	7,(hl)
		xor	a
		ld	(AYREG_AmplA),a
		ld	(AYREG_AmplB),a
		ld	(AYREG_AmplC),a
		ld	(AYREG_EnvSh+1),a
		dec	a
		ld	(tempocnt_0),a
		ld	(chkfadeout+1),a
		ld	xl,#3F
		jp	retsp

enablefade:	ld	a,48
forcefade:	ld	(chkfadeout+1),a
		ld	(countfadeout+1),a
		ld	(divfadeout+1),a
		xor	a
		ld	(apply_volume+1),a
		jr	loop_position

; ---------------------------------------------------------------------------
new_position:	ld	hl,(ChB.datptr+1)
		or	(hl)
		jp	nz,chkfadeout
		ld	hl,(ChC.datptr+1)
		or	(hl)
		jp	nz,chkfadeout
		ld	b,a
current_pos:	ld	a,0
		inc	a
song_length:	cp	0
		jr	nz,song_continue
		ld	hl,music_setup
		bit	1,(hl)
		jr	nz,enablefade
		bit	0,(hl)
		jr	nz,noloop
loop_position:	ld	a,0
song_continue:	ld	(current_pos+1),a
		ld	c,a

positions:	ld	hl,NULL
		add	hl,bc
		add	hl,bc
		ld	c,(hl)
		inc	hl
		ld	a,(hl)

patterns:	ld	hl,NULL
		add	hl,bc
		ld	sp,hl
		pop	hl
		ld	(ChA.datptr+1),hl
		pop	hl
		ld	(ChB.datptr+1),hl
		pop	hl
		ld	(ChC.datptr+1),hl
		add	a,#60
		ld	(ChC.playback+2),a
		ld	b,a
		ld	c,#F0
		jp	ChA.datptr

; ----------------------------------------------------------------------------
music_play:	di
		ld	(retsp+1),sp
		ld	d,0
		exx
ChC.playback:	ld	bc,#60F0
		ld	hl,tempocnt_0
		dec	(hl)
		jp	nz,ChB.playback
@speed:		ld	(hl),0
		inc	hl
		dec	(hl)
		jp	p,ChB.Xsmpptr
ChC.datptr:	ld	hl,NULL
		or	(hl)
		jp	z,ChB.Xsmpptr
ChC.readpatt:	ld	a,(hl)
		inc	hl
		cp	#C0
		jr	c,ChC.nocmd
		sub	c
		jr	nc,ChC.setvol
		sub	c
		jp	nc,ChC.nop
		sub	c
		jr	nc,ChC.setrst
		sub	c
		jp	ChC.setenv

ChC.nocmd:	sub	#80
		jr	nc,ChC.setspd
		sub	c
		jp	nc,ChC.setorn
		sbc	a,c
		jr	nc,ChC.setsmp
		add	a,b
		ld	(ChC.note+1),a
		ld	(ChC.datptr+1),hl
		xor	a
		ld	(ChC.smppos+1),a
		ld	(ChC.ornpos+1),a
		ld	l,a
		ld	h,a
		ld	(ChC.pitch+1),hl
		jp	ChC.exit

ChC.setport:	ld	a,(hl)
		inc	hl
		ld	(ChC.portamnt+1),a
		rla
		sbc	a,a
		ld	(ChC.portamnt+2),a
		jr	ChC.readpatt

ChC.setvol:	jr	z,ChC.setport
		dec	a
		ld	(ChC.volume+1),a
		jr	ChC.readpatt

ChC.setrst:	ld	a,#FC
		ld	(ChC.smppos+1),a
		ld	(ChC.datptr+1),hl
		jp	ChC.exit

ChC.setsmp:	add	a,a
		exx
		ld	e,a
ChC.smpoffs:	ld	hl,NULL
		add	hl,de
		ld	sp,hl
		pop	hl
		ld	a,(hl)
		add	a,a
		add	a,a
		ld	(ChC.smploop+1),a
		inc	hl
		ld	a,(hl)
		add	a,a
		add	a,a
		ld	(ChC.smplen+1),a
		ld	(ChC.smpptr+1),hl
		exx
		jp	ChC.readpatt

ChC.setspd:	ld	(ChC.exit+1),a
		jp	ChC.readpatt

ChC.setenv:	ld	(env.shape+1),a
		ld	a,#1F
		ld	(ChC.ampl+1),a
		ld	a,(hl)
		inc	hl
		ld	(env.period+1),a
		ld	de,empty_orn
		ld	(ChC.ornptr+1),de
		xor	a
		ld	(ChC.ornloop+1),a
		ld	(ChC.portamnt+1),a
		ld	(ChC.portamnt+2),a
		inc	a
		ld	(ChC.ornlen+1),a
		jp	ChC.readpatt

ChC.setorn:	add	a,a
		exx
		ld	e,a
		ld	a,#0F
		ld	(ChC.ampl+1),a
ChC.ornoffs:	ld	hl,NULL
		add	hl,de
		ld	sp,hl
		pop	hl
		ld	a,(hl)
		ld	(ChC.ornloop+1),a
		inc	hl
		ld	a,(hl)
		ld	(ChC.ornlen+1),a
		inc	hl
		ld	(ChC.ornptr+1),hl
		ld	l,d
		ld	h,d
		ld	(ChC.portamnt+1),hl
		exx
		jp	ChC.readpatt

; ---------------------------------------------------------------------------
ChB.playback:	ld	a,(hl)
		dec	a
		jp	nz,ChA.playback
		ld	hl,tempocnt_2
		dec	(hl)
		jp	p,chkfadeout
ChB.datptr:	ld	hl,NULL
		or	(hl)
		jp	z,chkfadeout
ChB.readpatt:	ld	a,(hl)
		inc	hl
		cp	#C0
		jr	c,ChB.nocmd
		sub	c
		jr	nc,ChB.setvol
		sub	c
		jr	nc,ChB.nop
		sub	c
		jr	nc,ChB.setrst
		sub	c
		jr	ChB.setenv

ChB.nocmd:	sub	#80
		jr	nc,ChB.setspd
		sub	c
		jp	nc,ChB.setorn
		sbc	a,c
		jr	nc,ChB.setsmp
		add	a,b
		ld	(ChB.Xnote+1),a
		ld	(ChB.datptr+1),hl
		xor	a
		ld	(ChB.Xsmppos+1),a
		ld	l,a
		ld	h,a
		ld	(ChB.Xpitch+1),hl
		ld	a,#22
		ld	(ChB.Xinst1),a
		ld	a,#32
		ld	(ChB.Xinst2),a
		jr	ChB.exit

ChB.setvol:	jr	z,ChB.setport
		dec	a
		ld	(ChB.Xvolume+1),a
		jr	ChB.readpatt

ChB.nop:	ld	(ChB.datptr+1),hl
ChB.exit:	ld	a,0
		ld	(tempocnt_2),a
		jp	chkfadeout

ChB.setrst:	ld	a,#FC
		ld	(ChB.Xsmppos+1),a
		ld	(ChB.datptr+1),hl
		jr	ChB.exit

ChB.setsmp:	add	a,a
		exx
		ld	e,a
ChB.smpoffs:	ld	hl,NULL
		add	hl,de
		ld	sp,hl
		pop	hl
		ld	a,(hl)
		add	a,a
		add	a,a
		ld	(ChB.Xsmploop+1),a
		inc	hl
		ld	a,(hl)
		add	a,a
		add	a,a
		ld	(ChB.Xsmplen+1),a
		ld	(ChB.Xsmpptr+1),hl
		exx
		jr	ChB.readpatt

ChB.setspd:	ld	(ChB.exit+1),a
		jr	ChB.readpatt

ChB.setenv:	ld	(env.shape+1),a
		ld	a,#1F
		ld	(ChB.Xampl+1),a
		ld	a,(hl)
		inc	hl
		ld	(env.period+1),a
		ld	de,empty_orn
		ld	(ChB.Xornptr+1),de
		ld	a,#22
		ld	(ChB.Xinst3),a
		xor	a
		ld	(ChB.Xornloop+1),a
		ld	(ChB.Xportamnt+1),a
		ld	(ChB.Xportamnt+2),a
		inc	a
		ld	(ChB.Xornlen+1),a
		jp	ChB.readpatt

ChB.setport:	ld	a,(hl)
		inc	hl
		ld	(ChB.Xportamnt+1),a
		rla
		sbc	a,a
		ld	(ChB.Xportamnt+2),a
		jp	ChB.readpatt

ChB.setorn:	add	a,a
		exx
		ld	e,a
		ld	a,#0F
		ld	(ChB.Xampl+1),a
ChB.ornoffs:	ld	hl,NULL
		add	hl,de
		ld	sp,hl
		pop	hl
		ld	a,(hl)
		ld	(ChB.Xornloop+1),a
		inc	hl
		ld	a,(hl)
		ld	(ChB.Xornlen+1),a
		inc	hl
		ld	(ChB.Xornptr+1),hl
		ld	l,d
		ld	h,d
		ld	(ChB.Xportamnt+1),hl
		ld	a,#22
		ld	(ChB.Xinst3),a
		exx
		jp	ChB.readpatt

; ---------------------------------------------------------------------------
ChA.playback:	dec	a
		jp	nz,chkfadeout
		dec	hl
		dec	(hl)
		jp	p,chkfadeout
ChA.datptr:	ld	hl,NULL
		or	(hl)
		jp	z,new_position
ChA.readpatt:	ld	a,(hl)
		inc	hl
		cp	#C0
		jr	c,ChA.nocmd
		sub	c
		jr	nc,ChA.setvol
		sub	c
		jr	nc,ChA.nop
		sub	c
		jr	nc,ChA.setrst
		sub	c
		jr	ChA.setenv

ChA.nocmd:	sub	#80
		jr	nc,ChA.setspd
		sub	c
		jp	nc,ChA.setorn
		sbc	a,c
		jr	nc,ChA.setsmp
		add	a,b
		ld	(ChA.Xnote+1),a
		ld	(ChA.datptr+1),hl
		xor	a
		ld	(ChA.Xsmppos+1),a
		ld	l,a
		ld	h,a
		ld	(ChA.Xpitch+1),hl
		ld	a,#22
		ld	(ChA.Xinst1),a
		ld	a,#32
		ld	(ChA.Xinst2),a
		jr	ChA.exit

ChA.setvol:	jr	z,ChA.setport
		dec	a
		ld	(ChA.Xvolume+1),a
		jr	ChA.readpatt

ChA.nop:	ld	(ChA.datptr+1),hl
ChA.exit:	ld	a,0
		ld	(tempocnt_1),a
		jp	chkfadeout

ChA.setrst:	ld	a,#FC
		ld	(ChA.Xsmppos+1),a
		ld	(ChA.datptr+1),hl
		jr	ChA.exit

ChA.setsmp:	add	a,a
		exx
		ld	e,a
ChA.smpoffs:	ld	hl,NULL
		add	hl,de
		ld	sp,hl
		pop	hl
		ld	a,(hl)
		add	a,a
		add	a,a
		ld	(ChA.Xsmploop+1),a
		inc	hl
		ld	a,(hl)
		add	a,a
		add	a,a
		ld	(ChA.Xsmplen+1),a
		ld	(ChA.Xsmpptr+1),hl
		exx
		jr	ChA.readpatt

ChA.setspd:	ld	(ChA.exit+1),a
		jr	ChA.readpatt

ChA.setenv:	ld	(env.shape+1),a
		ld	a,#1F
		ld	(ChA.Xampl+1),a
		ld	a,(hl)
		inc	hl
		ld	(env.period+1),a
		ld	de,empty_orn
		ld	(ChA.Xornptr+1),de
		ld	a,#22
		ld	(ChA.Xinst3),a
		xor	a
		ld	(ChA.Xornloop+1),a
		ld	(ChA.Xportamnt+1),a
		ld	(ChA.Xportamnt+2),a
		inc	a
		ld	(ChA.Xornlen+1),a
		jp	ChA.readpatt

ChA.setport:	ld	a,(hl)
		inc	hl
		ld	(ChA.Xportamnt+1),a
		rla
		sbc	a,a
		ld	(ChA.Xportamnt+2),a
		jp	ChA.readpatt

ChA.setorn:	add	a,a
		exx
		ld	e,a
		ld	a,#0F
		ld	(ChA.Xampl+1),a
ChA.ornoffs:	ld	hl,NULL
		add	hl,de
		ld	sp,hl
		pop	hl
		ld	a,(hl)
		ld	(ChA.Xornloop+1),a
		inc	hl
		ld	a,(hl)
		ld	(ChA.Xornlen+1),a
		inc	hl
		ld	(ChA.Xornptr+1),hl
		ld	l,d
		ld	h,d
		ld	(ChA.Xportamnt+1),hl
		ld	a,#22
		ld	(ChA.Xinst3),a
		exx
		jp	ChA.readpatt

; ---------------------------------------------------------------------------
ChA.mute:	xor	a
		ld	(AYREG_AmplA),a
		ld	xl,9
		jp	ChB.ornpos

ChB.mute:	xor	a
		ld	(AYREG_AmplB),a
		ld	a,xl
		or	#12
		ld	xl,a
		jp	ChC.ornpos

ChC.mute:	xor	a
		ld	(AYREG_AmplC),a
		ld	a,xl
		or	#24
		ld	xl,a
		jp	retsp

; ---------------------------------------------------------------------------
ChC.nop:	ld	(ChC.datptr+1),hl
ChC.exit:	ld	a,0
		ld	(pattstep_cnt),a
ChB.Xsmpptr:	ld	hl,NULL
		ld	(ChB.smpptr+1),hl
ChB.Xornptr:	ld	hl,NULL
		ld	(ChB.ornptr+1),hl
ChB.Xpitch:	ld	hl,NULL
ChB.Xinst1:	ld	(ChB.pitch+1),hl
ChB.Xportamnt:	ld	hl,NULL
ChB.Xinst3:	ld	(ChB.portamnt+1),hl
ChB.Xornlen:	ld	a,0
		ld	(ChB.ornlen+1),a
ChB.Xsmplen:	ld	a,0
		ld	(ChB.smplen+1),a
ChB.Xornloop:	ld	a,0
		ld	(ChB.ornloop+1),a
ChB.Xsmploop:	ld	a,0
		ld	(ChB.smploop+1),a
ChB.Xvolume:	ld	a,0
		ld	(ChB.volume+1),a
ChB.Xampl:	ld	a,#0F
		ld	(ChB.ampl+1),a
ChB.Xnote:	ld	a,0
		ld	(ChB.note+1),a
ChB.Xsmppos:	ld	a,0
		cp	1
		jr	z,ChA.Xsmpptr
		ld	(ChB.smppos+1),a
ChA.Xsmpptr:	ld	hl,NULL
		ld	(ChA.smpptr+1),hl
ChA.Xornptr:	ld	hl,NULL
		ld	(ChA.ornptr+1),hl
ChA.Xpitch:	ld	hl,NULL
ChA.Xinst1:	ld	(ChA.pitch+1),hl
ChA.Xportamnt:	ld	hl,NULL
ChA.Xinst3:	ld	(ChA.portamnt+1),hl
ChA.Xornlen:	ld	a,0
		ld	(ChA.ornlen+1),a
ChA.Xsmplen:	ld	a,0
		ld	(ChA.smplen+1),a
ChA.Xornloop:	ld	a,0
		ld	(ChA.ornloop+1),a
ChA.Xsmploop:	ld	a,0
		ld	(ChA.smploop+1),a
ChA.Xvolume:	ld	a,0
		ld	(ChA.volume+1),a
ChA.Xampl:	ld	a,#0F
		ld	(ChA.ampl+1),a
ChA.Xnote:	ld	a,0
		ld	(ChA.note+1),a
ChA.Xsmppos:	ld	a,0
		cp	1
		jr	z,env.shape
		ld	(ChA.smppos+1),a
env.shape:	ld	a,0
		ld	(AYREG_EnvSh+1),a
env.period:	ld	a,0
		ld	(AYREG_Env+1),a
		xor	a
		ld	(env.shape+1),a
ChA.Xinst2:	ld	(ChA.ornpos+1),a
ChB.Xinst2:	ld	(ChB.ornpos+1),a
		inc	a
		ld	(ChA.Xsmppos+1),a
		ld	(ChA.Xinst2),a
		ld	(ChB.Xsmppos+1),a
		ld	(ChB.Xinst2),a
		ld	(ChA.Xinst1),a
		ld	(ChB.Xinst1),a

; ---------------------------------------------------------------------------
chkfadeout:	ld	a,0
		or	a
		jr	z,ChA.ornpos
		ld	a,(music_setup)
		rlca
		jp	c,retsp
countfadeout:	ld	a,0
		dec	a
		ld	(countfadeout+1),a
		jr	nz,ChA.ornpos
		ld	e,16
apply_volume:	ld	a,0
		inc	a
		cp	e
		jp	z,resetfadeout
		ld	(apply_volume+1),a
		ld	(ChA.attn+1),a
		ld	(ChB.attn+1),a
		ld	(ChC.attn+1),a
		bit	3,a			; if value of attenuation
		jr	z,divfadeout		; hits the 8 we turn off
		xor	a			; also hw envelopes
		ld	(env.shape+1),a
		ld	(AYREG_EnvSh+1),a
		ld	a,e
		dec	a
		ld	(ChA.ampl+1),a
		ld	(ChB.ampl+1),a
		ld	(ChC.ampl+1),a
divfadeout:	ld	a,0
		srl	a
		ld	e,a
		srl	e
		add	a,e
		jr	nz,divfadeout1
		inc	a
divfadeout1:	ld	(divfadeout+1),a
		add	a,a
		ld	(countfadeout+1),a

ChA.ornpos:	ld	de,0
ChA.ornptr:	ld	hl,NULL
		add	hl,de
		inc	e
		ld	a,e
ChA.ornlen:	cp	0
		jr	nz,ChA.readorn
ChA.ornloop:	ld	a,0
ChA.readorn:	ld	(ChA.ornpos+1),a
ChA.note:	ld	a,0
		add	a,(hl)
		add	a,a
		ld	e,a
		ld	hl,freqtable
		add	hl,de
		ld	sp,hl
ChA.smppos:	ld	a,0
ChA.smpptr:	ld	hl,NULL
		inc	a
		jp	m,ChA.mute
		ld	e,a
		add	hl,de
		add	a,3
ChA.smplen:	cp	0
		jr	nz,ChA.readsmp
ChA.smploop:	ld	a,0
ChA.readsmp:	ld	(ChA.smppos+1),a
		pop	bc
		ld	sp,hl
		pop	de
		ld	h,0
		ld	a,e
		and	#0F
ChA.volume:	sub	0
		ld	l,a
		ccf
		sbc	a,a
		and	l
		srl	d
		jr	nc,ChA.attn
		set	4,h
ChA.attn:	sub	0
		jr	nc,ChA.attnn
		xor	a
ChA.attnn:	or	h
ChA.ampl:	and	0
		ld	(AYREG_AmplA),a
		ld	a,e
		rlca
		jr	c,ChA.pitch
		ld	xh,d
ChA.pitch:	ld	hl,NULL
ChA.portamnt:	ld	de,0
		add	hl,de
		ld	(ChA.pitch+1),hl
		add	hl,bc
		pop	bc
		add	hl,bc
		rlca
		rlca
		rlca
		and	#0F
		ld	xl,a
		ld	a,h
		and	#0F
		ld	h,a
		ld	(AYREG_TonA),hl

ChB.ornpos:	ld	de,0
ChB.ornptr:	ld	hl,NULL
		add	hl,de
		inc	e
		ld	a,e
ChB.ornlen:	cp	0
		jr	nz,ChB.readorn
ChB.ornloop:	ld	a,0
ChB.readorn:	ld	(ChB.ornpos+1),a
ChB.note:	ld	a,0
		add	a,(hl)
		add	a,a
		ld	e,a
		ld	hl,freqtable
		add	hl,de
		ld	sp,hl
ChB.smppos:	ld	a,NULL
ChB.smpptr:	ld	hl,NULL
		inc	a
		jp	m,ChB.mute
		ld	e,a
		add	hl,de
		add	a,3
ChB.smplen:	cp	0
		jr	nz,ChB.readsmp
ChB.smploop:	ld	a,0
ChB.readsmp:	ld	(ChB.smppos+1),a
		pop	bc
		ld	sp,hl
		pop	de
		ld	h,0
		ld	a,e
		and	#0F
ChB.volume:	sub	0
		ld	l,a
		ccf
		sbc	a,a
		and	l
		srl	d
		jr	nc,ChB.attn
		set	4,h
ChB.attn:	sub	0
		jr	nc,ChB.attnn
		xor	a
ChB.attnn:	or	h
ChB.ampl:	and	0
		ld	(AYREG_AmplB),a
		ld	a,e
		rrca
		rrca
		rrca
		and	#1E
		or	xl
		ld	xl,a
		and	#10
		jr	nz,ChB.pitch
		ld	xh,d
ChB.pitch:	ld	hl,NULL
ChB.portamnt:	ld	de,0
		add	hl,de
		ld	(ChB.pitch+1),hl
		add	hl,bc
		pop	bc
		add	hl,bc
		ld	a,h
		and	#0F
		ld	h,a
		ld	(AYREG_TonB),hl

ChC.ornpos:	ld	de,0
ChC.ornptr:	ld	hl,NULL
		add	hl,de
		inc	e
		ld	a,e
ChC.ornlen:	cp	0
		jr	nz,ChC.readorn
ChC.ornloop:	ld	a,0
ChC.readorn:	ld	(ChC.ornpos+1),a
ChC.note:	ld	a,0
		add	a,(hl)
		add	a,a
		ld	e,a
		ld	hl,freqtable
		add	hl,de
		ld	sp,hl
ChC.smppos:	ld	a,0
ChC.smpptr:	ld	hl,NULL
		inc	a
		jp	m,ChC.mute
		ld	e,a
		add	hl,de
		add	a,3
ChC.smplen:	cp	0
		jr	nz,ChC.readsmp
ChC.smploop:	ld	a,0
ChC.readsmp:	ld	(ChC.smppos+1),a
		pop	bc
		ld	sp,hl
		pop	de
		ld	h,0
		ld	a,e
		and	#0F
ChC.volume:	sub	0
		ld	l,a
		ccf
		sbc	a,a
		and	l
		srl	d
		jr	nc,ChC.attn
		set	4,h
ChC.attn:	sub	0
		jr	nc,ChC.attnn
		xor	a
ChC.attnn:	or	h
ChC.ampl:	and	0
		ld	(AYREG_AmplC),a
		ld	a,e
		rrca
		rrca
		and	#3C
		or	xl
		ld	xl,a
		and	#20
		jr	nz,ChC.pitch
		ld	xh,d
ChC.pitch:	ld	hl,NULL
ChC.portamnt:	ld	de,0
		add	hl,de
		ld	(ChC.pitch+1),hl
		add	hl,bc
		pop	bc
		add	hl,bc
		ld	a,h
		and	#0F
		ld	h,a
		ld	(AYREG_TonC),hl
@retsp:		ld	sp,0

outay:		ld	hl,AYREG_AmplC
		ld	de,#FFBF
		ld	bc,#FFFD
		ld	a,#0D
		out	(c),a
AYREG_EnvSh:	ld	a,0
		or	a
		jr	z,noenvelopes
		ld	b,e
		out	(c),a

		ld	a,#0B
		ld	b,d
		out	(c),a
		ld	b,e
AYREG_Env:	ld	a,0
		out	(c),a
		ld	b,d

noenvelopes:	ld	a,#0A
		out	(c),a
		ld	b,e
		outd
		dec	a
		ld	b,d
		out	(c),a
		ld	b,e
		outd
		dec	a
		ld	b,d
		out	(c),a
		ld	b,e
		outd
		dec	a
		ld	b,d
		out	(c),a
		ld	b,e
		ld	a,xl
		out	(c),a
		ld	a,6
		ld	b,d
		out	(c),a
		ld	b,e
		ld	a,xh
		out	(c),a
		ld	a,5
		ld	b,d
		out	(c),a
		ld	b,e
		outd
		dec	a
		ld	b,d
		out	(c),a
		ld	b,e
		outd
		dec	a
		ld	b,d
		out	(c),a
		ld	b,e
		outd
		dec	a
		ld	b,d
		out	(c),a
		ld	b,e
		outd
		dec	a
		ld	b,d
		out	(c),a
		ld	b,e
		outd
		dec	a
		ld	b,d
		out	(c),a
		ld	b,e
		outd
		ld	(AYREG_EnvSh+1),a
		ei
		ret

;------------------------------------------------------------------------------
freqtable	dw	#0EF8, #0E10, #0D60, #0C80, #0BD8, #0B28, #0A88, #09F0
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

empty_orn:	db	#00, #00, #00
empty_pattern:	db	#70, #80, #F1, #D0, #00

AYREG_TonA:	dw	0
AYREG_TonB:	dw	0
AYREG_TonC:	dw	0
AYREG_AmplA:	db	0
AYREG_AmplB:	db	0
AYREG_AmplC:	db	0
tempocnt_2:	db	0
tempocnt_1:	db	0
tempocnt_0:	db	0
pattstep_cnt:	db	0

;-----------------------------------------------------------------------------			
; bit.0 no loop, bit.1 fadeout after loop, bit.7 set when finished
music_setup:	db	1
			
			
			endmodule
