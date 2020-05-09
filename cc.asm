			DEVICE ZXSPECTRUMNEXT
            OPT reset --zxnext --syntax=abfw              
            slot 4
                
            DEFINE DISP_ADDRESS     $2000
            DEFINE SP_ADDRESS       $3D00
            OPT --zxnext=cspect
            DEFINE ORG_ADDRESS      $7000
            DEFINE TEST_CODE_PAGE   223         ; using the last page of 2MiB RAM (in emulator)
            DEFINE TILE_MAP_ADR     $4000           ; 80*32 = 2560 (10*256)
            DEFINE TILE_GFX_ADR     $6000;$5400           ; 128*32 = 4096
                                                    ; = 6656 $1A00 -> fits into ULA classic VRAM
    
	
			DEFINE CFG_FILENAME     dspedge.defaultCfgFileName
    		STRUCT S_MARGINS        ; pixels of margin 0..31 (-1 = undefined margin)
L           BYTE    -1      ; left
R           BYTE    -1      ; right
T           BYTE    -1      ; top
B           BYTE    -1      ; bottom
			ENDS
			STRUCT S_UI_DEFINITIONS
labelDot    WORD    0       ; address where to write display dot
cellAdr     WORD    0       ; address of big table cell on screen at [1,0] char inside
nextMode    BYTE    0
keyword     WORD    0       ; address of keyword used in the CFG file
			ENDS
			STRUCT S_MODE_EDGES
        ; current margins values (must be first four bytes of the structure)
cur         S_MARGINS
        ; original values (from file)
orig        S_MARGINS
        ; UI related config
ui          S_UI_DEFINITIONS
        ; internal flags and intermediate values
modified    BYTE    0       ; if this mode was modified in some way
leftT       BYTE    0       ; full tiles left
rightT      BYTE    0       ; full tiles right
midT        BYTE    0       ; amount of semi top/bottom tiles (w/o left/right corner tiles)
        ; bit masks to redraw gfx tile with Green/Background color (columns/rows in bits)
        ; preserve the order of offsets, they are processed in "++DE" way
maskLeftG   BYTE    0
maskLeftB   BYTE    0
maskRightG  BYTE    0
maskRightB  BYTE    0
maskTopG    BYTE    0
maskTopB    BYTE    0
maskBottomG BYTE    0
maskBottomB BYTE    0
    ENDS
    STRUCT S_STATE
timingIsUnlocked    BYTE    0
edge                BYTE    0   ; 0 left, 1 top, 2 right, 3 bottom (to align with chars)
lastCtrlKey         BYTE    0   ; save/reload/quit/hz/timing when waiting for confirm
debounceKey         BYTE    0
modified            BYTE    0   ; set if any of modes (even inactive) is modified
noFileFound         BYTE    0
esxErrorNo          BYTE    1
argsPtr             WORD    0
    ENDS
    STRUCT S_PRESERVE
        ; WORDs used, first byte is register number, second byte is preserved value
turbo_07            WORD    TURBO_CONTROL_NR_07
spr_ctrl_15         WORD    SPRITE_CONTROL_NR_15
transp_fallback_4A  WORD    TRANSPARENCY_FALLBACK_COL_NR_4A
tile_transp_4C      WORD    TILEMAP_TRANSPARENCY_I_NR_4C
ula_ctrl_68         WORD    ULA_CONTROL_NR_68
display_ctrl_69     WORD    DISPLAY_CONTROL_NR_69
tile_ctrl_6B        WORD    TILEMAP_CONTROL_NR_6B
tile_def_attr_6C    WORD    TILEMAP_DEFAULT_ATTR_NR_6C
tile_map_adr_6E     WORD    TILEMAP_BASE_ADR_NR_6E
tile_gfx_adr_6F     WORD    TILEMAP_GFX_ADR_NR_6F
tile_xofs_msb_2F    WORD    TILEMAP_XOFFSET_MSB_NR_2F
tile_xofs_lsb_30    WORD    TILEMAP_XOFFSET_LSB_NR_30
tile_yofs_31        WORD    TILEMAP_YOFFSET_NR_31
pal_ctrl_43         WORD    PALETTE_CONTROL_NR_43
pal_idx_40          WORD    PALETTE_INDEX_NR_40
mmu2_52             WORD    MMU2_4000_NR_52
mmu3_53             WORD    MMU3_6000_NR_53
    ; not preserving tilemode clip window coordinates, and tilemode palette
    ; (intentionally, not expecting anyone to need it, or not being able to fix it)
    ; (and subtle sub-states like half-written 9bit color to $44 -> will be lost too)
    ENDS
KEY_DEBOUNCE_WAIT   EQU     8
CHAR_DOT_RED        EQU     25
CHAR_DOT_YELLOW     EQU     26
CHAR_DOT_GREEN      EQU     27
CHAR_ARROW_L        EQU     28
CHAR_ARROW_T        EQU     29
CHAR_ARROW_R        EQU     30
CHAR_ARROW_B        EQU     31
;; some further constants, mostly machine/API related
    
    INCLUDE "constants.i.asm"
;-----------------------------------------------------------------------------
;-- ESX DOS functions
M_DOSVERSION                    equ $88
M_GETSETDRV                     equ $89     ; get current drive (or use A='*'/'$' for current/system drive!)
M_GETHANDLE                     equ $8D     ; get file handle of current dot command
M_GETERR                        equ $93
F_OPEN                          equ $9A
F_CLOSE                         equ $9B
F_READ                          equ $9D
F_WRITE                         equ $9E
F_SEEK                          equ $9F
F_FGETPOS                       equ $A0
F_UNLINK                        equ $AD
F_RENAME                        equ $B0
FA_READ                         equ $01
;; helper macros
ESXDOS      MACRO service? : push hl : pop ix : rst $08 : db service? : ENDM    ; copies HL into IX
NEXTREG2A   MACRO nextreg? : ld a,nextreg? : call readNextReg2A : ENDM
CSP_BREAK   MACRO : IFDEF TESTING : break : ENDIF : ENDM
    ;; reserved space for values (but not initialized, i.e. not part of the binary)
    ; actually in DISPLAYEDGE tool these re-use the same memory area where font was stored
    ; (turned out the DS/BLOCK does overwrite device memory always, so I'm reserving space
    ; here ahead of the real machine code is produced, the real code will later overwrite
    ; the memory as desired)
            org ORG_ADDRESS   


CHARS         equ  15616-256              
mystak        equ  24575         ;ar bi trary value picked to be be low
                                ;BFE0h and above 4000h
staksto       equ  24575         ;some where to put BA SIC's stack
                                ;pointer

                                ;last value out put to 7FFDh
port1         equ  #7FFD         ;ad dress of ROM/RAM switch ing port
                                ;in I/O map
catbuff       equ  #A000         ;some where for DOS to put its catalog
dos_catalog   equ  #011E         ;the DOS routine to call
filename 	  ds   15
typ 		  equ $-4
pocetpolozek	equ 200
virtmem		defb 0
maxlen		equ 261 + 4		;+4 je délka souboru
maxline equ 37
save_allfiles	defw 0	
OKNO 	defb 0

KURZL	defw $4002+160*2		;adresa kurzoru levého okna
KURZR	defw $4002+160*2+80		;adresa kurzoru pravého okna

POSKURZL	defb 0				;pozice kurzoru v levém okně
POSKURZR	defb 0				;pozice kurzoru v pravém okně

ALLPOSL		defw 0				;celková pozice na souboru
ALLPOSR		defw 0

STARTWINL	defw 1				;pocatecni soubor na zacatku okna
STARTWINR	defw 1


pathl		defw PATHLEFT
pathr       defw PATHRIGHT
PATHLEFT	defb "C:",255
			ds 261
PATHRIGHT   defb "C:",255
			ds 261
bottom		defb "                                                                                ",0			

ban1		defb ".      ",$a0,"   ",0
ban2		defb "..     ",$a0,"   ",0

ReadNextReg:
    ; reads nextreg in A into A (does modify currently selected NextReg on I/O port)
    push    bc
    ld      bc,#243B
    out     (c),a
    inc     b       ; bc = TBBLUE_REGISTER_ACCESS_P_253B
    in      a,(c)   ; read desired NextReg state
    pop     bc
    ret

START       
	

		ld a,3
		ld (OKNO),a
		nextreg MMU3_6000_NR_53,5*2+1
        ld      hl,TILE_GFX_ADR
        ld      de,TILE_GFX_ADR+1
        ld      bc,16*32-1
        ld      (hl),0
        ldir
		ld hl,$6000
		ld de,$6001
		ld bc,32*16
		ld (hl),l
		ldir
		ld		de,$6000 + 32*32

   ; convert ROM font to 4bpp tiles by code
        ld      hl,MEM_ROM_CHARS_3C00 + 32*8
        ld      b,128-' '
.RomCharsLoop
        call    ConvertRomCharTo4bpp
        djnz    .RomCharsLoop
		
		ld hl,specialchar
		ld de,$6000+32*16
		ld bc,specialchar2-specialchar

		ldir

        nextreg TURBO_CONTROL_NR_07,3               ; 28Mhz mode
        nextreg SPRITE_CONTROL_NR_15,%000'100'00    ; layer priority: USL
        nextreg TRANSPARENCY_FALLBACK_COL_NR_4A,0   ; black transparency fallback color
        nextreg TILEMAP_TRANSPARENCY_I_NR_4C,$0F
        nextreg ULA_CONTROL_NR_68,$80               ; disable ULA layer
        nextreg DISPLAY_CONTROL_NR_69,0             ; layer2 off, bank 5, timex=0
        nextreg TILEMAP_CONTROL_NR_6B,%1100'0011    ; 80x32x2, 4bpp, pal0, 512tile-mode, force tile-over-ULA
        nextreg TILEMAP_DEFAULT_ATTR_NR_6C,$00      ; no pal offset, no mirror/rot, 0 bit8
        nextreg TILEMAP_BASE_ADR_NR_6E,high TILE_MAP_ADR
        nextreg TILEMAP_GFX_ADR_NR_6F,high TILE_GFX_ADR
        nextreg CLIP_WINDOW_CONTROL_NR_1C,%0000'1000
        nextreg CLIP_TILEMAP_NR_1B,0
        nextreg CLIP_TILEMAP_NR_1B,159
        nextreg CLIP_TILEMAP_NR_1B,0
        nextreg CLIP_TILEMAP_NR_1B,255
        nextreg TILEMAP_XOFFSET_MSB_NR_2F,0
		nextreg TILEMAP_XOFFSET_LSB_NR_30,0
        nextreg TILEMAP_YOFFSET_NR_31,0
        
		
		; set tilemap palette
        nextreg PALETTE_CONTROL_NR_43,%0'011'0000   ; tilemap pal0
        nextreg PALETTE_INDEX_NR_40,0

        ld      hl,tilemapPalette
        ld      b,tilemapPalette_SZ
.setPalLoop:
        ld      a,(hl)
        inc     hl
        nextreg PALETTE_VALUE_9BIT_NR_44,a
        djnz    .setPalLoop           
		ld hl,$4000
		ld de,$4001
		ld bc,80*32*2
		ld (hl),0
		ldir

		ld hl,nadpis
		ld de,#4000
		ld bc,80
menu0		
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
		jr nz,menu0

		call kresli		;NAKRESLI HLAVNI OBRAZOVKU
STOP		
		call reload_dir
		
		ld hl,$4000+2
		ld (adrs+1),hl

		call getroot

		call showwin
		
		ld a,(OKNO)
		xor 16
		ld (OKNO),a
		
		call reload_dir
		
		ld hl,$4000+2+80
		ld (adrs+1),hl
		call getroot_reload
		call showwin
		ld a,(OKNO)
		xor 16
		ld (OKNO),a	

		ld a,32
		call writecur
		ld a,16

		
loop0	
		ld   hl,$4000+160*15+23
		ld (PROGS+1),hl

		;call gettime
		nextreg $56,0



		ld hl,1*256+30
		ld a,0
		ld de,seltxt
		call print

		ld hl,41*256+30
		ld a,0
		ld de,seltxt
		call print


		ld hl,(numsel)
		call NUM
		ld hl,11*256+30
		ld a,0
		ld de,NUMBUF
		
		call print

		ld hl,(numsel+2)
		call NUM
		ld hl,51*256+30
		ld a,0
		ld de,NUMBUF
		call print

		ld a,"/"
		ld ($4000+30*160+32),a
		ld a,"/"
		ld ($4000+30*160+112),a

		ld hl,(ALLFILES)
		call NUM
		ld hl,17*256+30
		ld a,0
		ld de,NUMBUF
		call print

		ld hl,(ALLFILES + 2)
		call NUM
		ld hl,57*256+30
		ld a,0
		ld de,NUMBUF
		call print



;*******************************

		; ld a,(POSKURZL)
		; ld l,a
		; ld h,0
		; call NUM
		; ld hl,1*256+31
		; ld a,16
		; ld de,NUMBUF
		; call print

		; ld hl,(STARTWINL)
		; call NUM
		; ld hl,9*256+31
		; ld a,16
		; ld de,NUMBUF
		; call print

		; ld a,(POSKURZL)
		; ld l,a
		; ld h,0
		; ex de,hl
		; ld hl,(STARTWINL)
		; add hl,de
		; inc hl
		; call find83
		; xor a
		; ld (TMP83+11),a
		; ld hl,20*256+31
		; ld a,16
		; ld de,TMP83
		
		; call print

		; ld a,(POSKURZR)
		; ld l,a
		; ld h,0
		; call NUM
		; ld hl,41*256+31
		; ld a,16
		; ld de,NUMBUF
		; call print

		; ld hl,(STARTWINR)
		; call NUM
		; ld hl,49*256+31
		; ld a,16
		; ld de,NUMBUF
		; call print

		; ld a,(POSKURZR)
		; ld l,a
		; ld h,0
		; ex de,hl
		; ld hl,(STARTWINR)
		; add hl,de
		; inc hl
		; call find83
		; xor a
		; ld (TMP83+11),a
		; ld hl,60*256+31
		; ld a,16
		; ld de,TMP83
		
		; call print

;*******************************

	
	
		call INKEY

		ld (klavesa),a

		cp ''
		jp z,info
		cp 10
		jp z,down
		cp 11 
		jp z,up
		cp 9
		jp z,rightcur
		cp 8
		jp z,leftcur
		cp 4		;true video
		
		jp z,changewin
		
		cp 13
		jp z,enter
		
		cp "8"
		jp z,delete
		cp "9"
		jp z,RENAME
		
		cp "0"
		jp z,menu
		
		cp "5"
		jp z,copy
		
		cp "6"
		jp z,move
		
		cp 32
		jp z,select
		
		cp "7"
		jp z,MKDIR

		cp "+"
		jp z,select_files

		cp "-"
		jp z,deselect

		cp "1"
		jp z,leftwin

		cp "2"
		jp z,rightwin
		cp "h"
		jp z,help
		jp loop0

setleftwin
		ld a,0
		call writecur
		ld a,3
		ld (OKNO),a
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
		ret

setrightwin
		ld a,0
		call writecur
		ld a,$13
		ld (OKNO),a
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
		ret
select_files_left
		call setleftwin
		jp select_files

deselect_files_left
		call setleftwin
		jp deselect

select_files_right
		call setrightwin
		jp select_files

deselect_files_right
		call setrightwin
		jp deselect

leftwin
		ld a,0
		call writecur
		ld a,3
		ld (OKNO),a
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
		jp loop0
rightwin
		ld a,0
		call writecur
		ld a,$13
		ld (OKNO),a
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
		jp loop0


souboru_na_radek	equ 26




					; CPPX1    ld   de,0
							 ; or   a
							 ; sbc  hl,de
							 ; jr   c,CPPNE1
					; CPPX3    ld   b,1
							 ; ld   (PROGPROM),hl
							 ; call PROGRES
					; CPPNE1

PROGPROM	defw 0
PROGPROM2	defw 0

;Výpočet důležitý pro progress
;Počet v registru DE
;		Výstup HL na CPPX1
;			   A na CPPX3
;			   PROGRES vynulovat
PROVYP   ld   h,d
         ld   l,e
         ld   a,d
         or   a
         jr   nz,PROV3
         ld   a,e
         cp   9
         jr   c,PROV4
PROV3    xor  a
         srl  h
         rr   l
         rra  
         srl  h
         rr   l
         rra  
         srl  h
         rr   l
         rra  
         srl  h
         rr   l
         rra  
         or   a
		 
         ld   a,1
         
		 ret  z
         inc  hl
         ret  

PROV4    ld   hl,TABPRO
         dec  de
         add  hl,de
         ld   a,(hl)
         ld   hl,1
		 
         ret  
TABPRO   db 16,8,5,4,3,2,2,2

PRGRS1   ld   b,1
PROGRES  push hl
PROGS    ld   hl,$4000+160*15+25
         ld   a,36		;barva teploměru
PROGRSM1 ld   (hl),a
         inc  hl
		 inc  hl
         djnz PROGRSM1
         ld   (PROGS+1),hl
         pop  hl
         ret  

PROGRES2 push hl
PROGS2   ld   hl,$4000+160*14+25
         ld   a,36		;barva teploměru
PROGRSM12 ld   (hl),a
         inc  hl
		 inc  hl
         djnz PROGRSM12
         ld   (PROGS2+1),hl
         pop  hl
         ret  

clearpr	
		ld   hl,$4000+160*14+25
		ld a,16
		ld b,40
clearpr2
		ld (hl),a
		inc hl
		inc hl
		djnz clearpr2
		ret




;blocklenght	equ 1024*6
		include "functions/copy.asm"
		include "functions/file.asm"
		include "functions/delete.asm"
		include "functions/input.asm"
		include "functions/createdir.asm"
		include "functions/rename.asm"



gettime
		call dospage
		call $01cc		;načti čas a datum  DE = time, BC DATE	
		ld (dostime),de
		ld (dosdate),bc
		jr nc,timeend
		ld ix,dostime
        ld   a,(ix+1)
        ld   b,(ix+0)
        srl  a
        rr   b
        srl  a
        rr   b
        srl  a
        rr   b
        srl  b
        srl  b
        push bc
		
		ld l,a
		ld h,0
		call DECIMAL2
		ld hl,50*256+0
		ld a,16
		ld de,NUMBUF
		call print


         ld   a,':'

        pop  af

		ld l,a
		ld h,0
		call DECIMAL2
		ld hl,56*256+0
		ld a,16
		ld de,NUMBUF
		call print

		jr timeend
        ld   h,b
        ld   l,c

;Zjištění datumu
		ld ix,dosdate
         ld   a,(ix + 0)
         and  31
;        call DEC8E
         ld   h,b
         ld   l,c
         ld   a,'/'
;        call CHAR
         ld   a,(ix + 0)
         ld   b,(ix+1)
         srl  b
         push bc
         rra  
         rra  
         rra  
         rra  
         rra  
         and  15
;        call DEC8E
         ld   h,b
         ld   l,c
         ld   a,'/'
;        call CHAR
         pop  af
	 	;call DEC8E

timeend		
		call basicpage
		ret
den		defb 0
mesic	defb 0
rok		defb 0

hodiny	defb 0
minuty	defb 0
dostime	defw 0
dosdate	defw 0



			
enter
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
		call BUFF83
		call find83
		pop hl
		call FINDLFN
		ld ix,TMP83
		bit 7,(ix+7)
		jp nz,enter_directory
		
		jp loop0		;**************************************
 		
		 ld hl,name
		ld de,name+1
		xor a
		ld (hl),a
		ld bc,60
		ldir
RUN
		ld hl,LFNNAME+59

run2    ld a,(hl)
        dec hl
        cp 32
        jr z,run2
        inc hl
		inc hl
		ld a,0
		ld (hl),a
		ld de,LFNNAME
		or a
		sbc hl,de
		ld b,h
		ld c,l
		
		ld hl,LFNNAME
		ld de,name
		ldir

  
  		call dospage
		ld a,1
		ld b,0
		ld c,0
		call $01d5
		call basicpage 

TADY		
		rst $08
		defw $8f

		jp loop0

cmd		defb "run "
name   	defs 60


enter_directory


		ld b,11
		ld hl,TMP83
CCC							;vynuluj všechny stavové bity v názvu (7.)
		res 7,(hl)
		inc hl
		djnz CCC
		ld hl,TMP83+10		;najdi poslední znak názvu souboru/adresare
chng2	ld a,(hl)
		cp 32
		jr nz,zap
		dec hl
		jr chng2
zap		ld a,255
		inc hl
		ld (hl),a
		call dospage
		xor a 			;change path
		ld hl,TMP83
AAAA
		call $01b1		;změň adresář
		
		call basicpage
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
		call reload_dir

		ld hl,pozicel
		call ROZHOD2
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a
		
		ld bc,38 * 256 + 27
		ld a,0
		call window

		ld hl,pathl
		call ROZHOD2
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a
		ld de,3
		add hl,de
		ld a,(hl)
		cp 255
		jr z,root
		ld a,1
		ld (star+1),a
		jr rcont
root	xor a
		ld (star+1),a
rcont	ld hl,adrl
		call ROZHOD2
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a
		
		
		ld (adrs+1),hl
star	ld hl,1
		call showwin
		ld a,32
		call writecur

		ld hl,pathl
		call ROZHOD2
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a
		ld de,3
		add hl,de
		ld a,(hl)
		cp 255
		jr z,snula
		ld hl,1
		jr scont
snula	ld hl,0
scont

		ld hl,ALLPOSL
		call ROZHOD2
		ld (hl),0
		ld hl,STARTWINL
		call ROZHOD2
		push hl
		call getroot_reload
		pop de
		ex de,hl
		
		ld (hl),e
		inc hl
		ld (hl),d
		jp loop0


getroot
		ld hl,pathl
		call ROZHOD2
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a
		ld de,3
		add hl,de
		ld a,(hl)
		cp 255
		jr z,mroot
		
		ld hl,STARTWINL
		call ROZHOD2
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a
		jr mmm0
mroot	
		ld hl,STARTWINL
		call ROZHOD2
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a
		dec hl
mmm0
		ret

NUMBUF	defb "        ",0
NUM		
		push hl
		ld hl,NUMBUF
		ld de,NUMBUF+1
		ld bc,5
		ld a,32
		ld (hl),a
		ldir
		ld hl,NUMBUF
		ld (numadr+1),hl
		pop hl
DECIMAL5 ld de,10000 ;řád desetitisíců
		 call DIGIT ;počet desetitisíců
DECIMAL4 ld de,1000 ;řád tisíců
		 call DIGIT ;a jeho počet
DECIMAL3 ld de,100 ;řád stovek
		 call DIGIT ;počet
DECIMAL2 ld de,10 ;desítky
		 call DIGIT ;počet
DECIMAL1 ld de,1 ;jednotky
DIGIT 	 ld a,"0"-1 ;do A kód znaku 0 bez jedné
DIGIT2 	 inc a ;přičti jedničku
		 or a ;vynuluj CARRY Flag
		 sbc hl,de ;pokusně odečti řád
		 jr nc,DIGIT2 ;pokud není výsledek záporný opakuj
		 add hl,de ;přičti řád zpátky
		 cp "9"+1 ;testuj znaky 0 až 9
		 jr c,DIGIT3 ;odskoč pokud platí
		 add a,"A"-"9"-1 ;oprava na A až F pro hexa čísla
DIGIT3   push 	hl
numadr	 ld hl,0
		 ld (hl),a
		 inc hl
		 ld (numadr+1),hl
		 pop hl
		 ret 




getroot_reload
		ld hl,pathl
		call ROZHOD2
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a
		ld de,3
		add hl,de
		ld a,(hl)
		cp 255
		jr z,amroot
		ld hl,1
		jr ammm0
amroot	
		ld hl,0
ammm0
		ret


DIRTMP  defb "C:",255

getdir	
		ld hl,pathl
		call ROZHOD2
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a
		ex de,hl
		push de
		ld hl,DIRTMP
		
	
		ld bc,4
		ldir

		call dospage
		
		pop hl
		ld a,1
		call $01b1
		call basicpage
		ret


pozicel	defw $01
pozice2 defw 40* 256 + 1

adrl	defw $4000+2
adrr 	defw $4000+2+80

changewin
		ld a,0
		call writecur
		ld a,(OKNO)
		xor 16
		ld (OKNO),a
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


		jp loop0

bufscr equ $e000
;Uloží obrazovku do banky 19 ZX Next. Ukládají se spodní dvě třetiny obrazovky,
;a celé atributy. Banka 19 se stránkuje od adresy $e000
savescr	
		nextreg $57,19				;Stránka na uložení VideoRam
		ld hl,16384
		ld de,bufscr
		ld bc,32*160
		ldir
		nextreg $57,1				;Nastránkuj zpátky
		ret



;Obnovení spodních dvou třetin obrazovky z 19 stárnky ZX Next.		
loadscr	
		nextreg $57,19
		ld hl,bufscr
		ld de,16384
		ld bc,32*160
		ldir
		nextreg $57,1
		ret
DDDD
down
		ld hl,POSKURZL
		call ROZHOD
		ld a,(hl)
		cp 26
		jr z,down0
		push af
		
		ld hl,STARTWINL
		call ROZHOD2
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a
		pop af
		ld e,a
		ld d,0
		add hl,de
		push hl
		ld hl,ALLFILES
		
		call ROZHOD2
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a
		dec hl
		pop de
		or a
		sbc hl,de
		jp z,loop0



		ld a,0
		call writecur

		ld hl,POSKURZL
		call ROZHOD

		inc (hl)
		ld hl,ALLPOSL
		call ROZHOD2
		push hl
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a
		pop de
		ex de,hl
		inc de
		ld (hl),e
		inc hl
		ld (hl),d


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

down0
		push af
		
		ld hl,STARTWINL
		call ROZHOD2
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a
		pop af
		ld e,a
		ld d,0
		add hl,de
		push hl
		ld hl,ALLFILES
		
		call ROZHOD2
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a
		dec hl
		pop de
		or a
		sbc hl,de
		jp z,loop0





		ld hl,ALLPOSL
		call ROZHOD2
		push hl
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a

		push hl
		ld hl,downall
		call ROZHOD2
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a
		pop de
		ex de,hl
		
		or a
		sbc hl,de
		add hl,de
		pop de
		jp z,loop0

		ex de,hl
		inc de
		ld (hl),e
		inc hl
		ld (hl),d
		
		ld hl,STARTWINL
		call ROZHOD2
		push hl
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a
		pop de
		
		ex de,hl
		inc de
		ld (hl),e
		inc hl
		ld (hl),d
		
		ld hl,adrl
		call ROZHOD2
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a
		ld (adrs+1),hl

		call getroot
		
DDD		
		call showwin
		ld a,32
		call writecur
		jp loop0

downall	defw 0, 0

up
		ld hl,POSKURZL
		call ROZHOD
		ld a,(hl)
		or a
		jr z,up0

		ld a,0
		call writecur
		ld hl,POSKURZL
		call ROZHOD
		dec (hl)
		
		
		ld hl,ALLPOSL
		call ROZHOD2
		push hl
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a
		pop de
		ex de,hl
		dec de
		ld (hl),e
		inc hl
		ld (hl),d
		

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

up0
UP0

		ld hl,pathl
		call ROZHOD2
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a
		ld de,3
		add hl,de
		ld a,(hl)
		cp 255
		jr z,upzero
		ld a,1
		ld (kolik+1),a
		jr opcont
upzero	xor a
		ld (kolik+1),a

opcont
		ld hl,STARTWINL
		call ROZHOD2
		push hl
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a
kolik	ld de,0
		or a
		ld (usavehl+1),hl
		sbc hl,de
		;add hl,de
		pop de
		ld a,l
		or h
		jp z,loop0
usavehl	ld hl,0		
		;or a
		;sbc hl,bc
		ex de,hl
		dec de
		ld (hl),e
		inc hl
		ld (hl),d
		

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
KL
klavesa	defb 0

leftpos	defw	0

;Test jestli je soubor označený nebo ne
;Z - není označený
;NZ - je označený

CHECKSEL
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
		inc hl
		call BUFF83
		call find83
		ld hl,TMP83
		bit 7,(hl)
		ld a,(curcolor+1)
		jr z,neni_oznacen
		or a
		ld a,80
		ret z
		ld a,96
		ret


neni_oznacen
		or a
		ld a,0
		ret z
		ld a,32
		ret

;Nakreslí kurzor
writecur 
		ld (curcolor+1),a
		call CHECKSEL
		ld (curcolor+1),a
		ld hl,KURZL
		call ROZHOD2
		
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a
		inc hl
		push hl
		ld hl,POSKURZL
		call ROZHOD
		ld a,(hl)
		ld e,a
		ld d,160
		mul d,e
		
		pop hl
		add hl,de
		
curcolor ld a,32
		ld b,38
wr0		ld (hl),a
		inc hl
		inc hl
		djnz wr0
		ret

ROZHOD   ld   a,(OKNO)
         bit  4,a
         ret  z
         inc  hl
         ret  
         
ROZHOD2  ld   a,(OKNO)
         bit  4,a
         ret  z
         inc  hl
         inc  hl
         ret  


ConvertRomCharTo4bpp:
        push    bc
        ld      bc,$08FF
.lineLoop:
        ld      a,(hl)
        inc     hl
        push    hl
        call    .convert8pixels
        pop     hl
        djnz    .lineLoop
        pop     bc
        ret
.convert8pixels:
        call    .convert4pixels
.convert4pixels:
        call    .convert2pixels
.convert2pixels:
        rlca
        rlca
        push    af
        and     3
        ld      hl,.pixelTable
        add     hl,a
        ldi
        pop     af
        ret
.pixelTable:
       DB      $00, $03, $30, $33


showwin	
		
		ld hl,STARTWINL
		call ROZHOD2
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a

		ld a,2
		ld (ypos+1),a
		push hl
		ld hl,ALLFILES
		call ROZHOD2
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a
		ld b,h
		ld c,l

		ld h,b
		ld l,c
		push hl

		ld hl,pathl
		call ROZHOD2
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a
		ld de,3
		add hl,de
		ld a,(hl)
		cp 255
		jr z,sw0
		dec bc
sw0



		pop hl
		ld de,28
		or a
		sbc hl,de
		jr c,SSS
		ld bc,27
		
		
SSS		pop hl
showloop
		push hl
		push bc
		inc hl
		call find83
		call BUFF83					

		ld hl,(foundfile)
		bit 7,(hl)		;testuj jestli je soubor označený
		jr z,nonselect
		ld a,80
		ld (inkcolor+1),a
		jr isselect
nonselect
		ld a,0
		ld (inkcolor+1),a
isselect		



		ld hl,(TMP83+11)
		ld (velikost+1),hl
		pop bc
		pop hl
		push hl
		push bc
		
		call FINDLFN
SSSS
ypos	ld e,2
		ld d,80 * 2
		mul d,e
adrs	ld hl,$4000 + 2

		add hl,de
		ex de,hl
		ld hl,LFNNAME
		
		ld bc,31
shw0	ld a,(hl)
		cp 255
		jr z,shw01
		ld (de),a
		inc de
		inc hl
inkcolor ld a,0
		ld (de),a
		inc de
		dec bc
		ld a,b
		or c
		jr nz,shw0
		
shw01		
		ld hl,TMP83 +7
		bit 7,(hl)
		jr z,shw20
		
dir		ld hl,dirext
		ld bc,5+2
shw1	ld a,(hl)

		ld (de),a
		inc de
		inc hl
		ld a,(inkcolor+1)
		ld (de),a
		inc de
		dec bc
		ld a,b
		or c
		jr nz,shw1
		jr shw3


shw20
velikost ld hl,0		
			

		push de
		call NUM
		ld hl,NUMBUF

		ld b,5
vel1	ld a,(hl)		
		cp "0"
		jr nz,vel2
		ld (hl)," "
		inc hl
		djnz vel1		
vel2	pop de
		ld hl,NUMBUF
shw2	ld b,5
sh		ld a,(hl)
		ld (de),a
		ld a,(inkcolor+1)
		inc de
		ld (de),a
		inc de
		inc hl
		djnz sh
		
		ld a,"k"
		ld (de),a
		inc de
		ld a,(inkcolor+1)
		ld (de),a
		inc de
		ld a,"B"
		ld (de),a
		inc de
		ld a,(inkcolor+1)
		ld (de),a
shw3	pop bc
		pop hl
		inc hl
		
		ld a,(ypos+1)
		inc a
		ld (ypos+1),a
		dec bc
		ld a,b
		or c
		jp nz,showloop

		
		ret
		
dirext	defb "  <DIR>",0		
		
; Násobení HL x B
; Vysledek HL
mull		ld d,l		;vynásob spodní byty
			ld e,b		;
			mul d,e		;vysledek je v de
			ex de,hl	;vysledek je v hl
			ld e,b		;násobitel do e
			mul d,e		;vynásob
			ld a,e		;do akumulátoru hoď výsledek (spodní byte)
			add a,h
			ld h,a		;konečný výsledek je v HL
			ret


;HL ... pozice
find83
		ld b,13
		call mull
		ld de,$a000
		add hl,de
		
		call BUFF83
		ld (foundfile),hl
		ld de,TMP83
		ld bc,13
		ldir
		call NOBUFF83
		ret
FFF
foundfile	defw 0		
TMP83	ds 13
INKEY 	
		xor  a				           
         ld   (aLAST_KEY+1),a		 
		ei

		ld b,2
CEKEJ	halt
		djnz CEKEJ
ahl0		 
		 call KEYSCAN			       

		 ld   a,e
         inc  a
         jr   z,INKEY
         ld   a,d
         ld   hl,SYMTAB
         cp   $18
         jr   z,aHLSM2
         ld   hl,CAPSTAB
         cp   $27
         jr   z,aHLSM2
         ld   hl,NORMTAB
aHLSM2    ld   d,0
         add  hl,de
         ld   a,(hl)
         or   a
         jr   z,INKEY
         
aLAST_KEY ld   b,0
         cp   b
         jr   z,aSEDI_KEY
         
         ld   b,3
aLOOP_LST halt 
         djnz aLOOP_LST
aSEDI_KEY 
         ld   (aLAST_KEY+1),a
		push af
		call beepk
		pop af
		ret 	

;KeyScan od Busyho z MRSu
KEYSCAN  ld   l,47			;testovani klavesnice
         ld   de,65535
         ld   bc,65278
KEYLINE  in   a,(c)
         cpl  
         and  31
         jr   z,KEYDONE
         ld   h,a
         ld   a,l
KEY3KEYS inc  d
         ret  nz
KEYBITS  sub  8
         srl  h
         jr   nc,KEYBITS
         ld   d,e
         ld   e,a
         jr   nz,KEY3KEYS
KEYDONE  dec  l
         rlc  b
         jr   c,KEYLINE
         ld   a,d
         inc  a
         ret  z
         cp   40
         ret  z
         cp   25
         ret  z
         ld   a,e
         ld   e,d
         ld   d,a
         cp   24
         ret  
keysound db 0				;key sound 0= yes,1= no, klavesnicove echo
SYMTAB   db "*^[&%>}/"
         db ",-]'$<{?"
         db ".+($"
         db 200
         db '/',' '
         db 0
         db "=;)@"
         db 201
         db "|:"
         db 32,13,34
         db "_!"
         db 199
         db "~",0
         
CAPSTAB  db "BHY"
         db 10,8
         db "TGV"
         db "NJU"
         db 11,5
         db "RFC"
         db "MKI"
         db 9,4
         db "EDX"
         db 2
         db "LO"
         db 15,6
         db "WSZ"
         db 1,13,"P"
         db 12,7
         db "QA"
         
NORMTAB  db "bhy65tgv"
         db "nju74rfc"
         db "mki83edx"
         db 0
         db "lo92wsz"
         db 32,13
         db "p01qa"
         db 0

beepk		ld a,(keysound)		;Busyho nahradni rutina,kratsi
		or a
		ret nz
		ld a,(BORDER)
		ld e,a
		ld b,$10
		add a,b
;		ld a,$10+border
		out ($fe),a
		ld b,$1c
beepk1		djnz beepk1
		ld a,$08
		add a,e
;		ld a,$08+border
		out ($fe),a
		ret
BORDER   db 1				;okraj


help1 defb "Controls:",0
help2 defb "1:          switch to left panel",0
help3 defb "2:          switch to right panel",0
help4 defb "True Video: switch between panels",0
help5 defb "SS+I:       Info about Calm Commander ",0
help6 defb "5:          Copy files (directory is not not support",0
help7 defb "6:          Move files (directory is not not support",0
help8 defb "7:          Create directory",0
help9 defb "8:          Delete files/directory",0
help10 defb "9:          Rename files/directory",0
help11 defb "0:          Menu (items is not activ",0
help12 defb "+:          Search and select files/directory",0
help13 defb "-:          Search and deselect files/directory",0
help14 defb "BREAK:      Cancel operations (copy, move, delete, ",0
help15 defb "            close this window...)",0

help	call savescr
		ld hl,8 * 256 + 10
		ld bc,60 * 256 + 16
		ld a,16
		call window

		ld hl,11*256+11
		ld a,16
		ld de,help1
		call print

		ld hl,11*256+13
		ld a,16
		ld de,help2
		call print

		ld hl,11*256+14
		ld a,16
		ld de,help3
		call print

		ld hl,11*256+15
		ld a,16
		ld de,help4
		call print

		ld hl,11*256+16
		ld a,16
		ld de,help5
		call print

		ld hl,11*256+17
		ld a,16
		ld de,help6
		call print

		ld hl,11*256+18
		ld a,16
		ld de,help7
		call print

		ld hl,11*256+19
		ld a,16
		ld de,help8
		call print

		ld hl,11*256+20
		ld a,16
		ld de,help9
		call print

		ld hl,11*256+21
		ld a,16
		ld de,help10
		call print

		ld hl,11*256+22
		ld a,16
		ld de,help11
		call print

		ld hl,11*256+23
		ld a,16
		ld de,help12
		call print

		ld hl,11*256+24
		ld a,16
		ld de,help13
		call print

		ld hl,11*256+25
		ld a,16
		ld de,help14
		call print


		ld hl,11*256+26
		ld a,16
		ld de,help15
		call print






help0		
		call INKEY
		cp 1
		jp z,infoend
		jr help0

notimplemented defb "This feature is not yet implemented.",0

notnow
		call savescr
		ld hl,8 * 256 + 10
		ld bc,60 * 256 + 3
		ld a,16
		call window

		ld hl,11*256+11
		ld a,16
		ld de,notimplemented
		call print
		

		ld hl,42*256+13
		ld a,32
		ld de,pressanykeytxt
		call print

		call INKEY
		call loadscr
	
		ld hl,nadpis
		ld de,#4000
		ld bc,80
not0		
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
		jr nz,not0	
	
		jp loop0


info	
		call savescr
		ld hl,8 * 256 + 10
		ld bc,60 * 256 + 10
		ld a,16
		call window

		ld hl,11*256+11
		ld a,16
		ld de,calmcommander
		call print

		ld hl,11*256+13
		ld a,16
		ld de,info1txt
		call print

		ld hl,11*256+15
		ld a,16
		ld de,info2txt
		call print

		ld hl,11*256+16
		ld a,16
		ld de,info3txt
		call print

		ld hl,11*256+18
		ld a,16
		ld de,info5txt
		call print


		ld hl,43*256+20
		ld a,32
		ld de,breaktxt
		call print

info0		
		call INKEY
		cp 1
		jp z,infoend
		jp info0
infoend call loadscr
		jp loop0
		
calmcommander	defb "CALM COMMANDER 0.2 (Development version 2020)",0		
breaktxt defb "BREAK: close this window",0		
info1txt defb "File manager for ZX Spectrum Next. ",0
info2txt defb "Main program: Shrek/MB Maniax",0
info3txt defb "Big help: ped7g",0
info4txt defb " ",0
info5txt defb "Greetinx: Logout, z00m, mborik",0

kresli	
		ld hl,0 * 256 + 1
		;	  delka      vyska
		ld bc,38 * 256 + 27
		ld a,0
		call window
		
		
		
		ld hl,40* 256 + 1
		;	  delka      vyska
		ld bc,38 * 256 + 27
		ld a,0
		call window
		ld a,0
		ld de,bottom
		ld hl,0*256+31
		call print
		
		ld hl,2*256+31
		ld a,32
		ld de,left_txt
		call print
		
		ld hl,10*256+31
		ld a,32
		ld de,right_txt
		call print
		
		ld hl,19*256+31
		ld a,32
		ld de,view_txt
		call print
		
		ld hl,27*256+31
		ld a,32
		ld de,edit_txt
		call print
		
		ld hl,35*256+31
		ld a,32
		ld de,copy_txt
		call print
		
		ld hl,43*256+31
		ld a,32
		ld de,move_txt
		call print

		ld hl,51*256+31
		ld a,32
		ld de,mkdir_txt
		call print
		
		ld hl,60*256+31
		ld a,32
		ld de,delete_txt
		call print

		ld hl,70*256+31
		ld a,32
		ld de,menu_txt
		call print

		
		ret
offset	equ 3		
		
left_txt defb "1: LEFT",0
right_txt defb "2: RIGHT",0
view_txt defb "3:     ",0
edit_txt defb "4:     ",0
copy_txt defb "5: COPY",0
move_txt defb "6: MOVE",0
mkdir_txt defb "7: MKDIR",0
delete_txt defb "8: DELETE",0
menu_txt defb "0: MENU",0




;vstup:
; HL .... XY
; DE .... TEXT zakonceny 0 bytem
; A ..... atribut palety
print  ld (paleta + 1),a
		push de
		ld d,l
		ld e,160
		mul d,e
		ld a,h
		add a,a
		ld l,a
		ld h,0
		add hl,de
		ld de,$4000
		add hl,de
		pop de
print0
		ld a,(de)
		or a
		ret z
		ld (hl),a
		
paleta	ld a,0		
		inc hl
		inc de
		ld (hl),a
		inc hl
		jr print0

window
		ld (atr1+1),a
		ld (atr2+1),a
		ld (atr3+1),a
		ld (atr4+1),a
		ld (atr5+1),a
		ld (atr6+1),a
		ld (atr7+1),a
		ld (atr8+1),a
		ld (atr9+1),a

		ld e,l
		ld d,160
		mul d,e
		ld a,h
		add a,a
		ld l,a
		ld h,0
		add hl,de
		ld de,#4000
		add hl,de		;adresa v tilemode
window0	push hl		
		
		ld a,18
		ld (hl),a
		inc hl
atr1	ld (hl),0
		ld a,b
		ld (w5+1),a
		ld a,16
		inc hl
		
		
w2		ld (hl),a
		inc hl
atr2	ld (hl),0
		inc hl
		djnz w2
		ld a,19
		ld (hl),a
		inc hl
		
atr3	ld (hl),0
		ld de,160-1
		add hl,de
		
		ld (w3+1),hl	;uloz adresu
		pop hl
		ld de,160
		add hl,de
		ld (w4+1),hl

w3		ld hl,0	
		ld a,23
		ld (hl),a
		inc hl
atr4	ld (hl),0
		
		ld de,160-1
		add hl,de
		ld (w3+1),hl
w4		ld hl,0					;leva cast
		ld a,22
		ld (hl),a
		inc hl
atr5	ld (hl),0
		push hl
		inc hl
		ld a,(w5+1)
		ld b,a
cisti	ld 	(hl),0
		inc hl
atr6	ld (hl),0
		inc hl
		djnz cisti
		
		pop hl
		ld de,160-1
		add hl,de
		ld (w4+1),hl
		dec c
		ld a,c
		or a
		jr nz,w3
		
		ld a,21
		ld (hl),a
		inc hl
atr7	ld (hl),0
w5 		ld b,0		
		ld a,17
w6		inc hl
		ld (hl),a
		inc hl
atr8	ld (hl),0
		djnz w6
		inc hl
		ld a,20
		ld (hl),a
		inc hl
atr9	ld (hl),0
		ret





reload_dir
			
			di
			ld hl,catbuff
			ld (Count11+1),hl
			ld hl,ALLFILES
			call ROZHOD2
			xor a
			ld (hl),a
			inc hl
			ld (hl),a
			
			ld hl,numsel
			call ROZHOD2
			xor a
			ld (hl),a
			inc hl
			ld (hl),a
			ld (virtmem),a
						
			call BUFF83
			ld hl,#a000
			ld de,#a001
			ld bc,1024
			xor a
			ld (hl),a
			ldir
			
			call dospage
                               ;be low BFE0h
                              ;in ter rupts can now be en abled

              ld hl,catbuff     ;some where for DOS to put the cata
                                ;log
              ld de,catbuff+1   ;
              ld bc,1024        ;max i mum (for +3DOS) is ac tu ally
                                ;64x13+13 = 845
              ld (hl),0
              ldir              ;make sure at least first en try is
              ld de,catbuff     ;the lo ca tion to be filled with the
aNextDirItem
              ld b,pocetpolozek ;the num ber of en tries in the
                                ;buffer
              ld c,%101            ;include sys tem files in the cata
              ld hl,stardstar   ;the file name ("*.*")
              call dos_catalog  ;call the DOS en try
NEXT0			  
			 
			  
			ld (savehl),hl
			ld (saveix),ix
			ld a,b
			cp pocetpolozek
			push af
			
			push hl
			push de
			push bc			
			
			ld hl,ALLFILES
			call ROZHOD2
			ld a,(hl)
			inc hl
			ld h,(hl)
			ld l,a
 		  	ld e,b
			ld d,0
			add hl,de
			dec hl
			push hl
			ld hl,ALLFILESL
			call ROZHOD2
			ld a,(hl)
			inc hl
			ld h,(hl)
			ld l,a
			ld (N0+1),hl
			pop hl
N0			ld (0),hl
			
			ld hl,(dirNum)
			add	hl,de
			dec hl
			ld 	(dirNum),hl

			pop bc
			pop de
			pop hl
			pop af
			jr c,acont

			ld a,b
			or a
			jr z,acont

			ld a,(virtmem)
			cp 2
			jr z, acont

			call CountMemory
			ex de,hl
			ld hl,virtmem
			inc (hl)
			jr aNextDirItem
acont			  
            push af
			
			ld hl,(dirNum)

			

              pop hl
              ld (dosret),hl    ;put it where it can be seen from
                                ; NextBASIC
              ld c,b            ;move num ber of files in cat a log to
                                ;low byte of BC
              ld b,0            ;this will be re turned in NextBASIC
                  
              di                ;about to ROM/RAM switch so be
                                ;care ful
              push bc           ;save num ber of files
			call basicpage
              pop bc            ;get back the saved num ber of files
              dec bc
              
              

			 call BUFF83
JKJK			 
			 ld hl,catbuff+13
askon		
			call getAllLFN	
			call getdir
			ld hl,(ALLFILES)
			dec hl
			
			push hl
			ld hl,downall
			call ROZHOD2
			ld a,(hl)
			inc hl
			ld h,(hl)
			ld l,a
			pop de
			ex de,hl
			ld (hl),e
			inc hl
			ld (hl),d
			
			ld hl,pathl
			call ROZHOD2
			ld a,(hl)
			inc hl
			ld h,(hl)
			ld l,a
			ld de,3
			add hl,de
			ld a,(hl)
			cp 255
			jr z,jetoroot
			ld a,1
			jr nenitoroot
jetoroot	xor a
nenitoroot		
			ld (pocatek+1),a
	
			ld hl,STARTWINL
			call ROZHOD2
pocatek		ld (hl),0
			
			ret
			
dospage		
		ld bc,port1
        ld a,(bankm) 
        res 4,a      
        or 7         
        ld (bankm),a 
        out (c),a   
		ret
		
basicpage
		ld bc,port1  
        ld a,(bankm)      
        set 4,a           
        and #F8           
        ld (bankm),a      
        out (c),a  
		ret


lfnpage	defb 24,60
			
getAllLFN	
			
			ld hl,0
			ld (numLoop),hl
			ld hl,#e000
			ld (InBuff+1),hl
			
			ld hl,lfnpage
			call ROZHOD
			ld a,(hl)
	
			ld (Page+1),a
			call BUFF83
			ld hl,catbuff+#d
			ld de,bufftmp
			ld bc,13
			ldir
			call NOBUFF83
			
			ld hl,catbuff+#d
			ld (tmpname),hl
			
			ld hl,ALLFILES
			call ROZHOD2
			ld a,(hl)
			inc hl
			ld b,(hl)
			ld c,a
			
LFN1		push bc
			
			ld hl,(numLoop)
			inc hl
			ld 	(numLoop),hl
lfnpos		ld de,18560+24	;pozice ve VideoRam

			di				;zakaž přerušení, jinak bude problém, nemáme nastránkovanou ZX Rom

			call dospage
BFN
BufferName	ld de,bufftmp			;jmeno souboru
			ld hl,stardstar
			ld ix,(savehl)
			ld bc,LFNNAME
			call $01b7  			;zjisti LFN
			ld (LFNNAME + 261),ix
			ld (LFNNAME + 261 + 2),hl
			call basicpage
			
Page		ld a,24
			nextreg $57,a	;nastrankuj stranku s volnymi daty
			
InBuff		ld de,#e000		;ukazatel na pamet v bufferu			
			ld hl,LFNNAME	;buffer pro LFN
			ld bc,maxlen		;ulož 64 bytů
			ldir

			call BUFF83
			ld hl,(tmpname)	;přesuň se na další položku v adresáři
			ld de,13
			add hl,de
			ld (tmpname),hl	;a ulož adresu, kde se nachází
			ld de,bufftmp
			ld bc,13
			ldir
			call NOBUFF83
			ld a,(Page+1)
			nextreg $57,a
			ld hl,(InBuff+1)		;vypočti další adresu v bance ZX Next, kam budu ukládat
			ld de,maxlen
			add hl,de
			ld (InBuff+1),hl
			ld de,#FFFF-261
			or a
			sbc hl,de
			jr c,contin
			ld hl,Page+1
			inc (hl)
			ld hl,#e000
			ld (InBuff+1),hl
contin		pop bc				;zopakuj to pro všechny soubory, které máme načtené... 
			dec bc
			ld a,b
			or c
			jr nz,LFN1
AAA
			nextreg $57,1			;vráť zpátky stránku, kde se nachází data pro player
			ret



; Input: HL = Dividend, C = Divisor, A = 0
; Output: HL = Quotient, A = Remainder
deleno		xor a
			ld b,16
de			
			add	hl,hl	
			rla			
			cp	c		
			jr	c,de1	
			sub	c		
			inc	l		
de1			djnz de			
			ret

addrlfn		dw 0

FINDLFN
			
			push hl
			ld hl,lfnpage
			call ROZHOD
			ld a,(hl)
			ld (lfnroot+1),a
			
			ld hl,LFNNAME
			ld de,LFNNAME+1
			ld bc,maxlen
			ld a,32
			ld (hl),a
			ldir

			pop hl
			or a
			ld de,30
			sbc hl,de
			add hl,de
			jr c,prvni
			ld c,30
			call deleno
		    jr oddeleno
prvni		ld a,l
			ld l,0
			
oddeleno	push af
lfnroot		ld a,24
			add a,l
			nextreg $57,a
			pop bc
			ld de,maxlen
			ld a,b
			or a
			ld hl,$e000		
			jr z,prvnizaznam
lll			
			add hl,de
			djnz lll
			
prvnizaznam 
			ld (addrlfn),hl
			ld de,LFNNAME
			ld bc,maxlen
popop			
			ld a,(hl)
			cp 255
			jr z,kon
			ld (de),a
			inc hl
			inc de
			dec bc
			ld a,b
			or c
			jr nz,popop
			
kon			
			ld hl,(addrlfn)
			ld de,261
			add hl,de
			ld de,LFNNAME+261
			ld bc,4
			ldir				;prenes velikost souboru
			ld hl,LFNNAME
			ld b,40
F22222
			ld a,(hl)
			cp 255
			call z,setspace	
			inc hl
			djnz F22222
lfnend		ld hl,LFNNAME
		
lfnat		ld de,20672+2
			ld hl,LFNNAME
			ret

setspace
		ld (hl),32
		ret

buffl		defb 20
buffr		defb 22

BUFF83		
			push hl
			push de
			push af
			ld hl,buffl	
			call ROZHOD
			ld a,(hl)
			nextreg $55,a	;nastrankuj data pro spravne okno
			pop af
			pop de
			pop hl
			ret

NOBUFF83	
			nextreg $55,5
			ret
CountMemory

			ld de,13*(pocetpolozek-1)
Count11		ld hl,catbuff
			add hl,de
			ld (Count11+1),hl
	
			ret


ALLFILESL  defw ALLFILES, ALLFILES2

ALLFILES    defw 0
ALLFILES2	defw 0
ALLFILESR	defw 0
LFNNAME		defs 270  ;263
tmpname		ds 2
BFT
bufftmp		ds 15		 
M_P3DOS		equ $94		
savehl		defw 0
saveix		defw 0	
bankm       defb 0 ;  equ  #5B5C         ;sys tem vari able that holds the	

stardstar:
            defb "*.*",#FF  
dosret:
            defw 0          
numLoop		defw 0
FILES    	defb 0
dirNum	 	defw 0
tilemapPalette:
                db  %000'000'11,1       ; 0 modra(paper)					0
                db  %100'100'10,1       ; 1 light grey (25% ink)
                db  %010'010'01,1       ; 2 dark grey (75% ink)
                db  %101'101'11,0       ; 0 white-blueish (ink)
                db  %110'001'00,1       ; 4 red
                db  %111'110'00,1       ; 5 yellow
                db  %000'100'00,0       ; 6 green
				ds 18
				db  %000'000'00,0       ; 0 modra (paper)					16
                db  %100'100'10,1       ; 1 light grey (25% ink)
                db  %010'010'01,1       ; 2 dark grey (75% ink)
				db  %101'101'11,0       ; 0 white-blueish (ink)
                db  %110'001'00,1       ; 4 red
                db  %111'110'00,1       ; 5 yellow
                db  %000'100'00,0       ; 6 green
				ds 18
				db  %000'011'10,1       ; 0 modra (paper)					32
                db  %100'100'10,1       ; 1 light grey (25% ink)
                db  %010'010'01,1       ; 2 dark grey (75% ink)
				db  %101'101'11,0       ; 0 white-blueish (ink)
                db  %110'001'00,1       ; 4 red
                db  %111'110'00,1       ; 5 yellow
                db  %000'100'00,0       ; 6 green
				ds 18
				db  %111'111'11,1       ; 0 modra (paper)					48
                db  %100'100'10,1       ; 1 light grey (25% ink)
                db  %010'010'01,1       ; 2 dark grey (75% ink)
				db  %000'000'000,0       ; 0 white-blueish (ink)
                db  %110'001'00,1       ; 4 red
                db  %111'110'00,1       ; 5 yellow
                db  %000'100'00,0       ; 6 green
				ds 18
				db  %011'101'00,1       ; 0 zluta (paper)					64
                db  %100'100'10,1       ; 1 light grey (25% ink)
                db  %010'010'01,1       ; 2 dark grey (75% ink)
				db  %000'000'00,0       ; 0 white-blueish (ink)
                db  %110'001'00,1       ; 4 red
                db  %111'110'00,1       ; 5 yellow
                db  %000'100'00,0       ; 6 green
				ds 18
				db  %100'100'10,0       ; 0 zluta (paper)					80
                db  %100'100'10,1       ; 1 light grey (25% ink)
                db  %010'010'01,1       ; 2 dark grey (75% ink)
				db  %000'000'00,0       ; 0 white-blueish (ink)
                db  %110'001'00,1       ; 4 red
                db  %111'110'00,1       ; 5 yellow
                db  %000'100'00,0       ; 6 green
				ds 18
				db  %101'101'10,1       ; 0 zluta (paper)					80
                db  %100'100'10,1       ; 1 light grey (25% ink)
                db  %010'010'01,1       ; 2 dark grey (75% ink)
				db  %000'000'00,0       ; 0 white-blueish (ink)
                db  %110'001'00,1       ; 4 red
                db  %111'110'00,1       ; 5 yellow
                db  %000'100'00,0       ; 6 green
				ds 18
				
				
				
				
tilemapPalette_SZ:  EQU $ - tilemapPalette            

              
tilemapFont:    ds   16*32
                                                                    ; 24 chars skipped (3*256)
                                                                    ; starts at character 32 - 4 dir_arrows - 3 color dots - 1 reserve = 24

FILEBUFF	
tilemapFont_char24:
            INCLUDE "tilemap_font_8x6.i.asm"
			org 49152


leftcur
		ld hl,POSKURZL
		call ROZHOD
		ld (smcur+1),hl
		ld a,(hl)
		or a
		jr z,leftcur0
		ld a,0
		
		push hl
		call writecur
		pop hl	
		xor a
		ld (hl),a
		ld a,32
		call writecur
		jp loop0


leftcur0
LFT0
		ld hl,0
		push hl
		ld hl,pathl
		call ROZHOD2
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a
		ld de,3
		add hl,de
		ld a,(hl)
		cp 255
		pop hl
		jr z,leftcur1
		inc hl
leftcur1
		ld (leftcur_porovnej + 1),hl


		ld hl,POSKURZL
		call ROZHOD
		ld a,(hl)
		ld l,a
		ld h,0

		push hl
		ld hl,STARTWINL
		call ROZHOD2
		ld (pocatekleft+1),hl
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a

		ex de,hl
		pop hl
		add hl,de
		inc hl			;aktualni pozice v HL
		ld (aktpos+1),hl

leftcur_porovnej 
		ld de,0
		or a
		sbc hl,de
		add hl,de
		jp z,loop0		;odskoc pokud jsi na prvni pozici
		ld de,28
		or a
		sbc hl,de
		add hl,de
		jr c,mensi_nez_28
		ld de,28
aktpos	ld hl,0		
		or a
		sbc hl,de
		ex de,hl
		jr pocatekleft
mensi_nez_28

		ld de,(leftcur_porovnej + 1)
pocatekleft	
		ld hl,0
		ld (hl),e
		inc hl
		ld (hl),d		
								;vykresli znova okno
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

RGHT
rightcur
		ld hl,POSKURZL
		call ROZHOD
		ld (smcur+1),hl
		ld a,(hl)
		cp 26
		jp z,rightcur0			;zobraz další stránku
		




		ld hl,ALLFILES
		call ROZHOD2
		ld a,(hl)
		inc hl
		ld d,(hl)
		ld e,a
		dec de
		push de
		ld hl,pathl
		call ROZHOD2
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a
		ld de,3
		add hl,de
		ld a,(hl)
		cp 255
		pop de
		jr z,aasw0
		
		dec de
aasw0
		ld hl,26
		or a
		sbc hl,de
		add hl,de
		jr c,posledniradek
		ld a,e
		ld (kamcur+1),a
		jr smcur
posledniradek
		ld a,26
		ld (kamcur+1),a
smcur	ld hl,0
		ld a,0
		call writecur
krcur	ld hl,(smcur+1)		
kamcur	ld (hl),26
		ld a,32
		call writecur

		jp loop0
rightcur0

		ld hl,STARTWINL
		call ROZHOD2
		ld (rightsedi+1),hl	;ulož adresu okna, které se bude vykreslovat
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a
		ld (AKT+1),hl
		inc hl
		
		ld de,27 +27
		add hl,de
		push hl
						;HL ... číslo souboru na kterém stojí kurzor + 26 (stránka)
		ld hl,ALLFILES
		call ROZHOD2
		ld a,(hl)
		inc hl
		ld d,(hl)
		ld e,a
		ld (MAXR+1),de
		dec de
		dec de
		pop hl
						;DE ... počet všech souborů v aktuálním okně

		or a
		ex de,hl
		sbc hl,de
		add hl,de

		jr c,right12		;když se nesmí odstránkovat celých 26 souborů
MAXR	ld hl,0
AKT		ld hl,0
		ld de,27
		or a
		add hl,de
		
		add hl,de
		push hl

		ld hl,pathl
		call ROZHOD2
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a
		ld de,3
		add hl,de
		ld a,(hl)
		cp 255
		pop hl
		jr z,asw0
		
		dec hl
asw0

		;dec hl
		ld de,27
		or a
		sbc hl,de
		jr	rightsedi
right12 ld hl,(MAXR+1)
		ld de,27
		or a
		sbc hl,de

rightsedi
  	    ld (0),hl


						;vykresli znova okno
		ld hl,adrl
		call ROZHOD2
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a
		ld (adrs+1),hl
SED
		call getroot
		
		call showwin
		ld a,32
		call writecur
		jp loop0


numsel	defw 0,0
seltxt defb "Selected: ",0
select
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
		inc hl				;v HL máme číslo souboru
		call BUFF83					
		call find83
		call BUFF83					
		ld hl,(foundfile)
		push hl
		ld de,ban1
		ld a,0
		call specific_search
		pop hl
		jp z,down
		push hl
		ld de,ban2
		ld a,0
		call specific_search
		pop hl
		jp z,down
		bit 7,(hl)
		jr z,select_file
		res 7,(hl)
		
		ld hl,numsel
		call ROZHOD2
		push hl
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a
		pop de
		dec hl
		ex de,hl
		ld (hl),e
		inc hl
		ld (hl),d
		
		jr deselect_file
select_file
		set 7,(hl)			;označ soubor
		ld hl,numsel
		call ROZHOD2
		push hl
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a
		pop de
		inc hl
		ex de,hl
		ld (hl),e
		inc hl
		ld (hl),d

deselect_file
		
selcont
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
		jp down

		include "functions/menu.asm"
		include "functions/search.asm"
		include "functions/selected.asm"
last:       
              
              CSPECTMAP player.map
              savenex open "CalmCommander.nex",START,$5ffe
              savenex core 2,0,0
              savenex auto
              savenex close