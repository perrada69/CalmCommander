;	Souborový manažer pro ZX Spectrum Next			
;	======================================
;
;	Naprogramoval: Shrek/MB Maniax
;	Za vydatné pomoci: ped75g
;
;	
			
			
			
			DEVICE ZXSPECTRUMNEXT
            OPT reset --zxnext --syntax=abfw              
            slot 4

	        MACRO VERSION : defb "0.4" : ENDM

            DEFINE DISP_ADDRESS     $2000
            DEFINE SP_ADDRESS       $3D00
            OPT --zxnext=cspect
            
			DEFINE ORG_ADDRESS      $7000 + 128
            DEFINE TEST_CODE_PAGE   223         ; using the last page of 2MiB RAM (in emulator)
            DEFINE TILE_MAP_ADR     $4000           ; 80*32 = 2560 (10*256)
            DEFINE TILE_GFX_ADR     $6000;$5400           ; 128*32 = 4096
	
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
DET



            org ORG_ADDRESS   
S1			jp START 

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
	
		call dospage
		ld l,0
		ld h,0
		call $01bd
		ld a,e
		ld (pocetstranek),a

disc	ld l,"A"
		cp "M"
		jr z,dalsi
		ld bc,bufferdisc
		call $00F7
		//jr nc,neskenuj
		jr z,dalsi	
DSCDET
dscdet	ld hl,discdetail
		ld (hl),a
		inc hl
		ld (dscdet+1),hl

setdisc	ld hl,listdisc


		ld a,(disc+1)
		ld (hl),a
		inc hl
		ld (setdisc+1),hl
		ld hl,pocetdisku
		inc (hl)


dalsi	
		ld a,(disc+1)
		cp "P"
		jr z,neskenuj
		inc a
		ld (disc+1),a
		jr disc
NES
neskenuj

		call basicpage
		call VSE_NASTAV
		
	




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

		call dospage

		ld a,255
		call $012d		;zjisti z jakého disku je spouštěn calmcommander
		ld (actdisc),a
		ld (actdisc+1),a
		call basicpage

		call reload_dir
		
		ld hl,$4000+2
		ld (adrs+1),hl

		call getroot

		call showwin
		
		call PROHOD	
		call reload_dir
		
		ld hl,$4000+2+80
		ld (adrs+1),hl
		call getroot_reload
		call showwin
		call PROHOD
		ld a,32
		call writecur
		ld a,16

		call freespace

		ld hl,emptypos
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        ld de,emptydir
        xor a
		call print
		ld a,(OKNO)
		xor 16
		ld (OKNO),a

		ld hl,emptypos
        call ROZHOD2
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        ld de,emptydir
        xor a
		call print

		ld a,(OKNO)
		xor 16
		ld (OKNO),a

		call GETDIR
        ld de,emptydir
		ld hl,48*256+1
		call print

		ld hl,50*256+1
		ld de,LFNNAME
		call print
L0
loop0	
		ld   hl,$4000+160*15+23
		ld (PROGS+1),hl

		call gettime
		nextreg $56,0
		nextreg $55,20 


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
		ld (NUMBUF+5),a
		ld de,NUMBUF
		call print

		ld hl,(ALLFILES + 2)
		call NUM
		ld hl,57*256+30
		ld a,0
		ld (NUMBUF+5),a
		ld de,NUMBUF
		call print


		
; ;*******************************
		; ld a,(klavesa)
		; ld l,a
		; xor a
		; ld h,a

		; call NUM
		; ld hl,40*256+31
		; ld a,16
		; ld de,NUMBUF
		; call print


; 		ld a,(POSKURZL)
; 		ld l,a
; 		ld h,0
; 		call NUM
; 		ld hl,1*256+31
; 		ld a,16
; 		ld de,NUMBUF
; 		call print

; 		ld hl,(STARTWINL)
; 		call NUM
; 		ld hl,9*256+31
; 		ld a,16
; 		ld de,NUMBUF
; 		call print

; 		ld a,(POSKURZL)
; 		ld l,a
; 		ld h,0
; 		ex de,hl
; 		ld hl,(STARTWINL)
; 		add hl,de
; 		inc hl
; 		call find83
; 		xor a
; 		ld (TMP83+11),a
; 		ld hl,20*256+31
; 		ld a,16
; 		ld de,TMP83
		
; 		call print

; 		ld a,(POSKURZR)
; 		ld l,a
; 		ld h,0
; 		call NUM
; 		ld hl,41*256+31
; 		ld a,16
; 		ld de,NUMBUF
; 		call print

; 		ld hl,(STARTWINR)
; 		call NUM
; 		ld hl,49*256+31
; 		ld a,16
; 		ld de,NUMBUF
; 		call print

; 		ld a,(POSKURZR)
; 		ld l,a
; 		ld h,0
; 		ex de,hl
; 		ld hl,(STARTWINR)
; 		add hl,de
; 		inc hl
; 		call find83
; 		xor a
; 		ld (TMP83+11),a
; 		ld hl,60*256+31
; 		ld a,16
; 		ld de,TMP83
		
; 		call print

;*******************************

	
	call NOBUFF83
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
		cp 7
		jp z,newdisc_left

		cp 6
		jp z,newdisc_right



		cp "+"
		jp z,select_files
		cp "*"
		jp z,invert_select_files
		cp "-"
		jp z,deselect

		cp "1"
		jp z,leftwin

		cp "2"
		jp z,rightwin
		cp "h"
		jp z,help
		cp "c"
		jp z,CHNG_ATTR

		cp "i"
		jp z,info_file

		cp 199
		jp z,quit

		jp loop0


freespace
		ld hl,24*256 + 30
		ld de,freetxt
		ld a,0
		call print
		ld hl,64*256 + 30
		ld de,freetxt
		ld a,0
		call print

		call dospage
		ld hl,29*256 + 30
		ld (dec32pos+1),hl
		ld a,(actdisc)
		call $121
		ld h,b
		ld l,c
		ex de,hl
		ld b,8
		call DEC32
		ld hl,37*256 + 30
		ld de,kb
		ld a,0
		call print


		ld hl,69*256 + 30
		ld (dec32pos+1),hl
		ld a,(actdisc)
		call $121
		ld h,b
		ld l,c
		ex de,hl
		ld b,8
		call DEC32
		ld hl,77*256 + 30
		ld de,kb
		ld a,0
		call print

		call basicpage
		ret

freetxt	defb "Free:",0
kb 		defb "kB",0
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

invert_select_files_left
		call setleftwin
		jp invert_select_files

invert_select_files_right
		call setrightwin
		jp invert_select_files

deselect_files_right
		call setrightwin
		jp deselect

newdisc_left
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
		jp changedrive



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


newdisc_right
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
		jp changedrive



souboru_na_radek	equ 26


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

LA
bufferdisc	defs 18
LD
listdisc		defs 15
pocetdisku 		defb 0
pocetstranek	defb 0
			
;blocklenght	equ 1024*6
rtcpresent	defb 0

gettime
		call dospage
		call $01cc		;načti čas a datum  DE = time, BC DATE	
		ld (dostime),de
		ld (dosdate),bc
		push af
		call basicpage
		pop af
		jp nc,notimeend
		ld a,1
		ld (rtcpresent),a
		 ld   a,d
         ld   b,e
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
		call NUM
		ld hl,63*256+0
		ld a,16
		ld de,NUMBUF+3
		call print

		ld hl,65*256+0
		ld a,16
		ld de,dvojt
		call print

        pop  af

		ld l,a
		ld h,0
		call NUM
		xor a
		ld (NUMBUF+3+2),a
		ld hl,66*256+0
		ld a,16
		ld de,NUMBUF+3

		call print
		
		

		ld de,(dosdate)
        ld   a,e
        and  31
        push de

		ld l,a
		ld h,0
		call NUM
		ld hl,69*256+0
		ld a,16
		ld de,NUMBUF+3
		call print

		ld hl,71*256+0
		ld a,16
		ld de,tecka
		call print

		pop de
		
		ld   a,e
        ld   b,d
        srl  b
        push bc
        rra  
        rra  
        rra  
        rra  
        rra  
        and  15

		ld l,a
		ld h,0
		call NUM
		;call smaznuly
		ld hl,72*256+0
		ld a,16
		ld de,NUMBUF+3
		call print
		ld hl,74*256+0
		ld a,16
		ld de,tecka
		call print

		pop af

		ld l,a
		ld h,0
		ld de,1980
		add hl,de
		call NUM
		call smaznuly
		ld hl,75*256+0

		ld de,NUMBUF+1
		xor a
		ld (NUMBUF+1+4),a
		ld a,16
		call print

timeend
		ret


notimeend	
		xor a
		ld (rtcpresent),a
		ld hl,63*256+0
		ld a,16
		ld de,notimetxt
		call print	
		ret
notimetxt defb "                ",0
istime	defb 0
den		defb 0
mesic	defb 0
rok		defb 0
dvojt	defb ":",0


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

enter_directory
		call dospage
		ld hl,actdisc
		call ROZHOD
		ld a,(hl)
		call $012d		;změna disku

		call basicpage
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
		call draw.window

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

		ld hl,POSKURZL
		call ROZHOD
		xor a
		ld (hl),a
		
		ld hl,ALLPOSL
		call ROZHOD2
		xor a
		ld (hl),a
		inc hl
		ld (hl),a
star	ld hl,1
		call getroot

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
		call showwin
		ld a,32
		call writecur
		call GETDIR

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

BUFF     defs   11
NUMB      ds 	11				;temp pro vypis cisel 
DEC32	 push iy
		 ld c,32
		 call D32B
DEC32SP  ld   de,NUMB
dec32pos ld   hl,1*256+1
decink	 ld a,0
         call  print
		 pop iy
		 ret

D32B     xor  a
         ld   iy,NUMB
         push de
         push bc
         ld   de,BUFF
         ld   b,10
DCC1     ld   (de),a
         inc  de
         djnz DCC1
         pop  bc
         pop  de
         push bc
         ld   b,$20
DCC2     add  hl,hl
         ex   de,hl
         adc  hl,hl
         ex   de,hl
         push bc
         push de
         ld   bc,$0A0A
         ld   de,BUFF
DCC3     ld   a,(de)
         adc  a,a
         cp   c
         jr   c,DCC4
         sub  c
DCC4     ld   (de),a
         ccf  
         inc  de
         djnz DCC3
         pop  de
         pop  bc
         djnz DCC2
         ld   d,b
         pop  bc
         ld   e,b
         ld   hl,BUFF-1
         add  hl,de
         dec  b
         jr   z,DCC8
DCC5     ld   a,(hl)
         or   a
         jr   nz,DCC6
         ld   a,c
         db $11
DCC6     ld   c,'0'
DCC7     or   c
         dec  hl
         or   a
         jr   z,DCC9
         ld   (iy+0),a
         inc  iy
DCC9     djnz DCC5
DCC8     ld   a,(hl)
         or   '0'
         ld   (iy+0),a
         inc  iy
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
		ret
amroot	
		ld hl,0
		ret


DIRTMP  defb "C:",255

getdir	
		ld hl,actdisc
		call ROZHOD
		ld a,(hl)
		ld (DIRTMP),a

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

PROHOD
		ld a,(OKNO)
		xor 16
		ld (OKNO),a

		call dospage
		ld hl,actdisc
		call ROZHOD
		ld a,(hl)
		call $012d
		call basicpage		;změn aktualni disk
		ret


changewin
CHANGEWIN		

		ld a,0
		call writecur
		call PROHOD
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
;Uloží obrazovku do banky 19 ZX Next.
savescr	
		nextreg $57,19				;Stránka na uložení VideoRam
		ld hl,16384
		ld de,bufscr
		ld bc,32*160
		ldir
		nextreg $57,1				;Nastránkuj zpátky
		ret



;Obnovení obrazovky z 19 stárnky ZX Next.		
loadscr	
		nextreg $57,19
		ld hl,bufscr
		ld de,16384
		ld bc,32*160
		ldir
		nextreg $57,1
		ret


downall	defw 0, 0
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

;Najde podle pozice souboru nazev 8.3, se kterým dále pracujeme
;HL ... pozice
find83
		push hl
		ld hl,pathl
		call ROZHOD2
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a

		ld de,2
		add hl,de
		ld a,(hl)

		pop hl
		cp 255
		jr nz,find830
		dec hl
find830
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

foundfile	defw 0		
TMP83		ds 13


INKEY 	call gettime
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

beepk	ld a,(keysound)		;Busyho nahradni rutina,kratsi
		or a
		ret nz
		ld a,(BORDER)
		ld e,a
		ld b,$10
		add a,b
;		ld a,$10+border
		out ($fe),a
		ld b,$1c
beepk1	djnz beepk1
		ld a,$08
		add a,e
;		ld a,$08+border
		out ($fe),a
		ret
BORDER   db 1				;okraj

offset	equ 3		

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


		module draw
window
UZZ
		ld (atr1+1),a
		ld (atr2+1),a
		ld (atr3+1),a
		ld (atr4+1),a
		ld (atr5+1),a
		ld (atr6+1),a
		ld (atr7+1),a
		ld (atr8+1),a
		ld (atr9+1),a
		ld (atr200+1),a
		

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

		rra			;vyděl dvěma
		ld b,12		;odečti 10
		or a
		sbc a,b
		ld b,a		
		ld (w20+1),a

		ld a,16
		inc hl
		
		
w2		ld (hl),a
		inc hl
atr2	ld (hl),0
		inc hl
		djnz w2

		ld a,"["
		ld (hl),a
		inc hl
		ld a,(atr200+1)
		ld (hl),a
		inc hl
		

		ld b,21
writmes
		ld a," "
		ld (hl),a
		inc hl
		ld a,(atr200+1)
		ld (hl),a
		inc hl
		djnz writmes
		ld a,"]"
		ld (hl),a
		inc hl
		ld a,(atr200+1)
		ld (hl),a
		inc hl		

		ld a,16
		ld (hl),a
		inc hl
		ld a,(atr200+1)
		ld (hl),a
		inc hl		
		ld a,16
w20		ld b,0

w2000	ld (hl),a
		inc hl
atr200	ld (hl),0
		inc hl
		djnz w2000



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

		endmodule


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
			call NOBUFF83

			ret
			
dospage		
		ld bc,port1
        ld a,(bankm) 
        res 4,a      
        or 7         
        ld (bankm),a 
        out (c),a   
		ret
	
basicpage				;Nastránkuje základní stránku ZXOS	
		ld bc,port1  
        ld a,(bankm)      
        set 4,a           
        and #F8           
        ld (bankm),a      
        out (c),a  
		ret

DETT
discdetail		defs 30

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
			or b
			ret z		
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
			ld (LFNNAME + 261),ix		;uloží delku
			ld (LFNNAME + 261 + 2),hl
			ld (LFNNAME + 261 + 4),bc	;uloží datum
			ld (LFNNAME + 261 + 6),de	;uloží čas
			
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
			jp nz,LFN1
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
changedrivetxt defb "Select drive:",0

selecttxt	defb "ENTER = select",0



enterdrv
		call dospage
HNH		
		ld a,(posdrv)
		ld e,a
		ld d,0	
		ld hl,listdisc
		add hl,de
		ld a,(hl)
		push af
		call $012d

		ld hl,pathl
		call ROZHOD2
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a
		
		ld hl,actdisc
		call ROZHOD
	
		pop af
		ld (hl),a

		call basicpage
		call getdir


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
		;jp vol_reload
RLD		call reload_dir

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
		jr z,aroot
		ld a,1
		ld (astar+1),a
		jr arcont
aroot	xor a
		ld (astar+1),a
arcont	ld hl,adrl
		call ROZHOD2
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a
		
		
		ld (adrs+1),hl
astar	ld hl,1
		push hl
		ld hl,ALLFILES
		call ROZHOD2
		ld a,(hl)
		inc hl
		ld h,(hl)
		or h

		pop hl
		jp z,loop0
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
		jr z,asnula
		ld hl,1
		jr ascont
asnula	ld hl,0
ascont

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

		call showwin
		ld a,32
		call writecur
		call GETDIR
		call zobraz_nadpis
		jp loop0



ramdisc	defb " (ramdisc)",0		;v současné verzi se nebude vypisovat
image	defb " (disc image)  ",0
sdcard defb " (SD card)",0
;Vypis kurzoru pri vyberu disku
writecurdrv
		push af
		ld hl,lftw
		call ROZHOD
		ld a,(hl)
		or a
		jr z,levy_panel
		ld hl,$4000 + 160*8 + 17 + 78	
		ld (kkam +1),hl
		jr pravy_panel
levy_panel
		ld hl,$4000 + 160*8 + 15
		ld (kkam +1),hl
pravy_panel
		pop af
		ld (chngcol+1),a
		ld a,(posdrv)
		
		ld e,a
		ld d,160
		mul d,e
kkam	ld hl,0
		add hl,de
		ld b,15
wrcurdrv
chngcol	ld (hl),64
		inc hl
		inc hl
		djnz wrcurdrv		

		ret


chng_save	
		call dospage
		ld d,00000111b
		ld e,0
		ld ix,TMP83+7
		bit 7,(ix+1)
		call z,clr_ro
		bit 7,(ix+2)
		call z,clr_sys
		bit 7,(ix+3)
		call z,clr_arch
		push de
		ld hl,(foundfile)		
		call BUFF83
		ld de,8
		add hl,de
		ex de,hl
		ld hl,TMP83+8
		ld bc,3
		ldir

		call NOBUFF83
		ld hl,TMP83
		ld b,11
		ld hl,TMP83
c_save							;vynuluj všechny stavové bity v názvu (7.)
		res 7,(hl)
		inc hl
		djnz c_save

		ld hl,TMP83
		ld a,255
		ld (TMP83+11),a
		pop de
		call 0148h 
		call basicpage

		call loadscr
		jp loop0

clr_ro	
		set 2,e
		ret
clr_sys	
		set 1,e
		ret
clr_arch	
		set 0,e
		ret


posdrv	defb 0

ALLFILESL  defw ALLFILES, ALLFILES2
name   	defs 60
ALLFILES    defw 0
ALLFILES2	defw 0
ALLFILESR	defw 0
;Buffer pro LFN
LFNNAME		defs 270
;Pomocný buffer pro LFN - při porovnávání
LFNNAME2	defs 270		
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



		include "functions/file.asm"
		include "functions/delete.asm"
		include "functions/input.asm"
		include "functions/createdir.asm"
		include "functions/rename.asm"
		include "functions/texts.asm"
		include "functions/getdir.asm"
              
                                                                    ; 24 chars skipped (3*256)
                                                                    ; starts at character 32 - 4 dir_arrows - 3 color dots - 1 reserve = 24

FILEBUFF	




;*****************************************************************************************
;*****************************************************************************************
;*****************************************************************************************
;*****************************************************************************************
;*****************************************************************************************
;*****************************************************************************************
;*****************************************************************************************
;*****************************************************************************************
;*****************************************************************************************
;*****************************************************************************************
;*****************************************************************************************
;*****************************************************************************************
;*****************************************************************************************
;*****************************************************************************************
;*****************************************************************************************
;*****************************************************************************************
;*****************************************************************************************
;*****************************************************************************************
;*****************************************************************************************
;*****************************************************************************************
;*****************************************************************************************
;*****************************************************************************************
;*****************************************************************************************
;*****************************************************************************************


E1			
			org $a000
			include "functions/copy.asm"
			include "functions/compare.asm"


			org 49152

S2
tilemapFont_char24:
            INCLUDE "tilemap_font_8x6.i.asm"
tilemapFont:    ds   16*32

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

VSE_NASTAV
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
		ret

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
                db  %000'110'00,0       ; 6 green
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

lftw	defb 0
		defb 1

changedrive
		call savescr
		call NOBUFF83
		ld hl,lftw
		call ROZHOD
		ld a,(hl)
		or a
		jr nz,chngright
		ld hl, 4 * 256 + 5
		ld bc,30 * 256 + 17
		ld a,16
		call window

		ld hl,5*256+6
		ld a,16
		ld de,changedrivetxt
		call print					

		ld hl,20*256+22
		ld a,16
		ld de,selecttxt
		call print					
		ld hl,24*256+21
		ld a,16
		ld de,notxt
		call print					

		ld a,(pocetdisku)
		ld b,a
		ld de,listdisc
		exx
		ld hl,discdetail
		exx
		ld hl,$4000 + 160*8 + 14
		jr chngdrv0
chngright
		ld hl,44 * 256 + 5
		ld bc,30 * 256 + 17
		ld a,16
		call window

		ld hl,45*256+6
		ld a,16
		ld de,changedrivetxt
		call print					

		ld hl,60*256+22
		ld a,16
		ld de,selecttxt
		call print					
		ld hl,64*256+21
		ld a,16
		ld de,notxt
		call print					

		ld a,(pocetdisku)
		ld b,a
		ld de,listdisc
		exx
		ld hl,discdetail
		exx
		ld hl,$4000 + 160*8 + 16 + 78

chngdrv0
		ld a,(de)
		ld (hl),a
		inc hl
		ld (hl),16
		inc hl
		ld (hl),":"
		inc hl
		ld (hl),16
		inc hl
		push hl

		exx
		ld a,(hl)
		inc hl
		exx
		cp 4
		pop hl
		push hl
		push de
		push af
		call z,showramdisc
		pop af
		push af
		cp 255
		call z,showimagedisc
		pop af
		cp 5
		call z,showsdcard
		cp 6
		call z,showsdcard
		pop de
		pop hl
		push de
		ld de,160-4
		add hl,de
		pop de
		inc de
		djnz chngdrv0
		ld a,64
		call writecurdrv
chng0	call INKEY
		cp 10
		jr z,curchngdown

		cp 11
		jr z,curchngup
		cp 1
		jr z,cancel
		cp 13
		jp z,enterdrv
		jp chng0
cancel	call loadscr
		call zobraz_nadpis
		jp loop0
curchngup
		ld a,(posdrv)
		cp 0
		jp z,chng0

		ld a,16
		call writecurdrv
		ld a,(posdrv)
		dec a
		ld (posdrv),a
		ld a,64
		call writecurdrv

		jp chng0


curchngdown
		ld a,(posdrv)
		inc a
		ld hl,pocetdisku
		cp (hl)
		jr z,chng0

		ld a,16
		call writecurdrv
		ld a,(posdrv)
		inc a
		ld (posdrv),a
		ld a,64
		call writecurdrv
		jp chng0

showramdisc	
		ld de,ramdisc
		call showtyp
		xor a
		ret
showimagedisc
		ld de,image
		call showtyp
		xor a
		ret

showsdcard 
		ld de,sdcard
		call showtyp
		xor a
		ret

showtyp	
		ld a,(de)
		or a
		ret z
shwtyp0	ld (hl),a
		inc hl
		ld (hl),16
		inc hl
		inc de
		jr showtyp


down
		ld hl,ALLFILES
		call ROZHOD2
		ld a,(hl)
		inc hl
		ld h,(hl)
		or h
		jp z,loop0
		
		
		
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
		
		call showwin
		ld a,32
		call writecur
		jp loop0


up
		ld hl,ALLFILES
		call ROZHOD2
		ld a,(hl)
		inc hl
		ld h,(hl)
		or h
		jp z,loop0

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


showwin	
		ld hl,ALLFILES
		call ROZHOD2
		ld a,(hl)
		inc hl
		ld h,(hl)
		or h
		ret z


		ld hl,STARTWINL
		call ROZHOD2
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a

		ld a,2
		ld (ypos+1),a			;vynuluj Y pozici

		push hl
		ld hl,ALLFILES
		call ROZHOD2
		ld a,(hl)
		inc hl
		ld b,(hl)
		ld c,a

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
		dec hl
		ld a,(hl)
		
		cp 255
		jr z,ssw0
		ld hl,1

		dec bc
		jr sw0
ssw0	pop de
		pop hl
		dec hl
		push hl
		push de
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

		call NOBUFF83
		ret


leftcur
		ld hl,ALLFILES
		call ROZHOD2
		ld a,(hl)
		inc hl
		ld h,(hl)
		or h
		jp z,loop0
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
		ld hl,ALLFILES
		call ROZHOD2
		ld a,(hl)
		inc hl
		ld h,(hl)
		or h
		jp z,loop0
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

;Označí soubor dle HL
OZNA
oznac_soubor_dle_pozice_v_hl
		call BUFF83					
		call find83
		call BUFF83					
		ld hl,(foundfile)
		push hl
		ld de,ban1
		ld a,0
		call specific_search
		pop hl
		jr z,odeselect_file
		push hl
		ld de,ban2
		ld a,0
		call specific_search
		pop hl
		jr z,odeselect_file
		bit 7,(hl)
		jr z,oselect_file
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
		
		jr odeselect_file

oselect_file
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

odeselect_file
		call NOBUFF83
		ret	

numsel	defw 0,0
seltxt defb "Selected: ",0
SES
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


quittxt	defb "You want realy quit from Calm Commander?",0
emul    defb "Sorry, you use emulator... ;) Reset not works.",0

quit
		call savescr
		ld hl,10 * 256 + 10
		ld bc,60 * 256 + 5
		ld a,16
		call window

		ld hl,11*256+11
		ld a,16
		ld de,quittxt
		call print		

		ld hl,60*256+15
		ld a,48
		ld de,yestxt
		call print		

		ld hl,60*256+14
		ld a,16
		ld de,notxt
		call print

quit0		
		call INKEY
		cp 1
		jp z,infoend
		cp 13
		jp z,softreset
		jp quit0

softreset 
		nextreg 2,1		

		ld hl,10 * 256 + 10
		ld bc,60 * 256 + 5
		ld a,16
		call window

		ld hl,11*256+11
		ld a,16
		ld de,emul
		call print		

		ld hl,11*256+15
		ld a,16
		ld de,pressanykeytxt
		call print		


		call INKEY
		call loadscr
		jp loop0


CHNG_ATTR
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

		call BUFF83
		ld hl,(foundfile)
		ld de,ban1
		ld a,0
		call specific_search
		jp z,loop0
		ld hl,(foundfile)
		ld de,ban2
		ld a,0
		call specific_search
		jp z,loop0


		call savescr

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
		ld hl,8 * 256 + 10
		ld bc,60 * 256 + 10
		ld a,16
		call window

		ld hl,11*256+11
		ld a,16
		ld de,attr_nadpis
		call print

		ld hl,11*256+13
		ld a,16
		ld de,readonlytxt
		call print

		ld hl,11*256+14
		ld a,16
		ld de,systemfiletxt
		call print

		ld hl,11*256+15
		ld a,16
		ld de,archivedtxt
		call print

		xor a
		ld hl,LFNNAME+44
		ld (hl),a
		ld hl,24*256+17
		ld a,16
		ld de,LFNNAME
		call print

		ld hl,11*256+17
		ld a,16
		ld de,namefile
		call print

		ld hl,55*256+20
		ld a,48
		ld de,savetxt
		call print		

		ld hl,55*256+19
		ld a,16
		ld de,notxt
		call print		
;vyhodnocení
		
chng00	call showattr
		call INKEY
		cp 1
		jr z,chng_end
		cp "r"
		jp z,switch_ro
		cp "s"
		jp z,switch_sys
		cp "a"
		jp z,switch_arch
		cp 13
		jp z,chng_save
		jr chng00
chng_end
		call loadscr
		jp loop0


switch_sys
		ld ix,TMP83+7
		bit 7,(ix+2)
		jr z,set_sys
		res 7,(ix+2)
		jp chng00
set_sys  set 7,(ix+2)
		jp chng00

switch_arch
		ld ix,TMP83+7
		bit 7,(ix+3)
		jp z,set_arch
		res 7,(ix+3)
		jp chng00
set_arch set 7,(ix+3)
		jp chng00

switch_ro
		ld ix,TMP83+7
		bit 7,(ix+1)
		jr z,set_ro
		res 7,(ix+1)
		jp chng00
set_ro  set 7,(ix+1)
		jp chng00

showattr	
		ld ix,TMP83+7

		bit 7,(ix+1)
		jr z,ro_null
		ld a,27
		jr readonly
ro_null	ld a,25

readonly
		ld hl,$4000 + 160 * 13 + 50
		ld (hl),a
		inc hl
		ld a,16
		ld (hl),a

		bit 7,(ix+2)
		jr z,sys_null
		ld a,27
		jr system
sys_null	ld a,25

system
		ld hl,$4000 + 160 * 14 + 50
		ld (hl),a
		inc hl
		ld a,16
		ld (hl),a


		bit 7,(ix+3)
		jr z,arch_null
		ld a,27
		jr archive
arch_null	ld a,25

archive
		ld hl,$4000 + 160 * 15 + 50
		ld (hl),a
		inc hl
		ld a,16
		ld (hl),a
		ret

showattr_info	
		ld ix,TMP83+7

		bit 7,(ix+1)
		jr z,ro_nulla
		ld a,27
		jr readonlya
ro_nulla	ld a,25

readonlya
		ld hl,$4000 + 160 * 17 + 128
		ld (hl),a
		inc hl
		ld a,16
		ld (hl),a

		bit 7,(ix+2)
		jr z,sys_nulla
		ld a,27
		jr systema
sys_nulla	ld a,25

systema
		ld hl,$4000 + 160 * 18 + 128
		ld (hl),a
		inc hl
		ld a,16
		ld (hl),a


		bit 7,(ix+3)
		jr z,arch_nulla
		ld a,27
		jr archivea
arch_nulla	ld a,25

archivea
		ld hl,$4000 + 160 * 19 + 128
		ld (hl),a
		inc hl
		ld a,16
		ld (hl),a
		ret

namefile	defb "Name of file:",0
attr_nadpis	defb "Edit file attribute",0
readonlytxt	defb "[R]ead only",0
systemfiletxt defb "[S]ystem file",0
archivedtxt defb "[A]rchived",0

readonlytxt2	defb "Read only",0
systemfiletxt2 defb "System file",0
archivedtxt2 defb "Archived",0


info_file
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

		call BUFF83
		ld hl,(foundfile)
		ld de,ban1
		ld a,0
		call specific_search
		jp z,loop0
		ld hl,(foundfile)
		ld de,ban2
		ld a,0
		call specific_search
		jp z,loop0


		call savescr

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
		ld hl,8 * 256 + 10
		ld bc,60 * 256 + 10
		ld a,16
		call window

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
		ld hl,8 * 256 + 10
		ld bc,60 * 256 + 10
		ld a,16


		ld hl,11*256+11
		ld a,16
		ld de,fileinfonadpis
		call print

		xor a
		ld hl,LFNNAME+44
		ld (hl),a
		ld hl,25*256+13
		ld a,16
		ld de,LFNNAME
		call print

		ld hl,11*256+13
		ld a,16
		ld de,namefile
		call print

		ld de,filedate
		ld hl,11*256+15
		ld a,16
		call print
		ld de,filetime
		ld hl,11*256+16
		ld a,16
		call print

		ld de,(LFNNAME+261+4)
		ld hl,17*256+15
		call showdate
		ld hl,17*256+16
		ld de,(LFNNAME+261+6)
		call showtime

		ld hl,51*256+15
		ld a,16
		ld de,sysatrtxt
		call print


		ld hl,51*256+17
		ld a,16
		ld de,readonlytxt2
		call print

		ld hl,51*256+18
		ld a,16
		ld de,systemfiletxt2
		call print

		ld hl,51*256+19
		ld a,16
		ld de,archivedtxt2
		call print

		ld hl,11*256+18
		ld a,16
		ld de,sizetxt
		call print


		ld hl,16*256+18
		ld (dec32pos+1),hl
		ld hl,(LFNNAME+261)
		ld de,(LFNNAME+261+2)
		ld b,10
		ld a,16
		ld (decink+1),a
		call DEC32
		ld a,0
		ld (decink+1),a

		ld hl,27*256+18
		ld a,16
		ld de,bytestxt
		call print


		ld hl,11*256+20
		ld a,16
		ld de,pressanykeytxt
		call print



		call showattr_info
		call INKEY
		call loadscr
		jp loop0

sizetxt	defb "Size:",0
bytestxt defb "bytes",0
;DE - datum
;HL - pozice
showdate
		ld (shdt1+1),hl
		inc h
		inc h
		ld (shdt2+1),hl
		inc h
		ld (shdt3+1),hl
		inc h
		inc h
		ld (shdt4+1),hl
		inc h
		ld (shdt5+1),hl

        ld   a,e
        and  31
        push de

		ld l,a
		ld h,0
		call NUM
shdt1	ld hl,17*256+15
		ld a,16
		ld de,NUMBUF+3
		call print

shdt2	ld hl,19*256+15
		ld a,16
		ld de,tecka
		call print

		pop de
		
		ld   a,e
        ld   b,d
        srl  b
        push bc
        rra  
        rra  
        rra  
        rra  
        rra  
        and  15

		ld l,a
		ld h,0
		call NUM
		;call smaznuly
shdt3	ld hl,20*256+15
		ld a,16
		ld de,NUMBUF+3
		call print
shdt4 	ld hl,22*256+15
		ld a,16
		ld de,tecka
		call print

		pop af

		ld l,a
		ld h,0
		ld de,1980
		add hl,de
		call NUM
		call smaznuly
shdt5	ld hl,23*256+15
		ld a,16
		ld de,NUMBUF+1
		call print
		ret
;DE - čas ve formátu MSDOS
;HL - pozice
showtime
		ld (shtm1+1),hl
		inc h
		inc h
		ld (shtm2+1),hl
		inc h
		ld (shtm3+1),hl
		inc h
		inc h
		ld (shtm4+1),hl
		inc h
		ld (shtm5+1),hl
							;zjisti vteriny
		ld a,e
		and 00011111b
		add a,a
		ld (vteriny+1),a
		 ld   a,d
         ld   b,e
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
		call NUM
shtm1	ld hl,17*256+16
		ld a,16
		ld de,NUMBUF+3
		call print

shtm2	ld hl,19*256+16
		ld a,16
		ld de,dvojtecka
		call print

         pop  af

		ld l,a
		ld h,0
		call NUM
shtm3	ld hl,20*256+16
		ld a,16
		ld de,NUMBUF+3
		call print

shtm4	ld hl,19*256+16
		ld a,16
		ld de,dvojtecka
		call print


vteriny	ld l,0		
		ld h,0
		call NUM
shtm5	ld hl,20*256+16
		ld a,16
		ld de,NUMBUF+3
		call print

		ret

smaznuly
		ld hl,NUMBUF

		ld b,5
snuly	ld a,(hl)		
		cp "0"
		ret nz
		ld (hl)," "
		inc hl
		djnz snuly
		ret

help	call savescr
		ld hl,8 * 256 + 4
		ld bc,60 * 256 + 18
		ld a,16
		call window

		ld hl,11*256+5
		ld a,16
		ld de,help1
		call print

		ld hl,11*256+7
		ld a,16
		ld de,help2
		call print

		ld hl,11*256+8
		ld a,16
		ld de,help3
		call print

		ld hl,11*256+9
		ld a,16
		ld de,help4
		call print

		ld hl,11*256+10
		ld a,16
		ld de,help5
		call print

		ld hl,11*256+11
		ld a,16
		ld de,help6
		call print

		ld hl,11*256+12
		ld a,16
		ld de,help7
		call print

		ld hl,11*256+13
		ld a,16
		ld de,help8
		call print

		ld hl,11*256+14
		ld a,16
		ld de,help9
		call print

		ld hl,11*256+15
		ld a,16
		ld de,help10
		call print

		ld hl,11*256+16
		ld a,16
		ld de,help11
		call print

		ld hl,11*256+17
		ld a,16
		ld de,help12
		call print

		ld hl,11*256+18
		ld a,16
		ld de,help13
		call print

		ld hl,11*256+19
		ld a,16
		ld de,help14
		call print


		ld hl,11*256+20
		ld a,16
		ld de,help15
		call print

		ld hl,11*256+21
		ld a,16
		ld de,help16
		call print

		ld hl,11*256+22
		ld a,16
		ld de,help17
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

		ld hl,11*256+14
		ld a,16
		ld de,info6txt
		call print

		ld a,(pocetstranek)
		ld e,a
		ld d,8
		mul d,e
		ex de,hl
		call NUM
		ld hl,30*256+14
		ld a,16
		ld de,NUMBUF+1
		
		call print

		ld hl,34*256+14
		ld a,16
		ld de,kB
		call print

		ld hl,11*256+15
		ld a,16
		ld de,rtc
		call print

		ld de,presenttxt
		ld a,(rtcpresent)
		or a
		jr nz,rtcje
		ld de,notpresenttxt
rtcje	
		
		ld hl,16*256+15
		ld a,16

		call print



		ld hl,11*256+17
		ld a,16
		ld de,info2txt
		call print

		ld hl,11*256+18
		ld a,16
		ld de,info3txt
		call print

		ld hl,11*256+19
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

kresli	
		call prekresli_prazdne_okna
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


FINDLFN
		push hl
		ld hl,pathl
		call ROZHOD2
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a

		ld de,2
		add hl,de
		ld a,(hl)

		pop hl
		cp 255
		jr nz,findlfn830
		inc hl
findlfn830
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
prvni	ld a,l
		ld l,0
			
oddeleno	push af
lfnroot	ld a,24
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
			ld bc,8
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





sysatrtxt	defb "System attributes:",0
fileinfonadpis
			defb "File/directory informations:",0 
filedate	defb "Date:",0
filetime	defb "Time:",0
tecka		defb ". ",0
dvojtecka 	defb ":",0
DISC



		include "functions/menu.asm"
		include "functions/search.asm"
		include "functions/selected.asm"
last:       
E2
 		;	SAVEBIN "cc1.bin",S1,E1-S1
 		;	SAVEBIN "cc2.bin",S2,E2-S2
			SAVEBIN "cc.bin", S1, E2-S1



              CSPECTMAP player.map
              savenex open "CalmCommander.nex",START,START-1
              savenex core 2,0,0
              savenex auto
              savenex close