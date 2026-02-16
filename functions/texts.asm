filename 	  ds   15
typ 		  equ $-4
pocetpolozek	equ 200
virtmem		defb 0
maxlen		equ 261 + 4 + 4		;+4 je délka souboru a 4 cas a datum
maxline 	equ 37
save_allfiles	defw 0	
OKNO 		defb 0

KURZL		defw $4002+160*2		;adresa kurzoru levého okna
KURZR		defw $4002+160*2+80		;adresa kurzoru pravého okna

ACTDISC
actdisc		defb "C","C"

pathl		defw 	PATHLEFT
pathr       defw 	PATHRIGHT

Cfg
PATHLEFT	defb 	"C:",255
			ds 		261
PATHRIGHT   defb 	"C:",255
			ds 		261

POSKURZL	defb 0				;pozice kurzoru v levém okně
POSKURZR	defb 0				;pozice kurzoru v pravém okně

ALLPOSL		defw 0				;celková pozice na souboru
ALLPOSR		defw 0

STARTWINL	defw 1				;pocatecni soubor na zacatku okna
STARTWINR	defw 1
cursorComp	defb 0

DelkaCfg	equ $-PATHLEFT

bottom		defb 	"                                                                                ",0			

ban1		defb 	".      ",$a0,"   ",0
ban2		defb 	"..     ",$a0,"   ",0
banlfn1 	defb 	".",255,0
banlfn2 	defb 	"..",255,0

origin		defb "Original:",0
newfile		defb "New file:",0

help1           defb    "Controls:",0
help2           defb    "1:          switch to left panel",0
help3           defb    "2:          switch to right panel",0
help4           defb    "True Video: switch between panels",0
help5           defb    "SS+I:       Info about Calm Commander ",0
help6           defb    "5:          Copy files (directory is not not support",0
help7           defb    "6:          Move files (directory is not not support",0
help8           defb    "7:          Create directory",0
help9           defb    "8:          Delete files/directory",0
help10          defb    "9:          Rename files/directory",0
help11          defb    "0:          Menu (items is not activ",0
help12          defb    "+:          Search and select files/directory",0
help13          defb    "-:          Search and deselect files/directory",0
help14          defb    "BREAK:      Cancel operations (copy, move, delete, ",0
help15          defb    "            close this window...)",0
help16			defb    "CAPS+1      Change drive in left window",0
help17			defb    "CAPS+2      Change drive in right window",0

calmcommander	defb    "CALM COMMANDER " : VERSION : defb " (Development version 2026)",0		
breaktxt 		defb    "BREAK: close this window",0		
info1txt 		defb    "File manager for ZX Spectrum Next. ",0
info2txt 		defb    "Main program: Shrek/MB Maniax",0
info3txt 		defb    "Big help: ped7g",0
info4txt 		defb    " ",0
info5txt 		defb    "Greetinx: Logout, z00m, mborik",0
info6txt        defb    "Available memory:",0
kB              defb    "kB",0
rtc             defb    "RTC:",0
presenttxt      defb    "present",0
notpresenttxt   defb    "not present",0
left_txt        defb    "1: LEFT",0
right_txt       defb    "2: RIGHT",0
view_txt        defb    "3:     ",0
edit_txt        defb    "4:     ",0
copy_txt        defb    "5: COPY",0
move_txt        defb    "6: MOVE",0
mkdir_txt       defb    "7: MKDIR",0
delete_txt      defb    "8: DELETE",0
menu_txt        defb    "0: MENU",0

file_exists_txt defb    "This file already exists!",0
overwrite_txt   defb    "Do you want to overwrite it?",0

dirext	defb "  <DIR>",0	

onecopytxt 	defb "Copy file: ",0
onemovetxt	defb "Move file: ",0
onedeletetxt 	defb "Delete file/directory: ",0
yestxt		defb "ENTER = yes",0
conttxt		defb "ENTER = continue",0
norun 		defb "This file type cannot be executed.",0
yesall		defb "SPACE = yes to all directory",0
yesallsp	defb "                            ",0

createtxt	defb "ENTER = create",0
renametxt	defb "ENTER = rename",0
searchtxt	defb "ENTER = search",0
savetxt		defb "ENTER = save ",0
notxt 		defb "BREAK = no",0


spaces 		defb "           ",0
pleasewait	defb "Please wait",0
bfname		defs 45
			defb 0

ismove	defb 0
please_wait	defb "Please wait...",0
nocopy	defb "I'm sorry, but you can't copy or move files to the same",0
nocopy2 defb "directory.",0