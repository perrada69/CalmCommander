sjasmplus.exe cc.asm --lst=cc.lst --sym=cc.sym.txt
sjasmplus.exe plugin\text.asm
sjasmplus.exe plugin\zxscreen.asm
sjasmplus.exe plugin\nxi.asm
sjasmplus.exe plugin\pt2test.asm
sjasmplus.exe plugin\pt3test.asm
sjasmplus.exe plugin\stctest.asm
sjasmplus.exe plugin\stptest.asm
sjasmplus.exe plugin\sqtest.asm
sjasmplus.exe plugin\HelloWord.asm

set IMG=D:\Source\Assembler\CSpect\cspect-next-2gb.img
set HDF=D:\Source\Assembler\CSpect\hdfmonkey.exe
set PLUG=CalmCommander/plugin

rem -- Odstranit aktualni pluginy pred kopirovanim --
%HDF% rm %IMG% %PLUG%/text.ccp
%HDF% rm %IMG% %PLUG%/zxscreen.ccp
%HDF% rm %IMG% %PLUG%/nxi.ccp
%HDF% rm %IMG% %PLUG%/pt2test.ccp
%HDF% rm %IMG% %PLUG%/pt3test.ccp
%HDF% rm %IMG% %PLUG%/stctest.ccp
%HDF% rm %IMG% %PLUG%/stptest.ccp
%HDF% rm %IMG% %PLUG%/sqtest.ccp
%HDF% rm %IMG% %PLUG%/HelloWord.ccp
%HDF% rm %IMG% %PLUG%/asctest.ccp
%HDF% rm %IMG% %PLUG%/TXT,ASM,BAS,CFG,INI_Text-Viewer.CCP
%HDF% rm %IMG% %PLUG%/SCR_ZX-Screen.CCP
%HDF% rm %IMG% %PLUG%/NXI_NXI-Image.CCP
%HDF% rm %IMG% %PLUG%/PT2_PT2-Player.CCP
%HDF% rm %IMG% %PLUG%/PT3_PT3-Player.CCP
%HDF% rm %IMG% %PLUG%/STC_STC-Player.CCP
%HDF% rm %IMG% %PLUG%/STP_STP-Player.CCP
%HDF% rm %IMG% %PLUG%/SQT_SQT-Player.CCP
%HDF% rm %IMG% %PLUG%/HLO_Hello-Demo.CCP

rem -- Hlavni binaries --
%HDF% put %IMG% CalmCommander.nex CalmCommander/
%HDF% put %IMG% cc.bin CalmCommander/

rem -- Viewer pluginy --
%HDF% put %IMG% plugin\text.ccp %PLUG%/
%HDF% put %IMG% plugin\zxscreen.ccp %PLUG%/
%HDF% put %IMG% plugin\nxi.ccp %PLUG%/
%HDF% put %IMG% plugin\pt2test.ccp %PLUG%/
%HDF% put %IMG% plugin\pt3test.ccp %PLUG%/
%HDF% put %IMG% plugin\stctest.ccp %PLUG%/
%HDF% put %IMG% plugin\stptest.ccp %PLUG%/
%HDF% put %IMG% plugin\sqtest.ccp %PLUG%/
%HDF% put %IMG% plugin\HelloWord.ccp %PLUG%/

D:\Source\Assembler\CSpect\CSpect.exe -zxnext -basickeys -nextrom -map=player.map -tv -mmc=%IMG%
