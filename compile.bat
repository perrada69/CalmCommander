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
%HDF% put %IMG% plugin\TXT,ASM,BAS,CFG,INI_Text-Viewer.CCP %PLUG%/
%HDF% put %IMG% plugin\SCR_ZX-Screen.CCP %PLUG%/
%HDF% put %IMG% plugin\NXI_NXI-Image.CCP %PLUG%/
%HDF% put %IMG% plugin\PT2_PT2-Player.CCP %PLUG%/
%HDF% put %IMG% plugin\PT3_PT3-Player.CCP %PLUG%/
%HDF% put %IMG% plugin\STC_STC-Player.CCP %PLUG%/
%HDF% put %IMG% plugin\STP_STP-Player.CCP %PLUG%/
%HDF% put %IMG% plugin\SQT_SQT-Player.CCP %PLUG%/
%HDF% put %IMG% plugin\HLO_Hello-Demo.CCP %PLUG%/

D:\Source\Assembler\CSpect\CSpect.exe -zxnext -basickeys -nextrom -map=player.map -tv -mmc=%IMG%
