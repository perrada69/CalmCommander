@echo off
setlocal

rem ============================================================
rem CalmCommander - build + copy to CSpect SD image + run CSpect
rem Usage: compile.bat [clean|build|run]
rem   clean - smaze build vystupy
rem   build - jen sestavi projekt
rem   run   - sestavi, nakopiruje do img a spusti CSpect
rem   bez argumentu = stejne jako run
rem ============================================================

set "PROJECT=CalmCommander"
set "SJASMPLUS=sjasmplus.exe"
set "BIN=build\cc.bin"
set "BAS=build\cc.bas"
set "CSPECT_LOG=cspect_debug.log"

set "CSPECT_DIR=D:\Source\Assembler\CSpect"
set "IMG=%CSPECT_DIR%\cspect-next-2gb.img"
set "HDF=%CSPECT_DIR%\hdfmonkey.exe"
set "CSPECT=%CSPECT_DIR%\CSpect.exe"
set "PLUG=CalmCommander/plugin"

if /I "%1"=="clean" goto :do_clean
if /I "%1"=="build" goto :do_build_only
if /I "%1"=="run" goto :do_run
if "%1"=="" goto :do_run

echo Unknown argument: %1
echo Usage: compile.bat [clean^|build^|run]
exit /b 1

:do_clean
echo [CLEAN] Deleting build files...
if exist build\*.bin del /Q build\*.bin
if exist build\*.bas del /Q build\*.bas
if exist build\*.lst del /Q build\*.lst
if exist build\*.sym del /Q build\*.sym
if exist build\plugin\*.ccp del /Q build\plugin\*.ccp
if exist "%CSPECT_LOG%" del /Q "%CSPECT_LOG%"
echo [OK]
goto :end

:do_build_only
call :build_project
goto :end

:do_run
call :build_project
if errorlevel 1 goto :end

echo.
echo [SD] Odstranuji stare soubory ze SD karty...
"%HDF%" rm "%IMG%" %PLUG%/text.ccp 2>nul
"%HDF%" rm "%IMG%" %PLUG%/zxscreen.ccp 2>nul
"%HDF%" rm "%IMG%" %PLUG%/nxi.ccp 2>nul
"%HDF%" rm "%IMG%" %PLUG%/pt2test.ccp 2>nul
"%HDF%" rm "%IMG%" %PLUG%/pt3test.ccp 2>nul
"%HDF%" rm "%IMG%" %PLUG%/stctest.ccp 2>nul
"%HDF%" rm "%IMG%" %PLUG%/stptest.ccp 2>nul
"%HDF%" rm "%IMG%" %PLUG%/sqtest.ccp 2>nul
"%HDF%" rm "%IMG%" %PLUG%/HelloWord.ccp 2>nul
"%HDF%" rm "%IMG%" %PLUG%/asctest.ccp 2>nul
"%HDF%" rm "%IMG%" %PLUG%/TXT,ASM,BAS,CFG,INI_Text-Viewer.CCP 2>nul
"%HDF%" rm "%IMG%" %PLUG%/SCR_ZX-Screen.CCP 2>nul
"%HDF%" rm "%IMG%" %PLUG%/NXI_NXI-Image.CCP 2>nul
"%HDF%" rm "%IMG%" %PLUG%/PT2_PT2-Player.CCP 2>nul
"%HDF%" rm "%IMG%" %PLUG%/PT3_PT3-Player.CCP 2>nul
"%HDF%" rm "%IMG%" %PLUG%/STC_STC-Player.CCP 2>nul
"%HDF%" rm "%IMG%" %PLUG%/STP_STP-Player.CCP 2>nul
"%HDF%" rm "%IMG%" %PLUG%/SQT_SQT-Player.CCP 2>nul
"%HDF%" rm "%IMG%" %PLUG%/HLO_Hello-Demo.CCP 2>nul

echo [SD] Kopiruji soubory na SD kartu...
echo   [PUT] %BIN% -^> CalmCommander/
"%HDF%" put "%IMG%" "%BIN%" CalmCommander/
if errorlevel 1 (
    echo *** KOPIROVANI BIN SELHALO ***
    exit /b 1
)
echo   [PUT] build\cc_xb.bin -^> CalmCommander/
"%HDF%" rm "%IMG%" CalmCommander/cc_xb.bin 2>nul
"%HDF%" put "%IMG%" build\cc_xb.bin CalmCommander/
if errorlevel 1 (
    echo *** KOPIROVANI XB SELHALO ***
    exit /b 1
)
echo   [PUT] %BAS% -^> CalmCommander/
"%HDF%" put "%IMG%" "%BAS%" CalmCommander/
if errorlevel 1 (
    echo *** KOPIROVANI BAS SELHALO ***
    exit /b 1
)
echo   [PUT] plugins -^> %PLUG%/
"%HDF%" put "%IMG%" build\plugin\text.ccp %PLUG%/
"%HDF%" put "%IMG%" build\plugin\zxscreen.ccp %PLUG%/
"%HDF%" put "%IMG%" build\plugin\nxi.ccp %PLUG%/
"%HDF%" put "%IMG%" build\plugin\pt2test.ccp %PLUG%/
"%HDF%" put "%IMG%" build\plugin\pt3test.ccp %PLUG%/
"%HDF%" put "%IMG%" build\plugin\stctest.ccp %PLUG%/
"%HDF%" put "%IMG%" build\plugin\stptest.ccp %PLUG%/
"%HDF%" put "%IMG%" build\plugin\sqtest.ccp %PLUG%/
"%HDF%" put "%IMG%" build\plugin\HelloWord.ccp %PLUG%/

echo.
echo [OK] Soubory zkopirovany na SD kartu.
if exist "%CSPECT_LOG%" del /Q "%CSPECT_LOG%"
echo Spoustim CSpect... (log: %CD%\%CSPECT_LOG%)
"%CSPECT%" -zxnext -basickeys -nextrom -map=player.map -tv -mmc="%IMG%" 1>>"%CSPECT_LOG%" 2>&1
set "CSPECT_EXIT=%ERRORLEVEL%"
echo.
echo --- POSLEDNI RADKY CSPECT LOGU ---
powershell -NoProfile -Command "if (Test-Path '%CD%\%CSPECT_LOG%') { Get-Content '%CD%\%CSPECT_LOG%' -Tail 80 }"
echo.
echo [INFO] CSpect exit code: %CSPECT_EXIT%
exit /b %CSPECT_EXIT%

:build_project
echo Building %PROJECT%...
echo.

if not exist build\nul mkdir build
if not exist build\plugin\nul mkdir build\plugin

echo [BUILD] cc.asm ...
"%SJASMPLUS%" cc.asm --lst=build\cc.lst --sym=build\cc.sym.txt
if errorlevel 1 (
    echo.
    echo *** BUILD FAILED: cc.asm ***
    exit /b 1
)

rem -- sjasmplus vytvari cc.bin, cc.bas a cc_xb.bin v rootu, prekopirujem do build\ --
if exist cc.bin copy /Y cc.bin "%BIN%" >nul
if exist cc.bas copy /Y cc.bas "%BAS%" >nul
if exist cc_xb.bin copy /Y cc_xb.bin "build\cc_xb.bin" >nul

if not exist "%BIN%" (
    echo.
    echo *** BUILD OK, ALE cc.bin NENALEZEN ***
    exit /b 1
)

echo [BUILD] plugins...
"%SJASMPLUS%" plugin\text.asm
if errorlevel 1 ( echo *** BUILD FAILED: text.asm *** & exit /b 1 )
if exist plugin\text.ccp ( copy /Y plugin\text.ccp build\plugin\text.ccp >nul )

"%SJASMPLUS%" plugin\zxscreen.asm
if errorlevel 1 ( echo *** BUILD FAILED: zxscreen.asm *** & exit /b 1 )
if exist plugin\zxscreen.ccp ( copy /Y plugin\zxscreen.ccp build\plugin\zxscreen.ccp >nul )

"%SJASMPLUS%" plugin\nxi.asm
if errorlevel 1 ( echo *** BUILD FAILED: nxi.asm *** & exit /b 1 )
if exist plugin\nxi.ccp ( copy /Y plugin\nxi.ccp build\plugin\nxi.ccp >nul )

"%SJASMPLUS%" plugin\pt2test.asm
if errorlevel 1 ( echo *** BUILD FAILED: pt2test.asm *** & exit /b 1 )
if exist plugin\pt2test.ccp ( copy /Y plugin\pt2test.ccp build\plugin\pt2test.ccp >nul )

"%SJASMPLUS%" plugin\pt3test.asm
if errorlevel 1 ( echo *** BUILD FAILED: pt3test.asm *** & exit /b 1 )
if exist plugin\pt3test.ccp ( copy /Y plugin\pt3test.ccp build\plugin\pt3test.ccp >nul )

"%SJASMPLUS%" plugin\stctest.asm
if errorlevel 1 ( echo *** BUILD FAILED: stctest.asm *** & exit /b 1 )
if exist plugin\stctest.ccp ( copy /Y plugin\stctest.ccp build\plugin\stctest.ccp >nul )

"%SJASMPLUS%" plugin\stptest.asm
if errorlevel 1 ( echo *** BUILD FAILED: stptest.asm *** & exit /b 1 )
if exist plugin\stptest.ccp ( copy /Y plugin\stptest.ccp build\plugin\stptest.ccp >nul )

"%SJASMPLUS%" plugin\sqtest.asm
if errorlevel 1 ( echo *** BUILD FAILED: sqtest.asm *** & exit /b 1 )
if exist plugin\sqtest.ccp ( copy /Y plugin\sqtest.ccp build\plugin\sqtest.ccp >nul )

"%SJASMPLUS%" plugin\HelloWord.asm
if errorlevel 1 ( echo *** BUILD FAILED: HelloWord.asm *** & exit /b 1 )
if exist plugin\HelloWord.ccp ( copy /Y plugin\HelloWord.ccp build\plugin\HelloWord.ccp >nul )

echo.
echo [OK] Build hotov.
echo.
echo --- VELIKOST cc.bin ---
dir "%BIN%" | findstr /i "cc.bin"
echo.
exit /b 0

:end
endlocal
