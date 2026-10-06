@echo off
rem Automaticke testy Calm Commanderu (viz tests\README.md)
rem   test.bat            vsechny testy
rem   test.bat -v         s nazvem kazdeho testu
rem   test.bat -k sort    jen testy, jejichz nazev obsahuje "sort"
cd /d %~dp0
python -m unittest discover -s tests %*
