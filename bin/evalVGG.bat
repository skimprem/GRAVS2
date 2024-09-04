@ECHO OFF
SET _exe=Wzz21
ECHO SCRIPT FILE TO EVALUATE VERTICAL GRAV GRADIENT WITH %_exe% (BY T.OJA, LAST MODIF. 2021-03-21)
IF NOT "%1"=="-h" GOTO step25
ECHO  USAGE:
ECHO   SCRIPTNAME(%~nx0) ^<datafile^>  ^<deg of poly^>  ^<sigma0^>
ECHO   Example: ^>%~n0  Haanja_2008-2019.dat 2 1
ECHO            ^>%~n0  Haanja.dat
ECHO.
ECHO All variables after datafile are optional. By default deg=1, sigma0=1, ie linear model
ECHO with a priori standard deviation of unit weight is used. Script assumes a datafile name 
ECHO that begins with the station name separated by "." or "_", e.g. "Haanja.dat",
ECHO "Haanja_2008-2019.dat". If "body.inp" file with dimensions, densities of modelled bodies 
ECHO is found then RCR method is used. Otherwise polynomial function is applied for VGG model.
goto :EOF
:step25
:: MAIN BODY
set _dataf=%1
set _datafn=%~n1
REM set nimi=%_datafn:~0,5%
SET _lopp=%_datafn:*_=%
CALL SET _nimi=%%_datafn:_%_lopp%=%%
ECHO Input data file: %_dataf%
ECHO Station name: "%_nimi%"
ECHO.
REM set _deg=2
set _deg=%2
IF "%_deg%"=="" set _deg=1
set _s0p=%3
IF "%_s0p%"=="" set _s0p=1
set _tmpfile=wzz2.inp
set _met=POLY
set _bodyf=body.inp
IF EXIST %_bodyf% SET _met=RCR
(echo %_bodyf%
REM echo %3 > %_tmpfile%
REM echo data.inp >> %_tmpfile%
echo %_dataf%
echo %_deg%
REM echo T 1.0 >>%_tmpfile%
echo T %_s0p%
)>%_tmpfile%
ECHO %_exe% INPUT FILE %_tmpfile%:
type %_tmpfile%
REM set _outfile=%_exe%.out
REM set _fnimi=%_nimi%_%_met%_d%_deg%_%_exe:~0,5%
set _fnimi=%_datafn%_%_met%_d%_deg%_%_exe:~0,5%
set _outfile=%_fnimi%.out
ECHO File: %_outfile% >%_outfile%
echo Comp.time: %date%, %time% >> %_outfile%
echo ------------------------------------------------------- >>%_outfile%
echo -------------------------------------------------------
ECHO CHECK THE CORRECTNESS OF ARGUMENTS TO CONTINUE WITH PROCESSING
ECHO (CTRL+C TO ABORT OR CHANGE INPUT FILE "%_tmpfile%" WITH TXT EDITOR)...
PAUSE
ECHO Command line used: "%_exe% < %_tmpfile% > %_outfile%"
echo *** PROGRAM %_exe% STARTS...
%_exe% < %_tmpfile% >> %_outfile%

7z a -tzip %_fnimi%.vgg.zip %_nimi%*.vgg
REM DEL %_tmpfile% %_nimi%*.vgg
REM PAUSE
