@echo off
set exe=grredu303
:: MAIN BODY
echo SCRIPT FILE FOR GRREDU3 by T.OJA, LAST MODIF. 2021-03-19
IF NOT "%1"=="-h" GOTO step25
echo USAGE:
echo SCRIPTNAME(%~nx0) ^<obsfile(or txtfile with path/names list of obs files)^>  ^<T^/F^>  ^<coordfile^>
ECHO  E.g. ^>%~n0  obs.list  T  coords.dat
ECHO       ^>%~n0  Calibr2010_S-36.obs
ECHO.
echo  NB! Use NO spaces in filenames. With single obsfile (as 1st argument) the 2nd argument is automatically F.
echo      With obs list file as 1st argument, use T. If central coordinate file with all observed points
echo      is used with full path given in script, the last argument is not needed. With single obsfile also
echo      2nd argument is optional since its def.value is F. Formats of input files are described in manual.
echo      Other arguments (links to gravimeter's and tide files, input parameters,...) are edited in script
echo      and are also printed out for check by user.
goto :EOF
:step25
REM echo Coordfile format: id#(^%8i),name(^%s12),geod.lat ^& lon(dec.deg.),orth./norm.height,g_dot,const.vgg ^& lin.vgg
REM echo          Example:    80010      TTU_GR2  59.394704  24.671466      2.39  -0.48  3027 0
REM echo                   21021901 GRAVsurvey01  59.395040  24.660010     32.9    0.00  3086 0 ...
echo.
echo INSERTING FILE NAMES AND PARAMETER VALUES...
REM echo Redundant lines are commented out (by REM at the beginning), explanation start with ::
REM echo NB!Script NEED TO BE MODIFIED by the user if the NAME LIST of obs.files used!
REM When the list of file names used, set MULT=T (by default single file -> MULT=F)!
set MULT=%2
REM set MULT=T
IF "%MULT%"=="" set MULT=F
ECHO 2ND ARGUMENT FOR SINGLE OBSFILE/MULTIPLE OBSFILES: %MULT%
IF "%MULT%"=="F" echo   -^> Single obsfile name is expected!
IF "%MULT%"=="T" echo   -^> Text file name with path/names list of obs files is expected!
echo NAME OF OBS.DATA FILE:
set infile=%1
IF "%infile%"=="" ECHO ABORT: FILENAME MISSING! & PAUSE & GOTO :EOF
echo  %infile%
ECHO NAME OF COORDINATE FILE:
:: Define the folder where necessary input files are prepared, incl. coordinate file
IF "%gr2main%"=="" SET gr2main=..
set sharef=%gr2main%\share
set _COORD=%3
IF "%_COORD%"=="" set _COORD=%sharef%\coords_NKG16gdot.dat
REM SET _KOORD=D:\data\CG5data\2015\2015-07-14_18_TOIL-HAAN\koords
ECHO  %_COORD%
ECHO.
:: FILLING TEMP FILE...
REM set tmpfile=temp_in
set tmpfile=grredu3.inp
ECHO COLLECTING ALL GRREDU3 INPUT TO "%tmpfile%" ...
ECHO.
echo *** ADDITIONAL ARGUMENTS (CAN BE EDITED IN SCRIPT):
echo SET TIMEZONE (..,-1,0,1,2,..) and EPOCH (YYYY-MM-DD):
set par=0 2000-01-01
echo %par%>%tmpfile% && echo  %par%
echo SET LTIDE,LPRES,LFAIR,LTIME,LCALI,LPMOT:
ECHO NB! FOR HIGH PRECISION USE: T T T T T F , FOR SURVEY: T F T F T F , FOR CALIBRATION: T T T T F F 
set par=T T T T F F
echo %par%>>%tmpfile% && echo  %par%
ECHO SET IPRINT,IMODEL,IRIGID:
echo 0 2 0 >>%tmpfile% && echo  0 2 0
ECHO SET PCOEF (uGal/hPa):
set par=-0.3
echo %par%>>%tmpfile% && echo  %par%
echo *** ADDITIONAL INPUT FILES...
:: Name of coord file name defined before
echo %_COORD%>>%tmpfile%
ECHO LOCAL AMPLITUDE FACTORS, PHASE LAGS FOR THE MAIN TIDAL WAVES(OPTIONAL):
set inpf=%sharef%\coords.tide
REM set inpf=%2.tide
REM set inpf=%_KOORD%.tide
IF "%inpf%"=="" echo SET inpf=%_KOORD%.tide
IF EXIST %inpf% GOTO step50
ECHO  TIDAL WAVE NAME/FILE (*.tide) NOT FOUND, EMPTY FILE IS MADE...
ECHO  NB! FOR HIGH PRECISION GRAVIMETRY PREPARE FILE WITH GTPAR
type nul>%inpf%
:step50
echo %inpf%>>%tmpfile% && echo  %inpf%
ECHO TIDAL POTENTIAL DEVELOPMENTS:
set inpf=%sharef%\ETCPOT.DAT
echo %inpf%>>%tmpfile% && echo  %inpf%
ECHO POLAR MOTION DATA (OPTIONAL - NAME):
set inpf=%sharef%\eopc04.2000-now
echo %inpf%>>%tmpfile% && echo  %inpf%
ECHO CALIBRATION TABLE OF LCR G/D GRAVIMETERS:
set inpf=%sharef%\LCRmeter.tab
echo %inpf%>>%tmpfile% && echo  %inpf%
ECHO PARAMETERS OF CALIBRATION CORRECTIONS
set inpf=%sharef%\gmeters.par
REM set inpf=gmeters.par
echo %inpf%>>%tmpfile% && echo  %inpf%
:: Insert NAME OF DATA FILE into temp file:
IF "%MULT%"=="F" echo %infile%>>%tmpfile%
:: Insert LIST OF DATA FILE NAME(S) into temp file:
IF "%MULT%"=="T" type %infile%>>%tmpfile%
echo.
ECHO CHECK THE CORRECTNESS OF ARGUMENTS TO CONTINUE WITH PROCESSING
ECHO (CTRL+C TO ABORT OR CHANGE INPUT FILE "%tmpfile%" WITH TXT EDITOR)...
PAUSE
REM PAUSE & GOTO :EOF
REM SET _LOG=%exe%_%1.out
SET _LOG=GRREDU3_processing.log
echo %date%, %time%>%_LOG%
echo ------------------------------------------------------->>%_LOG%
echo *** PROGRAM %EXE% START...
%exe%<%tmpfile%>>%_LOG%
REM PAUSE
REM del %tmpfile%
del *.stat
