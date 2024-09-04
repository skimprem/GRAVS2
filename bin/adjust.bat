@echo off
set exe=Gradj305
echo SCRIPT FILE FOR ADJUSTMENT WITH GRADJ3 by T.OJA, LAST MODIF. 2021-03-19
IF NOT "%1"=="-h" GOTO step25
echo USAGE:
echo SCRIPTNAME(%~nx0) ^<redufile(or txtfile with path/names list of redu files)^>  ^<projfile^>  ^<fixfile^>  ^<T^/F^>
ECHO  E.g. ^>%~n0  redu.list  Ice_survey_2010  FIXED  T
ECHO       ^>%~n0  Calibr2010_S-36.redu  Calibr_lineA_2010
ECHO.
echo  NB! Use NO spaces in filenames. With single redufile (as 1st argument) other arguments are optional
echo      (useful e.g. for quick processing). Then def.projfile=ADJ0, fixfile=FIXED are prepared by script.
echo      If input is redu list file, all 4 arguments are required and last argument must be T (default is F).
echo      If no projfile exists, give a project name (without extension) and file with def.values is prepared 
echo      to be edited later. If no fixfile name given, empty file FIXED is made.
goto :EOF
:step25
echo INSERTING FILE NAMES AND PARAMETER VALUES...
REM NB! Redundant lines are commented out (by REM at the beginning)
REM When the list of file names used, set MULT=T (by default single file -> MULT=F)!
set MULT=%4
REM set MULT=T
IF "%MULT%"=="" set MULT=F
ECHO Last argument: %MULT%
IF "%MULT%"=="F" echo   Single redufile name is expected!
IF "%MULT%"=="T" echo   Text file name with path/names list of redu files is expected!
ECHO.
echo NAME OF REDUCED OBS.DATA FILE:
set infile=%1
IF "%infile%"=="" ECHO ABORT: FILENAME MISSING! & PAUSE & GOTO :EOF
echo  %infile%
echo.
echo FILE NAME OF FIXED STATIONS:
set fixfile=%3
REM set fixfile=D:\data\CG5data\fixed_points
IF "%fixfile%"=="" SET fixfile=FIXED
echo  %fixfile%
IF EXIST %fixfile% GOTO step50
echo  FIXFILE NOT FOUND, EMPTY FIXFILE IS MADE...
type nul > %fixfile%
:step50
echo.
echo PROJECT NAME:
REM (NAME (without .proj extension!) is optional, without NAME script makes a default proj file (can be later renamed and/or edited)!)
set proj=%2
IF "%proj%"=="" SET proj=ADJ0
echo  %proj%
ECHO.
:: Check the existence of proj file, if exists keeps it
IF EXIST "%proj%.proj" GOTO step65
echo PROJECT FILE NOT FOUND, NEW FILE "%proj%.proj" WITH DEF.VALUES IS MADE...
echo 6 F 99 >%proj%.proj
echo 0.005 1.0 0.95>>%proj%.proj
echo 5000 0.005>>%proj%.proj
echo F 2008-07-31>>%proj%.proj
:step65
echo.
ECHO ADDITIONAL ARGUMENTS (CAN BE EDITED IN SCRIPT):
echo SCALE FACTOR FOR GRAPH IN *.resi/mean.resi FILE (uGal/div):
set gsf=10
set gsf=1
REM set gsf=%3
echo  %gsf%
echo DETERMINE VERBOSE MODE (BY DEFAULT =F, FOR PROGRAM TESTING PURPOSES =T):
set vmode=F
echo  %vmode%
ECHO.
ECHO COLLECTING ALL INPUT OF GRADJ3 TO...
REM set tmpfile=in  =%proj%_gradj3-input.txt
set tmpfile=%proj%_gradj3.inp
echo  %tmpfile%
REM to keep tmpfile: 
 IF EXIST "%tmpfile%" GOTO step70
ECHO MAKING INPUT FILE "%tmpfile%" ...
echo %proj%.proj > %tmpfile%
echo %fixfile% >> %tmpfile%
echo %gsf% >> %tmpfile%
echo %vmode% >> %tmpfile%
REM NAME OF DATA FILE(S) into temp file:
IF "%MULT%"=="F" echo %infile% >> %tmpfile%
IF "%MULT%"=="T" type %infile% >> %tmpfile%
 GOTO step85
 :step70
 ECHO INPUT FILE "%tmpfile%" FOUND, USING IT ... 
 :step85
ECHO.
ECHO CHECK THE CORRECTNESS OF ARGUMENTS TO CONTINUE WITH PROCESSING
ECHO (CTRL+C TO ABORT OR CHANGE INPUT FILE "%tmpfile%" WITH TXT EDITOR)...
PAUSE
echo *** PROGRAM %EXE% STARTS...
REM pause & goto :eof
REM %exe% < %tmpfile% > %exe%_%proj%.out
%exe%<%tmpfile%>%proj%_gradj3-process.log
echo SORT %proj%.ties...
sort<%proj%.ties>%proj%.ties.sort
REM PAUSE
REM del %tmpfile%
REM goto :eof
del %proj%.ties
7z a -tzip %proj%.cov.zip %proj%.cov
del %proj%.cov
7z a -tzip resid4hist.dat.zip resid4hist.dat
del resid4hist.dat
