@echo off
set exe=CG5form2f
ECHO SCRIPT FILE TO CONVERT SCINTREX CG-5 OBS FILE (*.txt) TO GRAVS2 FORMAT FILE
ECHO BY USING PROGRAM %exe% (LAST MOD. 2021-03-29 by T.OJA)
IF NOT "%1"=="-h" GOTO step25
ECHO.
echo USAGE: "%~nx0  <CG5 txt file name *.txt>"
echo  AFTER 1ST RUN DUMMY ADD.INF FILE IS MADE. WHEN NECESSARY, FILL *.INF FILE WITH DATA,  
echo  RENAME IT AND ATTACH IT BY TYPING: "%~nx0 <*.txt> <*.inf>"
echo.
echo   Program %exe% works only as intended if the station Designation System option of CG5 is "XYm"
echo   (CG5 Setup Menu -^> Survey -^> PARAMS(F1) -^> Station Designation System: XYm)
goto :EOF
:step25
ECHO.
:: MAIN BODY
set tmpin=%exe%.inp
echo MAKING INPUT FILE %tmpin%...
ECHO  1) Default values introduced for gravimeter's height,
ECHO     the benchmark's height and air pressure:
REM echo 211 0 1000.0 >%tmpin%
ECHO 300 0 -999.9 >%tmpin%
ECHO  2) Max time difference between readings (in hours):
ECHO  (if over max time then new session with new header)
echo 8 >>%tmpin%
ECHO  3) Input txt file:
echo %1>>%tmpin%
ECHO  4) Input inf file (if no filename or "ECHO is off." msg
ECHO     by DOS found then dummy file "add.inf" is made):
echo %2>>%tmpin%
echo.
ECHO CHECK THE CORRECTNESS OF ARGUMENTS TO CONTINUE WITH PROCESSING
ECHO (CTRL+C TO ABORT OR CHANGE INPUT FILE "%tmpin%" WITH TXT EDITOR)...
type %tmpin% & pause
%exe%<%tmpin%>%exe%.log
REM pause
REM del %tmpin%
