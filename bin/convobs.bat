@ECHO OFF
SET exe=tform2x276.exe

ECHO This script converts obs files from old GRREDU2.X (0<=X<6) format
ECHO to GRREDU2.76 or newer format by using program %exe%
ECHO.

ECHO INPUT OBS FILE: %1

ECHO TEMP FILE...
SET tmpfile=in
ECHO %tmpfile%
ECHO %1 > %tmpfile%

ECHO *** Program %exe% starts...
%exe% < %tmpfile% > %12

REM pause
DEL %tmpfile%
