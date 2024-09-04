@echo off
:: Define absolute or relative path. Path names with spaces should be avoided.
:: Absolute path
set gr2main=d:\GRAVS2
REM set gr2main=c:\gravs2
:: Relative path
REM SET gr2main=..\GRAVS2
:: Script starts...
TITLE GRAVS2 command window
ECHO.
ECHO Setup of GRAVS2 with paths for exe and parameter files
ECHO NB! Commands for quick help:
ECHO reduce -h
ECHO adjust -h
ECHO evalvgg -h
ECHO cg5form -h
ECHO.
SET PATH=%PATH%;%gr2main%\bin;%gr2main%\share
ECHO GRAVS2 PATH SET TO: %gr2main%
ECHO.
:: User can give its own path of data and for processing
SET workpath=%1
IF "%workpath%"=="" (
 ECHO NO WORKPATH SET BY USER ^(to set workpath, use: GRAVS2 ^<working path^>^, eg gravs2 d:\data\2020\Profile23^)
 ) ELSE (
 ECHO WORKPATH SET BY USER: %workpath% 
 )
REM IF "%workpath%"=="" set workpath=d:\GRAVS2\data\
:: Open cmd with paths specified
REM %windir%\system32\cmd.exe /k cd "%workpath%"
cmd /k pushd "%workpath%"
REM not working: START /WAIT "GRAVS2" /D "%workpath%"
REM START "GRAVS2" CMD /k pushd "%workpath%" START "GRAVS2 command window" CMD /k