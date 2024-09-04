@echo off
rem set coordfile=coord.txt
REM 
set coordfile=%1

REM set ajutfile=%~n1.gtp
REM gawk "{print $1,$2,$3,$4,$5}"<%1>%ajutfile%

set tmpfile=in
rem *** Input parameters
REM echo D:\data\WPARM.DAT >%tmpfile%
echo WPARM.DAT >%tmpfile%
echo %coordfile% >>%tmpfile%

rem *** Program work
GTPAR <%tmpfile% >%coordfile%.gtpar.out

pause
del %tmpfile%
