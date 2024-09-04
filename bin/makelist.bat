@echo off
echo Script for making list of filenames *.%1 in current folder. Modify script to add subfolders!
echo "dir /s /b /O:N *.%1 >> %1list"

:: To list files in current folder only
REM dir /b /O:N *.%1 >> %1list  --it gives only filenames
REM 
for /f "tokens=*" %%A in ('dir /b /O:N *.%1') do @echo %%~dpnxA>>%1list

:: To list also files from subfolders
REM dir /s /b /O:N *.%1 >> %1list

echo DONE!
REM pause
