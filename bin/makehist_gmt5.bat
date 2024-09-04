@echo off
echo MS-DOS script file %~nx0
echo Program for histogram drawing, usage: HIST param1 param2 etc...
echo NB! If no parameters will be inserted, default values are used.
echo But you can change parameters with that script too (see below)!
echo If residuals are not found from 1st column then use gawk script below! 
echo NB!!Script needs GMT (gmt.soest.hawaii.edu) installation (pshistogram) with GAWK!
REM Normal dist graph (or some other dist) would be included - done with hist4
REM axes scales and units should choose autmatically in hist graph
REM should ask also no of column(with residuals) and scale 
echo.

REM gmtset X_ORIGIN 1.4c Y_ORIGIN 2.9c
gmtset ANNOT_FONT_SIZE_PRIMARY 18

set lkys=T
set kys1=n
set kys2=n
set kys3=n

IF "%1"=="" goto step03
set lkys=F
set maxp=%1
set lim=%2
set binw=%3
set filein=%4
set units=%5
set fileout=%6
set sc=%7
goto step05

:step03
set parfile=hist5.param
IF NOT EXIST %parfile% goto step05
echo Found %0 parameter file %parfile% - parameter values taken from it.
echo You can change it in txt file or delete it if you want diff values!
type %parfile%>ajut.bat
call ajut.bat
del ajut.bat

:step05
IF "%maxp%"=="" set maxp=100
IF "%lim%"=="" set lim=70
IF "%binw%"=="" set binw=2.5
IF "%filein%"=="" set filein=resid
IF "%units%"=="" set units=uGal
IF "%fileout%"=="" set fileout=histogram
IF "%sc%"=="" set sc=100

echo Param1: Max. percentage of the figure (Y axis):	%maxp%
echo Param2: Max. limit of X axis: 			%lim%
echo Param3: Bin width used for histogram: 		%binw%
echo Param4: Input file: 				%filein%
echo Param5: Unit of x: 				%units%
echo Param6: Output file: 				%fileout%
echo Param7: scale of theor.normal dist.curve: 	%sc%

if "%lkys%"=="T" SET /p kys1="Do you want to change parameters? (y/n)? (default n):"
if %kys1%==n goto step20

echo NB! Press only enter if you want to keep value
SET /p maxp="1)maxp="
SET /p lim="2)lim="
SET /p binw="3)binw="
SET /p filein="4)filein="
SET /p units="5)units="
SET /p fileout="6)fileout="
SET /p sc="7)sc="

echo.
echo New parameters' values:
echo Param1: Max. percentage of the figure (Y axis):	%maxp%
echo Param2: Max. limit of X axis: 			%lim%
echo Param3: Bin width used for histogram: 		%binw%
echo Param4: Input file: 				%filein%
echo Param5: Unit of x: 				%units%
echo Param6: Output file: 				%fileout%
echo Param7: Scale of n.curve: 			%sc%

echo.
echo Parameter values are inserted also to %parfile%...
REM Sometimes problem here with writing correctly num data, use then x.x
echo set maxp=%maxp%>%parfile%
echo set lim=%lim%>>%parfile%
echo set binw=%binw%>>%parfile%
echo set filein=%filein%>>%parfile%
echo set units=%units%>>%parfile%
echo set fileout=%fileout%>>%parfile%
echo set sc=%sc%>>%parfile%

:step20

echo.
echo File %filein% with residual data are processed...
REM If residuals are not found from 1st column then change column no (1,2,...) after dollar sign here
REM 
gawk "{print $1}"<%filein%>tempf
REM gawk "{if ($1!~/>/) print $9}"<%filein%>tempf

gawk "END {print NR}"<tempf>tempr
set /p ino=<"tempr"
echo ino=%ino%

REM calculates annotation interval for x-axis
gawk "BEGIN{print %binw%*4}">tempr
set /p interv=<"tempr"
echo interv=%interv%

if "%lkys%"=="T" SET /p kys2="Do you want to plot PDF curve of normal distribution? (y/n)? (default %kys2%):"
if %kys2%==n goto step30
set lisa=-K
echo %lisa%

REM calculates stdev
gawk "BEGIN{x=0;n=0};{x+=$1**2;n++};END{print sqrt(x/(n-1))}"<tempf>tempr
set /p stdev=<"tempr"
echo stdev=%stdev%

:step30
pshistogram tempf -R-%lim%/%lim%/0/%maxp% -Ba%interv%f%binw%:"Residual (%units%)":/a10f5:"Frequency"::,%%::."Histogram of residuals (i=%ino%)":WSne -JX15c/15c -F -G200 -L0.5p -W%binw% -N0+p1p,darkblue -N1+p1p,darkgreen -Z1 -Y2.8c -X3.4c %lisa% > %fileout%.ps
:: GMT5 version has option -N(0,1,2) to draw the equivalent normal distribution, eg -N1+p6p,lightred
:: CHECKED: -N0 (mean and standard deviation [Default]) gives same curve as normalPDF
:: -N1 gives median and L1 scale
if %kys2%==n goto step40

echo 0 %stdev% %lim% %binw% >tempr
normalPDF<tempr>tempf
REM set sc=200.0
REM set sc=1525.0
if %sc% neq 100.0 echo PDF curve is scaled by sc=%sc% !!!
gawk "{$2=$2*%sc%; print $0}" < tempf|psxy -R -JX -W1p,darkred -O -t40 >> %fileout%.ps
ECHO Mean, stdev^= 0 %stdev%> normalpdf_values
:step40

if "%lkys%"=="T" SET /p kys3="Do you wish to get *.png file? (y/n)? (default n):"
REM 
if %kys3%==y ps2raster %fileout%.ps -A -P -Tg

REM pause
del gmt.* tempf tempr
