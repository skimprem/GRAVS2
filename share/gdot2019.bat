@ECHO OFF
echo SCRIPT INTERPOLATES GRAVITY CHANGE (gdot) VALUES (uGal/yr) FROM SELECTED MODEL
echo SYNOPSIS: %~nx0 filename (name of input coordinate file with format: ID, name, lat, lon, H)
echo.
ECHO The open source GMT (Generic Mapping Tools,gmt.soest.hawaii.edu)
ECHO MUST be INSTALLED to get it work (check that GAWK is included)!

echo.
SET kys=y
SET /p kys="Have you installed GMT, do you want to continue (y/n)? (default y):"
if %kys%==n goto LOPP2
if %kys%==N goto LOPP2
IF %kys%==y goto SAMM2
IF NOT %kys%==Y goto VIGA1
:SAMM2
echo.
REM SET vork=D:\MuDokud\Geodynamics\PGR\NKG2005LU\grid\NKG2005LU.gmt_gdot.grd
SET vork=NKG2016LU_gdot0163.nc
ECHO THE USED MODEL FOR INTERPOLATION: %vork%

gawk "{print $4,$3}"<%1|grdtrack -G%vork% -Z|gawk "{printf(\"%%.2f\n\", $0)}" > _ajut_intp
:: REMOVE EMPTY LINES
gawk NF <%1>_ajut
:: MERGE 2 FILES BY COLUMNS
gawk "{a=$1\"\t\"$2\"\t\"$3\"\t\"$4\"\t\"$5; getline b <\"_ajut_intp\"; $0=a\"\t\"b; print $0,3086,0}"<_ajut>%~n1_gdot%~x1

goto LOPP1
:VIGA1
ECHO ANSWER: %kys%
echo ERROR: TRY AGAIN!
:LOPP1
PAUSE
del .gmt* _ajut_intp _ajut
:LOPP2
