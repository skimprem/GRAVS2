@echo off
ECHO Quick test of GRAVS2 binaries

SET _gr2main=..\..\GRAVS2
SET PATH=%PATH%;%_gr2main%\bin

CG5form2f<CG5form2f.inp>CG5form2f.log

grredu303<grredu3.inp>grredu3.log

gradj305<GoF2010_S36_gradj3.inp>GoF2010_S36_gradj3.log

PAUSE
