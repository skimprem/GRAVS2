C      SUBROUTINE ETMUTC(IUN6,IPRINT,DTUJD,DDT)
      SUBROUTINE ETMUTC(IPRINT,DTUJD,DDT)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C     Routine ETMUTC, version 930712 FORTRAN 77.                       C
C                                                                      C
C     All variables with D as first character are DOUBLE PRECISION.    C
C                                                                      C
C     Input parameter description:                                     C
C     ----------------------------                                     C
C                                                                      C
C     IUN6...      formatted printer unit.                             C
C     IPRINT...    printout parameter. For IPRINT=0, nothing will be   C
C                  written on unit IUN6. For IPRINT=2, the tables DTX, C
C                  DTJULD and DTY will be printed for the first call   C
C                  of routine ETMUTC.                                  C
C     DTUJD...     Julian date of epoch (Universal time).              C
C                                                                      C
C     Output parameter description:                                    C
C     -----------------------------                                    C
C                                                                      C
C     DDT...       difference ET - UTC   resp. TDT - UTC in seconds    C
C                  from 1955.5 until 2019.5 . For epochs less 1955.5,  C
C                  DDT is set to 31.59 s. For epochs exceeding 2010.0, C
C                  DDT is set to 66.184 s. ET  is Ephemeris Time.      C
C                  TDT is Terrestrial Dynamical Time.                  C
C                  UTC is Universal Time Coordinated, as bradcasted by C
C                  radio or GPS satellites.                            C
C                                                                      C
C     The table  DTAB has to be extended, when new data are available. C
C     Change parameter NTAB and DIMENSIONS !!!                         C
C                                                                      C
C     Routine creation:  W. Zuern, BFO Schiltach, Heubach 206,         C
C                        D-7620 WOLFACH (in program RIGTID).           C
C     Last modification: 930712 by H.-G. Wenzel.                       C
C                        Geodaetisches Institut,                       C
C                        Universitaet Karlsruhe,                       C
C                        Englerstr. 7,                                 C
C                        D-76128 KARLSRUHE 1,                          C
C                        Germany.                                      C
C                        Tel.: 0721-6082307,                           C
C                        FAX:  0721-694552.                            C
C                        e-mail: wenzel@gik.bau-verm.uni-karlsruhe.de  C
C                                                                      C
C                        Modif. 2019-05-16 by T. Oja (ELB)             C
C**********************************************************************C
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT INTEGER*4 (I-N)
      DOUBLE PRECISION DTAB(3,99)
      DOUBLE PRECISION DTAB1(3,20),DTAB2(3,20),DTAB3(3,20),DTAB4(3,20),
     1 DTAB5(3,19)
      EQUIVALENCE (DTAB(1,1), DTAB1(1,1)),(DTAB(1,21),DTAB2(1,1)),
     1            (DTAB(1,41),DTAB3(1,1)),(DTAB(1,61),DTAB4(1,1)),
     2            (DTAB(1,81),DTAB5(1,1))
      SAVE NTAB,IWARN,ITAB,DTAB
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     DTAB(1,I) is the year (in decimal form),                         C
C     DTAB(2,I) is the JULIAN date (in days), and                      C
C     DTAB(3,I) is the difference ET minus UTC, or TDT minus UTC       C
C     in sec, as taken from the Astronomical Ephemeris or the Bulletin C
C     of the International Earth Rotation Servive.                     C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DATA DTAB1/ 1955.50000D0,   2435289.50000D0,   31.590D0,
     1            1956.50000D0,   2435655.50000D0,   32.060D0,
     2            1957.50000D0,   2436020.50000D0,   31.820D0,
     3            1958.50000D0,   2436385.50000D0,   32.690D0,
     4            1959.50000D0,   2436750.50000D0,   33.050D0,
     5            1960.50000D0,   2437116.50000D0,   33.160D0,
     6            1961.50000D0,   2437481.50000D0,   33.590D0,
     7            1962.00000D0,   2437665.50000D0,   34.032D0,
     8            1962.50000D0,   2437846.50000D0,   34.235D0,
     9            1963.00000D0,   2438030.50000D0,   34.441D0,
     *            1963.50000D0,   2438211.50000D0,   34.644D0,
     1            1964.00000D0,   2438395.50000D0,   34.950D0,
     2            1964.50000D0,   2438577.50000D0,   35.286D0,
     3            1965.00000D0,   2438761.50000D0,   35.725D0,
     4            1965.50000D0,   2438942.50000D0,   36.160D0,
     5            1966.00000D0,   2439126.50000D0,   36.498D0,
     6            1966.50000D0,   2439307.50000D0,   36.968D0,
     7            1967.00000D0,   2439491.50000D0,   37.444D0,
     8            1967.50000D0,   2439672.50000D0,   37.913D0,
     9            1968.00000D0,   2439856.50000D0,   38.390D0/
      DATA DTAB2/ 1968.25000D0,   2439947.50000D0,   38.526D0,
     1            1968.50000D0,   2440038.50000D0,   38.760D0,
     2            1968.75000D0,   2440130.50000D0,   39.000D0,
     3            1969.00000D0,   2440222.50000D0,   39.238D0,
     4            1969.25000D0,   2440312.50000D0,   39.472D0,
     5            1969.50000D0,   2440403.50000D0,   39.707D0,
     6            1969.75000D0,   2440495.50000D0,   39.946D0,
     7            1970.00000D0,   2440587.50000D0,   40.185D0,
     8            1970.25000D0,   2440677.50000D0,   40.420D0,
     9            1970.50000D0,   2440768.50000D0,   40.654D0,
     *            1970.75000D0,   2440860.50000D0,   40.892D0,
     1            1971.00000D0,   2440952.50000D0,   41.131D0,
     2            1971.08500D0,   2440983.50000D0,   41.211D0,
     3            1971.16200D0,   2441011.50000D0,   41.284D0,
     4            1971.24700D0,   2441042.50000D0,   41.364D0,
     5            1971.32900D0,   2441072.50000D0,   41.442D0,
     6            1971.41400D0,   2441103.50000D0,   41.522D0,
     7            1971.49600D0,   2441133.50000D0,   41.600D0,
     8            1971.58100D0,   2441164.50000D0,   41.680D0,
     9            1971.66600D0,   2441195.50000D0,   41.761D0/
      DATA DTAB3/ 1971.74800D0,   2441225.50000D0,   41.838D0,
     1            1971.83300D0,   2441256.50000D0,   41.919D0,
     2            1971.91500D0,   2441286.50000D0,   41.996D0,
     3            1971.99999D0,   2441317.49999D0,   42.184D0,
     4            1972.00000D0,   2441317.50000D0,   42.184D0,
     5            1972.49999D0,   2441499.49999D0,   42.184D0,
     6            1972.50000D0,   2441499.50000D0,   43.184D0,
     7            1972.99999D0,   2441683.49999D0,   43.184D0,
     8            1973.00000D0,   2441683.50000D0,   44.184D0,
     9            1973.99999D0,   2442048.49999D0,   44.184D0,
     *            1974.00000D0,   2442048.50000D0,   45.184D0,
     1            1974.99999D0,   2442413.49999D0,   45.184D0,
     2            1975.00000D0,   2442413.50000D0,   46.184D0,
     3            1975.99999D0,   2442778.49999D0,   46.184D0,
     4            1976.00000D0,   2442778.50000D0,   47.184D0,
     5            1976.99999D0,   2443144.49999D0,   47.184D0,
     6            1977.00000D0,   2443144.50000D0,   48.184D0,
     7            1977.99999D0,   2443509.49999D0,   48.184D0,
     8            1978.00000D0,   2443509.50000D0,   49.184D0,
     9            1978.99999D0,   2443874.49999D0,   49.184D0/
      DATA DTAB4/ 1979.00000D0,   2443874.50000D0,   50.184D0,
     1            1979.99999D0,   2444239.49999D0,   50.184D0,
     2            1980.00000D0,   2444239.50000D0,   51.184D0,
     3            1981.49999D0,   2444786.49999D0,   51.184D0,
     4            1981.50000D0,   2444786.50000D0,   52.184D0,
     5            1982.49999D0,   2445151.49999D0,   52.184D0,
     6            1982.50000D0,   2445151.50000D0,   53.184D0,
     7            1983.49999D0,   2445516.49999D0,   53.184D0,
     8            1983.50000D0,   2445516.50000D0,   54.184D0,
     9            1985.49999D0,   2446247.49999D0,   54.184D0,
     *            1985.50000D0,   2446247.50000D0,   55.184D0,
     1            1987.99999D0,   2447161.49999D0,   55.184D0,
     2            1988.00000D0,   2447161.50000D0,   56.184D0,
     3            1989.99999D0,   2447892.49999D0,   56.184D0,
     4            1990.00000D0,   2447892.50000D0,   57.184D0,
     5            1990.99999D0,   2448257.49999D0,   57.184D0,
     6            1991.00000D0,   2448257.50000D0,   58.184D0,
     7            1992.49999D0,   2448804.49999D0,   58.184D0,
     8            1992.50000D0,   2448804.50000D0,   59.184D0,
     9            1993.49999D0,   2449169.49999D0,   59.184D0/
      DATA DTAB5/ 1993.50000D0,   2449169.500000D0,   60.184D0,
     1            1994.49999D0,   2449534.499999D0,   60.184D0,
     2            1994.50000D0,   2449534.500000D0,   61.184D0,
     3            1995.99999D0,   2450083.499999D0,   61.184D0,
     4            1996.00000D0,   2450083.500000D0,   62.184D0,
     5            1997.49999D0,   2450630.499999D0,   62.184D0,
     6            1997.50000D0,   2450630.500000D0,   63.184D0,
     7            1998.99999D0,   2451179.499999D0,   63.184D0,
     8            1999.00000D0,   2451179.500000D0,   64.184D0,
     9            2005.99999D0,   2453736.499999D0,   64.184D0,
     *            2006.00000D0,   2453736.500000D0,   65.184D0,
     1            2008.99999D0,   2454832.499999D0,   65.184D0,
     2            2009.00000D0,   2454832.500000D0,   66.184D0,
     3            2012.49999D0,   2456109.499999D0,   66.184D0,
     4            2012.50000D0,   2456109.500000D0,   67.184D0,
     5            2015.49999D0,   2457204.499999D0,   67.184D0,
     6            2015.50000D0,   2457204.500000D0,   68.184D0,
     7            2015.99999D0,   2457388.499999D0,   68.184D0,
     8            2016.00000D0,   2457388.500000D0,   69.184D0/
      DATA NTAB/99/,IWARN/1/,ITAB/1/
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Print table for the first call of ETMUTC.                        C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IF(IPRINT.NE.2) GOTO 10
      IF(ITAB.EQ.0) GOTO 10
C      WRITE(IUN6,7001)
      IF(IPRINT.GT.0) then
       WRITE(*,7001)
       DO 1 I=1,NTAB
C       WRITE(IUN6,7002) I,DTAB(1,I),DTAB(2,I),DTAB(3,I)
       WRITE(*,7002) I,DTAB(1,I),DTAB(2,I),DTAB(3,I)
    1  CONTINUE
      endif
      ITAB=0
   10 CONTINUE
      IF(DTUJD.LT.DTAB(2,NTAB)) GOTO 11
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     DTUJD exceeds last tabulated epoch DTAB(NTAB,3).                 C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
   12 DDT=DTAB(3,NTAB)
C      IF(IWARN.EQ.1) WRITE(IUN6,7051) DTAB(1,NTAB)
CCC As last value is from 1999.0 then no warnings (04.2004)
C      IF(IWARN.EQ.1) WRITE(*,7051)    DTAB(1,NTAB)
      IWARN=0
      IF(IPRINT.GT.0) WRITE(*,7055) DTUJD,DDT
      RETURN
   11 DO  20  I=1,NTAB
      IF(DTUJD-DTAB(2,I))   21,22,20
   22 DDT=DTAB(3,I)
      IF(IPRINT.GT.0) WRITE(*,7055) DTUJD,DDT
      RETURN
   21 N=I-1
      GOTO  23
   20 CONTINUE
   23 DDT=(DTAB(3,N+1)*(DTUJD-DTAB(2,N))-DTAB(3,N)*(DTUJD-DTAB(2,N+1)))/
     1 (DTAB(2,N+1)-DTAB(2,N))
      IF(IPRINT.GT.0) WRITE(*,7055) DTUJD,DDT
      RETURN
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Format statements:                                               C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 7001 FORMAT(//' Routine ETMUTC, version 930712 FTN 77.'//
     1' List of tables:'//
     2'       No.           Juld            DTX       DTY'//)
 7002 FORMAT(I10,2F15.5,F10.3)
 7051 FORMAT(/' ***** Warning from routine ETMUTC, version 930712.'/
     1' ***** Epoch exceeds the last tabulated value:',F10.5/
     2' ***** DDT of last tabulated epoch is used.'/
     3' ***** Please try to update tabels in routine ETMUC.'/)
 7055 FORMAT(/'*** Routine ETMUTC, version 930712.'/
     1'*** For epoch',f14.5,' TDT - UTC in seconds:',f7.3)
      END
