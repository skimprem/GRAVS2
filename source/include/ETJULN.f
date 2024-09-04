      SUBROUTINE ETJULN(IYEAR,IMONTH,IDAY,IHOUR,IMIN,ISEK,DMJD)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C     Routine ETJULN, version 940715 FORTRAN 77.                       C
C                                                                      C
C     The routine ETJULN computes the modified                         C
C     Julian date. ETJULN is a modified version of routine MJD given   C
C     in PASCAL by MONTENBRUCK and PFLEGER.                            C
C                                                                      C
C     The routine is valid for every date since year -4713.            C
C     Comparison with reference values between years -1410 and +3200   C
C     from JPL was successfully.                                       C 
C                                                                      C
C     Reference: MONTENBRUCK, O. and T. PFLEGER 1989: Astronomie mit   C
C                dem Personal Computer. Springer Verlag, Berlin 1989.  C
C                                                                      C
C     Input parameter description:                                     C
C     ----------------------------                                     C
C                                                                      C
C     IYEAR:       year   (INTEGER).                                   C
C     IMONTH:      month  (INTEGER).                                   C
C     IDAY:        day    (INTEGER).                                   C
C     IHOUR:       hour   (INTEGER).                                   C
C     IMIN:        minute (INTEGER).                                   C
C     ISEK:        second (INTEGER).                                   C
C                                                                      C
C     Output parameter description:                                    C
C     -----------------------------                                    C
C     DMJD:        Modified Julian date (DOUBLE PRECISION). To obtain  C
C                  the true Julian date DJULD, add 2400000.5 to DMJD.  C
C     DJULD:       Julian date (DOUBLE PRECISION).                     C
C                  16. April   -1410, 0.00 H is DJULD = 1206160.5D0    C
C                  31. January -1100, 0.00 H is DJULD = 1319312.5D0    C
C                  24. January -0800, 0.00 H is DJULD = 1428880.5D0    C
C                  17. January -0500, 0.00 H is DJULD = 1538448.5D0    C
C                  10. January -0200, 0.00 H is DJULD = 1648016.5D0    C
C                  03. January   100, 0.00 H is DJULD = 1757584.5D0    C
C                  29. February  400, 0.00 H is DJULD = 1867216.5D0    C
C                  20. December  699, 0.00 H is DJULD = 1976720.5D0    C
C                  15. February 1000, 0.00 H is DJULD = 2086352.5D0    C
C                  08. February 1300, 0.00 H is DJULD = 2195920.5D0    C
C                  11. February 1600, 0.00 H is DJULD = 2305488.5D0    C
C                  06. February 1900, 0.00 H is DJULD = 2415056.5D0    C
C                  01. January  1988, 0.00 H is DJULD = 2447161.5D0    C
C                  01. February 1988, 0.00 H is DJULD = 2447192.5D0    C
C                  29. February 1988, 0.00 H is DJULD = 2447220.5D0    C
C                  01. March    1988, 0.00 H is DJULD = 2447221.5D0    C
C                  01. February 2200, 0.00 H is DJULD = 2524624.5D0    C
C                  27. January  2500, 0.00 H is DJULD = 2634192.5D0    C
C                  23. January  2800, 0.00 H is DJULD = 2743760.5D0    C
C                  22. December 3002, 0.00 H is DJULD = 2817872.5D0    C
C                                                                      C
C     USED ROUTINES...NONE.                                            C
C     ================                                                 C
C                                                                      C
C     Routine creation:  920919 by H.-G. Wenzel,                       C
C                        Geodaetisches Institut,                       C
C                        Universitaet Karlsruhe,                       C
C                        Englerstr. 7,                                 C
C                        D-76128 KARLSRUHE,                            C
C                        Germany.                                      C
C                        Tel.: 0721-6082301.                           C
C                        FAX:  0721-694552.                            C
C                        e-mail: wenzel@gik.bau-verm.uni-karlsruhe.de  C
C     Last modification: 940715 by H.-G. Wenzel.                       C
C                        2005/04/18 by T. Oja                          C
C**********************************************************************C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      ITY=IYEAR
      IF (IMONTH.le.0.or.IMONTH.gt.12) GOTO 5000
      IF (IDAY.le.0.or.IDAY.gt.31) GOTO 5000
      IF (IHOUR.lt.0.or.IHOUR.ge.24) GOTO 5000
      IF (IMIN.lt.0.or.IMIN.ge.60.or.ISEK.lt.0.or.ISEK.ge.60) GOTO 5000
      ITM=IMONTH
      ITD=IDAY
      A=10000.0D0*ITY+100.0D0*ITM+ITD
      IF(IMONTH.LE.2) THEN
         ITM=ITM+12
         ITY=ITY-1
      END IF
      IF(A.LE.15821004.1D0) THEN
         B=-2+(ITY+4716)/4-1179
      ELSE
         B=ITY/400-ITY/100+ITY/4
      ENDIF
      A=365.0D0*ITY-679004.0D0
      DMJD=A+B+INT(30.6001D0*(ITM+1))+ITD+IHOUR/24.D0+IMIN/1440.D0+
     .ISEK/86400.D0
C      DJULD=DMJD+2400000.5D0
      RETURN
 5000 WRITE(*,7050) IYEAR,IMONTH,IDAY,IHOUR,IMIN,ISEK
      STOP
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Format statements:                                               C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 7050 FORMAT(/' *****Error in routine ETJULN, version 940715 FTN77.'/
     1' *** Number of Months or days are wrong:',2X,I4,2('/',I2),', ',
     2I2,2(':',I2)/
     3' *****Routine ETJULN stops the execution.'/)
      END
