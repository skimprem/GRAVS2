CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      SUBROUTINE ETASTE(IUN6,IPRINT,IMODEL,DLON,ITY,ITM,ITD,DTH,DAS,
C     1 DASP,DDT0)
      SUBROUTINE ETASTE(IPRINT,IMODEL,DLON,dmjul,DTH,DAS,DASP,DDT0)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C     Routine ETASTE, version 930531 FORTRAN 77.                       C
C                                                                      C
C     The routine ETASTE computes the astronomical elements for        C
C     a specific epoch, given in UTC. This routine compiles under      C
C     operation system UNIX using the SUN-FORTRAN compiler and under   C
C     operation system MS-DOS using the MS 5.0 FORTRAN compiler.       C
C                                                                      C
C     Reference: TAMURA, Y. 1987: A Harmonic Development of the Tide-  C
C                generating Potential. Bulletin d'Informations Marees  C
C                Terrestres no. 99, 6813-6855, Bruxelles 1987.         C
C                                                                      C
C                WENZEL, H.-G. 1976: Zur Genauigkeit von gravimetri-   C
C                schen Erdgezeitenbeobachtunngen. Wissenschaftliche    C
C                Arbeiten der Lehrtsuehle fuer Geodaesie, Photogramme- C
C                trie und Kartographie an der Technischen Universitaet C
C                Hannover NR. 67, Hannover 1976.                       C
C                                                                      C
C     All variables with D as first character are double precision.    C
C                                                                      C
C     Input parameter description:                                     C
C     ----------------------------                                     C
C                                                                      C
C     IUN6...      unit number of formatted printout file.             C
C     IPRINT...    printout parameter. For IPRINT=0, nothing will      C
C                  be printed on unit IUN6.                            C
C     IMODEL...    parameter for the used tidal potential model.       C
C                  IMODEL = 0: DOODSON 1921     tidal potential.       C
C                  IMODEL = 1: CTED 1973        tidal potential.       C
C                  IMODEL = 2: TAMURA 1987      tidal potential.       C
C                  IMODEL = 3: BUELLESFELD 1985 tidal potential.       C
C     DLON...      ellipsoidal longitude of the station referring to   C
C                  geodetic reference system GRS80 in degree,          C
C                  positive east of Greenwhich.                        C
C     ITY...       integer year  of the epoch (e.g. 1988).             C
C     ITM...       integer month of the epoch (e.g. January = 1).      C
C     ITD...       integer day   of the epoch.                         C
C     DTH...       hour          of the epoch in UTC.                  C
C                                                                      C
C     Output parameter description:                                    C
C     -----------------------------                                    C
C                                                                      C
C     DAS(1)...    mean local Moontime in degree.                      C
C     DAS(2)...    mean longitude of the Moon in degree.               C
C     DAS(3)...    mean longitude of the Sun  in degree.               C
C     DAS(4)...    mean longitude of the perigee of the Moon's orbit   C
C                  in degree.                                          C
C     DAS(5)...    neagtive mean longitude of the ascending node of    C
C                  the Moon's orbit in degree.                         C
C     DAS(6)...    mean longitude of the perigee of the Suns's orbit   C
C                  in degree.                                          C
C     DAS(7)...    argument of Jupiter's opposition in degree (for     C
C                  TAMURA's 1987 tidal potential development).         C
C     DAS(8)...    argument of Venus's conjunction in degree (for      C
C                  TAMURA's 1987 tidal potential development).         C
C     DASP(1...8): time derivatives of the corresponding variables     C
C                  DAS in degree per hour.                             C
C     DDT0...      difference TDT minus UTC at initial epoch in sec.   C
C                                                                      C
C     Used routines:                                                   C
C     --------------                                                   C
C                                                                      C
C     ETEXTI: computes jobtime.                                        C
C     ETJULD: computes Julian date.                                    C
C                                                                      C
C     Numerical accuracy:                                              C
C     -------------------                                              C
C                                                                      C
C     The routine has been tested on IBM Pc with 15 digits accuracy    C
C     in double precision using different compilers.                   C
C                                                                      C
C     Execution time:                                                  C
C     ---------------                                                  C
C                                                                      C
C     The execution time is about 0.00017 s per call of routine ETASTE C
C     on ICM Pc 80486 DX2 with 66 MHz speed.                           C
C                                                                      C
C     Routine creation:  880130 BY H.-G.WENZEL.                        C
C                        Geodaetisches Institut,                       C
C                        Universitaet Karlsruhe,                       C
C                        Englerstr. 7,                                 C
C                        D-76128 KARLSRUHE 1,                          C
C                        Germany.                                      C
C                        Tel.: 0721-6082307,                           C
C                        FAX:  0721-694552.                            C
C                        e-mail: wenzel@gik.bau-verm.uni-karlsruhe.de  C
C     Last modification: 930531 by H.-G. Wenzel.                       C
C**********************************************************************C
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT INTEGER*4 (I-N)
      DOUBLE PRECISION DAS(8),DASP(8)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     COMMON /CONST/:                                                  C
C     DPI...        3.1415....                                         C
C     DPI2...       2.D0*DPI                                           C
C     DRAD...       DPI/180.D0                                         C
C     DRO...        180.D0/DPI                                         C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      COMMON /CONST/ DPI,DPI2,DRAD,DRO
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute Julian date for epoch ITY, ITM, ITD, DTH.                C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      CALL ETJULD(IUN6,ITY,ITM,ITD,DTH,DTUJD)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute Universal Time epoch DTUT in Julian centuries referring  C
C     to 1. January 1900.                                              C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      DTUT=(DTUJD-2415020.0D0)/36525.0D0
      DTUT=(dmjul-15019.5D0)/36525.0D0
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute Universal Time epoch DTUT20 in Julian Centuries          C
C     referring to 1. January 2000 12 h.                               C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      DTUT20=(DTUJD-2451545.0D0)/36525.0D0
      DTUT20=(dmjul-51544.5D0)/36525.0D0
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute Terrestrial Dynamical Time epoch DT in Julian Centuries  C
C     referring to 1. January 1990 and DTDT20 referring to 1 January   C
C     2000 12 h.                                                       C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      CALL ETMUTC(IUN6,IPRINT,DTUJD,DDT0)
      DTUJD = dmjul + 2400000.5D0
      CALL ETMUTC(IPRINT,DTUJD,DDT0)
      DT=DTUT+DDT0/3155760000.0D0
      DTDT20=DTUT20+DDT0/3155760000.0D0
C      IF(IPRINT.GT.0) WRITE(IUN6,7001) ITY,ITM,ITD,DTH,DTUJD,DDT0
      IF(IPRINT.GT.0) WRITE(*,7001) DTUJD,DDT0
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute astronomical elements for initial epoch from             C
C     NEWCOMB's formula for the Sun and from BROWN's formulas for the  C
C     Moon (see Astronomical Ephemeris, Explanatory Supplement).       C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DT2=DT*DT
      DT3=DT2*DT
      IF(IMODEL.EQ.2) GOTO 100
      DAS(2)=270.434164D0 +481267.8831417D0*DT-0.0011333D0*DT2
     1 +0.0000019D0*DT3
      DAS(3)=279.696678D0 +36000.768925D0*DT +0.0003025D0*DT2
      DAS(4)=334.329556D0 +4069.0340333D0*DT -0.010325D0*DT2
     1 -0.0000125D0*DT3
      DAS(5)=100.816725D0 +1934.1420083D0*DT -0.0020778D0*DT2
     1 -0.0000022D0*DT3
      DAS(6)=281.220833D0 +1.719175D0*DT +0.0004528D0*DT2
     1 +0.0000033D0*DT3
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute Jupiter's and Venus's arguments from TAMURA's 1987       C
C     formulas.                                                        C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DAS(7)=248.1D0+32964.47D0*DTDT20
      DAS(8)= 81.5D0+22518.44D0*DTDT20
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute speeds in degree per hour:                               C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DASP(2)=0.54901652195037D0 -2.58575D-9*DT +6.46D-12*DT2
      DASP(3)=0.04106863897444D0 +6.902D-10*DT
      DASP(4)=0.00464183667960D0 -2.355692D-8*DT-4.278D-11*DT2
      DASP(5)=0.00220641342494D0 -4.74054D-9*DT -7.60D-12*DT2
      DASP(6)=0.00000196118526D0 +1.03303D-9*DT +1.141D-11*DT2
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute Jupiter's and Venus's speed from TAMURA's 1987           C
C     formulas.                                                        C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DASP(7)=32964.47D0/(24.D0*36525.D0)
      DASP(8)=22518.44D0/(24.D0*36525.D0)
      DASP(1)=DASP(3)-DASP(2)+15.0D0
      DO 10 I=2,8
      DAS(I)=DMOD(DAS(I),360.0D0)
      IF(DAS(I).LT.0.D0) DAS(I)=DAS(I)+360.D0
   10 CONTINUE
      DAS(1)=DAS(3)-DAS(2)+DLON+DTH*15.0D0
      IF(DAS(1).LT.0.D0) DAS(1)=DAS(1)+360.0D0
      GOTO 200
  100 CONTINUE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute astronomical elements from TAMURA's 1987 formulas:       C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DT2=DTUT20*DTUT20
      DT3=DT2*DTUT20
      DAL=280.4606184D0 + 36000.7700536D0*DTUT20 + 0.00038793D0*DT2
     1 -0.0000000258D0*DT3
      DALP=(36000.7700536D0 +2.0D0*0.00038793D0*DTUT20
     1 -3.0D0*0.0000000258D0*DT2)/(24.0D0*36525.D0)
      DT2=DTDT20**2
      DS=218.316656D0+481267.881342D0*DTDT20-0.001330D0*DT2
      DSP=(481267.881342D0-2.0D0*0.001330D0*DTDT20)/(24.D0*36525.0D0)
      DH=280.466449D0+36000.769822D0*DTDT20+0.0003036D0*DT2
      DHP=(36000.769822D0+2.0D0*0.0003036D0*DTDT20)/(24.D0*36525.0D0)
      DDS=0.0040D0*DCOS((29.D0+133.0D0*DTDT20)*DRAD)
      DDSP=(-0.0040D0*133.0D0*DRAD*DSIN((29.D0+133.0D0*DTDT20)*DRAD))/
     1 (24.0D0*36525.0D0)
      DDH=0.0018D0*DCOS((159.D0+19.D0*DTDT20)*DRAD)
      DDHP=(-0.0018D0*19.0D0*DRAD*DSIN((159.D0+19.D0*DTDT20)*DRAD))/
     1 (24.0D0*36525.0D0)
      DAS(1)=DAL-DS+DLON+DTH*15.0D0
      DAS(2)=DS+DDS
      DAS(3)=DH+DDH
      DAS(4)=83.353243D0 + 4069.013711D0*DTDT20 -0.010324D0*DT2
      DAS(5)=234.955444D0 +1934.136185D0*DTDT20 -0.002076D0*DT2
      DAS(6)=282.937348D0 + 1.719533D0*DTDT20 +0.0004597D0*DT2
      DAS(7)=248.1D0+32964.47D0*DTDT20
      DAS(8)= 81.5D0+22518.44D0*DTDT20
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute the speedsin degree per hour:                            C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DASP(1)=DALP-DSP+15.0D0
      DASP(2)=DSP+DDSP
      DASP(3)=DHP+DDHP
      DASP(4)=(4069.013711D0-2.0D0*0.010324D0*DTDT20)/(24.0D0*36525.0D0)
      DASP(5)=(1934.136185D0-2.0D0*0.002076D0*DTDT20)/(24.0D0*36525.0D0)
      DASP(6)=(1.719533D0+2.0D0*0.0004597D0*DTDT20)/(24.0D0*36525.0D0)
      DASP(7)=32964.47D0/(24.D0*36525.D0)
      DASP(8)=22518.44D0/(24.D0*36525.D0)
      DO 110 I=1,8
      DAS(I)=DMOD(DAS(I),360.0D0)
      IF(DAS(I).LT.0.D0) DAS(I)=DAS(I)+360.0D0
  110 CONTINUE
  200 CONTINUE
      IF(IPRINT.EQ.0) RETURN
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Print astronomical elements:                                     C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      IF(IMODEL.NE.2) WRITE(IUN6,7003) (DAS(K),DASP(K),K=1,8)
C      IF(IMODEL.EQ.2) WRITE(IUN6,7004) (DAS(K),DASP(K),K=1,8)
      IF(IMODEL.NE.2) WRITE(*,7003) (DAS(K),DASP(K),K=1,8)
      IF(IMODEL.EQ.2) WRITE(*,7004) (DAS(K),DASP(K),K=1,8)
 5000 CONTINUE
C      WRITE(IUN6,7030)
      WRITE(*,7030)
      RETURN
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Format statements:                                               C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 7001 FORMAT(' Routine ETASTE, version 930531 FTN 77.'/
     1' Astronomic elements for initial epoch '/,
C,I4,2I3,F3.0,' UTC'/
     2' Julian date                   : ',F15.4/,
     3' Correction to UTC to give TDT : ',F15.3,' s'/)
 7003 FORMAT(' BROWNs formulas for the Moon, NEWCOMBs formulas for the',
     1' Sun'/' TAMURAs formulas for Jupiter and Venus'//
     2' TAU',F20.11,' deg  TAU.',F20.11,' deg/hour'/
     3' S  ',F20.11,' deg  S.  ',F20.11,' deg/hour'/
     4' H  ',F20.11,' deg  H.  ',F20.11,' deg/hour'/
     5' P  ',F20.11,' deg  P.  ',F20.11,' deg/hour'/
     6' N  ',F20.11,' deg  N.  ',F20.11,' deg/hour'/
     7' P1 ',F20.11,' deg  P1. ',F20.11,' deg/hour'/
     8' JU ',F20.11,' deg  JU. ',F20.11,' deg/hour'/
     9' VE ',F20.11,' deg  VE. ',F20.11,' deg/hour'/)
 7004 FORMAT(' TAMURAs 1987 formulas are used.'//
     1' F1 ',F20.11,' deg  F1. ',F20.11,' deg/hour'/
     2' F2 ',F20.11,' deg  F2. ',F20.11,' deg/hour'/
     3' F3 ',F20.11,' deg  F3. ',F20.11,' deg/hour'/
     4' F4 ',F20.11,' deg  F4. ',F20.11,' deg/hour'/
     5' F5 ',F20.11,' deg  F5. ',F20.11,' deg/hour'/
     6' F6 ',F20.11,' deg  F6. ',F20.11,' deg/hour'/
     7' F7 ',F20.11,' deg  F7. ',F20.11,' deg/hour'/
     8' F8 ',F20.11,' deg  F8. ',F20.11,' deg/hour'/)
 7030 FORMAT(///' ***** Routine ETASTE finished the execution.'/)
      END

