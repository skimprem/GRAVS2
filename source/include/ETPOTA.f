CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      SUBROUTINE ETPOTA(IUN4,IUN6,IUN14,IPRINT,IMODEL,DLAT,DLON,DH,
C     1 DGRAV,DAZ,IC,ITY,ITM,ITD,DTH,DDT0,MAXNW,NRW,DTHAM,DTHPH,DTHFR,
      SUBROUTINE ETPOTA(IUN4,IPRINT,IMODEL,DLAT,DLON,DH,
     1 DGRAV,DAZ,IC,dmjul,DTH,DDT0,MAXNW,NRW,DTHAM,DTHPH,DTHFR,
     2 DBODY,NW)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C     Routine ETPOTA, version 930710 FORTRAN 77.                       C
C                                                                      C
C     The routine ETPOTA computes amplitudes, phases, frequencies and  C
C     body tide amplitude factors for a number of different Earth tide C
C     components using three different tidal potential developments.   C
C                                                                      C
C     Attention: This routine has finally not been tested for vertical C
C                and horizontal displacements and for shear tidal      C
C                strain !!!!                                           C
C                                                                      C
C     All variables with D as first character are DOUBLE PRECISION.    C
C                                                                      C
C     Input parameter description:                                     C
C     ----------------------------                                     C
C                                                                      C
C     IUN4...      formatted unit, on which the tidal potential        C
C                  development has to be stored before the execution   C
C                  of routine ETPOTA  (e.g. file ETCPOT.DAT).          C
C     IUN6...      formatted line printer unit.                        C
C     IUN14...     unformatted copy of IUN4. This unit will be opened  C
C                  as file ETCPOT.UFT during the execution of routine  C
C                  ETPOTA with STATUS=OLD if it exists and with        C
C                  STATUS=NEW, if it does not exist. If the STATUS is  C
C                  NEW, ETCPOT.DAT will be established during the      C
C                  execution of routine ETPOTA.                        C
C     IPRINT...    printout parameter.                                 C
C                  for IPRINT = 0, nothing will be printed.            C
C                  for IPRINT = 1, a short list will be printed.       C
C                  for IPRINT = 2, a long list will be printed         C
C                  (including the tidal potential development).        C
C     IMODEL...    parameter for selecting the tidal potential         C
C                  development.                                        C
C                  IMODEL = 0: DOODSON 1921 tidal potential develop-   C
C                              ment with 378 waves.                    C
C                  IMODEL = 1: CARTWRIGHT-TAYLOR-EDDEN 1973 tidal      C
C                              potential development with 505 waves.   C
C                  IMODEL = 2: TAMURA 1987 tidal potential develop-    C
C                              ment with 1200 waves.                   C
C                  IMODEL = 3: BUELLESFELD 1985 tidal potential        C
C                              development with 656 waves.             C
C     DLAT...      ellipsoidal latitude  referring to Geodetic         C
C                  Reference System 1980 in degree.                    C
C     DLON...      ellipsoidal longitude referring to Geodetic         C
C                  Reference System 1980 in degree, positive east of   C
C                  Greenwhich.                                         C
C     DH...        ellipsoidal height referring to Geodetic Reference  C
C                  System 1980 in meter.                               C
C     DGRAV...     gravity in m/s**2. If the gravity is input below    C
C                  1 m/s**2, the gravity will be replaced by the       C
C                  computed normal gravity for reference system GRS80. C
C     DAZ...       azimuth in degree from north direction (only valid  C
C                  for tidal tilt, horizontal displacement, and        C
C                  horizontal strain).                                 C
C     IC...        Earth tide component to be computed.                C
C                  IC=-1: tidal potential in m**2/s**2.                C
C                  IC= 0: vertical tidal acceleration (gravity tide),  C
C                         in nm/s**2 (positive downwards).             C
C                  IC= 1: horizontal tidal acceleration (tidal tilt)   C
C                         in azimuth DAZ in mas = arc sec/1000.        C
C                  IC= 2: vertical tidal displacement, geodetic        C
C                         coefficients in mm (positive upwards).       C
C                  IC= 3: horizontal tidal displacement in azimuth     C
C                         DAZ in mm.                                   C
C                  IC= 4: vertical tidal strain in 10**-9 = nstr.      C
C                  IC= 5: horizontal tidal strain in azimuth DAZ       C
C                         in 10**-9 = nstr.                            C
C                  IC= 6: areal  tidal strain in 10**-9 = nstr.        C
C                  IC= 7: shear  tidal strain in 10**-9 = nstr.        C
C                  IC= 8: volume tidal strain in 10**-9 = nstr.        C
C                  IC= 9: ocean tides, geodetic coefficients in        C
C                         millimeter.                                  C
C     IR...   printout parameter for the tidal potential development.  C
C             IR= 0: no printout of the tidal potential development.   C
C             IR= 1: tidal potential development and the development   C
C                    of the specific tidal component will be printed.  C
C     ITY...       year  of initial epoch of tidal development.        C
C     ITM...       month of initial epoch of tidal development.        C
C     ITD...       day   of initial epoch of tidal development.        C
C     DTH...       hour  of initial epoch of tidal development (UTC).  C
C                  Example: September 27th 1984, 10 o'clock UTC is     C
C                  ITY = 1984, ITM = 9, ITD = 27, DTH = 10.D0          C
C     MAXNW...     maximum number of waves, used for DIMENSION of      C
C                  arrays NRW, DTHAM, DTHPH, DTHFR, DBODY. Although    C
C                  the TAMURA 1987 potential development contains 1200 C
C                  waves only, the CTED 1973 and the BUELLESFELD 1985  C
C                  tidal potential developments contain some few waves C
C                  with small amplitude, which do not exist in the     C
C                  TAMURA 1987 potentail development. Thus, the total  C
C                  maximum number of waves MAXNW of file ETCPOT.DAT is C
C                  1214.                                               C
C                                                                      C
C     Output parameter description:                                    C
C     -----------------------------                                    C
C                                                                      C
C     DDT0...      difference TDT - UTC at initial epoch in sec.       C
C     NRW...       array(1...MAXNW) of wave numbers.                   C
C     DTHAM...     array (1...MAXNW) of tidal amplitudes given in      C
C                  units of CUNIT(IC2).                                C
C     DTHPH...     array (1...MAXNW) of tidal phases in radians at     C
C                  initial epoch.                                      C
C     DTHFR...     array (1...MAXNW) of tidal frequencies in radian    C
C                  per hour.                                           C
C     DBODY...     array (1...MAXNW) of bady tide amplitude factors    C
C                  for tidal gravity and tidal tilt. In order to       C
C                  compute the body tide, the amplitudes DTHAM have to C
C                  be multiplied by DBODY. In case of IRIGID=1, all    C
C                  body tide amplitude factors are set to 1.0000       C
C     NW...        number of defined tidal waves.                      C
C                                                                      C
C     Used routines:                                                   C
C     --------------                                                   C
C                                                                      C
C     ETASTE: computes astronomical elements.                          C
C     ETGCOF: computes geodetic coefficients.                          C
C     ETJULD: computes Julian date.                                    C
C     ETLOVE: computes latitude dependent elastic parameters (called   C
C             ETGCOF).                                                 C
C     ETMUTC: computes the difference TDT minus UTC (called by ETASTE).C
C     GEOEXT: computes jobtime.                                        C
C                                                                      C
C     Numerical accuracy:                                              C
C     -------------------                                              C
C                                                                      C
C     The routine has been tested under operation systems UNIX and     C
C     MS-DOS with 15 digits in DOUBLE PRECISION.                       C
C                                                                      C
C     Routine creation:  880427 by H.-G. Wenzel.                       C
C                        Geodaetisches Institut,                       C
C                        Universitaet Karlsruhe,                       C
C                        Englerstr. 7,                                 C
C                        D-76128 KARLSRUHE 1,                          C
C                        Germany.                                      C
C                        Tel: 0049-721-6082307,                        C
C                        FAX: 0049-721-694552.                         C
C                        e-mail: wenzel@gik.bau-verm.uni-karlsruhe.de  C
C     Last modification: 930706 by H.-G. Wenzel.                       C
C**********************************************************************C
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT INTEGER*4 (I-N)
      LOGICAL*1 LEX14
      CHARACTER CHEAD(8)*10,CENDH*10,CUNIT(11)*8,CWN*4,CMODEL(4)*13
      INTEGER*2 NS(8)
      DOUBLE PRECISION DX(3),DHH(4)
      DOUBLE PRECISION DAS(8),DASP(8),DGK(12),DPK(12)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     The following DIMENSION statement is concerning the number of    C
C     waves of the tidal potential development, which is MAXNW.        C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      INTEGER*4 NRW(MAXNW)
      DOUBLE PRECISION DTHAM(MAXNW),DTHPH(MAXNW),DTHFR(MAXNW),
     1 DBODY(MAXNW)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     The following DIMENSION statement is concerning the elastic      C
C     Earth model for the different degree and order constituents.     C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DOUBLE PRECISION DELTA(12)
      COMMON /UNITS/ CUNIT,IC2
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     COMMON /CONST/:                                                  C
C     DPI...        3.1415....                                         C
C     DPI2...       2.D0*DPI                                           C
C     DRAD...       DPI/180.D0                                         C
C     DRO...        180.D0/DPI                                         C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      COMMON /CONST/ DPI,DPI2,DRAD,DRO
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     COMMON /LOVE/ contains gravimeter factors, LOVE-numbers, SHIDA-  C
C     numbers and tilt factors for degree 2...4 at latitude DLAT:      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DIMENSION DGLAT(12),DHLAT(12),DKLAT(12),DLLAT(12),DTLAT(12)
      COMMON /LOVE/ DOM0,DOMR,DGLAT,DGR,DHLAT,DHR,DKLAT,DKR,DLLAT,DLR,
     1 DTLAT,DTR
      DATA CENDH/'C*********'/
      DATA CMODEL/'DOODSON 1921 ','CTED 1973    ','TAMURA 1987  ',
     1            'BUELLESF.1985'/
      IF(IPRINT.EQ.0) GOTO 10
C      WRITE(IUN6,7001) CMODEL(IMODEL+1)
      WRITE(*,7001)    CMODEL(IMODEL+1)
   10 CONTINUE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Test, whether there exist already unformatted file ETCPOT.UFT:   C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      OPEN(UNIT=IUN14,FILE='ETCPOT.UFT',FORM='UNFORMATTED',STATUS='OLD',
C     1 ERR=11)
C      LEX14=.TRUE.
C      REWIND IUN14
C      GOTO 12
C   11 OPEN(UNIT=IUN14,FILE='ETCPOT.UFT',FORM='UNFORMATTED',STATUS='NEW')
      LEX14=.FALSE.
C      REWIND IUN4
C   12 CONTINUE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute geodetic coefficients and body tide amplitude factors    C
C     for the WAHR-DEHANT-ZSCHAU model. The NDFW resonance is          C
C     approximated by                                                  C
C                                                                      C
C     G0 - GR*(DOM - DOM0)/(DOMR - DOM),                               C
C                                                                      C
C     similar equations hold for the other components.                 C
C                                                                      C
C     Gravimetric amplitude factors, LOVE numbers h and k for zero to  C
C     third degree tidal potential have been taken from DEHANT 1987,   C
C     table 7, 8 and 9 for elliptical, uniformly rotating, oceanless   C
C     Earth with liquid outer core and inelastic mantle (PREM Earth    C
C     model with inelastic mantle from ZSCHAU) and for the fourth      C
C     degree from DEHANT et al. 1989, table 6). The resonance factors  C
C     GR have been computed to fit the difference between body tide    C
C     amplitude factors at waves O1 and PSI1 from DEHANT 1987, PREM    C
C     model with elastic mantle (table 1...3). The NDFW resonance      C
C     frequency is 15.073729 degree per hour  = 1.004915267 CPD UT,    C
C     taken from WAHR 1981 (because it is not given in any of DEHANT's C
C     papers).                                                         C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      CALL ETGCOF(IUN6,IPRINT,DLAT,DLON,DH,DGRAV,DAZ,IC,DGK,DPK)
      CALL ETGCOF(IPRINT,DLAT,DLON,DH,DGRAV,DAZ,IC,DGK,DPK)
      IC2=IC+2
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Define default body tide amplitude factors for components        C
C     IC=2...9.                                                        C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DO 50 I=1,12
  50  DELTA(I)=1.D0
      DELTAR=0.D0
      GOTO (100,200,300),IC2
      GOTO 1000
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     IC=-1, compute body tide amplitude factors for tidal potential:  C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
  100 CONTINUE
      DO 110 I=1,12
  110 DELTA(I)=DKLAT(I)
      DELTAR=DKR
      GOTO 1000
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     IC=0, compute body tide amplitude factors for vertical component C
C     (gravity tides):                                                 C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
  200 CONTINUE
      DO 210 I=1,12
  210 DELTA(I)=DGLAT(I)
      DELTAR=DGR
      GOTO 1000
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     IC=1: compute body tide amplitude factors for horizontal         C
C     component (tidal tilt):                                          C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
  300 CONTINUE
      DO 310 I=1,12
  310 DELTA(I)=DTLAT(I)
      DELTAR=DKR-DHR
 1000 CONTINUE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     compute JULIAN date for initial epoch ITY,ITM,ITD,DTH:           C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      CALL ETJULD(IUN6,ITY,ITM,ITD,DTH,DT)
C      DT = dmjul + 2400000.5D0
C      write(*,*)'dmjul,DT,DTH',dmjul,dmjul + 2400000.5D0,DTH
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute astronomical elements for initial epoch:                 C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      CALL ETASTE(IUN6,IPRINT,IMODEL,DLON,ITY,ITM,ITD,DTH,DAS,DASP,
C     1 DDT0)
      CALL ETASTE(IPRINT,IMODEL,DLON,dmjul,DTH,DAS,DASP,DDT0)
C      DT2000=(DT-2451544.D0)/36525.0D0
C      DT=(DT-2415020.0D0)/36525.0D0
      DT2000=(dmjul-51543.5D0)/36525.0D0
      DT=(dmjul-15019.5D0)/36525.0D0
      IC2=IC+2
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Interpolation factors for CTED 1973 potential coefficients at    C
C     start epoch by linear least squares interpolation (see WENZEL    C
C     1976).                                                           C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DX(1)=0.550376D0-1.173312D0*DT
      DX(2)=0.306592D0+0.144564D0*DT
      DX(3)=0.143033D0+1.028749D0*DT
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Read file header of tidal potential file on unit IUN4:           C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      IF(LEX14) READ(IUN14) (CHEAD(I),I=1,8)
      IF(.NOT.LEX14) READ(IUN4,7028)  (CHEAD(I),I=1,8)
C      IF(.NOT.LEX14) WRITE(IUN14) (CHEAD(I),I=1,8)
C      WRITE(IUN6,7029) (CHEAD(I),I=1,8)
C      WRITE(*,7029)    (CHEAD(I),I=1,8)
C 1100 IF(LEX14) READ(IUN14) (CHEAD(I),I=1,8)
 1100 IF(.NOT.LEX14) READ(IUN4,7028)  (CHEAD(I),I=1,8)
C      IF(.NOT.LEX14) WRITE(IUN14) (CHEAD(I),I=1,8)
C      IF(IPRINT.GT.1) WRITE(IUN6,7029) (CHEAD(I),I=1,8)
      IF(IPRINT.GT.1) WRITE(*,7029)    (CHEAD(I),I=1,8)
      IF(CHEAD(1).NE.CENDH) GOTO 1100
C      IF(LEX14) READ(IUN14) NW
      IF(.NOT.LEX14) READ(IUN4,7005) NW
C      IF(.NOT.LEX14) WRITE(IUN14) NW
      IF(NW.GT.MAXNW) GOTO 5000
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute tidal development for the specific component from tidal  C
C     potential development:                                           C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DSH1=0.D0
      DSH2=0.D0
      DSH3=0.D0
      DSHD=0.D0
      DSHT=0.D0
      IW=1
      NAMPL=0
 1110 CONTINUE
 1120 CONTINUE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     NRT is the wave number of TAMURA's 1987 tidal potential          C
C     development, NRC is the wave number of CARTWRIGHT-TAYLER-EDDEN   C
C     1973 tidal potential development.                                C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      IF(LEX14) READ(IUN14,END=2000) NRI,K,(NS(J),J=1,8),NP,
C     1 (DHH(J),J=1,3),CWN,DHD,DHT,DHTD,DHB
      IF(.NOT.LEX14) READ(IUN4,7006,END=2000) NRI,K,(NS(J),J=1,8),NP,
     1 (DHH(J),J=1,3),CWN,DHD,DHT,DHTD,DHB
C      IF(.NOT.LEX14) WRITE(IUN14) NRI,K,(NS(J),J=1,8),NP,
C     1 (DHH(J),J=1,3),CWN,DHD,DHT,DHTD,DHB
      DSHT=DSHT+DHT
      J=IW-1
C     IF(MOD(J,51).EQ.0.AND.IPRINT.EQ.2) WRITE(IUN6,7007) ITY,ITM,ITD,
C    1 DTH,IC,NW,CUNIT(IC2)
      DHH(4)=0.D0
      DSH1=DSH1+DHH(1)
      DSH2=DSH2+DHH(2)
      DSH3=DSH3+DHH(3)
      DSHD=DSHD+DHD
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     CTED 1973 tidal potential development is used:                   C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DO 1130 J=1,3
 1130 DHH(4)=DHH(4)+DHH(J)*DX(J)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     DOODSON's 1921 tidal potential development is used:              C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IF(IMODEL.EQ.0) DHH(4)=DHD
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     TAMURA's 1987 tidal potential development is used:               C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IF(IMODEL.EQ.2) DHH(4)=DHT+DHTD*DT2000
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     BUELLESFELD's 1985 tidal potential development is used:          C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IF(IMODEL.EQ.3) DHH(4)=DHB
      IF(DABS(DHH(4)).LT.1.D-6) GOTO 1120
      NAMPL=NAMPL+1
      DC2=0.D0
      DC3=0.D0
      DO 1140 J=1,8
      DC2=DC2+NS(J)*DAS(J)
 1140 DC3=DC3+NS(J)*DASP(J)
      J=(K+1)*K/2-2+NS(1)
      DC2=DC2+DPK(J)+NP*90.D0
      DC1=DHH(4)*DGK(J)
      DBODY(IW)=DELTA(J)
      IF(J.EQ.2) DBODY(IW)=DELTA(J)+DELTAR*(DC3-DOM0)/(DOMR-DC3)
      IF(DC1.GE.0.D0) GOTO 1160
      DC1=-DC1
      DC2=DC2-180.D0
 1160 DC2=DMOD(DC2,360.D0)
      IF(DC2.GE.0.D0) GOTO 1170
      DC2=DC2+360.D0
      GOTO 1160
 1170 CONTINUE
      DTHAM(IW)=DC1
      DTHPH(IW)=DC2*DRAD
      DTHFR(IW)=DC3*DRAD
      NRW(IW)=NRI
C     IF(IPRINT.EQ.2) WRITE(IUN6,7008) NRI,K,(NS(J),J=1,8),NP,
C    1 (DHH(J),J=1,4),DC1,DC2,DC3,CWN,DHD,DHT,DHTD
C      IF(IPRINT.EQ.2) WRITE(IUN6,7011) DC1,DC2,DC3,CWN,DBODY(IW)
      IF(IPRINT.EQ.2) WRITE(*,7011)    DC1,DC2,DC3,CWN,DBODY(IW)
      IW=IW+1
      GOTO 1110
 2000 CONTINUE
      NW=IW-1
      IF(IPRINT.EQ.0) RETURN
C      WRITE(IUN6,7009) DSH1,DSH2,DSH3,DSHD,DSHT
      WRITE(*,7009)    DSH1,DSH2,DSH3,DSHD,DSHT
C      WRITE(IUN6,7010) NW,NAMPL
      WRITE(*,7010)    NW,NAMPL
C      WRITE(IUN6,7030)
      WRITE(*,7030)
      RETURN
 5000 CONTINUE
C      WRITE(IUN6,7050) NW,MAXNW
      WRITE(*,7050)    NW,MAXNW
      STOP
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Format statements:                                               C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 7001 FORMAT(' Routine ETPOTA, version 930710 FORTRAN 77.'/
     1' Tidal component development from tidal potential development.'/
     2 1X,A13,' tidal potential development is used.'/)
 7005 FORMAT(2I5)
 7006 FORMAT(I4,9X,10I2,3F7.5,1X,A4,F7.5,F8.6,F9.6,F8.6)
 7007 FORMAT(' Routine ETPOTA, version 930710 FORTRAN 77'/
     1' Tidal potential and tidal component development'/
     2' Initial epoch',I6,2(1H.,I2),F5.2/
     3' Component: ',I4/' Number of waves: ',I4/
     3' no. argum.  argum.nrs. NP  H1870  H1924',
     4'  H1960   H(T)   ampl.  phase   frequency'/
     5 55X,A6,' [deg]  [deg/h]'/)
 7008 FORMAT(1X,I4,10I2,4F7.5,F8.4,F9.4,F12.8,1X,A4/F7.5,F8.6,F9.6)
 7009 FORMAT(//' Sum of potential coefficients :'/5F10.6)
 7010 FORMAT(//' Number of waves is         :',I6/
     1         ' Number of amplitudes > 0 is:',I6/ )
 7011 FORMAT(3F10.5,2X,A6,2X,F10.6)
 7028 FORMAT(8A10)
 7029 FORMAT(1X,8A10)
 7030 FORMAT(///' ***** Routine ETPOTA finished execution.'/)
 7050 FORMAT(/' ***** Error in routine ETPOTA, version 930710.'/
     1' ***** The current number of waves:',I5,' exceeds the maximum',
     2' number of waves:',I5/
     3' ***** Routine ETPOTA stops the execution.'/)
      END

