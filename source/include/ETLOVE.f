C      SUBROUTINE ETLOVE(IUN6,IPRINT,DLAT,DELV)
      SUBROUTINE ETLOVE(IPRINT,DLAT,DELV)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C     Routine ETLOVE, version 930706 FORTRAN 77.                       C
C                                                                      C
C     The routine computes latitude dependent LOVE-numbers DH, DK,     C
C     SHIDA-numbers DL, gravimeter factors DG and tilt factors DT      C
C     using the WAHR-DEHANT-ZSCHAU model.                              C
C                                                                      C
C     Body tide amplitude factors for WAHR-DEHANT-ZSCHAU model.        C
C     The NDFW resonance is approximated by                            C
C                                                                      C
C     G(RES) = GLAT - GR*(DOM - DOM0)/(DOMR - DOM).                    C
C                                                                      C
C     similar equations hold for the other parameters.                 C
C                                                                      C
C     Gravimetric amplitude factors, LOVE numbers h and k for degree   C
C     0...3 have been taken from DEHANT 1987, Table 7, 8 and 9         C
C     for an elliptical, uniformly rotating, oceanless Earth with      C
C     liquid outer core and inelastic mantle (PREM Earth model with    C
C     inelastic mantle from ZSCHAU) and for the fourth degree from     C
C     DEHANT et. al 1989, Table 6). The resonance factors GR have      C
C     been computed to fit the difference between body tide amplitude  C
C     factors at O1 and PSI1 from DEHANT 1987, PREM model with         C
C     elastic mantle (Table 1...3). The NDFW resonance frequency is    C
C     15.073729 degree per hour = 1.004915267 CPD UT, taken from       C
C     WAHR 1981 (because it is not given in DEHANT's papers).          C
C                                                                      C
C     Input parameter description:                                     C
C     ----------------------------                                     C
C                                                                      C
C     IUN6...      formatted line printer unit.                        C
C     IPRINT...    printout parameter. For IPRINT=1, the computed      C
C                  LOVE- and SHIDA- number s will be printed.          C
C     DLAT...      ellipsoidal latitude in degree.                     C
C     DELV...      ellipsoidal height in meter.                        C
C                                                                      C
C     Description of COMMON /LOVE/:                                    C
C     -----------------------------                                    C
C                                                                      C
C     DOM0...      frequency of O1 in degree per hour.                 C
C     DOMR...      frequency of the FCN eigenfrequency in degree per   C
C                  hour.                                               C
C     DGLAT...     array(1..12) containing the gravimetric factors at  C
C                  latitude DLAT.                                      C
C     DGR...       resonance factor for gravimetric factors.           C
C     DHLAT        array(1..12) containing the LOVE-numbers h at       C
C                  latitude DLAT.                                      C
C     DHR...       resonance factor for the LOVE-number h(2,1).        C
C     DKLAT        array(1..12) containing the LOVE-numbers k at       C
C                  latitude DLAT.                                      C
C     DKR...       resonance factor for the LOVE-number k(2,1).        C
C     DLLAT...     array(1..12) containing the SHIDA-numbers l at      C
C                  latitude DLAT.                                      C
C     DLR...       resonance factor for the SHIDA-number l(2,1).       C
C     DTLAT...     array(1..12) containing the tilt factors at         C
C                  latitude DLAT.                                      C
C                                                                      C
C     Reference:                                                       C
C     ----------                                                       C
C                                                                      C
C                                                                      C
C     DEHANT, V. 1987: Tidal Parameters for an Inelastic Earth.        C
C           Physics of the Earth and Planetary Interiors, 49, 97-116,  C
C           1987.                                                      C
C                                                                      C
C     WAHR, J.M. 1981: Body tides on an elliptical, rotating, elastic  C
C           and oceanless earth. Geophysical Journal of the Royal      C
C           astronomical Society, vol. 64, 677-703, 1981.              C
C                                                                      C
C     ZSCHAU, J. and R. WANG 1987: Imperfect elasticity in the Earth's C
C           mantle. Implications for Earth tides and long period       C
C           deformations. Proceedings of the 9th International Sym-    C
C           posium on Earth Tides, New York 1987, pp. 605-629, editor  C
C           J.T. KUO, Schweizerbartsche Verlagsbuchhandlung, Stuttgart C
C           1987.                                                      C
C                                                                      C
C                                                                      C
C     Routine creation:  930703 by H.-G. Wenzel.                       C
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
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     The following DIMENSION statement is concerning the elastic      C
C     Earth model for the different degree and order constituents.     C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DOUBLE PRECISION DG0(12),DGP(12),DGM(12)
      DOUBLE PRECISION DH0(12),DHP(12),DHM(12)
      DOUBLE PRECISION DK0(12),DKP(12),DKM(12)
      DOUBLE PRECISION DL0(12),DLP(12),DLM(12)
      DOUBLE PRECISION DLATP(12),DLATM(12)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     COMMON /LOVE/ contains gravimeter factors, LOVE-numbers, SHIDA-  C
C     numbers and tilt factors for degree 2...4 at latitude DLAT:      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DIMENSION DGLAT(12),DHLAT(12),DKLAT(12),DLLAT(12),DTLAT(12)
      COMMON /LOVE/ DOM0,DOMR,DGLAT,DGR,DHLAT,DHR,DKLAT,DKR,DLLAT,DLR,
     1 DTLAT,DTR
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     COMMON /CONST/:                                                  C
C     DPI...        3.1415....                                         C
C     DPI2...       2.D0*DPI                                           C
C     DRAD...       DPI/180.D0                                         C
C     DRO...        180.D0/DPI                                         C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      COMMON /CONST/ DPI,DPI2,DRAD,DRO
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     The following DATA statements are concerning the elastic         C
C     Earth model for the different degree and order constituents.     C
C     The latitude dependency is not given for all constituents in     C
C     the WAHR-DEHANT-ZSCHAU model !!!!!!                              C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DATA DG0/1.1576D0,1.1542D0,1.1600D0,1.0728D0,1.0728D0,1.0728D0,
     1 1.0728D0,1.0363D0,1.0363D0,1.0363D0,1.0363D0,1.0363D0/
      DATA DGP/-0.0016D0,-0.0018D0,-0.0010D0,0.D0,0.D0,0.D0,-0.0010D0,
     1 0.D0,0.D0,0.D0,0.D0,-0.000315D0/
      DATA DGM/0.0054D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,
     1 0.D0,0.D0/
      DATA DH0/0.6165D0,0.6069D0,0.6133D0,0.2946D0,0.2946D0,0.2946D0,
     1 0.2946D0,0.1807D0,0.1807D0,0.1807D0,0.1807D0,0.1807D0/
      DATA DHP/0.0007D0,0.0007D0,0.0005D0,0.D0,0.D0,0.D0,0.0003D0,
     1 0.D0,0.D0,0.D0,0.D0,0.00015D0/
      DATA DHM/0.0018D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,
     1 0.D0,0.D0/
      DATA DK0/0.3068D0,0.3009D0,0.3034D0,0.0942D0,0.0942D0,0.0942D0,
     1 0.0942D0,0.0427D0,0.0427D0,0.0427D0,0.0427D0,0.0427D0/
      DATA DKP/0.0015D0,0.0014D0,0.0009D0,0.D0,0.D0,0.D0,0.0007D0,
     1 0.D0,0.D0,0.D0,0.D0,0.00066D0/
      DATA DKM/-0.0004D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,
     1 0.D0,0.D0/
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     SHIDA-numbers:                                                   C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DATA DL0/ 0.0840D0,0.0841D0,0.0852D0,0.0149D0,0.0149D0,0.0149D0,
     1 0.0149D0,0.0100D0,0.0100D0,0.0100D0,0.0100D0,0.0100D0/
      DATA DLP/-0.002D0,-0.002D0,-0.001D0,0.0000D0,0.0000D0,0.0000D0,
     1 0.0000D0,0.0000D0,0.0000D0,0.0000D0,0.0000D0,0.0000D0/
      DATA DLM/ 0.0000D0,0.0000D0,0.0000D0,0.0000D0,0.0000D0,0.0000D0,
     1 0.0000D0,0.0000D0,0.0000D0,0.0000D0,0.0000D0,0.0000D0/
      DATA DLATP/0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,
     1 0.D0,0.D0/
      DATA DLATM/0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,0.D0,
     1 0.D0,0.D0/
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Definition of parameters of Geodetic Reference System 1980.      C
C     DEA  is major semi axis in meter.                                C
C     DEE  is square of first excentricity (without dimnension).       C
C     DEGM is geocentric gravitational constant in m*3/s**2.           C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DATA DEA/6378137.00D0/,DEE/6.69438002290D-3/
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Define resonance frequency and resonance factors:                C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DOMR=15.073729D0
      DOM0=13.943036D0
      DGR =-0.000625D0
      DHR =-0.002505D0
      DKR =-0.001261D0
      DLR =0.0000781D0
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     DCLAT is cos and DSLAT is sin of ellipsoidal latitude.           C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DCLAT=DCOS(DLAT*DRAD)
      DSLAT=DSIN(DLAT*DRAD)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute ellipsoidal curvature radius DN in meter.               C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DN=DEA/DSQRT(1.D0-DEE*DSLAT**2)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute geocentric latitude DPSI in degree:                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DPSI=DRO*DATAN(((DN*(1.D0-DEE)+DELV)*DSLAT)/((DN+DELV)*DCLAT))
      DTHET=90.D0-DPSI
      DCT=DCOS(DTHET*DRAD)
      DCT2=DCT*DCT
      DLATP(1)=0.335410D0*(35.D0*DCT2*DCT2-30.D0*DCT2+3.D0)/
     1 (3.D0*DCT2-1.D0)
      DLATM(1) =0.894427D0/(3.D0*DCT2-1.D0)
      DLATP(2) =0.612372D0*(7.D0*DCT2-3.D0)
      DLATP(3) =0.866025D0*(7.D0*DCT2-1.D0)
      DLATP(7) =0.829156D0*(9.D0*DCT2-1.D0)
      DLATP(12)=0.806226D0*(11.D0*DCT2-1.D0)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute latitude dependent gravimeter factors DG:                C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DO 110 I=1,12
  110 DGLAT(I)=DG0(I)+DGP(I)*DLATP(I)+DGM(I)*DLATM(I)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute latitude dependent LOVE-numbers DH (for vertical         C
C     displacement):                                                   C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DO 120 I=1,12
  120 DHLAT(I)=DH0(I)+DHP(I)*DLATP(I)+DHM(I)*DLATM(I)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute latitude dependent LOVE-numbers DK:                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DO 130 I=1,12
  130 DKLAT(I)=DK0(I)+DKP(I)*DLATP(I)+DKM(I)*DLATM(I)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute latitude dependent SHIDA-numbers DL:                     C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DO 140 I=1,12
  140 DLLAT(I)=DL0(I)+DLP(I)*DLATP(I)+DLM(I)*DLATM(I)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute latitude dependent tilt factors DT:                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DO 150 I=1,12
      DTLAT(I)=1.D0+DK0(I)-DH0(I)+DLATP(I)*(DKP(I)-DHP(I))+
     1 DLATM(I)*(DKM(I)-DHM(I))
  150 CONTINUE
      DTR=DKR-DHR
      IF(IPRINT.EQ.0) RETURN
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Print out of parameters:                                         C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      WRITE(IUN6,7001) DOM0,DOMR,DGR,DHR,DKR,DLR,DTR
      WRITE(*,7001) DOM0,DOMR,DGR,DHR,DKR,DLR,DTR
      I=0
C      WRITE(IUN6,7002) DLAT
      WRITE(*,7002) DLAT
      DO 300 L=2,4
C      WRITE(IUN6,7004)
      WRITE(*,7004)
      DO 300 M=0,L
      I=I+1
C      WRITE(IUN6,7003)  L,M,DGLAT(I),DHLAT(I),DKLAT(I),DLLAT(I),DTLAT(I)
      WRITE(*,7003)  L,M,DGLAT(I),DHLAT(I),DKLAT(I),DLLAT(I),DTLAT(I)
  300 CONTINUE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Format statements:                                               C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 7001 FORMAT(/' Routine ETLOVE, version 930706 FTN77.'/
     1' Latitude dependent parameters for an elliptical, rotating,'/
     2' inelastic and oceanless Earth from WAHR-DEHANT-ZSCHAU model.'//
     3'    frequency of wave O1:',F10.6,' deg per hour'/
     4'    resonance frequency :',F10.6,' deg per hour'//
     5'    resonance factor for G:',F10.6/
     6'    resonance factor for h:',F10.6/
     7'    resonance factor for k:',F10.6/
     8'    resonance factor for l:',F10.6/
     9'    resonance factor for T:',F10.6/)
 7002 FORMAT(//' Latitude dependent elastic parameters'//
     1' ellipsoidal latitude:',F10.4,' deg'//
     2' G    is gravimetric factor delta'/
     3' h    is LOVE-number  h'/
     4' k    is LOVE-number  k'/
     5' l    is SHIDA-number l'/
     6' T    is tilt factor gamma'//
     7' degree  order         G         h         k         l',
     8'         T')
 7003 FORMAT(2I7,5F10.6)
 7004 FORMAT(' ')
      RETURN
      END

