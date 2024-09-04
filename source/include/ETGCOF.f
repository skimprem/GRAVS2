C      SUBROUTINE ETGCOF(IUN6,IPRINT,DLAT,DLON,DH,DGRAV,DAZ,IC,DGK,DPK)
      SUBROUTINE ETGCOF(IPRINT,DLAT,DLON,DH,DGRAV,DAZ,IC,DGK,DPK)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C     Routine ETGCOF, version 930706 FORTRAN 77.                       C
C                                                                      C
C     The routine ETGCOF computes the geodetic coefficients for        C
C     the tidal potential developments, DOODSON's normalization.       C
C                                                                      C
C     All variables with D as first character are DOUBLE PRECISION.    C
C                                                                      C
C     Input parameter description:                                     C
C     ----------------------------                                     C
C                                                                      C
C     IUN6...      formatted line printer unit.                        C
C     DLAT...      ellipsoidal latitude in degree, referring to        C
C                  geodetic reference system GRS80.                    C
C     DLON...      ellipsoidal longitude in degree, referring to       C
C                  geodetic reference system GRS80, positiv east of    C
C                  Greenwhich.                                         C
C     DH...        ellipsoidal height in meter, referring to geodetic  C
C                  reference system GRS80.                             C
C     DGRAV...     gravity in m/s**2. If DGRAV less than  9.50 m/s**2, C
C                  DGRAV will be overwritten by normal gravity         C
C                  referring to geodetic reference system 1980.        C
C     DAZ...       azimuth in degree from north direction counted      C
C                  clockwise (necessary for tidal tilt only).          C
C     IC...        Earth tide component to be computed.                C
C                  IC=-1: tidal potential, geodetic coefficients       C
C                         in m**2/s**2.                                C
C                  IC= 0: vertical tidal acceleration (gravity tide),  C
C                         geodetic coefficients in nm/s**2 (positive   C
C                         down).                                       C
C                  IC= 1: horizontal tidal acceleration (tidal tilt)   C
C                         in azimuth DAZ, geodetic coefficients in     C
C                         mas = arc sec/1000.                          C
C                  IC= 2: vertical tidal displacement, geodetic        C
C                         coefficients in mm.                          C
C                  IC= 3: horizontal tidal displacement in azimuth     C
C                         DAZ, geodetic coefficients in mm.            C
C                  IC= 4: vertical tidal strain, geodetic coefficients C
C                         in 10**-9 = nstr.                            C
C                  IC= 5: horizontal tidal strain in azimuth DAZ,      C
C                         geodetic coefficients in 10**-9 = nstr.      C
C                  IC= 6: areal tidal strain, geodetic coefficients    C
C                         in 10**-9 = nstr.                            C
C                  IC= 7: shear tidal strain, geodetic coefficients    C
C                         in 10**-9 = nstr.                            C
C                  IC= 8: volume tidal strain, geodetic coefficients   C
C                         in 10**-9 = nstr.                            C
C                  IC= 9: ocean tides, geodetic coefficients in        C
C                         millimeter.                                  C
C                                                                      C
C     Output parameter description:                                    C
C     -----------------------------                                    C
C                                                                      C
C     DGK...       array (1...12) of geodetic coefficients.            C
C     DGK(1)...    geodetic coefficient for degree 2 and order 0.      C
C     DGK(2)...    geodetic coefficient for degree 2 and order 1.      C
C     DGK(3)...    geodetic coefficient for degree 2 and order 2.      C
C     DGK(4)...    geodetic coefficient for degree 3 and order 0.      C
C     DGK(5)...    geodetic coefficient for degree 3 and order 1.      C
C     DGK(6)...    geodetic coefficient for degree 3 and order 2.      C
C     DGK(7)...    geodetic coefficient for degree 3 and order 3.      C
C     DGK(8)...    geodetic coefficient for degree 4 and order 0.      C
C     DGK(9)...    geodetic coefficient for degree 4 and order 1.      C
C     DGK(10)...   geodetic coefficient for degree 4 and order 2.      C
C     DGK(11)...   geodetic coefficient for degree 4 and order 3.      C
C     DGK(12)...   geodetic coefficient for degree 4 and order 4.      C
C                                                                      C
C     DPK...       array (1...12) of phases in degree.                 C
C     DPK(1)...    phase in degree for      degree 2 and order 0.      C
C     DPK(2)...    phase in degree for      degree 2 and order 1.      C
C     DPK(3)...    phase in degree for      degree 2 and order 2.      C
C     DPK(4)...    phase in degree for      degree 3 and order 0.      C
C     DPK(5)...    phase in degree for      degree 3 and order 1.      C
C     DPK(6)...    phase in degree for      degree 3 and order 2.      C
C     DPK(7)...    phase in degree for      degree 3 and order 3.      C
C     DPK(8)...    phase in degree for      degree 4 and order 0.      C
C     DPK(9)...    phase in degree for      degree 4 and order 1.      C
C     DPK(10)..    phase in degree for      degree 4 and order 2.      C
C     DPK(11)..    phase in degree for      degree 4 and order 3.      C
C     DPK(12)..    phase in degree for      degree 4 and order 4.      C
C                                                                      C
C     Used routines:                                                   C
C     --------------                                                   C
C                                                                      C
C     ETLOVE: computes latitude dependent elastic parameters.          C
C                                                                      C
C     Numerical accuracy:                                              C
C     -------------------                                              C
C                                                                      C
C     The routine has been tested under operation system MS-DOS and    C
C     UNIX in double precision (8 byte words = 15 digits) using        C
C     different compilers.                                             C
C                                                                      C
C     References:                                                      C
C                                                                      C
C     WILHELM, H. and W. ZUERN 1984: Tidal forcing field.              C
C           In: LANDOLT-BOERNSTEIN, Zahlenwerte und Funktionen aus     C
C           Naturwissenschaften und Technik, New series, group V,      C
C           Vol. 2, Geophysics of the Solid Earth, the Moon and the    C
C           Planets, Berlin 1984.                                      C
C                                                                      C
C     ZUERN, W. and  H. WILHELM 1984: Tides of the solid Earth.        C
C           In: LANDOLT-BOERNSTEIN, Zahlenwerte und Funktionen aus     C
C           Naturwissenschaften und Technik, New series, group V, Vol. C
C           2, Geophysics of the Solid Earth, the Moon and the Planets,C
C           Berlin 1984.                                               C
C                                                                      C
C     Routine creation:  880129 by H.-G. Wenzel,                       C
C                        Geodaetisches Institut,                       C
C                        Universitaet Karlsruhe,                       C
C                        Englerstr. 7,                                 C
C                        D-76128 KARLSRUHE 1,                          C
C                        Germany.                                      C
C                        Tel.: 0721-6082307,                           C
C                        FAX:  0721-694552.                            C
C                        e-mail: wenzel@gik.bau-verm.uni-karlsruhe.de  C
C     Last modification: 930706 by H.-G. Wenzel.                       C
C**********************************************************************C
      IMPLICIT DOUBLE PRECISION (D)
      CHARACTER CUNIT(11)*8
      DIMENSION DGK(12),DPK(12),DGX(12),DGY(12),DGZ(12)
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
      COMMON /UNITS/ CUNIT,IC2
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Definition of parameters of Geodetic Reference System 1980.      C
C     DEA  is major semi axis in meter.                                C
C     DEE  is square of first excentricity (without dimension).        C
C     DEGM is geocentric gravitational constant in m*3/s**2.           C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DATA DEA/6378137.00D0/,DEE/6.69438002290D-3/,DEGM1/398600.5D0/
      DEGM=DEGM1*1.D9
C      IF(IPRINT.GT.0) WRITE(IUN6,7000) DEA,DEE,DEGM1
      IF(IPRINT.GT.0) WRITE(*,7000) DEA,DEE,DEGM1
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     DCLAT is cos and DSLAT is sin of ellipsoidal latitude.           C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DCLAT=DCOS(DLAT*DRAD)
      DSLAT=DSIN(DLAT*DRAD)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute normal gravity in m/s**2:                                C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IF(DGRAV.LT.9.50D0) DGRAV=9.78032677D0*(1.D0+0.001931851353D0*
     1 DSLAT**2)/DSQRT(1.D0-DEE*DSLAT**2)-0.3086D-5*DH
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute ellipsoidal curvature radius DN in meter.               C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DN=DEA/DSQRT(1.D0-DEE*DSLAT**2)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute geocentric latitude DPSI in degree:                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DPSI=DRO*DATAN(((DN*(1.D0-DEE)+DH)*DSLAT)/((DN+DH)*DCLAT))
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute geocentric radius DR1 in meter:                          C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DR1=DSQRT((DN+DH)**2*DCLAT**2+(DN*(1.D0-DEE)+DH)**2*DSLAT**2)
C      IF(IPRINT.GT.0) WRITE(IUN6,7001) DLAT,DPSI,DLON,DH,DGRAV,DR1,IC,
      IF(IPRINT.GT.0) WRITE(*,7001) DLAT,DPSI,DLON,DH,DGRAV,DR1,IC,
     1 DAZ
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Definition of astronomical parameters I.A.U. 1984.               C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DPAR=3422.448D0
      DMAS=1.D0/0.01230002D0
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute DOODSON's constant DDC in m**2/s**2:                     C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DR0=DEA*(1.D0-DEE/6.D0-5.D0*DEE**2/72.D0-DEE**3*55.D0/1296.D0)
      DDC=DR0**2*0.75D0*DEGM/(DEA**3*DMAS)
      DDC=DDC*(DPAR*DRAD/3600.D0)**3
      DF=DRO*3.600D-3/DGRAV
C      IF(IPRINT.GT.0) WRITE(IUN6,7002) DPAR,DMAS,DDC,DF
      IF(IPRINT.GT.0) WRITE(*,7002) DPAR,DMAS,DDC,DF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute latitude dependent elastic parameters from WAHR-DEHANT-  C
C     ZSCHAU model:                                                    C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      CALL ETLOVE(IUN6,IPRINT,DLAT,DH)
      CALL ETLOVE(IPRINT,DLAT,DH)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     DCPSI is cos and DSPSI is sin of geocentric latitude.            C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DCPSI=DCOS(DPSI*DRAD)
      DSPSI=DSIN(DPSI*DRAD)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute geodetic coefficients for tidal potential, stored        C
C     provisional in array DGK.                                        C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DC2=DDC*(DR1/DR0)**2
      DC3=DDC*(DR1/DR0)**3
      DC4=DDC*(DR1/DR0)**4
      DGK(1) =DC2*0.5D0*(1.D0-3.D0*DSPSI**2)
      DGK(2) =DC2*2.D0*DSPSI*DCPSI
      DGK(3) =DC2*DCPSI**2
      DGK(4) =DC3*1.118033989D0*DSPSI*(3.D0-5.D0*DSPSI**2)
      DGK(5) =DC3*0.726184378D0*DCPSI*(1.D0-5.D0*DSPSI**2)
      DGK(6) =DC3*2.598076212D0*DSPSI*DCPSI**2
      DGK(7) =DC3*DCPSI**3
      DGK(8) =DC4*0.125000000D0*(3.D0-30.D0*DSPSI**2+35.D0*DSPSI**4)
      DGK(9) =DC4*0.473473091D0*2.D0*DSPSI*DCPSI*(3.D0-7.D0*DSPSI**2)
      DGK(10)=DC4*0.777777778D0*DCPSI**2*(1.D0-7.D0*DSPSI**2)
      DGK(11)=DC4*3.079201436D0*DSPSI*DCPSI**3
      DGK(12)=DC4*DCPSI**4
      DO 10 I=1,12
   10 DPK(I)=0.D0
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute geodetic coefficients for tidal acceleration vector      C
C     orientated to sperical coordinate system, stored in DGX (north), C
C     DGY (east), DGZ (radially upwards), dimensions are nm/s**2.      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DC2=DDC/DR0*(DR1/DR0)*1.D9
      DC3=DDC/DR0*(DR1/DR0)**2*1.D9
      DC4=DDC/DR0*(DR1/DR0)**3*1.D9
      DGX(1) =-DC2*3.D0*DSPSI*DCPSI
      DGX(2) = DC2*2.D0*(DCPSI**2-DSPSI**2)
      DGX(3) =-DC2*2.D0*DSPSI*DCPSI
      DGX(4) = DC3*1.118033989D0*DCPSI*(3.D0-15.D0*DSPSI**2)
      DGX(5) = DC3*0.726184378D0*DSPSI*(4.D0-15.D0*DCPSI**2)
      DGX(6) = DC3*2.598076212D0*DCPSI*(1.D0-3.D0*DSPSI**2)
      DGX(7) =-DC3*3.D0*DSPSI*DCPSI**2
      DGX(8) =-DC4*0.125000000D0*DSPSI*DCPSI*(60.D0-140.D0*DSPSI**2)
      DGX(9) = DC4*0.473473091D0*(6.D0-54.D0*DSPSI**2+56.D0*DSPSI**4)
      DGX(10)=-DC4*0.777777778D0*DCPSI*(16.D0*DSPSI-28.D0*DSPSI**3)
      DGX(11)= DC4*3.079201436D0*DCPSI**2*(4.D0*DCPSI**2-3.D0)
      DGX(12)=-DC4*4.D0*DCPSI**3*DSPSI
C
      DGY(1) = 0.D0
      DGY(2) = 2.D0*DC2*DSPSI
      DGY(3) = 2.D0*DC2*DCPSI
      DGY(4) = 0.D0
      DGY(5) = DC3*0.726184378D0*(1.D0-5.D0*DSPSI**2)
      DGY(6) = 2.D0*DC3*2.598076212D0*DSPSI*DCPSI
      DGY(7) = 3.D0*DC3*DCPSI**2
      DGY(8) = 0.D0
      DGY(9) = 2.D0*DC4*0.473473091D0*DSPSI*(3.D0-7.D0*DSPSI**2)
      DGY(10)= 2.D0*DC4*0.777777778D0*DCPSI*(1.D0-7.D0*DSPSI**2)
      DGY(11)= 3.D0*DC4*3.079201436D0*DSPSI*DCPSI**2
      DGY(12)= 4.D0*DC4*DCPSI**3
C
      DGZ(1) = DC2*(1.D0-3.D0*DSPSI**2)
      DGZ(2) = 4.D0*DC2*DSPSI*DCPSI
      DGZ(3) = 2.D0*DC2*DCPSI**2
      DGZ(4) = 3.D0*DC3*1.118033989D0*DSPSI*(3.D0-5.D0*DSPSI**2)
      DGZ(5) = 3.D0*DC3*0.726184378D0*DCPSI*(1.D0-5.D0*DSPSI**2)
      DGZ(6) = 3.D0*DC3*2.598076212D0*DSPSI*DCPSI**2
      DGZ(7) = 3.D0*DC3*DCPSI**3
      DGZ(8) = 4.D0*DC4*0.125000000D0*(3.D0-30.D0*DSPSI**2+35.D0*
     1 DSPSI**4)
      DGZ(9) = 8.D0*DC4*0.473473091D0*DSPSI*DCPSI*(3.D0-7.D0*DSPSI**2)
      DGZ(10)= 4.D0*DC4*0.777777778D0*DCPSI**2*(1.D0-7.D0*DSPSI**2)
      DGZ(11)= 4.D0*DC4*3.079201436D0*DSPSI*DCPSI**3
      DGZ(12)= 4.D0*DC4*DCPSI**4
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Compute geodetic coefficients for tidal acceleration vector      C
C     orientated to ellipsoidal coordinate system stored in            C
C     DGX (north), DGY (east) and DGZ (upwards), all in nm/s**2.       C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DCDLAT=DCLAT*DCPSI+DSLAT*DSPSI
      DSDLAT=DSLAT*DCPSI-DCLAT*DSPSI
      DO 50 I=1,12
      DUMMY =DCDLAT*DGX(I)-DSDLAT*DGZ(I)
      DGZ(I)=DSDLAT*DGX(I)+DCDLAT*DGZ(I)
      DGX(I)=DUMMY
   50 CONTINUE
      IC2=IC+2
      DCAZ=DCOS(DAZ*DRAD)
      DSAZ=DSIN(DAZ*DRAD)
      GOTO(100,200,300,400,500,600,700,800,900,1000,1100),IC2
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     IC=-1, compute geodetic coefficients for tidal potential.        C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
  100 CONTINUE
      GOTO 2000
  200 CONTINUE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     IC=0, compute geodetic coefficients for vertical component       C
C           (gravity tide).                                            C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DO 210 I=1,12
      DGK(I)=DGZ(I)
  210 DPK(I)=180.0D0
      GOTO 2000
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     IC=1, compute geodetic coefficients for horizontal component     C
C           (tidal tilt) in azimuth DAZ, in mas.                       C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
  300 CONTINUE
      DO 310 I=1,12
      DGK(I)=DSQRT((DGX(I)*DCAZ)**2+(DGY(I)*DSAZ)**2)*DF
      DPK(I)=0.D0
      IF(DGX(I)*DCAZ.EQ.0.D0.AND.DGY(I)*DSAZ.EQ.0.D0) GOTO 310
      DPK(I)=DRO*DATAN2(DGY(I)*DSAZ,DGX(I)*DCAZ)
  310 CONTINUE
      GOTO 2000
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     IC=2, compute geodetic coefficients for vertical displacement    C
C           in mm.                                                     C
C     Attention: this component has never been tested !!!              C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
  400 CONTINUE
      DFAK=1.D3/DGRAV
      DO 410 I=1,12
      DGK(I)=DGK(I)*DHLAT(I)*DFAK
  410 DPK(I)=0.0D0
C      WRITE(IUN6,*) '*****The component',IC,' has never been tested !!!'
      WRITE(*,*)    '*****The component',IC,' has never been tested !!!'
      GOTO 2000
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     IC=3, compute geodetic coefficients for horizontal displacement  C
C           in azimuth DAZ in mm.                                      C
C     Attention: this component has never been tested !!!              C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
  500 CONTINUE
      DFAK=1.D3*DR1/DGRAV
      DO 510 I=1,12
      DGK(I)=DSQRT((DGX(I)*DCAZ)**2+(DGY(I)*DSAZ)**2)*DLLAT(I)*DFAK
      DPK(I)=0.D0
      IF(DGX(I)*DCAZ.EQ.0.D0.AND.DGY(I)*DSAZ.EQ.0.D0) GOTO 510
      DPK(I)=DRO*DATAN2(DGY(I)*DSAZ,DGX(I)*DCAZ)
  510 CONTINUE
C      WRITE(IUN6,*) '*****The component',IC,' has never been tested !!!'
      WRITE(*,*)    '*****The component',IC,' has never been tested !!!'
      GOTO 2000
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     IC=4, compute geodetic coefficients for vertical strain at the   C
C           Earth's deformed surface in 10**-9 units = nstr.           C
C           We use a spherical approximation for the vertical strain,  C
C           i.e. eps(rr) , and a POISSON ratio of 0.25 (see ZUERN and  C
C           WILHELM 1984, p. 282).                                     C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
  600 CONTINUE
      DPOISS=0.25D0
      DFAK=1.D9*DPOISS/(DPOISS-1.D0)
      DO 610 I=1,3
  610 DGK(I)=DGK(I)*DFAK*(2.D0*DHLAT(I)-2.D0*3.D0*DLLAT(I))/(DGRAV*DR1)
      DO 620 I=4,7
  620 DGK(I)=DGK(I)*DFAK*(2.D0*DHLAT(I)-3.D0*4.D0*DLLAT(I))/(DGRAV*DR1)
      DO 630 I=8,12
  630 DGK(I)=DGK(I)*DFAK*(2.D0*DHLAT(I)-4.D0*5.D0*DLLAT(I))/(DGRAV*DR1)
      DO 640 I=1,12
  640 DPK(I)=0.0D0
      GOTO 2000
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     IC=5, compute geodetic coefficients for horizontal strain        C
C           in azimuth DAZ, in 10**-9 units.                           C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
  700 CONTINUE
      DTHETA=(90.D0-DPSI)*DRAD
      DAZR=(DAZ+180.D0)*DRAD
      DCAZ =DCOS(DAZR)
      DSAZ =DSIN(DAZR)
      DSAZ2=DSIN(2.D0*DAZR)
      DCSTS=-0.5D0*DSIN(2.D0*DAZR)
      DCT=DSPSI
      DST=DCPSI
      DCT2=DCT*DCT
      DST2=DST*DST
      DCC2=DCOS(2.D0*DPSI*DRAD)
      DC2T=-DCC2
      DCOTT =1.D0/DTAN(DTHETA)
      DCOTT2=1.D0/DTAN(2.D0*DTHETA)
      DFAK=1.D9/(DR1*DGRAV)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Real part is stored in DGX, imaginary part is stored in DGY.     C
C     Formulas were given by Dr. W. Zuern, BFO Schiltach (personal     C
C     communication) and tested against horizontal strain computed     C
C     (with lower precision) by program ETIDEL.                        C
C     Results agreed to 0.3 % and 0.1 degree for most of the waves,    C
C     except for 2N2 and L2 (deviation of 3 %).                        C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DGX(1)=(DHLAT(1)-(6.D0*DLLAT(1)*DC2T)/(3.D0*DCT2-1.D0))*DCAZ**2
     1      +(DHLAT(1)-(6.D0*DLLAT(1)*DCT2)/(3.D0*DCT2-1.D0))*DSAZ**2
      DGY(1)=0.D0
      DGX(2)=(DHLAT(2)-4.D0*DLLAT(2))*DCAZ**2+(DHLAT(2)-DLLAT(2)/DST2
     1 +2.D0*DLLAT(2)*DCOTT*DCOTT2)*DSAZ**2
      DGY(2)=2.D0*DLLAT(2)*(2.D0*DCOTT2-DCOTT)*DCSTS/DST
      DGX(3)=(DHLAT(3)+2.D0*DLLAT(3)*(DCOTT*DCOTT-1.D0))*DCAZ**2
     1 +(DHLAT(3)-4.D0*DLLAT(3)/DST2+2.D0*DLLAT(3)*DCOTT*DCOTT)*DSAZ**2
      DGY(3)=4.D0*DLLAT(3)*DCOTT*DCSTS/DST
      DGX(4)=(DHLAT(4)+DLLAT(4)*(33.D0-45.D0*DCT2)/(5.D0*DCT2-3.D0))*
     1 DCAZ**2+(DHLAT(4)-DLLAT(4)*(1.D0+10.D0*DCT2/(5.D0*DCT2-3.D0)))*
     2 DSAZ**2
      DGY(4)=0.D0
      DGX(5)=(DHLAT(5)-DLLAT(5)*(1.D0+10.D0*(1.D0-4.D0*DCT2)/
     1 (1.D0-5.D0*DCT2)))*DCAZ**2+(DHLAT(5)+DLLAT(5)*
     2 (DCOTT*DCOTT-1.D0/DST2-10.D0*DCT2/(5.D0*DCT2-1.D0)))*DSAZ**2
      DGY(5)=-20.D0*DLLAT(5)*DCT*DCSTS/(5.D0*DCT2-1.D0)
      DGX(6)=(DHLAT(6)+DLLAT(6)*(2.D0*DCOTT*DCOTT-7.D0))*DCAZ**2
     1 +(DHLAT(6)+DLLAT(6)*(2.D0*DCOTT*DCOTT-1.D0-4.D0/DST2))*DSAZ**2
      DGY(6)=-4.D0*DLLAT(6)*(DCOTT-1.D0/DCOTT)*DCSTS/DST
      DGX(7)=(DHLAT(7)+DLLAT(7)*(6.D0*DCOTT*DCOTT-3.D0))*DCAZ**2
     1 +(DHLAT(7)+DLLAT(7)*(3.D0*DCOTT*DCOTT-9.D0/DST2))*DSAZ**2
      DGY(7)=12.D0*DLLAT(7)*DCOTT*DCSTS/DST
      DGX(8)=(DHLAT(8)-4.D0*DLLAT(8)*(4.D0-3.D0*(5.D0*DCT2-1.D0)/
     1 (35.D0*DCT2*DCT2-30.D0*DCT2+3.D0)))*DCAZ**2+
     2 (DHLAT(8)-4.D0*DLLAT(8)*(1.D0+3.D0*(5.D0*DCT2-1.D0)/
     3 (35.D0*DCT2*DCT2-30.D0*DCT2+3.D0)))*DSAZ**2
      DGY(8)=0.D0
      DGX(9)=  (DHLAT(9)-2.D0*DLLAT(9)*(8.D0-3.D0/(7.D0*DCT2-3.D0)))*
     1 DCAZ**2+(DHLAT(9)-2.D0*DLLAT(9)*(2.D0+3.D0/(7.D0*DCT2-3.D0)))*
     2 DSAZ**2
      DGY(9)=DLLAT(9)*3.D0/DCT*(1.D0+2.D0/(7.D0*DCT2-3.D0))*DSAZ2
      DGX(10)=(DHLAT(10)-4.D0*DLLAT(10)*(4.D0+3.D0*DCT2/
     1 (7.D0*DCT2**2-8.D0*DCT2+1.D0)))*DCAZ**2
     2       +(DHLAT(10)-4.D0*DLLAT(10)*(1.D0-3.D0*DCT2/
     2 (7.D0*DCT2**2-8.D0*DCT2+1.D0)))*DSAZ**2
      DGY(10)=-DLLAT(10)*6.D0*DCT/DST**2*(1.D0-4.D0/(7.D0*DCT2-1.D0))*
     1 DSAZ2
      DGX(11)=(DHLAT(11)-2.D0*DLLAT(11)*(8.D0-3.D0/DST2))*DCAZ**2
     1       +(DHLAT(11)-2.D0*DLLAT(11)*(2.D0+3.D0/DST2))*DSAZ**2
      DGY(11)= DLLAT(11)*3.D0/DCT*(3.D0-2.D0/DST2)*DSAZ2
      DGX(12)=(DHLAT(12)-4.D0*DLLAT(12)*(4.D0-3.D0/DST2))*DCAZ**2
     1       +(DHLAT(12)-4.D0*DLLAT(12)*(1.D0+3.D0/DST2))*DSAZ**2
      DGY(12)= DLLAT(12)*12.D0*DCT/DST2*DSAZ2
      DO 710 I=1,12
      DGK(I)=DGK(I)*DSQRT(DGX(I)**2+DGY(I)**2)*DFAK
  710 DPK(I)=DPK(I)+DATAN2(DGY(I),DGX(I))*DRO
      GOTO 2000
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     IC=6, compute geodetic coefficients for areal strain             C
C           in 10**-9 units = nstr.                                    C
C           We use a spherical approximation for the aereal strain,    C
C           i.e. eps(t,t) + eps(l,l), (see ZUERN and WILHELM 1984,     C
C           p. 282).                                                   C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
  800 CONTINUE
      DO 810 I=1,3
  810 DGK(I)=DGK(I)*(2.D0*DHLAT(I)-2.D0*3.D0*DLLAT(I))/(DGRAV*DR1)*1.D9
      DO 820 I=4,7
  820 DGK(I)=DGK(I)*(2.D0*DHLAT(I)-3.D0*4.D0*DLLAT(I))/(DGRAV*DR1)*1.D9
      DO 830 I=8,12
  830 DGK(I)=DGK(I)*(2.D0*DHLAT(I)-4.D0*5.D0*DLLAT(I))/(DGRAV*DR1)*1.D9
      DO 840 I=1,12
  840 DPK(I)=0.0D0
      GOTO 2000
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     IC=7, compute geodetic coefficients for shear tidal strain       C
C           at the Earth's deformed surface in 10**-9 units = nstr.    C
C           We use a spherical approximation, i.e. eps(t,l)            C
C     Attention: this component has never been tested !!!!             C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
  900 CONTINUE
      DTHETA=(90.D0-DPSI)*DRAD
      DAZR=(DAZ+180.D0)*DRAD
      DCAZ =DCOS(DAZR)
      DSAZ =DSIN(DAZR)
      DSAZ2=DSIN(2.D0*DAZR)
      DCSTS=-0.5D0*DSIN(2.D0*DAZR)
      DCT=DSPSI
      DST=DCPSI
      DCT2=DCT*DCT
      DST2=DST*DST
      DCC2=DCOS(2.D0*DPSI*DRAD)
      DC2T=-DCC2
      DCOTT =1.D0/DTAN(DTHETA)
      DCOTT2=1.D0/DTAN(2.D0*DTHETA)
      DFAK=1.D9/(DR1*DGRAV)
      DGY(1)=0.D0
      DGY(2)=2.D0*DLLAT(2)*(2.D0*DCOTT2-DCOTT)*DCSTS/DST
      DGY(3)=4.D0*DLLAT(3)*DCOTT*DCSTS/DST
      DGY(4)=0.D0
      DGY(5)=-20.D0*DLLAT(5)*DCT*DCSTS/(5.D0*DCT2-1.D0)
      DGY(6)=-4.D0*DLLAT(6)*(DCOTT-1.D0/DCOTT)*DCSTS/DST
      DGY(7)=12.D0*DLLAT(7)*DCOTT*DCSTS/DST
      DGY(8)=0.D0
      DGY(9)=DLLAT(9)*3.D0/DCT*(1.D0+2.D0/(7.D0*DCT2-3.D0))*DSAZ2
      DGY(10)=-DLLAT(10)*6.D0*DCT/DST**2*(1.D0-4.D0/(7.D0*DCT2-1.D0))*
     1 DSAZ2
      DGY(11)=DLLAT(11)*3.D0/DCT*(3.D0-2.D0/DST2)*DSAZ2
      DGY(12)=DLLAT(12)*12.D0*DCT/DST2*DSAZ2
      DO 910 I=1,12
      DGK(I)=DGK(I)*DGY(I)*DFAK
  910 DPK(I)=0.D0
C      WRITE(IUN6,*) ' ***** The shear strain has never been tested !!!'
      WRITE(*,*)    ' ***** The shear strain has never been tested !!!'
      GOTO 2000
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     IC=8, compute geodetic coefficients for volume strain            C
C           at the Earth's deformed surface in 10**-9 units = nstr.    C
C           We use a spherical approximation, i.e. eps(t,t)+eps(l,l).  C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 1000 CONTINUE
      DPOISS=0.25D0
      DFAK=1.D9*(1.D0-2.D0*DPOISS)/(1.D0-DPOISS)
      DO 1010 I=1,3
 1010 DGK(I)=DGK(I)*DFAK*(2.D0*DHLAT(I)-2.D0*3.D0*DLLAT(I))/(DGRAV*DR1)
      DO 1020 I=4,7
 1020 DGK(I)=DGK(I)*DFAK*(2.D0*DHLAT(I)-3.D0*4.D0*DLLAT(I))/(DGRAV*DR1)
      DO 1030 I=8,12
 1030 DGK(I)=DGK(I)*DFAK*(2.D0*DHLAT(I)-4.D0*5.D0*DLLAT(I))/(DGRAV*DR1)
      DO 1040 I=1,12
 1040 DPK(I)=0.0D0
      GOTO 2000
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     IC=9, compute geodetic coefficients for ocean tides in mm:       C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 1100 CONTINUE
      DFAK=1.D3/DGRAV
      DO 1110 I=1,12
      DGK(I)=DGK(I)*DFAK
 1110 DPK(I)=0.0D0
 2000 CONTINUE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Common loop for all components.                                  C
C     For negative geodetic coefficients, use absolute value           C
C     and add 180 deg to the phase.                                    C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DO 2010 I=1,12
      IF(DGK(I).GE.0.D0) GOTO 2010
      DGK(I)=-DGK(I)
      DPK(I)=DPK(I)+180.D0
      IF(DPK(I).LT.0.D0) DPK(I)=DPK(I)+360.D0
 2010 CONTINUE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Print geodetic coefficients:                                     C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IF(IPRINT.EQ.0) RETURN
C      WRITE(IUN6,7003) IC,DAZ,(DGK(I),CUNIT(IC2),DPK(I),I=1,12)
      WRITE(*,7003) IC,DAZ,(DGK(I),CUNIT(IC2),DPK(I),I=1,12)
 5000 CONTINUE
C      IF(IPRINT.GT.0) WRITE(IUN6,7005)
      IF(IPRINT.GT.0) WRITE(*,7005)
      RETURN
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Format statements:                                               C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 7000 FORMAT(' Routine ETGCOF, version 930706 FTN 77.'//
     1' Computation of geodetic coefficients'//
     3' Parameters of Geodetic Reference System 1980:'/
     4' Major semi axis                  ',F12.0,'  m'/
     5' 1. excentricity                  ',F12.8/
     6' Geocentric gravitational constant',F12.1,' 10**9 m**3/s**2'/)
 7001 FORMAT(' Station paremeters:'//
     1' Latitude                       ',F12.6,' deg'/
     2' Geocentric latitude            ',F12.6,' deg'/
     3' Longitude                      ',F12.6,' deg'/
     4' Height                         ',F12.3,' m'/
     5' Gravity                        ',F12.6,' m/s**2'/
     6' Geocentric radius              ',F12.3,' m'/
     7' Component of observations      ',I12/
     8' Azimuth from north direction   ',F12.6,' deg'//)
 7002 FORMAT(' Astronomic constants International Astronomic Union',
     1' 1984 m.kg.s-system'/
     2' Moons mean sine parallax  ',F12.6/
     3' Mass relation Earth/Moon  ',F12.6//
     4' DOODSONs constant         ',F12.6,' m**2/s**2'/
     6' F                         ',F12.6,' mas/(nm/s**2)'//)
 7003 FORMAT(//' Geodetic coefficients and phases for component',I4/
     1' azimuth:',F12.6,' degree'//
     2' GC 2,0',F14.5,2X,A8,2X,F14.6,' deg'/
     3' GC 2,1',F14.5,2X,A8,2X,F14.6,' deg'/
     4' GC 2,2',F14.5,2X,A8,2X,F14.6,' deg'/
     5' GC 3,0',F14.5,2X,A8,2X,F14.6,' deg'/
     6' GC 3,1',F14.5,2X,A8,2X,F14.6,' deg'/
     7' GC 3,2',F14.5,2X,A8,2X,F14.6,' deg'/
     8' GC 3,3',F14.5,2X,A8,2X,F14.6,' deg'/
     9' GC 4,0',F14.5,2X,A8,2X,F14.6,' deg'/
     *' GC 4,1',F14.5,2X,A8,2X,F14.6,' deg'/
     1' GC 4,2',F14.5,2X,A8,2X,F14.6,' deg'/
     2' GC 4,3',F14.5,2X,A8,2X,F14.6,' deg'/
     3' GC 4,4',F14.5,2X,A8,2X,F14.6,' deg'//)
 7005 FORMAT(///' ***** Routine ETGCOF finished the execution.'/)
      END

