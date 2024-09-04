CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C***********************************************************************
C*  NAME OF THE PROGRAM                                                *
C*       POL       COMPUTES THE EFFECT OF THE POLAR MOTION ON THE      *
C*                 MEASURED GRAVITY                                    *
C*       FINNISH GEODETIC INSTITUTE                                    *
C*       1977/HEIKKINEN                                                *
C*                                                                     *
C*  ARGUMENTS                                                          *
C*       SLA       GEODETIC LATITUDE OF THE COMPUTATION POINT, DEGREES *
C*       SLO       LONGITUDE OF THE COMPUTATION POINT, DEGREES         *
C*       SH        HEIGHT OF THE COMPUTATION POINT (ABOVE ELLIPSOID), M*
C*       SPLA      GEOCENTRIC COLATITUDE OF THE POLE, SECONDS          *
C*       SPLO      LONGITUDE OF THE POLE, DEGREES                      *
C*       PVER      VERTICAL COMPONENT OF THE OUTPUT, MICROGALS         *
C*       POS       NORTH-SOUTH     "                                   *
C*       PEW       EAST-WEST       "                                   *
C*                                                                     *
C*  REFERENCED SUBROUTINES                                             *
C*       DATAN     ANSI STANDARD BASIC EXTERNAL FUNCTION               *
C*       DSIN      ANSI STANDARD BASIC EXTERNAL FUNCTION               *
C*       DCOS      ANSI STANDARD BASIC EXTERNAL FUNCTION               *
C*       DSQRT     ANSI STANDARD BASIC EXTERNAL FUNCTION               *
C*                                                                     *
C*  VARIABLES                                                          *
C*       P         P=3.14...                                           *
C*       CD        CONVERSION FACTOR FROM DEGREES TO RADIANS           *
C*       AE        EQUATORIAL RADIUS OF THE EARTH                      *
C*       E2        SQUARE OF THE EXCENTRICITY                          *
C*       OLA       SLA IN RADIANS                                      *
C*       GLA       GEOCENTRIC LATITUDE, RADIANS                        *
C*       R         GEOCENTRIC RADIUS OF THE COMPUTATION PLACE, METERS  *
C*       SA        SINE OF AN AUXILIARY ANGLE                          *
C*       AL        THE ABOVE ANGLE ITSELF, RADIANS                     *
C*       XO        X-COMPONENT OF THE RADIUS VECTOR OF THE COMPUTATION *
C*                 PLACE, METERS                                       *
C*       YO        Y-     "                                            *
C*       ZO        Z-     "                                            *
C*       PLA       GEOCENTRIC LATITUDE OF THE POLE, RADIANS            *
C*       PLO       LONGITUDE OF THE POLE FROM THE MERIDIAN OF THE      *
C*                 COMPUTATION PLACE, RADIANS                          *
C*       XP        X-COMPONENT OF THE UNIT VECTOR IN THE DIRECTION OF  *
C*                 THE POLE. (LATER OF THE PROJECTION OF THE           *
C*                 RADIUS VECTOR (XO,YO,ZO))                           *
C*       YP        Y-     "                                            *
C*       ZP        Z-     "                                            *
C*       T         THE LENGTH OF THE PROJECTION OF (XO,YO,ZO) ON       *
C*                 (XP,YP,ZP), METERS                                  *
C*       DX        X-COMPONENT OF THE DIRECT DISTANCE TO THE           *
C*                 ROTATION AXIS, METERS                               *
C*       DY        Y-     "                                            *
C*       DZ        Z-     "                                            *
C*       W         RATE OF ROTATION OF THE EARTH, 1/S                  *
C*       W2        W*W                                                 *
C*       FX        X-COMPONENT OF THE POLAR EFFECT, M/S/S              *
C*       FY        Y-     "                                            *
C*       FZ        Z-     "                                            *
C***********************************************************************
      SUBROUTINE POL(SLA,SLO,SH,SPLA,SPLO,PVER,POS,PEW)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      P=4.D0*DATAN(1.D0)
      CD=P/180.D0
C****
C**** RADIUS VECTOR TO THE COMPUTATION POINT
C****
C GRS-80 eq.radius and sq.excentricity in April 2004 by T.Oja
C      AE=6378160.D0
      AE=6378137.0D0
C      E2=0.00669460532856D0
      E2=6.69438002290D-3
      OLA=SLA*CD
      GLA=DATAN((1.D0-E2)*DSIN(OLA)/DCOS(OLA))
      R=AE/DSQRT(1.D0+E2/(1.D0-E2)*DSIN(GLA)*DSIN(GLA))
      R=DSQRT(R*R+SH*SH+2.D0*R*SH*DCOS(OLA-GLA))
      SA=SH/R*DSIN(OLA-GLA)
      AL=DATAN(SA/DSQRT(1.D0-SA*SA))
      GLA=GLA+AL
      PLA=(90.D0*3600.D0-SPLA)*CD/3600.D0
      XO=R*DCOS(GLA)
      YO=0.D0
      ZO=R*DSIN(GLA)
      PLO=(SPLO-SLO)*CD
C****
C**** UNIT VECTOR TO THE POLE
C****
      XP=DCOS(PLA)*DCOS(PLO)
      YP=DCOS(PLA)*DSIN(PLO)
      ZP=DSIN(PLA)
      T=XO*XP+ZO*ZP
      XP=T*XP
      YP=T*YP
      ZP=T*ZP
C****
C**** DIRECT DISTANCE TO THE AXIS
C****
      DX=XO-XP
      DY=YO-YP
      DZ=ZO-ZP
      W=7.2921151467D-5
      W2=W*W
C****
C**** CENTRIFUGAL FORCE RELATIVE TO THE NORMAL STATE (SYMMETRY AXIS)
C****
      FX=W2*(DX-XO)
      FY=W2*DY
      FZ=W2*DZ
C****
C**** VERTICAL AND HORIZONTAL COMPONENTS IN MICROGALS
C****
      PVER=(FX*DCOS(OLA)+FZ*DSIN(OLA))*1.D8
      POS=(-FX*DSIN(OLA)+FZ*DCOS(OLA))*1.D8
      PEW=FY*1.D8
      RETURN
      END
