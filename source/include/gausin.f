      double precision FUNCTION GAUSIN(P)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Computes a "Normal Deviate" (Inverse of Normal Frequency Function)
C which is actually the cumulative distribution function (cdf), see
C http://en.wikipedia.org/wiki/Normal_distribution#Cumulative_distribution_function
C P should be in (0,1)! See also FREQ
C     Based on G.W. Hill & A.W. Davis, Algorithm 442 Normal Deviate
C     Collected Algorithms from CACM
C     (CERNLIB, wwwasdoc.web.cern.ch/wwwasdoc/cernlib.html)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      implicit double precision (a-h,o-z)
      PARAMETER (C = 2.50662 827)
C
      IF(P .LE. 0.d0 .OR. P .GE. 1.d0) THEN
       H=0.d0
       WRITE(*,101) P
      ELSEIF(P .EQ. 0.5d0) THEN
       H=0.d0
      ELSE
       X=P
       IF(P .GT. 0.5d0) X=1-P
       X=DSQRT(-2.d0*DLOG(X))
       X=X-((7.45551*X+450.636)*X+1271.059)/
     1     (((X+110.4212)*X+750.365)*X+500.756)
       IF(P .LT. 0.5d0) X=-X
       H=C*(P-FREQ(X))*DEXP(0.5d0*X**2)+X
      ENDIF
      GAUSIN=H
      RETURN
  101 FORMAT('ARGUMENT P =',1P,E15.5,' NOT IN RANGE')
      END
C
C      include 'freq.f'

