      double precision FUNCTION FREQ(X)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Normal Frequency Function
C See also GAUSIN
C (CERNLIB, wwwasdoc.web.cern.ch/wwwasdoc/cernlib.html)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      implicit double precision (a-h,o-z)
      DIMENSION P2(0:4),Q2(0:4)
      PARAMETER(Z1 = 1.d0, HF = Z1/2.d0)
      PARAMETER(C1 = 0.56418 958, W2 = 1.41421 356, RW2 = 1.d0/W2)
      DATA P10,Q10,P11 /+3.67678 77, +3.25845 93, -9.79704 65E-2/
      DATA (P2(I),Q2(I),I=0,4)
     +/+7.37388 83E+0, +7.37396 09E+0, +6.86501 85E+0, +1.51849 08E+1,
     1 +3.03179 93E+0, +1.27955 30E+1, +5.63169 62E-1, +5.35421 68E+0,
     2 +4.31877 87E-5, +1.d0/
      DATA P30,Q30,P31 /-1.24368 54E-1, +4.40917 06E-1, -9.68210 36E-2/
C
      V=RW2*DABS(X)
      IF(V .LT. HF) THEN
       Y=V**2
       H=V*(P10+P11*Y)/(Q10+Y)
       HC=1-H
      ELSEIF(V .LT. 4.d0) THEN
       AP=P2(4)
       AQ=Q2(4)
       DO 2 I = 3,0,-1
       AP=P2(I)+V*AP
    2  AQ=Q2(I)+V*AQ
       HC=DEXP(-V**2)*AP/AQ
       H=1-HC
      ELSE
       Y=1/V**2
       HC=DEXP(-V**2)*(C1+Y*(P30+P31*Y)/(Q30+Y))/V
       H=1-HC
      ENDIF
      IF(X .GT. 0.d0) THEN
       FREQ=HF+HF*H
      ELSE
       FREQ=HF*HC
      ENDIF
      RETURN
      END

