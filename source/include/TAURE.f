      double precision function TAURE(NT,NU,ALPH)
C      SUBROUTINE TAURE(NT,NU,ALPH,CRTAU)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Subroutine to compute crtical tau value based on Pope (1976)
C  ALPH - significance level of one-tile (A) or (1-A) !?
C  NT - number of observations (residuals)
C  NU - degree of freedom
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        PI=4.D0*DATAN(1.D0)
        PD=2.D0/PI
        S=1.D0
        WNU=NU
        U=WNU-1.D0
        IF (U.EQ.0.D0) GOTO 72
        IF (ALPH.EQ.0.D0) GOTO 72
        IF (ALPH.LT.1.D0) GOTO 10
C        CRTAU=0.D0 !?
         TAURE=0.D0
         WRITE(*,*)'TAURE ERROR!'
        RETURN
10      Q=NT
        IF (ALPH.GT..5D0) GOTO 19
        AA=ALPH/Q
        DELT=AA
        DO 18 I=1,100
        XI=I
        DELT=DELT*ALPH*((XI*Q-1.D0)/((XI+1.D0)*Q))
        IF (DELT.LT.1.D-14) GOTO 20
18      AA=AA+DELT
19      AA=1.D0-(1.D0-ALPH)**(1.D0/Q)
20      P=1.D0-AA
        IF (U.EQ.1.D0) GOTO 71
        F=1.3862943611199D0-2.D0*DLOG(AA)
        G=DSQRT(F)
        X=G-(2.515517D0+.802853D0*G+.010328*F)/(1.D0+1.432788D0*G+
     .F*(.189269D0+.001308*G))
        Y=X**2
        A=X*(1.D0+Y)/4.D0
        B=X*(3.D0+Y*(16.D0+5.D0*Y))/96.D0
        C=X*(-15.D0+Y*(17.D0+Y*(19.D0+3.D0*Y)))/384.D0
        E=X*(-945.D0+Y*(-1920.D0+Y*(1482.D0+Y*(776.D0+79.D0*Y))))/
     .92160.D0
        V=1.D0/U
        T=X+V*(A+V*(B+V*(C+E*V)))
        S=T/DSQRT(U+T**2)
        UM=U-1.D0
        DELL=1.D0
        DO 70 M=1,50
        H=1.D0-S**2
        R=0.D0
        IF (DMOD(U,2.D0).EQ.0.D0) GOTO 49
        DD=DSQRT(H)
        N=.5D0*UM
        DO 45 I=1,N
        Z=2*I
        R=R+DD
        D=DD
45      DD=DD*H*(Z/(Z+1.D0))
        R=PD*(R*S+DASIN(S))
        D=PD*D*UM
        GOTO 61
49      DD=1.D0
        N=.5D0*U
        DO 55 I=1,N
        Z=2*I
        R=R+DD
        D=DD
55      DD=DD*H*((Z-1.D0)/Z)
        R=R*S
        D=D*UM
61      CONTINUE
        DEL=(P-R)/D
        IF (DABS(DEL).LT.1.D-8) GOTO 72
70      CONTINUE
        GOTO 72
71      S=DSIN(P/PD)
72      TAURE=S*DSQRT(WNU)
C72      CRTAU=S*DSQRT(WNU)
        RETURN
        END

