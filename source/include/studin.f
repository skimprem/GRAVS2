      double precision FUNCTION STUDIN(Q,N)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     THIS ROUTINE COMPUTES THE INVERSE OF THE DISTRIBUTION
C     FUNCTION OF THE STUDENT DISTRIBUTION WITH N DEGREES OF FREEDOM.
C     Q=1-A PROBABILITY FOR ONE-TAILED t-VALUE,
C     FOR TWO-TAILED t-VALUE INSERT Q=1-A/2 PROBABILITY
C     N=INTEGER GREATER THAN ZERO.
C     THE ROUTINE WAS WRITTEN BY G.W.HILL IN ALGOL
C     C.A.C.M. ALGORITHM 396
C     AT LEAST 6 SIGNIFICANT FIGURES ARE CORRECT.
C     (CERNLIB, wwwasdoc.web.cern.ch/wwwasdoc/cernlib.html)
C     Modified by T.Oja (23.03.04) - If N=3 then wrong result, now corrected!
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      implicit double precision (a-h,o-z)
      DATA HP/1.5707963268/
      IF(N. LT. 1) GO TO 10
      RL=1.d0
      IF(Q .GE. 0.5) GO TO 1
      RL=-1.d0
      P=2.d0*Q
      GO TO 2
    1 P=2.d0*(1.d0-Q)
    2 IF(P .LE. 0.d0 .OR. P .GT. 1.d0) GO TO 20
      IF(N .GT. 1) GO TO 3
      PP=DCOS(HP*P)
      STUDIN=PP/DSQRT(1.d0-PP*PP)*RL
      RETURN
    3 IF(N .GT. 2) GO TO 4
      STUDIN=DSQRT(2.d0/(P*(2.d0-P))-2.d0)*RL
      RETURN
    4 RN=N
      A=1.d0/(RN-0.5d0)
      B=48.d0/(A*A)
      C=((20700.d0*A/B-98.d0)*A-16.d0)*A+96.36
      D=((94.5d0/(B+C)-3.d0)/B+1.d0)*DSQRT(A*HP)*RN
      X=D*P
      Y=X**(2.d0/RN)
C      write(*,*)'1. Y, A+0.05:',Y,A+0.05
C If N=3 then goto 5 !!!
C      IF(Y .LE. 0.05+A) GO TO 5
      PP=0.5d0*P
      X=GAUSIN(PP)
      Y=X*X
C      write(*,*)'2. Y:',Y
      IF(N .GE. 5) C=C+0.3d0*(RN-4.5d0)*(X+0.6d0)
      C=(((0.05d0*D*X-5.d0)*X-7.d0)*X-2.d0)*X+B+C
      Y=(((((0.4d0*Y+6.3d0)*Y+36.d0)*Y+94.5d0)/C-Y-3.d0)/B+1.d0)*X
      Y=A*Y*Y
      IF(Y .LE. 0.002) Y=0.5d0*Y*Y+Y
      IF(Y .GT. 0.002) Y=DEXP(Y)-1.d0
C      write(*,*)'3. Y:',Y
C      GO TO 6
C Erroneus equation when N = 3
C    5 Y=dabs(((1./(((RN+6.)/(RN*Y)-0.089*D-0.822)*(RN+2.)*3.)+
C     .0.5/(RN+4.))*Y-11.)*(RN+1.)/(RN+2.)+1./Y)
C      write(*,*)'4. Y:',Y
C    6 ...
      STUDIN=DSQRT(RN*Y)*RL
      RETURN
   10 WRITE(*,7) N
   20 WRITE(*,8) Q
C      STOP
      RETURN
    7 FORMAT(/10X,'DEGREE OF FREEDOM N=',I5,' IN STUDIN ILLEGAL'/)
    8 FORMAT(/10X,'ARGUMENT  Q=',E15.5,' IN STUDIN ILLEGAL'/)
      END
C
C      include 'gausin.f'
