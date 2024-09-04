      double precision FUNCTION silinder(z1,z2,D,rho)
C      subroutine Silinder(z0,r,h,rho,g)
c
c  Function Silinder models the vertical gravity attraction along the
c  axis of cylinder (its axis and vertical z coincide).
c
c  Input parameters:
c    Observation point (from the top surface) is z1 (x,y are 0s).
c    The cylinder with diameter D extends from z1(top) to z2(bottom).
c    Density of prism is rho.
c    All distance parameters in units of m; rho in units of kg/(m**3).
c
c  Output parameters:
c    Vertical attraction of gravity, g, in µGal.
c
c  Equation ref: Metrologia Vol 3 No 3 1995 pp. 191
c
c  Created by T.Oja, 2009-03-05
c  Modified by T.Oja, 2009-07-29 subr.->function, mGal->µGal ...)

      implicit double precision (a-h,o-z)
CCC  integer i,j,k,l,m,n
      data gamma/6.67428e-11/,twopi/6.28318530717959/,si2cgs/1.e8/
      h=z2-z1
      R=D/2.d0
      sum=h+dsqrt(z1**2+R**2)-dsqrt((z1+h)**2+R**2)
      silinder=rho*twopi*gamma*sum*si2cgs
      return
      end
