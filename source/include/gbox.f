      double precision FUNCTION gbox(x0,y0,z0,x1,y1,z1,x2,y2,z2,rho)
C      subroutine gbox(x0,y0,z0,x1,y1,z1,x2,y2,z2,rho,g)
c
c  Subroutine GBOX computes the vertical attraction of a 
c  rectangular prism.  Sides of prism are parallel to x,y,z axes,
c  and z axis is vertical down.  
c
c  Input parameters:
c    Observation point is (x0,y0,z0).  The prism extends from x1
c    to x2, from y1 to y2, and from z1 to z2 in the x, y, and z 
c    directions, respectively.  Density of prism is rho.  All 
c    distance parameters in units of m; rho in units of
c    kg/(m**3). 
c
c  Output parameters:
c    Vertical attraction of gravity, g, in uGal.
c
c  Reference: "Potential theory in gravity and magnetic applications"
c  Richard J. Blakely, Cambridge University Press, 1996
c
c  Modified by T.Oja, 2007-12-12 (km -> m, Double Prec, ...)
c                     2009-07-17 subr.->function, ...)
c
      implicit double precision (a-h,o-z)
CCC  integer i,j,k,l,m,n
C      real km2m
      dimension x(2),y(2),z(2),isign(2)
      data isign/-1,1/,gamma/6.67428e-11/,twopi/6.28318530717959/,
     1 si2cgs/1.e8/
C     1     si2mg/1.e5/
C     ,km2m/1.e3/
      x(1)=x0-x1
      y(1)=y0-y1
      z(1)=z0-z1
      x(2)=x0-x2
      y(2)=y0-y2
      z(2)=z0-z2
      sum=0.d0
      do 100 i=1,2
         do 100 j=1,2
            do 100 k=1,2
               rijk=dsqrt(x(i)**2+y(j)**2+z(k)**2)
               ijk=isign(i)*isign(j)*isign(k)
               arg1=datan2((x(i)*y(j)),(z(k)*rijk))
               if(arg1.lt.0.)arg1=arg1+twopi
               arg2=rijk+y(j)
               arg3=rijk+x(i)
               if(arg2.le.0.) stop 'GBOX(arg2):  Bad field point'
               if(arg3.le.0.) stop 'GBOX(arg3):  Bad field point'
               arg2=dlog(arg2)
               arg3=dlog(arg3)
               sum=sum+ijk*(z(k)*arg1-x(i)*arg2-y(j)*arg3)
  100          continue
      gbox=rho*gamma*sum*si2cgs
C      g=rho*gamma*sum*si2mg
C*km2m
      return
      end

