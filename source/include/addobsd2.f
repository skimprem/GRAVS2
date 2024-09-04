      subroutine addobsd2(c, ndia, n, b, ib, w)
      implicit double precision(a-h,o-z)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c                            A D D O B S D 2
c
c  subroutine adds one observation to normal equations
c
c  parameters:
c
c  c   normal equation matrix
c
c  ndia  length of initial diagonal
c  n   number of unknowns
c
c  b   observation equation coefficients which are non-zero.
c      right hand side (n+1) must be specified as last value.
c
c  ib  column numbers of coefficients in b, terminated by n+1
c      only the first value can be in the range 1 .. ndia
c
c  w   weight for specific observation
c
c  (c) rf/kms, 1986, converted to fortran june 1990
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      dimension c(*), b(*), ib(*)
      i = 0
c
c  i-loop
c
10    i = i+1
!      write(*,*) 'test addobsd ib,b = ',ib(i),b(i)
      ir = ib(i)
      if (i.gt.1.and.ir.le.ndia) stop '*** obs eqn error ***'
      do 20 j = 1, i
        is = ib(j)
        if (ir.le.ndia) then
          ipos = ir
        else
          ipos = (ir*(ir-1)+ndia*(1-ndia))/2 + is
        endif
        c(ipos) = c(ipos) + b(i)*b(j)*(w**2)
!	write (*,*) 'i,j,ir,is,ipos=',i,j,ir,is,ipos,c(ipos)
20    continue
      if (ir.le.n) goto 10
c
      return
      end
      
