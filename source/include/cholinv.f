      double precision function cholinv(c, n, ndia, inv)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c                           C H O L I N V
c
c  this subroutine obtains inverse element no. 'inv' from the inverse
c  matrix of the normal equation matrix c. c must be cholesky reduced,
c  i.e. chold must have been called.
c  the right-hand side is not used, but modified on exit
c  
c  (c) rf/kms oct 86, converted to fortran june 90
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit double precision (a-h,o-z)
      dimension c(*)
      integer p, col, c1, row
      logical diag
c
      diag = (inv.le.ndia)
      i = indx(n+1, ndia)
c
c  cholesky reduction of right-hand side  0, ..., 1, 0, ..
c
      if (diag) then
        c(i+inv) = 1.0d0/c(inv)
        row = ndia+1
      else
        c(i+inv) = 1.0d0/c(indx(inv+1,ndia))
        row = inv
      endif
c
      ir1 = row+1
      if (diag) ir1 = row
      do 20 col = ir1, n
        c1 = col-1
        ic = indx(col,ndia)
        if (diag) then
          sum = -c(i+inv)*c(ic+inv)
        else
          sum = 0
        endif
        do 18 p = row, c1
18      sum = sum - c(i+p)*c(ic+p)
        c(i+col) = sum/c(ic+col)
20    continue
c
c  back substitution
c
      do 30 col = n, row, -1
        ic = indx(col,ndia)
        rr = c(i+col)/c(ic+col)
        c(i+col) = rr
        do 28 p = col-1, row, -1
28      c(i+p) = c(i+p) - rr*c(ic+p)
        if (diag) c(i+inv) = c(i+inv) - rr*c(ic+inv)
30    continue
      if (diag) c(i+inv) = c(i+inv)/c(inv)
      cholinv = c(i+inv)
      return
      end

