cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c                           C H O L D
c  
c  subroutine for solving a set of normal equations where the first
c  'ndia' rows are diagonal. the system is solved by choleskys method.
c  the equations are stored columnwise, upper diagonal only, starting
c  with the first 'ndia' diagonal elements only
c
c  
c  parameters:
c  
c  c       array. at call coefficients and right-hand side. On return c con-
c          tains the reduced matrix. normal equations may be build up
c          efficiently with subroutine 'addobsd'. the size of the array
c          must be at least (n*(n+1) - ndia*(ndia-1))/2
c  
c  n       total number of unknowns.
c          if n < 0 is specified an earlier solution
c          is utilized with a new right-hand side.
c
c  ndia    length of initial diagonal.
c
c  nsing   number of singularities, = 0 for ok solution
c
c  cifmax  the actual maximal loss of digits encountered.
c
c  lsing   logical array signalling singular rows.
c
c  (c) rf/kms, june 1990. conversion of algol procedure 'grnll'
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine chold(c, n, ndia, nsing, cifmax, lsing)
      implicit double precision (a-h,o-z)
      dimension c(*)
      logical lsing(*)
      integer row, col, p, i, ir, ic
      logical findl
c
        cifmax = 0
        nsing = 0
        findl = (n.gt.0)
        n = abs(n)
c
        icol1 = 1
        if (.not.findl) icol1 = n+1
c  
        do 50 row = icol1, n+1
          ir = indx(row,ndia)  
          i = ir
          irow1 = 1
          if (row.le.ndia) irow1 = row
          do 40 col = irow1, row
            sum = 0
            ic = indx(col,ndia)
            i = i+1
            if (col.gt.ndia) then
              do 30 p = 1,col-1
30            sum = sum + c(ir+p)*c(ic+p)
            endif 
c
            ci = c(i)
            if (row.ne.col) then
              c(i)= (ci - sum)/c(indx(col+1,ndia))
            elseif (row.le.n) then
              ci1 = ci - sum
              cif = 0
              if (ci1.eq.0.or.ci.eq.0) then
                cif = 999.d0
              else
                cif = 0.434*dlog(abs(ci/ci1))
              endif
              if (cif.gt.cifmax) cifmax = cif
              if (ci1.le.0) then
                nsing = nsing+1
                lsing(row) = .true.
                c(i) = 9.9d33
              else
                lsing(row) = .false.
                c(i) = sqrt(ci1)
              endif
            endif
40        continue
50      continue
c
c  back substitution
c
        do 60 col = n, 1, -1
          i = i-1
          ir = i
          ic = indx(col+1,ndia)
          c(i) = c(i)/c(ic)
          if (col.le.ndia) goto 60
          do 58 p = col-1, 1, -1
            ir = ir-1
            ic = ic-1
            c(ir) = c(ir) - c(i)*c(ic)
58        continue
60      continue
        return
        end

