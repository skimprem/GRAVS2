CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c                    l i s t n o
c  subroutine for finding station number in station list
c  'idx' is zero if 'istat' is not in array 'ia' of 'n' elements
c  search begins at index 'idx', e.g. from previous call
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine listno(istat, ia, n, idx)
      dimension ia(*)
      if (n.le.0) then
        idx = 0
        return
      else
        if (idx.le.0.or.idx.gt.n) idx = 1
        j = idx
10      if (ia(j).eq.istat) goto 20
        j = j+1
        if (j.gt.n) j = 1
        if (j.eq.idx) goto 30
        goto 10
      endif
20    idx = j
      return
30    idx = 0
      return
      end

