cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c                    L I S T N O
c
c  subroutine for finding station number in unsorted station list
c  'idx' is zero if 'istat' is not in array 'ia' of 'n' elements
c  search begins at index from previous call
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      integer function listno(ia, n, istat)
      dimension ia(*)
      save idx
      if (n.eq.0) goto 30
      if (idx.le.0.or.idx.gt.n) idx = 1
      j = idx
10    if (ia(j).eq.istat) goto 20
      j = j+1
      if (j.gt.n) j = 1
      if (j.eq.idx) goto 30
      goto 10
20    idx = j
      listno = idx
      return
30    idx = 0
      listno = idx
      return
      end

