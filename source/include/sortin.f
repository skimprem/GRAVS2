ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c                    S O R T I N
c
c  Subroutine sorts data in array d from 1 to n.
c  Result is given in integer array i, so that d(ix(k)), k = 1..n
c  is the sorted sequence.
c  The routine is fast for near-sorted data, slow otherwise.
c
c  (c) RF/KMS. Jan 1992
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine sortin(d,n,ix)
      integer d,dd
      dimension d(*),ix(*)
c
      ix(1) = 1
      do 20 i = 2, n
        dd = d(i)
        j = i-1
10      if (dd.lt.d(ix(j))) then
          ix(j+1) = ix(j)
        else
          ix(j+1) = i
          goto 20
        endif
        j = j-1
        if (j.gt.0) goto 10
        ix(1) = i
20    continue
      return
      end
