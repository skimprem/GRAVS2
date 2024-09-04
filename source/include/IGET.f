Ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c                     i g e t
c  reads integer from array c from position 'ipos',
c  terminated by non-number
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      integer function iget(ch,ipos)
      parameter (n=95)
      character ch*(n)
      character*1 c
c
      j = ipos-1
      isig = 1
c
10    j = j+1
      if (j.gt.n) then
        write(*,*) ch(1:n)
        stop '*** getint: no integer in line'
      endif
      c = ch(j:j)
      if (c.eq.' ') goto 10
      if (c.eq.'-') then
        isig = -1
        goto 10
      endif
      if (c.lt.'0'.or.c.gt.'9') then
        write(*,*) ch(1:n)
        stop '*** getint: wrong character in front of integer'
      endif
c
      i = ichar(c)-48
20    j = j+1
      c = ch(j:j)
      if (j.ge.n.or.c.lt.'0'.or.c.gt.'9') goto 30
      i = i*10 + ichar(c)-48
      goto 20
c
30    iget = isig*i
      return
      end

