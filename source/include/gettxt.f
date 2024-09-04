ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c                        g e t t x t
c  subroutine read txt portion starting with letter in line,
c  terminates with 2 blanks
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine gettxt(ch,txt)
      parameter (len1=255,len2=12)
      character ch*(len1), txt*(len2)
      character*1 c,c1
c
      txt = '            '
      j = 0
10    j = j+1
      if (j.gt.len1) goto 30
      c = ch(j:j)
      if ('a'.le.c.and.c.le.'z'.or.'A'.le.c.and.c.le.'Z') goto 20
      goto 10
c
20    j0 = j
      jmax = j0+(len2+1)
21    j = j+1
      if (j.le.len1) then
        c1 = ch(j:j)
      else
        c1 = ' '
      endif
      if (c.eq.' '.and.c1.eq.' '.or.j.ge.jmax) goto 29
      c = c1
      goto 21
c
29    txt = ch(j0:j-2)
30    return
      end
      
