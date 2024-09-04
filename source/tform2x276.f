      program tform2x276
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C   With that program you can convert GRREDU2.x obs file (where 0<=x<5)
C   into GRREDU2.76 obs file with new time format
C   T.Oja 2008-11-09
C   Modif. 2019-06-20
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      implicit double precision(a-h,o-z)
      character*255 hdr,ifile
      logical lffb
      data idi/10/,lffb/.false./
C
      read(*,'(a)') ifile
C      write(*,*) ifile

      open(idi,file=ifile,form='formatted',status='old',IOSTAT=KODE)
      if(KODE.ne.0) STOP'!!! Reduced observation file not found!!! '

80    read(idi,'(a)',end=150) hdr
      if (hdr(1:8).eq.'            '.or.hdr(1:1).eq.'#') then
       write(*,535) hdr
535    format(a80)
      else
       backspace(idi)
       read(idi,*,IOSTAT=KODE) ino,iday,t,rdg,pres,ih
C       write(*,*) ino,iday,t,rdg,std,dh,pres,tempc,tempo,ihum
       if(KODE.ne.0) then
        lffb=.true.
        backspace(idi)
        read(idi,*,IOSTAT=KODE) ino,iday,t,rdg,ffb,pres,ih
       endif
       call MJD(iday,t,dmodjd,yr)
       call PETGREN3(dmodjd+2400000.5D0,ITY,ITM,ITD,ITH,ITMIN,ITS,NERR)
C
       if(.not.lffb) then
        write(*,1160) ino,ITY,ITM,ITD,ITH,ITMIN,ITS,rdg,-9.9999,ih,pres
       else
        write(*,1170) ino,ITY,ITM,ITD,ITH,ITMIN,ITS,rdg,ffb,-9.9999,ih,
     1  pres
       endif
1160  format
     1(i8,i6,2('-',i2.2),i4.2,2(':',i2.2),f12.4,f8.4,i6,f9.1)
1170  format
     1(i8,i6,2('-',i2.2),i4.2,2(':',i2.2),f12.4,f7.1,f8.4,i6,f9.1)
      endif
      goto 80
150   continue
      stop 'TFORM2x276 terminated OK'
      end
C Subroutine MJD computes the Modified Julian Date (MJD)
      include 'd:/PROGS/include/MJD.f'
      include 'd:/PROGS/include/PETGREN3.f'

