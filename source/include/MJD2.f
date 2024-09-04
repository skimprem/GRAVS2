      subroutine mjd2(y,month,day,ih,im,is,mdjul,yr)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  Subroutine computes the Modified Julian Date (MJD) or time interval
C  in units of days from A.D. 1858 November 17 at 00.00 to the epoch
C  given by the parameter which must be a GI-standard date (UT or UTC).
C  iday is given as an integer in form '130591', meaning May 13, 1991.
C  time is a real giving hr and min (e.g. 17.22), and possible minute
C  decimals.
C  !!!! Subroutine works only between 1. Jan 1950 and 31. Dec 2049 !!!!
C  Output:
C   mdjul - Modified Julian Date (double precision)
C   yr - year in decimal (double precision)
C
C  In case Julian Date is necessary then add constant value
C  to the output of mjd
C                       mdjul + 2400000.5
C  If starting epoch A.D. 1900 January 0.5 (1. Jan 12.00) is
C  required then
C                       mdjul - 15019.5
C
C  Subroutine is based on the KMS function epoch1900 originally
C  developed by W. L. Weng (Oct 1976) and R. Forsberg (Oct 1991)
C  Substantially modified by T.Oja (first 210304, last 2008-10-19)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      implicit none
      double precision mdjul, yr
C time, m,
C      double precision yr
      integer y,year,month,day,ih,im,is,i,j,ii(12),days
      DATA ii/0,31,59,90,120,151,181,212,243,273,304,334/
C
C      year  = mod(iday,100)
      year = y-1900
C  y2k fix
      if (year.lt.50.or.year.ge.150) goto 5000
C       year = year+100
C      month = mod(iday/100,100)
C      day   = iday/10000
C      h = time
C      m = (time-h)*100
C
      if (month.le.0.or.month.gt.12.or.day.le.0.or.day.gt.31.or.ih.ge.
     .24.or.im.ge.60.or.is.ge.60) goto 5000
C Modified Julian Date computation
      i = 0
      if (year.gt.3.and.month.lt.3) i = 1
      days = year*365 + (year - i)/4 + ii(month) + day
      mdjul = days + ih/24.d0 + im/1440.d0 + is/86400.d0 + 15019.d0
      j = 0
      i = 0
C If leap year
      if (year/4.eq.year/4.d0) j = 1
      if (month.gt.2) i = j
C Decimal year computation
      yr =
     .y+(ii(month)+day+i+ih/24.d0+im/1440.d0+is/86400.d0)/(365.d0+j)
      return
5000  write(*,*) '*** MDJ error with date:',y,month,day,ih,im,is
      stop '!!! Subroutine MJD - illegal date spec!'
      end

