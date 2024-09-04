C GNU FORTRAN COMPILER g77 intrinsic function ETIME
C add next declaration line to the main program:
C      real etime,time,lap(2)
      time = ETIME(lap)
      write(*,'(/a,f6.2,a)') 'Program has used', time,
     .' seconds of CPU time!'
      write(*,'(a,f6.2,a,f6.2,a)') 'This includes',lap(1),
     .' seconds of user time and',lap(2),' seconds of system time.'

