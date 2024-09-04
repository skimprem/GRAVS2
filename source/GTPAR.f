      PROGRAM GTPAR
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C     GTPAR is based on the program WPAREX (version 1996.09.07         !
C     Fortran 90 by Hans-Georg Wenzel.)                                           !
C     The program extracts tidal parameters for individual stations    !
C     from world wide synthetic gravity tide parameter file            !
C     "wparm.dat".                                                     !
C                                                                      !
C     wparex.dat:   Formatted file on which the station coordinates    !
C                   have to be stored before the execution of program  !
C                   WPAREX.                                            !
C     wparex.prn:   Print file of program WPAREX.                      !
C     Last modification: 2004.04.02 by Tõnis Oja                       !
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IMPLICIT DOUBLE PRECISION (D)
      COMMON /ESYNTI/ DLATMIN,DLATMAX,DLONMIN,DLONMAX
      DIMENSION DAMSY(10),DPHSY(10),N1(10),N2(10)
      CHARACTER CSTAT*12,CWAVE(10)*2,C2STAR*4,ifile*80,ofile*80,
     1gridfile*80
      real etime,time,lap(2)
C      DATA IUN12/2/
      DATA C2STAR/'**'/,IUN12/10/,IUN16/20/
      DATA CWAVE/'MF','Q1','O1','P1','K1','N2','M2','S2','K2','M3'/
      DATA N1/2,286,429,538,556,740,891,974,1005,1122/
      DATA N2/285,428,537,555,593,890,973,1004,1121,1214/
C
      WRITE(*,1704)
 1704 FORMAT(
     1' Program GTPAR interpolates synthetic gravity tide parameters'/
     2' (for 10 wavegroups) from 1 by 1 degree grid of Timmen and Wenzel
     3  (1994)'/
     4' For 13 wavegroups (CTE505) M0S0, J1 and OO1 are added with const
     5ant values'/)
C      write(*,'(a)')'Input station file (eg. GRREDU2.1 coord file):'
      read(*,*) gridfile
      read(*,*) ifile
      k=index(ifile,'.')
      if (k.eq.0) k=index(ifile,' ')
      ofile=ifile(1:k-1)//'.tide'
      write(*,'((a),(a))')'.. GTPAR output: tidal waves parameter file (
     1.tide): ',ofile
c
      OPEN(UNIT=IUN12,FILE=ifile,FORM='FORMATTED',status='old')
      OPEN(UNIT=IUN16,FILE=ofile,FORM='FORMATTED',
     .status='unknown')
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C     Initialize storage array for tidal parameters:                   !
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IENT=1
      CALL WSYNTI(IENT,gridfile,IUN16,DLAT,DLON,DAMSY,DPHSY)
      IENT=2
      Indi=0
      AZ=0.D0
      UN=1.D0
      IB=99
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C     Read stations latitude and longitude:                            !
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 2005 READ(IUN12,*,END=2100) NRST,CSTAT,DLAT,DLON,DH
      WRITE(IUN16,2020) NRST,CSTAT,DLAT,DLON,DH
      DLONI=DLON
      IF(DLONI.GT.180.D0) DLONI=DLONI-360.D0
      gTH=9.780327*(1.D0+(0.0053024*DSIN(DLAT)*DSIN(DLAT)))
      gTHB=gTH*100.0
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C     Interpolate synthetic gravity tide parameter from grid:          !
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IENT=2
      CALL WSYNTI(IENT,gridfile,IUN16,DLAT,DLONI,DAMSY,DPHSY)
      WRITE(*,2020) NRST,CSTAT,DLAT,DLON,DH
 2020 FORMAT('# ',I8,1X,A12,2F11.6,F10.3)
C Data output to TWpar.out
      NJ1=1
      NJ2=1
      DAMSYJ1=1.000
      DPHSYJ1=0.0
      WRITE(IUN16,2075) NJ1,NJ2,'M0S0',DAMSYJ1,DPHSYJ1
C
      DO 2045 IW=1,5
      WRITE(IUN16,2075) N1(IW),N2(IW),CWAVE(IW),DAMSY(IW),DPHSY(IW)
 2045 CONTINUE
C
      NJ1=594
      NJ2=634
      DAMSYJ1=1.160
      DPHSYJ1=0.0
      WRITE(IUN16,2075) NJ1,NJ2,'  J1',DAMSYJ1,DPHSYJ1
      NJ1=635
      NJ2=739
      WRITE(IUN16,2075) NJ1,NJ2,' OO1',DAMSYJ1,DPHSYJ1
C
      DO 2046 IW=6,10
      WRITE(IUN16,2075) N1(IW),N2(IW),CWAVE(IW),DAMSY(IW),DPHSY(IW)
 2046 CONTINUE
C
 2075 FORMAT(2I5,2X,A4,2F8.4)
      GOTO 2005
 2100 CONTINUE
      close(IUN16)
C
C GNU FORTRAN COMPILER g77 intrinsic function ETIME
      time = ETIME(lap)
      write(*,*)
      write(*,'(a,f6.2,a)') 'Program has used', time,
     .' seconds of CPU time!'
      write(*,'(a,f6.2,a,f6.2,a)') 'This includes',lap(1),
     .' seconds of user time and',lap(2),' seconds of system time.'
C
      END
C
      SUBROUTINE WSYNTI(IENT,gridfile,IUN16,DLAT,DLON,DAMSY,DPHSY)
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C                                                                      !
C     Routine WSYNTI, version 1994.05.17 Fortran 90.                   !
C                                                                      !
C     With IENT=1, the routine reads the synthetic gravity tide        !
C                  parameters for and stores them in RAM.              !
C     With IENT=2, the routine interpolates by bilinear interpolation  !
C                  the synthetic gravity tide parameters DAMSYN, DPHSY !
C                  at given station coordinates DLAT, DLON.            !
C                                                                      !
C     Routine creation:  1994.05.16 by Hans-Georg Wenzel,              !
C                        Geodaetisches Institut,                       !
C                        Universitaet Karlsruhe,                       !
C                        Englerstr. 7,                                 !
C                        D-7500 KARLSRUHE 1,                           !
C                        Germany.                                      !
C                        Tel.: 0721-6082301.                           !
C                        FAX:  0721-694552.                            !
C                        e-mail: wenzel@gik.bau-verm.uni-karlsruhe.de  !
C     Last modification: 1994.05.17 by Hans-Georg Wenzel.              !
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IMPLICIT DOUBLE PRECISION (D)
      DIMENSION DAMSY(10),DPHSY(10)
      DIMENSION DAMSIN(10),DPHSIN(10)
      PARAMETER (ILATMA=180,ILONMA=360)
      DIMENSION DAMSYN(10,ILATMA,ILONMA),DPHSYN(10,ILATMA,ILONMA)
      COMMON /ESYNTI/ DLATMIN,DLATMAX,DLONMIN,DLONMAX
      CHARACTER CTEXT(8)*10,CENDT*10,gridfile*80
      DATA CENDT/'C*********'/
      DATA IUN1/15/,DDLAT/-1.D0/,DDLON/1.D0/
      DATA NOUT/0/
      IF(IENT.NE.1) GOTO 2000
      OPEN(UNIT=IUN1,FILE=gridfile,FORM='FORMATTED')
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C     Define grid area:                                                !
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      DLATMAX=  89.5D0
      DLATMIN= -89.5D0
      DLONMIN= -179.5D0
      DLONMAX=  179.5D0
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C     Fill grid with dummys:                                           !
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      NLAT=DLATMAX-DLATMIN+1.1
      NLON=DLONMAX-DLONMIN+1.1
      DO 1100 ILAT=1,NLAT
      DO 1100 ILON=1,NLON
      DO 1100 IW=1,10
      DAMSYN(IW,ILAT,ILON)=999.999D0
 1100 DPHSYN(IW,ILAT,ILON)=999.999D0 
      NSTO=0
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C     Read grid of synthetic tidal parameters:                         !
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 1200 READ(IUN1,17001)   (CTEXT(I),I=1,8) 
C      WRITE(IUN16,17002) (CTEXT(I),I=1,8)
C      WRITE(*,17002)     (CTEXT(I),I=1,8)
      IF(CTEXT(1).NE.CENDT) GOTO 1200
 1210 CONTINUE
      READ(IUN1,17003,END=3000) DLAT,DLON
      IF(DLAT.LT.DLATMIN) GOTO 3000
      ILAT=DLATMAX-DLAT+1.1
      IF(DLON.GT.180.D0) DLON=DLON-360.D0
      ILON=DLON-DLONMIN+1.1
      IF(ILON.LT.0) ILON=ILON-1
      READ(IUN1,17004) (DAMSIN(IW),IW=1,10)
      READ(IUN1,17004) (DPHSIN(IW),IW=1,10)
      IF(ILAT.LT.1)      GOTO 1210
      IF(ILAT.GT.ILATMA) GOTO 1210
      IF(ILON.LT.1)      GOTO 1210
      IF(ILON.GT.ILONMA) GOTO 1210
      DO 1220 IW=1,10
      DAMSYN(IW,ILAT,ILON)=DAMSIN(IW)
      DPHSYN(IW,ILAT,ILON)=DPHSIN(IW)
 1220 CONTINUE
      NSTO=NSTO+1     
      GOTO 1210
 2000 CONTINUE
      CLOSE (IUN1)
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C     IENT=2: Interpolate gravity tide parameter.                      !
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      DLONR=DLON
      IF(DLONR.GT.180.D0) DLONR=DLONR-360.D0
      ILAT=DLATMAX-DLAT +1.01
      ILON=DLONR-DLONMIN+1.01
      DO 2010 IW=1,10
      DAMSY(IW)=999.999D0
      DPHSY(IW)=999.999D0
 2010 CONTINUE
      DLAT1=DLATMAX-DBLE(ILAT-1)
      DLON1=DLONMIN+DBLE(ILON-1)
      DLAT2=DLAT1+DDLAT
      DLON2=DLON1+DDLON
      DO 2020 IW=1,10
      IUNDEF=0
      IF(DAMSYN(IW,ILAT,ILON).GT.999.D0) THEN
         NOUT=NOUT+1
         IF(DLON1.LT.0.D0) DLON1=DLON1+360.D0
         WRITE(*,17101) NOUT,DLAT1,DLON1
         IUNDEF=IUNDEF+1
      END IF
      IF(DAMSYN(IW,ILAT+1,ILON).GT.999.D0) THEN
         NOUT=NOUT+1
         IF(DLON1.LT.0.D0) DLON1=DLON1+360.D0
         WRITE(*,17101) NOUT,DLAT2,DLON1
         IUNDEF=IUNDEF+1
      END IF
      IF(DAMSYN(IW,ILAT,ILON+1).GT.999.D0) THEN
         NOUT=NOUT+1
         IF(DLON2.LT.0.D0) DLON2=DLON2+360.D0
         WRITE(*,17101) NOUT,DLAT1,DLON2
         IUNDEF=IUNDEF+1
      END IF
      IF(DAMSYN(IW,ILAT+1,ILON+1).GT.999.D0) THEN
         NOUT=NOUT+1
         IF(DLON2.LT.0.D0) DLON2=DLON2+360.D0
         WRITE(*,17101) NOUT,DLAT2,DLON2
         IUNDEF=IUNDEF+1
      END IF
      IF(IUNDEF.EQ.0) GOTO 2200
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C     Grid point is undefined.                                         !
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
17101 FORMAT(I10,2F10.4,'Grid point is undefined!')
      RETURN
 2200 CONTINUE
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C     Compute values by bilinear interpolation:                        !
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      DI = (DLAT-DLAT1)/DDLAT
      DJ = (DLONR-DLON1)/DDLON
      DIM1 = 1.D0-DI
      DJM1 = 1.D0-DJ
      DAMSY(IW)=DIM1*DJM1*DAMSYN(IW,ILAT,ILON)
     1     + DJ*DIM1*DAMSYN(IW,ILAT,ILON+1)
     2     + DI*DJM1*DAMSYN(IW,ILAT+1,ILON)
     3     + DI*DJ*DAMSYN(IW,ILAT+1,ILON+1)
      DPHSY(IW)=DIM1*DJM1*DPHSYN(IW,ILAT,ILON)
     1     + DJ*DIM1*DPHSYN(IW,ILAT,ILON+1)
     2     + DI*DJM1*DPHSYN(IW,ILAT+1,ILON)
     3     + DI*DJ*DPHSYN(IW,ILAT+1,ILON+1)
 2020 CONTINUE
 3000 CLOSE(IUN1)
      RETURN
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C     Format statements:                                               !
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
17001 FORMAT(8A10)
17002 FORMAT(1X,7A10,A8)
17003 FORMAT(2F8.1)
17004 FORMAT(10F8.4)
      END
      
