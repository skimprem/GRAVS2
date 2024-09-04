      program grredu3
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                       G R R E D U 3
C  This program converts gravity meter's raw observations (collected
C  from the field) into reduced observations by applying conversion 
C  according to the calibration info and several corrections.
C
C  Data are corrected for tides, atm.pressure (with user
C  selected coefficient), height, polar motion and long-term gravity
C  change (e.g. the secular effect of GIA process).
C  Constant or time-dependent scale change values (eg for Scintrex CG5)
C  or manufacturer's table with multiple entries (LCR G,D) as well as
C  the coefficients for scale change function (linear, polynomial or
C  periodic). Periodic function is for LCR.
C
C  For tidal correction the algorithms of ETGTAB (930821, H.-G. Wenzel)
C  has been implemented. ETGTAB computes model tides using different
C  tidal potential developments (e.g. CARTWRIGHT-TAYLER-EDDEN 1973 with
C  505 waves, TAMURA 1987 with 1200 waves) for a number of different
C  tidal components using observed or estimated (e.g. by a body
C  tide and ocean tide model) tidal parameters.
C
C  GRREDU3 program input:
C  timezone,epoch
C  ltide,lpres,lfree,ltime,lcali,lpmot
C  iprint,imodel,irigid
C  pcoef
C  <stations' coordinate and param.file>
C  <stations' tidal wave parameter file>
C  <IERS time series (C04) file>
C  <inst. calibration table file>
C  <inst. parameters file>
C  <raw observation file>
C
C  where
C  timezone: 0 = UT, 1 = MET, 2 = MST, -2 = Greenland summer time etc.
C            In Estonia L(ocal)T(ime) = +2 (+3 in Summer)
C  epoch: YYYY-MM-DD = if ltime = true (see below) then all readings are
C         converted from the observation time (applying const. gravity
C         rate, e.g due to GIA) to the common epoch.
C         For Estonian gravity system the current epoch = 2000-01-01.
C
C  ltide,lpres,lfree,ltime,lcali,lpmot: if all true (value =t or =T) then
C     program computes all possible corrections (tidal, atm.pressure,
C     height, temporal g change, calibration, polar motion).
C     Every parameter can be false(F,f), eg. t t f f f f gives tidal and
C     atm.pressure corrections but no height, g change, scale change,
C     polar motion corrections.
C
C  iprint,imodel,irigid: ETGTAB parameters
C     IPRINT..printout parameter for tidal potential development.
C      IPRINT = 0: tidal potential development will not be printed.
C      IPRINT = 1: geodetic coefficients and astronomical elements
C                    will be printed only.
C      IPRINT = 2: geodetic coefficients, astronomical elements, and
C                    tidal potential development will be printed.
C     IMODEL..tidal potential development to be used.
C      IMODEL=0: DOODSON 1921 development with 378 waves
C      IMODEL=1: CARTWRIGHT-TAYLER-EDDEN 1973 development with 505 waves
C      IMODEL=2: TAMURA  1987     development with 1200 waves (DEFAULT)
C      IMODEL=3: BUELLESFELD 1985 development with  665 waves
C     Read parameter IRIGID = 1 for rigid   Earth model tides,
C                    IRIGID = 0 for elastic Earth model tides.
C     Attention: IRIGID=1 should only be used for comparison with
C                model tides computed from ephemeris programs. For
C                read world predictions, use always IRIGID=0.
C
C  pcoef: single coefficient for the reduction of the atm.pressure (uGal/hPa),
C         eg. -0.3 uGal/hPa (program computes normal pressure at the sea
C         level (in hPa) from DIN 5450 formula, thus correct absolute
C         height is necessary).
C
C  INPUT FILES
C  Format of raw observations:
C  # <instrument name>-<number>  <any text>
C  Statno,time,reading,st.dev,gravimeter height,atm.pressure
C  ...
C
C  NB! Leave first 9 columns of header for inst.name and #
C  Example:
C  # G-867  Greenland 1993
C  1001   1993-06-21  12:00:00  5323.125 0.010  264 1014.6
C  1016   1993-06-21  12:20:30  5344.220 0.010  266 1016.6
C  ....
C
C  Several header records may be in the file defining new observation sequences.
C  NB! Feed-back observations given by F after instrument number in label,
C  eg. "# G-5F"or"# G-1000F"
C
C  For unknown gravimeter height,atm.pressure,...
C  the values -999  -999.9 can be used.
C
C  Format of coordinate list (need not to be sorted):
C  ID_number, ID_name, latitude, longitude (both in decimal degrees),
C  height above sea level(m), g_dot (uGal/yr), a, b
C   Parameters a[uGal/m(*-10)], b[uGal/m^2(*-10)] are used to
C   evalute VGG by linear function: VGG(h)=a+2*b*h. If a<0 then link to
C   the WZZ2.1 output file *.vgg are expected.
C
C  Example:
C  1001 station1   82.0123  -40.9876  22.15  0.1  2717 -45
C  1002 station2   82.0001  -35.0122   4.40  0.0  3086   0
C  80003 Stat3    58.500100 25.012256 104.40 -0.5 -9999  D:\VGGmodelling\Stat3.vgg
C  ....
C
C  Format of inst. calibration table file (for LCR G-type), eg.
C  # G-4
C  4200 4346.059 1.0359300
C  4300 4449.652 1.0358800
C  ...
C  NB! If no LCR G-type meter is used, set <inst. calibration table file> = 0
C  Since vers. 2.71 only part of cal.table (which covers obs.range) necessary to introduce.
C
C  Inst. parameter file includes  position of gravimeter reference point
C     from the top (hsys[mm]), scale change value(s) for gravimeter (sf, unitless)
C     and factor for feedback (sffb[uGal/mCU]), period (C.U.), amplitude (uGal),
C     phase [ยบ] for scale periodic errors: amp*sin(2pi*z/per + zer)
C
C  Format of inst. parameter file:
C  # <instrument name>-<number>
C  hsys
C  npoly  NB! if npoly = 1 or 99 -> const. scale corrections (unitless):
C                sf*g or (sf-1)*g, respectively (e.g. evaluated by GRAD2, see GRADJ2 source),
C                npoly = 2, 3, ... -> polynomial scale correction,
C                npoly = -1, -2,... -> time dependent scale corrections for CG-5 (unit ppm)
C  poly(1),sffb  (sffb is only needed for the scale of LCR feedback system, e.g. G-115F)
C  poly(2)
C  ...
C
C  nper          NB! Next part for LCR gravimeters only
C  per(1),amp(1),zer(1)
C  per(2),amp(2),zer(2)
C  ...
C
C  Example table with time dependent scale change of CG-5:
C  -2
C  2004.40  -185.3    NB! All readings taken before 2004.4 or after 2018.54
C  2018.54   453.6    are corrected by using first or last value, accordingly
C
C  NB! 8 or more empty spaces at the beginning of the line in every input
C  file are regarded as empty line and thus ignored
C
C  program output with reduced gravity data and with self-explanation headers:
C  <outputfile *.redu>
C
C  (c) Kort- og Matrikelstyrelsen
C  RF feb 1993. Based on older Algol programs, first programmed 1978.
C  Ver. 1.1 - Y2K fix June 2000, rf (Last update by Rene Frosberg)
C  Upgraded+updated continuously from 2000 to 2019 (ver. 2.0..3.x) by
C  T.Oja/ELB. Latest updates in 2021
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC  declaring, integer i,j,k,l,m,n
      implicit double precision (a-h,o-z)
CCC FIXED PARAMETERS
CCC ncmax - Max no of stations, maxfile - max no of input files,
CCC nmax - max deg for calibration function (polynomial, Fourier series),
CCC nrc - No of corrections
      parameter (ncmax=500, nmax=10, nrc=6, maxfile=150,maxlen=255)
      character version*(*)
      parameter (version='*** GRREDU3, ver. 2021-03-03 ***')
CCC VECTORS, MATRICES
      dimension istat(ncmax),rlat(ncmax),rlon(ncmax),rh(ncmax),
     .r1vgg(ncmax),r2vgg(ncmax),rpn(ncmax),per(nmax),amp(nmax),zer(nmax)
     .,poly(nmax),cor(nrc),caver(nrc),cmax(nrc),cmin(nrc),cal(71),
     .djd0(ncmax)
      dimension rgdot(ncmax),gg(ncmax,ncmax)
      character*12 cstat(ncmax),stxt,clabel(nrc)
      character*(maxlen) cfile,calfile,mfile,tidfile,polfile,etcfile,hdr
     1,hdr1,ofile,stfile,ifile,vggfile
      logical lfeedback,ltest,lcr,ltest1,lscintrex,lvgg(ncmax)
      logical ltide,lpres,lfree,lpmot,ltime,lcali
CCC  variables for intrinsic function ETIME
      real etime,time,lap(2)
CCC  DATA BLOCK
      data ltide,lpres,lfree,lpmot,ltime,lcali /6*.false./
      data idc/10/,idcal/11/,idm/12/,idtid/13/,idpol/14/
     1,idi/20/,ido/21/,idstat/22/,idvgg/23/
      data caver/nrc*0.d0/,cmax/nrc*-9999/,cmin/nrc*9999/,
     .istat/ncmax*0.d0/
      data clabel/'tides      :','atm.press. :','height     :',
     .'pol. motion:','g_dot      :','cal.error  :'/
CCC  ETGTAB block (for tidal correction)
      CHARACTER CUNIT(11)*8,CVERS*10,CMODEL(4)*13
      PARAMETER (MAXWG=85,NGR=13)
      DIMENSION NA(ncmax,MAXWG),NE(ncmax,MAXWG),INA(MAXWG),INE(MAXWG),
     1 DG0(MAXWG),DPHI0(MAXWG),N1(NGR),N2(NGR)
      CHARACTER CNSY(MAXWG)*8,CWAVE(NGR)*4,mark(2)*1
      PARAMETER (MAXNW=1214)
      DIMENSION DTHAM(MAXNW),DTHPH(MAXNW),DTHFR(MAXNW),DBODY(MAXNW)
      DIMENSION NRW(MAXNW)
      DIMENSION DDTHAM(ncmax,MAXNW),DDTHPH(ncmax,MAXNW),
     1DDTHFR(ncmax,MAXNW)
      COMMON /UNITS/ CUNIT,IC2
      COMMON /CONST/ DPI,DPI2,DRAD,DRO
      DATA IUN4/4/
      DATA CVERS/'3.0 930821'/,mark/'+','-'/
      DATA DPI/3.141592653589793D0/,DPI2/6.283185307179586D0/,
     1 DRAD/1.745329251994330D-02/,DRO/57.295779513082320D0/
      DATA CUNIT/'(m/s)**2','nm/s**2 ',' mas    ',' mm     ',' mm     ',
     1' nstr   ',' nstr   ',' nstr   ',' nstr   ',' nstr   ',' mm     '/
      DATA CWAVE/'M0S0','MF','Q1','O1','P1','J1','OO1','K1','N2','M2',
     1'S2','K2','M3'/
      DATA N1/1,2,286,429,538,556,594,635,740,891,974,1005,1122/
      DATA N2/1,285,428,537,555,593,634,739,890,973,1004,1121,1214/
      DATA CMODEL/'DOODSON 1921 ','CTED 1973    ','TAMURA 1987  ',
     1            'BUELLESF.1985'/
CCC  PROGRAM MAIN UNIT
CCC  For program testing (increase verbosity) ltest1 = .true.
      ltest1 = .false.
C
      write(*,'(a)') version
      write(*,*)'------------------------------------------------------'
CCC  read input parameters...
      write(*,*)'Insert timezone, epoch:'
      write(*,*)' timezone = 0: UT'
      write(*,*)'          =+2: in Estonia (+3 in Summer)'
      write(*,*)' epoch = YYYY-MM-DD - important for temporal gravity (g
     .dot) correction'
      read(*,*) izone,stxt
      read(stxt,'(i4,2(a1,i2))') iyr,ch,imonth,ch,iday
      write(*,1100) izone,iyr,imonth,iday
1100  format(i3,i6,2('-',i2.2))
      call MJD2(iyr,imonth,iday,0,0,0,tt,epoch)
      write(*,*) 'Epoch for computations:',epoch
C
      write(*,*)'------------------------------------------------------'
      write(*,*)'Insert ltide,lpres,lfair,ltime,lcali,lpmot:'
      read(*,*) ltide,lpres,lfree,ltime,lcali,lpmot
      write(*,*) ltide,lpres,lfree,ltime,lcali,lpmot
      if(.not.ltide) write(*,*)'No tidal correction applied!'
      if(.not.lpres) write(*,*)'No atm.pressure correction applied!'
      if(.not.lfree) write(*,*)'No height correction applied!'
      if(.not.ltime) write(*,*)'No g time change correction applied!'
      if(.not.lcali) write(*,*)'No calibration correction applied!'
      if(.not.lpmot) write(*,*)'No polar motion correction applied!'
      write(*,*)'------------------------------------------------------'
CCC
CCC  ETGTAB initial values
      write(*,*)'Insert iprint,imodel,irigid:'
      write(*,*)'NB! Suggested values are 0 2 0 -> tidal potential devel
     .opment table not printed + TAMURA(1987) development with 1200 wave
     .s + elastic Earth model tides'
C      write(*,*)'IPRINT = 0: tidal potential development will not be pri
C     1nted'
C      write(*,*)'IPRINT = 1: geodetic coefficients and astronomical elem
C     1ents will be printed only'
C      write(*,*)'IPRINT = 2: geodetic coefficients, astronomical element
C     1s, and tidal potential development will be printed'
C      write(*,*)'IMODEL=1: CARTWRIGHT-TAYLER-EDDEN 1973 development with
C     1 505 waves'
C      write(*,*)'IMODEL=2: TAMURA 1987 development with 1200 waves'
C      write(*,*)'IRIGID = 1 for rigid   Earth model tides'
C      write(*,*)'IRIGID = 0 for elastic Earth model tides'
      read(*,*) iprint,imodel,irigid
      write(*,*) iprint,imodel,irigid,' -> Tidal potential development u
     .sed: ',CMODEL(IMODEL+1)
      write(*,*)'------------------------------------------------------'
CCC End of parameters input
      dgrav = 0.d0
      daz = 0.d0
      ic = 0
      delta = 1.16D0
CCC Other values
C      pcoef = -0.3D0
      write(*,*)'Insert single admittance value pcoef [uGal/hPa] for the
     1 reduction of the atm.pressure (e.g. -0.3):'
      read(*,*) pcoef
      write(*,*)'Reduction of the atm.pressure using the coef.',pcoef
CCC
      nf = 0        !Total no of observation lists
      nrfile = 0    !No of files
      nc = 0        !No of stations included
      write(*,*)'------------------------------------------------------'
CCC  read input files...
      write(*,*)'Read input files...'
      write(*,'(a)') ' station coordinate file:'
      read(*,'(a)') cfile
      write(*,'(a)') cfile
      write(*,'(a)') ' station tide param.file: '
      read(*,'(a)') tidfile
      write(*,'(a)') tidfile
      write(*,'(a)') '  ETCPOT.DAT file: '
      read(*,'(a)') etcfile
      write(*,'(a)') etcfile
      write(*,'(a)') '  IERS(C04) time series file (for polar motion cor
     1rection): '
      read(*,'(a)') polfile
      write(*,'(a)') polfile
      write(*,'(a)') '  LCR calibr. table file: '
      read(*,'(a)') calfile
      write(*,'(a)') calfile
      write(*,'(a)') '  gravimeter param. file: '
      read(*,'(a)') mfile
      write(*,'(a)') mfile
CCC
      if(lpmot) then
      open(idpol,file=polfile,form='formatted',status='old',IOSTAT=KODE)
      if(KODE.ne.0) lpmot=.false.
      endif
CCC
      write(*,'((a),i3,(a))')'Read obs.files (max.',maxfile,' files per
     1project)...'
      write(*,*)'------------------------------------------------------'
CCC
CCC  First computations
      pi2 = 8.d0*datan2(1.d0,1.d0)
      if(ltest1) write(*,'(a6,f20.17)') 'pi2 = ',pi2
      radeg = 360.d0/pi2
CCC Read input files with reduced data
CCCC Program can read file name with full dir path (max.lenght=maxlen)
75    read(*,'(a)',end=180) ifile
      if (ifile(1:8).eq.'        ') goto 75
      nrfile = nrfile+1
      write(*,500) nrfile,ifile
CCC  Format labels between 500- 1000
500   format(/'input file no',i4,' : ',a)
      if (nrfile.gt.maxfile)
     1 STOP ': Too many input files, increase maxfile!!!'
      kj = 1
CCC  to get special char '\', use '\\'
46    kk=index(ifile(kj:),'\\')
      if(ltest1) then
       write(*,*)'ifile(kj:)=',ifile(kj:)
       write(*,*)'kk=',kk
      endif
      kj = kj + kk
      if(ltest1) write(*,*)'kj=',kj
      if(kk.ne.0) goto 46
CCC
      k=index(ifile(kj:),'.')
      if (k.eq.0) k=index(ifile(kj:),' ')
       if(ltest1) write(*,*)'k=',k
       k = k+kj-1
       if(k.lt.kj)
     1 STOP ': Name of the obs.file too long, increase maxlen!'
CCC
      ofile=ifile(kj:k-1)//'.redu'
      stfile=ifile(kj:k-1)//'.stat'
      write(*,'((a),(a))') 'output file: ',ofile
      write(*,*)
CCC  open obs. file
      open(idi,file=ifile,form='formatted',status='old',IOSTAT=KODE)
      if(KODE.ne.0) STOP': Reduced observation file not found!'
      open(ido,file=ofile,form='formatted',status='unknown')
CCC File with stations included
      open(idstat,file=stfile,form='formatted',status='unknown')
      istatp=0
C      write(idstat,'(a1)')'>'
CCC  main reading loop
      i=1
      if(mark(1).ne.'+') mark(1)='+'  !Sometimes it may be overwritten (used in tidal comp)
      if (izone.eq.0) then
       write(*,*) 'NB! All times in UT'
      else
       if (izone.lt.0) i=2
       write(*,510) mark(i),abs(izone)
      endif
510   format('NB! Local time (UT ',a1,i2,') is used in obs files'/)
CCC
      write(ido,'(36x,(a))') '|-C.U./mGal----uGal|-----------CORRECTIONS
     1-(uGal)-----------|--------mGal--------|'
      write(ido,530) mark(i),abs(izone)
530   format(' station      date,     time     obs    reading   stdev
     1 tides   air-    free-   polar    gdot    calibr.  reduced   stati
     2on'/,'      ID',14x,'(UT ',a1,i2,')    ID',31x,'pres.   air     mo
     3tion           error    reading   name')
CCC
      yrk = 0           !Average epoch of the campaign
      inst = 0          !Instrument number
      no = 0            !Number of observations in list
C
CCC  Start reading gravimeter's observation file
80    read(idi,'(a)',end=150) hdr
      if(ltest1) write(*,'(i5,1x,a80)') hdr
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C151   if(ltest1) STOP '*** Temporary STOP for program TESTING ***'
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      if (hdr(1:8).eq.'        ') goto 80
      if (hdr(1:1).eq.'#') then
       write(idstat,'(a1)')'>' ! break to stat list (polygon) file
       if (no.gt.0) then
        write(*,*)
        write(*,590)
        write(*,600)
        write(*,610)(clabel(i),cmin(i),cmax(i),cmax(i)-cmin(i),
     .caver(i)/no,i=1,nrc)
        write(*,*)
        write(*,*) 'Number of observations in list: ',no
        write(*,'(a,f10.4)') 'Average epoch of the campaign : ',yrk/no
      write(*,*)'------------------------------------------------------'
        no = 0
        yrk = 0
        do 90 i=1,nrc
         caver(i)=0.d0
         cmax(i)=-9999
         cmin(i)=9999
90      continue
       endif
CCC  NEW GRAVIMETER INDRODUCED
CCC  In the future there should be arrays for gravimeters' no and calibration data
       write(*,*)
       write(*,535) hdr
       write(ido,535) hdr
535    format(a80)
       lcr = .false.
       lscintrex = .false.
       lfeedback = .false.
       if (hdr(3:4).eq.'G-') lcr = .true.
       if (hdr(3:4).eq.'S-') lscintrex = .true.
       kk=index(hdr,'-')
       if(kk.eq.0) stop
     .'ERRC: Check instrument name in header, eg symbol "-" between inst
     .rument type (G, S, ...) and number!'
C       write(*,*)'kk',kk
       inst = iget(hdr,kk+1)
       write(*,*) 'Instrument number: ',inst
       nf = nf+1
       if(lcr) then
        k=index(hdr(6:9),'F')
        if (k.eq.0) k=index(hdr(6:9),'f')
C         write(*,*) 'k=',k, k+5
        k = k + 5
        lfeedback = (hdr(k:k).eq.'F'.or.hdr(k:k).eq.'f')
        if (lfeedback)
     .  write(*,*) 'Code f/F found - feed-back instrument observations!'
CCC  read gravimeter's calibration function (table) for LCR G gravimeters
        open(idcal,file=calfile,form='formatted',status='old')
100     read (idcal,'(a)',end=105) hdr1
        if(hdr1(1:1).ne.'#'.or.hdr1(1:8).eq.'        ') goto 100
        inst1 = iget(hdr1,5)
        if (inst.ne.inst1) goto 100
CCC Initializing cal vector
        do 101 i=1,71
101      cal(i)=0
CCC Read table...
C103     read(idcal,*,err=105) i,cal(i/100+1)
103     read(idcal,*,end=105) hdr1
        if(hdr1(1:1).eq.'#'.or.hdr1(1:8).eq.'        ') goto 105
        backspace(idcal)
        read(idcal,*) i,cal(i/100+1)
        if(ltest1) write(*,*) i,cal(i/100+1)
        goto 103
105     close(idcal)
       endif
CCC  read gravimeter's scale factor,reference point height,periodic error
CCC  corrections
       sf = 1.d0
       sffb = 1.d0
       hsys = 0.d0
       npoly = 0
       nsf=0
       nper = 0
       inst1 = 0
       ltest = .false.
C       if (lscale) then
       open(idm,file=mfile,form='formatted',status='old')
C       rewind(15)
110    read (idm,'(a)',end=115) hdr1
       if(hdr1(1:1).ne.'#'.or.hdr1(1:8).eq.'        ') goto 110
       k = 0
       kk=index(hdr1,'-')
       inst1 = iget(hdr1,kk+1)
       if (inst.ne.inst1) goto 110
       if(ltest1) write(*,*) 'hdrs',hdr(3:4),hdr1(3:4)
       if(hdr(3:4).ne.hdr1(3:4)) goto 110
       if(lcr) then
        k=index(hdr1(6:9),'F')
        if (k.eq.0) k=index(hdr1(6:9),'f')
        if (lfeedback) then
         if(k.eq.0) goto 110
        else
         if(k.ne.0) goto 110
        endif
       endif
       ltest = .true.
115     if (inst.ne.inst1.or.(.not.ltest))
     .  stop ':Gravimeter not found in meter file!'
CCC Read gravimeter's data now
       read(idm,*) hsys
       write(*,560) hsys
560    format('Position of gravimeter reference point: ',f6.1,' mm')
C
       read(idm,*) npoly
       write(*,*) 'Degree of calibration function=',npoly
       if(npoly.gt.0) then
        write(*,*)'Linear/polynomial calibration correction function:'
        if (npoly.gt.nmax.and.npoly.ne.99)
     .   stop ': *** Too high polynomial degree,npoly > nmax! ***'
        k=npoly
        if(npoly.eq.99) k=1
        do i=1,k
         if (lfeedback.and.i.eq.1) then
CCC sffb is necessary to convert LCR FFB (force feedback) readings into counter units
          read (idm,*) poly(i), sffb
          write(*,540) i,poly(i)
          write(*,550) sffb
         else
          read (idm,*) poly(i)
          write(*,540) i,poly(i)
         endif
        enddo
CCC Read table for time dependent scale correction (for 2 values npoly=-2, for 3 npoly=-3,..)
       else if(npoly.lt.0) then
        write(*,*) 'Deg value npoly negative, thus table (nrow=-npoly) w
     1ith time dependent scale changes is expected!'
        nsf=iabs(npoly)
C        write(*,*) 'TEST: NSF=',nsf
        do i=1,nsf
         read (idm,*) per(i), poly(i)    ! i asemel muu indeks, mida hiljem interpoleerimiseks vaja
C         write(*,*) 'TEST: PER, POLY=',per(i), poly(i)
        enddo
       else
        write (*,*)'*** No linear scale factor/polynomial correction fun
     .ction was applied ***'
       endif
540    format('degree, term = ',i3,f10.6)
550    format(
     1 'feedback linear sc.factor [C.U./FFB_units] = ',f10.6)
C
       if(lcr) then
        read (idm,*) nper
        if (nper.gt.nmax)
     .  stop ': Too many periodic waves (increase nmax)!'
        if (nper.eq.0) then
         write (*,*)'*** No periodic correction was applied ***'
        else
         write (*,*)'Periodic correction was applied using following',
     .  ' data:'
         write (*,*)' Period(C.U.)  Amplitude(uGal)  Zero Phase'
         do 120 i=1,nper
          read (idm,*) per(i),amp(i),zer(i)
          write (*,570) per(i),amp(i),zer(i)
120      continue
        endif
       endif
       close(idm)
570    format(3f13.4)
       write (*,*)
C      endif  !if (lscale) then ...
CCC  Next line is connected with line near label 345 (if (hdr(1:1).eq.'#') then ...)
CCC  Now read observation data
      else
       backspace(idi)
       if(inst.eq.0) stop': instrument number not found, check input fil
     .es!'
       if (.not.lfeedback) then
        read(idi,*,end=150)
     .  ino,stxt,hdr,rdg,std,dh,hdr1
C     .  ino,stxt,hdr,rdg,std,dh,pres,tempc,tempo,ihum
CCC NB! Temporary use of stxt and hdr to read in date and time
C       elseif (lscintrex) then
C        read(idi,*,end=150) ino,iday,t,rdg,std,dh,pres,tempc,tempo,ihum
       else
CCC Feedback readings should be in FFB units (times 1000) and sffb in CU per FFB reading unit
        read(idi,*,end=150)
     .  ino,stxt,hdr,r1,r2,std,dh,hdr1
C     .  ino,stxt,hdr,r1,r2,std,dh,pres,tempc,tempo,ihum
CCC Conversion for LCR FFB (force feedback) readings
        rdg = r1 - (r2/1000.d0)*sffb
       endif
       no = no + 1
CCCC Date and time info
       read(stxt,'(i4,2(a1,i2))') iyr,ch,imonth,ch,iday
       read(hdr,'(i2,2(a1,i2))') ih,ch,im,ch,is
CCCC Air pressure info (hdr1 used to be backward compatible with older input format)
       read(hdr1,'(f8.2)') pres
C        write(*,*)'Test date and time:',iyr,imonth,iday,ih,im,is
C       if(lpres.and.(pres.lt.900.d0.or.pres.gt.1100.d0)) then
C        write(*,*)'Date and time:',iyr,imonth,iday,ih,im,is
C        stop'*** ERROR: wrong airpressure value found in obs file! ***'
C       endif
CCC  Output data in GRED2 (FGI program) format - not compatible with current date and time format!
CCC  How to print ino as a string (A8)?
C       write(stxt,'(I12)') ino
C       imonth = mod(iday/100,100)
C       iiday   = iday/10000
C       it = (t-int(t))*1000
C       ipres = pres*10
C       itempc=tempc*10
C       itempo=tempo*10
C       if(lfeedback) then
C        irdg=r1*10000
C        write(*,1050) ino,imonth,iiday,int(t),it,irdg,int(dh),itempo,
C     1itempc,ipres,int(r2*10)
C       else
C        irdg=rdg*10000
C        write(*,1051) ino,imonth,iiday,int(t),it,irdg,int(dh),itempo,
C     1itempc,ipres
C       endif
C1050   FORMAT(I6,2X,2I2.2,1X,I2.2,1X,I3.3,1X,I8,3I4,I5,I13)
C1051   FORMAT(I6,2X,2I2.2,1X,I2.2,1X,I3.3,1X,I8,3I4,I5)
C
CCC  Modified Julian date computation using routine MJD2
       call MJD2(iyr,imonth,iday,ih,im,is,dmodjd,yr)
       if(ltest1) write(*,*)'dmodjd=',dmodjd
       yrk = yrk + yr
       dmodjd = dmodjd-izone/24.d0
CCC  LOOKING FOR STATION COORDINATES AND OTHER PARAMETERS
       call listno(ino,istat,nc,ii)
       if(ii.eq.0) then
        open(idc,file=cfile,form='formatted',status='old',IOSTAT=KODE)
        if(KODE.ne.0) STOP ': Coordinate file not found!'
50      read(idc,*,end=70) i
        if(ino.ne.i) goto 50
        nc = nc + 1
        if (nc.gt.ncmax)
     .  stop ': too many coordinates, increase NCMAX!'
CCC  Read coordinates, height, free air gradient and normal pressure
CCC  from coord file
        backspace(idc)
        read(idc,*) istat(nc),cstat(nc),rlat(nc),rlon(nc),rh(nc),
     .  rgdot(nc),r1vgg(nc),hdr1
C        write(*,*)'istat, cstat=', istat(nc),cstat(nc)
        close(idc)
CCC Normal pressure at the sea level from DIN 5450 formula
        rpn(nc)=0.d0
        if(lpres) rpn(nc)=
     1  1013.25D0*((1-0.0065D0*rh(nc)/288.15D0)**(5.2559D0))
CCC VGG information for lin.relation (const+slope). If const is negative
CCC then vgg file name is expected instead of slope.
        if(r1vgg(nc).ge.0.d0) then
         lvgg(nc)=.true.
         kkj=index(hdr1,'.')
CCCC Next hack is needed to avoid problem by reading integer value
         if(kkj.eq.0) then
          read(hdr1,'(i10)') kjj
          r2vgg(nc)=dble(kjj)
         else
          read(hdr1,'(f10.3)') r2vgg(nc)
         endif
C         write(*,*)'TEST: nc,hdr1,r1vgg(nc),r2vgg(nc)',
C     .   nc,hdr1,r1vgg(nc),r2vgg(nc)
         write(*,'(2a,2f8.2)')cstat(nc),': inserted polynomial model (n=
     12) parameters for vert.gradient (based on SI units nm/s^2/m, multi
     2plied by -1):',r1vgg(nc),r2vgg(nc)
         write(*,'(2(a,f8.2),a)')'Accordingly, parameters with CGS units
     .:',r1vgg(nc)*(-0.1D0),' uGal/m, ',r2vgg(nc)*(-0.1D0),' uGal/m^2'
        else
         write(*,'(2a)')cstat(nc),': negative gradient value found, thus
     . the link to *.vgg (output of WZZ2) file expected!'
         read(hdr1,'(a)') vggfile
         write(*,'(2a)')'VGG file: ',vggfile
CCC Test filename of *.vgg, if too short STOP. Should be tested id number or txt!?
         if(len(vggfile).le.3) then
          write(*,1090) cstat(nc)
          STOP ': invalid VGG file name!'
         endif
         lvgg(nc)=.false.
         r1vgg(nc)=0.d0
         open(idvgg,file=vggfile,form='formatted',status='old',
     1    IOSTAT=KODE)
CCC Test availability of *.vgg file
          if(KODE.ne.0) then
           write(*,1090) cstat(nc)
           STOP
          endif
CCC Store first 500 g values (h=0.000,0.001,...,0.499 m) from second column of *.vgg file to array gg
          do igg=1,500
           read (idvgg,*) hdr1,gg(nc,igg),hdr1
          enddo
         close(idvgg)
        endif
CCCC VGG BLOCK ENDED
CCCCC ERROR CODE
1090  format('NB! Problem to read *.vgg file for station: ',a)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC Read and compute tidal parameters for every new station, segments
CCC (CAPITAL LETTERS) taken from ETGTAB software by Wenzel (1994)
       if(ltide) then
        djd0(nc) = dmodjd
        dth = ih + im/60.D0 + is/3600.D0
       IF(IRIGID.EQ.1) WRITE(*,7009)
 7009  FORMAT(/' ***** Parameter IRIGID = 1 has been input. '/
     1 ' ***** IRIGID =1 should only be used for comparison with model'/
     2 ' ***** tides computed from ephemeris programs. For real world '/
     3 ' ***** computations, use always IRIGID=0.'/)
CCC Compute amplitudes, frequencies and phases of the tidal waves
        OPEN(IUN4,FILE=etcfile)
        CALL ETPOTA(IUN4,IPRINT,IMODEL,rlat(nc),rlon(nc),rh(nc),DGRAV
     1  ,DAZ,IC,djd0(nc),DTH,DDT0,MAXNW,NRW,DTHAM,DTHPH,DTHFR,DBODY,NW)
        CLOSE(IUN4)
C        if(no.eq.1.and.nf.eq.1)
C     1  WRITE(*,*)'Tidal potential development used: ',CMODEL(IMODEL+1)
C       WRITE(IUN13,7018) CVERS,sLAT,sLON,sH,DGRAV,DAZ,IC,ITY,ITM,ITD,ITH,
C     1 IMODEL,IRIGID
CCC  Read wave groups and observed tidal parameters:
CCC  DG0 - amplitude factor, DPHI0 - phase lead in degree.
        jj = 0
        open(idtid,file=tidfile,form='formatted',status='old')
250     read(idtid,*,end=260,err=260) hdr
        if(hdr(1:1).ne.'#'.or.hdr(1:8).eq.'        ') goto 250
        backspace(idtid)
        read(idtid,*) mark(1),i
        if(ino.ne.i) goto 250
        jj = nc
260     IF(IPRINT.GT.0) WRITE(*,7013) ino
 7013  FORMAT(/'Wave groups and observed tidal parameters at station no'
     1,i9,/
     2'  no. from   to ampl.fac. phase lead        ampl.  WD body '/
     3'                              [deg]')
        do 125 IG=1,NGR
         if (jj.ne.0) then
          READ(idtid,*) NA(jj,IG),NE(jj,IG),CNSY(IG),DG0(IG),DPHI0(IG)
         else
          NA(nc,IG)= N1(IG)
          NE(nc,IG)= N2(IG)
          CNSY(IG)= CWAVE(IG)
          DG0(IG)=delta
          DPHI0(IG)=0.d0
          if(IG.EQ.1) then
           if(N2(IG).eq.1) DG0(IG)=1.d0
           write(*,572) cstat(nc),tidfile,delta,0.0
 572       format('!!! Station ',a,' not found in ',a/'    ->unit values
     . (',f5.2,',',f5.2,') used for tidal wavegroups !!!')
          endif
         endif
C         WRITE(*,*) NA(IG),NE(IG),CNSY(IG),DG0(IG),DPHI0(IG)
          DO 930 IW=1,NW
           IF(NRW(IW).LT.NA(nc,IG)) GOTO 930
           INA(IG)=IW
           GOTO 940
  930     CONTINUE
  940     CONTINUE
        INE(IG)=NW
        DO 950 IW=1,NW
         IF(NRW(IW).LE.NE(nc,IG)) GOTO 950
         INE(IG)=IW-1
         GOTO 960
  950   CONTINUE
  960   CONTINUE
        NAK=INA(IG)
        NEK=INE(IG)
CCC  Search for main wave of the group:
        DAM=0.D0
        DO 970 IW=NAK,NEK
         IF(IRIGID.EQ.1) DBODY(IW)=1.D0
         IF(DTHAM(IW).LT.DAM) GOTO 970
         DAM=DTHAM(IW)
         DBOD=DBODY(IW)
  970   CONTINUE
C       WRITE(IUN6,7015) IG,INA(IG),INE(IG),DG0(IG),DPHI0(IG),CNSY(IG),
        IF(IPRINT.GT.0)
     1  WRITE(*,7015) IG,INA(IG),INE(IG),DG0(IG),DPHI0(IG),CNSY(IG),
     2  DAM,DBOD
 7015   FORMAT(3I5,2F10.4,1X,A8,2F10.4)
CCC  Compute observed amplitude. Constituents of different degree are
CCC  scaled by WAHR-DEHANT body tide amplitude factors:
        DO 920 IW=NAK,NEK
         DTHAM(IW)=DTHAM(IW)*DG0(IG)*DBODY(IW)/DBOD
  920    DTHPH(IW)=DTHPH(IW)+DPHI0(IG)*DRAD
125     continue
        close(idtid)
        do 265 j=1,MAXNW
         DDTHAM(nc,j) = DTHAM(j)
         DDTHPH(nc,j) = DTHPH(j)
         DDTHFR(nc,j) = DTHFR(j)
265     enddo
        endif   !if(ltide) then ...
CCCC End of tidal parameters block
        ii = nc
        write(*,*)
70     endif   !if(ii.eq.0) then ... (near label 548)
       if(ii.eq.0) then
        write(*,575)ino,cfile
575     format('!!! Station',i8,' not found in ',a/
     1'please add station info into coordinate file !!!')
        stop
       endif
CCCC
       slat = rlat(ii)
       slon = rlon(ii)
       sh = rh(ii)
       s1vgg=r1vgg(ii)
       s2vgg=r2vgg(ii)
       spn = rpn(ii)
       sgdot = rgdot(ii)
       stxt = cstat(ii)
       icc = 12
CCC  Output of station coords for mapping program (eg. GMT)
       if (istatp.ne.istat(ii)) then
        write(idstat,450) slat,slon,stxt
        istatp=istat(ii)
       endif
450    format(2f12.6,2x,a12)
CCC CORRECTIONS
CCC  initialize array of corrections
       do i=1,nrc
        cor(i)=0.d0
       enddo
       jc=1
CCC  ETGTAB tidal correction (quick thanks to the previously computed station parameters)
       if(ltide) then
        DT = (dmodjd - djd0(ii))*24.d0
C      write(*,*)'DT',DT
        DDC=0.D0
        DO 1030 IG=1,NGR
        DO 1030 IW=NA(ii,IG),NE(ii,IG)
 1030   DDC=DDC+DDTHAM(ii,IW)*DCOS(DDTHPH(ii,IW)+DT*DDTHFR(ii,IW))
        cor(jc)=-DDC/10.d0 !from SI to CGS
C      WRITE(IUN13,7021) ITY,ITM,ITD,DTH,DGI
C 7021 FORMAT(I4,2I2,1X,2F10.3)
C      WRITE(*,7030)
C 7030 FORMAT(///' ***** ETGTAB finished execution.'/)
       endif
       jc=jc+1
CCC Atm.pressure correction
       if(lpres) then
        dpres=pres-spn
CCC Test if pres is unrealistically high or low
        if(dabs(dpres).lt.100.d0) then
C        if(pres.gt.900.d0.and.pres.lt.1100.d0) then
         cor(jc) = dpres * (-pcoef)
        else
         write(*,*)'!Obs (id=',no,') air pres. value unknown(e.g.-999.9)
     1 or invalid -> no correction!'
         cor(jc) = 0.d0
        endif
       endif
       jc=jc+1
CCC Height correction
       if(lfree) then
        if(dh.gt.-9999.d0) then
CCC Sensor height dhh from mm to m
         dhh=(dh-hsys)*0.001d0
CCCC Polynomial function dg(h2-h1=0)=dg(h2)=a*h2+b*(h2^2)
         if(lvgg(ii)) then
          cor(jc)=(s1vgg*dhh+s2vgg*(dhh**2))*0.1d0
         else
CCCC Modeled g and VGG info from WZZ2 output file *.vgg
CCCC Usage of height as index of gg array
          ihh=IDNINT(dh-hsys)
C          write(*,*)'TEST NINT: dh-hsys,ihh,dhh:',dh-hsys,ihh,dhh
          g1=gg(ii,1)
          g2=gg(ii,ihh+1)
          cor(jc)=g1-g2
C          write(*,*)'TEST2: ihh+1,g1-g2=cor(jc)',ihh+1,g1,g2,cor(jc)
         endif
        else
         write(*,*)'!Obs (id=',no,') meter height unknown(e.g.-9999) or
     1invalid -> no correction!'
         cor(jc) = 0.d0
        endif
       endif
       jc=jc+1
CCC  POLAR MOTION CORRECTION
CCCC  read header lines
       if(lpmot) then
        VER = 0.d0
        if(no.eq.1) then
         rewind(idpol)
         do 155 i=1,9
155      read(idpol,*) hdr
         modjd0 = 0
         modjd1 = 0
         modjd2 = 0
        endif
        if(modjd0.lt.int(dmodjd)) then
160      read(idpol,578,end=165) hdr,modjd1,x1,y1,hdr1
C         write(*,*) hdr,modjd1,x1,y1,hdr1
C         write(*,*) modjd1,x1,y1
         if(modjd1.lt.int(dmodjd)) goto 160
CCCC If step only 1 day then x1,y1 and x2,y2 reversed
         if(modjd2.eq.int(dmodjd)) then
          dmark=-1.d0
          modjd0=modjd2
         else
          read(idpol,578,end=165) hdr,modjd2,x2,y2,hdr1
          dmark=1.d0
          modjd0=modjd1
         endif
578      format(a14,I5,2F11.6,a114) ! Optimized for IERS eopc04.62-now file
165      if(modjd0.lt.int(dmodjd)) then
          write(*,2100) iyr,imonth,iday
2100      format('WARNING: For date ',i6,2('-',i2.2),
     1    ' no IERS X,Y data!')
          dmark=0.d0
          x1=0.d0
          y1=0.d0
         endif
         dx=dmark*(x2-x1)
         dy=dmark*(y2-y1)
        endif
CCC Linear interpolation
        ddt=dmodjd-modjd1
        X=x1+dx*ddt
        Y=y1+dy*ddt
C        write(*,*) X, Y
        SPLA=DSQRT(X*X+Y*Y)
        IF (SPLA.LT.1.D-4) GO TO 163
        SPLO=DATAN2(-Y,X)*radeg
        CALL POL(slat,slon,sh,SPLA,SPLO,VER,OS,EW)
163     cor(jc)=VER
       endif
       jc=jc+1
CCC TIME-DEPENDENT CORRECTION
CCC Conversion from the observation time (yr) to the same epoch (epoch)
       if(ltime) cor(jc)=sgdot*(epoch-yr)
C      write(*,*) 'cor(jc),sgdot,(epoch-yr):',cor(jc),sgdot,epoch-yr
       jc=jc+1
CCCCC CALIBRATION BLOCK
       g = rdg
       rdng = rdg
CCC What value should be taken for gravimeter with feedback reading (FBr) system?
CCC rdg is already corrected with FBr, rdng is without FBr. No big diff for LCR table
CCC but calibr.corr depends how calib.parameters were found - based on rdg or r1 (GRADJ uses latter)
       if (lfeedback) rdng = r1
CCC CALIBRATION TABLE CONVERSION FROM CU TO CGS (mGal) FOR LCR G, D METERS
       if (lcr) then
        i = rdg/100+1
        if(ltest1) write(*,*) 'i,cal(i),cal(i+1)=',i,cal(i),cal(i+1)
        if(i.gt.1.and.(cal(i).eq.0.or.cal(i+1).eq.0)) then
         write(*,*)'ERROR: LCR reading out of *.cal file range! Upgrade
     .*.cal file.'
         STOP
        endif
        g = cal(i)
        g = g + (cal(i+1)-cal(i))*(rdg/100.d0-dble(i)+1.d0)
       endif
CCC CORRECTIONS (Periodic, polynomial, nonlin.adj.based or time-dependent scale change)
       if(lcali) then
CCCC  Periodic error correction for LCR G(D)
        if (lcr.and.(nper.ne.0)) then
         err=0.d0
         do 130 i=1,nper
          err=-amp(i)*dsin(pi2*rdng/per(i)+zer(i)/radeg)
          cor(jc)=cor(jc)+err
130      continue
        endif
CCCC Constant or polynomial function for scale change
        if(npoly.gt.0) then
CCC This scale value sf is taken from nonlinear adjustment (func.model multiplied by 1/sf expanded into Taylor series)
         if(npoly.eq.99) then
          sf=poly(1)
          cor(jc)=cor(jc)/1000.0d0+(sf-1.0d0)*g   ! why g here not rdng? According GRADJ3 seems correct here to use g
         else
CCC Polyn.parameters from linear adjustment
          polysum=0.d0
          do i=1,npoly
C           polysum=polysum+poly(i)*(rdng**i)
           polysum=polysum-poly(i)*(rdng**i)
          enddo
          cor(jc)=cor(jc)/1000.d0+polysum
         endif
        endif
CCCC Linear interpolation from table: C(t)= (c2-c1)/(t2-t1)*(t-t1)+c1
CCCC if outside table time range, first or last value is taken
        if(npoly.lt.0) then
         sf=poly(1)
         if(npoly.ne.-1) then
          if(yr.ge.per(nsf)) then
           sf=poly(nsf)
          else if(yr.gt.per(1)) then
           do i=2,nsf
            if(yr.gt.per(i-1).AND.yr.le.per(i))
     1sf=((poly(i)-poly(i-1))/(per(i)-per(i-1)))*(yr-per(i-1))+poly(i-1)
           enddo
          endif
         endif
CCCC Since scale change values (not corrections) are used above, it is used also here!
C         cor(jc)=sf*g*1.0E-06
         cor(jc)=sf*g*(-1.0D-06)
        endif     ! if(npoly.lt.0) then .. line 926
       endif      ! if(lcali) then .. line 886
CCC
CCC sum all corrections (mGal)
       do i=1,nrc
        if(i.lt.nrc) then
         g = g + cor(i)/1000.d0
        else
         g = g + cor(i)
        endif
       enddo
CCC data output
       rdng1=std*1000.d0
       if(lfeedback) rdng1=r2 !this line may be commented out
       write(ido,580) ino,iyr,imonth,iday,ih,im,is,no,rdng,rdng1,
     . (cor(i),i=1,nrc),g,(stxt(j:j),j=1,icc)
580    format(i8,i6,2('-',i2.2),', ',i2.2,2(':',i2.2),i6,f11.4,f8.1,1x,
     . 5f8.1,2f11.4,1x,12a1)
CCC average, max, min
       do 140 i=1,nrc
        caver(i) = caver(i)+cor(i)
        if(cor(i).gt.cmax(i)) cmax(i)=cor(i)
        if(cor(i).lt.cmin(i)) cmin(i)=cor(i)
140    continue
      endif     ! if (hdr(1:1).eq.'#') then ... line 377 ??
      goto 80
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC  End of cycle with one file
C150   write(*,*)
150   close(idi)
      close(ido)
      close(idstat)
CCC  min,max,average etc...
        write(*,590)
        write(*,600)
        write(*,610)(clabel(i),cmin(i),cmax(i),cmax(i)-cmin(i),
     .caver(i)/no,i=1,nrc)
        write(*,*)
        write(*,*) 'Number of observations in list: ',no
        write(*,'(a,f10.4)') 'Average epoch of the campaign : ',yrk/no
        do i=1,nrc
          caver(i)=0.d0
          cmax(i)=-9999
          cmin(i)=9999
        enddo
      write(*,600)
      goto 75
590   format('Correction(uGal) min     max   range  average')
600   format('----------------------------------------------')
610   format(a12,4f8.1)
CCC  All files processed...
180   write(*,*)
      write(*,*) 'Total no of observation lists:  ',nf
      write(*,*) 'Total no of observation files:  ',nrfile
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GNU FORTRAN COMPILER g77 intrinsic function ETIME
C      include 'include/EXETIME.f'
      include 'EXETIME.f'
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
999   stop 'GRREDU3 terminated OK'
      end
CCC  Subroutines, functions
C subroutine LISTNO finds station number in station list
      include 'LISTNO.f'
C function IGET reads integer from array c
      include 'IGET.f'
C Subroutine MJD computes the Modified Julian Date (MJD)
      include 'MJD2.f'
C routine ETPOTA computes amplitudes, phases, frequencies and
C body tide amplitude factors for a number of different Earth tide
C components using three different tidal potential developments
      include 'ETPOTA.f'
C routine ETASTE computes the astronomical elements for a specific epoch
      include 'ETASTE.f'
C routine ETGCOF computes the geodetic coefficients for the tidal
C potential developments
      include 'ETGCOF.f'
C routine ETLOVE computes latitude dependent LOVE-numbers DH, DK,
C SHIDA-numbers DL, gravimeter factors DG and tilt factors DT
C using the WAHR-DEHANT-ZSCHAU model
      include 'ETLOVE.f'
C routine ETMUTC gives difference ET - UTC   resp. TDT - UTC in seconds
C from 1955.5 until 2016.0
      include 'ETMUTC2.f'
C SUBROUTINE POL COMPUTES THE EFFECT OF THE POLAR MOTION ON THE
C MEASURED GRAVITY
      include 'POL.f'

