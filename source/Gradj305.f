      program gradj3
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c                      G R A D J 3
c  Adjustment program for observations of relative gravimeter.
c
c  The program uses one observation equations for each
c  reading. Readings are assumed uncorrelated, which means
c  that large adjustments take up only limited space, since first
c  part of normal equations is diagonal.
c
c  The program solves for bias values, drifts and/or correctional
c  calibration function dF, with basic obs.eq.
c       obs  =  g + bias + drift(D1*t+D2*t^2+...) + dF(z)
c  Stations which are only observed once do not take part in the
c  adjustment, but are computed from adjusted parameters.
c
c  The adjustment requires a number of loops to stabilize.
c  The drift function may constrained to prevent too
c  high drift values reflecting poor network geometry.
c
c  Program input:
c  proj file name
c  fixed station file name
c  gsf
c  vmode
c  name/list of redu file/files
c
c  where
c  proj file - project file (*.proj) of adjustment to contain following lines:
c   dtmax, lsc, driftpar
c   sigma, sc_std, confl
c   rbias, stdevr
c    where
c    dtmax - maximum time (hrs) allowed between two measurements
c            a tare is set automatically by program for longer intervals
c    lsc   = t - solve for correctional calibration function,
c          (*.cal file needed, see below)
c          = f - no calibration
c    driftpar - std.dev. of drift constraint (zero drift dummy obs):
c             = 0 - no drift estimated,
c             = 0.05 - typical constraint
c 	     = 99 - no constraint on drift
c    sigma  - apriori standard dev. of unit weight (in mGal)
c    sc_std - the factor to scale uncertainty of fixed and adjusted gravity values
c    stdevr - apriori standard dev. of gravimeter readings
c    confl  - confidence level for statistical tests (e.g. 0.95)
c
c   fixed station file - contains a list of stations's ID with fixed g
c                        values and uncertainties
c
c   gsf - parameter for scaling resi file graph (eg 0.010 mGal)
c   vmode - verbose mode, if = t then detailed info about arrays,
c           matrixes and computation process are printed, useful for
c           testing purposes
c
c  redu file name/list - to present reduced observations with unique sequence
c                        number, provided by 'grredu3'
c
c  With *.redu file also *.par file can be introduced with same name as *.redu
c  *.par file:
c  # <Inst_name>-<inst_no>
c  code, seqno
c  ...
c  # <Inst_name2>-<inst_no2>	(consistently with headers in *.redu file)
c  code, seqno
c  ...
c
c  NB! The headers (eq # G-618) are mandatory, and the first 7 characters
c  with the "Inst_name-inst_no" must correspond exactly to the <redobs-file>
c
c  The optional codes in *.par file refer to the sequence number in *.redu file:
c
c  dn(-i)  new drift and bias parameter since observation sequence
c          number n ('seqno'), -i is an option for higher drift degree i
c          (eg. d1-2 means 2nd order drift from the 1st observation)
c  tn      assume tare occurred just before n (or 'seqno'), i.e. new bias
c  sn(-nn)  skip observation 'seqno', -nn option makes available to skip
c           readings up to nn (eg. s10-15 means that readings 10-15 will
c           be skipped.
c  un(-nn) d.dd  new st.dev value d.dd for extra weighting
c                since observation n (or from n to nn), e.g. u25
c                0.010 (u25-35 0.010) - since reading 25 all 
c                observations get st.dev value 0.010 (or from 25 to 35).
c  wn(-nn) s  unit weight (see *.proj file stdevr) of obs n (or obs group n-nn)
c             is multiplied by s (can be integer or real number).
c  xn(-i) extra duplicate observation with tare and drift (deg. i)
c         (this option should only be used in special cases, e.g.
c         separating local observations from long-range ties).
c
c  Text file with the list of fixed stations:
c   station_no1, fixg_value1, sigma, station_name1
c   station_no2, fixg_value2, sigma
c   ....
c
c  For calibration a *.cal file need to be composed (should be named after *.proj):
c   # <Inst_name>-<inst_no>
c   k
c   l
c   per1
c   ...
c   where
c   k - degree of polynomial correction function
c   l - degree of harmonic periodic func. with periods per1, per2, ...
c       (in scale units, only for LCR meters)
c
c  GRADJ2 output:     filename extension
c   <gravfile>         *.grav
c   <residualfile>     *.resi
c   <residualfile2>    *.resi2
c   <tiefile>          *.ties
c   <tiefile.adj>      *.ties.adj
c   <covfile>          *.cov
c   <resfile>          resid4hist.dat
c
c  where
c   <gravfile> outputfile with gravity, calibration, time dependent g
c              results, with some statistical tests
c   <residualfile> residual file with tares and drift parameter
c                  only stations observed more than once are listed in
c                  this file.
c   <residualfile2> same as <residualfile> but average all readings per
c                   session
c   <tiefile> file with ties and residuals on differences
c             between stations in adjustment (no tie when tare). Averaged
c             readings are used, see <residualfile2>.
c             Ties file may be used as input for tie adjustment
c             programs, or may be sorted to show repeat ties
c             (dos or unix: sort <*.ties >*.ties.sort).
c   <tiefile.adj> adjusted gravity differences between all stations
c   <covfile> contains full cov.matrix of adjusted parameters
c   <resfile> residuals of all readings (for historam plotting)
c
C  Originally developed by Rene Forsberg as GRADJ from June 1993 to
C  Mar 10 1995
C  Substantially modified since 2000 by Tonis Oja (Estonian Land Board,
C  Tallinn University of Technology, Estonia). Version gradj -> gradj3
C  Currently ver. 3.0.x (nonlinear drift, weights, gravity change,
C  multiple obs.files, polynom., periodical calibration etc.)
C  Last modif. by T. Oja 2021 (see Gradj_history.txt)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      implicit double precision (a-h,o-z)
CCC  integer i,j,k,l,m,n
c  maxn ... maximal number of unknowns and stations
c  maxc ... max dimension of normal equation matrix
C!! Next relation gives approx. correct maxc dim, see next comments !!
      parameter (maxn=1500,maxc=(maxn**2+3*maxn)/2)
CCC maxc=(maxn(maxn+1))/2+maxn -> maxc=(maxn**2+3*maxn)/2
CCC if maxn=1000 -> maxc=501 500, 2000 -> 2 003 000 !!
      parameter (maxfix=500,maxlist=10,maxcode=250)
      parameter (maxper=10,maxfile=500,maxlen=255,maxobs=25000,maxj=35)
      parameter (gbias=981000.d0)
C
      dimension islist(maxn),ia(maxn),lslist(maxn),ix(maxn),
     .g(maxn),sig(maxn),gdot(maxn),sigdot(maxn),sb(maxobs)
      dimension instlist(maxlist),ipoldeg(maxlist)
      dimension isfix(maxfix),gfix(maxfix),sigfix(maxfix),iifix(maxfix)
C why b ja ib dim are maxcode?
      dimension iseqc(maxcode),b(maxcode),ib(maxcode),idot(maxn)
      dimension c(maxc),sol(maxn),t0(maxn),perr(maxper),cov2(maxc),
     1 ginv(maxc),iper(maxlist),per(maxper,maxlist),iiisc(maxlist)
      dimension amp(maxper),damp(maxper),phs(maxper),dphs(maxper)
CCC Arrays for coef.vector
      dimension coef(maxj,maxobs),icoef(maxj+1,maxobs)
CCC
      logical lslist,lsing(maxn),lsc,ldrift,lcr,lfeedback,ldot,
     .ltest,ltest1,lscintrex
      character*(maxlen) hdr,hdr1,hdr2, arg
      character*(maxlen) ifile,parfile,projfile/''/,gfile,rfile,rfile2,
     .tfile,tafile,calfile,covfile,fixfile/''/,redu_files/''/,help
C     gnfile
      character stxt(maxn)*12 ,obstxt*12, cplot(15)*13
      character code(maxcode)*1,ch*1,instname*3,instname1*3,fr*7,test*6,
     .tyhi*8
      character version*(*)
      parameter (version='*** GRADJ3, ver. 2021-03-03 ***')
CCC  DATA BLOCK to initialise arrays
      data idproj/10/,idg/11/,idt/12/,idr/13/,idr2/14/,idres/15/,
     1 idcal/16/,idpar/17/,idi/18/,idcov/19/,idtmp/20/,idnost/29/,
     2 idtmp2/21/,idfix/22/,idta/23/,idredu/24/
C     ,idgn/14/
      data nfix,adt,aobs,aw,atriiv,av,astres,ared/8*0.d0/
      data ltest/.false./,tyhi/'        '/
C      ,b/maxcode*0.0d0/
CCCCCC COMMON BLOCK
      common /cpar/ pi2,nastat,ndrift,iidrift,iisc,iirhs,iibias,
     .ndot,ldrift,lsc,ldot,av_rd
CCCCCC gnu f77 intrinsic function parameters
      real etime,time,lap(2)
CCCCCC constant 2*Pi
      pi2 = 8.d0*datan2(1.d0,1.d0)
C      write(*,*) pi2
      gsc = 1.0

      help = 'Usage: gradj3 '//
     .'--proj <projfile> '//
     .'--fixed <fixedfile> '//
     .'--gsf <gsc> '//
     .'--ltest '//
     .'--redu <redufiles>'  

      write(*,'(a)') version
      write(*,'(a)')'**************************************************'
CCC  Project file with epoch,adj.param.,abs.stations etc.
C      write(*,*) 'project file (.proj):'

C      write(*,*) 'Insert project file name:'
C      read(*,'(a)',IOSTAT=KODE) projfile

      i = 0
      do while(i < command_argument_count())
       i = i + 1
       call get_command_argument(i, arg)
       select case(arg)
       case('--proj')
        i = i + 1
        call get_command_argument(i, arg)
        projfile = trim(adjustl(arg)) 
       case('--fixed')
        i = i + 1
        call get_command_argument(i, arg)
        fixfile = trim(adjustl(arg))
       case('--gsf')
        i = i + 1
        call get_command_argument(i, arg)
        read(arg, *) gsc
       case('--ltest')
        ltest = .true.
       case('--redu')
        i = i + 1
        call get_command_argument(i, arg)
        redu_files = trim(adjustl(arg))
        open(idredu, file=redu_files, status='old', action='read')
      case ('--help')
        print *, help
        stop
       end select
      end do

C      if(KODE.ne.0) STOP': Problem with proj.file!'

      if (len(trim(projfile)) <= 1) then
       print *, help
       stop '"Project file" not provided'
      end if
      if (len(trim(fixfile)) <= 1) then
       print *, help
       stop '"Fixed file" not provided'
      end if
      if (len(trim(redu_files)) <= 1) then
       print *, help
       stop '"Reduced files" not provided'
      end if

      k=index(projfile,'.')
      if (k.eq.0) k=index(projfile,' ')
      write(*,'(a)') projfile(1:k+10)
      write(*,*)

C      write(*,*) 'Insert fixed station file name:'
C      read(*,'(a)',IOSTAT=KODE) fixfile

       if(KODE.ne.0) STOP': Problem with fix.station file!'
      kk=index(fixfile,'.')
      if (kk.eq.0) kk=index(fixfile,' ')
      write(*,'(a)') fixfile(1:k+10)
      write(*,*)
C      write(*,*)
C     1'Insert new scale factor (uGal/div) for residual graph in .resi fi
C     2le:'

C      read(*,*,IOSTAT=KODE) gsc

      if(KODE.ne.0) STOP': Problem with sc.factor value!'
C      if(KODE.ne.0) then
C       gsc = 0.010
C       write(*,*)'No factor inserted, default value ',gsc,' used!'
C      else
       write(*,*) gsc
C      endif
      write(*,*)

C      write(*,*)
C     1'Insert ltest value (T/F) - verbose mode for program testing purpo
C     2ses:'
C      read(*,*,IOSTAT=KODE) ltest

      if(KODE.ne.0) STOP': Problem with ltest value!'
      write(*,*) ltest
      write(*,*)
C
      gfile=projfile(1:k-1)//'.grav'
      rfile=projfile(1:k-1)//'.resi'
      rfile2=projfile(1:k-1)//'.resi.mean'
      tfile=projfile(1:k-1)//'.ties'
      tafile=projfile(1:k-1)//'.ties.adj'
CCC for gravnet:      gnfile=projfile(1:k-1)//'.gn'
      calfile=projfile(1:k-1)//'.cal'
      covfile=projfile(1:k-1)//'.cov'
c
      write(*,*)'Naming output files...'
      write(*,'((a),(a))') ' 1)           adj.grav file (.grav): ',
     .gfile(1:k+4)
      write(*,'((a),(a))') ' 2)           adj.resi file (.resi): ',
     .rfile(1:k+4)
      write(*,'((a),(a))') ' 3) mean adj.resi file (.resi.mean): ',
     .rfile2(1:k+9)
      write(*,'((a),(a))') ' 4)           out-ties file (.ties): ',
     .tfile(1:k+4)
      write(*,'((a),(a))') ' 5)       adj.ties file (.ties.adj): ',
     .tafile(1:k+8)
      write(*,'(a)')       ' 6)  tr.vector of adj.resi for hist: resid4h
     .ist.dat'
      write(*,'((a),(a))') ' 7)     full cov.matrix file (.cov): ',
     .covfile(1:k+3)
C      write(*,'((a),(a))') '                 GRAVNET file (.gn): ',
C     .gnfile(1:k+2)
c
      open(idproj,file=projfile,status='old')
      open(idfix,file=fixfile,status='old')
      open(idg,file=gfile,status='unknown')
      open(idt,file=tfile,status='unknown')
      open(idta,file=tafile,status='unknown')
      open(idr,file=rfile,status='unknown')
      open(idr2,file=rfile2,status='unknown')
C      open(idgn,file=gnfile,status='unknown')
      open(idres,file='resid4hist.dat',status='unknown')
      open(idcov,file=covfile,status='unknown')
      open(idtmp,form='unformatted',status='scratch')
      open(idtmp2,form='unformatted',status='scratch')
C
      write(*,*)'------------------------------------------------------'
      write(*,*)'Reading adjustment parameters...'
C
      write(*,510)
510   format('...dtmax,lsc,driftpar',/
     1'dtmax       - max. time(hr) between two measurements,tare is set
     2automatically for longer intervals',/
     3'lsc =f      - no calibration, =t : calibration correction functio
     4n is evaluated based on the supplemental *.cal file',/
     5'driftpar =0 - no drift estimated, =0.05 - typical constraint, =99
     6 - no constraint on drift')
      read(idproj,*) dtmax,lsc,driftpar
      write(*,*) dtmax,lsc,driftpar
      ldrift = (driftpar.gt.0)
      write(*,*)
      write(*,512)
512   format('...sigma, sc_std, confl',/
     1'sigma  - apriori standard deviation of unit weight (mGal)',/
     2'sc_std - param. to scale st.dev. of fixed and adj g values',/
     3'confl  - confidence level for statistical tests (0<=confl<=1)')
      read(idproj,*) sigma, sc_std, confl
      write(*,*) sigma, sc_std, confl
      write(*,*)
      write(*,513)
513   format('...rbias, stdevr',/
     1'rbias  - should change if bias values are too high or low',/
     2'stdevr - apriori standard deviation of readings (mGal)')
      read(idproj,*) rbias,stdevr
      write(*,*) rbias,stdevr
      write(*,*)
CCC  Read ldot and epoch for temporal gravity change computation
      write(*,514)
514   format('...ldot, epoch',/
     1'ldot =T - solve for gravity changes, =F - no gravity change',/
     2'epoch   - ref. epoch for gravity change solution (eg 2000-01-01)'
     3)
      read(idproj,*) ldot, hdr
      write(*,*) 'ldot=',ldot
      read(hdr,'(i4,2(a1,i2))') iyr,ch,imonth,ch,iday
C      write(*,*) iyr,imonth,iday
      call MJD2(iyr,imonth,iday,0,0,0,tt,epoch)
      write(*,1100) iyr,imonth,iday,epoch
1100  format('Epoch (in decim.yr):',i6,2('-',i2.2),',',f8.2)
C
      close(idproj)
CCC  Read data of fixed points
      write(*,*)'------------------------------------------------------'
      write(*,*) 'Reading fixed stations...'
      write(*,*) 'list of: no,g,sigma (1 .. nfix)'
48    read(idfix,'(a)',end=49) hdr
       if (hdr(1:8).eq.tyhi) goto 48
       ch = hdr(1:1)
       if (ch.eq.'!') goto 48 !Possible to comment abs.station out
       nfix=nfix+1
       if (nfix.gt.maxfix)
     1  stop ': Too many fixed stations, increase maxfix!'
       backspace(idfix)
       read(idfix,*) isfix(nfix),gfix(nfix),sigfix(nfix)
       write(*,*) isfix(nfix),gfix(nfix),sigfix(nfix)
       goto 48
49    continue
      write(*,*) 'Number of fixed stations:',nfix
      close(idfix)
C
      iobs = 0
      idup = 0
      nobs = 0
      istat = 1
      ibias = 0
      idrift = 1
      idrift0 = 0
      nrinst = 0
      nstat = 0
      nastat = 0
      ideg = 0
      ideg0 = 0
      iti = 0
      iti0= 0
      npoldeg = 0
      nper = 0
      av_rd = 0
C
c  reset all gravity by 981000 internally in program
      do k = 1,nfix
        gfix(k) = gfix(k) - gbias
      enddo
CCC  read idi input files with reduced data  CCCCCCCCCCCCCCCCCCCCCCC
CCC  Program can read file name with full dir path (max.lenght=maxlen)
CCC  under MS-DOS
      write(*,*)'------------------------------------------------------'
      nrfile=0
      write(*,'(a)') 'Read list of reduced reading files (.redu):'
      write(*,'((a),i4,(a))') '(max.',maxfile,' files per project)'

C     45    read(*,'(a)',end=51,IOSTAT=KODE) ifile
45    read(idredu,'(a)',end=51,IOSTAT=KODE) ifile

C      write(*,*) 'KODE:', KODE
       if (ifile(1:8).eq.tyhi) goto 45
       nlist = 0
       nrfile=nrfile+1
       if (nrfile.gt.maxfile) STOP' Too many observation files: increase
     1 maxfile! '
       write(*,'((a),i3,(a),(a))')'No',nrfile,' reading file: ',ifile
CCC
       kj = 1
CCC  to get special char '\', use '\\'
46     kk=index(ifile(kj:),'\\')
C       write(*,*)'ifile(kj:)=',ifile(kj:)
C       write(*,*)'kk=',kk
       kj = kj + kk
C       write(*,*)'kj=',kj
       if(kk.ne.0) goto 46
CCC
       k=index(ifile(kj:),'.')
       if (k.eq.0) k=index(ifile(kj:),' ')
C       write(*,*)'k=',k
       k = k+kj-1
       if(k.lt.kj)
     1 STOP' Name of obs.file too long. Increase maxlen! '
CCC  open obs. file
       open(idi,file=ifile,status='old',IOSTAT=KODE)
       if(KODE.ne.0) STOP': Reduced observation file not found!'
CCC  locate and open file with adjustment parameters
       parfile=ifile(1:k-1)//'.par'
       write(*,'((a),(a))')'Open additional adj. parameter file: ',
     1 parfile
       open(idpar,file=parfile,status='old',IOSTAT=KODE)
C       write(*,*)'KODE=',KODE
       if(KODE.ne.0) then
        write(*,*)'Parameter file (.par) not found. Default values (t1,d
     .1-1) are used!'
       else
47      read (idpar,'(a)') hdr1
        if (hdr1(1:8).eq.tyhi) goto 47
       endif
       write(*,*)
C
C  read observation file - may be with multiple labels
C  output all observations on tmp unit idtmp
C  set pointers to scale, bias and drift
C  ----------------------------------------------------
      do 12 i = 1,3
12    read(idi,*) hdr
C
10    read(idi,'(a)',end=30) hdr
      if (hdr(1:8).eq.tyhi) goto 10
c
      if (hdr(1:1).eq.'#') then
C
C  read hdr line and hdr line info in input file
C  ---------------------------------------------
        if (nlist.gt.0) then
         write(*,*) 'number of observations: ',iobs
         if (idup.gt.0) write(*,*) 'number of duplicates:   ',idup
         write(*,*) '----------------------------------'
        endif
        write(*,'(a)') hdr
C        write(idt,'(a)') hdr
C        if (hdr(3:4).ne.'G-') stop '** header lacks G- in column 3-4'
C        inst = iget(hdr,5)
        lcr = .false.
        lscintrex = .false.
        lfeedback = .false.
        if (hdr(3:4).eq.'G-') lcr = .true.
        if (hdr(3:4).eq.'S-') lscintrex = .true.
        kk=index(hdr,'-')
        instname=hdr(3:kk-1)
C       write(*,*)'kk',kk
        inst = iget(hdr,kk+1)
        if(lcr) then
         k=index(hdr(6:9),'F')
         if (k.eq.0) k=index(hdr(6:9),'f')
C         write(*,*) 'k=',k, k+5
         k = k + 5
         lfeedback = (hdr(k:k).eq.'F'.or.hdr(k:k).eq.'f')
        endif
        if(lfeedback) then
         write(*,*)'Instrument name and number:',instname,'-',inst,
     1   hdr(k:k)
        else
         write(*,*)'Instrument name and number:',instname,'-',inst
        endif
C
        iobs = 0
        idup = 0
        nlist = nlist+1
CCC find index for instruments and scale factors
         idinst = listno(instlist,nrinst,inst)
         if (idinst.eq.0) then
CCC number of instruments: nrinst
           nrinst = nrinst+1
           if (nrinst.gt.maxlist)
     .      stop ': Too many instruments, increase maxlist!'
           instlist(nrinst) = inst
           idinst = nrinst
           iiisc(idinst) = 0
           ipoldeg(idinst) = 0
           iper(idinst) = 0
C  if lsc=.true., then open <projectname>.cal file with the parameters
C  for corrections of calibration function
          if(lsc) then
           iiisc(idinst) = npoldeg+nper+1
           write(*,'(/a/a/)')
     1'Open correction parameters file for gravity meters calibration..'
     2,calfile
           open(idcal,file=calfile,status='old',IOSTAT=KODE2)
C           write(*,*)'KODE2=',KODE2
           if(KODE2.ne.0) stop
     .      ': .cal file not found, calibration impossible!'
CCC Reading gravimeter info from .cal file
           ltest1 = .false.
54         read (idcal,'(a)',end=55) hdr2
           if (hdr2(1:8).eq.tyhi) goto 54
           if(hdr2(1:1).ne.'#') goto 54
           k=index(hdr2,'-')
           inst1 = iget(hdr2,k+1)
           if (inst.ne.inst1) goto 54
C           write(*,*) 'hdrs',hdr(3:4),hdr2(3:4)
           if(hdr(3:4).ne.hdr2(3:4)) goto 54
           ltest1 = .true.
C           write(*,*) inst,inst1,ltest1
55         if (inst.ne.inst1.or.(.not.ltest1))
     .      stop ': Gravimeter not found in .cal file!'
CCC  Read info from file (degree of polynomial term, if lcr then also
CCC  periodic terms
           read (idcal,*) ipoldeg(idinst)
           write(*,*)'Degree of polynomial calibration func.:',
     .     ipoldeg(idinst)
CCC  Degree of polynomial limited to 3
           if(ipoldeg(idinst).gt.3.and.ipoldeg(idinst).ne.99) STOP
     .      ': Calibration failed, polynomial degree too high!'
C           write(*,*)'idinst,ipoldeg(idinst):',idinst,ipoldeg(idinst)
           if(ipoldeg(idinst).eq.99) then
            npoldeg = npoldeg + 1
           else
            npoldeg = npoldeg + ipoldeg(idinst)
           endif
           if(lcr) then
            read (idcal,*) iper(idinst)
            write(*,*)'No of periodic waves:',iper(idinst)
            if(iper(idinst).gt.maxper) then
             write(*,*)': Calibration failed! Too many periodic wave
     .s (not over ',maxper,')!'
             STOP'Program stopped!'
            endif
            do i=1,iper(idinst)
             read (idcal,*) per(i,idinst)
             if(ltest) write (*,*)
     1       'idinst,iper(idinst),i,per(i,idinst):',
     2       idinst,iper(idinst),i,per(i,idinst)
            enddo
            write(*,*)'Periods:',(per(i,idinst),i=1,iper(idinst))
            nper = nper + 2*iper(idinst)
           endif
           write(*,*)
           close(idcal)
           if (ipoldeg(idinst).eq.0.and.iper(idinst).eq.0)
     .iiisc(idinst)=0
          endif    ! if(lsc) then...
         endif     ! if (idinst.eq.0) then...
c
CCCC Drift/tare/skip info from input file
c
        ncode = 1
        nv = 0
        i = 0
        n = 0
C        w = 0.5d0
        w = sigma/stdevr
        if(KODE.ne.0) goto 18     !!!If *.par file does not exists...
CCCC Read data from *.par file
        kk=index(hdr1,'-')
        instname1=hdr1(3:kk-1)
C       write(*,*)'kk',kk
        inst1 = iget(hdr1,kk+1)
C        write(*,*)'inst1,inst:',inst1,inst
C        write(*,*)'instname1,instname:',instname1,instname
        if (inst1.ne.inst.or.instname1.ne.instname) then
         write(*,515) hdr,hdr1
         stop ': Error in program work! '
        endif
        if(lcr) then
         k=index(hdr1(6:9),'F')
         if (k.eq.0) k=index(hdr1(6:9),'f')
C         write(*,*) 'k=',k, k+5
         if((lfeedback.and.k.eq.0).or.(.not.lfeedback.and.k.ne.0)) then
          write(*,515) hdr,hdr1
          stop 'Error in program work!'
         endif
        endif
515   format('ERRC: Instrument name is different in parameter and data f
     .ile:'/,a15/,a15)
c
16      read(idpar,'(a)',end=18) hdr1
        if (hdr1(1:8).eq.tyhi) goto 16
          ch = hdr1(1:1)
          if (ch.eq.'#') goto 18
          if (ch.eq.'!') goto 16 ! Commented line
CCC  Information for weighting, input apriori st.dev of readings
          if(ch.eq.'u'.or.ch.eq.'w') then
           n = n + 1
           backspace(idpar)
           read(idpar,*) hdr1,b(n)
           if(ch.eq.'w') b(n)=dsqrt(b(n))*stdevr !unit weight multiplied by b(n)
           ib(n) = iget(hdr1,2)
CCC  Checking uniqueness of obs id within weight info
           ic1 = listno(ib,n,ib(n))
C           write (*,*)'n,ic1=',n,ic1
           if (n.ne.ic1) then
C            write (*,*) 'WARNING! Previously inserted weight info overwrite.'
            b(ic1)=b(n)
            n=n-1
           endif
CCC  If group of readings are weighted, eg w10-15 0.05 (readings with
CCC  id 10..15 have st.dev 0.05 mGal)
           k = index(hdr1,'-')
           if (k.ne.0) then
            n=n+1
            ib(n) = iget(hdr1,k+1)+1
            b(n) = stdevr
C            if(b(n-2).gt.0.0d0) b(n) = b(n-2)
           endif
C           write (*,*)'b(',n,') =',b(n)
           goto 16
          endif
CCCCCC Other parameters input
          code(ncode) = ch
          iseqc(ncode) = iget(hdr1,2)
          ncode = ncode+1
C  warnings!!!
          if (ncode.gt.maxcode) stop 'ERRC: Too many codes, increase max
     .code!'
          if (.not.(ch.eq.'t'.or.ch.eq.'d'.or.ch.eq.'s'.or.ch.eq.'x'))
     .    stop ': code not t, d, s or x read from input!'
          if ((ch.eq.'x'.or.ch.eq.'t').and.iseqc(ncode).eq.1)
     .    stop ': For first reading no code x or t allowed!'
c
CCC  Read degree of drift func and store it in array ia
          if (ch.eq.'d'.or.ch.eq.'x') then
           i = i + 1
           ia(i) = 1
           k = index(hdr1,'-')
C           write (*,*) 'k=',k
           if (k.eq.0) goto 16
           ia(i) = iget(hdr1,k+1)
C           write (*,*) 'ideg=',ia(i)
           if(ia(i).gt.5)
     .     stop': Too high drift deg value(>5) - danger of overfit!'
C          endif
CCCCC Next part makes possible to skip more than one reading with
CCCCC a sentence si-ii, eg s12-20 skips readings 12-20!
          elseif (ch.eq.'s') then
           k = index(hdr1,'-')
           if (k.ne.0) then
            ii = iget(hdr1,k+1)
C            write (*,*) 'iskip=',ii
            do j=iseqc(ncode-1)+1,ii
             code(ncode) = 's'
             iseqc(ncode) = j
             ncode = ncode+1
            enddo
           endif
          endif
          goto 16
c
18      ncode = ncode-1
        write(*,*)'Adjustment parameters(weight/drift/tare/skip etc.):'
        do 19 k = 1, n
19       write(*,*) 'u/w ',ib(k),b(k)
        i = 0
        do j = 1, ncode
         if (code(j).eq.'d'.or.code(j).eq.'x') then
          i = i + 1
          write(*,*) code(j),iseqc(j),ia(i)
C          write(*,'(x,a1,2i4)') code(j),iseqc(j),ia(i)
         else
          write(*,*) code(j),iseqc(j)
         endif
        enddo
        write(*,*)
CCCCCCCCCCCCCCCCCCCCCC
       if(ltest) then
        write(*,*)'Test temp file idtmp'
        write(*,*) 'iseq,isno,istat,iyr,imonth,iday,ih,im,is,rd,obs,tt,i
     .ti,isc,idinst,ibias,idrift,ideg,weight'
       endif
CCCCCCCCCCCCCCCCCCCCCC
      else
C  read observation line
c  ---------------------
        call gettxt(hdr,obstxt)
        backspace(idi)
C
C20      read(idi,*,end=30) isno,hdr,hdr2,iseq,rd,rd1,r,r,r,r,r,r,obs
        read(idi,*,end=30) isno,hdr,hdr2,iseq,rd,rd1,r,r,r,r,r,r,obs
CCCC Date and time info
        read(hdr,'(i4,2(a1,i2))') iyr,ch,imonth,ch,iday
        read(hdr2,'(i2,2(a1,i2))') ih,ch,im,ch,is
C        write(*,*)'Test date and time:',iyr,imonth,iday,ih,im,is
C
C21      iobs = iobs+1
        iobs = iobs+1
        nobs = nobs+1
CCC  Weights for Scintrex gravimeters on the basis of st.dev (rd1) of single reading (rd)
CCC  sigma and st.dev are not squared because the setup of norm.eq. matrix needs so!
C        if(lscintrex) w = (sigma/rd1)*1000
        if (iobs.gt.1) then
          ttprev = tt
C          tt = djul(iday,t) - t0(iti)
           call MJD2(iyr,imonth,iday,ih,im,is,tt,yr)
          tt = tt - t0(iti)
          if(ltest) write(*,*)'tt',tt
          dt = tt-ttprev
          if (dt.lt.0) write(*,22) iyr,imonth,iday,ih,im,is
22        format('WARNING: time reverse at ',i6,2('-',i2.2),', ',
     1    i2.2,2(':',i2.2))
        else
          iti = iti + 1
C          t0(iti) = djul(iday,t)
          call MJD2(iyr,imonth,iday,ih,im,is,t0(iti),yr)
          if(ltest) write(*,*)'t0',t0(iti)
          tt = 0.d0
          dt = 0.d0
        endif
C  find index for weight etc.
        ic1 = listno(ib,n,iseq)
        if (ic1.gt.0) w=sigma/b(ic1)
C        write(*,*)'n,iseq,ic1,c(icl),w',n,iseq,ic1,c(ic1),w
C  find index for bias, drift etc.
        ic = listno(iseqc,ncode,iseq)
C        write(*,*)'ncode,iseq,ic=',ncode,iseq,ic
        if (ic.gt.0) then
         if (code(ic).eq.'s') then
          if (ic1.gt.0) stop ': WEIGHT FOR SKIPPED READING! '
          iobs = iobs-1
          nobs = nobs-1
          goto 10
         endif
         if (code(ic).eq.'t'.or.code(ic).eq.'d'.or.code(ic).eq.'x') then
          ibias = ibias+1
          if (code(ic).eq.'x') then
           iobs = iobs+1
           nobs = nobs+1
           idup = idup+1
           idrift0 = idrift
           ideg0 = ideg
           iti0 = iti
           if(ltest) write(*,*) 't0(iti),tt:',t0(iti),tt
           ttprev = tt
           w0 = w
           if(ic1.gt.0) w0=sigma/c(ic1-1)
          endif
          if (code(ic).ne.'t') then
           nv = nv + 1
           if(ia(nv).eq.0) goto 24
           idrift = idrift + ideg
           ideg = ia(nv)
CCCCCC New epoch setup due to drift computation
           if (iobs.gt.1) iti = iti + 1
           call MJD2(iyr,imonth,iday,ih,im,is,t0(iti),yr)
           tt = 0.d0
           dt = 0.d0
           if(ltest) write(*,*) 't0(iti+1),ttprev:',t0(iti),ttprev
24        endif
         endif
        elseif (dt.gt.dtmax/24) then
          ibias = ibias+1
        elseif (iobs.eq.1) then
          ibias = ibias+1
          idrift = idrift + ideg
          ideg = 1
        endif
C  find station number index
C  islist ... list of all stations (1->nstat)
C  lslist ... t if station in adjustment (1->nstat)
C  check for fixed stations
C  set preliminary g-values from LC&R offset
        istat = listno(islist,nstat,isno)
        if (istat.eq.0) then
          nstat = nstat + 1
          if (nstat.gt.maxn) 
     .    stop ': Too many stations, increase maxn!'
          islist(nstat) = isno
          stxt(nstat) = obstxt
          g(nstat) = obs - rbias
          lslist(nstat) = .false.
          if (listno(isfix,nfix,isno).gt.0) lslist(nstat) = .true.
          istat = nstat
          av_rd = (av_rd*(nstat-1) + rd)/nstat !average reading value
C          write(*,*)'nstat,rd,av_rd:',nstat,rd,av_rd
        else
          lslist(istat) = .true.
          if (stxt(istat).eq.'            ') stxt(istat) = obstxt
        endif
C
CCCCCC *** test writing to file idtmp ***
       if(ltest) then
        if (code(ic).eq.'x')
     .  write(*,347) iseq,isno,istat,iyr,imonth,iday,ih,im,is,rd,obs,
     .  ttprev,iti0,iiisc(idinst),idinst,ibias-1,idrift0,ideg0,w0
        write(*,347) iseq,isno,istat,iyr,imonth,iday,ih,im,is,rd,obs,tt,
     .  iti,iiisc(idinst),idinst,ibias,idrift,ideg,w
       endif
347   format(i4,i10,i4,i5,2i3,3i3,3f11.4,6i4,f10.5)
CCCCCC ***
CCCCC DATA INTO SCRATCH FILE
        if (code(ic).eq.'x')
     .  write(idtmp) iseq,isno,istat,iyr,imonth,iday,ih,im,is,rd,obs,
     .  ttprev,iti0,iiisc(idinst),idinst,ibias-1,idrift0,ideg0,w0
        write(idtmp) iseq,isno,istat,iyr,imonth,iday,ih,im,is,rd,obs,tt,
     .  iti,iiisc(idinst),idinst,ibias,idrift,ideg,w
      endif
      goto 10
CCCCC END OF CYCLE
30    write(*,*) 'number of observations: ',iobs
      if (idup.gt.0) write(*,*) 'number of duplicates:   ',idup
      close(idi)
      close(idpar)
      write(*,*)'------------------------------------------------------'
CCC  Observation file will be read
      goto 45
CCC  Last file is read
51    close(idi)
      close(idpar)
      write(*,*)'------------------------------------------------------'
C  set adjustment station index ia(istat) is pointing to adj.element
C  -----------------------------------------------------------------
      nastat = 0
      ndot = 0
      do 32 i = 1,nstat
        if (lslist(i)) then
          nastat = nastat + 1
          ia(i) = nastat
CCC  set g change index idot for every station except absolute stations
         if(ldot.and.listno(isfix,nfix,islist(i)).eq.0) then
CCCC temporary function
          open(idnost,file='nostat.list',status='unknown',IOSTAT=KOD)
          if(KOD.ne.0) goto 334
C     1  write(*,*) 'No nostat.list file found - all stations included to
C     2 the g change computation!'
C        endif
         rewind(idnost)
333      read(idnost,*,end=334) isno
          if(islist(i).eq.isno) then
           idot(i)=0
           write(*,*)islist(i),stxt(i),' - no g change estimated!'
           goto 32
          else
           goto 333
          endif
334      ndot = ndot + 1
         idot(i) = ndot
         else
          idot(i) = 0
         endif
        else
          ia(i) = 0
          idot(i) = 0
        endif
32    continue
c
c  set pointers for fixed stations
c  -------------------------------
      do 53 i = 1, nfix
        ii = listno(islist,nstat,isfix(i))
        iifix(i) = ii
53    continue
c
c  output adjustment parameter info
c  --------------------------------
      ndrift = idrift + ideg - 1
      if (.not.ldrift) ndrift = 0
C
      if (.not.lsc) then
       npoldeg = 0
       nper = 0
      endif
      if(ltest) write(*,*)
     1 'nrinst,npoldeg,nper:',nrinst,npoldeg,nper
      if (.not.ldot) ndot = 0
c
      iibias = nastat + ndot    !no. of g and g_change parameters
      iidrift = iibias + ibias  !add no. of rel.gravimeter bias param.
      iisc = iidrift + ndrift   !add no. of drift func. param.
      n = iisc + npoldeg + nper !add no. of calibr.func. param.=total no. of unknowns
      idiag = n*(n+1)/2   !dim. of symm. covariance matrix (triangular part only)
      iirhs = idiag-nastat*(nastat-1)/2 !dim. of symm. normal matrix (triangular part without zero elements)
      icdim = iirhs + n !dim. of extended normal matrix (right side included)
c
      if(ltest)
     1write(*,335) n,icdim,iirhs,idiag,iibias,iidrift,iisc
335   format(/'n = ',i7,
     ./'icdim=',i7,
     ./'iirhs=',i7,
     ./'idiag=',i7,
     ./'iibias=',i7,
     ./'iidrift=',i7,
     ./'iisc=',i7)
c
      write(*,34) nobs,nrinst,nstat,nrfile
34    format('Totally read:',i6,' observations, collected with',i3,' gra
     .vimeters in',i5,' stations.',/'No. of obs. files: ',i3)
      write(*,35) nastat,ndot,ibias,ndrift,npoldeg+nper,n
35    format(/' Number of unknowns in adjustment:',
     ./' Station gravity values:  ',i5,
     ./' Change of gravity:       ',i5,
     ./' Bias parameters:         ',i5,
     ./' Drift parameters:        ',i5,
     ./' Calibration parameters:  ',i5,
     ./' Total number of unknowns:',i5)
      if (icdim.gt.maxc)
     .stop ': Adjustment too large, increase maxc! ***'
      if (n.gt.maxn)
     .stop ': Too many unknowns, increase maxn! ***'
c
CCCCCC ***  test file idtmp output
      if(ltest) then
       rewind(idtmp)
       write(*,*) 'test: binary obs ofile:'
       write(*,*)'iseq, isno,istat,ia(istat), rd, obs, tt, iti, isc, id
     .inst, ibias, idrift, ideg, weight'
       do 50 i = 1,nobs
        read(idtmp) iseq,isno,istat,iyr,imonth,iday,ih,im,is,rd,obs,tt,
     . iti,isc,idinst,ibias,idrift,ideg,w
        call MJD2(iyr,imonth,iday,ih,im,is,dt,tdot)
        tdot= tdot - epoch
        write(*,37) iseq,isno,istat,ia(istat),rd,obs,tdot,tt,
     . iti,isc,idinst,ibias,idrift,ideg,w
50     continue
      endif
37    format(i5,i9,2i4,4f11.4,6i4,f8.2)
c
c  main loop for normal equations
c  ------------------------------
c  first set initial values and loop counter
c  nastat is length of diagonal, n total number of unknowns
      maxloop = 50
      if (lsc) maxloop = 100
       if(ltest) maxloop = 2
      iloop = 0
      do i = 1,nstat
        if (ia(i).gt.0) sol(ia(i)) = g(i)
      enddo
      if(ldot) then
       do i = nastat+1,iibias
        sol(i) = 0.d0
       enddo
      endif
      do 56 i = iibias+1,iidrift
56     sol(i) = rbias
      do 57 i = iidrift+1,iisc
57     sol(i) = 0.d0
      do 58 i = iisc+1,n
58     sol(i) = 0.d0
CCC  find where s0, then sol(i)=1
      if(lsc) then
       do 63 i = 1, nrinst
C        if(iiisc(i).eq.0) goto 63
C        if(ipoldeg(i).eq.0) goto 63
C        write(*,*) 'i,nrinst,iiisc(i)+iisc',i,nrinst,iiisc(i)+iisc
        if(ipoldeg(i).eq.99) sol(iisc+iiisc(i))= 1.d0
63     continue
      endif
c
      if(ltest) then
       write(*,*)'write(*,785) (sol(i),i=1,n)'
       write(*,785) (sol(i),i=1,n)
      endif
785   format('test sol vector:'/,4(5f16.4,/))
C
CCC  apriori variance of single observation
CCC  this variance is updated in each loop
      sigma0 = sigma
      write(*,59) sigma
59    format(/' Adjustment, initial assumed std.dev. of readings:',
     .f8.3)
c
c  adjustment loop entry
c  ---------------------
      maxjj= 0
c
60    rewind(idtmp)
      iloop = iloop+1
c
      naobs = 0
      nnobs = 0
      pvvsum = 0
      gvvsum = 0
      if (iloop.le.maxloop) then
        do 61 i = 1, icdim
61      c(i) = 0
      endif
c
c  scan through reading observations
c  ---------------------------------
70    read(idtmp,end=75) iseq,isno,istat,iyr,imonth,iday,ih,im,is,rd,
     .obs,tt,iti,isc,idinst,ibias,idrift,ideg,w
      rd1=rd-av_rd
c
      iastat = ia(istat)
      if (iastat.eq.0) goto 70
      naobs = naobs+1
      nnobs = naobs
c
CCC  Set up the rows of coefficient (design) matrix
      iidot = idot(istat)
       call MJD2(iyr,imonth,iday,ih,im,is,dt,tdot)
      tdot = tdot - epoch
C
      s0 = 1.d0
      ipol = 0
      iperr = 0
C
      if (lsc.and.isc.ne.0) then
       ipol = ipoldeg(idinst)
C       write(*,*)'idinst,ipoldeg(idinst)',idinst,ipoldeg(idinst)
       if(ipol.eq.99) then
C        ipol=1
        s0 = sol(iisc+isc)
       endif
       iperr = iper(idinst)
       do i=1,iperr
        perr(i) = per(i,idinst)
C        write(*,*)'i,idinst,iperr,perr(i)',i,idinst,iperr,perr(i)
       enddo
      endif
C
C      if(ltest) then
C      write(*,*)
C     1'iastat,iidot,tdot,ibias,idrift,ideg,tt,isc,ipol,iperr,perr(i),rd:
C     2',iastat,iidot,tdot,ibias,idrift,ideg,tt,isc,ipol,iperr,
C     3(perr(i),i=1,iperr),rd
C      endif
C
      pobs = predobs(sol,iastat,iidot,tdot,ibias,idrift,ideg,tt,idinst,
     .isc,ipol,iperr,perr,rd)
CCCC NB! if ipol=99 (see above) then ipol got value 1 now!
c
CCC  Station info
      b(1) = 1.d0/s0
      ib(1) = iastat
      j = 2
CCC  Gravity change at specific station
      if(ldot) then
       if(iidot.ne.0) then
        b(j) = tdot/s0
        ib(j) = nastat + iidot
       else
        b(j) = 0.d0
        ib(j) = nastat + 1
       endif
       j = j + 1
      endif
CCC  Gravimeter's bias info
      b(j) = 1.d0/s0
      ib(j) = iibias+ibias
      j = j + 1
CCC  Gravimeter's nonlinear drift
      if (ldrift) then
       do 422 i = 1, ideg
        b(j) = (tt**i)/s0
        ib(j) = iidrift+idrift+i-1
        j = j + 1
422    enddo
      endif
CCC  Gravimeter's calibration function
      if (lsc.and.isc.ne.0) then
C       if(ipol.gt.0) then
CCCC  Scale factor part
        if(ipoldeg(idinst).eq.99) then
C         write(*,*)'pobs,s0:',pobs,s0
         b(j) = -pobs/s0
         ib(j) = iisc+isc
         j = j+1
        else
CCCC  Polynomial part
         do i = 1, ipol
          b(j)= (rd1**i)/s0
          ib(j) = iisc+isc+i-1
          j=j+1
         enddo
        endif
C       endif
CCCC  Periodical part for LCR G, D gravimeters
       k = -1
       do i = 1, iperr
        k = k+2
        b(j)= dcos(pi2*rd/perr(i))/s0
        ib(j) = iisc+isc+ipol+k-1
        b(j+1)= dsin(pi2*rd/perr(i))/s0
        ib(j+1) = iisc+isc+ipol+k
        j=j+2
       enddo
      endif
C
CCC  Obs info to coefficient matrix (extended)
      b(j) = (obs - pobs)
      ib(j) = n+1
C
      if(ltest) then
       write(*,*)'b:',(b(jj)*w,jj=1,j)
       write(*,*)'ib:',(ib(jj),jj=1,j)
      endif
CCC  Setup of coef. matrix coef(row,col) - observations stored by columns
      if (iloop.le.maxloop) then
       jj=j-1
C      write(*,*)'nnobs, j:',nnobs, jj
       if (jj.gt.maxjj) maxjj=jj
       if(maxjj.gt.maxj.or.nnobs.gt.maxobs) then
C     .STOP'Increase "maxj" or "maxobs"!'
        write(*,*)'maxjj=',maxjj,' , nnobs=',nnobs
        STOP': Increase parameters "maxj" or "maxobs"!'
       endif
       icoef(1,nnobs)=jj ! j-1 value stored as first element
       do i=1,jj
        if(ltest) write(*,*)'i, nnobs, b(i), ib(i):',i,nnobs,b(i),ib(i)
        coef(i,nnobs)=b(i)
        icoef(i+1,nnobs)=ib(i)
       enddo
      endif
CCC  Statistics
      gvvsum = gvvsum + (b(j)*w)**2
      pvvsum = pvvsum + (b(j)*w)**2
CCC  Setup of the normal matrix (by adding row of coefficient matrix)
      if (iloop.le.maxloop) then
       call addobsd2(c, nastat, n, b, ib, w)
      endif
C
      goto 70
c
75    if (iloop.eq.1) write(*,*)
     .'Number of readings in adjustment:      ',naobs
      if (naobs.gt.0) gvvsum = dsqrt(gvvsum/naobs)
c
c  scan through fixed obs
c  ----------------------
      do 76 i = 1, nfix
        if (iifix(i).eq.0) goto 76
        naobs = naobs+1
C        w = sigma0/sigfix(i)
        w = sigma0*sc_std/sigfix(i)
C  limit weight when sigma very large - why to limit? T.Oja
C        if (weight.gt.100) weight = 100
        j = ia(iifix(i))
        if (j.eq.0.or.j.gt.nastat) stop ': error in fixed obs prog!'
        b(1) = 1.d0
        ib(1) = j
        b(2) = gfix(i)-sol(j)
        ib(2) = n+1
        if(ltest) then
         write(*,*)'b:',(b(jj)*w,jj=1,2)
         write(*,*)'ib:',(ib(jj),jj=1,2)
        endif
        if (iloop.le.maxloop) then
         call addobsd2(c, nastat, n, b, ib, w)
        endif
        pvvsum = pvvsum + (b(2)*w)**2
76    continue
      if (iloop.eq.1) then
       write(*,*)'Total number of observation equations: ',naobs
       write(*,*)'Degr.of freedom (dof=obs.eq-unknowns): ',naobs-n
       if (naobs.lt.n)
     . stop ': Too few observations, degr.of freedom negative!'
      endif
C
C  apriori diagonal drift constraints
CCC  Necessary when poor drift control presented
      if (ldrift.and.driftpar.lt.99.d0.and.iloop.le.maxloop) then
        w = sigma0/driftpar
        b(2) = 0
        ib(2) = n+1
        do 77 i = 1, ndrift
          b(1) = 1.d0
          ib(1) = iidrift+i
          call addobsd2(c, nastat, n, b, ib, w)
77      continue
        write(*,771) driftpar
771     format(' Drift parameters constrained at rms:',
     .  f8.3, ' mGal/day')
      endif
C  statistics and residuals, exit last loop
C  ----------------------------------------
      if (nastat.gt.0.and.iloop.gt.1) write(*,78) gvvsum
78    format(' r.m.s. reading residuals (mGal): ',f15.5)
      sigma = -999.999     
C      if (naobs-n.gt.0) Stop'Error!'
      sigma = dsqrt(pvvsum/(naobs-n))
      if (iloop.gt.1) write(*,79) sigma
79    format(' SIGMA single reading = ',f15.5,' mGal')
      if (iloop.gt.maxloop) goto 100
c
      write(*,*)
      write(*,*) 'SOLUTION LOOP: ',iloop
c
c  solve equations
c  ---------------
      if(ltest) then
       fr='f16.4'
       do kk=1,icdim
        if(c(kk).gt.1e+09) fr='e16.4'
       enddo
       write(*,'(a20/,8(5'//fr//',/))')
     . ' test norm eq matrix',(c(kk),kk=1,icdim+1)
C       write(*,*)'write(*,791) (c(kk),kk=1,icdim+1)'
C       write(*,791) (c(kk),kk=1,icdim+1)
      endif
C791   format(' test norm eq matrix'/,8(5f16.4,/))

      do kk=1,icdim
       if(c(kk).gt.1e+09) then
         write(*,*)'WARNING: large numbers in normal matrix, computation
     1 floating point precision may cause wrong results! Modify RBIAS va
     2lue.'
        goto 80
       endif
      enddo
80    continue
c
C      write(*,*) 'icdim,n,nastat:',
C     1icdim,n,nastat
      call chold(c,n,nastat,nsing,cif,lsing)
c
CCC  Next part demonstrate that func. CHOLINV modify the right-hand
CCC  side of the vector c. Thus after inv=1,...,n call of CHOLINV there
CCC  is no need to call CHOLINV again- just use c(iirhs+1,...,iirhs+n)!
      if(ltest) then
       write(*,*) 'write(*,792) iirhs,(c(kk),kk=1,icdim+1)'
       write(*,792) iirhs,(c(kk),kk=1,icdim+1)
C       write(*,794) n,(cholinv(c,n,nastat,i),i=1,n)
      endif
792   format(' test after chold iirhs =',i5,/,8(5f16.4,/))
C794   format('n, cov vector:',i6,/,4(5f16.4,/))
c
      if (nsing.eq.0) then
        write(*,82) cif
82      format(' - solution OK, max loss of digits:',f5.1)
      else
        write(*,*) '- number of normal equation singularities: ',nsing
        do 83 i= 1,n
83      if (lsing(i)) write(*,*) '  singularity row no.: ',i
      endif
C
c
c  update solution vector
c  ----------------------
c
      solmax = -9.9d9
      solrms = 0
c last n parameters from c array
      do 90 i = 1, n
        r = c(i+iirhs)
        if (ltest) write(*,*) 'c(',i+iirhs,')','r=',r
        if (r.gt.solmax) solmax = r
        solrms = solrms + r**2
        sol(i) = sol(i) + r
90    continue
      if (iloop.gt.1) write(*,91) dsqrt(solrms/n), solmax
91    format(' r.m.s. and max parameter change: ',2f14.7)
      if (solmax.lt.1.d-6) iloop = maxloop+1
c
      if (ltest) then
       write(*,*)'write(*,786) (sol(i),i=1,n)'
       write(*,786) (sol(i),i=1,n)
      endif
786   format('test sol vector:'/,4(5f16.4,/))
c  go back to make residuals in start of next loop
c  -----------------------------------------------
c
C      if(.not.ltest) goto 60
      goto 60
c
c  exit of loops: make inverse for standard deviations or find single g
c  --------------------------------------------------------------------
c
100   rewind(idtmp)
      do 110 i = 1, nstat
        if (lslist(i)) then
          j = ia(i)
          g(i) = sol(j)
          sig(i) = dsqrt(cholinv(c,n,nastat,j))*sigma
           if(ltest) then
            write(idcov,*) 'Test cholinv:', j,
     1      cholinv(c,n,nastat,j)
            write(idcov,*) 'Vector c:',(c(jj+iirhs),jj=1,n)
           endif
          if (ldot.and.idot(i).ne.0) then
           j = idot(i)
           gdot(i) = sol(nastat+j)*1000.d0
           sigdot(i) = dsqrt(cholinv(c,n,nastat,nastat+j))*sigma*1000.d0
          else
           gdot(i)= 0.d0
           sigdot(i) = 0.d0
          endif
        else
105       read(idtmp) iseq,isno,istat,iyr,imonth,iday,ih,im,is,rd,obs,
     .    tt,iti,isc,idinst,ibias,idrift,ideg,w
          if (isno.ne.islist(i)) goto 105
          rd1=rd-av_rd
          r = sol(iibias+ibias)
C           write (*,*) 'r = ',r
          if (ldrift) then
           do 107 k = 1, ideg
107         r = r + sol(iidrift+idrift+k-1)*(tt**k)
          endif
          s = 1.d0
          ipol=ipoldeg(idinst)
          if (lsc.and.isc.ne.0) then
           if(ipol.eq.99) then
            s = sol(iisc+isc)
            ipol = 1
           else
            do k = 1, ipol
             r = r + sol(iisc+isc+k-1)*(rd1**k)
            enddo
           endif
           j = -1
           do k = 1, iper(idinst)
            j = j + 2
            x = sol(iisc+isc+ipol+j-1)
            y = sol(iisc+isc+ipol+j)
            r = r + x*dcos(pi2*rd/per(k,idinst))+
     1      y*dsin(pi2*rd/per(k,idinst))
           enddo
          endif
          g(i) = s*obs - r
          sig(i) = 99.999
        endif
110   continue
C
CC  Output of the adjustment results
CCC Full covariance matrix COV2 of the adjusted parameters
      call CHINV2(c, cov2, ginv, n, nastat, idiag, iirhs)
      if(ltest) then
       write(*,*) 'test COV2 vector:'
       write(*,*) (cov2(i),i=1,idiag)
       write(*,*) 'test ginv vector:'
       do i=1,n
        jj=i
        if(i.le.nastat) jj=1
        write(*,*) (ginv(indx(i,nastat)+j),j=1,jj)
       enddo
C        write(*,*) (ginv(i),i=1,iirhs)
       write(*,*)
       write(*,*)'test COEF matrix without abs.grav and drift info (row=
     11,...,',nnobs,';col=1,...,',maxjj,'):'
       do i=1,nnobs
        jj=icoef(1,i)
         do k=1,n
          b(k)=0.d0
          do ii=1,jj
           if(icoef(ii+1,i).eq.k) b(k)=coef(ii,i)
          enddo
         enddo
C        write(*,*) 'jj,icoef:',jj,',',(icoef(j+1,i),j=1,jj)
C        write(*,*) 'coef(row=',i,'):',(coef(j,i),j=1,jj)
        write(*,*) (b(k),k=1,n)
       enddo
      endif
C
CCCC Find diagonal elements of observations' cov. matrix Sb
      write(*,*)
      do 115 j=1,nnobs !Rows of coef. matrix and Sb
       sb(j)=0.d0
       sum=0.d0
       jj=icoef(1,j)
       do k=1,n !No of sums and Row no. of G_inv
        kk=k
        if(k.gt.jj) kk=jj
        do 113 i=1,kk
         ii=icoef(i+1,j)
         if(ii.gt.k) goto 114
CCCC  If element of G_inv from diagonal part then ...
         ic=ii
         if(k.le.nastat) then
          if(k.ne.ii) goto 113
          ic=1
         endif
         sum=sum+coef(i,j)*ginv(indx(k,nastat)+ic)
113     enddo
114     sb(j)=sb(j)+sum**2
        sum=0.d0
       enddo
115   enddo
C
      if(ltest) then
       write(*,*) 'test Sb vector:'
       sum=0.d0
       do j=1,nnobs
        write(*,*) 'sb(',j,')=',sb(j)
        sum=sum+sb(j)
       enddo
       write(*,*) 'trace(Sb)=',sum
      endif
C
      write(idcov,'((a),2(i4,(a)))')
     1'Lower triangular of symmetric covariance matrix (Cx) of adjusted
     2parameters with dim(',n,',',n,'):'
      do i=1,n
       ii=i*(i-1)/2
       write(idcov,796) (cov2(ii+j),j=1,i)
      enddo
796   format(100(100f18.6))
CCCC
C      write(idcov,796) (cholinv(c,n,nastat,i),i=1,n)
C796   format('cov vector:'/,4(5f16.4,/))
C      if(ltest) STOP '*** Temporary STOP for program TESTING ***'
CCCC critical t-value for statistical parameter testing is computed
CCCC t-distribution inversion (two-tailed, Q=1-A/2=1-(1-confl)/2)
      idf=naobs-n
      tinv=studin(0.5d0+confl/2.d0,idf)
CCC  After Vanicek et al. (1986) Q=1-A/(2*n) for multivariate stat.!??
C      tinv2=studin((2.0d0*n-1+confl)/(2.0d0*n),idf)
C      write(idg,*)'a1,tinv=',(1.0d0+confl)/(2.0d0),tinv
C      write(idg,*)'a2,tinv2=',(2.0d0*n-1+confl)/(2.0d0*n),tinv2
CCCC sort station list
      write(*,*)
      write(*,*) '- sorting results, output files: '
CCC  islist(i=1..nstat) will be sorted by the ino of stations,
CCC  islist(ix) is sorted array
      call sortin(islist,nstat,ix)
CCCC
C      write(*,*) gfile
C      write(*,*) rfile
C      write(*,*) tfile
C      write(*,*)
CCCCC GRAV FILE CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      write(idg,1250) projfile, sc_std
1250  format('# Adjusted results for project: ',a,//
     .'# ======= Fixed stations and adjustment residuals (mGal) ======='
     .,/'# seq   stat      fix g   sigma   weight        adj g       res
     .  statname',/
     .'# no      ID             (sc_std=1/',f5.1,')')
      jknr=1
      s2u=1000.d0*sigma
CCC Generate header for FILE with adjusted gravity ties
      write(idta,1350)
1350  format('  from     /      to         adj.dg(mGal)+/- std(uGal)')
CCC Print the list of fixed stations
      do i = 1, nfix
       if (iifix(i).gt.0) then
        ii=ia(iifix(i))
        iii=ii*(ii-1)/2
        write(idg,140) jknr,isfix(i),gfix(i)+gbias,sigfix(i)/sc_std,
     1  (sigma0*sc_std/sigfix(i))**2,sol(ii)+gbias,sol(ii)-gfix(i),
     2  stxt(ii)
CCCCC Output of adjusted differences between fixed points with standard deviations from full Cx matrix
        do j = i+1, nfix
         if (iifix(j).gt.0) then
          jj=ia(iifix(j))
          jjj=jj*(jj-1)/2
          kk=jjj+ii
          if(ii.gt.jj) kk=iii+jj
          tstd=s2u*dsqrt(cov2(iii+ii)+cov2(jjj+jj)-2*cov2(kk))
          stat=dabs(sol(jj)-sol(ii))*1000.d0/tstd
          test='  '
          if(stat.le.tinv) test=' !'
          write(idta,1400) stxt(iifix(i)),stxt(iifix(j)),
     1    sol(jj)-sol(ii),tstd,test
C     2    s2u*dsqrt(cov2(iii+ii)+cov2(jjj+jj))
         endif
        enddo
CCCCC Output of adjusted differences between fixed and adjusted points with std from Cx
        do j = 1,nstat
C        write(idcov,*) stxt(ix(j)),listno(isfix,nfix,islist(ix(j))),
C     1  dabs(sig(ix(j))-99.999)
         if (listno(isfix,nfix,islist(ix(j))).eq.0) then
          jj=ia(ix(j))
C          if(abs(sig(ix(j))-99.999).gt.0.01) then
          if(jj.gt.0) then
           jjj=jj*(jj-1)/2
           kk=jjj+ii
           if(ii.gt.jj) kk=iii+jj
           tstd=s2u*dsqrt(cov2(iii+ii)+cov2(jjj+jj)-2*cov2(kk))
           stat=dabs(sol(jj)-sol(ii))*1000.d0/tstd  !! Fixed value for fixed point is used here!?
           test='  '
           if(stat.le.tinv) test=' !'
           write(idta,1400) stxt(iifix(i)),stxt(ix(j)),
     1     g(ix(j))-sol(ii),tstd,test
C     2     s2u*dsqrt(cov2(iii+ii)+cov2(jjj+jj))
          else
           write(idta,1410) stxt(iifix(i)),stxt(ix(j)),
     1     g(ix(j))-sol(ii)
          endif
         endif
        enddo
CCCCC Fixed stations not observed
       else
        write(idg,141) jknr,isfix(i),gfix(i)+gbias,sigfix(i)/sc_std
       endif
       jknr=jknr+1
       write(idta,*)
      enddo
140   format(i3,i9,f13.4,1x,f6.4,1x,f7.2,2x,f13.4,1x,f8.4,2x,a12)
141   format(i3,i9,f13.4,1x,f6.4,'  -- not observed -- ')
1400  format(a12,' --> ',a12,f12.4,' +/-',f7.1,a2)
1410  format(a12,' --> ',a12,f12.4)
c
      write(idg,1260)
1260  format(/,'# ======= Adjusted results with standard deviations (mGa
     .l) ======')
      if(.not.ldot) then
       write(idg,1265) sc_std
      else
       write(idg,1266) sc_std
      endif
1265  format(
     .'# seq    stat     gravity     std      statname',/
     .'# no       ID             (sc_std=',f5.1,')')
1266  format(
     .'# seq    stat     gravity     std   g-dot stdev t-stat    statnam
     .e',/
     .'# no      ID              (sc_std=',f5.1,')')
CCCCCCCCCCCCC  To test different arrays  CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      if(ltest) then
       do i = 1, nfix
        write(*,*)'Test: i, iifix(i), ia(iifix(i)), stxt(iifix(i)), isfi
     .x(i)'
        write(*,*) i, iifix(i), ia(iifix(i)), stxt(iifix(i)), isfix(i)
       enddo
       write(*,*)
       do i=1,nstat
        write(*,*)'Test:i(1->nstat),ix(i),islist(ix(i)),stxt(ix(i)),ia(i
     .),ia(ix(i))'
        write(*,*) i,ix(i),islist(ix(i)),stxt(ix(i)),ia(i),ia(ix(i))
       enddo
      endif
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCC  Output of adjusted gravity values and their differences
      jknr=1
      do 120 i = 1, nstat
        if (listno(isfix,nfix,islist(ix(i))).gt.0) goto 120
        ii=ia(ix(i))
C        if (abs(sig(ix(i))-99.999).gt.0.01) then
        if (ii.gt.0) then
         if(.not.ldot) then
          write(idg,121) jknr,islist(ix(i)),g(ix(i))+gbias,
     .    sig(ix(i))*sc_std,stxt(ix(i))
         else
          stat=dabs(gdot(ix(i)))/sigdot(ix(i))
          test='   '
          if(stat.le.tinv) test=' ! '
          write(idg,125) jknr,islist(ix(i)),g(ix(i))+gbias,
     .    sig(ix(i))*sc_std,gdot(ix(i)),sigdot(ix(i)),stat,test,
     .    stxt(ix(i))
         endif
        else
          write(idg,122) jknr,islist(ix(i)),g(ix(i))+gbias,stxt(ix(i))
        endif
        jknr=jknr+1
CCCCC Output of adjusted differences between adjusted points
        do j=i+1, nstat
          if(listno(isfix,nfix,islist(ix(j))).eq.0) then
           jj=ia(ix(j))
           if(ii.gt.0.and.jj.gt.0) then
CCCC Compute stdev from COV2 array
            iii=ii*(ii-1)/2
            jjj=jj*(jj-1)/2
CCCC IF-statement to find correct index for Cx element
            if(ii.lt.jj) then
             kk=jjj+ii
            else
             kk=iii+jj
            endif
            tstd=s2u*dsqrt(cov2(iii+ii)+cov2(jjj+jj)-2*cov2(kk))
            stat=dabs(g(ix(j))-g(ix(i)))*1000.d0/tstd
            test='  '
            if(stat.le.tinv) test=' !'
            write(idta,1400) stxt(ix(i)),stxt(ix(j)),
     1      g(ix(j))-g(ix(i)),tstd,test
            if(ltest) then
             write(idcov,*)'i,j,ii,jj,iii,jjj,kk:',i,j,ii,jj,iii,jjj,kk
             write(idcov,*)
     1       's2u,cov2(iii+ii),cov2(jjj+jj),cov2(kk):'
     2       ,s2u,cov2(iii+ii),cov2(jjj+jj),cov2(kk)
             write(idcov,*)'SUM of cov2s:',
     1       cov2(iii+ii)+cov2(jjj+jj)-2*cov2(kk)
            endif
           else
            write(idta,1410) stxt(ix(i)),stxt(ix(j)),
     1      g(ix(j))-g(ix(i))
           endif
          endif
         enddo
        write(idta,*)
CCCCCC FORMATTING of output
121     format(i4,i9,2x,f11.4,1x,f7.4,5x,a12)
122     format(i4,i9,2x,f11.4,'    N/A ',5x,a12)
125     format(i4,i9,2x,f11.4,1x,f7.4,1x,3f6.1,a3,2x,a12)
120   continue
CCCC  Other adjusted parameters (e.g. calibration)
      if (lsc) then
        write(idg,*)
        write(idg,*) '=== Calibration results of gravimeters ==='
C        write(idg,*) '   inst.no, polynomial, periodical terms :'
        do i = 1, nrinst
         if(iiisc(i).ne.0) then
C          write(idg,*)
          write(idg,*) 'Instrument no:',instlist(i)
CCCC Linear scale factor or polynomial function terms...
          if(ipoldeg(i).gt.0) then
           ipol=ipoldeg(i)
           if(ipol.eq.99) then
            write(idg,*)'# Term of linear scale factor (deg=99):'
            write(idg,*)'# coef.  with  std.dev      t-stat.'
            ii=iisc+iiisc(i)
            stat=dabs(1-sol(ii))/(dsqrt(cholinv(c,n,nastat,ii))*sigma)
            test='  '
            if(stat.le.tinv) test=' !'
            fr='F14.10'
            write(idg,'(2'//fr//',1x,f7.1,a2)')
     .      sol(ii),dsqrt(cholinv(c,n,nastat,ii))*sigma,stat,test
            ipol=1
           else
            write(idg,*)'# Terms of polynomial calibration function:'
            write(idg,*)'# deg.  coef. with std.dev      t-stat.'
            do j=1,ipol
C            write(idg,'(i3,2f12.6)') j,sol(iisc+iiisc(i)+j-1),
             ii=iisc+iiisc(i)+j-1
             stat=dabs(sol(ii))/(dsqrt(cholinv(c,n,nastat,ii))*sigma)
             test='  '
             if(stat.le.tinv) test=' !'
             fr='E14.6'
             write(idg,'(i3,2'//fr//',1x,f7.1,a2)')
     .       j,sol(ii),dsqrt(cholinv(c,n,nastat,ii))*sigma,stat,test
CCCC 1st term of polynomial calibration function, subtracted from 1 (1.0-coef.):'
             if(j.eq.1.and.sol(ii).lt.1.d0)
     1        write(idg,'(a1,i2,2f14.10,a1,a57)')
     2       '(',j,1.d0-sol(ii),dsqrt(cholinv(c,n,nastat,ii))*sigma,')',
     3       '-> 1st term 1-coef. value (compatible with deg=99 term)'
C     3       '# This line gives (1.0-coef.) value of the 1st term'
            enddo
           endif
          endif
CCCC Periodical function terms...
          k = -1
          if(iper(i).ne.0) then
           write(idg,*)
           write(idg,*)'# Terms of periodical calibration function:'
           write(idg,*)'# Per.(CU),  x(cos) with st.dev, t-test, y(sin) w
     .ith st.dev, t-test [UNIT=uGal]'
           jj=0
           do j=1,iper(i)
            k = k + 2
            kj=iisc+iiisc(i)+ipol+k-1
            iii=kj*(kj-1)/2
            jjj=(kj+1)*kj/2
            x = sol(kj)
C            dx = dsqrt(cholinv(c,n,nastat,kj))*sigma
            y = sol(kj+1)
C            dy = dsqrt(cholinv(c,n,nastat,kj+1))*sigma
            dx=dsqrt(cov2(iii+kj))*sigma
            dy=dsqrt(cov2(jjj+kj+1))*sigma
C            write(*,*)'TEST, kj,dx,dy:',kj,cholinv(c,n,nastat,kj),
C     .      cholinv(c,n,nastat,kj+1)
C            write(*,*)'TEST, dx,dy:',ddx,ddy
C           write(*,*)'TEST, i,j,covx,covy:',iii,jjj,jjj+kj,cov2(iii+kj),
C     1      cov2(jjj+kj+1),cov2(jjj+kj)
            stat=dabs(x)/dx
            stat2=dabs(y)/dy
            test='  '
            if(stat.le.tinv.or.stat2.le.tinv) test=' !'
C          write(idg,'(f10.4,',',2(f10.4,' +/-',f8.4,',',f7.1),a2)')
C     1    per(j,i),x*1000,dx*1000,stat,y*1000,dy*1000,stat2,test
           write(idg,128) per(j,i),x*1000,dx*1000,stat,y*1000,dy*1000,
     1     stat2,test
128        format(f10.4,',',2(f10.4,' +/-',f8.4,',',f7.1),a2)
           jj=jj+1
           amp(jj) = dsqrt(x**2+y**2)
C          damp(jj) = dsqrt((x*dx)**2+(y*dy)**2)/amp(jj)
           phs(jj) = datan(x/y)*(360.d0/pi2)
C           dphs(jj) = dsqrt((y*dx)**2+(x*dy)**2)/(amp(jj)**2)
C     1     *(360.d0/pi2)
           damp(jj) = sigma*dsqrt(cov2(iii+kj)*x**2+cov2(jjj+kj+1)*y**2+
     1     2*x*y*cov2(jjj+kj))/amp(jj)
           dphs(jj) = sigma*dsqrt(cov2(iii+kj)*y**2+cov2(jjj+kj+1)*x**2-
     1     2*x*y*cov2(jjj+kj))/(amp(jj)**2)*(360.d0/pi2)
CCC Missing covariance parts in previously found st.dev-s!!! SOLVED!
            if(y.lt.0.d0) then
             phs(jj) = phs(jj) + 180.d0
            else
             if(phs(jj).lt.0.d0) phs(jj)=phs(jj)+360.d0
            endif
           enddo
C           if(iper(i).ne.0) write(idg,*)'..and ampl.A(uGal) and phase for !this if not neccessary?
          write(idg,*)'#..and ampl.A(uGal) and phase for A*sin(2*pi*z/pe
     1riod+phase) with stdev-s'
C           if(iper(i).ne.0) write(idg,*)'..and ampl.A with stdev(uGal),ph
C     .ase with stdev,for A*sin(2*pi*z/period+phase)'
           do j=1,iper(i)
            write(idg,130)
     1      per(j,i),amp(j)*1000,phs(j),damp(j)*1000,dphs(j)
C     1      per(j,i),amp(j)*1000,damp(j)*1000,phs(j),dphs(j)
           enddo
          endif ! if(iper(i).ne.0) then ...
130       format(3(f10.4,','),' +/-',f10.4,', +/-',f10.4)
C          write(idg,132) instlist(i),sol(iisc+iiisc(i)),
C     .    sqrt(cholinv(c,n,nastat,iisc+iiisc(i)))*sigma
         else
          write(idg,*)
          write(idg,*)'Instrument no:',instlist(i),' - not calibrated'
         endif
        enddo
      endif
C132   format(i8,2f11.6)
c
C      write(idg,*)
C      write(idg,*) '=== Statistics of adjustment ==='
      write(idg,138) naobs,nastat,n,naobs-n,sigma0,sigma
138   format(/
     .'=== Statistics of adjustment ===',/
     .' Adjustment observations:',i6/
     .' Stations:               ',i6/
     .' Total unknowns:         ',i6/
     .' Degrees of freedom:     ',i6/
     .' SIGMA1 (apriori st.dev. of unit weight, mGal)    : ',f8.4/
     .' SIGMA2 (aposteriori st.dev. of unit weight, mGal): ',f8.4)
      write(idg,*)
CCCC Statistical tests
      write(idg,'(a)') '=== Statistical tests ==='
      write(idg,'((a),f6.3,/)')' Confidence level:     ', confl
      write(idg,'((a),f6.2)')
     1'== Critical t-value for statistical parameter testing: t-crit='
     2,tinv
      write(idg,'((a),/)')
     1' NB! If t-stat > t-crit -> parameter significant, else with " ! "
     2 mark.'
C
CCCC  Chi-square test of variance factor (SIGMA1)
      chiv=pvvsum/idf
      stat=chiv/(sigma0**2)
      chi1=chi2inv((1-confl)/2,idf)/idf
      chi2=chi2inv((1+confl)/2,idf)/idf
      chik=(chi1+chi2)/2
      test='FAILED'
      if(stat.gt.chi1.and.stat.lt.chi2) test='PASSED'
      write(idg,142) stat,chi1,chi2,test
142   format(
     .'== Chi-square test of the SIGMA1 (variance factor) ==',/
     .' Statistic (norm. with dof): X^2 = ',f5.2/
     .'                 X^2 lower value = ',f5.2/,
     .'                 X^2 upper value = ',f5.2,'  ->  test ',a6,'!')
CCCC Ratio to suggest possible best SIGMA1 value
      if(test.eq.'FAILED') write(idg,1280) dsqrt(chiv/chik)
1280  format(/'== Suggested SIGMA1 value:',f8.4)
CCCC The computation of Akaike's,Bayesian Information Criteria (AIC, BIC)
CCCC to select the functional model which fits best observations with the
CCCC smallest set of parameters (to avoid overfitting) in the adjustment.
CCCC The lowest AIC, BIC indicate the best model but it is relative measure -
CCCC within a set of models. Thus effort by user must be made to ensure
CCCC well founded models: AIC = 2K + N*ln(RSS), BIC = AIC + K(ln(N)-2)
CCCC where K = n + 1, N = naobs in GRADJ3
      write(idg,1290)
1290  format(/'== Information Criteria AIC, BIC as relative measures'/,
     .'== to select the best adjustment (the lower is better):')
      kn = n+1
      rss = pvvsum*1000.d0                 !RSS scaled up to uGal for positive ln() result -> in f77 log()!
      aic_c = 2*kn*(kn+1)/(naobs-kn-1)     !Correctional term for small naobs, generally insignificant
      aic = 2*kn + naobs*dlog(rss) + aic_c   !Original approach
C      aicm = 2*kn - naobs*dlog(pvvsum) + aic_c  !Modif. approach by TOja, not correct
      bic = aic + kn*(dlog(dble(naobs))-2)
      write(idg,1300) aic, bic
1300  format(' # AIC = ',f10.2,', BIC = ',f10.2)
C      write(idg,*)'TEST: 2*kn,naobs,pvvsum,dlog(pvvsum),aic_c:',
C     .2*kn,naobs,pvvsum,dlog(pvvsum),aic_c
C      write(idg,*)'TEST: kn,dble(naobs),dlog(dble(naobs)):',
C     .kn,dble(naobs),dlog(dble(naobs))
CCCCC RESI FILE CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      write(idr,'(a)') projfile
      write(idr,'(a)')
     1' ---------------------------------------------------------------'
      write(idr2,'(a)') projfile
      write(idr2,'(a)')
     1' ---------------------------------------------------------------'
CCCC Critical tau value for Pope's tau-test to tests statistically residuals
CCCC (observations) to find blunders (gross errors)
C      taui=taure(nnobs,idf,(1-confl))
      taui=taure(nnobs,idf,confl)
C      taui2=taure(nnobs,idf,1-(1-confl)/(2*nnobs))
C      taui3=taure(nnobs,idf,1-(1-confl**(1/nnobs))/2)
C      taui2=taure(6,4,0.05d0) !Test TAURE, compare with Koch(1999),pp 307
      write(idr,'(a)')
     1' Critical values for parameter and residual statistical tests:'
      write(idr,'(3((a),f6.2),/)')
     1' conf.level=',confl,', t-crit=',tinv,', tau-crit=',taui
      write(idr2,'(a)')
     1' Critical values for parameter and residual statistical tests:'
      write(idr2,'(3((a),f6.2),/)')
     1' conf.level=',confl,', t-crit=',tinv,', tau-crit=',taui
CCCC
      jj=gsc
      gsc=gsc/1000  !conversion from uGal to mGal
      write(idr,*)'=== Residuals of the readings with tare and drift inf
     .o ==='
      write(idr2,*)'=== AVERAGED readings, residuals etc with tare and d
     .rift info ==='
      write(idr,1450)
      write(idr2,1460)
      write(idr,143) jj
143   format(
     1' station                      instr.  oID  red.obs            dri
     2ft    res  st.res   red.no   res.plot    station'/,
     3'      ID     date,      time     ID         (mGal)   weight  |---
     4(uGal)---|     tau-test   (',i2,' uGal/div)  name')
      write(idr2,144) jj
144   format(
     1' station                      instr. seqno  oID  red.obs
     2    drift   res  st.res   red.no   res.plot    station'/,
     3'      ID     date,     time      ID               (mGal)   weight
     4  |---(uGal)---|     tau-test   (',i2,' uGal/div)  name')
      write(idr,1450)
      write(idr2,1460)
1450  format(1x,112('-'))
1460  format(1x,118('-'))
C
      cplot(1) = 'X     !      '
      cplot(2) = '*     !      '
      cplot(3) = ' *    !      '
      cplot(4) = '  *   !      '
      cplot(5) = '   *  !      '
      cplot(6) = '    * !      '
      cplot(7) = '     *!      '
      cplot(8) = '      *      '
      cplot(9) = '      !*     '
      cplot(10)= '      ! *    '
      cplot(11)= '      !  *   '
      cplot(12)= '      !   *  '
      cplot(13)= '      !    * '
      cplot(14)= '      !     *'
      cplot(15)= '      !     X'
c
      write(idt,161)
161   format(3x,'from    /   to',7x,'inst.no',2x,'date',8x,'time',5x,
     1'dt(hr)',1x,'dg(mGal)',1x,'dD(uGal)',1x,'dg+dD',4x,'dv',3x
     2,'dg+dD+dv')
C      write(idgn,164)
C164   format('start, end, difference(mgal), mjd(from), mjd(to),',
C     .' reading(from,CU), reading(to,CU), st.dev.(mgal)')
C
      jknr=1   !! No of ties (or averaged readings)
      ino=0    !!Sequence of adjusted reading
      idinst = 0
      idinst0 = idinst
      ibias0 = 0
      idrift0 = 0
      triiv0 = 0
      isno00 = 0
      istat00 = 0
      nv = 0
      gvvsum = 0
      wnv = 0
      wgvvsum = 0
      nv2 = 0
      gvvsum2 = 0
      wnv2 = 0
      wgvvsum2 = 0
      summ=0
C      stdev=dsqrt(2.d0)*sigma !For GRAVNET output?
      ikobs = 1
      iseq0 = 1
CCCC iobs again in use as a number of readings per point obs
      iobs = 0
      ibias1 = ibias0
      istat00 = istat01
      rewind(idtmp)
c
c  output residual loop
c  --------------------
150   continue
      read(idtmp,end=170) iseq,isno,istat,iyr,imonth,iday,ih,im,is,rd,
     .obs,tt,iti,isc,idinst,ibias,idrift,ideg,w
      iastat = ia(istat)
      iidot = idot(istat)
      ipol = 0
      iperr = 0
C
      if (lsc.and.isc.ne.0) then
       ipol = ipoldeg(idinst)
       iperr = iper(idinst)
       do i=1,iperr
        perr(i) = per(i,idinst)
       enddo
      endif
C
CCCCC Output of average observation data per station into resi2
      if (ibias0.gt.0.and.(ibias.ne.ibias0.or.istat.ne.istat00)) then
       call PETGREN3(adt/iobs+2400000.5D0,iyra,imontha,idaya,iha,ima,isa
     1 ,NERR)
       i = nint((av/iobs)/gsc)
       if (i.lt.-7) i = -7
       if (i.gt.7) i = 7
       test='  '
       if(astres/iobs.gt.taui) test=' !'
       if (ia(istat00).ne.0) then
       write(idr2,146) isno00,iyra,imontha,idaya,iha,ima,isa
     1 ,instlist(idinst0),ikobs,iseq0,aobs/iobs,(aw/iobs)**2
     2 ,(atriiv/iobs)*1000.d0,(av/iobs)*1000.d0,astres/iobs,test
     3 ,ared/iobs,cplot(i+8),stxt(istat00)
       else
       write(idr2,147) isno00,iyra,imontha,idaya,iha,ima,isa
     1 ,instlist(idinst0),ikobs,iseq0,aobs/iobs,(aw/iobs)**2
     2 ,(atriiv/iobs)*1000.d0,stxt(istat00)
       endif
146    format(i8,i6,2('-',i2.2),', ',i2.2,2(':',i2.2),i5,2i6,f11.4,f7.2,
     1 f9.1,2f7.1,a2,f7.1,1x,a13,1x,a12)
147    format(i8,i6,2('-',i2.2),', ',i2.2,2(':',i2.2),i5,2i6,f11.4,f7.2,
     . f9.1,4x,2('---',4x),2x,'---',15x,a12)
C
CCCC Gravity ties between successive points (for *.ties file)
CCCC Computations are one step behind therefore 00 and 01!
C       write(idt,*) istat01,istat00,ibias1,ibias0
       if(istat01.ne.istat00.and.ibias1.eq.ibias0) then
        call PETGREN3(adt0+2400000.5D0,iyra,imontha,idaya,iha,ima,isa
     1  ,NERR)
CCC  If station no needed (not names), just replace stxt(istat) with istat 
CCC  below and modify formats of 162, 163
       if(ia(istat01).ne.0.and.ia(istat00).ne.0) then
        if (isno01.lt.isno00) then
         write(idt,162)
     1   stxt(istat01),stxt(istat00),instlist(idinst0),iyra,imontha,
     2   idaya,iha,ima,isa,(adt/iobs-adt0)*24.0d0,aobs/iobs-aobs0,
     3   (atriiv0-atriiv/iobs)*1000.d0,
     4   aobs/iobs-aobs0+(atriiv0-atriiv/iobs),
     5   (av/iobs-av0)*1000.d0,
     6   aobs/iobs-aobs0+(atriiv0-atriiv/iobs)+(av/iobs-av0)
        else
         write(idt,162)
     1   stxt(istat00),stxt(istat01),instlist(idinst0),iyra,imontha,
     2   idaya,iha,ima,isa,(adt/iobs-adt0)*24.0d0,-(aobs/iobs-aobs0),
     3   -(atriiv0-atriiv/iobs)*1000.d0,
     4   -(aobs/iobs-aobs0)-(atriiv0-atriiv/iobs),
     5   -(av/iobs-av0)*1000.d0,
     6   -(aobs/iobs-aobs0)-(atriiv0-atriiv/iobs)-(av/iobs-av0)
        endif
       else
         write(idt,163)
     1   stxt(istat01),stxt(istat00),instlist(idinst0),iyra,imontha,
     2   idaya,iha,ima,isa,(adt/iobs-adt0)*24.0d0,aobs/iobs-aobs0,
     3   (atriiv0-atriiv/iobs)*1000.d0,
     4   aobs/iobs-aobs0+(atriiv0-atriiv/iobs)
       endif
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
162   format(a12,1x,a12,1x,i5,i6,2('-',i2.2),', ',i2.2,2(':',i2.2),f7.3
     .,f10.4,2(f6.1,f10.4))
163   format(a12,1x,a12,1x,i5,i6,2('-',i2.2),', ',i2.2,2(':',i2.2),f7.3
     .,f10.4,f6.1,f10.4,3x,3('-'),3x,7('-'))
C
       jknr=jknr+1
C162   format(a12,1x,a12,'  G-',i4,'  ',i6,',',2f7.3,4f10.4)
C        isno01 = isno00
C        iastat01 = iastat00
C        istat01 = istat00
C
CCCC Output for GRAVNET input: start, end, difference(mgal), mjd(from),
CCCC mjd(to),reading(from,CU), reading(to,CU), st.dev.(mgal)
C       ddobs=dobs+dtriiv
C       write(idgn,160) stxt(istat00),stxt(istat),ddobs,tt00+t0(iti),
C     . tt+t0(iti),rd00,rd,stdev
C160    format(a12,1x,a12,f10.4,2f14.6,2f11.4,f6.3)
C       tt00 = tt
C       rd00 = rd
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
       endif
C        idinst1=idinst0
CCCC There should be average time for the tie but at the moment starting time
        adt0=adt/iobs
        aobs0=aobs/iobs
C        aw0=aw/iobs
        atriiv0=atriiv/iobs
        av0=av/iobs
        ibias1=ibias0
        istat01=istat00
        isno01=isno00
CCCCC Next initialisation
       iobs=0
       adt=0.d0
       aobs=0.d0
       aw=0.d0
       atriiv=0.d0
       av=0.d0
       astres=0.d0
       ared=0.d0
       ikobs=ikobs+1
       iseq0=iseq
      endif
CCCC Epoch initialization for ldot
      call MJD2(iyr,imonth,iday,ih,im,is,dt,tdot)
C      if(ino.eq.0) write(idg,450) tdot
C450   format(/,' # Common epoch:',f10.3)
      if(ldot.and.ino.eq.0) write(idg,450) tdot
450   format(/,' # Epoch initialization:',f10.3)
      tdot = tdot - epoch
C
      if(ltest) then
       if(jknr.lt.1)
     1write(*,*)'iseq,isno,iastat,iyr,imonth,iday,ih,im,is,rd,obs,tt,iti
     2,isc,ibias,idrift,istat'
      write(*,*) iseq,isno,iastat,iyr,imonth,iday,ih,im,is,rd,obs,tt,iti
     1,isc,ibias,idrift,istat
      endif
CCCCC
      if (idinst.ne.idinst0.and.nv.gt.0) then
CCC  RMS, WRMS of single session
       if(nv2.gt.0) then
        write(idr,153) dsqrt(gvvsum2/nv2)*1000.d0,
     1  dsqrt(wgvvsum2/wnv2)*1000.d0
        write(idr2,153) dsqrt(gvvsum2/nv2)*1000.d0,
     1  dsqrt(wgvvsum2/wnv2)*1000.d0
153     format('Set RMS of res: ',f8.1,' uGal'/,
     1         '   WRMS of res: ',f8.1,' uGal'/,
     2         ' ----------------------------')
        nv2 = 0
        gvvsum2 = 0
        wnv2 = 0
        wgvvsum2 = 0
       endif
CCC  RMS, WRMS of the whole set
       write(idr,151) dsqrt(gvvsum/nv)*1000.d0,
     1 dsqrt(wgvvsum/wnv)*1000.d0
       write(idr2,151) dsqrt(gvvsum/nv)*1000.d0,
     1 dsqrt(wgvvsum/wnv)*1000.d0
151    format('Total(prev sets) RMS of residuals: ',f8.1,' uGal'/,
     1        '                WRMS of residuals: ',f8.1,' uGal'/,
     2        '------------------------------------------------')
       nv = 0
       wnv = 0
       gvvsum = 0
       wgvvsum = 0
       ikobs=1
C        write(idr,152) instname,instlist(idinst)
C152     format('# ',a3,'-',i4)
      endif
CCCCCC  DRIFT info output to res file
      if (ldrift.and.idrift.ne.idrift0) then
CCC  RMS, WRMS of single session (separated with new drift comp)
       if(nv2.gt.0) then
        write(idr,153) dsqrt(gvvsum2/nv2)*1000.d0,
     1  dsqrt(wgvvsum2/wnv2)*1000.d0
        write(idr2,153) dsqrt(gvvsum2/nv2)*1000.d0,
     1  dsqrt(wgvvsum2/wnv2)*1000.d0
        nv2 = 0
        gvvsum2 = 0
        wnv2 = 0
        wgvvsum2 = 0
       endif
CCC
       write(idr,*) 'DRIFT degree =',ideg
       write(idr2,*) 'DRIFT degree =',ideg
C       write(idt,*) 'DRIFT degree =',ideg
       do 798 k = 1, ideg
        stat=dabs(sol(iidrift+idrift+k-1))/
     .(dsqrt(cholinv(c,n,nastat,iidrift+idrift+k-1))*sigma)
        test=')  '
        if(stat.le.tinv) test=') !'
        write(idr,155) k,sol(iidrift+idrift+k-1)*1000,
     1dsqrt(cholinv(c,n,nastat,iidrift+idrift+k-1))*sigma*1000,k,stat,
     2test
        write(idr2,155) k,sol(iidrift+idrift+k-1)*1000,
     1dsqrt(cholinv(c,n,nastat,iidrift+idrift+k-1))*sigma*1000,k,stat,
     2test
798    enddo
155   format(' DRIFT of',i3,' order:',f10.1,' +/-',f10.1,' uGal/day^',i1
     1,' (t-stat=',f6.2,a3)
CCCCCC  Drift plot to output
C        i = nint(sol(iidrift+idrift)/0.070)
C        if (i.lt.-7) i = -7
C        if (i.gt.7) i = 7
C        write(*,'(i6,f7.2,2f9.1,a15)')iday,t,-sol(iidrift+idrift)*1000,
C     .  dsqrt(cholinv(c,n,nastat,iidrift+idrift))*sigma*1000,cplot(i+8)
      endif
CCCCCC  BIAS and TARE info output to res file
      if (ibias.ne.ibias0) then
       triiv0 = 0.d0
       do 805 k = 1, ideg
        triiv0 = triiv0 + sol(iidrift+idrift+k-1)*(tt**k)
805    enddo
        i=iibias+ibias
        if (idinst.eq.idinst0) then
CCCC Compute stdev of TARE and test significance of TARE
         ii=i*(i-1)/2
         j=i-1
         jj=j*(j-1)/2
         tstd=sigma*dsqrt(cov2(ii+i)+cov2(jj+j)-2*cov2(ii+j))
         stat=dabs(sol(i)-sol(j))/tstd
         test=')  '
         if(stat.le.tinv) test=') !'
CCCCCC test print
C      write(idr,*)
C     1'cholinv of bias(',i,'), bias(',j,')=',
C     2 cholinv(c,n,nastat,i),cholinv(c,n,nastat,j)
C      write(idr,*)
C     1'cov. of bias(',i,i,'), bias(',j,j,')=',cov2(ii+i),cov2(jj+j)
C      write(idr,*)
C     1'cov. of bias(',i,j,')=',cov2(ii+j)
CCCCCC
C         write(idr,156) (sol(i)-rbias)*1000.d0,
C     1   dsqrt(cholinv(c,n,nastat,i))*sigma*1000.d0,
C     2   (sol(i)-sol(j))*1000.d0,tstd*1000.d0,stat,test
         write(idr,156) (sol(i)-sol(j))*1000.d0,tstd*1000.d0,stat,test
         write(idr2,156) (sol(i)-sol(j))*1000.d0,tstd*1000.d0,stat,test
        else
         write(idr,157)
     1   (sol(i)-rbias)*1000.d0,
     2   dsqrt(cholinv(c,n,nastat,i))*sigma*1000.d0
         write(idr2,157)
     1   (sol(i)-rbias)*1000.d0,
     2   dsqrt(cholinv(c,n,nastat,i))*sigma*1000.d0
        endif
      endif
C156   format(' BIAS parameter:',f15.1,' ±',f7.1,' uGal'/,
C     .      '            TARE:',f10.1,' ±',f7.1,' uGal (t-stat=',f6.2,a3)
156   format('              TARE:',f10.1,' +/-',f7.1,' uGal   (t-stat=',
     1f6.2,a3)
157   format(' BIAS parameter:',f15.1,' +/-',f7.1,' uGal')
C
      triiv = 0.d0
      do 810 k = 1, ideg
       triiv = triiv + sol(iidrift+idrift+k-1)*(tt**k)
810   enddo
CCCC Computation of residuals
      if (iastat.ne.0) then
       v=predobs(sol,iastat,iidot,tdot,ibias,idrift,ideg,tt,idinst,
     1 isc,ipol,iperr,perr,rd)-obs
CCCCC Write residuals into file RESID to plot histogram
       write(idres,*) v*1000.d0
C       write(idres,*) instlist(idinst),rd, v*1000
C
CCCC Find redundancy number (red), standard deviation of the residual (vstd)
CCCC and standardized residual (stres)
       ino=ino+1
       red=1-sb(ino)*(w**2)
       vstd=sigma*dsqrt((1/(w**2))-sb(ino))
       stres=-99.9d0
       if(vstd.ne.0.0d0) stres=dabs(v)/vstd
C
       test='  '
       if(stres.gt.taui) test=' !'
CCCC Check: does the trace of Qvv*P (cofactor matrix of residuals times
CCCC weight matrix P) tr(Qvv*P)=idf (see Koch (1999) pp. 305)
       if(ltest) summ=summ+(1-sb(ino)*w**2)
C       write(idr,*) 'sigma,w,sb(ii),vstd=',sigma,w,sb(ino),vstd
C
       nv = nv+1
       wnv = wnv+w**2
       gvvsum = gvvsum + v**2
       wgvvsum = wgvvsum + (v*w)**2

       nv2 = nv2+1
       wnv2 = wnv2+w**2
       gvvsum2 = gvvsum2 + v**2
       wgvvsum2 = wgvvsum2 + (v*w)**2
C
C       i = nint(v/0.0100)
       i = nint(v/gsc)
       if (i.lt.-7) i = -7
       if (i.gt.7) i = 7
       write(idr,158) isno,iyr,imonth,iday,ih,im,is,instlist(idinst),
     1 iseq,obs,w**2,(triiv-triiv0)*1000.d0,v*1000.d0,stres,test,red,
     2 cplot(i+8),stxt(istat)
C
158    format(i8,i6,2('-',i2.2),', ',i2.2,2(':',i2.2),i5,i6,f11.4,f7.2,
     1 f9.1,2f7.1,a2,f7.1,1x,a13,1x,a12)
      else
       v = -999.9d0
       write(idr,159) isno,iyr,imonth,iday,ih,im,is,instlist(idinst),
     . iseq,obs,w**2,(triiv-triiv0)*1000,stxt(istat)
      endif
159    format(i8,i6,2('-',i2.2),', ',i2.2,2(':',i2.2),i5,i6,f11.4,f7.2,
     . f9.1,4x,2('---',4x),2x,'---',15x,a12)
C
CCCC Computation of the average observation data per station continues...
      iobs=iobs+1
      adt=adt+dt
CCCCC Should be weighted average in the future aobs=aobs+obs*(w**2)/iobs
CCCCC where iobs=iobs+w**2. But what about kv,ktriiv etc?
      aobs=aobs+obs
      aw=aw+w
      atriiv=atriiv+(triiv-triiv0)
      if (ia(istat).ne.0) then
       av=av+v
       astres=astres+stres
       ared=ared+red
      endif
C
      isno00 = isno
      istat00 = istat
      ibias0 = ibias
      idinst0 = idinst
      idrift0 = idrift
      goto 150
c
170   continue
C
CCCCC Last average reading into resi2 file
       call PETGREN3(adt/iobs+2400000.5D0,iyra,imontha,idaya,iha,ima,isa
     1 ,NERR)
       i = nint((av/iobs)/gsc)
       if (i.lt.-7) i = -7
       if (i.gt.7) i = 7
       test='  '
       if(astres/iobs.gt.taui) test=' !'
       if (ia(istat).ne.0) then
       write(idr2,146) isno,iyra,imontha,idaya,iha,ima,isa
     1 ,instlist(idinst),ikobs,iseq0,aobs/iobs,(aw/iobs)**2
     2 ,(atriiv/iobs)*1000.d0,(av/iobs)*1000.d0,astres/iobs,test
     3 ,ared/iobs,cplot(i+8),stxt(istat)
       else
       write(idr2,147) isno,iyra,imontha,idaya,iha,ima,isa
     1 ,instlist(idinst),ikobs,iseq0,aobs/iobs,(aw/iobs)**2
     2 ,(atriiv/iobs)*1000.d0,stxt(istat)
       endif
CCCCC Last tie into ties file....
        call PETGREN3(adt0+2400000.5D0,iyra,imontha,idaya,iha,ima,isa
     1  ,NERR)
       if(ia(istat01).ne.0.and.ia(istat00).ne.0) then
        if (isno01.lt.isno00) then
         write(idt,162)
     1   stxt(istat01),stxt(istat00),instlist(idinst0),iyra,imontha,
     2   idaya,iha,ima,isa,(adt/iobs-adt0)*24.0d0,aobs/iobs-aobs0,
     3   (atriiv0-atriiv/iobs)*1000.d0,
     4   aobs/iobs-aobs0+(atriiv0-atriiv/iobs),
     5   (av/iobs-av0)*1000.d0,
     6   aobs/iobs-aobs0+(atriiv0-atriiv/iobs)+(av/iobs-av0)
        else
         write(idt,162)
     1   stxt(istat00),stxt(istat01),instlist(idinst0),iyra,imontha,
     2   idaya,iha,ima,isa,(adt/iobs-adt0)*24.0d0,-(aobs/iobs-aobs0),
     3   -(atriiv0-atriiv/iobs)*1000.d0,
     4   -(aobs/iobs-aobs0)-(atriiv0-atriiv/iobs),
     5   -(av/iobs-av0)*1000.d0,
     6   -(aobs/iobs-aobs0)-(atriiv0-atriiv/iobs)-(av/iobs-av0)
        endif
       else
         write(idt,163)
     1   stxt(istat01),stxt(istat00),instlist(idinst0),iyra,imontha,
     2   idaya,iha,ima,isa,(adt/iobs-adt0)*24.0d0,aobs/iobs-aobs0,
     3   (atriiv0-atriiv/iobs)*1000.d0,
     4   aobs/iobs-aobs0+(atriiv0-atriiv/iobs)
       endif
      write(idt,'(i5, a)') jknr, ' ties'
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      if (nv.gt.0) then
       if (nv2.gt.0) then
        write(idr,153) dsqrt(gvvsum2/nv2)*1000.d0,
     1  dsqrt(wgvvsum2/wnv2)*1000.d0
        write(idr2,153) dsqrt(gvvsum2/nv2)*1000.d0,
     1  dsqrt(wgvvsum2/wnv2)*1000.d0
       endif
       write(idr,151) dsqrt(gvvsum/nv)*1000.d0,
     1 dsqrt(wgvvsum/wnv)*1000.d0
       write(idr2,151) dsqrt(gvvsum/nv)*1000.d0,
     1 dsqrt(wgvvsum/wnv)*1000.d0
      endif
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      if(ltest) write(*,*) 'trace(Qvv*P)=',summ
      if(ltest) then
       write(*,*) 'nstat, nastat=',nstat, nastat
       write(*,*) 'i,ia(i)='
       write(*,*) (i,ia(i),i=1,nstat)
      endif
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GNU FORTRAN COMPILER g77 intrinsic function ETIME
      include 'EXETIME.f'
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      stop 'GRADJ2 terminated OK '
      end
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C                    P R E D O B S
C
C  yields predicted obs based on current contents of sol vector
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      double precision function predobs(sol,iastat,iidot,tdot,ibias,
     1idrift,ideg,tt,idinst,isc,ipol,iperr,perr,rd)
      implicit double precision(a-h,o-z)
      logical ldrift,lsc,ldot
      common /cpar/ pi2,nastat,ndrift,iidrift,iisc,iirhs,iibias,
     .ndot,ldrift,lsc,ldot,av_rd
      dimension sol(*),perr(*)
c
      r = sol(iastat) + sol(iibias+ibias)
      rd1=rd-av_rd
C      write(*,*) 'r=',r
C
      if(ldot.and.(iidot.ne.0)) r=r+tdot*sol(nastat+iidot)
      if (ldrift) then
       do 20 i = 1, ideg
20      r = r + sol(iidrift+idrift+i-1)*(tt**i)
      endif
CCC  Calibration correction part
      s=1.0d0
      if (lsc.and.isc.ne.0) then
CCC  Polyniomial terms s2,s3,...
       if(ipol.eq.99) then
        s=sol(iisc+isc)
        ipol=1 !!! NB! This changes also argument ipol value when function returns!
       else
        do i = 1, ipol
         r = r + sol(iisc+isc+i-1)*(rd1**i)
C        write(*,*)'r,ipol,i,iisc+isc+i,sol(iisc+isc+i),rd**(i+1)'
C     .  ,r,ipol,i,iisc+isc+i,sol(iisc+isc+i),rd**(i+1)
        enddo
       endif
CCC  Periodic terms x1,y1,x2,y2,... for LCR
       j = - 1
       do i = 1, iperr
        j = j + 2
        x = sol(iisc+isc+ipol+j-1)
        y = sol(iisc+isc+ipol+j)
C        write(*,*)'r,iperr,i,j,iisc+isc+ipol+j-1'
C     .  ,r,iperr,i,j,iisc+isc+ipol+j-1
        r = r + x*dcos(pi2*rd/perr(i))+y*dsin(pi2*rd/perr(i))
C        write(*,*)'x,y,pi2,perr(i),pi2*rd/perr(i)'
C     .  ,x,y,pi2,perr(i),pi2*rd/perr(i)
       enddo
CCC  applying nonlinear term s1 (or s0)
C       if(ipol.ne.0) r = r/sol(iisc+isc)
       r = r/s
      endif
      predobs = r
      return
      end
CCCCCCCCCCCCCCCCCC Include Statements CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  Subroutine for finding station number in unsorted station list
C  ERROR! Does not work with Force 3.0.0 b2 - a g77 option problem???
      include 'listno1.f'
C  Subroutine sorts data in array d from 1 to n.
      include 'sortin.f'
C  subroutine for solving a set of normal equations where the first
C  'ndia' rows are diagonal. the system is solved by choleskys method.
      include 'chold.f'
C  integer function finds the number of matrix elements prior to column
C  'm', i.e. c(indx(m)+1) is first element of column 'm'
      include 'indx.f'
C  Function obtains inverse element no. 'inv' from the inverse
C  matrix of the normal equation matrix c. c must be cholesky reduced,
C  i.e. chold must have been called.
      include 'cholinv.f'
C  Subroutine adds one observation to normal equations
C      include 'addobsd.f'
C  Subroutine adds one weighted observation to normal equations
      include 'addobsd2.f'
C  Subroutine reads txt portion starting with letter in line,
C  terminates with 2 blanks
      include 'gettxt.f'
C Function IGET reads integer from array c
      include 'IGET.f'
C Subroutine MJD computes the Modified Julian Date (MJD)
      include 'MJD2.f'
C From Julian Date (JD) back to civilian date (YYYY, MM, DD, HH, MM, SS)
      include 'PETGREN3.f'
C Subroutine CHINV2 returns inversion of full normal matrix
      include 'CHINV2m.f'
C Function COMPUTES THE INVERSE OF THE DISTRIBUTION FUNCTION OF THE STUDENT DIST.
      include 'studin.f'
C Function CHI2 returns value of chi-square distribution
      include 'chi2inv.f'
C Subroutines needed by studin and chi2inv
      include 'gausin.f'
      include 'freq.f'
C Function TAURE returns value of tau distribution
      include 'TAURE.f'

