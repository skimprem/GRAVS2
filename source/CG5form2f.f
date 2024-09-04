      program SFORM2
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C   FORTRAN77 (GNU compiler g77 compatible) program for the Scintrex
C   *.txt file conversion to the GRREDU2 observation file format. With
C   separate info file it is possible to include additional information
C   about instrument height, bentcmark height, air pressure.
C
C   Created by T.Oja May 30, 2004
C   Contacts: tonis.oja@maaamet.ee, Estonian Land Board
C
C   Updated: Feb.2005 - Support of the newer version of SCTUTIL(date of obs).
C            2005-08-17, 2006-Aug, Sept., Oct.(PETGREN->PETGREN2)
C            2008-10-18 date and time format changes, clearing code etc
C            ... see SFORM2_history.txt
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      implicit double precision(a-h,o-z)
CCC  integer i,j,k,l,m,n
      character txt(4)*255,version*30,ch*1
      character survey*30,client*8,operator*16
      logical ltide,linf
      data version/'CG5form ver. 2f (2021-03-29)'/
      data idi/10/,idinf/25/,ido/30/,idtmp/35/
      data ltide,linf/.false.,.false./,inr,istat0/0,0/
CCC Main
      write(*,'(a)') version
      write(*,'(a/)')'Program to convert Scintrex *.txt file to GRREDU3
     1obs file'
CCC  Read default values for preliminary inf file
      write(*,'(a)')'Insert default inf-file values for height (gravimet
     1er and BM),pressure:'
C      write(*,*)'e.g. 300 0 -999.9'
      read(*,*)korg0,krel0,pres0
      write(*,*)korg0,krel0,pres0
CCC  If time difference between observations higher than dtmax then new header
CCC  to automatically separate different sessions
C    is dtmax really necessary?!
      write(*,'(a)')'Insert max time diff.(in hours):'
      read(*,*) dtmax
      write(*,*) dtmax
C
CCC  Read input filename
      write(*,'(a)')'Insert input-file name (Scintrex txt file):'
      read(*,'(a)') txt(1)
      write(*,'(a/)') txt(1)
C
      write(*,'(a)')'Insert inf file with observation info (optional):'
      read(*,'(a)',IOSTAT=ierr4) txt(4)
      if(ierr4.eq.0) then
       if(txt(4)(1:4).eq.'ECHO') goto 210 !Works only in MS-DOS with line "ECHO is off."
       write(*,'(a/)') txt(4)
       linf=.true.
      else
210    write(*,1040)
      endif
1040  format('Name of the file not inserted, inf file created with defau
     1lt values.'/'Upgrade and rename(!) it for later addition of height
     2 and air pres. information into observation file!')
C
CCC  Open input files...
      open(idi,file=txt(1),form='formatted',status='old',IOSTAT=ierr2)
      if (ierr2.ne.0) Stop'Could not find Scintrex txt file!'
CCC  If inf file found, open it
      if(linf) then
       open(idinf,file=txt(4),form='formatted',status='old',
     .IOSTAT=ierr4)
C      write(*,*) 'ierr4=',ierr4
       if (ierr4.ne.0) Stop'Could not find inf file!'
      endif
CCC  Naming output file ...
      k=index(txt(1),'.')
      if (k.eq.0) k=index(txt(1),' ')
      txt(3)=txt(1)(1:k-1)//'.obs'
      write(*,'(/(a),(a)/)')'..output: obs. file: ',txt(3)
      open(ido,file=txt(3),form='formatted',status='unknown')
      if(.not.linf)
     . open(idtmp,file='add.inf',form='formatted',status='unknown')
CCC  Read data
250   read(idi,'(a)',end=350) txt(1)
C      write(*,*) txt(1)(1:1)
      if (txt(1)(1:12).eq.'            ') goto 250
      if (txt(1)(1:1).eq.'/') then
       if(txt(1)(1:3).eq.'/--') then
        if(inr.ne.0) write(*,1100) inr
        write(ido,1050) NRinst,survey,iyr,client,operator
        if(.not.linf)
     .   write(idtmp,1050) NRinst,survey,iyr,client,operator
        write(*,*) NRinst,survey,iyr,client,operator
        inr=0
        istat0=0
        goto 250
       endif
       k=index(txt(1),':')
       if (k.eq.0) goto 250
       write(*,*) txt(1)(1:29),k
CCC Specify start and end points ka and kl for Survey name, check var declar!
       ka=19
       kl=49
CCC End points for client, operator, check var declar!
       kll=35
       if(txt(1)(3:k-1).eq.'Survey name') then
        survey=txt(1)(ka:kl)
       elseif(txt(1)(3:k-1).eq.'Instrument S/N') then
        backspace(idi)
        read (idi,'(a18,i6)') txt(1),NRinst
        NRinst=mod(NRinst,1000)
       elseif(txt(1)(3:k-1).eq.'Client') then
        client=txt(1)(ka:kll)
       elseif(txt(1)(3:k-1).eq.'Operator') then
        operator=txt(1)(ka:kll)
       elseif(txt(1)(3:k-1).eq.'Date') then
        backspace(idi)
        read (idi,1070) txt(1),iyr,txt(1),imonth,txt(1),iday
       elseif(txt(1)(3:k-1).eq.'GMT DIFF.') then
        backspace(idi)
        read (idi,'(a16,f8.5)') txt(1),ut
       elseif(txt(1)(3:k-1).eq.'Tide Correction') then
        if(txt(1)(23:25).eq.'YES') ltide=.true.
C        write(*,*) txt(1)(23:25),ltide
       endif
       goto 250
      else
       if(txt(1)(1:4).eq.'Line') goto 250
CCC read first year, month, day
       k=index(txt(1),'/')
       read(txt(1)(k-4:),'(i4,2(a1,i2))') iyr,ch,imonth,ch,iday
C       write(*,*) k,txt(2)(k-4:k+5)
CCC next read observation data
       backspace(idi)
       read(idi,*) rline,rstat,alt,grav,std,tiltx,tilty,temp,tide,
     1 dur,irej,txt(1),date
C
       inr=inr+1
       istat = rstat
       if (rstat.eq.0.d0) istat = 0  !?? is it necessary
CCC
       if(inr.eq.1) date0 = date
       dt = (date - date0)*24.d0
C       write(*,*)'date,date0,dt,dtmax:',date,date0,dt,dtmax
       date0 = date
       if(dt.ge.dtmax) then
C        write(*,1100) inr-1
        write(ido,1050) NRinst,survey,iyr,client,operator
        if(.not.linf)
     .   write(idtmp,1050) NRinst,survey,iyr,client,operator
        write(*,*) NRinst,survey,iyr,client,operator
        inr=1
       endif
CCC  Removing the tide correction by Scintrex algorithm (inaccurate Longman)...
       if (ltide) grav = grav - tide
CCC read hour, min, sec
       read(txt(1),'(2(i2,a1),i2)') ihh,ch,imm,ch,iss
C       write(*,*)'iyr,imonth,iday,ihh,imm,iss:',
C     .iyr,imonth,iday,ihh,imm,iss
C
CCC  date and time calculation
CCCC  Compute Modified Julian date
       call ETJULN(iyr,imonth,iday,ihh,imm,iss,DMJD)
C       write(*,*)'ETJULN: DMJD=',DMJD
CCCC  Shift reading time of CG-5 by value DUR/2 to get average time
       DMJD=DMJD + (dur/2.d0)/86400.d0
CCCC  local time -> universal time
       if(ut.ne.0.d0) DMJD=DMJD-ut/24.d0
CCC  From Modified Julian date bact to normal date (Gregorian)
        call PETGREN3(DMJD+2400000.5D0,ITY,ITM,ITD,ITH,ITMIN,ITS,NERR)
C       write(*,*)'PETGREN: DMJD->ITY,ITM,ITD,ITH,ITMIN,ITS ='
C     1 ,DMJD,'->',ITY,ITM,ITD,ITH,ITMIN,ITS
C
CCC  Date and time conversion to KMS format (for GRREDU2)
C       idate=ITD*10000+ITM*100+mod(ITY,100)
C       time=ITH+ITMIN/100.d0+ITS/6000.d0
C
CCC  Reading from inf file or creation of default inf file...
       if(istat.ne.istat0) then
        if(linf) then
299      read(idinf,'(a)') ch
C         write(*,*) 'ch:',ch
C         write(*,*) 'txt(1):',txt(1)
         if(ch.eq.'#') goto 299
         backspace(idinf)
         read(idinf,*) ii,txt(1),txt(2),korg,krel,pres
CCC  In the future should be here check of date consistency!?
C
C         korg=korg-krel (since vers. h opposite sign)
         korg=korg+krel
CCC  Check of point id consistency
         if(istat.ne.ii) then
          write(*,*) 'Station',istat,', with date-time:',ITY,ITM,ITD,ITH
     1,ITMIN,ITS,' not found from file ',txt(4)
          stop'Error in optional inf file !!!'
         endif
C
        else
         write(idtmp,1150)
     1istat,ITY,ITM,ITD,ITH,ITMIN,ITS,korg0,krel0,pres0
         korg = korg0
         krel = krel0
         pres = pres0
        endif
        istat0 = istat
       endif
C
CCC  Output
       write(ido,1160)
     1  istat,ITY,ITM,ITD,ITH,ITMIN,ITS,grav,std,korg,pres
CCCC GRREDU old version obs file
C       write(ido,2160) istat,idate,time,grav
C2160   format(2i8,f7.2,f10.3)
       goto 250
      endif
C1050  format('# S-',i3,3x,a25,i5,2(1x,a16))
1050  format('# S-',i3,3x,a30,i5,1x,a8,1x,a16)
1070  format(a18,i4,a1,i2,a1,i2)
1100  format('No of readings in list:',i5)
1150  format(i8,i6,2('-',i2.2),i4.2,2(':',i2.2),i8,i5,f9.1)
1160  format
     1(i8,i6,2('-',i2.2),i4.2,2(':',i2.2),f12.4,f8.4,i6,f9.1)
C
350   continue
      write(*,1100) inr
      write(*,*) survey,client,operator,ut
      write(*,*) iyr,imonth,iday
C
      write(*,*)
      write(*,*)'program SCform2 terminated OK'
      stop 'program SCform2 terminated OK'
      end
CCC  Included subroutines
      include 'PETGREN3.f'
      include 'ETJULN.f'

