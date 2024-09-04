      program Wzz2
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C   Program to compute vertical attraction of multiple bodies (currently
C   rectangular prisms and cylinders only) at different heights (along
C   vertical z axis) and to fit observation data (along z)
C   T.Oja (ELB) 2009-03-06
C   modifications:
C      2009-08-13 multiple attracting bodies (ver.1.1)
C      2009-11-20 slight improvements,commented lines in input files
C                 allowed,without body file no RMR etc
C      2018-11-02 Weights of obs data, full COV matrix of adjusted parameters
C      2019-02-16 Detailed vertical gradient info into separate file
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CCCC Declarations
      implicit double precision (a-h,o-z)
CCC  integer i,j,k,l,m,n
      parameter (maxlen=255, maxp=3, maxb=5, maxobs=99)!Max dim of inut file names,pol.order,no of bodies,no of obs
      parameter (maxn=maxp+maxb, maxc=(maxn**2+3*maxn)/2)!Max no of unknowns, dim.of lin.system (from GRADJ2?? - ok same as n(n+1)/2+n)
      character*(maxlen) bodyfile,obsfile,outfile,ofile2
      character hdr*10, version*27, tyyp*1, cplot(15)*13
      logical lsing(maxn),ldens(maxb),lfix,lrho,lbody,lweight
      dimension b(maxn),ib(maxn),tyyp(maxb),x(2,maxb),y(2,maxb),
     1          z(2,maxb),rho(maxb),bstd(maxp)
      dimension c(maxc),cov2(maxc),ginv(maxc),g(maxobs),h1(maxobs),
     1          h2(maxobs),sumdg1(maxobs),sumdg2(maxobs),id(maxobs),
     2          unc(maxobs),u2(maxp),covv(maxp),dh(maxp)
      data idb/10/,ido/11/,idout/13/,ldens/maxb*.false./,lrho/.false./,
     1     lbody/.true./
      data c/maxc*0.d0/,b/maxn*0.d0/,ib/maxn*0/
      data version/'*** Wzz2.1 (2019-02-16) ***'/
CCCCCCCC Main
      write(*,*) version
      write(*,*)'Insert the names (without spaces!) of input files (body
     1file, obsfile):'
      read(*,'(a)') bodyfile
      read(*,'(a)') obsfile

      k=index(obsfile,'.')
      kk=index(obsfile,' ')
      if (k.eq.0) k=kk
      if (kk.eq.0) stop'Erroneous obsfile name. Probably too long!'
      outfile=obsfile(1:k-1)//'.wzz'
      ofile2=obsfile(1:k-1)//'.vgg'
      write(*,*)'Input files: ',bodyfile(1:kk-1),', ',obsfile(1:kk-1)
      write(*,*)'Result file: ',outfile(1:k+3)
      write(*,*)'Detailed result file: ',ofile2(1:k+3)
      
      write(*,*)'Insert order of polynom (max.',maxp,'):'
      read(*,*) ipol
      write(*,*)'Inserted order:',ipol
      if(ipol.gt.maxp) then
       write(*,*)'Order of polynomial too high - ipol lowered to avoid
     . overfit!'
       ipol=maxp
      endif

      write(*,*)'The weighted LSQ according uncertainties in obs file (T
     1/F) and a priori st.dev of unit weight:'
      read(*,*) lweight,s0
      write(*,*)'The weights included:',lweight,' , stdev=',s0
      
      open(ido,file=obsfile,status='old')
      open(idb,file=bodyfile,status='old',IOSTAT=nb) !use nb temporary
       if(nb.ne.0) lbody=.false.
      open(idout,file=ofile2,status='unknown')

CCCC Input attracting bodies' dim and rho (max nmax)
      nb=0
      write(*,*)
      if(lbody) then
       write(*,*)
     . 'Insert attracting bodies (max no',maxb,') dim and rho:'
110   read(idb,'(a)',end=250) hdr
       if(hdr(1:1).eq.'!'.or.hdr(1:10).eq.'          ') goto 110
       if(hdr(1:1).ne.'#') goto 919
       write(*,'(a5)') hdr
       nb=nb+1
       if(nb.gt.maxb) stop'!!! Too many bodies, increase MAXB !!!'
       tyyp(nb)=hdr(3:3) !(R)ectangular or (C)ylinder
       write(*,*)'Tyyp(',nb,')=',tyyp(nb)
       if(tyyp(nb).eq.'R') then
        write(*,*)'(R)ectangular inserted'
       elseif(tyyp(nb).eq.'C') then
        write(*,*)'(C)ylinder inserted'
       else
        STOP'!! Body type should be (R)ectangular or( C)ylinder only !'
       endif

130    read(idb,*) hdr
       if(hdr(1:1).eq.'!') goto 130 !Possible to comment one line out
       backspace(idb)
       read(idb,*,ERR=919) rho(nb)
CCCC Density as a unknown parameter in adjustment
       if (dabs(rho(nb)).lt.0.01) then
        ldens(nb)=.true.
        lrho=.true.
       endif

       ii=1 !In case of C only 3 variables needed (z1,z2,D - depth of top,bottom and diameter)
       if(tyyp(nb).eq.'R') ii=ii+1 !In case of R 6 variables (x1,y1,z1,x2,y2,z2 - sides of rectangle body)
       do i=1,ii
135     read(idb,*) hdr
        if(hdr(1:1).eq.'!') goto 135 !Possible to comment one line out
        backspace(idb)
        read(idb,*,ERR=919) x(i,nb),y(i,nb),z(i,nb)
C        write(*,*) 'xyz(',i-1,')of body no',nb,':'
C        write(*,*)  x(i,nb),y(i,nb),z(i,nb)
       enddo
       write(*,*)'Dim vec of body (no',nb,'):'
       write(*,*) 'X:',(x(i,nb),i=1,ii)
       write(*,*) 'Y:',(y(i,nb),i=1,ii)
       write(*,*) 'Z:',(z(i,nb),i=1,ii)
       write(*,*) 'rho=',rho(nb)
       if(.not.ldens(nb)) then
        if(tyyp(nb).eq.'R') then
         w=(x(2,nb)-x(1,nb))*(y(2,nb)-y(1,nb))*(z(2,nb)-z(1,nb))*rho(nb)
        elseif(tyyp(nb).eq.'C') then
C         write(*,*)'Pi=',4.d0*datan2(1.d0,1.d0)
         w=(4.d0*datan2(1.d0,1.d0))*((z(1,nb)/2.d0)**2)*
     .   (y(1,nb)-x(1,nb))*rho(nb)
        endif
        write(*,'((a),f8.1,(a))')'Weight of body:',w,' kg'
       else
        write(*,'((a),f8.1,(a))')'NB! Density of body will be found from
     . the adjustment!'
       endif
       write(*,*)
      goto 110

919    write(*,*) 'Problem with body no',nb
       STOP'!!! Format problem in body inp file !!! '
250    write(*,*)'No of bodies:',nb
      else
       write(*,*)'No information about bodies inserted, no RMR method, o
     1nly polynomial fit!'
      endif
      
      n=ipol+1 !no of unknowns including g(href)
      if(lrho) n=n+1 !a parameter for unknown density

CCCC No of unknowns -> dim of matrixes
      idiag = n*(n+1)/2   !dim. of symm. covariance matrix (triangular part only)
      icdim = idiag + n !dim. of extended normal matrix (right side included)
      write(*,1500) n,idiag,icdim
1500  format(/' Numbers of adjustment process:',
     ./' total no. of unknowns:           ',i5,
     ./' dim. of symm. covariance matrix: ',i5,
     ./' dim. of extended normal matrix:  ',i5)

      if (n.gt.maxn) stop '*** too many unknowns, increase maxn'
      if (icdim.gt.maxc) stop '*** adjustment too large, increase maxn'

CCCC Read the fixed values with ref.heights (h1 should be same for all fixed obs!)
      do i=1,3
       if(i.eq.2) then
        read(ido,*) href
       else
        read(ido,*) hdr
       endif
      enddo

      nfix=0
      nobs=0
      lfix=.true.
      ii=1
      jj=0
150   read(ido,*,end=350) hdr
      if(hdr(1:1).eq.'!') then
       jj=jj+1
       goto 150
      endif
      if(hdr(1:1).eq.'#') then
       lfix=.false.
       goto 150
      endif
      jj=jj+1
      backspace(ido)
      if(lfix) then
       nfix=nfix+1
       read(ido,*) g(ii),unc(ii),h2(ii)
!       write(*,*) 'fix:',g(ii),stdev,h2(ii)
       h1(ii)=href
       id(ii)=jj
      else
       read(ido,*) g(ii),unc(ii),h1(ii),h2(ii)
!       write(*,*) 'obs:',g(ii),stdev,h1(ii),h2(ii)
       nobs=nobs+1
       id(ii)=jj
      endif
CCCC Setup of normal matrix
CCCC Arrays' initialization
C      do i = 1, j
C       b(i) = 0.d0
C       ib(i) = 0
C      enddo
      j=1
      b(j)=0.d0
      if(lfix) b(j)=1.d0
      ib(j)=1
      j=j+1
      do i = 1, ipol
       b(j)= (h2(ii)**i-h1(ii)**i)
       ib(j) = 1+i
       j=j+1
      enddo
C       k=j
C       b(j)=0.d0
      sumdg1(ii)=0.d0
      sumdg2(ii)=0.d0
      do i=1,nb
C       j=k
       if(tyyp(i).eq.'R') then
C        g1(ii,nb)=gbox(x(1,nb),y(1,nb),-h1(ii),x(2,nb),y(2,nb),z(2,nb),
        g1=gbox(0.d0,0.d0,-h1(ii),x(1,i),y(1,i),z(1,i),
     1  x(2,i),y(2,i),z(2,i),1.d0)
C        g2(ii,nb)=gbox(x(1,nb),y(1,nb),-h2(ii),x(2,nb),y(2,nb),z(2,nb),
        g2=gbox(0.d0,0.d0,-h2(ii),x(1,i),y(1,i),z(1,i),
     1  x(2,i),y(2,i),z(2,i),1.d0)
       elseif(tyyp(i).eq.'C') then
        g1=silinder(x(1,i)+h1(ii),y(1,i)+h1(ii),z(1,i),1.d0) !silinder(z1,z2,D,rho)-x,y are used for z1,z2!
        g2=silinder(x(1,i)+h2(ii),y(1,i)+h2(ii),z(1,i),1.d0)
       endif
        dg=g2-g1
       if(ldens(i)) then ! rho as unknown parameter
        sumdg1(ii)=sumdg1(ii)+dg
       else
        sumdg2(ii)=sumdg2(ii)+rho(i)*dg
       endif
C        write(*,*)
C     .  'Body No',i,' g1,g2=',g1,g2,'(h1,h2=',h1(ii),h2(ii),')'
      enddo
      if(lrho)then
       b(j)=sumdg1(ii)
       ib(j)=1+ipol+1
       j=j+1
      endif
CCCC  Obs info to coefficient matrix (extended)
       b(j) = g(ii)-sumdg2(ii)
       ib(j) = n+1
CCC  Setup of the normal matrix (by adding row of coefficient matrix)
      w=1.d0  ! no weighted adjustment
      if(lweight) w = s0/unc(ii)
      call addobsd2(c, 1, n, b, ib, w)
      ii=ii+1
      goto 150
CCC
350   write(*,*)'No of fixed readings:',nfix
      write(*,*)'No of observations:',nobs
      write(*,*)'No of all obs:',nfix+nobs
      write(*,*)'No of unknowns:',n
      write(*,*)'dof:',nfix+nobs-n
C      write(*,*)'write(*,1791) (c(kk),kk=1,icdim+1):'
C      write(*,1791) (c(kk),kk=1,icdim+1)
C1791  format(' test norm eq matrix'/,8(5g16.4,/))
      call chold(c,n,1,nsing,cif,lsing) !Solve normal eq.matrix with Cholesky decomposition
CCCC Test the ill-condition of normal matrix
      if (nsing.eq.0) then
       write(*,1082) cif
1082   format(' - solution OK, max loss of digits:',f5.1)
      else
       write(*,*) '- number of normal equation singularities: ',nsing
       do 183 i= 1,n
183    if (lsing(i)) write(*,*) '  singularity row no.: ',i
      endif
C       write(*,*) 'write(*,792) idiag,(c(kk),kk=1,icdim+1)'
C       write(*,1792) idiag,(c(kk),kk=1,icdim+1)
C1792   format(' test after chold idiag =',i5,/,8(5g16.4,/))
      write(*,*)
      g0=c(idiag+1)
      do i=1,ipol
       b(i)=c(idiag+1+i)
      enddo
      if(lrho) rho0=c(idiag+n)
CCCC Diagonal of covariance matrix
C       write(*,1794) n,(cholinv(c,n,1,i),i=1,n)
C1794   format('n, cov vector:',i6,/,4(5f16.4,/))
C       write(*,*) 'write(*,792) idiag,(c(kk),kk=1,icdim+1)'
C       write(*,1795) idiag,(c(kk),kk=1,icdim+1)
C1795   format(' test after cholinv =',i5,/,8(5g16.4,/))
CCCC Full covariance matrix COV2 of the adjusted parameters
       call CHINV2(c, cov2, ginv, n, 1, idiag, idiag)
C       write(*,*) 'test CHINV2m:'
C       write(*,*) (cov2(i),i=1,idiag)
       write(*,'((a),2(i4,(a)))')
     1'Lower triangular of symmetric covariance matrix (Cx) of adjusted
     2parameters with dim(',n,',',n,'):'
      do i=1,n
       iii=i*(i-1)/2
       write(*,796) (cov2(iii+jjj),jjj=1,i)
      enddo
796   format(100(100f18.6))
CCCC
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
      gsc=0.5
CCCC Vector of residuals
      write(*,*)'--------------------------------------'
      write(*,*)'Input data, adjusted results and residuals:'
      write(*,'((a),f5.2,(a))')' Id     obs g/dg   weight    h1     h2
     1    adj g/dg      res    wres  res.plot(',gsc,' uGal/div)'
      sumsq=0.d0
      do 675 j=1,nfix+nobs
        adj=0.d0
        if(j.le.nfix) adj=g0
        adj=adj+sumdg2(j)
        if(lrho) adj=adj+rho0*sumdg1(j)
        do i=1,ipol
         adj=adj+b(i)*(h2(j)**i-h1(j)**i)
        enddo
        w=1.d0
        if(lweight) w=(s0/unc(j))**2
        v=adj-g(j)
        sumsq=sumsq+(v**2)*w
CCCC Residual plot
        i = nint(v/gsc)
        if (i.lt.-7) i = -7
        if (i.gt.7) i = 7
C        write(*,1750) id(j),g(j),w,h1(j),h2(j),adj,v,v*dsqrt(w)
        write(*,1750) id(j),g(j),w,h1(j),h2(j),adj,v,v*dsqrt(w),
     1  cplot(i+8)
675   continue
C1750  format(i3,f12.1,f8.2,2f7.3,f14.1,2f8.1)
1750  format(i3,f13.2,1x,f8.2,2f7.3,f13.2,1x,2f8.2,1x,a13)

CCCC Computation of residuals and sigma
       rms=dsqrt(sumsq/(nfix+nobs))
       sigma=dsqrt(sumsq/(nfix+nobs-n))
      write(*,*)'--------------------------------------'
      write(*,*)'Statistics of residuals:'
      write(*,1770)'RMS=  ',rms
      write(*,1770)'STDEV=',sigma
1770  format((a),f8.3)
      write(*,*)'--------------------------------------'
      write(*,1780)
C     .'g(0) and its stdev=',g0,sigma*dsqrt(cholinv(c,n,1,1))
C      write(*,1780)
     .'g(0) and its stdev=',g0,sigma*dsqrt(cov2(1))
      do i=1,ipol
C       write(*,1782)'c(',i,'),stdev=',
C     . b(i),sigma*dsqrt(cholinv(c,n,1,i+1))
       iii=i+1
       jjj=(iii)*(iii+1)/2
       bstd(i)=dsqrt(cov2(jjj))
       write(*,1782)'b(',i,'),stdev=',b(i),sigma*bstd(i)
       if(i.gt.1) then
C        do j=1,i-1
C         write(*,*)'bstd(',i-j,')=',bstd(i-j)
C         write(*,*)'bstd(',i,')=',bstd(i)
C         write(*,*)'cov2(',jjj-j,')=',cov2(jjj-j)
C         write(*,1784)'corr(',i-j,i,')=',cov2(jjj-j)/(bstd(i-j)*bstd(i))
        do j=i-1,1,-1
         write(*,1784)'corr(',i-j,i,')=',cov2(jjj-j)/(bstd(i-j)*bstd(i))
C         write(*,1784)'corr(',i-j,i,')/STDEV=',
C     1   cov2(jjj-j)/(bstd(i-j)*bstd(i)*sigma**2)
        enddo
       endif
      enddo
1780  format((a),f13.2,f7.2)
1782  format((a),i1,(a),2f10.2)
1784  format((a),2i1,(a),f10.5)
      if(lrho) then
       write(*,1790)'adjusted rho=',rho0
       write(*,1790)'...and its stdev=',sigma*dsqrt(cholinv(c,n,1,n))
       nnn=n*(n+1)/2
       write(*,1790)'...and its stdev=',sigma*dsqrt(cov2(nnn))
      endif
1790  format((a),f8.1)
CCCC Gravity values and VG at different heights
      write(*,*)'------------------------------------------------------'
      write(*,*)'  h        g(h)           VG*       Gravity change due
     1to..'
      if(lbody) then
       write(hdr,'(i1)') nb
       hdr=hdr(1:1)//'i8'
C       write(*,*) '!!! hdr=',hdr
       write(*,'((a),/41x,(a),3x,'//hdr//')') '  (m)      (uGal)       (
     1uGal/m) ..constVG  ..noncVG  ..attraction of body #',
     2'(uGal)',(i,i=1,nb)
      else
       write(*,*)' (m)     (uGal)         (uGal/m)   constVG  nonlinVG'
      endif
      write(*,*)'------------------------------------------------------'
CCC Initialization
      gh0=0.d0
      gh=0.d0
CCC Gravity attraction of disturbing body at ref.height
      do i=1,nb
       if(ldens(i)) rho(i)=rho0
       if(tyyp(i).eq.'R') then
        gg=gbox(0.d0,0.d0,-href,x(1,i),y(1,i),z(1,i),
     1  x(2,i),y(2,i),z(2,i),rho(i))
       elseif(tyyp(i).eq.'C') then
        gg=silinder(x(1,i)+href,y(1,i)+h0,z(1,i),rho(i)) !silinder(z1,z2,D,rho)-x,y are used for z1,z2!
       endif
       gh0=gh0+gg
       sumdg1(i)=gg
      enddo
      g1=gh0
C       write(*,*) 'g1:',g1
CCCC Next section computes values after every 1 mm step (from -1 to 1500 mm)
CCCC but only results after every 50 mm are printed
      h0=0.d0
      step=1000.d0
      j0=idnint(h0*step)
      j1=j0+idnint(1.5*step)
C      write(*,*) 'j0,j1:',j0,j1
CCCC Compute unc and cov terms of polynomial parameters
      jj=0
      do i=1,ipol
       u2(i)=bstd(i)**2
CCCC Find and store cov elements
       if(i.gt.1) then
        jjj=(i+1)*(i+2)/2
        do ja=i-1,1,-1
         jj=jj+1
         covv(jj)=cov2(jjj-ja)
        enddo
       endif
      enddo
C
      j2=50
      jj=0
C      do j=0,15
      do j=j0-1,j1 !start 1 mm earlier to compute VG at BM level
       gg=g0
       vg=0.d0
       g2=0.d0
       u2dg=0.d0
       u2vg=0.d0
       do i=1,ipol
CCCC Computation of dg
        dh(i)=(j/step)**i-href**i
        gg=gg+b(i)*dh(i)
CCCC Uncertainty comp. of dg(href - hj) without correlation
        u2dg=u2dg+u2(i)*(dh(i)**2)
CCCC Comp of VGG
        hi=(j/step)**(i-1)
        vg=vg+b(i)*i*hi
CCCC Uncetainty of VGG (without correlation)
        u2vg=u2vg+u2(i)*(i*hi)**2
       enddo
C       write(idout,*)'Au2=',dsqrt(u2vg),dsqrt(u2dg)
CCCC Covariance parts for dg (work even if ipol>3) and VGG (works only if ipol<=3)
       if(ipol.gt.1) then
        k=0
        do i=1,ipol-1
         do ii=1,i
          k=k+1
          u2dg=u2dg+2*dh(ii)*dh(i+1)*covv(k)
C          write(idout,*)'dh(ii),dh(i+1),covv(k)',dh(ii),dh(i+1),covv(k)
          u2vg=u2vg+2*ii*(i+1)*((j/step)**k)*covv(k)
C          write(idout,*)'ii,i+1,k=',ii,i+1,k
         enddo
        enddo
       endif
C       write(idout,*)'Bu2=',dsqrt(u2vg),dsqrt(u2dg)
       do i=1,nb
        if(tyyp(i).eq.'R') then
         gh=gbox(0.d0,0.d0,-(j/step),x(1,i),y(1,i),z(1,i),
     1   x(2,i),y(2,i),z(2,i),rho(i))
        elseif(tyyp(i).eq.'C') then
         gh=silinder(x(1,i)+j/step,y(1,i)+j/step,z(1,i),rho(i)) !silinder(z1,z2,D,rho)-x,y are used for z1,z2!
        endif
C        write(*,*) i,gg,gh,gh0
        gg=gg+(gh-gh0/nb) !divided by nb? -> because gh0 is sum which is summed here nb times
        sumdg2(i)=gh-sumdg1(i) !Effect of a single body
        g2=g2+gh
       enddo
C       write(*,*) 'g0,gh0,gg:',g0,gh0,gg
       vg=vg+(g2-g1)*step
C       write(*,*) 'Ag1,g2:',g1,g2
       g1=g2
       g2=b(1) !Linear gradient stored here
       if(ipol.lt.1) g2=0.d0
C       write(*,*) 'B_g1,g2:',g1,g2
CCCC write detailed Wzz to ofile2.
CCCC Uncertainty of dg(z1,z2),VGG(z) - currently only for poly parameters, no dist.body and rho
       if((j/step).ge.0.d0)
     1 write(idout,1845)(j/step),gg,vg,sigma*dsqrt(u2vg),gg-g0,
     2 sigma*dsqrt(u2dg)
CCCC Write after j2 step to output
       if(j.eq.j0+jj*j2) then
        jj=jj+1
        if(nb.lt.1) then
         write(*,1850)(j/step),gg,vg,g2*(j/step-href),
     1   gg-g0-g2*(j/step-href)
        else
        write(hdr,'(i1)') nb
        hdr=hdr(1:1)//'f8.2'
C        write(*,*) hdr(1:5)
         write(*,'(f6.3,f15.2,3f10.2,'//hdr//')')
     1   (j/step),gg,vg,g2*(j/step-href),gg-g0-g2*(j/step-href),
     2   (sumdg2(i),i=1,nb)
        endif
       endif
      enddo
CCCC End of section
C1845  format(f8.3,f16.3,f12.3,' ±',f7.3,f12.3,' ±',f7.3)
1845  format(f8.3,f16.3,2(f12.3,f7.3))
C
1850  format(f6.3,f15.2,3f10.2)
      write(*,*)'------------------------------------------------------'
      write(*,'((a),f7.4,(a))')
     1'* NB! Actual resolution of VG computation:',1/step,' m'
      write(*,*)
      write(*,*)'Program Wzz2 OK'
      stop'Program Wzz2 OK'
      end
C      include 'gbox2.f'
      include 'gbox.f'
      include 'silinder.f'
C  Subroutine adds one weighted observation to normal equations
      include 'addobsd2.f'
C  subroutine for solving a set of normal equations where the first
C  'ndia' rows are diagonal. the system is solved by choleskys method.
      include 'chold.f'
C  Function obtains inverse element no. 'inv' from the inverse
C  matrix of the normal equation matrix c. c must be cholesky reduced,
C  i.e. chold must have been called.
      include 'cholinv.f'
C  integer function finds the number of matrix elements prior to column
C  'm', i.e. c(indx(m)+1) is first element of column 'm'
      include 'indx.f'
C Subroutine CHINV2 returns inversion of full normal matrix
      include 'CHINV2m.f'

