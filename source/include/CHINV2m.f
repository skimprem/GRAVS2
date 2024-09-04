      subroutine CHINV2(c, ninv, ginv, n, ndia, ndim, idim)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC   Subroutine inverses upper triangular matrix G obtained from
CCC   Cholesky decomposition and returns inversion of full normal matrix
CCC   G'G
CCC   Input:
CCC    c - vector (from CHOLD) which contains upper triangular part of
CCC        G
CCC    n - number of rows, columns of matrix G and G'G (no of unknowns)
CCC    ndia - number of first 'ndia' diagonal elements -> dim. of
CCC           diagonal part of G'.
CCC    ndim - dimension of ninv, ndim=n*(n+1)/2
CCC    idim - dimension of c and ginv, idim=ndim-ndia*(ndia-1)/2
CCC   Output:
CCC    ginv - vector which contains the inverse of G
CCC    ninv - vector which contains the lower(upper?) triangular part of the
CCC           inversion of normal matrix G'G
CCC
CCC   Algorithm for computing the lower(?) triangle of inv. matrix has been
CCC   taken from the online book Numerical Recipes in Fortran 77,
CCC   www.library.cornell.edu
CCC
CCC   written by T. Oja (2006-07), mod. 2009-09
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      implicit none
      integer n, ndim, idim, ndia, i, j, ii, jj, k, ic, ir, indx
      double precision c(idim),sum,ginv(idim),ninv(ndim),a1, a2
      logical ltest
C
CCC   Initialize
      ltest = .false.
C      ltest = .true.
C
      do 115 k=1,idim
115    ginv(k)=c(k)
C
      if(ltest) then
       write(*,*)'write(*,791) (ginv(k),k=1,idim),idim=',idim
       write(*,791) (ginv(k),k=1,idim)
      endif
791   format(' test vec c:'/,7(7f16.4,/))
C
CCC Find inv. matrix from vector C and store it into ginv
      do 145 i=1,n
       ii=i
       if(i.le.ndia) ii=1
       ii=indx(i,ndia)+ii
       if(c(ii).eq.0) STOP'CHINV2 ERROR: division by zero !!!'
       ginv(ii)=1.d0/c(ii)
       do 145 j=i+1,n
        if(j.le.ndia) goto 145 !First ndia rows' nondiagonal elements are zero anyway
        sum = 0.d0
        do 150 k=i,j-1
         if(k.le.ndia.and.k.ne.i) goto 150 !Those elements also zero
CCC  Find a(j,k)
         jj=indx(j,ndia)+k
         a1=ginv(jj)
CCC  find a(k,i)
         jj=indx(k,ndia)+i
         if(k.le.ndia) jj=k !Position of diagonal element
         a2=ginv(jj)
CCC  Find sum
         sum=sum-a1*a2
150     enddo
C        ir=j !Row
C        ic=i
        jj=indx(j,ndia)
        if(c(jj+j).eq.0) STOP'CHINV2: division by zero !!!'
        ginv(jj+i)=sum/c(jj+j)
145   enddo
C
      if(ltest) then
       write(*,*)'test ginv(indx(i,ndia)+j),j=1,jj):'
       do i=1,n
        jj=i
        if(i.le.ndia) jj=1
        write(*,*) (ginv(indx(i,ndia)+j),j=1,jj)
       enddo
      endif
C
CCC  Multiply G inv (upper triangular) and transposed G inv (lower triangular)
CCC  and store lower part of symmetric matrix inv(G'G)=inv(G)inv(G)' into ninv
      do 230 i=1,n
       ii=i*(i-1)/2
       do 230 k=1,i
        ninv(ii+k)=0.d0
        if(i.le.k) then
         jj=k
        else
         jj=i
        endif
        do 230 j=jj,n
CCC  If nondiagonal elements of diagonal part of inv(G)
         if((j.le.ndia).and.(j.ne.i.or.j.ne.k)) goto 230
         ir=indx(j,ndia)
         ic=ir+k !a(j,k) -> C(ic)
         ir=ir+i !a(j,i) -> C(ir)
         if(j.le.ndia) then
          ir=ir-i+1
          ic=ic-k+1
         endif
C         write(*,*)'(j,i),(j,k)=','(',j,i,'),(',j,k,')'
C         write(*,*)'ir,ic=',ir,ic
         ninv(ii+k)=ninv(ii+k)+ginv(ir)*ginv(ic)
230   continue
C
      if(ltest) then
       write(*,*)'test ninv:'
       do i=1,n
        ii=i*(i-1)/2
        write(*,*) i,(ninv(ii+j),j=1,i)
       enddo
      endif
C      if(ltest) STOP '*** Temporary STOP for program TESTING ***'
      return
      end
      
