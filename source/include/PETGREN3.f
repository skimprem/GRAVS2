      SUBROUTINE PETGREN3(DJULD,ITY,ITM,ITD,ITH,ITMIN,ITS,NERR)
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C                                                                      !
C     Routine PETGREN, version 1999.08.28 Fortran 90.                  !
C                                                                      !
C     The routine PETGREN computes civilian date from given Julian     !
C     date. PETGREN is a modified version of routine CALDAT given in   !
C     Fortran by                                                       !
C     Press, W.H., S.A. Teukolsky, W.T. Vetterling and B.P. Flannery   !
C     (1992):  Numerical recipes in FORTRAN: the art of scientific     !
C              computing. Second edition, Cambridge University Press   !
C              1992.                                                   !
C                                                                      !
C     The routine has been tested and found to be correct between      !
C     years -3000 and +3000.                                           !
C                                                                      !
C     This routine may be freely used for scientific purpose. No part  !
C     of this routine may be used for commercial purpose without       !
C     written permission of the program author.                        !
C                                                                      !
C     Input parameter description:                                     !
C     ----------------------------                                     !
C                                                                      !
C     DJULD:       Julian date (DOUBLE PRECISION).                     !
C                                                                      !
C     Output parameter description:                                    !
C     -----------------------------                                    !
C                                                                      !
C     ITY:         year   (INTEGER).                                   !
C     ITM:         month  (INTEGER).                                   !
C     ITD:         day    (INTEGER).                                   !
C     DTH:         hour   (DOUBLE PRECISION).                          !
C     ITH:         hour   (INTEGER).                                   !
C     DTMS:        minute (DOUBLE PRECISION).                          !
C     NERR:        error code, counts the number of errors which       !
C                  occured during the execution of routine PETGREN.    !
C                                                                      !
C     Routine creation:  1995.11.04 by Georg Wenzel,                   !
C                        Institut fuer Erdmessung,                     !
C                        Universitaet Hannover,                        !
C                        Schneiderberg 50,                             !
C                        D-30167 HANNOVER,                             !
C                        Germany.                                      !
C                        FAX:  ++49-511-762-4006.                      !
C                        e-mail: wenzel@ife.uni-hannover.de            !
C     Last modification: 2008/10/19 by Tõnis Oja (see remarks below)   !
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DATA DGREG/2299160.499999D0/
      NERR=0
      IF(DJULD.LT.625307.D0.OR.DJULD.GT.2817153.D0) THEN
        NERR=1
        WRITE(*,17050)      DJULD
      ENDIF
      JULIAN=INT(DJULD)
      DTH=12.D0
C      ITH=12
      DFRAC=DJULD-DBLE(JULIAN)
      DTH=DTH+DFRAC*24.D0
      ITH=INT(DTH)
C      ITH=ITH+DFRAC*24
      DTMS=(DTH-DBLE(ITH))*60.D0
CCC  Next lines added by T. Oja (2008-10-19)
      ITMIN=INT(DTMS)
      DTS=(DTMS-DBLE(ITMIN))*60.D0
      ITS=DNINT(DTS)
      IF(ITS.GT.59) THEN
       IF(ITS.GT.119) STOP'*****Error in routine PETGREN.'
       ITS=ITS-60
C       ITS=0
       ITMIN=ITMIN+1
      ENDIF
C      if(dint(DTMS*10.0).ge.599) then
C       DTMS=0.d0
C       ITH=ITH+1
C      endif
      IF(ITMIN.GT.59) THEN
       IF(ITMIN.GT.119) STOP'*****Error in routine PETGREN.'
       ITMIN=ITMIN-60
       ITH=ITH+1
      ENDIF

C      write(*,*)'DFRAC,DTH,ITH,DTMS=',DFRAC,DTH,ITH,DTMS
C      IF(DTH.GE.23.9999D0) THEN
C         DTH=DTH-24.D0
      IF(ITH.GE.24) THEN
         ITH=ITH-24
         JULIAN=JULIAN+1
      ENDIF
      IF(DJULD.GT.DGREG) THEN
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C     Cross over from Gregorian calendar procudes this correction:     !
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         JALPHA=INT(((JULIAN-1867216)-0.25D0)/36524.25D0)
         JA=JULIAN+1+JALPHA-INT(0.25D0*DBLE(JALPHA))
      ELSE
         JA=JULIAN
      ENDIF
      JB=JA+1524
      JC=INT(6680.D0+((JB-2439870)-122.1D0)/365.25D0)
      JD=365*JC+INT(0.25D0*DBLE(JC))
      JE=INT((JB-JD)/30.6001D0)
      ITD=JB-JD-INT(30.6001D0*DBLE(JE))
C
      ITM=JE-1
      IF(ITM.GT.12) ITM=ITM-12
      ITY=JC-4715
      IF(ITM.GT.2) ITY=ITY-1
      RETURN
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C     Format statements:                                               !
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
17050 FORMAT(/' *****Error in routine PETGREN.'/
     1' *****Julian date is:',F20.6/
     1' *****Year is less -3000 or greater +3000.'/
     2' *****Routine PETGREN has not been tested for this case.'/
     3' *****Routine PETGREN continues the execution.'/)
      END

