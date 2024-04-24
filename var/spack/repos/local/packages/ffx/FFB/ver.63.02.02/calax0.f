C=======================================================================
      SUBROUTINE CALAX0(A, S, AS, NP, NE, NCRS, IPCRS, NPP,
     *                  N2,ME,IPART,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *                  RX,RY,JUNROL,NPPMAX,NCRS2,TS,TA,ITPCRS,
     *                  IUT0,IERR)
C=======================================================================
      IMPLICIT NONE
      INTEGER NP, NE, NCRS
      REAL*4 A(NCRS), S(NP), AS(NP)
      INTEGER IPCRS(NCRS), NPP(NP)
      INTEGER IP, K, ICRS, IP2
      REAL*4  BUF
C
      INTEGER MAXBUF,IDUM
      INTEGER N2,ME,IPART,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,IUT0,IERR
      REAL*4  RX,RY
C
      DIMENSION LDOM(NDOM),NBPDOM(NDOM)
      DIMENSION IPSLF(MBPDOM,NDOM),IPSND(MBPDOM,NDOM)
      DIMENSION RX(0:N2,ME),RY(0:N2,ME)
C
C     [FULL UNROOL]
      INTEGER*4 JUNROL
      INTEGER*4 NPPMAX,NCRS2,ITPCRS(NCRS2),ITCRS
      REAL*4    TS(0:NP),TA(NCRS2)
C
      CHARACTER*60 ERMSGC
     & /' ## SUBROUTINE CALAXC: FATAL      ERROR REPORT   ; RETURNED'/
C
#ifdef cputime
C     include 'timer.h'
      include 'mpif.h'
C
      INTEGER*4 NUMCRS
      REAL*4 DTCRSA,DTCRSR,DTCRS2 
      COMMON /CPUCRS/ NUMCRS,DTCRSA,DTCRSR,DTCRS2
      REAL*4 DTCPU,TBUF1,TBUF2,TBUF3
#endif
C
C     CALCULATE THE PRODUCT OF MATRIX A AND VECTOR X IN PRESSURE EQUATION 
C         ( 3-D CALCULATION : SINGLE WORD & MULTI ELEMENT VERSION )
C                                              2009.12.01 RIST
C
C          OPERATION COUNTS:   FLOP /ELEMENT
C          DATA LOADINGS   :   WORDS/ELEMENT
C                           (  WORDS CONTIGUOUSLY,
C                              WORDS BY 4-WORD STRIDE, AND
C                              WORDS BY LIST )
C
C     ARGUMENT LISTINGS
C
C       (1) INPUT
C          ME          ; MAX. NUMBER OF TOTAL ELEMENTS
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          NP          ; NUMBER OF TOTAL    NODES
C
C          NCRS        ; NUMBER OF NONZERO ELEMENTS IN MATRIX
C                        OF CRS FORMAT
C          A     (ICRS); NODE-WISE COEFFICIENT MATRIX IN CRS FORMAT
C          S       (IP); GLOBAL FORCE VECTOR
C          IPCRS (ICRS); NODE NO. TABLE BASED ON CRS FORMAT
C          NPP     (IP); NUMBER OF ADJACENT NODES    TO NODE    IP
C
C          IPART       ; SUB-DOMAIN NUMBER THAT THIS TASK SHOULD TAKE/IS
C                       TAKING CARE OF. IPART BEING SET ZERO MEANS THAT
C                       THE PROGRAM SHOULD RUN/IS RUNNING IN SERIAL 
C                       MODE.
C          LDOM  (IDOM); NEIBERING SUB-DOMAIN NUMBER
C          NBPDOM(IDOM); NUMBER OF INTER-CONNECT BOUNDARY NODES
C                       SHARING WITH THE IDOM'TH NEIBERING SUB-DOMAIN,
C                       LDOM(IDOM)
C          NDOM        ; NUMBER OF THE NERIBERING SUB-DOMAINS
C          IPSLF (IBP,IDOM); INTER-CONNECT BOUNDARY NODE NUMBER IN THE
C                           CALLING TASK'S SUB-DOMAIN, FOR THE IDOM'TH
C                           NEIBERING SUB-DOMAIN, LDOM(IDOM)
C          IPSND (IBP,IDOM); INTER-CONNECT BOUNDARY NODE NUMBER IN THE
C                           SUB-DOMAIN THAT IS RECEIVING THE CALLING
C                           TASK'S RESIDUALS.
C          MBPDOM      ; THE MAXIMUM NUMBER OF THE INTER-CONNECT 
C                       BOUNDARY NODES FOR ONE NEIBERING SUB-DOMAIN
C
C          IUT0        ; FILE NUMBER TO REPORT ERROR OCCURRENCE
C
C       (2) OUTPUT
C          AS      (IP); GLOBAL SOLUTION VECTOR (PROVIDE INITIAL GUESS)
C          IERR        ; RETURN CODE TO REPORT ERROR OCCURENCE
C
C       (3) WORK
C          RX    (I,IE); USED IN DDCOMX 
C          RY    (I,IE); USED IN DDCOMX
C
C
#ifdef cputime
      NUMCRS=NUMCRS+1
C     CALL CPU_TIME( TBUF1 )
      TBUF1 = MPI_WTIME() 
#endif
      IERR=0
C
      MAXBUF = NE*(N2+1)
C
      DO 10 IP=1,NP
         AS(IP)=0.0E0
 10   CONTINUE   
C
      IF (JUNROL.EQ.0) GOTO 500
C
CC
CC    FULL UNROL
CC
      TS(0)=0.0E0
      DO 100 IP=1,NP
         TS (IP)=S(IP)
 100  CONTINUE
C
      IF (NPPMAX.EQ.30) GOTO 200
      IF (NPPMAX.EQ.40) GOTO 300
      IF (NPPMAX.EQ.50) GOTO 400
C
CC
CC    FULL UNROL : NPPMAX <= 30
CC
#ifndef __AIX__
!ocl CACHE_SECTOR_SIZE(1,11)
!ocl CACHE_SUBSECTOR_ASSIGN(TS)
!!ocl UNROLL(0)
#endif
 200  CONTINUE
      DO 210 IP=1,NP
         BUF=0.0E0
         ICRS=(IP-1)*30
         BUF=BUF+  TA(ICRS+ 1)*TS(ITPCRS(ICRS+ 1))
     &            +TA(ICRS+ 2)*TS(ITPCRS(ICRS+ 2))
     &            +TA(ICRS+ 3)*TS(ITPCRS(ICRS+ 3))
     &            +TA(ICRS+ 4)*TS(ITPCRS(ICRS+ 4))
     &            +TA(ICRS+ 5)*TS(ITPCRS(ICRS+ 5))
     &            +TA(ICRS+ 6)*TS(ITPCRS(ICRS+ 6))
     &            +TA(ICRS+ 7)*TS(ITPCRS(ICRS+ 7))
     &            +TA(ICRS+ 8)*TS(ITPCRS(ICRS+ 8))
     &            +TA(ICRS+ 9)*TS(ITPCRS(ICRS+ 9))
     &            +TA(ICRS+10)*TS(ITPCRS(ICRS+10))
     &            +TA(ICRS+11)*TS(ITPCRS(ICRS+11))
     &            +TA(ICRS+12)*TS(ITPCRS(ICRS+12))
     &            +TA(ICRS+13)*TS(ITPCRS(ICRS+13))
     &            +TA(ICRS+14)*TS(ITPCRS(ICRS+14))
     &            +TA(ICRS+15)*TS(ITPCRS(ICRS+15))
     &            +TA(ICRS+16)*TS(ITPCRS(ICRS+16))
     &            +TA(ICRS+17)*TS(ITPCRS(ICRS+17))
     &            +TA(ICRS+18)*TS(ITPCRS(ICRS+18))
     &            +TA(ICRS+19)*TS(ITPCRS(ICRS+19))
     &            +TA(ICRS+20)*TS(ITPCRS(ICRS+20))
     &            +TA(ICRS+21)*TS(ITPCRS(ICRS+21))
     &            +TA(ICRS+22)*TS(ITPCRS(ICRS+22))
     &            +TA(ICRS+23)*TS(ITPCRS(ICRS+23))
     &            +TA(ICRS+24)*TS(ITPCRS(ICRS+24))
     &            +TA(ICRS+25)*TS(ITPCRS(ICRS+25))
     &            +TA(ICRS+26)*TS(ITPCRS(ICRS+26))
     &            +TA(ICRS+27)*TS(ITPCRS(ICRS+27))
     &            +TA(ICRS+28)*TS(ITPCRS(ICRS+28))
     &            +TA(ICRS+29)*TS(ITPCRS(ICRS+29))
     &            +TA(ICRS+30)*TS(ITPCRS(ICRS+30))
         AS(IP)=AS(IP)+BUF
 210  CONTINUE
#ifndef __AIX__
!ocl END_CACHE_SUBSECTOR
!ocl END_CACHE_SECTOR_SIZE
#endif
C
      GOTO 900
C
CC
CC    FULL UNROL : 30 < NPPMAX <= 40
CC
#ifndef __AIX__
!ocl CACHE_SECTOR_SIZE(1,11)
!ocl CACHE_SUBSECTOR_ASSIGN(TS)
!!ocl UNROLL(0)
#endif
 300  CONTINUE
      DO 310 IP=1,NP
         BUF=0.0E0
         ICRS=(IP-1)*40
         BUF=BUF+  TA(ICRS+ 1)*TS(ITPCRS(ICRS+ 1))
     &            +TA(ICRS+ 2)*TS(ITPCRS(ICRS+ 2))
     &            +TA(ICRS+ 3)*TS(ITPCRS(ICRS+ 3))
     &            +TA(ICRS+ 4)*TS(ITPCRS(ICRS+ 4))
     &            +TA(ICRS+ 5)*TS(ITPCRS(ICRS+ 5))
     &            +TA(ICRS+ 6)*TS(ITPCRS(ICRS+ 6))
     &            +TA(ICRS+ 7)*TS(ITPCRS(ICRS+ 7))
     &            +TA(ICRS+ 8)*TS(ITPCRS(ICRS+ 8))
     &            +TA(ICRS+ 9)*TS(ITPCRS(ICRS+ 9))
     &            +TA(ICRS+10)*TS(ITPCRS(ICRS+10))
     &            +TA(ICRS+11)*TS(ITPCRS(ICRS+11))
     &            +TA(ICRS+12)*TS(ITPCRS(ICRS+12))
     &            +TA(ICRS+13)*TS(ITPCRS(ICRS+13))
     &            +TA(ICRS+14)*TS(ITPCRS(ICRS+14))
     &            +TA(ICRS+15)*TS(ITPCRS(ICRS+15))
     &            +TA(ICRS+16)*TS(ITPCRS(ICRS+16))
     &            +TA(ICRS+17)*TS(ITPCRS(ICRS+17))
     &            +TA(ICRS+18)*TS(ITPCRS(ICRS+18))
     &            +TA(ICRS+19)*TS(ITPCRS(ICRS+19))
     &            +TA(ICRS+20)*TS(ITPCRS(ICRS+20))
     &            +TA(ICRS+21)*TS(ITPCRS(ICRS+21))
     &            +TA(ICRS+22)*TS(ITPCRS(ICRS+22))
     &            +TA(ICRS+23)*TS(ITPCRS(ICRS+23))
     &            +TA(ICRS+24)*TS(ITPCRS(ICRS+24))
     &            +TA(ICRS+25)*TS(ITPCRS(ICRS+25))
     &            +TA(ICRS+26)*TS(ITPCRS(ICRS+26))
     &            +TA(ICRS+27)*TS(ITPCRS(ICRS+27))
     &            +TA(ICRS+28)*TS(ITPCRS(ICRS+28))
     &            +TA(ICRS+29)*TS(ITPCRS(ICRS+29))
     &            +TA(ICRS+30)*TS(ITPCRS(ICRS+30))
     &            +TA(ICRS+31)*TS(ITPCRS(ICRS+31))
     &            +TA(ICRS+32)*TS(ITPCRS(ICRS+32))
     &            +TA(ICRS+33)*TS(ITPCRS(ICRS+33))
     &            +TA(ICRS+34)*TS(ITPCRS(ICRS+34))
     &            +TA(ICRS+35)*TS(ITPCRS(ICRS+35))
     &            +TA(ICRS+36)*TS(ITPCRS(ICRS+36))
     &            +TA(ICRS+37)*TS(ITPCRS(ICRS+37))
     &            +TA(ICRS+38)*TS(ITPCRS(ICRS+38))
     &            +TA(ICRS+39)*TS(ITPCRS(ICRS+39))
     &            +TA(ICRS+40)*TS(ITPCRS(ICRS+40))
         AS(IP)=AS(IP)+BUF
 310  CONTINUE
#ifndef __AIX__
!ocl END_CACHE_SUBSECTOR
!ocl END_CACHE_SECTOR_SIZE
#endif
C
      GOTO 900
C
CC
CC    FULL UNROL : 40 < NPPMAX <= 50
CC
#ifndef __AIX__
!ocl CACHE_SECTOR_SIZE(1,11)
!ocl CACHE_SUBSECTOR_ASSIGN(TS)
!!ocl UNROLL(0)
#endif
 400  CONTINUE
      DO 410 IP=1,NP
         BUF=0.0E0
         ICRS=(IP-1)*50
         BUF=BUF+  TA(ICRS+ 1)*TS(ITPCRS(ICRS+ 1))
     &            +TA(ICRS+ 2)*TS(ITPCRS(ICRS+ 2))
     &            +TA(ICRS+ 3)*TS(ITPCRS(ICRS+ 3))
     &            +TA(ICRS+ 4)*TS(ITPCRS(ICRS+ 4))
     &            +TA(ICRS+ 5)*TS(ITPCRS(ICRS+ 5))
     &            +TA(ICRS+ 6)*TS(ITPCRS(ICRS+ 6))
     &            +TA(ICRS+ 7)*TS(ITPCRS(ICRS+ 7))
     &            +TA(ICRS+ 8)*TS(ITPCRS(ICRS+ 8))
     &            +TA(ICRS+ 9)*TS(ITPCRS(ICRS+ 9))
     &            +TA(ICRS+10)*TS(ITPCRS(ICRS+10))
     &            +TA(ICRS+11)*TS(ITPCRS(ICRS+11))
     &            +TA(ICRS+12)*TS(ITPCRS(ICRS+12))
     &            +TA(ICRS+13)*TS(ITPCRS(ICRS+13))
     &            +TA(ICRS+14)*TS(ITPCRS(ICRS+14))
     &            +TA(ICRS+15)*TS(ITPCRS(ICRS+15))
     &            +TA(ICRS+16)*TS(ITPCRS(ICRS+16))
     &            +TA(ICRS+17)*TS(ITPCRS(ICRS+17))
     &            +TA(ICRS+18)*TS(ITPCRS(ICRS+18))
     &            +TA(ICRS+19)*TS(ITPCRS(ICRS+19))
     &            +TA(ICRS+20)*TS(ITPCRS(ICRS+20))
     &            +TA(ICRS+21)*TS(ITPCRS(ICRS+21))
     &            +TA(ICRS+22)*TS(ITPCRS(ICRS+22))
     &            +TA(ICRS+23)*TS(ITPCRS(ICRS+23))
     &            +TA(ICRS+24)*TS(ITPCRS(ICRS+24))
     &            +TA(ICRS+25)*TS(ITPCRS(ICRS+25))
     &            +TA(ICRS+26)*TS(ITPCRS(ICRS+26))
     &            +TA(ICRS+27)*TS(ITPCRS(ICRS+27))
     &            +TA(ICRS+28)*TS(ITPCRS(ICRS+28))
     &            +TA(ICRS+29)*TS(ITPCRS(ICRS+29))
     &            +TA(ICRS+30)*TS(ITPCRS(ICRS+30))
     &            +TA(ICRS+31)*TS(ITPCRS(ICRS+31))
     &            +TA(ICRS+32)*TS(ITPCRS(ICRS+32))
     &            +TA(ICRS+33)*TS(ITPCRS(ICRS+33))
     &            +TA(ICRS+34)*TS(ITPCRS(ICRS+34))
     &            +TA(ICRS+35)*TS(ITPCRS(ICRS+35))
     &            +TA(ICRS+36)*TS(ITPCRS(ICRS+36))
     &            +TA(ICRS+37)*TS(ITPCRS(ICRS+37))
     &            +TA(ICRS+38)*TS(ITPCRS(ICRS+38))
     &            +TA(ICRS+39)*TS(ITPCRS(ICRS+39))
     &            +TA(ICRS+40)*TS(ITPCRS(ICRS+40))
     &            +TA(ICRS+41)*TS(ITPCRS(ICRS+41))
     &            +TA(ICRS+42)*TS(ITPCRS(ICRS+42))
     &            +TA(ICRS+43)*TS(ITPCRS(ICRS+43))
     &            +TA(ICRS+44)*TS(ITPCRS(ICRS+44))
     &            +TA(ICRS+45)*TS(ITPCRS(ICRS+45))
     &            +TA(ICRS+46)*TS(ITPCRS(ICRS+46))
     &            +TA(ICRS+47)*TS(ITPCRS(ICRS+47))
     &            +TA(ICRS+48)*TS(ITPCRS(ICRS+48))
     &            +TA(ICRS+49)*TS(ITPCRS(ICRS+49))
     &            +TA(ICRS+50)*TS(ITPCRS(ICRS+50))
         AS(IP)=AS(IP)+BUF
 410  CONTINUE
#ifndef __AIX__
!ocl END_CACHE_SUBSECTOR
!ocl END_CACHE_SECTOR_SIZE
#endif
C
      GOTO 900
C
CC
CC    ORIGINAL
CC
 500  CONTINUE
      ICRS=0
      DO 510 IP=1,NP
         BUF=0.0E0
         DO 520 K=1,NPP(IP)
            ICRS=ICRS+1
            IP2=IPCRS(ICRS)
            BUF=BUF+A(ICRS)*S(IP2)
 520     CONTINUE   
         AS(IP)=AS(IP)+BUF
 510  CONTINUE
C
 900  CONTINUE
#ifdef cputime
C     CALL CPU_TIME( TBUF2 )
      TBUF2 = MPI_WTIME() 
      DTCPU=TBUF2-TBUF1
      DTCRSA=DTCRSA+DTCPU
      DTCRSR=DTCRSR+DTCPU*DTCPU
#endif
C
      IDUM = 1
      CALL DDCOMX(IPART,IDUM,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *            AS,AS,AS,NP,IUT0,IERR,RX,RY,MAXBUF)
      IF(IERR.NE.0) THEN
          WRITE(IUT0,*)
          WRITE(IUT0,*) ERMSGC
          RETURN
      ENDIF
#ifdef cputime
C     CALL CPU_TIME( TBUF3 )
      TBUF3 = MPI_WTIME() 
      DTCPU=TBUF3-TBUF1
      DTCRS2=DTCRS2+DTCPU
#endif
C
      RETURN
      END
