C=======================================================================
      SUBROUTINE CALAXC(A, S, AS, NP, NE, NCRS, IPCRS, NPP,
     *                  N,ME,IPART,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *                  RX,RY,IUT0,IERR,
     *                  N2,NEX,NODE, 
     *                  MWRK,WRKN,
     *                  JUNROL,NPPMAX,NCRS2,TS,TA,ITPCRS)
C=======================================================================
      IMPLICIT NONE
      INTEGER NP, NE, NCRS
      REAL*4 A(NCRS), S(NP), AS(NP)
      INTEGER IPCRS(NCRS), NPP(NP)
      INTEGER IP, IP_TMP, K, ICRS, IP2, IBP
      REAL*4  BUF
C
      INTEGER MAXBUF,IDUM
      INTEGER N,ME,IPART,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,IUT0,IERR
      REAL*4  RX,RY
C
      DIMENSION LDOM(NDOM),NBPDOM(NDOM)
      DIMENSION IPSLF(MBPDOM,NDOM),IPSND(MBPDOM,NDOM)
      DIMENSION RX(0:N,ME),RY(0:N,ME)
C
CCCC  [INPUT:MID NODE DATA]
      INTEGER*4 N2,NEX(8),NODE(N2,NE)
      INTEGER*4 MWRK
      REAL*4    WRKN(MWRK,2)
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
      IF (NPPMAX.EQ.27) GOTO 200
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
         ICRS=(IP-1)*27
         DO 220 IP_TMP=1,27
            BUF=BUF+TA(ICRS+IP_TMP)*TS(ITPCRS(ICRS+IP_TMP))
 220     CONTINUE           
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
CC    FULL UNROL : 27 < NPPMAX <= 40
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
         DO 320 IP_TMP=1,40
            BUF=BUF+TA(ICRS+IP_TMP)*TS(ITPCRS(ICRS+IP_TMP))
 320     CONTINUE           
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
         DO 420 IP_TMP=1,50
            BUF=BUF+TA(ICRS+IP_TMP)*TS(ITPCRS(ICRS+IP_TMP))
 420     CONTINUE
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
CC    WRITE(*,*) 'DRCRS',DTCPU
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
C
#ifdef cputime
C     CALL CPU_TIME( TBUF3 )
      TBUF3 = MPI_WTIME() 
      DTCPU=TBUF3-TBUF1
      DTCRS2=DTCRS2+DTCPU
#endif
C
      RETURN
      END
