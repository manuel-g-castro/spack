C=======================================================================
      SUBROUTINE ALE_CALAX(A, S, AS, NP, NE, NCRS, IPCRS, NPP,
     *                   N,ME,IPART,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *                   RX,RY,IUT0,IERR)
C=======================================================================
      IMPLICIT NONE
      INTEGER NP, NE, NCRS
      REAL*4  A(NCRS*9), S(NP*3), AS(NP*3)
      INTEGER IPCRS(NCRS), NPP(NP)
      INTEGER IP, K, ICRS, IP2
C
      INTEGER MAXBUF,IDUM
      INTEGER N,ME,IPART,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,IUT0,IERR
      REAL*4  RX,RY
C
      DIMENSION LDOM(NDOM),NBPDOM(NDOM)
      DIMENSION IPSLF(MBPDOM,NDOM),IPSND(MBPDOM,NDOM)
      DIMENSION RX(0:N,ME),RY(0:N,ME)
C
      REAL*4  UBUF,VBUF,WBUF,AUBUF,AVBUF,AWBUF,
     *        A11,A12,A13,A21,A22,A23,A31,A32,A33
C
      CHARACTER*60 ERMSGC
     & /' ## SUBROUTINE CALAXC: FATAL      ERROR REPORT   ; RETURNED'/
C
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
      IERR=0
C
      MAXBUF = NE*(N+1)
C
      ICRS=0
      DO 110 IP=1,NP
          AUBUF=0.0E0
          AVBUF=0.0E0
          AWBUF=0.0E0
          DO 100 K=1,NPP(IP)
              ICRS=ICRS+1
              IP2=IPCRS(ICRS)
              UBUF=S(IP2+NP*0)
              VBUF=S(IP2+NP*1)
              WBUF=S(IP2+NP*2)
              A11=A((ICRS-1)*9+1)
              A12=A((ICRS-1)*9+2)
              A13=A((ICRS-1)*9+3)
              A21=A((ICRS-1)*9+4)
              A22=A((ICRS-1)*9+5)
              A23=A((ICRS-1)*9+6)
              A31=A((ICRS-1)*9+7)
              A32=A((ICRS-1)*9+8)
              A33=A((ICRS-1)*9+9)
              AUBUF=AUBUF+A11*UBUF+A12*VBUF+A13*WBUF
              AVBUF=AVBUF+A21*UBUF+A22*VBUF+A23*WBUF
              AWBUF=AWBUF+A31*UBUF+A32*VBUF+A33*WBUF
  100     CONTINUE   
          AS(IP+NP*0)=AUBUF
          AS(IP+NP*1)=AVBUF
          AS(IP+NP*2)=AWBUF
  110 CONTINUE   
C
      IDUM = 3
      CALL DDCOMX(IPART,IDUM,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *            AS,AS(NP+1),AS(2*NP+1),NP,IUT0,IERR,RX,RY,MAXBUF)
      IF(IERR.NE.0) THEN
          WRITE(IUT0,*)
          WRITE(IUT0,*) ERMSGC
          RETURN
      ENDIF
C
      RETURN
      END
