C=======================================================================
      SUBROUTINE DGNSCL(A,AR,NP,NE,NCRS,IPCRS,NPP,ME)
C=======================================================================
      IMPLICIT NONE
      INTEGER NP, NE, NCRS
      REAL*4  A(NCRS), AR(NP)
      INTEGER IPCRS(NCRS), NPP(NP)
      INTEGER IP, K, ICRS, IP2
      REAL*4  BUF
C
      INTEGER MAXBUF,IDUM,ICRDIG
      INTEGER ME,IUT0,IERR
C
      CHARACTER*60 ERMSGC
     & /' ## SUBROUTINE RELAXM: FATAL      ERROR REPORT   ; RETURNED'/
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
      CALL USTSTA(73)
C
      IERR=0
C
      ICRS=0
      DO 110 IP=1,NP
          BUF = 0.0E0
          DO 100 K=1,NPP(IP)
              ICRS = ICRS+1
              A(ICRS)=A(ICRS)/AR(IP)
  100     CONTINUE
  110 CONTINUE
C
      CALL USTEND(73)
C
      RETURN
      END
      
C=======================================================================
      SUBROUTINE DGNSCL2(A,AR,NP,NCRS,LSTDGN)
C=======================================================================
      IMPLICIT NONE
      INTEGER NP,NCRS
      REAL*4  A(NCRS), AR(NP)
      INTEGER LSTDGN(NCRS)
      INTEGER IP, ICRS
C 
      CALL USTSTA(77)
C
      DO 1000 ICRS=1,NCRS
          IP=LSTDGN(ICRS)
          A(ICRS)=A(ICRS)/AR(IP)
 1000 CONTINUE   

      CALL USTEND(77)
C
      RETURN
      END
