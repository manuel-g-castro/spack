      SUBROUTINE BCGS3X(IMODE,IPART,NMAX,EPS,ME,N2,NE,NEX,NP,
     *                  NPP,NCRS,IPCRS,AAAPC,NODE,B,S,NITR,RESR,
     *                  NDOM,MBPDOM,LDOM,NBPDOM,IPSLF,IPSND,NUMIP,
     *                  WEIGHT,R0,RK,PK,APK,ATK,TK,S0,RX,RY,MWRK,WRKN,
     *                  IUT0,IERR,AR,LPFIX3D,
     *                  JUNROL,NPPMAX,NCRS2,TS,TACRS,ITPCRS)
      IMPLICIT NONE
C
      REAL*4    AR(NP)
      INTEGER*4 LPFIX3D(NP)
C 
CCCC  [INPUT:CONTROL PARAMETERS]     
      INTEGER*4 IMODE,IPART,NMAX,ME,N2,NE,NEX(8),NP,IUT0
      REAL*4    EPS
C
CCCC  [INPUT:MATRIX]     
      INTEGER*4 NCRS,NPP(NP),IPCRS(NCRS),NODE(N2,NE) 
      REAL*4    AAAPC(NCRS),B(NP)
C
CCCC  [INPUT:INTER CONNECT DATA]
      INTEGER*4 NDOM,MBPDOM,LDOM(NDOM),NBPDOM(NDOM),
     *          IPSLF(MBPDOM,NDOM),IPSND(MBPDOM,NDOM),NUMIP(NP)
C
CCC  [INPUT/OUTPUT]
      REAL*4    S(NP)
C
CCC  [OUTPUT]
      INTEGER*4 NITR,IERR
      REAL*4    RESR
C
CCC  [WORK]    
      REAL*4    WRKSCT(NP),
     *          RX(0:N2,ME),RY(0:N2,ME),WEIGHT(NP),
     *          R0(NP),RK(NP),PK(NP),APK(NP),ATK(NP),TK(NP),S0(NP)
      INTEGER*4 MWRK
      REAL*4    WRKN(MWRK,4)
C
C     [FULL UNROOL]
      INTEGER*4 JUNROL
      INTEGER*4 NPPMAX,NCRS2,ITPCRS(NCRS2)
      REAL*4    TS(0:NP),TACRS(NCRS2)
C      
CCC  [LOCAL VARIABLE]    
      INTEGER*4 MAXBUF,IP,IBP
      REAL*4 RKDOT,RKDOTA,BDOT,BDOTA,APDOT,APDOTA,
     *       ATTDOT,ATTDTA,QK,AT2DOT,AT2DTA,RKDOTP,RSDOT,RSDOTA,
     *       EPS0,RES,RESMIN,ALFA,BETA
      DATA EPS0 / 1.E-30 /
C
      CHARACTER*60 ERMSGC
     & /' ## SUBROUTINE BCGST3X: FATAL     ERROR REPORT   ; RETURNED' /
C
C      SOLVE MATRIX EQUATION BY BI-CGSTAB METHOS
C         ( 3-D CALCULATION : SINGLE WORD & MULTI ELEMENT VERSION )
C                                           CODED BASED ON 'BCGSTB'
C      2011.01.14 MODIFIED TO SUPPORT MID NODE BY Y. YAMADE
C
C          OPERATION COUNTS:   77 FLOP /ELEMENT/ITERATION
C          DATA LOADINGS   :  101 WORDS/ELEMENT/ITERATION
C                           (  69 WORDS CONTIGUOUSLY,
C                               8 WORDS BY 4-WORD STRIDE, AND
C                              24 WORDS BY LIST )
C
C
C     ARGUMENT LISTINGS
C
C (1)INPUT
C
C (1.1) CONTROL PARAMETERS
C INT *4 IMODE        ; BEST SOLUTION STORING FUNCTION (1:ON,0:OFF)
C INT *4 IPART        ; SUB-DOMAIN NUMBER THAT THIS TASK SHOULD TAKE/IS
C                       TAKING CARE OF. IPART BEING SET ZERO MEANS THAT
C                       THE PROGRAM SHOULD RUN/IS RUNNING IN SERIAL 
C                       MODE.
C INT *4 NMAX         ; MAXIMUM NUMBER OF ITERATIONS
C INT *4 ME           ; MAX. NUMBER OF TOTAL ELEMENTS
C INT *4 N            ; =8
C INT *4 N2           ; =8
C INT *4 NE           ; NUMBER OF TOTAL ELEMENTS
C INT *4 NEX       (I); INCLUDES NUMBER OF ELEMENTS AND NUMBER OF 
C                       LOCAL NODES AS FOLOOWS
C    NEX(1)           ; NUMBER OF TET.    ELEMENTS
C    NEX(2)           ; NUMBER OF PYRAMID ELEMENTS
C    NEX(3)           ; NUMBER OF WEGDE   ELEMENTS
C    NEX(4)           ; NUMBER OF HEX.    ELEMENTS
C    NEX(5)           ; NUMBER OF LOCAL NODES IN A TET.    ELEMENT (=4)
C    NEX(6)           ; NUMBER OF LOCAL NODES IN A PYRAMID ELEMENT (=5)
C    NEX(7)           ; NUMBER OF LOCAL NODES IN A WEGDE   ELEMENT (=6)
C    NEX(8)           ; NUMBER OF LOCAL NODES IN A HEX.    ELEMENT (=8)
C INT *4 NP           ; NUMBER OF TOTAL    NODES
C INT *4 IUT0         ; FILE NUMBER TO REPORT ERROR OCCURRENCE
C REAL*4 EPS          ; CONVERGENCE CRITERIA (L2-NORM RESIDUAL)
C
C (1.2) MATRIX
C INT *4 NCRS         ; NUMBER OF NONZERO ELEMENTS IN MATRIX OF CRS FORMAT
C INT *4 NPP     (IP) ; NUMBER OF ADJACENT NODES    TO NODE    IP
C INT *4 IPCRS (ICRS) ; NODE NO. TABLE BASED ON CRS FORMAT
C INT *4 NODE  (I,IE) ; NODE TABLE
C INT *4 AAAPC (ICRS) ; MATRIX COEEFICIENTS
C REAL*4 B       (IP) ; GLOBAL FORCE VECTOR
C
C   NOTES ; FOR PARALLEL COMPUTATIONS, CONTRIBUTIONS FROM THE
C          NEIGHBORING DOMAINS MUST HAVE BEEN SUPERIMPOSED
C          TO THE GLOBAL FORCE VECTOR BEFORE THIS SUBROUTINE IS
C          CALLED.
C
C (1.3) INTER CONNECT DATA
C INT *4 NDOM         ; NUMBER OF THE NERIBERING SUB-DOMAINS
C INT *4 MBPDOM       ; THE MAXIMUM NUMBER OF THE INTER-CONNECT 
C                       BOUNDARY NODES FOR ONE NEIBERING SUB-DOMAIN
C INT *4 LDOM   (IDOM); NEIBERING SUB-DOMAIN NUMBER
C INT *4 NBPDOM (IDOM); NUMBER OF INTER-CONNECT BOUNDARY NODES
C                       SHARING WITH THE IDOM'TH NEIBERING SUB-DOMAIN,
C                       LDOM(IDOM)
C INT *4 IPSLF(I,IDOM); INTER-CONNECT BOUNDARY NODE NUMBER IN THE
C                       CALLING TASK'S SUB-DOMAIN, FOR THE IDOM'TH
C                       NEIBERING SUB-DOMAIN, LDOM(IDOM)
C INT *4 IPSND(I,IDOM); INTER-CONNECT BOUNDARY NODE NUMBER IN THE
C                       SUB-DOMAIN THAT IS RECEIVING THE CALLING
C                       TASK'S RESIDUALS.
C INT *4 NUMIP    (IP);
C
C (2) INPUT-OUTPUT
C REAL*4 S         (IP); GLOBAL SOLUTION VECTOR (PROVIDE INITIAL GUESS)
C
C (3) OUTPUT
C REAL*4 RES           ; L2-NORM RESIDUAL OF THE FINAL SOLUTION VECTOR
C INT *4 NITR          ; NUMBER OF ITERATIONS DONE
C INT* 4 IERR          ; RETURN CODE TO REPORT ERROR OCCURRENCE
C                   0 --- NORMAL TERMINATION
C                   1 --- A FATAL ERROR HAS OCCURRED
C
#ifdef USE_TIMER
      real*8 ts0, te0

      include 'timer.h'
      include 'mpif.h'

      nbcgs3x = nbcgs3x + 1
      tstart = MPI_WTIME()
#endif      
      IF(NMAX.EQ.0) then
#ifdef USE_TIMER
         tend = MPI_WTIME()
         tbcgs3x = tbcgs3x + (tend - tstart)
#endif
         RETURN
      ENDIF
C
      NITR=0
      MAXBUF=8*NE
C
C
CCC   1. SET WEIGHTING FUNCTION FOR COMPUTING AN INNER PRODUCT
C
C
      DO 100 IP = 1 , NP
          WEIGHT(IP) = 1.E0/(FLOAT(NUMIP(IP))+1.E0)
  100 CONTINUE
C
C
CCC   2. SET INITIAL RESIDUAL VECTOR AND SEARCH-DIRECTION VECTOR
C
C   
C          OPERATION COUNTS:   36 FLOP /ELEMENT
C          DATA LOADINGS   :   48 WORDS/ELEMENT
C                           (  32 WORDS CONTIGUOUSLY,
C                               4 WORDS BY 4-WORD STRIDE, AND
C                              12 WORDS BY LIST )
#ifdef USE_TIMER
      tend = MPI_WTIME()
      tbcgs3x = tbcgs3x + (tend - tstart)
#endif
      CALL USTSTA(13)
      CALL CALAX3(AAAPC, S, RK, NP, NE, NCRS, IPCRS, NPP,
     *            N2,ME,IPART,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *            RX,RY,IUT0,IERR,
     *            N2,NEX,NODE, 
     *            MWRK,WRKN,AR,LPFIX3D,
     *            JUNROL,NPPMAX,NCRS2,TS,TACRS,ITPCRS)
      CALL USTEND(13)
#ifdef USE_TIMER
      tstart = MPI_WTIME()
#endif          
C
      IF(IERR.EQ.1) THEN
          WRITE(IUT0,*) ERMSGC
#ifdef USE_TIMER
          tend = MPI_WTIME()
          tbcgs3x = tbcgs3x + (tend - tstart)
#endif
          RETURN
      ENDIF
C
      RKDOT = 0.E0
      BDOT = 0.E0
      DO 400 IP = 1 , NP
          RK (IP) = B (IP)-RK (IP)
          R0 (IP) = RK(IP)
          PK (IP) = RK(IP)
          TK (IP) = 0.E0
          S0 (IP) = S(IP)
          RKDOT = RKDOT+WEIGHT(IP)*R0(IP)*RK(IP)
          BDOT = BDOT+WEIGHT(IP)*B(IP)*B(IP)
  400 CONTINUE
C
      IF(IPART.GE.1) THEN
#ifdef USE_TIMER
          tend = MPI_WTIME()
          tbcgs3x = tbcgs3x + (tend - tstart)
#endif
          CALL DDCOM2(RKDOT,RKDOTA)
          CALL DDCOM2(BDOT,BDOTA)
#ifdef USE_TIMER
          tstart = MPI_WTIME()
#endif          
          RKDOT = RKDOTA
          BDOT = BDOTA
      ENDIF
C
      IF(ABS(BDOT).LE.EPS0) BDOT = 1.0E0
C
      RES  = SQRT(RKDOT)
      RESR = RES/SQRT(BDOT)
      RESMIN = RES
C
C     IF(RES.LE.EPS.OR.RESR.LE.EPS) RETURN
C     USE RELATIVE RESIDUAL
      IF(RESR.LE.EPS) then
#ifdef USE_TIMER
         tend = MPI_WTIME()
         tbcgs3x = tbcgs3x + (tend - tstart)
#endif
         RETURN
      ENDIF
C
C
CCC   3. COMPUTE PRODUCT OF COEFFICIENT MATRIX AND SEARCH-DIRECTION VECTOR
CCC      AND INNER PRODUCT OF COMPUTED PRODUCT AND SEARCH-DIRECTION VECTOR
C
C 
 10   CONTINUE
C
      NITR=NITR+1
C
C
CCC   3.1 COMPUTE APK,ALFA
C
C
C          OPERATION COUNTS:   36 FLOP /ELEMENT
C          DATA LOADINGS   :   48 WORDS/ELEMENT
C                           (  32 WORDS CONTIGUOUSLY,
C                               4 WORDS BY 4-WORD STRIDE, AND
C                              12 WORDS BY LIST )
#ifdef USE_TIMER
      tend = MPI_WTIME()
      tbcgs3x = tbcgs3x + (tend - tstart)
#endif
      CALL USTSTA(13)
      CALL CALAX3(AAAPC, PK, APK, NP, NE, NCRS, IPCRS, NPP,
     *            N2,ME,IPART,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *            RX,RY,IUT0,IERR,
     *            N2,NEX,NODE, 
     *            MWRK,WRKN,AR,LPFIX3D,
     *            JUNROL,NPPMAX,NCRS2,TS,TACRS,ITPCRS)
      CALL USTEND(13)
#ifdef USE_TIMER
      tstart = MPI_WTIME()
#endif          
C
      IF(IERR.EQ.1) THEN
          WRITE(IUT0,*) ERMSGC
#ifdef USE_TIMER
          tend = MPI_WTIME()
          tbcgs3x = tbcgs3x + (tend - tstart)
#endif
          RETURN
      ENDIF
C
      APDOT = 0.E0
      DO 700 IP = 1 , NP
          APDOT = APDOT+WEIGHT(IP)*R0(IP)*APK(IP)
  700 CONTINUE
C
      IF(IPART.GE.1) THEN
#ifdef USE_TIMER
          tend = MPI_WTIME()
          tbcgs3x = tbcgs3x + (tend - tstart)
#endif
          CALL DDCOM2(APDOT,APDOTA)
#ifdef USE_TIMER
          tstart = MPI_WTIME()
#endif          
          APDOT = APDOTA
      ENDIF
C
      IF(APDOT .EQ. 0.0E0) then
#ifdef USE_TIMER
         tend = MPI_WTIME()
         tbcgs3x = tbcgs3x + (tend - tstart)
#endif
         RETURN
      endif
      ALFA = RKDOT/APDOT
C
C
CCC   3.2 COMPUTE TK=RK-ALFA*APK     
C
C
      DO 800 IP = 1 , NP
          TK (IP) = RK(IP)-ALFA*APK(IP) 
  800 CONTINUE 
C
C
CCC   3.3 COMPUTE ATK
C
C
C          OPERATION COUNTS:   36 FLOP /ELEMENT
C          DATA LOADINGS   :   48 WORDS/ELEMENT
C                           (  32 WORDS CONTIGUOUSLY,
C                               4 WORDS BY 4-WORD STRIDE, AND
C                              12 WORDS BY LIST )
#ifdef USE_TIMER
      tend = MPI_WTIME()
      tbcgs3x = tbcgs3x + (tend - tstart)
#endif
      CALL USTSTA(13)
      CALL CALAX3(AAAPC, TK, ATK, NP, NE, NCRS, IPCRS, NPP,
     *            N2,ME,IPART,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *            RX,RY,IUT0,IERR,
     *            N2,NEX,NODE, 
     *            MWRK,WRKN,AR,LPFIX3D,
     *            JUNROL,NPPMAX,NCRS2,TS,TACRS,ITPCRS)
      CALL USTEND(13)
#ifdef USE_TIMER
      tstart = MPI_WTIME()
#endif          
C
      IF(IERR.EQ.1) THEN
          WRITE(IUT0,*) ERMSGC
#ifdef USE_TIMER
          tend = MPI_WTIME()
          tbcgs3x = tbcgs3x + (tend - tstart)
#endif
          RETURN
      ENDIF
C
C
CCC   3.4 COMPUTE QK
C
C
      ATTDOT  = 0.E0
      AT2DOT  = 0.E0
      DO 1100 IP = 1 , NP
          ATTDOT = ATTDOT+WEIGHT(IP)*ATK(IP)* TK(IP)
          AT2DOT = AT2DOT+WEIGHT(IP)*ATK(IP)*ATK(IP)
 1100 CONTINUE
C
      IF(IPART.GE.1) THEN
#ifdef USE_TIMER
          tend = MPI_WTIME()
          tbcgs3x = tbcgs3x + (tend - tstart)
#endif
          CALL DDCOM2(ATTDOT,ATTDTA)
          CALL DDCOM2(AT2DOT,AT2DTA)
#ifdef USE_TIMER
          tstart = MPI_WTIME()
#endif          
          ATTDOT = ATTDTA
          AT2DOT = AT2DTA
      ENDIF
C
      IF(AT2DOT .EQ. 0.E0) then
#ifdef USE_TIMER
         tend = MPI_WTIME()
         tbcgs3x = tbcgs3x + (tend - tstart)
#endif
         RETURN
      endif
      QK = ATTDOT/AT2DOT
C
C
CCC   3.5 UPDATE SOLUTION VECTOR AND RESIDUAL VECTOR
CCC   3.6 RETURN IF L2-NORM OF UPDATED SOLUTION VECTOR IS LESS THAN CRITERIA
C
C
      RKDOTP = RKDOT
      RKDOT  = 0.E0
      RSDOT  = 0.E0
      DO 1200 IP = 1 , NP
          S  (IP) = S (IP)+ ALFA*PK(IP) + QK*TK (IP)   
          RK (IP) = TK(IP)              - QK*ATK(IP) 
          RKDOT = RKDOT+WEIGHT(IP)*R0(IP)*RK(IP)
          RSDOT = RSDOT+WEIGHT(IP)*RK(IP)*RK(IP)
 1200 CONTINUE
C
C
      IF(IPART.GE.1) THEN
#ifdef USE_TIMER
          tend = MPI_WTIME()
          tbcgs3x = tbcgs3x + (tend - tstart)
#endif
          CALL DDCOM2(RKDOT,RKDOTA)
          CALL DDCOM2(RSDOT,RSDOTA)
#ifdef USE_TIMER
          tstart = MPI_WTIME()
#endif          
          RKDOT = RKDOTA
          RSDOT = RSDOTA
      ENDIF
C
      RES  = SQRT(RSDOT)
      RESR = RES/SQRT(BDOT)
C
      IF(RESR.LE.EPS) then
#ifdef USE_TIMER
         tend = MPI_WTIME()
         tbcgs3x = tbcgs3x + (tend - tstart)
#endif
         RETURN
      endif
C
C
CCC   3.7 UPDATE SEARCH-DIRECTION VECTOR
C     
C
      IF(QK     .EQ. 0.E0) then
#ifdef USE_TIMER
         tend = MPI_WTIME()
         tbcgs3x = tbcgs3x + (tend - tstart)
#endif
         RETURN
      endif
      IF(RKDOTP .EQ. 0.E0) then
#ifdef USE_TIMER
         tend = MPI_WTIME()
         tbcgs3x = tbcgs3x + (tend - tstart)
#endif
         RETURN
      endif
      BETA = (ALFA/QK)*(RKDOT/RKDOTP)
C 
      DO 1300 IP = 1 , NP
          PK (IP) = RK(IP)+BETA*(PK(IP)-QK*APK(IP))
 1300 CONTINUE
C
C
CCC   3.7 RETURN IF ITERATION NUMBER HAS REACHED THE GIVEN MAXIMUM NUMBER,
CCC       OTHERWISE CONTINUE ITERATIONS UNTIL SOLUTION IS CONVERGED
C
C
      IF(NITR.EQ.NMAX) THEN          
C
          IF(IMODE.EQ.0) then
#ifdef USE_TIMER
             tend = MPI_WTIME()
             tbcgs3x = tbcgs3x + (tend - tstart)
#endif
             RETURN
          endif
C
          DO 1400 IP = 1 ,  NP
              S(IP)=S0(IP)
 1400      CONTINUE   
          RES=RESMIN
#ifdef USE_TIMER
          tend = MPI_WTIME()
          tbcgs3x = tbcgs3x + (tend - tstart)
#endif
          RETURN
      END IF  
C
      IF(RES.LT.RESMIN) THEN
          RESMIN=RES
          DO 1500 IP = 1 ,  NP
              S0(IP)=S(IP)
 1500     CONTINUE   
      ENDIF
      GO TO 10
C
      END
