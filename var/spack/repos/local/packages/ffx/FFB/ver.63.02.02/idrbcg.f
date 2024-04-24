!======================================================================!
!                                                                      !
! Software Name : FFB ver. 7.2                                         !
!                                                                      !
! Module Name : IDR Solver                                             !
!                                                                      !
! Written by Yoshihisa SHIZAWA (RIST)                                  !
!                                                                      !
! Contact address : RIST                                               !
!                                                                      !
! Copyright 2010, 2011 by Research Organization                        !
! for Information Science & Technology (RIST)                          !
!                                                                      !
!======================================================================!
      SUBROUTINE IDRBCG(ME,N2,NE,NP,NS,NL,NMAX,EPS,RES,RESR,NITR,
     *                  NCRS,NPP,ACRS,IPCRS,B,S,NUMIP,
     *                  IPART,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *                  JUNROL,NPPMAX,NCRS2,TS,TACRS,ITPCRS,
     *                  IUT0,IUT6,IERR)
      IMPLICIT NONE
CC[INPUT]
      INTEGER*4 ME,N2,NE,NP,NS,NL,NMAX,NCRS,IUT0,IUT6,
     *          NPP(NP),IPCRS(NCRS),NUMIP(NP),
     *          IPART,NDOM,MBPDOM,LDOM(NDOM),NBPDOM(NDOM),
     *          IPSLF(MBPDOM,NDOM),IPSND(MBPDOM,NDOM)
      REAL*4    EPS,ACRS(NCRS),B(NP)
C
CC[IN-OUT]
      REAL*4    S(NP)
C
CC[OUT]
      INTEGER*4 NITR,IERR
      REAL*4    RES,RESR
C
CC[LOCAL]
      INTEGER*4 IWORKS(NS)
      INTEGER*4 MAXBUF,IP,IL,IIL,JL,IS,JS,JJS,IR,I,J,K
      REAL*4    WEIGHT(NP),RESV(NP,0:NL+1),UMAT(NP,NS,0:NL+1),
     *          TR0T(NS,NP),WRKS01(NP,NS),WRKS02(NP,NS),WRK01(NP),
     *          RX(0:N2,ME),RY(0:N2,ME)
      REAL*4    GAM0(NL),GAM1(NL),GAM2(NL),TAU(NL,NL),
     *          MMAT(NS,NS),WKSMAT(NS,NS),MVEC(NS),BETA(NS),
     *          BNRM,R0NRM,SIGIJ,SIGII,SIGJJ,SIGJ0,BUF
C
C     [FULL UNROOL]
      INTEGER*4 JUNROL
      INTEGER*4 NPPMAX,NCRS2,ITPCRS(NCRS2)
      REAL*4    TS(0:NP),TACRS(NCRS2)
C
      INTEGER*4 NUM1
      DATA NUM1  /1/
      REAL*4    OMEGA
      DATA OMEGA  /-1.0E0/
C
C     SOLVE A MATRIX EQUATION BY IDR-BCG METHOD
C
C     ARGUMENTS LISTINGS
C
C     (1) INPUT
C         INTEGER ME         ; MAX. NUMBER OF ELEMENTS
C         INTEGER N          ; MAXIMUM NUMBER OF NODES ASSIGNED TO AN ELEMENT (=8)
C         INTEGER NE         ; NUMBER OF ELEMENTS
C         INTEGER NP         ; NUMBER OF NODES
C         INTEGER NS         ; IDR-LEVEL INDEX
C         INTEGER NL         ; IDR-LEVEL INDEX
C         INTEGER NMAX       ; MAXIMUM NUMBER OF ITERATION
C         REAL    EPS        ; CONVERGENCE CRITERIA (I,L2-NORM RESIDUAL,J)
C         REAL    ACRS(NCRS) ; CRS-MATRIX
C         INTEGER NPP(NP+1)  ; INDEX FOR FIRST NON-ZERO ELEMENT OF i-th ROW OF A
C         INTEGER IPCRS(NCRS); COLUMN ADDRESSING OF i-th ELEMENT OF ACRS
C         REAL    B(NP)      ; RIGHT-HAND SIDE VECTOR OF AS=B
C         INTEGER NUMIP(NX)  ; COUNTING OF INTER-CONNECT BOUNDARY NODES SHARED
C
C         INTEGER IPART           ; SUB-DOMAIN INDEX OF THE TASK
C                                   (IF PROGRAM IS RUN IN SERIAL-MODE, IPART=0)
C         INTEGER LDOM(NDOM)      ; NEIGHBOURING SUB-DOMAIN NUMBER
C         INTEGER NBPDOM(NDOM)    ; NUMBER OF INTER-CONNECT BOUNDARY NODES SHARING WITH
C                                   THE i-th NEIGHBOURING SUB-DOMAIN LDOM(i)
C         INTEGER NDOM            ; NUMBER OF NEIGHBOURING SUB-DOMAINS
C         INTEGER IPSLF(NBP,NDOM) ; INTER-CONNECT BOUNDARY NODE-NUMBER 
C                                   IN THE CALLING TASK'S SUB-DOMAIN,
C                                   FOR THE i-th NEIGHBOURING SUB-DOMAIN, LDOM(i)
C         INTEGER IPSND(NBP,NDOM) ; INTER-CONNECT BOUNDARY NODE-NUMBER IN THE 
C                                   SUB-DOMAIN THE IS RECIEVING THE CALLING TASK'S RESIDUALS
C         INTEGER MBPDOM          ; THE MAXIMUM NUMBER OF THE INTER-CONNECT BOUNDARY NODES
C
C     (2) INPUT-OUTPUT
C         REAL    S(NP)      ; (INPUT)  / INITIAL GUESS OF SOLUTION VECTOR  
C                              (OUTPUT) / GLOBAL SOLUTION VECTOR
C     (3) OUTPUT
C         REAL    RES        ; ABSOLUTE L2-NORM RESIDUAL OF THE FINAL SOLUTION VECTOR
C         REAL    RESR       ; RELATIVE L2-NORM RESIDUAL OF THE FINAL SOLUTION VECTOR
C         INTEGER NITR       ; NUMBER OF ITERATIONS DONE 
C         INTEGER IERR       ; RETURN CODE TO REPORT ERROR OCCURENCE
C
C
      IERR=0
C
CC      WRITE(IUT6,*) "SOLVER: IDRBCG"
CC      WRITE(IUT6,*) "SOLVER PARAMETERS: "
CC      WRITE(IUT6,*) "       NS: ", NS
CC      WRITE(IUT6,*) "       NL: ", NL
C
      MAXBUF = 4*ME
      IF(NMAX.EQ.0) RETURN
C     
C     // PREPARE WEIGHT FACTOR
      IF(IPART.EQ.0) THEN
          DO 100 IP=1, NP
              WEIGHT(IP) = 1.0E0
 100      CONTINUE
      ELSE
          DO 110 IP=1, NP
              WEIGHT(IP) = 1.E0 / FLOAT(NUMIP(IP) + 1)
 110      CONTINUE
      ENDIF
C
C     
C     // INITIAL RESIDUAL (NORM OF B-VECTOR)
C     NOTE THAT DDCOM2 IS CALLED IN 'DOTWVC' 
      CALL DOTWVC (NP,B,B,WEIGHT,BNRM)
C     
C  // INITIALZIE COEFFICIENTS GAMMA AND TAU
C     
      DO 120 IL=1, NL
          GAM0(IL) = 0.0E0
          GAM1(IL) = 0.0E0
          GAM2(IL) = 0.0E0
          DO 121 JL=1, NL
              TAU(IL,JL) = 0.0E0
 121      CONTINUE
 120  CONTINUE
C
C      WRITE(IUT6,*) "STEP 1"
C  // STEP 1: PREPARE MATRIX TR0T
C
      DO 200 IS=1, NS
          DO 210 IP=1, NP
              TR0T(IS,IP) = 0.0E0
 210      CONTINUE
 200  CONTINUE
C     
C     // SEED OF RANDOM VALUE
      IR = 12345 + IPART
C
C     // PREPARE TR0T by (PSEUDO) RANDOM MATRIX
      DO 300 IS=1, NS
          CALL URAND1(NP,WRKS01(1,IS),IR)
          DO 310 IP=1, NP
              WRKS01(IP,IS) = (WRKS01(IP,IS)-0.5E0)*WEIGHT(IP)
  310     CONTINUE
          CALL DDCOMX(IPART,NUM1,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *                WRKS01(1,IS),WRK01,WRK01,NP,
     *                IUT0,IERR,RX,RY,MAXBUF)
  300 CONTINUE      
C
      CALL MGS1GV(NP,NS,WRKS01,TR0T,WEIGHT)
C
C  // CALCULATE AS
C
      CALL CALAX0(ACRS,S,WRK01,NP,NE,NCRS,IPCRS,NPP,
     *            N2,ME,IPART,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *            RX,RY,JUNROL,NPPMAX,NCRS2,TS,TACRS,ITPCRS,
     *            IUT0,IERR)
C
C      WRITE(IUT6,*) "STEP 2a,3"
C  // STEP 2a,3: INITIAL RESIDUAL AND U_i0
C
      DO 400 IP=1, NP
          BUF  = B(IP) - WRK01(IP)
          RESV(IP  ,0) = BUF
          UMAT(IP,1,0) = BUF
 400  CONTINUE
C
C      WRITE(IUT6,*) "STEP 2b"
C  // STEP 2b: U_0 = [r, Ar, A^2r, ..., A^(S-1)r]
C
      DO 410 JS=2, NS
          CALL CALAX0(ACRS,UMAT(1,JS-1,0),UMAT(1,JS,0),
     *                NP,NE,NCRS,IPCRS,NPP,
     *                N2,ME,IPART,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *                RX,RY,JUNROL,NPPMAX,NCRS2,TS,TACRS,ITPCRS,
     *                IUT0,IERR)
 410  CONTINUE
C
C      WRITE(IUT6,*) "STEP 2c"
C  // STEP 2c: TRANSFORM U_0
C      
      DO 430 IS=1, NS
          DO 431 IP=1, NP
              WRKS01(IP,IS) = UMAT(IP,IS,0) 
 431      CONTINUE
 430  CONTINUE
      CALL MGSGVC(NP,NS,WRKS01,WRKS02,WEIGHT)
      DO 440 IS=1, NS
          DO 441 IP=1, NP
              UMAT(IP,IS,0) = WRKS02(IP,IS)
 441      CONTINUE
 440  CONTINUE
C
C      WRITE(IUT6,*) "STEP 2d"
C  // STEP 2d: CALCULATE U_1: U_1 = A U_0
C
      DO 450 JS=1, NS
          CALL CALAX0(ACRS,UMAT(1,JS,0),UMAT(1,JS,1),
     *                NP,NE,NCRS,IPCRS,NPP,
     *                N2,ME,IPART,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *                RX,RY,JUNROL,NPPMAX,NCRS2,TS,TACRS,ITPCRS,
     *                IUT0,IERR)
 450  CONTINUE
C
C      WRITE(IUT6,*) "STEP 4a"
C  // STEP 4a: CALCULATE M: M = TR0T^* U_1
C
      DO 500 JS=1, NS
          CALL MATMLW (NP,NS,TR0T,UMAT(1,JS,1),WRK01,WEIGHT)
          CALL DDCOM2V(WRK01,MMAT(1,JS),NS)
 500  CONTINUE
C     
C      WRITE(IUT6,*) "STEP 4b"
C  // STEP 4b: CALCULATE m: m = TR0T^* r_0
C
      CALL MATMLW (NP,NS,TR0T,RESV(1,0),WRK01,WEIGHT)
      CALL DDCOM2V(WRK01,BETA,NS)
C
C      WRITE(IUT6,*) "STEP 5"
C  // STEP 5: SOLVE EQUATION: M beta = m for beta
C
      DO 520 JS=1, NS
          DO 521 IS=1, NS
              WKSMAT(JS,IS) = MMAT(IS,JS)
 521      CONTINUE
          WRK01(JS)=BETA(JS)
 520  CONTINUE
C
      CALL MATGAU(NS,NS,WKSMAT,BETA,WRK01,IWORKS,IUT0,IERR)
C
      IF(IERR.NE.0) RETURN
C 
C
C     
C      WRITE(IUT6,*) "STEP 6a"
C  // STEP 6a: CALCULATE RESIDUAL r_0: r_0 = r_0 - U_1 beta
C
      CALL MATMLS(NP,NS,UMAT(1,1,1),BETA,WRK01)
      DO 542 IP=1, NP
          RESV(IP,0) = RESV(IP,0) - WRK01(IP)
 542  CONTINUE
C
C      WRITE(IUT6,*) "STEP 6b"
C  // STEP 6b: UPDATE SOLUTION VECTOR: x_0 = x_0 + U_0 beta
C
      CALL MATMLS(NP,NS,UMAT(1,1,0),BETA,WRK01)
      DO 552 IP=1, NP
          S(IP) = S(IP) + WRK01(IP)
 552  CONTINUE
C
C      WRITE(IUT6,*) "STEP 7a"
C  // STEP 7a: RESIDUAL r_1: r_1 = A r_0   
C
      CALL CALAX0(ACRS,RESV(1,0),RESV(1,1),
     *            NP,NE,NCRS,IPCRS,NPP,
     *            N2,ME,IPART,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *            RX,RY,JUNROL,NPPMAX,NCRS2,TS,TACRS,ITPCRS,
     *            IUT0,IERR)
C
C      WRITE(IUT6,*) "STEP 7b"
C  // STEP 7b: Nitr = 0, omega = -1
C
      NITR  = 0
      OMEGA = -1.0E0
C
C      WRITE(IUT6,*) "STEP 8"
C  // STEP 8: ITERATION LOOP
C       
 8000 CONTINUE
C
C      WRITE(IUT6,*) "STEP 8a"
C  // STEP 8a: CALCULATE ||r_0||
C
      CALL DOTWVC (NP,RESV(1,0),RESV(1,0),WEIGHT,R0NRM)
      RES  = SQRT(R0NRM)
      RESR = SQRT(R0NRM/BNRM)
C
C      WRITE(IUT6,*) "STEP 8b"
C  // STEP 8b: CONVERGENCE CHECK
C
CC    WRITE(IUT6,*) 
CC   * "     NITR =", NITR, "  RES =", RESR
CCYYDEB---
      IF(IPART.LE.1) THEN
          WRITE(90,'(I8,E13.5)') NITR,RESR
      ENDIF
CCYYDEB---
C
      IF(RESR.LT.EPS.OR.NITR.GT.NMAX) GOTO 9000
C
C       
C      WRITE(IUT6,*) "STEP 9"
C  // STEP 9: M  = -omega M       
C
      DO 600 IS = 1, NS
          DO 601 JS = 1, NS
              MMAT(IS,JS) = - OMEGA * MMAT(IS,JS)
  601     CONTINUE
  600 CONTINUE
C
C      WRITE(IUT6,*) "STEP 10"
C  // STEP 10: FOR i = 0, 1, ..., L-1       
      DO 8100 IL = 0, NL-1
C
C      WRITE(IUT6,*) "STEP 11"
C  // STEP 11: IF(Nitr = 0), SKIP i=0 LOOP
          IF(NITR.EQ.0.AND.IL.EQ.0) THEN
              IIL = 1
          ELSE
              IIL = IL
          ENDIF
C
C      WRITE(IUT6,*) "STEP 12"
C  // STEP 12: CALCULATE m: m = TR0T^* r_i
C
          CALL MATMLW (NP,NS,TR0T,RESV(1,IIL),WRK01,WEIGHT)
          CALL DDCOM2V(WRK01,MVEC,NS)
C     
C      WRITE(IUT6,*) "STEP 13"
C  // STEP 13: FOR j=1, ..., S
C
          DO 8200 JJS = 1, NS
C               
C      WRITE(IUT6,*) "STEP 14"
C  // STEP 14: IF(j=1)
C     
              IF(JJS.EQ.1) THEN
C
C      WRITE(IUT6,*) "STEP 15"
C  // STEP 15: SOLVE EQUATION: M beta = m for beta
C                   
                  DO 800 IS=1, NS
                      BETA(IS)=MVEC(IS)
                      DO 801 JS=1, NS
                          WKSMAT(JS,IS) = MMAT(IS,JS)
 801                  CONTINUE
 800              CONTINUE
                  CALL  MATGAU(NS,NS,WKSMAT,BETA,
     *                         WRK01,IWORKS,IUT0,IERR)
                  IF(IERR.NE.0) RETURN
C
C      WRITE(IUT6,*) "STEP 16"
C  // STEP 16: CALCULATE U_k e_j = r_k - sum_{q=1}^s U_k e_q beta(q)
C              FOR K=0, ..., i
C
                  DO 900 K=0, IIL
                      DO 910 IP=1, NP
                          WRK01(IP) = RESV(IP,K)
 910                  CONTINUE
                      DO 920 JS=1, NS
                          DO 921 IP=1, NP
                              WRK01(IP) = WRK01(IP)
     *                                    - UMAT(IP,JS,K) * BETA(JS)
 921                      CONTINUE
 920                  CONTINUE
                      DO 930 IP=1, NP
                          UMAT(IP,JJS,K) = WRK01(IP)
 930                  CONTINUE
 900              CONTINUE
C
C      WRITE(IUT6,*) "STEP 17"
C  // STEP 17: ELSE (FOR IF IN STEP 14)
C
              ELSE
C
C      WRITE(IUT6,*) "STEP 18a"
C  // STEP 18a: MAKE MATRIX M' = [m, Me_1, ..., Me_{j-2}, Me_{j}, ..., Me_s]
C                   
                  DO 1000 IS = 1, NS
                      WKSMAT(1,IS) = MVEC(IS)
 1000             CONTINUE
                  DO 1010 JS = 2, JJS-1
                      DO 1011 IS=1, NS
                          WKSMAT(JS,IS) = MMAT(IS,JS-1)
 1011                 CONTINUE
 1010             CONTINUE
                  DO 1020 JS = JJS, NS
                      DO 1021 IS=1, NS
                          WKSMAT(JS,IS) = MMAT(IS,JS)
 1021                 CONTINUE
 1020             CONTINUE
C
C      WRITE(IUT6,*) "STEP 18b"
C  // STEP 18b: RHS
C
                  DO 1030 IS=1, NS
                      BETA(IS) = MMAT(IS, JJS-1)
 1030             CONTINUE
C
                  CALL  MATGAU(NS,NS,WKSMAT,BETA,
     *                         WRK01,IWORKS,IUT0,IERR)
                  IF(IERR.NE.0) RETURN
C
C      WRITE(IUT6,*) "STEP 19"
C  // STEP 19: CALCULATE
C                  U_k e_j = U_{k+1}e_{j-1} - r_k beta(1)
C                               - sum_{q=1}^{j-2} U_{k+1} e_q beta(q+1)
C                               - sum_{q=j}^s U_k e_q beta(q)
C              FOR K=0, ..., i
C
                  DO 1100 K = 0, IIL
                      DO 1110 IP = 1, NP
                          WRK01(IP) = UMAT(IP,JJS-1,K+1)
     *                                  - RESV(IP,K) * BETA(1)
 1110                 CONTINUE
                      DO 1120 JS = 1, JJS-2
                          DO 1121 IP= 1, NP
                              WRK01(IP) = WRK01(IP)
     *                                  - UMAT(IP,JS,K+1) * BETA(JS+1)
 1121                     CONTINUE
 1120                 CONTINUE
                      DO 1130 JS = JJS, NS
                          DO 1131 IP = 1, NP
                              WRK01(IP) = WRK01(IP)
     *                                  - UMAT(IP,JS,K) * BETA(JS)
 1131                     CONTINUE
 1130                 CONTINUE
                      DO 1140 IP = 1, NP
                          UMAT(IP,JJS,K) = WRK01(IP)
 1140                 CONTINUE
 1100             CONTINUE
C
C      WRITE(IUT6,*) "STEP 20"
C  // STEP 20: ENDIF (FOR IF IN STEP 14)
C
              ENDIF
C
C      WRITE(IUT6,*) "STEP 21"
C  // STEP 21: CALCULATE U_{i+1} e_j = A U_i e_j
C
      CALL CALAX0(ACRS,UMAT(1,JJS,IIL),UMAT(1,JJS,IIL+1),
     *            NP,NE,NCRS,IPCRS,NPP,
     *            N2,ME,IPART,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *            RX,RY,JUNROL,NPPMAX,NCRS2,TS,TACRS,ITPCRS,
     *            IUT0,IERR)
C
C
C      WRITE(IUT6,*) "STEP 22"
C  // STEP 22: CALCULATE M e_j = TR0T^* U_{i+1} e_j
              CALL MATMLW (NP,NS,TR0T,UMAT(1,JJS,IIL+1),WRK01,WEIGHT)
              CALL DDCOM2V(WRK01,MMAT(1,JJS),NS)
C
C      WRITE(IUT6,*) "STEP 23"
C  // STEP 23: END FOR IN STEP 13
C
 8200     CONTINUE
C     
C      WRITE(IUT6,*) "STEP 24"
C  // STEP 24: SOLVE EQUATION M beta = m for beta
C
          DO 1400 IS=1, NS
              BETA(IS) = MVEC(IS)
              DO 1401 JS=1, NS
                  WKSMAT(JS,IS) = MMAT(IS,JS)
 1401         CONTINUE
 1400     CONTINUE
          CALL  MATGAU(NS,NS,WKSMAT,BETA,WRK01,IWORKS,IUT0,IERR)
          IF(IERR.NE.0) RETURN
C
C      WRITE(IUT6,*) "STEP 25"
C  // STEP 25: CALCULATE r_k = r_k - U_{k+1} beta (k = 0, ..., i)
C           
          DO 1500 K = 0, IIL               
              DO 1501 IS=1, NS
                  DO 1502 IP=1, NP
                      WRKS01(IP,IS) = UMAT(IP,IS,K+1)
 1502             CONTINUE
 1501         CONTINUE
              CALL MATMLS(NP,NS,WRKS01,BETA,WRK01)
              DO 1510 IP=1, NP
                  RESV(IP,K) = RESV(IP,K) - WRK01(IP)
 1510         CONTINUE
 1500     CONTINUE
C     
C      WRITE(IUT6,*) "STEP 26"
C  // STEP 26: CALCULATE x_0 = x_0 + U_0 beta
C
          DO 1600 IS=1, NS
              DO 1601 IP=1, NP
                  WRKS01(IP,IS) = UMAT(IP,IS,0)
 1601         CONTINUE
 1600     CONTINUE
          CALL MATMLS(NP,NS,WRKS01,BETA,WRK01)
          DO 1602 IP=1, NP
              S(IP) = S(IP) + WRK01(IP)
 1602     CONTINUE
C     
C      WRITE(IUT6,*) "STEP 27"
C  // STEP 27: CALCULATE r_{i+1} = A r_i
C     
          CALL CALAX0(ACRS,RESV(1,IIL),RESV(1,IIL+1),
     *                NP,NE,NCRS,IPCRS,NPP,
     *                N2,ME,IPART,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *                RX,RY,JUNROL,NPPMAX,NCRS2,TS,TACRS,ITPCRS,
     *                IUT0,IERR)
C
C      WRITE(IUT6,*) "STEP 28"
C  // STEP 28: END FOR IN STEP 10
C
 8100 CONTINUE
C
C      WRITE(IUT6,*) "STEP 29"
C  // STEP 29: FOR JL = 1, ..., L      
C
      DO 2000 JL=1, NL
C
C      WRITE(IUT6,*) "STEP 30"
C  // STEP 30: CALCULATE TAU: TAU_IJ = SIG_IJ / SIG_II,
C                                SIG_IJ = (r_i, r_j)
C                                r_j = r_j - tau_ij r_i                     
C
          DO 2010 IP=1, NP
              WRK01(IP)=RESV(IP,JL)
 2010     CONTINUE
          DO 2020 IL=1, JL-1
              CALL DOTWVC(NP,RESV(1,IL),RESV(1,JL),WEIGHT,SIGIJ)
              CALL DOTWVC(NP,RESV(1,IL),RESV(1,IL),WEIGHT,SIGII)
              TAU(IL,JL) = SIGIJ / SIGII
              DO 2040 IP=1, NP
                  WRK01(IP) = WRK01(IP) - TAU(IL,JL) * RESV(IP,IL)
 2040         CONTINUE
 2020     CONTINUE
          DO 2050 IP=1, NP
              RESV(IP,JL) = WRK01(IP)
 2050     CONTINUE
C
C      WRITE(IUT6,*) "STEP 31"
C  // STEP 31: CALCULATE GAMMA: GAMMA_j = SIG_J0 / SIG_JJ
C
          CALL DOTWVC(NP,RESV(1,JL),RESV(1,JL),WEIGHT,SIGJJ)
          CALL DOTWVC(NP,RESV(1,JL),RESV(1, 0),WEIGHT,SIGJ0)
          GAM1(JL) = SIGJ0 / SIGJJ
C
C      WRITE(IUT6,*) "STEP 32"
C  // STEP 32: END FOR IN STEP 29
C
 2000 CONTINUE
C
C      WRITE(IUT6,*) "STEP 33"
C  // STEP 33: 
C
      GAM0(NL)= GAM1(NL)
      OMEGA   = GAM0(NL)
C
C
C      WRITE(IUT6,*) "STEP 34"
C  // STEP 34:
C
      DO 2100 J = 1, NL-1
          JL = NL - J
          BUF = GAM1(JL)
          DO 2110 IL = JL+1, NL
              BUF = BUF - TAU(JL,IL) * GAM0(IL)
 2110     CONTINUE
          GAM0(JL) = BUF
 2100 CONTINUE
C
C      WRITE(IUT6,*) "STEP 35"
C  // STEP 35:
C
      DO 2200 JL=1, NL-1
          BUF = GAM0(JL+1)
          DO 2210 IL = JL+1, NL-1
              BUF = BUF + TAU(JL,IL) * GAM0(IL+1)
 2210     CONTINUE
          GAM2(JL) = BUF
 2200 CONTINUE
C
C      WRITE(IUT6,*) "STEP 36"
C  // STEP 36:
C
      DO 2300 IP=1, NP
          S(IP) = S(IP) + GAM0(1) * RESV(IP,0)
          RESV(IP,0) = RESV(IP,0) - GAM1(NL) * RESV(IP,NL)
          DO 2310 IS=1, NS
              UMAT(IP,IS,0) = UMAT(IP,IS,0) - GAM0(NL) * UMAT(IP,IS,NL)
 2310     CONTINUE
 2300 CONTINUE
C
C      WRITE(IUT6,*) "STEP 37"
C  // STEP 37:      
C
      DO 2400 JL=1, NL-1
          DO 2410 IS=1, NS
              DO 2411 IP=1, NP
                  UMAT(IP,IS,0) = UMAT(IP,IS,0)
     *                            - GAM0(JL) * UMAT(IP,IS,JL)
 2411         CONTINUE
 2410     CONTINUE
 2400 CONTINUE
C
C      WRITE(IUT6,*) "STEP 38"
C  // STEP 38:
C
      DO 2500 JL=1, NL-1
          DO 2510 IP=1, NP
              S(IP) = S(IP) + GAM2(JL) * RESV(IP,JL)
              RESV(IP,0) = RESV(IP,0) - GAM1(JL) * RESV(IP,JL)
 2510     CONTINUE
 2500 CONTINUE
C
C      WRITE(IUT6,*) "STEP 39"
C  // STEP 39: 
C
      NITR = NITR + (NS+1)*NL
C     
C      WRITE(IUT6,*) "STEP 40"
C  // STEP 40: END REPEAT
C     
      GOTO 8000
C
 9000 CONTINUE
C
      RETURN 
      END
