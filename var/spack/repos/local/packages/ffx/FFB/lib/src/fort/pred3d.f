C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    PRED3D                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE PRED3D(IMODEL,IFORM,NFRAME,UFRAME,VFRAME,WFRAME,
     *                  JSET,IEATTR,IPATTR,OMEGA,TIMER,
     *                  DT,VISC,U,V,W,P,NODE,LOCAL,X,Y,SX,SY,SZ,CM,
     *                  SN,DNX,DNY,DNZ,EX,EY,EZ,EXX,EYY,EZZ,EXY,EXZ,EYZ,
     *                  IENP,JENP,NEP,ME,MEP,NE,NP,N,GI,EI,TI,
     *                  ITIME,DEVLP1,DEVLP2,ACCELX,ACCELY,ACCELZ,
     *                  NPINLT,LPINLT,UINLT,VINLT,WINLT,
     *                  NPWALL,LPWALL,UWALL,VWALL,WWALL,
     *                  NPCON,LPCON,XPWALL,YPWALL,ZPWALL,
     *                  NEWALL,LEWALL,SNWALL,TAUX  ,TAUY  ,TAUZ,
     *                  NPSYMT,LPSYMT,XPSYMT,YPSYMT,ZPSYMT,
     *                  NPDEP ,LPDEP1,LPDEP2,
     *                  NPCCL ,LPCCL1,LPCCL2,NECCL ,LECCL ,
     *                  XNCCL ,YNCCL ,ZNCCL ,AECCL ,
     *                  QCCLF ,QCCL  ,QCCLP ,PCCLF ,PCCL  ,PCCLP,
     *                  IPART ,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *                  LPSET1,LPSET2,LPSET3,GPSET,EPSET,TPSET,NPSET,
     *                  NPSND ,LPSND ,NPTSND,IPSET,IPSRC,
     *                  NPRCV ,LPRCV ,NPTRCV,RX,RY,RZ,FX,FY,FZ,UG,VG,WG,
     *                  IUT0,IERR)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION IEATTR(NE),IPATTR(NP),
     1          UFRAME(2,NFRAME),VFRAME(2,NFRAME),WFRAME(2,NFRAME),
     2          VISC(NE),U(NP),V(NP),W(NP),P(NE),X(NP),Y(NP),
     3          NODE(N,NE),LOCAL(4,6),SX(NE),SY(NE),SZ(NE),CM(NP),
     4          SN   (N,NE),DNX  (N,NE),DNY  (N,NE),DNZ  (N,NE),
     5          EX (ME,N,N),EY (ME,N,N),EZ (ME,N,N),
     6          EXX(ME,N,N),EYY(ME,N,N),EZZ(ME,N,N),
     7          EXY(ME,N,N),EXZ(ME,N,N),EYZ(ME,N,N),
     8          IENP(MEP,NP),JENP(MEP,NP),NEP(NP),GI(N),EI(N),TI(N)
C
      DIMENSION LPINLT(NPINLT),
     1          UINLT (NPINLT),VINLT (NPINLT),WINLT (NPINLT),
     2          LPWALL(NPWALL),LPCON (NPCON),
     3           UWALL(NPWALL), VWALL(NPWALL), WWALL(NPWALL),
     4          XPWALL(NPWALL),YPWALL(NPWALL),ZPWALL(NPWALL),
     5          LEWALL(2,NEWALL),SNWALL(N,NEWALL),
     6          TAUX  (NEWALL),TAUY(NEWALL),TAUZ(NEWALL),
     7          LPSYMT(NPSYMT),
     8          XPSYMT(NPSYMT),YPSYMT(NPSYMT),ZPSYMT(NPSYMT),
     9          LPDEP1(NPDEP ),LPDEP2(NPDEP),
     A          LPCCL1(NPCCL ),LPCCL2(NPCCL),LECCL (2,NECCL),
     B          XNCCL (NECCL ),YNCCL (NECCL),ZNCCL (NECCL),AECCL(NECCL)
C
      DIMENSION LDOM(NDOM),NBPDOM(NDOM),
     1          IPSLF(MBPDOM,NDOM),IPSND(MBPDOM,NDOM)
C
      DIMENSION LPSET1(NPSET),LPSET2(NPSET),LPSET3(NPSET),
     1          GPSET (NPSET),EPSET (NPSET),TPSET (NPSET),
     2          LPSND (NPSND),NPTSND(NPSND),LPRCV (NPRCV),NPTRCV(NPRCV),
     3          IPSET (MBPDOM,NPSND),IPSRC (MBPDOM,NPSND)
C
      DIMENSION RX(N,NE),RY(N,NE),RZ(N,NE),
     1          FX(NP),FY(NP),FZ(NP),UG(NE),VG(NE),WG(NE)
C
      CHARACTER*60 ERMSGC
     & /' ## SUBROUTINE PRED3D: FATAL      ERROR REPORT   ; RETURNED' /
C
      DATA IDIM    / 3       /
      DATA ALF     / 1.0E-2  /
      DATA DQPMIN  / 1.0E-1  /
      DATA DQPMAX  / 1.0E+3  /
      DATA EPS     / 1.0E-30 /
C
C      CALCULATE VELOCITY PREDICTOR
C         ( 3-D CALCULATION : SINGLE WORD VERSION )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          IMODEL      ; SPECIFIES WALL BOUNDARY CONDITIONS AS FOLLOWS
C             0,1,3,5 --- NO-SLIP CONDITIONS
C                 2,4 --- STRESS  CONDITIONS
C
C          IFORM       ; SPECIFIES CONVECTION TERM DISCRETIZATIONS
C                   0 --- NORMAL GALERKIN TYPE
C                   3 --- STREAMLINE UPWIND PETROV-GALERKIN TYPE
C                   4 --- TIME-ACCURATE STREAMLINE UPWIND TYPE
C
C          NFRAME      ; NUMBER OF TRANSLATING FRAMES OF REFERENCE 
C
C          JSET        ; OVERSET WILL BE DONE WHEN SET GREATER THAN ZERO
C
C          IEATTR  (IE); ELEMENT FRAME ATTRIBUES (IE. FRAME NUMBER)
C          IPATTR  (IP); NODE    FRAME ATTRIBUES (IE. FRAME NUMBER)
C
C          OMEGA       ; ANGULAR VELOCITY OF REFERENCE FRAME -1
C
C          UFRAME(1,IF); X-DIR. VELOCITY    OF TRANSLATING FRAME 'IF'
C          VFRAME(1,IF); Y-DIR. VELOCITY    OF TRANSLATING FRAME 'IF'
C          WFRAME(1,IF); Z-DIR. VELOCITY    OF TRANSLATING FRAME 'IF'
C          UFRAME(2,IF); X-DIR. ACCELERAION OF TRANSLATING FRAME 'IF'
C          VFRAME(2,IF); Y-DIR. ACCELERAION OF TRANSLATING FRAME 'IF'
C          WFRAME(2,IF); Z-DIR. ACCELERAION OF TRANSLATING FRAME 'IF'
C
C          TIMER       ; PRESENT TIME OF OVERSET CONDITIONS DATA
C           NOTES ; 'TIMER' WILL BE REFERED TO FOR INTER-FLAME OVERSET.
C
C          DT          ; TIME INCTREMENT
C          VISC    (IE); ELEMENT VISCOSITY
C          P       (IE); ELEMENT PRESSURE
C          NODE  (I,IE); NODE NO. TABLE BASED ON ELEMENT
C          LOCAL (I,IS); NODE NUMBER TABLE DEFINING ELEMENT SURFACES
C          X       (IP); X-COORDINATES OF NODES
C          Y       (IP); Y-COORDINATES OF NODES
C          Z       (IP); Z-COORDINATES OF NODES
C          SX      (IE); UPWIND VECTOR IN X-DIR.
C          SY      (IE); UPWIND VECTOR IN Y-DIR.
C          SZ      (IE); UPWIND VECTOR IN Z-DIR.
C
C          CM      (IP); LUMPED MASS MATRIX
C          DNX   (I,IE); INTEGRATED ELEMENT VECTOR OF NX
C          DNY   (I,IE); INTEGRATED ELEMENT VECTOR OF NY
C          DNZ   (I,IE); INTEGRATED ELEMENT VECTOR OF NZ
C          EX  (IE,I,J); INTEGRATED ELEMENT MATRIX OF N*NXT
C          EY  (IE,I,J); INTEGRATED ELEMENT MATRIX OF N*NYT
C          EZ  (IE,I,J); INTEGRATED ELEMENT MATRIX OF N*NZT
C          EXX (IE,I,J); INTEGRATED ELEMENT MATRIX OF NX*NXT
C          EYY (IE,I,J); INTEGRATED ELEMENT MATRIX OF NY*NYT
C          EZZ (IE,I,J); INTEGRATED ELEMENT MATRIX OF NZ*NZT
C          EXY (IE,I,J); INTEGRATED ELEMENT MATRIX OF NX*NYT
C          EXZ (IE,I,J); INTEGRATED ELEMENT MATRIX OF NX*NZT
C          EYZ (IE,I,J); INTEGRATED ELEMENT MATRIX OF NY*NZT
C
C          IENP(IEP,IP); ADJACENT ELEMENT NUMBER TO NODE IP
C                      ( IF NEP(IP).LT.MEP , THEN IENP(NEP(IP)+1,IP),
C                       IENP(MEP,IP) MUST BE SET TO AN IMAGINARY
C                       ELEMENT NO. BETWEEN NE+1,ME.)
C          JENP(IEP,IP); LOCAL NODE NUMBER OF IP IN ELEMENT IENP(IEP,IP)
C          NEP     (IP); NUMBER OF ADJACENT ELEMENTS TO NODE IP
C          ME          ; MAX. NUMBER OF TOTAL ELEMENTS
C          MEP         ; FIRST DIMENSION SIZE OF ARRAY IENP,JENP
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          NP          ; NUMBER OF TOTAL    NODES
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT ( = 8 )
C          GI       (I); LOCAL GZAI  COORDINATES OF ELEMENT'S NODES
C          EI       (I); LOCAL EATA  COORDINATES OF ELEMENT'S NODES
C          TI       (I); LOCAL THETA COORDINATES OF ELEMENT'S NODES
C
C          ITIME       ; CUREENT TIME STEP
C          DEVLP1      ; DEVELOPMENT FUNCTION FOR INLET VELOCITIES
C          DEVLP2      ; DEVELOPMENT FUNCTION FOR ALL THE OTHER VALUES
C          ACCELX      ; X-DIR. ACCELERATION TERMS ADDED TO ALL FRAMES
C          ACCELY      ; Y-DIR. ACCELERATION TERMS ADDED TO ALL FRAMES
C          ACCELZ      ; Z-DIR. ACCELERATION TERMS ADDED TO ALL FRAMES
C
C        A. INLET BOUNDARY
C          NPINLT      ; NUMBER OF INLET BOUNDARY NODES
C          LPINLT (IBP); INLET BOUNDARY NODES
C          UINLT  (IBP); INLET BOUNDARY U-VELOCITIES
C          VINLT  (IBP); INLET BOUNDARY V-VELOCITIES
C          WINLT  (IBP); INLET BOUNDARY W-VELOCITIES
C
C        B. WALL BOUNDARY
C          NPWALL      ; NUMBER OF WALL BOUNDARY NODES
C          LPWALL (IBP); WALL BOUNDARY NODES
C          UWALL  (IBP); WALL BOUNDARY U-VELOCITIES
C          VWALL  (IBP); WALL BOUNDARY V-VELOCITIES
C          WWALL  (IBP); WALL BOUNDARY W-VELOCITIES
C          XPWALL (IBP); X NORMAL OF WALL BOUNDARY NODES
C          YPWALL (IBP); Y NORMAL OF WALL BOUNDARY NODES
C          ZPWALL (IBP); Z NORMAL OF WALL BOUNDARY NODES
C          NPCON       ; NUMBER OF CORNER WALL NODES
C          LPCON  (IPC); CORNER WALL NODES
C          NEWALL      ; NUMBER OF WALL BOUNDARY SURFACES
C          LEWALL(I,IBE); WALL BOUNDARY ELEMENT AND ITS SURFACE
C          SNWALL(I,IBE);INTEGRATED ELEMENT VECTOR OF N  AT WALL SURFACE
C          TAUX   (IBE); X-DIR. SHEAR STRESS COMPONENT AT WALL SURFACES
C          TAUY   (IBE); Y-DIR. SHEAR STRESS COMPONENT AT WALL SURFACES
C          TAUZ   (IBE); Z-DIR. SHEAR STRESS COMPONENT AT WALL SURFACES
C
C        C. SYMMETRIC BOUNDARY
C          NPSYMT      ; NUMBER OF SYMMETRIC BOUNDARY NODES
C          LPSYMT (IBP); SYMMETRIC BOUNDARY NODES
C          XPSYMT (IBP); X-DIR COMPONENT OF SYMMETRIC NODE NORMAL VECTOR
C          YPSYMT (IBP); Y-DIR COMPONENT OF SYMMETRIC NODE NORMAL VECTOR
C          ZPSYMT (IBP); Z-DIR COMPONENT OF SYMMETRIC NODE NORMAL VECTOR
C
C        D. DEPENDING BOUNDARY
C          NPDEP       ; NUMBER OF DEPENDING BOUNDARY NODES
C          LPDEP1 (IBP); DEPENDING BOUNDARY NODES-1
C          LPDEP2 (IBP); DEPENDING BOUNDARY NODES-2
C
C        E. CYCLIC BOUNDARY
C          NPCCL       ; NUMBER OF CYCLIC BOUNDARY NODES
C          LPCCL1 (IBP); CYCLIC BOUNDARY NODES-1
C          LPCCL2 (IBP); CYCLIC BOUNDARY NODES-2
C          NECCL       ; NUMBER OF CYCLIC BOUNDARY ELEMENTS
C          LECCL(I,IBE); CYCLIC BOUNDARY ELEMENT AND ITS SURFACE
C          XNCCL  (IBE); X NORMAL OF CYCLIC BOUNDARY SURFACE
C          YNCCL  (IBE); Y NORMAL OF CYCLIC BOUNDARY SURFACE
C          ZNCCL  (IBE); Z NORMAL OF CYCLIC BOUNDARY SURFACE
C          AECCL  (IBE); AREA     OF CYCLIC BOUNDARY SURFACE
C          QCCLF       ; GIVEN    FLOW RATE OF CYCLIC BOUNDARY
C          QCCL        ; CURRENT  FLOW RATE OF CYCLIC BOUNDARY
C          QCCLP       ; PREVIOUS FLOW RATE OF CYCLIC BOUNDARY
C          PCCLF       ; GIVEN    PRESSURE DIFFERENCE OF CYCLIC BOUNDARY
C          PCCL        ; CURRENT  PRESSURE DIFFERENCE OF CYCLIC BOUNDARY
C          PCCLP       ; PREVIOUS PRESSURE DIFFERENCE OF CYCLIC BOUNDARY
C           NOTES; THE PRESSURE DIFFERENCE FOR CYCLIC BOUNDARY WILL BE
C                 CONTROLLED (THUS, NEED NOT BE SPECIFIED) SO AS TO
C                 MAINTAIN THE GIVEN FLOW RATE, EXCEPT WHEN THE FLOW
C                 RATE IS SET TO ZERO. IN THIS CASE, GIVEN CONSTANT
C                 PRESSURE DIFFERENCE WILL BE APPLIED FOR THE CYCLIC
C                 BOUNDARY.
C
C        F. INTER-CONNECT BOUNDARY
C          IPART       ; SUB-DOMAIN NUMBER THAT THIS TASK SHOULD TAKE/IS
C                       TAKING CARE OF. IPART BEING SET ZERO MEANS THAT
C                       THE PROGRAM SHOULD RUN/IS RUNNING IN SERIAL 
C                       MODE.
C
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
C        G. OVERSET BOUNDARY NODES
C          NPSET       ; NUMBER OF OVERSET BOUNDARY NODES
C          LPSET1 (IBP); OVERSET BOUNDARY NODES
C          LPSET2 (IBP); ELEMENT NUMBER TO CALCULATE OVERSET VALUES
C          LPSET3 (IBP); DOMAIN NUMBER TO SEND/RECEIVE OVERSET VALUES
C                   0 --- CALCULATE AND SET OVERSET VALUE WITHIN THE
C                         SELF-DOMAIN
C          (POS. INT.)--- SEND    OVERSET VALUE TO   DOMAIN  LPSET3(IB)
C                         AFTER CALCULATING IT WITHIN THE SELF-DOMAIN
C          (NEG. INT.)--- RECEIVE OVERSET VALUE FROM DOMAIN -LPSET3(IB)
C
C          GPSET  (IBP); LOCAL GZAI-COORDINATE IN INTERPOLATING ELEMENT
C          EPSET  (IBP); LOCAL EATA-COORDINATE IN INTERPOLATING ELEMENT
C          TPSET  (IBP); LOCAL ZETA-COORDINATE IN INTERPOLATING ELEMENT
C
C          NPSND       ; NUMBER OF DOMAINS TO SEND OVERSET NODE VALUE
C          LPSND (IDOM); DOMAIN NUMBER     TO SEND OVERSET NODE VALUE
C          NPTSND(IDOM); NUMBER OF OVERSET NODE POINTS TO SEND TO
C                        DOMAIN 'LPSND(IDOM)'
C          IPSET(IPT,IDOM); OVERSET NODE NUMBER IN THE DOMAIN RECEIVING
C                           THE OVERSET VALUES.
C          IPSRC(IPT,IDOM); INDICATES POSITION IN THE OVERSET-VALUES
C                           PASSING ARRAYS WHEN OVERSET NODE DATA
C                           ARE COMPILED SEQUENTIALLY
C
C          NPRCV       ; NUMBER OF DOMAINS TO RECEIVE OVERSET NODE VALUE
C          LPRCV (IDOM); DOMAIN NUMBER     TO RECEIVE OVERSET NODE VALUE
C          NPTRCV(IDOM); NUMBER OF OVERSET POINTS TO RECEIVE FROM
C                       DOMAIN 'LPRCV(IDOM)'
C
C          IUT0        ; FILE NUMBER TO REPORT ERROR OCCURENCE
C
C       (2) OUTPUT
C          IERR        ; RETURN CODE TO REPORT ERROR OCCURENCE
C                   0 --- NORMAL TERMINATION
C                   1 --- A FATAL ERROR HAS OCCURED
C
C       (3) INPUT-OUTPUT
C          U       (IP); X-DIR. VELOCITY COMPONENT
C          V       (IP); Y-DIR. VELOCITY COMPONENT
C          W       (IP); Z-DIR. VELOCITY COMPONENT
C
C       (4) WORK
C          RX    (I,IE); HOLDS X-DIR. ELEMENT MOMENTUM RESIDUAL
C          RY    (I,IE); HOLDS Y-DIR. ELEMENT MOMENTUM RESIDUAL
C          RZ    (I,IE); HOLDS Z-DIR. ELEMENT MOMENTUM RESIDUAL
C          FX      (IP); HOLDS X-DIR. NODE    MOMENTUM RESIDUAL
C          FY      (IP); HOLDS Y-DIR. NODE    MOMENTUM RESIDUAL
C          FZ      (IP); HOLDS Z-DIR. NODE    MOMENTUM RESIDUAL
C          UG      (IE); HOLDS ELEMENT CENTER U-VELOCITY
C          VG      (IE); HOLDS ELEMENT CENTER V-VELOCITY
C          WG      (IE); HOLDS ELEMENT CENTER W-VELOCITY
C
C
      MAXBUF = NE*N
C
      IF(ITIME.EQ.0) GO TO 1310
C
C
C SET INLET BOUNDARY CONDITIONS
C
C
*POPTION INDEP(U,V,W)
C*$*ASSERT PERMUTATION ( LPINLT )
      DO 100 IPINLT = 1 , NPINLT
          U(LPINLT(IPINLT)) = DEVLP1*UINLT(IPINLT)
          V(LPINLT(IPINLT)) = DEVLP1*VINLT(IPINLT)
          W(LPINLT(IPINLT)) = DEVLP1*WINLT(IPINLT)
  100 CONTINUE
C
C
C SET PRESSURE DIFFERENCE FOR CYCLIC BOUNDARY CONDITIONS
C
C
      IF(ABS(QCCLF).LT.EPS) THEN
          PCCL  = DEVLP2*PCCLF
      ELSE
          QF    = DEVLP2*QCCLF
          IF(ITIME.EQ.1) PCCLP = 0.E0
          DQP   = (QCCL-QCCLP)/(PCCL-PCCLP+SIGN(EPS,PCCL-PCCLP))
          IF(DQP.GT.DQPMAX) DQP   = DQPMAX
          IF(DQP.LT.DQPMIN) DQP   = DQPMIN
C
          PCCLP = PCCL
          IF(ITIME.EQ.1) PCCLP = 0.E0
          PCCL  = PCCL+ALF*(QF-QCCL)/DQP
      ENDIF
C
C
C CLEAR ARRAYS
C
C
      DO 210 I = 1 , N
          DO 200 IE = 1 , NE
              RX(I,IE) = 0.E0
              RY(I,IE) = 0.E0
              RZ(I,IE) = 0.E0
  200     CONTINUE
  210 CONTINUE
C
      IF(IFORM.EQ.0) THEN
          DO 300 IE = 1 , NE
              SX(IE) = 0.E0
              SY(IE) = 0.E0
              SZ(IE) = 0.E0
  300     CONTINUE
      ENDIF
C
C
C COMPUTE ELEMENT CENTER VELOCITY
C
C
      DO 400 IE = 1 , NE
          UG(IE) = 0.125*(U(NODE(1,IE))+U(NODE(5,IE))
     &                   +U(NODE(2,IE))+U(NODE(6,IE))
     &                   +U(NODE(3,IE))+U(NODE(7,IE))
     &                   +U(NODE(4,IE))+U(NODE(8,IE)))
C
          VG(IE) = 0.125*(V(NODE(1,IE))+V(NODE(5,IE))
     &                   +V(NODE(2,IE))+V(NODE(6,IE))
     &                   +V(NODE(3,IE))+V(NODE(7,IE))
     &                   +V(NODE(4,IE))+V(NODE(8,IE)))
C
          WG(IE) = 0.125*(W(NODE(1,IE))+W(NODE(5,IE))
     &                   +W(NODE(2,IE))+W(NODE(6,IE))
     &                   +W(NODE(3,IE))+W(NODE(7,IE))
     &                   +W(NODE(4,IE))+W(NODE(8,IE)))
  400 CONTINUE
C
C
C COMPUTE ADVECTION & DIFFUSION TERMS
C
C
      DO 620 J = 1 , N
          DO 500 IE = 1 , NE
              FX(IE) = U(NODE(J,IE))
              FY(IE) = V(NODE(J,IE))
              FZ(IE) = W(NODE(J,IE))
  500     CONTINUE
C
          DO 610 I = 1 , N
              DO 600 IE = 1 , NE
                  AI =    (EX(IE,I,J)+SX(IE)*EXX(IE,I,J)
     &            +SY(IE)*EXY(IE,J,I)+SZ(IE)*EXZ(IE,J,I))*UG(IE)
     &                   +(EY(IE,I,J)+SX(IE)*EXY(IE,I,J)
     &            +SY(IE)*EYY(IE,I,J)+SZ(IE)*EYZ(IE,J,I))*VG(IE)
     &                   +(EZ(IE,I,J)+SX(IE)*EXZ(IE,I,J)
     &            +SY(IE)*EYZ(IE,I,J)+SZ(IE)*EZZ(IE,I,J))*WG(IE)
C
                  DI=VISC(IE)*(EXX(IE,I,J)+EYY(IE,I,J)+EZZ(IE,I,J))
C
                  RX(I,IE)=RX(I,IE)-(AI+DI)*FX(IE)
                  RY(I,IE)=RY(I,IE)-(AI+DI)*FY(IE)
                  RZ(I,IE)=RZ(I,IE)-(AI+DI)*FZ(IE)
C
                  RX(I,IE)=RX(I,IE)
     &            -VISC(IE)*(EXX(IE,I,J)*FX(IE)+EXY(IE,J,I)*FY(IE)
     &                      +EXZ(IE,J,I)*FZ(IE))
                  RY(I,IE)=RY(I,IE)
     &            -VISC(IE)*(EXY(IE,I,J)*FX(IE)+EYY(IE,I,J)*FY(IE)
     &                      +EYZ(IE,J,I)*FZ(IE))
                  RZ(I,IE)=RZ(I,IE)
     &            -VISC(IE)*(EXZ(IE,I,J)*FX(IE)+EYZ(IE,I,J)*FY(IE)
     &                      +EZZ(IE,I,J)*FZ(IE))
  600         CONTINUE
  610     CONTINUE
  620 CONTINUE
C
C
C      ADD PRESSURE & ACCELERATION TERMS (TO ALL FRAMES)
C
C
      DO 900 IE = 1 , NE
          RX(1,IE) = RX(1,IE)+P(IE)*DNX(1,IE)+ACCELX*SN(1,IE)
          RX(2,IE) = RX(2,IE)+P(IE)*DNX(2,IE)+ACCELX*SN(2,IE)
          RX(3,IE) = RX(3,IE)+P(IE)*DNX(3,IE)+ACCELX*SN(3,IE)
          RX(4,IE) = RX(4,IE)+P(IE)*DNX(4,IE)+ACCELX*SN(4,IE)
          RX(5,IE) = RX(5,IE)+P(IE)*DNX(5,IE)+ACCELX*SN(5,IE)
          RX(6,IE) = RX(6,IE)+P(IE)*DNX(6,IE)+ACCELX*SN(6,IE)
          RX(7,IE) = RX(7,IE)+P(IE)*DNX(7,IE)+ACCELX*SN(7,IE)
          RX(8,IE) = RX(8,IE)+P(IE)*DNX(8,IE)+ACCELX*SN(8,IE)
C
          RY(1,IE) = RY(1,IE)+P(IE)*DNY(1,IE)+ACCELY*SN(1,IE)
          RY(2,IE) = RY(2,IE)+P(IE)*DNY(2,IE)+ACCELY*SN(2,IE)
          RY(3,IE) = RY(3,IE)+P(IE)*DNY(3,IE)+ACCELY*SN(3,IE)
          RY(4,IE) = RY(4,IE)+P(IE)*DNY(4,IE)+ACCELY*SN(4,IE)
          RY(5,IE) = RY(5,IE)+P(IE)*DNY(5,IE)+ACCELY*SN(5,IE)
          RY(6,IE) = RY(6,IE)+P(IE)*DNY(6,IE)+ACCELY*SN(6,IE)
          RY(7,IE) = RY(7,IE)+P(IE)*DNY(7,IE)+ACCELY*SN(7,IE)
          RY(8,IE) = RY(8,IE)+P(IE)*DNY(8,IE)+ACCELY*SN(8,IE)
C
          RZ(1,IE) = RZ(1,IE)+P(IE)*DNZ(1,IE)+ACCELZ*SN(1,IE)
          RZ(2,IE) = RZ(2,IE)+P(IE)*DNZ(2,IE)+ACCELZ*SN(2,IE)
          RZ(3,IE) = RZ(3,IE)+P(IE)*DNZ(3,IE)+ACCELZ*SN(3,IE)
          RZ(4,IE) = RZ(4,IE)+P(IE)*DNZ(4,IE)+ACCELZ*SN(4,IE)
          RZ(5,IE) = RZ(5,IE)+P(IE)*DNZ(5,IE)+ACCELZ*SN(5,IE)
          RZ(6,IE) = RZ(6,IE)+P(IE)*DNZ(6,IE)+ACCELZ*SN(6,IE)
          RZ(7,IE) = RZ(7,IE)+P(IE)*DNZ(7,IE)+ACCELZ*SN(7,IE)
          RZ(8,IE) = RZ(8,IE)+P(IE)*DNZ(8,IE)+ACCELZ*SN(8,IE)
  900 CONTINUE
C
C
C      ADD ACCELERATION TERMS (TO ROTATING FRAME)
C
C
      DO 1000 IE = 1 , NE
          IFRAME = IEATTR(IE)
          IF(IFRAME.NE.-1) GO TO 1000
C
          XG = 0.125*(X(NODE(1,IE))+X(NODE(5,IE))
     &               +X(NODE(2,IE))+X(NODE(6,IE))
     &               +X(NODE(3,IE))+X(NODE(7,IE))
     &               +X(NODE(4,IE))+X(NODE(8,IE)))
C
          YG = 0.125*(Y(NODE(1,IE))+Y(NODE(5,IE))
     &               +Y(NODE(2,IE))+Y(NODE(6,IE))
     &               +Y(NODE(3,IE))+Y(NODE(7,IE))
     &               +Y(NODE(4,IE))+Y(NODE(8,IE)))
C
          RX(1,IE)= RX(1,IE)+(OMEGA*OMEGA*XG+2.E0*OMEGA*VG(IE))*SN(1,IE)
          RX(2,IE)= RX(2,IE)+(OMEGA*OMEGA*XG+2.E0*OMEGA*VG(IE))*SN(2,IE)
          RX(3,IE)= RX(3,IE)+(OMEGA*OMEGA*XG+2.E0*OMEGA*VG(IE))*SN(3,IE)
          RX(4,IE)= RX(4,IE)+(OMEGA*OMEGA*XG+2.E0*OMEGA*VG(IE))*SN(4,IE)
          RX(5,IE)= RX(5,IE)+(OMEGA*OMEGA*XG+2.E0*OMEGA*VG(IE))*SN(5,IE)
          RX(6,IE)= RX(6,IE)+(OMEGA*OMEGA*XG+2.E0*OMEGA*VG(IE))*SN(6,IE)
          RX(7,IE)= RX(7,IE)+(OMEGA*OMEGA*XG+2.E0*OMEGA*VG(IE))*SN(7,IE)
          RX(8,IE)= RX(8,IE)+(OMEGA*OMEGA*XG+2.E0*OMEGA*VG(IE))*SN(8,IE)
C
          RY(1,IE)= RY(1,IE)+(OMEGA*OMEGA*YG-2.E0*OMEGA*UG(IE))*SN(1,IE)
          RY(2,IE)= RY(2,IE)+(OMEGA*OMEGA*YG-2.E0*OMEGA*UG(IE))*SN(2,IE)
          RY(3,IE)= RY(3,IE)+(OMEGA*OMEGA*YG-2.E0*OMEGA*UG(IE))*SN(3,IE)
          RY(4,IE)= RY(4,IE)+(OMEGA*OMEGA*YG-2.E0*OMEGA*UG(IE))*SN(4,IE)
          RY(5,IE)= RY(5,IE)+(OMEGA*OMEGA*YG-2.E0*OMEGA*UG(IE))*SN(5,IE)
          RY(6,IE)= RY(6,IE)+(OMEGA*OMEGA*YG-2.E0*OMEGA*UG(IE))*SN(6,IE)
          RY(7,IE)= RY(7,IE)+(OMEGA*OMEGA*YG-2.E0*OMEGA*UG(IE))*SN(7,IE)
          RY(8,IE)= RY(8,IE)+(OMEGA*OMEGA*YG-2.E0*OMEGA*UG(IE))*SN(8,IE)
 1000 CONTINUE
C
C
C      ADD ACCELERATION TERMS (TO TRANSLATING FRAMES)
C
C
      DO 1150 IE = 1 , NE
          IFRAME = IEATTR(IE)
          IF(IFRAME.LE.0) GO TO 1150
C
          RX(1,IE) = RX(1,IE)+UFRAME(2,IFRAME)*SN(1,IE)
          RX(2,IE) = RX(2,IE)+UFRAME(2,IFRAME)*SN(2,IE)
          RX(3,IE) = RX(3,IE)+UFRAME(2,IFRAME)*SN(3,IE)
          RX(4,IE) = RX(4,IE)+UFRAME(2,IFRAME)*SN(4,IE)
          RX(5,IE) = RX(5,IE)+UFRAME(2,IFRAME)*SN(5,IE)
          RX(6,IE) = RX(6,IE)+UFRAME(2,IFRAME)*SN(6,IE)
          RX(7,IE) = RX(7,IE)+UFRAME(2,IFRAME)*SN(7,IE)
          RX(8,IE) = RX(8,IE)+UFRAME(2,IFRAME)*SN(8,IE)
C
          RY(1,IE) = RY(1,IE)+VFRAME(2,IFRAME)*SN(1,IE)
          RY(2,IE) = RY(2,IE)+VFRAME(2,IFRAME)*SN(2,IE)
          RY(3,IE) = RY(3,IE)+VFRAME(2,IFRAME)*SN(3,IE)
          RY(4,IE) = RY(4,IE)+VFRAME(2,IFRAME)*SN(4,IE)
          RY(5,IE) = RY(5,IE)+VFRAME(2,IFRAME)*SN(5,IE)
          RY(6,IE) = RY(6,IE)+VFRAME(2,IFRAME)*SN(6,IE)
          RY(7,IE) = RY(7,IE)+VFRAME(2,IFRAME)*SN(7,IE)
          RY(8,IE) = RY(8,IE)+VFRAME(2,IFRAME)*SN(8,IE)
C
          RZ(1,IE) = RZ(1,IE)+WFRAME(2,IFRAME)*SN(1,IE)
          RZ(2,IE) = RZ(2,IE)+WFRAME(2,IFRAME)*SN(2,IE)
          RZ(3,IE) = RZ(3,IE)+WFRAME(2,IFRAME)*SN(3,IE)
          RZ(4,IE) = RZ(4,IE)+WFRAME(2,IFRAME)*SN(4,IE)
          RZ(5,IE) = RZ(5,IE)+WFRAME(2,IFRAME)*SN(5,IE)
          RZ(6,IE) = RZ(6,IE)+WFRAME(2,IFRAME)*SN(6,IE)
          RZ(7,IE) = RZ(7,IE)+WFRAME(2,IFRAME)*SN(7,IE)
          RZ(8,IE) = RZ(8,IE)+WFRAME(2,IFRAME)*SN(8,IE)
 1150 CONTINUE
C
C
C
C GIVE WALL SHEAR STRESS
C
C
C
      IF(IMODEL.EQ.2 .OR. IMODEL.EQ.4) THEN
          DO 1120 IEWALL = 1 , NEWALL
              IE = LEWALL(1,IEWALL)
              DO 1110 I = 1 , N
                  RX(I,IE) = RX(I,IE)+TAUX(IEWALL)*SNWALL(I,IEWALL)
                  RY(I,IE) = RY(I,IE)+TAUY(IEWALL)*SNWALL(I,IEWALL)
                  RZ(I,IE) = RZ(I,IE)+TAUZ(IEWALL)*SNWALL(I,IEWALL)
 1110         CONTINUE
 1120     CONTINUE
      ENDIF
C
C
C GATHER ELEMENT RESIDUALS TO GLOBAL NODES
C
C
      CALL SUPUE2(IDIM,RX,IENP,JENP,NEP,ME,MEP,NE,NP,N,
     *            FX,IUT0,IERR)
      CALL SUPUE2(IDIM,RY,IENP,JENP,NEP,ME,MEP,NE,NP,N,
     *            FY,IUT0,IERR)
      CALL SUPUE2(IDIM,RZ,IENP,JENP,NEP,ME,MEP,NE,NP,N,
     *            FZ,IUT0,IERR)
C
      IF(IERR.EQ.1) THEN
          WRITE(IUT0,*) ERMSGC
          RETURN
      ENDIF
C
C
C IMPOSE CYCLIC BOUNDARY CONDITIONS
C
C
      DO 1210 IEP = 1 , MEP
*POPTION INDEP(FX,FY,FZ)
          DO 1200 IPCCL = 1 , NPCCL
              IP1 = LPCCL1(IPCCL)
              IP2 = LPCCL2(IPCCL)
C
              IF(IEP.LE.NEP(IP1)) THEN
                  FX(IP2)=FX(IP2)+RX(JENP(IEP,IP1),IENP(IEP,IP1))
     &                     -PCCL*DNX(JENP(IEP,IP1),IENP(IEP,IP1))
                  FY(IP2)=FY(IP2)+RY(JENP(IEP,IP1),IENP(IEP,IP1))
     &                     -PCCL*DNY(JENP(IEP,IP1),IENP(IEP,IP1))
                  FZ(IP2)=FZ(IP2)+RZ(JENP(IEP,IP1),IENP(IEP,IP1))
     &                     -PCCL*DNZ(JENP(IEP,IP1),IENP(IEP,IP1))
              ENDIF
C
              IF(IEP.LE.NEP(IP2)) THEN
                  FX(IP1)=FX(IP1)+RX(JENP(IEP,IP2),IENP(IEP,IP2))
     &                     +PCCL*DNX(JENP(IEP,IP2),IENP(IEP,IP2))
                  FY(IP1)=FY(IP1)+RY(JENP(IEP,IP2),IENP(IEP,IP2))
     &                     +PCCL*DNY(JENP(IEP,IP2),IENP(IEP,IP2))
                  FZ(IP1)=FZ(IP1)+RZ(JENP(IEP,IP2),IENP(IEP,IP2))
     &                     +PCCL*DNZ(JENP(IEP,IP2),IENP(IEP,IP2))
              ENDIF
 1200     CONTINUE
 1210 CONTINUE
C
C
C SUPERIMPOSE NEIBERING ELEMENT CONTRIBUTIONS
C
C
      IF(IPART.GE.1) THEN
          IDUM = 3
          CALL DDCOM3(IPART,IDUM,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *                FX,FY,FZ,NP,IUT0,IERR,RX,RY,MAXBUF)
          IF(IERR.NE.0) THEN
              WRITE(IUT0,*)
              WRITE(IUT0,*) ERMSGC
              RETURN
          ENDIF
      ENDIF
C
C
C
C UPDATE VELOCITY COMPONENTS
C
C
      DO 1300 IP = 1 , NP
          U(IP) = U(IP)+DT*CM(IP)*FX(IP)
          V(IP) = V(IP)+DT*CM(IP)*FY(IP)
          W(IP) = W(IP)+DT*CM(IP)*FZ(IP)
 1300 CONTINUE
C
C
C SET AND PRESCRIBE BOUNDARY CONDITIONS
C
C
 1310 CONTINUE
C
C      A. INLET BOUNDARY CONDITIONS
C
*POPTION INDEP(U,V,W)
C*$*ASSERT PERMUTATION ( LPINLT )
      DO 1400 IPINLT = 1 , NPINLT
          U(LPINLT(IPINLT)) = DEVLP1*UINLT(IPINLT)
          V(LPINLT(IPINLT)) = DEVLP1*VINLT(IPINLT)
          W(LPINLT(IPINLT)) = DEVLP1*WINLT(IPINLT)
 1400 CONTINUE
C
C      B. WALL BOUNDARY CONDITIONS
C
      IF(IMODEL.EQ.0 .OR. IMODEL.EQ.1 .OR. IMODEL.EQ.3 .OR.
     &   IMODEL.EQ.5) THEN
*POPTION INDEP(U,V,W)
C*$*ASSERT PERMUTATION ( LPWALL )
          DO 1500 IPWALL = 1 , NPWALL
              U(LPWALL(IPWALL)) = DEVLP2*UWALL(IPWALL)
              V(LPWALL(IPWALL)) = DEVLP2*VWALL(IPWALL)
              W(LPWALL(IPWALL)) = DEVLP2*WWALL(IPWALL)
 1500     CONTINUE
      ELSE
          IF(NPCON.GE.1) THEN
*POPTION INDEP(FX,FY,FZ)
C*$*ASSERT PERMUTATION ( LPCON )
              DO 1600 IPWALL = 1 , NPWALL
                  IP = LPWALL(IPWALL)
                  FX(IP) = DEVLP2*UWALL(IPWALL)
                  FY(IP) = DEVLP2*VWALL(IPWALL)
                  FZ(IP) = DEVLP2*WWALL(IPWALL)
 1600         CONTINUE
          ENDIF
*POPTION INDEP(U,V,W)
C*$*ASSERT PERMUTATION ( LPCON )
          DO 1700 IPCON = 1 , NPCON
              U(LPCON(IPCON)) = FX(LPCON(IPCON))
              V(LPCON(IPCON)) = FY(LPCON(IPCON))
              W(LPCON(IPCON)) = FZ(LPCON(IPCON))
 1700     CONTINUE
C
*POPTION INDEP(U,V,W)
C*$*ASSERT PERMUTATION ( LPWALL )
          DO 1800 IPWALL = 1 , NPWALL
              COF = XPWALL(IPWALL)*U(LPWALL(IPWALL))
     &             +YPWALL(IPWALL)*V(LPWALL(IPWALL))
     &             +ZPWALL(IPWALL)*W(LPWALL(IPWALL))
              U(LPWALL(IPWALL))=U(LPWALL(IPWALL))-COF*XPWALL(IPWALL)
              V(LPWALL(IPWALL))=V(LPWALL(IPWALL))-COF*YPWALL(IPWALL)
              W(LPWALL(IPWALL))=W(LPWALL(IPWALL))-COF*ZPWALL(IPWALL)
 1800     CONTINUE
      ENDIF
C
C      C. SYMMETRIC BOUNDARY CONDITIONS
C
*POPTION INDEP(U,V,W)
C*$*ASSERT PERMUTATION ( LPSYMT )
      DO 1900 IPSYMT = 1 , NPSYMT
          COF = XPSYMT(IPSYMT)*U(LPSYMT(IPSYMT))
     &         +YPSYMT(IPSYMT)*V(LPSYMT(IPSYMT))
     &         +ZPSYMT(IPSYMT)*W(LPSYMT(IPSYMT))
          U(LPSYMT(IPSYMT)) = U(LPSYMT(IPSYMT))-COF*XPSYMT(IPSYMT)
          V(LPSYMT(IPSYMT)) = V(LPSYMT(IPSYMT))-COF*YPSYMT(IPSYMT)
          W(LPSYMT(IPSYMT)) = W(LPSYMT(IPSYMT))-COF*ZPSYMT(IPSYMT)
 1900 CONTINUE
C
C      D. DEPENDING BOUNDARY CONDITIONS
C
*POPTION INDEP(U,V,W)
C*$*ASSERT PERMUTATION ( LPDEP1, LPDEP2 )
      DO 2000 IPDEP = 1 , NPDEP
          U(LPDEP2(IPDEP)) = U(LPDEP1(IPDEP))
          V(LPDEP2(IPDEP)) = V(LPDEP1(IPDEP))
          W(LPDEP2(IPDEP)) = W(LPDEP1(IPDEP))
 2000 CONTINUE
C
C      E. CYCLIC BOUNDARY CONDITIONS
C
C NOTES; THE BELOW PART JUST GUARANTEES EXACT EQUALITY OF THE CYCLIC
C       NODES AND CALCULATES FLOW RATE ON THE CYCLIC BOUNDARIES WHICH
C       WILL BE NEEDED TO DETERMINE THE PRESSURE DIFFERENCE OF THE NEXT
C       TIME INTEGRATION. THE MAJOR PART OF CYCLIC BOUNDARY CONDITION
C       IS IMPLEMENTED IN THE SUPERPOSITION OF ELEMENT RESIDUALS.
C
*POPTION INDEP(U,V,W)
C*$*ASSERT PERMUTATION ( LPCCL1, LPCCL2 )
      DO 2100 IPCCL = 1 , NPCCL
          U(LPCCL2(IPCCL)) = U(LPCCL1(IPCCL))
          V(LPCCL2(IPCCL)) = V(LPCCL1(IPCCL))
          W(LPCCL2(IPCCL)) = W(LPCCL1(IPCCL))
 2100 CONTINUE
C
      QCCLP = QCCL
      IF(ITIME.EQ.0)  QCCLP = 0.E0
      QCCL  = 0.E0
      DO 2200 IECCL = 1 , NECCL
          IE = LECCL(1,IECCL)
          IS = LECCL(2,IECCL)
          UB = U(NODE(LOCAL(1,IS),IE))+U(NODE(LOCAL(2,IS),IE))
     &        +U(NODE(LOCAL(3,IS),IE))+U(NODE(LOCAL(4,IS),IE))
          VB = V(NODE(LOCAL(1,IS),IE))+V(NODE(LOCAL(2,IS),IE))
     &        +V(NODE(LOCAL(3,IS),IE))+V(NODE(LOCAL(4,IS),IE))
          WB = W(NODE(LOCAL(1,IS),IE))+W(NODE(LOCAL(2,IS),IE))
     &        +W(NODE(LOCAL(3,IS),IE))+W(NODE(LOCAL(4,IS),IE))
C
          QCCL = QCCL+0.25E0*AECCL(IECCL)
     &          *(XNCCL(IECCL)*UB+YNCCL(IECCL)*VB+ZNCCL(IECCL)*WB)
 2200 CONTINUE
C
C
C     OVERSET NODAL VELOCITIES
C
C
      IF(JSET.GE.1) THEN
C
          TH    = OMEGA*TIMER
          COSTH = COS(TH)
          SINTH = SIN(TH)
C
C        INTERPOLATE VELOCITIES
C
          DO 2300 IBP = 1 , NPSET
              ISEND = LPSET3(IBP)
              IF(ISEND.LT.0) GO TO 2300
C
              IE = LPSET2(IBP)
C
              S1 = 0.125*(1.E0+GI(1)*GPSET(IBP))
     &                  *(1.E0+EI(1)*EPSET(IBP))
     &                  *(1.E0+TI(1)*TPSET(IBP))
C
              S2 = 0.125*(1.E0+GI(2)*GPSET(IBP))
     &                  *(1.E0+EI(2)*EPSET(IBP))
     &                  *(1.E0+TI(2)*TPSET(IBP))
C
              S3 = 0.125*(1.E0+GI(3)*GPSET(IBP))
     &                  *(1.E0+EI(3)*EPSET(IBP))
     &                  *(1.E0+TI(3)*TPSET(IBP))
C
              S4 = 0.125*(1.E0+GI(4)*GPSET(IBP))
     &                  *(1.E0+EI(4)*EPSET(IBP))
     &                  *(1.E0+TI(4)*TPSET(IBP))
C
              S5 = 0.125*(1.E0+GI(5)*GPSET(IBP))
     &                  *(1.E0+EI(5)*EPSET(IBP))
     &                  *(1.E0+TI(5)*TPSET(IBP))
C
              S6 = 0.125*(1.E0+GI(6)*GPSET(IBP))
     &                  *(1.E0+EI(6)*EPSET(IBP))
     &                  *(1.E0+TI(7)*TPSET(IBP))
C
              S7 = 0.125*(1.E0+GI(7)*GPSET(IBP))
     &                  *(1.E0+EI(7)*EPSET(IBP))
     &                  *(1.E0+TI(7)*TPSET(IBP))
C
              S8 = 0.125*(1.E0+GI(8)*GPSET(IBP))
     &                  *(1.E0+EI(8)*EPSET(IBP))
     &                  *(1.E0+TI(8)*TPSET(IBP))
C
              FX(IBP) = S1*X(NODE(1,IE))+S5*X(NODE(5,IE))
     &                 +S2*X(NODE(2,IE))+S6*X(NODE(6,IE))
     &                 +S3*X(NODE(3,IE))+S7*X(NODE(7,IE))
     &                 +S4*X(NODE(4,IE))+S8*X(NODE(8,IE))
C
              FY(IBP) = S1*Y(NODE(1,IE))+S5*Y(NODE(5,IE))
     &                 +S2*Y(NODE(2,IE))+S6*Y(NODE(6,IE))
     &                 +S3*Y(NODE(3,IE))+S7*Y(NODE(7,IE))
     &                 +S4*Y(NODE(4,IE))+S8*Y(NODE(8,IE))
C
              UG(IBP) = S1*U(NODE(1,IE))+S5*U(NODE(5,IE))
     &                 +S2*U(NODE(2,IE))+S6*U(NODE(6,IE))
     &                 +S3*U(NODE(3,IE))+S7*U(NODE(7,IE))
     &                 +S4*U(NODE(4,IE))+S8*U(NODE(8,IE))
C
              VG(IBP) = S1*V(NODE(1,IE))+S5*V(NODE(5,IE))
     &                 +S2*V(NODE(2,IE))+S6*V(NODE(6,IE))
     &                 +S3*V(NODE(3,IE))+S7*V(NODE(7,IE))
     &                 +S4*V(NODE(4,IE))+S8*V(NODE(8,IE))
C
              WG(IBP) = S1*W(NODE(1,IE))+S5*W(NODE(5,IE))
     &                 +S2*W(NODE(2,IE))+S6*W(NODE(6,IE))
     &                 +S3*W(NODE(3,IE))+S7*W(NODE(7,IE))
     &                 +S4*W(NODE(4,IE))+S8*W(NODE(8,IE))
 2300     CONTINUE
C
C         CONVERT INTERPOLATED VELOCITIES FOR STATIONARY REFERENCE FRAME
C
          DO 2310 IBP = 1 , NPSET
              ISEND = LPSET3(IBP)
              IF(ISEND.LT.0) GO TO 2310
C
              IE     = LPSET2(IBP)
              IFRAME = IEATTR(IE)
C
              IF(IFRAME.EQ.-1) THEN
                  UR = UG(IBP)-OMEGA*FY(IBP)
                  VR = VG(IBP)+OMEGA*FX(IBP)
                  UG(IBP) = UR*COSTH-VR*SINTH
                  VG(IBP) = UR*SINTH+VR*COSTH
              ENDIF
C
              IF(IFRAME.GE. 1) THEN
                  UG(IBP) = UG(IBP)+UFRAME(1,IFRAME)
                  VG(IBP) = VG(IBP)+VFRAME(1,IFRAME)
                  WG(IBP) = WG(IBP)+WFRAME(1,IFRAME)
              ENDIF
 2310     CONTINUE
C
C         PERFORM SELF-DOMAIN VELOCITY OVERSETS
C
          NB = 0
*POPTION INDEP(U,V,W)
          DO 2320 IBP = 1 , NPSET
              ISEND = LPSET3(IBP)
              IF(ISEND.LT.0) GO TO 2320
C
              IP = LPSET1(IBP)
C
              IF(ISEND.EQ.0) THEN
                  U(IP) = UG(IBP)
                  V(IP) = VG(IBP)
                  W(IP) = WG(IBP)
              ELSE
                  NB = NB+1
                  FX(NB) = UG(IBP)
                  FY(NB) = VG(IBP)
                  FZ(NB) = WG(IBP)
              ENDIF
 2320     CONTINUE
C
C         PERFORM INTER-DOMAIN VELOCITY OVERSETS
C
          IF(IPART.GE.1) THEN
              CALL DDSET3(NPSND,LPSND,NPTSND,IPSET,IPSRC,FX,FY,FZ,NB,
     *                    NPRCV,LPRCV,NPTRCV,U,V,W,NP,
     *                    IDIM,MBPDOM,IUT0,IERR,RX,RY,MAXBUF)
              IF(IERR.NE.0) THEN
                  WRITE(IUT0,*)
                  WRITE(IUT0,*) ERMSGC
                  RETURN
              ENDIF
          ENDIF
C
C     CONVERT OVERSET VELOCITIES FOR THE OWN REFERENCE FRAME
C
*POPTION INDEP(U,V,W)
          DO 2330 IBP = 1 , NPSET
              ISEND = LPSET3(IBP)
              IF(ISEND.GE.1) GO TO 2330
C
              IP     = LPSET1(IBP)
              IFRAME = IPATTR(IP)
C
              IF(IFRAME.EQ.-1) THEN
                  UR    = U(IP)*COSTH+V(IP)*SINTH
                  VR    =-U(IP)*SINTH+V(IP)*COSTH
                  U(IP) = UR+OMEGA*Y(IP)
                  V(IP) = VR-OMEGA*X(IP)
              ENDIF
C
              IF(IFRAME.GE. 1) THEN
                  U(IP) = U(IP)-UFRAME(1,IFRAME)
                  V(IP) = V(IP)-VFRAME(1,IFRAME)
                  W(IP) = W(IP)-WFRAME(1,IFRAME)
              ENDIF
 2330     CONTINUE
      ENDIF
C
C
      RETURN
      END
