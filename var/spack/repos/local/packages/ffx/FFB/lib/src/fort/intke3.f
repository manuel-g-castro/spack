C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    INTKE3                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE INTKE3(JSET,VKAP,CNUE,C1,C2,SIGK,SIGE,GI,EI,TI,
     *                  CILTK,CILTE,CLMTK,CLMTE,VISC,VISCM,UTAU,YP,
     *                  IFORM,DT,ITIME,U,V,W,TK,TE,PD,SX,SY,SZ,
     *                  SN,CM,EX,EY,EZ,EXX,EYY,EZZ,EXY,EXZ,EYZ,
     *                  NODE,IENP,JENP,NEP,ME,MEP,NE,NP,N,
     *                  NPINLT,LPINLT,UINLT,VINLT,WINLT,
     *                  NPWALL,LPWALL,NEWALL,MEPWL,IEPWL,NEPWL,
     *                  NPCCL ,LPCCL1,LPCCL2,NPDEP ,LPDEP1,LPDEP2,
     *                  IPART ,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *                  LPSET1,LPSET2,LPSET3,GPSET,EPSET,TPSET,NPSET,
     *                  NPSND ,LPSND ,NPTSND,IPSET,IPSRC,
     *                  NPRCV ,LPRCV ,NPTRCV,
     *                  RK,RE,FK,FE,FX,FY,FZ,GK,GE,UG,VG,WG,
     *                  IUTWRN,IUT0,IERR)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION VISC(NE),UTAU(NEWALL),YP(NEWALL),
     1          U(NP),V(NP),W(NP),TK(NP),TE(NP),PD(NE),
     2          SX(NE),SY(NE),SZ(NE),GI(N),EI(N),TI(N),
     3          SN(N,NE),CM(NP),
     4          EX (ME,N,N),EY (ME,N,N),EZ (ME,N,N),
     5          EXX(ME,N,N),EYY(ME,N,N),EZZ(ME,N,N),
     6          EXY(ME,N,N),EXZ(ME,N,N),EYZ(ME,N,N),
     7          NODE(N,NE),IENP(MEP,NP),JENP(MEP,NP),NEP(NP)
C
      DIMENSION LPINLT(NPINLT),
     1          UINLT (NPINLT),VINLT (NPINLT),WINLT (NPINLT),
     2          LPWALL(NPWALL),IEPWL(MEPWL,NPWALL),NEPWL(NPWALL),
     3          LPCCL1(NPCCL ),LPCCL2(NPCCL),
     4          LPDEP1(NPDEP ),LPDEP2(NPDEP)
C
      DIMENSION LDOM(NDOM),NBPDOM(NDOM),
     1          IPSLF(MBPDOM,NDOM),IPSND(MBPDOM,NDOM)
C
      DIMENSION LPSET1(NPSET),LPSET2(NPSET),LPSET3(NPSET),
     1          GPSET (NPSET),EPSET (NPSET),TPSET (NPSET),
     2          LPSND (NPSND),NPTSND(NPSND),LPRCV (NPRCV),NPTRCV(NPRCV),
     3          IPSET (MBPDOM,NPSND),IPSRC (MBPDOM,NPSND)
C
      DIMENSION RK(N,NE),RE(N,NE),FK(NP),FE(NP),
     1          FX(NE),FY(NE),FZ(NE),GK(NE),GE(NE),UG(NE),VG(NE),WG(NE)
C
      CHARACTER*60 ERMSGC
     & /' ## SUBROUTINE INTKE3: FATAL      ERROR REPORT   ; RETURNED' /
C
      DATA IDIM    / 3 /
      DATA EPS     / 1.0E-20 /
C
C
C      INTEGRAL TRANSPORT EQUATIONS FOR K-EPSILON MODEL
C         ( 3-D CALCULATION : SINGLE WORD VERSION )
C
C
C     NOTES; WALL BOUNDARY VALUES OF K AND EPSILON ARE NOT EXACTLY
C           CALCULATED IN PARALLEL MODE EXECUTION.
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          JSET        ; OVERSET WILL BE DONE WHEN SET GREATER THAN ZERO
C          VKAP        ; VON-KARMAN CONSTANT
C          CNUE        ; CONSTANT FOR TURBULENT KINEMATIC VISCOSITY 
C          C1          ; SCALE FACTOR FOR PRODUCTION  TERM FOR EPSIRION
C          C2          ; SCALE FACTOR FOR DISSIPATION TERM FOR EPSIRION
C          SIGK        ; TURBULENT PRANDTL NUMBER FOR DIFFUSION OF 'K'
C          SIGE        ; TURBULENT PRANDTL NUMBER FOR DIFFUSION OF 'E'
C
C          CILTK       ; RATIO OF TURBULENT/MEAN ENERGY AT INLET
C          CILTE       ; RATIO OF TURBULENT/MOLECULAR VISCOSITY AT INLET
C          CLMTK       ; LOWER BOUND OF TURBULENT ENERGY
C          CLMTE       ; LOWER BOUND OF DISSIPATION RATE OF TURBULENT E.
C
C          VISC    (IE); ELEMENT TOTAL VISCOSITY ( MOLECULAR+TURBULENT )
C          VISCM       ; MOLECULAR VISCOSITY
C          UTAU   (IBE); FRICTION VELOCITY AT WALL SURFACES
C          YP     (IBE); DISTANCE BETWEEN WALL AND ITS OPPOSITE SURFACE
C
C          IFORM       ; SPECIFIES CONVECTION TERM DISCRETIZATIONS
C                   0 --- NORMAL GALERKIN TYPE
C                   3 --- STREAMLINE UPWIND PETROV-GALERKIN TYPE
C                   4 --- TIME-ACCURATE STREAMLINE UPWIND TYPE
C
C          DT          ; TIME INCTREMENT
C          ITIME       ; CUREENT TIME STEP
C          U       (IP); X-DIR. VELOCITY COMPONENT
C          V       (IP); Y-DIR. VELOCITY COMPONENT
C          W       (IP); Z-DIR. VELOCITY COMPONENT
C          PD      (IE); PRODUCTION TERM FOR K-EQUATION
C          SX      (IE); UPWIND VECTOR IN X-DIR.
C          SY      (IE); UPWIND VECTOR IN Y-DIR.
C          SZ      (IE); UPWIND VECTOR IN Z-DIR.
C
C          SN    (I,IE); INTEGRATED ELEMENT VECTOR OF N
C          CM      (IP); INVERSED LUMPED MASS MATRIX
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
C          NODE  (I,IE); NODE NO. TABLE BASED ON ELEMENT
C          IENP(IEP,IP); ADJACENT ELEMENT NUMBER TO NODE IP
C                      ( IF NEP(IP).LT.MEP , THEN IENP(NEP(IP)+1,IP),
C                       IENP(MEP,IP) MUST BE SET TO AN IMAGINARY
C                       ELEMENT NO. BETWEEN NE+1,ME.)
C          JENP(IEP,IP); LOCAL NODE NUMBER OF IP IN ELEMENT IENP(IEP,IP)
C          NEP     (IP); NUMBER OF ADJACENT ELEMENTS TO NODE IP
C          ME          ; THE MAXIMUM NUMBER  OF ELEMETS
C          MEP         ; THE FIRST DIMENSION OF ARRAY IENP,JENP
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          NP          ; NUMBER OF TOTAL    NODES
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT
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
C          NEWALL      ; NUMBER OF WALL BOUNDARY SURFACES
C          MEPWL       ; MAX. NUMBER OF ADJACENT WALL SURFACE  ELEMENTS
C          IEPWL (IEP, ; WALL SURFACE ELEMENT NUMBER ADJACENT TO WALL
C                  IBP) BOUNDARY NODE 'IBP'
C          NEPWL  (IBP); NUMBER OF WALL SURFACE ELEMENTS ADJACENT TO
C                       WALL BOUNDARY NODE 'IBP'
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
C
C          IUTWRN      ; FILE NUMBER TO ISSUE  WARNINGS IF SET POSITIVE
C          IUT0        ; FILE NUMBER TO REPORT ERROR OCCURENCE
C
C          IPART       ; SUB-DOMAIN NUMBER THAT THIS TASK SHOULD TAKE/IS
C                       TAKING CARE OF. IPART BEING SET ZERO MEANS THAT
C                       THE PROGRAM SHOULD RUN/IS RUNNING IN SERIAL 
C                       MODE.
C
C        F. INTER-CONNECT BOUNDARY
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
C
C          NPRCV       ; NUMBER OF DOMAINS TO RECEIVE OVERSET NODE VALUE
C          LPRCV (IDOM); DOMAIN NUMBER     TO RECEIVE OVERSET NODE VALUE
C          NPTRCV(IDOM); NUMBER OF OVERSET POINTS TO RECEIVE FROM
C                       DOMAIN 'LPRCV(IDOM)'
C
C       (2) OUTPUT
C          IERR        ; RETURN CODE TO REPORT ERROR OCCURENCE
C                   0 --- NORMAL TERMINATION
C                   1 --- A FATAL ERROR HAS OCCURED
C
C       (3) INPUT-OUTPUT
C          TK      (IP); TURBULENT KINETIC ENERGY
C          TE      (IP); DISSIPATION RATE OF TURBULENT KINETIC ENERGY
C
C       (4) WORK
C          RK    (I,IE); HOLDS K      -EQUATION ELEMENT-WISE RESIDUAL
C          RE    (I,IE); HOLDS EPSILON-EQUATION ELEMENT-WISE RESIDUAL
C          FK      (IP); HOLDS K      -EQUATION GLOBAL NODE  RESIDUAL
C          FE      (IP); HOLDS E      -EQUATION GLOBAL NODE  RESIDUAL
C          FX      (IE); HOLDS TEMPORARILY KEEPS U(NODE(I,IE))
C          FY      (IE); HOLDS TEMPORARILY KEEPS V(NODE(I,IE))
C          FZ      (IE); HOLDS TEMPORARILY KEEPS W(NODE(I,IE))
C          GK      (IE); HOLDS STORES ELEMENT VALUE OF K
C          GE      (IE); HOLDS STORES ELEMENT VALUE OF EPSILON
C          UG      (IE); HOLDS ELEMENT CENTER U-VELOCITY
C          VG      (IE); HOLDS ELEMENT CENTER V-VELOCITY
C          WG      (IE); HOLDS ELEMENT CENTER W-VELOCITY
C
C
      MAXBUF = NE*N
C
      IF(ITIME.EQ.0) GO TO 1010
C
C
C CALCULATE ELEMENT VALUE OF VELOCITY COMPONENTS, K, AND EPSILON
C
C
      DO 100 IE = 1 , NE
          UG(IE) = 0.125E0*(U(NODE(1,IE))+U(NODE(5,IE))
     &                     +U(NODE(2,IE))+U(NODE(6,IE))
     &                     +U(NODE(3,IE))+U(NODE(7,IE))
     &                     +U(NODE(4,IE))+U(NODE(8,IE)))
          VG(IE) = 0.125E0*(V(NODE(1,IE))+V(NODE(5,IE))
     &                     +V(NODE(2,IE))+V(NODE(6,IE))
     &                     +V(NODE(3,IE))+V(NODE(7,IE))
     &                     +V(NODE(4,IE))+V(NODE(8,IE)))
          WG(IE) = 0.125E0*(W(NODE(1,IE))+W(NODE(5,IE))
     &                     +W(NODE(2,IE))+W(NODE(6,IE))
     &                     +W(NODE(3,IE))+W(NODE(7,IE))
     &                     +W(NODE(4,IE))+W(NODE(8,IE)))
C
          GK(IE) = 0.125E0*(TK(NODE(1,IE))+TK(NODE(5,IE))
     &                     +TK(NODE(2,IE))+TK(NODE(6,IE))
     &                     +TK(NODE(3,IE))+TK(NODE(7,IE))
     &                     +TK(NODE(4,IE))+TK(NODE(8,IE)))
          GE(IE) = 0.125E0*(TE(NODE(1,IE))+TE(NODE(5,IE))
     &                     +TE(NODE(2,IE))+TE(NODE(6,IE))
     &                     +TE(NODE(3,IE))+TE(NODE(7,IE))
     &                     +TE(NODE(4,IE))+TE(NODE(8,IE)))
  100 CONTINUE
C
C
C CLEAR ARRAYS
C
C
      DO 210 I = 1 , N
          DO 200 IE = 1 , NE
              RK(I,IE) = 0.E0
              RE(I,IE) = 0.E0
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
C CALCULATE ADVECTION & DIFFUSION TERMS
C
C
      DO 520 J = 1 , N
          DO 400 IE = 1 , NE
              FX(IE) = U (NODE(J,IE))
              FY(IE) = V (NODE(J,IE))
              FZ(IE) = W (NODE(J,IE))
              FK(IE) = TK(NODE(J,IE))
              FE(IE) = TE(NODE(J,IE))
  400     CONTINUE
C
          DO 510 I = 1 , N
              DO 500 IE = 1 , NE
                  AI =        (EX (IE,I,J)+SX(IE)*EXX(IE,I,J)
     &                 +SY(IE)*EXY(IE,J,I)+SZ(IE)*EXZ(IE,J,I))*UG(IE)
     &                       +(EY (IE,I,J)+SX(IE)*EXY(IE,I,J)
     &                 +SY(IE)*EYY(IE,I,J)+SZ(IE)*EYZ(IE,J,I))*VG(IE)
     &                       +(EZ (IE,I,J)+SX(IE)*EXZ(IE,I,J)
     &                 +SY(IE)*EYZ(IE,I,J)+SZ(IE)*EZZ(IE,I,J))*WG(IE)
C
                  DK = ((VISC(IE)-VISCM)/SIGK+VISCM)*(EXX(IE,I,J)
     &                                   +EYY(IE,I,J)+EZZ(IE,I,J))
                  DE = ((VISC(IE)-VISCM)/SIGE+VISCM)*(EXX(IE,I,J)
     &                                   +EYY(IE,I,J)+EZZ(IE,I,J))
C
                  RK(I,IE)=RK(I,IE)-(AI+DK)*FK(IE)
                  RE(I,IE)=RE(I,IE)-(AI+DE)*FE(IE)
  500         CONTINUE
  510     CONTINUE
  520 CONTINUE
C
C
C CALCULATE SOURCE TERMS
C
C
      DO 800 IE = 1 , NE
          RK(1,IE)=RK(1,IE)+SN(1,IE)*(PD(IE)-GE(IE))
          RK(2,IE)=RK(2,IE)+SN(2,IE)*(PD(IE)-GE(IE))
          RK(3,IE)=RK(3,IE)+SN(3,IE)*(PD(IE)-GE(IE))
          RK(4,IE)=RK(4,IE)+SN(4,IE)*(PD(IE)-GE(IE))
          RK(5,IE)=RK(5,IE)+SN(5,IE)*(PD(IE)-GE(IE))
          RK(6,IE)=RK(6,IE)+SN(6,IE)*(PD(IE)-GE(IE))
          RK(7,IE)=RK(7,IE)+SN(7,IE)*(PD(IE)-GE(IE))
          RK(8,IE)=RK(8,IE)+SN(8,IE)*(PD(IE)-GE(IE))
C
          RE(1,IE)=RE(1,IE)+SN(1,IE)*GE(IE)/GK(IE)*(C1*PD(IE)-C2*GE(IE))
          RE(2,IE)=RE(2,IE)+SN(2,IE)*GE(IE)/GK(IE)*(C1*PD(IE)-C2*GE(IE))
          RE(3,IE)=RE(3,IE)+SN(3,IE)*GE(IE)/GK(IE)*(C1*PD(IE)-C2*GE(IE))
          RE(4,IE)=RE(4,IE)+SN(4,IE)*GE(IE)/GK(IE)*(C1*PD(IE)-C2*GE(IE))
          RE(5,IE)=RE(5,IE)+SN(5,IE)*GE(IE)/GK(IE)*(C1*PD(IE)-C2*GE(IE))
          RE(6,IE)=RE(6,IE)+SN(6,IE)*GE(IE)/GK(IE)*(C1*PD(IE)-C2*GE(IE))
          RE(7,IE)=RE(7,IE)+SN(7,IE)*GE(IE)/GK(IE)*(C1*PD(IE)-C2*GE(IE))
          RE(8,IE)=RE(8,IE)+SN(8,IE)*GE(IE)/GK(IE)*(C1*PD(IE)-C2*GE(IE))
  800 CONTINUE
C
C
C GATHER ELEMENT RESIDUALS TO GLOBAL NODES
C
C
      CALL SUPUE2(IDIM,RK,IENP,JENP,NEP,ME,MEP,NE,NP,N,
     *            FK,IUT0,IERR)
      CALL SUPUE2(IDIM,RE,IENP,JENP,NEP,ME,MEP,NE,NP,N,
     *            FE,IUT0,IERR)
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
      DO 910 IEP = 1 , MEP
*VOPTION VEC
          DO 900 IPCCL = 1 , NPCCL
              IP1 = LPCCL1(IPCCL)
              IP2 = LPCCL2(IPCCL)
              IF(IEP.LE.NEP(IP1)) THEN
                  FK(IP2)=FK(IP2)+RK(JENP(IEP,IP1),IENP(IEP,IP1))
                  FE(IP2)=FE(IP2)+RE(JENP(IEP,IP1),IENP(IEP,IP1))
              ENDIF
C
              IF(IEP.LE.NEP(IP2)) THEN
                  FK(IP1)=FK(IP1)+RK(JENP(IEP,IP2),IENP(IEP,IP2))
                  FE(IP1)=FE(IP1)+RE(JENP(IEP,IP2),IENP(IEP,IP2))
              ENDIF
  900     CONTINUE
  910 CONTINUE
C
C
C SUPERIMPOSE NEIBERING ELEMENT CONTRIBUTIONS
C
C
      IF(IPART.GE.1) THEN
          IDUM = 2
          CALL DDCOM3(IPART,IDUM,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *                FK,FE,FE,NP,IUT0,IERR,RK,RE,MAXBUF)
          IF(IERR.NE.0) THEN
              WRITE(IUT0,*)
              WRITE(IUT0,*) ERMSGC
              RETURN
          ENDIF
      ENDIF
C
C
C
C UPDATE K AND EPSILON
C
C
      DO 1000 IP = 1 , NP
          TK(IP) = TK(IP)+DT*CM(IP)*FK(IP)
          TE(IP) = TE(IP)+DT*CM(IP)*FE(IP)
 1000 CONTINUE
C
 1010 CONTINUE
C
C
C LOWER-BOUND K AND EPSILON
C
C
      NBNDK = 0
      NBNDE = 0
      DO 1100 IP = 1 , NP
          IF(TK(IP).LT.CLMTK) THEN
             TK(IP) = CLMTK
             NBNDK  = NBNDK+1
          ENDIF
C
          IF(TE(IP).LT.CLMTE) THEN
              TE(IP) = CLMTE
              NBNDE  = NBNDE+1
          ENDIF
 1100 CONTINUE
C
      IF(IUTWRN.GE.0) THEN
          IF(NBNDK.GT.0) WRITE(IUTWRN,*) 
     &    '##INTKE3: K-VALUE SMALLER THAN', CLMTK, NBNDK, ' NODES'
C
          IF(NBNDE.GT.0) WRITE(IUTWRN,*) 
     &    '##INTKE3: E-VALUE SMALLER THAN', CLMTE, NBNDE, ' NODES'
      ENDIF
C
C
C PRESCRIBE BOUNDARY CONDITIONS
C
C
C      A. INLET BOUNDARY CONDITIONS
C
C*$*ASSERT PERMUTATION ( LPINLT )
      DO 1200 IPINLT = 1 , NPINLT
          TK(LPINLT(IPINLT)) = 0.5E0*CILTK*(UINLT(IPINLT)**2
     &                                     +VINLT(IPINLT)**2
     &                                     +WINLT(IPINLT)**2)
          TE(LPINLT(IPINLT)) = CNUE*TK(LPINLT(IPINLT))**2/(CILTE*VISCM)
 1200 CONTINUE
C
C      B. WALL BOUNDARY CONDITIONS
C
C*$*ASSERT PERMUTATION ( LPWALL )
      DO 1300 IPWALL = 1 , NPWALL
          TK(LPWALL(IPWALL)) = 0.E0
          TE(LPWALL(IPWALL)) = 0.E0
 1300 CONTINUE
C
      DO 1410 IEP = 1 , MEPWL
C*$*ASSERT PERMUTATION ( LPWALL )
          DO 1400 IPWALL = 1 , NPWALL
              IP = LPWALL(IPWALL)
              IF(IEP.LE.NEPWL(IPWALL)) THEN
                  IEWALL = IEPWL(IEP,IPWALL)
                  TK(IP) = TK(IP)+UTAU(IEWALL)**2/SQRT(CNUE)
                  TE(IP) = TE(IP)+UTAU(IEWALL)**3/(VKAP*YP(IEWALL))
              ENDIF
 1400     CONTINUE
 1410 CONTINUE
C
C*$*ASSERT PERMUTATION ( LPWALL )
      DO 1500 IPWALL = 1 , NPWALL
          IP = LPWALL(IPWALL)
          TK(IP) = TK(IP)/(FLOAT(NEPWL(IPWALL))+EPS)
          TE(IP) = TE(IP)/(FLOAT(NEPWL(IPWALL))+EPS)
 1500 CONTINUE
C
C      D. DEPENDING BOUNDARY CONDITIONS
C
*VOPTION VEC
C*$*ASSERT PERMUTATION ( LPDEP1, LPDEP2 )
      DO 1600 IPDEP = 1 , NPDEP
          TK(LPDEP2(IPDEP)) = TK(LPDEP1(IPDEP))
          TE(LPDEP2(IPDEP)) = TE(LPDEP1(IPDEP))
 1600 CONTINUE
C
C      E. CYCLIC BOUNDARY CONDITIONS
C
C NOTES; THE BELOW PART JUST GUARANTEES EXACT EQUALITY OF THE CYCLIC
C       NODES. THE MAJOR PART OF CYCLIC BOUNDARY CONDITION IS
C       IMPLEMENTED IN THE SUPERPOSITION OF ELEMENT RESIDUALS.
C
*VOPTION VEC
C*$*ASSERT PERMUTATION ( LPCCL1, LPCCL2 )
      DO 1700 IPCCL = 1 , NPCCL
          TK(LPCCL2(IPCCL)) = TK(LPCCL1(IPCCL))
          TE(LPCCL2(IPCCL)) = TE(LPCCL1(IPCCL))
 1700 CONTINUE
C
C
C
C OVERSET NODAL TURBULENT ENERGY AND DISSIPATION RATE
C
C
C
      IF(JSET.GE.1) THEN
          NB = 0
          DO 1820 IBP = 1 , NPSET
              ISEND = LPSET3(IBP)
              IF(ISEND.LT.0) GO TO 1820
C
              IP = LPSET1(IBP)
              IE = LPSET2(IBP)
              TKS = 0.E0
              TES = 0.E0
              DO 1810 I = 1 , N
                  SHAPE = 0.125*(1.E0+GI(I)*GPSET(IBP))
     &                         *(1.E0+EI(I)*EPSET(IBP))
     &                         *(1.E0+TI(I)*TPSET(IBP))
                  TKS = TKS+SHAPE*TK(NODE(I,IE))
                  TES = TES+SHAPE*TE(NODE(I,IE))
 1810         CONTINUE
C
              IF(ISEND.EQ.0) THEN
                  TK(IP) = TKS
                  TE(IP) = TES
              ELSE
                  NB = NB+1
                  FX(NB) = TKS
                  FY(NB) = TES
              ENDIF
 1820     CONTINUE
          IF(IPART.GE.1) THEN
              NDUM = 2
              CALL DDSET3(NPSND,LPSND,NPTSND,IPSET,IPSRC,FX,FY,FY,NB,
     *                    NPRCV,LPRCV,NPTRCV,TK,TE,TE,NP,
     *                    NDUM,MBPDOM,IUT0,IERR,RK,RE,MAXBUF)
              IF(IERR.NE.0) THEN
                  WRITE(IUT0,*)
                  WRITE(IUT0,*) ERMSGC
                  RETURN
              ENDIF
          ENDIF
      ENDIF
C
C
      RETURN
      END
