C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    RELAX3                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE RELAX3(IMODEL,NFRAME,UFRAME,VFRAME,WFRAME,
     *                  JSET,IEATTR,IPATTR,OMEGA,TIMER,
     *                  DT,ALF,EPS,NMAX,U,V,W,P,DNXI,DNYI,DNZI,DELTA,
     *                  SN,DNX,DNY,DNZ,CM,GI,EI,TI,X,Y,
     *                  NODE,IENP,JENP,NEP,ME,MEP,NE,NP,N,
     *                  NPINLT,LPINLT,
     *                  NPWALL,LPWALL,NPCON ,LPCON ,
     *                  XPWALL,YPWALL,ZPWALL,
     *                  NPSYMT,LPSYMT,XPSYMT,YPSYMT,ZPSYMT,
     *                  NPDEP ,LPDEP2,
     *                  NPCCL ,LPCCL1,LPCCL2,
     *                  IPART ,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *                  LPSET1,LPSET2,LPSET3,GPSET,EPSET,TPSET,NPSET,
     *                  NPSND ,LPSND ,NPTSND,IPSET,IPSRC,
     *                  NPRCV,LPRCV,NPTRCV,
     *                  LESET1,LESET2,LESET3,GESET,EESET,TESET,NESET,
     *                  NESND ,LESND ,NETSND,IESET,IESRC,
     *                  NERCV,LERCV,NETRCV,
     *                  IRN,NRN,DIVMAX,DIVAV,
     *                  RX,RY,RZ,DIV,DP,FX,FY,FZ,UG,VG,WG,PG,IUT0,IERR)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION IEATTR(NE),IPATTR(NP),
     1          UFRAME(2,NFRAME),VFRAME(2,NFRAME),WFRAME(2,NFRAME),
     2          U(NP),V(NP),W(NP),P(NE),
     3          DNXI(N,NE),DNYI(N,NE),DNZI(N,NE),DELTA(NE),
     4          SN  (N,NE),DNX (N,NE),DNY (N,NE),DNZ(N,NE),CM(NP),
     5          GI(N),EI(N),TI(N),X(NP),Y(NP),
     6          NODE(N,NE),IENP(MEP,NP),JENP(MEP,NP),NEP(NP),
     7          RX(N,NE),RY(N,NE),RZ(N,NE),
     8          DIV(NE),DP(NE),FX(NP),FY(NP),FZ(NP),
     9          UG(NP),VG(NP),WG(NP),PG(NP)
C
      DIMENSION LPINLT(NPINLT),
     1          LPWALL(NPWALL),LPCON (NPCON ),
     2          XPWALL(NPWALL),YPWALL(NPWALL),ZPWALL(NPWALL),
     3          LPSYMT(NPSYMT),
     4          XPSYMT(NPSYMT),YPSYMT(NPSYMT),ZPSYMT(NPSYMT),
     5          LPDEP2(NPDEP ),
     6          LPCCL1(NPCCL ),LPCCL2(NPCCL)
C
      DIMENSION LDOM(NDOM),NBPDOM(NDOM),
     1          IPSLF(MBPDOM,NDOM),IPSND(MBPDOM,NDOM)
C
      DIMENSION LPSET1(NPSET),LPSET2(NPSET),LPSET3(NPSET),
     1          GPSET (NPSET),EPSET (NPSET),TPSET (NPSET),
     2          LPSND (NPSND),NPTSND(NPSND),LPRCV (NPRCV),NPTRCV(NPRCV),
     3          IPSET (MBPDOM,NPSND),IPSRC (MBPDOM,NPSND)
C
      DIMENSION LESET1(NESET),LESET2(NESET),LESET3(NESET),
     1          GESET (NESET),EESET (NESET),TESET (NESET),
     2          LESND (NESND),NETSND(NESND),LERCV (NERCV),NETRCV(NERCV),
     3          IESET (MBPDOM,NESND),IESRC (MBPDOM,NESND)
C
C
      CHARACTER*60 ERMSGC
     & / ' ## SUBROUTINE RELAX3: FATAL      ERROR REPORT   ; RETURNED' /
C
      DATA IDIM   / 3 /
      DATA IMODE  / 1 /
C
C
C      CORRECT PRESSURE FIELD AND VELOCITY FIELD
C     BY A SIMULTANEOUS PENALTY RELAXATION METHOD
C         ( 3-D CALCULATION : SINGLE WORD VERSION )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          IMODEL      ; SPECIFIES WALL BOUNDARY CONDITIONS AS FOLLOWS
C             0,1,3,5 --- NO-SLIP CONDITIONS
C                 2,4 --- STRESS  CONDITIONS
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
C          DT          ; TIME INCREMENT
C          ALF         ; RELAXATION COEFFICIENT
C          EPS         ; MAXIMUM ALLOWABLE ERROR
C          NMAX        ; MAXIMUM ITERATION NUMBER
C          DNXI  (I,IE); ELEMENT CENTER VALUE OF NX
C          DNYI  (I,IE); ELEMENT CENTER VALUE OF NY
C          DNZI  (I,IE); ELEMENT CENTER VALUE OF NZ
C          DELTA   (IE); ELEMENT CHARACTERISTIC DIMENSION
C
C          SN    (I,IE); INTEGRATED ELEMENT VECTOR OF N
C          DNX   (I,IE); INTEGRATED ELEMENT VECTOR OF NX
C          DNY   (I,IE); INTEGRATED ELEMENT VECTOR OF NY
C          DNZ   (I,IE); INTEGRATED ELEMENT VECTOR OF NZ
C          CM      (IP); LUMPED    MASS MATRIX
C
C          GI       (I); LOCAL GZAI  COORDINATES OF ELEMENT'S NODES
C          EI       (I); LOCAL EATA  COORDINATES OF ELEMENT'S NODES
C          TI       (I); LOCAL THETA COORDINATES OF ELEMENT'S NODES
C          X       (IP); X-COORDINATES OF NODES
C          Y       (IP); Y-COORDINATES OF NODES
C
C          NODE  (I,IE); NODE TABLE
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
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT
C
C        A. INLET BOUNDARY
C          NPINLT      ; NUMBER OF INLET BOUNDARY NODES
C          LPINLT (IBP); INLET BOUNDARY NODES
C
C        B. WALL BOUNDARY
C          NPWALL      ; NUMBER OF WALL BOUNDARY NODES
C          LPWALL (IBP); WALL BOUNDARY NODES
C          NPCON       ; NUMBER OF CORNER WALL NODES
C          LPCON  (IPC); CORNER WALL NODES
C          XPWALL (IBP); X NORMAL OF WALL BOUNDARY NODES
C          YPWALL (IBP); Y NORMAL OF WALL BOUNDARY NODES
C          ZPWALL (IBP); Z NORMAL OF WALL BOUNDARY NODES
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
C          LPDEP2 (IBP); DEPENDING BOUNDARY NODES-2
C
C        E. CYCLIC BOUNDARY
C          NPCCL       ; NUMBER OF CYCLIC BOUNDARY NODES
C          LPCCL1 (IBP); CYCLIC BOUNDARY NODES-1
C          LPCCL2 (IBP); CYCLIC BOUNDARY NODES-2
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
C        H. OVERSET BOUNDARY NODES
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
C        I. OVERSET BOUNDARY ELEMENTS
C          NESET       ; NUMBER OF OVERSET BOUNDARY ELEMENTS
C          LESET1 (IBE); OVERSET BOUNDARY ELEMENTS
C          LESET2 (IBE); ELEMENT NUMBER TO CALCULATE OVERSET VALUES
C          LESET3 (IBE); DOMAIN NUMBER TO SEND/RECEIVE OVERSET VALUES
C                   0 --- CALCULATE AND SET OVERSET VALUE WITHIN THE
C                         SELF-DOMAIN
C          (POS. INT.)--- SEND    OVERSET VALUE TO   DOMAIN  LPSET3(IB)
C                         AFTER CALCULATING IT WITHIN THE SELF-DOMAIN
C          (NEG. INT.)--- RECEIVE OVERSET VALUE FROM DOMAIN -LPSET3(IB)
C
C          GESET  (IBE); LOCAL GZAI-COORDINATE IN INTERPOLATING ELEMENT
C          EESET  (IBE); LOCAL EATA-COORDINATE IN INTERPOLATING ELEMENT
C          TESET  (IBE); LOCAL ZETA-COORDINATE IN INTERPOLATING ELEMENT
C
C          NESND       ; NUMBER OF DOMAINS TO SEND OVERSET ELEM VALUE
C          LESND (IDOM); DOMAIN NUMBER     TO SEND OVERSET ELEM VALUE
C          NETSND(IDOM); NUMBER OF OVERSET ELEMENTS TO SEND TO
C                        DOMAIN 'LESND(IDOM)'
C          IESET(IPT,IDOM); OVERSET ELEMENT NUMBER IN THE DOMAIN
C                        RECEIVING THE OVERSET VALUES.
C          IESRC(IPT,IDOM); INDICATES POSITION IN THE OVERSET-VALUES
C                           PASSING ARRAYS WHEN OVERSET ELEMENT DATA
C                           ARE COMPILED SEQUENTIALLY
C
C          NERCV       ; NUMBER OF DOMAINS TO RECEIVE OVERSET ELEM VALUE
C          LERCV (IDOM); DOMAIN NUMBER     TO RECEIVE OVERSET ELEM VALUE
C          NETRCV(IDOM); NUMBER OF OVERSET POINTS TO RECEIVE FROM
C                       DOMAIN 'LERCV(IDOM)'
C
C          IUT0        ; FILE NUMBER TO REPORT ERROR OCCURENCE
C
C       (2) OUTPUT
C          IRN         ; RETURN CODE TO REPORT CONVERGENCE
C                   0 --- NOT CONVERGED
C                   1 ---     CONVERGED
C          NRN         ; CALCULATION ITERATED NUMBER
C          DIVMAX      ; MAXIMUM ABSOLUTE DIVERGENT
C          DIVAV       ; SPATIALLY AVERAGED ABSOLUTE DIVERGENT
C          IERR        ; RETURN CODE TO REPORT ERROR OCCURENCE
C                   0 --- NORMAL TERMINATION
C                   1 --- A FATAL ERROR HAS OCCURED
C
C
C       (3) INPUT-OUTPUT
C          U       (IP); X-DIR. VELOCITY COMPONENT
C          V       (IP); Y-DIR. VELOCITY COMPONENT
C          W       (IP); Z-DIR. VELOCITY COMPONENT
C          P       (IE); ELEMENT PRESSURE
C
C       (4) WORK
C          RX    (I,IE); X-DIR. ELEMENT RESIDUAL
C          RY    (I,IE); Y-DIR. ELEMENT RESIDUAL
C          RZ    (I,IE); Z-DIR, ELEMENT RESIDUAL
C          DIV     (IE); ELEMENT DIVERGENT
C          DP      (IE); PRESSURE CORRECTOR
C          FX      (IP); X-DIR. RESIDUAL FORCE VECTOR
C          FY      (IP); Y-DIR. RESIDUAL FORCE VECTOR
C          FZ      (IP); Z-DIR. RESIDUAL FORCE VECTOR
C          UG      (IB); USED FOR OVERSET COMPUTATIONS
C          VG      (IB); USED FOR OVERSET COMPUTATIONS
C          WG      (IB); USED FOR OVERSET COMPUTATIONS
C          PG      (IB); USED FOR OVERSET COMPUTATIONS
C
C
      MAXBUF = NE*N
C
      IRN = 0
      NRN = 0
C
   10 CONTINUE
      NRN = NRN+1
C
C      CALCULATE ELEMENT DIVERGENT
C
      CALL FIELD3(IMODE,U,V,W,DNXI,DNYI,DNZI,NODE,NE,NP,N,DIV)
C
      DIVMAX = 0.E0
      DIVAV  = 0.E0
      DO 100 IE = 1 , NE
          ABSDIV = ABS(DIV(IE))
          DIVAV  = DIVAV+ABSDIV
          DIVMAX = AMAX1(ABSDIV,DIVMAX)
  100 CONTINUE
      DIVAV = DIVAV/NE
      IF(NRN.GT.NMAX) GO TO 2000
      IF(IPART.EQ.0 .AND. DIVMAX.LE.EPS) THEN
          IRN = 1
          GO TO 2000
      ENDIF
C
C      RELAX PRESSURE FIELD
C
      COEFF = -ALF/DT
C
      DO 200 IE = 1 , NE
          DP(IE) = COEFF*DIV(IE)*DELTA(IE)*DELTA(IE)
          P (IE) = P(IE)+DP(IE)
  200 CONTINUE
C
C      CALCULATE ELEMENT RESIDUAL VECTOR FOR VELOCITY CORRECTOR
C
      DO 400 IE = 1 , NE
          RX(1,IE) = DP(IE)*DNX(1,IE) 
          RX(2,IE) = DP(IE)*DNX(2,IE) 
          RX(3,IE) = DP(IE)*DNX(3,IE) 
          RX(4,IE) = DP(IE)*DNX(4,IE) 
          RX(5,IE) = DP(IE)*DNX(5,IE) 
          RX(6,IE) = DP(IE)*DNX(6,IE) 
          RX(7,IE) = DP(IE)*DNX(7,IE) 
          RX(8,IE) = DP(IE)*DNX(8,IE) 
C
          RY(1,IE) = DP(IE)*DNY(1,IE) 
          RY(2,IE) = DP(IE)*DNY(2,IE) 
          RY(3,IE) = DP(IE)*DNY(3,IE) 
          RY(4,IE) = DP(IE)*DNY(4,IE) 
          RY(5,IE) = DP(IE)*DNY(5,IE) 
          RY(6,IE) = DP(IE)*DNY(6,IE) 
          RY(7,IE) = DP(IE)*DNY(7,IE) 
          RY(8,IE) = DP(IE)*DNY(8,IE) 
C
          RZ(1,IE) = DP(IE)*DNZ(1,IE) 
          RZ(2,IE) = DP(IE)*DNZ(2,IE) 
          RZ(3,IE) = DP(IE)*DNZ(3,IE) 
          RZ(4,IE) = DP(IE)*DNZ(4,IE) 
          RZ(5,IE) = DP(IE)*DNZ(5,IE) 
          RZ(6,IE) = DP(IE)*DNZ(6,IE) 
          RZ(7,IE) = DP(IE)*DNZ(7,IE) 
          RZ(8,IE) = DP(IE)*DNZ(8,IE) 
  400 CONTINUE
C
C      SUPERIMPOSE ELEMENT RESIDUAL VECTOR
C
      CALL SUPUE2(IDIM,RX,IENP,JENP,NEP,ME,MEP,NE,NP,N,FX,IUT0,IERR)
      CALL SUPUE2(IDIM,RY,IENP,JENP,NEP,ME,MEP,NE,NP,N,FY,IUT0,IERR)
      CALL SUPUE2(IDIM,RZ,IENP,JENP,NEP,ME,MEP,NE,NP,N,FZ,IUT0,IERR)
C
      IF(IERR.EQ.1) THEN
          WRITE(IUT0,*) ERMSGC
          RETURN
      ENDIF
C
C      IMPOSE CYCLIC BOUNDARY CONDITIONS
C
      DO 610 IEP = 1 , MEP
*POPTION INDEP(FX,FY,FZ)
          DO 600 IPCCL = 1 , NPCCL
              IP1 = LPCCL1(IPCCL)
              IP2 = LPCCL2(IPCCL)
              IF(IEP.LE.NEP(IP1)) THEN
                  FX(IP2)=FX(IP2)+RX(JENP(IEP,IP1),IENP(IEP,IP1))
                  FY(IP2)=FY(IP2)+RY(JENP(IEP,IP1),IENP(IEP,IP1))
                  FZ(IP2)=FZ(IP2)+RZ(JENP(IEP,IP1),IENP(IEP,IP1))
              ENDIF
              IF(IEP.LE.NEP(IP2)) THEN
                  FX(IP1)=FX(IP1)+RX(JENP(IEP,IP2),IENP(IEP,IP2))
                  FY(IP1)=FY(IP1)+RY(JENP(IEP,IP2),IENP(IEP,IP2))
                  FZ(IP1)=FZ(IP1)+RZ(JENP(IEP,IP2),IENP(IEP,IP2))
              ENDIF
  600     CONTINUE
  610 CONTINUE
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
C      PRESCRIBE BOUNDARY CONDITIONS
C
C      A. INLET BOUNDARY CONDITIONS
C
*POPTION INDEP(FX,FY,FZ)
C*$*ASSERT PERMUTATION ( LPINLT )
      DO 800 IPINLT = 1 , NPINLT
          FX(LPINLT(IPINLT)) = 0.E0
          FY(LPINLT(IPINLT)) = 0.E0
          FZ(LPINLT(IPINLT)) = 0.E0
  800 CONTINUE
C
C      B. WALL BOUNDARY CONDITIONS
C
      IF(IMODEL.EQ.0 .OR. IMODEL.EQ.1 .OR. IMODEL.EQ.3 .OR.
     &   IMODEL.EQ.5) THEN
*POPTION INDEP(FX,FY,FZ)
C*$*ASSERT PERMUTATION ( LPWALL )
          DO 900 IPWALL = 1 , NPWALL
              FX(LPWALL(IPWALL)) = 0.E0
              FY(LPWALL(IPWALL)) = 0.E0
              FZ(LPWALL(IPWALL)) = 0.E0
  900     CONTINUE
      ELSE
*POPTION INDEP(FX,FY,FZ)
C*$*ASSERT PERMUTATION ( LPCON )
          DO 1000 IPCON = 1 , NPCON
              FX(LPCON(IPCON)) = 0.E0
              FY(LPCON(IPCON)) = 0.E0
              FZ(LPCON(IPCON)) = 0.E0
 1000     CONTINUE
C
*POPTION INDEP(FX,FY,FZ)
C*$*ASSERT PERMUTATION ( LPWALL )
          DO 1100 IPWALL = 1 , NPWALL
              FACTOR = XPWALL(IPWALL)*FX(LPWALL(IPWALL))
     &                +YPWALL(IPWALL)*FY(LPWALL(IPWALL))
     &                +ZPWALL(IPWALL)*FZ(LPWALL(IPWALL))
              FX(LPWALL(IPWALL)) = FX(LPWALL(IPWALL))
     &                        -FACTOR*XPWALL(IPWALL)
              FY(LPWALL(IPWALL)) = FY(LPWALL(IPWALL))
     &                        -FACTOR*YPWALL(IPWALL)
              FZ(LPWALL(IPWALL)) = FZ(LPWALL(IPWALL))
     &                        -FACTOR*ZPWALL(IPWALL)
 1100     CONTINUE
      ENDIF
C
C      C. SYMMETRIC BOUNDARY CONDITIONS
C
*POPTION INDEP(FX,FY,FZ)
C*$*ASSERT PERMUTATION ( LPSYMT )
      DO 1200 IPSYMT = 1 , NPSYMT
          FACTOR = XPSYMT(IPSYMT)*FX(LPSYMT(IPSYMT))
     &            +YPSYMT(IPSYMT)*FY(LPSYMT(IPSYMT))
     &            +ZPSYMT(IPSYMT)*FZ(LPSYMT(IPSYMT))
          FX(LPSYMT(IPSYMT)) = FX(LPSYMT(IPSYMT))-FACTOR*XPSYMT(IPSYMT)
          FY(LPSYMT(IPSYMT)) = FY(LPSYMT(IPSYMT))-FACTOR*YPSYMT(IPSYMT)
          FZ(LPSYMT(IPSYMT)) = FZ(LPSYMT(IPSYMT))-FACTOR*ZPSYMT(IPSYMT)
 1200 CONTINUE
C
C      D. DEPENDING BOUNDARY CONDITIONS
C
*POPTION INDEP(FX,FY,FZ)
C*$*ASSERT PERMUTATION ( LPDEP2 )
      DO 1300 IPDEP = 1 , NPDEP
          FX(LPDEP2(IPDEP)) = 0.E0
          FY(LPDEP2(IPDEP)) = 0.E0
          FZ(LPDEP2(IPDEP)) = 0.E0
 1300 CONTINUE
C
C
C      RELAX VELOCITY FIELD
C
C
      DO 1400 IP = 1 , NP
          U(IP) = U(IP)+DT*CM(IP)*FX(IP)
          V(IP) = V(IP)+DT*CM(IP)*FY(IP)
          W(IP) = W(IP)+DT*CM(IP)*FZ(IP)
 1400 CONTINUE
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
          DO 1500 IBP = 1 , NPSET
              ISEND = LPSET3(IBP)
              IF(ISEND.LT.0) GO TO 1500
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
 1500     CONTINUE
C
C         CONVERT INTERPOLATED VELOCITIES FOR STATIONARY REFERENCE FRAME
C
          DO 1510 IBP = 1 , NPSET
              ISEND = LPSET3(IBP)
              IF(ISEND.LT.0) GO TO 1510
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
 1510     CONTINUE
C
C         PERFORM SELF-DOMAIN VELOCITY OVERSETS
C
          NB = 0
*POPTION INDEP(U,V,W)
          DO 1520 IBP = 1 , NPSET
              ISEND = LPSET3(IBP)
              IF(ISEND.LT.0) GO TO 1520
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
 1520     CONTINUE
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
          DO 1530 IBP = 1 , NPSET
              ISEND = LPSET3(IBP)
              IF(ISEND.GE.1) GO TO 1530
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
 1530     CONTINUE
      ENDIF
C
C
C      OVERSET ELEMENT PRESSURE
C
C
      IF(JSET.GE.1) THEN
C
C        INTERPOLATE PRESSURES
C
          CALL NODALE(IDIM,SN,CM,IENP,JENP,NEP,ME,MEP,NE,NP,N,
     *                IPART,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *                P,FZ,IUT0,IERR,RX,RY,MAXBUF)
C
          DO 1540 IBE = 1 , NESET
              ISEND = LESET3(IBE)
              IF(ISEND.LT.0) GO TO 1540
C
              IE = LESET2(IBE)
C
              S1 = 0.125*(1.E0+GI(1)*GESET(IBE))
     &                  *(1.E0+EI(1)*EESET(IBE))
     &                  *(1.E0+TI(1)*TESET(IBE))
C
              S2 = 0.125*(1.E0+GI(2)*GESET(IBE))
     &                  *(1.E0+EI(2)*EESET(IBE))
     &                  *(1.E0+TI(2)*TESET(IBE))
C
              S3 = 0.125*(1.E0+GI(3)*GESET(IBE))
     &                  *(1.E0+EI(3)*EESET(IBE))
     &                  *(1.E0+TI(3)*TESET(IBE))
C
              S4 = 0.125*(1.E0+GI(4)*GESET(IBE))
     &                  *(1.E0+EI(4)*EESET(IBE))
     &                  *(1.E0+TI(4)*TESET(IBE))
C
              S5 = 0.125*(1.E0+GI(5)*GESET(IBE))
     &                  *(1.E0+EI(5)*EESET(IBE))
     &                  *(1.E0+TI(5)*TESET(IBE))
C
              S6 = 0.125*(1.E0+GI(6)*GESET(IBE))
     &                  *(1.E0+EI(6)*EESET(IBE))
     &                  *(1.E0+TI(7)*TESET(IBE))
C
              S7 = 0.125*(1.E0+GI(7)*GESET(IBE))
     &                  *(1.E0+EI(7)*EESET(IBE))
     &                  *(1.E0+TI(7)*TESET(IBE))
C
              S8 = 0.125*(1.E0+GI(8)*GESET(IBE))
     &                  *(1.E0+EI(8)*EESET(IBE))
     &                  *(1.E0+TI(8)*TESET(IBE))
C
              PG(IBE) = S1*FZ(NODE(1,IE))+S5*FZ(NODE(5,IE))
     &                 +S2*FZ(NODE(2,IE))+S6*FZ(NODE(6,IE))
     &                 +S3*FZ(NODE(3,IE))+S7*FZ(NODE(7,IE))
     &                 +S4*FZ(NODE(4,IE))+S8*FZ(NODE(8,IE))
 1540     CONTINUE
C
C         PERFORM SELF-DOMAIN PRESSURE OVERSETS
C
          NB = 0
*POPTION INDEP(P)
          DO 1550 IBE = 1 , NESET
              ISEND = LESET3(IBE)
              IF(ISEND.LT.0) GO TO 1550
C
              IF = LESET1(IBE)
C
              IF(ISEND.EQ.0) THEN
                  P(IF) = PG(IBE)
              ELSE
                  NB = NB+1
                  FX(NB) = PG(IBE)
              ENDIF
 1550     CONTINUE
C
C         PERFORM INTER-DOMAIN PRESSURE OVERSETS
C
          IF(IPART.GE.1) THEN
              IDUM = 1
              CALL DDSET3(NESND,LESND,NETSND,IESET,IESRC,FX,FX,FX,NB,
     *                    NERCV,LERCV,NETRCV,P,P,P,NE,
     *                    IDUM,MBPDOM,IUT0,IERR,RX,RY,MAXBUF)
              IF(IERR.NE.0) THEN
                  WRITE(IUT0,*)
                  WRITE(IUT0,*) ERMSGC
                  RETURN
              ENDIF
          ENDIF
      ENDIF
C
C      ITERATION
C
      GO TO 10
C
C
 2000 CONTINUE
      NRN = NRN-1
C
C
      RETURN
      END
