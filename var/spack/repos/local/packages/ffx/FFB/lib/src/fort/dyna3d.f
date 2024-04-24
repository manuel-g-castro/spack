C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    DYNA3D                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE DYNA3D(VISCM,ALFDYN,GAMDYN,NAVDYN,FILTER,U,V,W,
     *                  DNXI,DNYI,DNZI,SN,EXX,EYY,EZZ,CM,
     *                  NODE,IENP,JENP,NEP,ME,MEP,NE,NP,N,
     *                  IPART,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *                  CS,AML,AMM,S,UI,UIJ,SIJN,SIJ,SSIJ,
     *                  RX,RY,RZ,FX,FY,FZ,DUE,DVE,DWE,
     *                  IUT0,IERR)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION FILTER(NE),U(NP),V(NP),W(NP),
     1          DNXI(N,NE),DNYI(N,NE),DNZI(N,NE),
     2          SN  (N,NE),EXX (ME,N,N),EYY(ME,N,N),EZZ(ME,N,N),CM(NP),
     3          NODE(N,NE),IENP(MEP,NP),JENP(MEP,NP),NEP(NP)
C
      DIMENSION LDOM(NDOM),NBPDOM(NDOM),
     1          IPSLF(MBPDOM,NDOM),IPSND(MBPDOM,NDOM)
C
      DIMENSION CS  (NE),AML  (NP),AMM   (NP),S     (NP),
     1          UI(3,NP),UIJ(6,NP),SIJN(6,NP),SIJ (6,NP),SSIJ(6,NP)
C
      DIMENSION RX(N,NE),RY(N,NE),RZ(N,NE),
     1          FX(NP),FY(NP),FZ(NP),DUE(NE),DVE(NE),DWE(NE)
C
C
      CHARACTER*60 ERMSGC
     & /' ## SUBROUTINE DYNA3D: FATAL      ERROR REPORT   ; RETURNED' /
C
      DATA IDIM   / 3    /
C
      DATA CLEAR  / 0.15   /
      DATA EPS    / 1.0E-2 /
C
C
C      COMPUTE SMAGIRINSKY CONSTANT BY DYNAMIC PROCEDURE PROPOSED
C     BY GERMANO AND LILLY
C         ( 3-D CALCULATION : SINGLE WORD VERSION )
C
C
C     NOTE ; IN-DOMAIN NOR INTER-DOMAIN CYCLIC BOUNDARY CONDITIONS HAVE
C           NOT BEEN SUPPORTED YET.
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          VISCM       ; MOLECULAR VISCOSITY (USED TO CLEAR CS VALUES)
C          ALFDYN      ; RATIO OF WIDTH OF TEST-FILTERED GRID FILTER TO
C                       THAT OF GRID FILTER
C          GAMDYN      ; RATIO OF WIDTH OF TEST FILTER TO THAT OF GRID
C                       FILTER.
C          NAVDYN      ; NUMBER OF TEST-FILTER OPERATIONS APPLIED FOR
C                       AVERAGING LOCAL SMAGORINSKY CONSTANT
C          FILTER  (IE); ELEMENT GRID FILTER WIDTH
C
C          U       (IP); X-DIR. VELOCITY COMPONENT
C          V       (IP); Y-DIR. VELOCITY COMPONENT
C          W       (IP); Z-DIR. VELOCITY COMPONENT
C
C          DNXI  (I,IE); ELEMENT CENTER VALUE OF NX
C          DNYI  (I,IE); ELEMENT CENTER VALUE OF NY
C          DNZI  (I,IE); ELEMENT CENTER VALUE OF NZ
C          SN    (I,IE); INTEGRATED ELEMENT VECTOR OF N
C          EXX (IE,I,J); INTEGRATED ELEMENT MATRIX OF NX*NXT
C          EYY (IE,I,J); INTEGRATED ELEMENT MATRIX OF NY*NYT
C          EZZ (IE,I,J); INTEGRATED ELEMENT MATRIX OF NZ*NZT
C          CM      (IP); LUMPED MASS MATRIX
C
C          NODE  (I,IE); NODE NO. TABLE BASED ON ELEMENT
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
C
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
C          IUT0        ; FILE NUMBER TO REPORT ERROR OCCURENCE
C
C       (2) OUTPUT
C          CS      (IE); ELEMENT SMAGORINSKY CONSTANT
C          AML     (IP); LEAST-SQUARE AVERAGED LEONARD STRESS
C          AMM     (IP); LEAST-SQUARE AVERAGED MODELED REYNOLDS STRESS
C          S       (IP); TEST-FILTERED STRAIN VELOCITY TENSOR MAGNITUDE
C          UI    (3,IP); TEST-FILTERED VELOCITIES
C          UIJ   (6,IP); TEST-FILTERED VELOCITY CORRELATIONS
C          SIJN  (6,IP); NODAL STRAIN VELOCITY TENSOR
C          SIJ   (6,IP); TEST-FILTERED STRAIN VELOCITY TENSOR
C          SSIJ  (6,IP); TEST-FILTERED STRAIN VELOCITY TENSOR MULTIPLIED
C                       BY ITS MAGNITUDE
C
C          IERR        ; RETURN CODE TO REPORT ERROR OCCURENCE
C                   0 --- NORMAL TERMINATION
C                   1 --- A FATAL ERROR HAS OCCURED
C
C       (4) WORK
C          RX    (I,IE); HOLDS X-DIR. ELEMENT RESIDUAL
C          RY    (I,IE); HOLDS Y-DIR. ELEMENT RESIDUAL
C          RZ    (I,IE); HOLDS Z-DIR. ELEMENT RESIDUAL
C          FX      (IP); HOLDS X-DIR. NODE    RESIDUAL
C          FY      (IP); HOLDS Y-DIR. NODE    RESIDUAL
C          FZ      (IP); HOLDS Z-DIR. NODE    RESIDUAL
C          DUE     (IE); HOLDS ELEMENT VELOCITY GRADIENT
C          DVE     (IE); HOLDS ELEMENT VELOCITY GRADIENT
C          DWE     (IE); HOLDS ELEMENT VELOCITY GRADIENT
C
C
      ALF2   = ALFDYN*ALFDYN
      GAM2   = GAMDYN*GAMDYN
C
      MAXBUF = NE*N
C
C
C
C COMPUTE NODAL VALUE OF STRAIN VELOCITY TENSOR
C
C
C  (1) DIAGONAL TERMS
C
C
      DO 100 IE = 1 , NE
          DUE(IE) = DNXI(1,IE)*U(NODE(1,IE))+DNXI(5,IE)*U(NODE(5,IE))
     &             +DNXI(2,IE)*U(NODE(2,IE))+DNXI(6,IE)*U(NODE(6,IE))
     &             +DNXI(3,IE)*U(NODE(3,IE))+DNXI(7,IE)*U(NODE(7,IE))
     &             +DNXI(4,IE)*U(NODE(4,IE))+DNXI(8,IE)*U(NODE(8,IE))
C
          DVE(IE) = DNYI(1,IE)*V(NODE(1,IE))+DNYI(5,IE)*V(NODE(5,IE))
     &             +DNYI(2,IE)*V(NODE(2,IE))+DNYI(6,IE)*V(NODE(6,IE))
     &             +DNYI(3,IE)*V(NODE(3,IE))+DNYI(7,IE)*V(NODE(7,IE))
     &             +DNYI(4,IE)*V(NODE(4,IE))+DNYI(8,IE)*V(NODE(8,IE))
C
          DWE(IE) = DNZI(1,IE)*W(NODE(1,IE))+DNZI(5,IE)*W(NODE(5,IE))
     &             +DNZI(2,IE)*W(NODE(2,IE))+DNZI(6,IE)*W(NODE(6,IE))
     &             +DNZI(3,IE)*W(NODE(3,IE))+DNZI(7,IE)*W(NODE(7,IE))
     &             +DNZI(4,IE)*W(NODE(4,IE))+DNZI(8,IE)*W(NODE(8,IE))
  100 CONTINUE
C
      CALL SUPUE1(IDIM,DUE,SN,IENP,JENP,NEP,ME,MEP,NE,NP,N,
     *            FX,IUT0,IERR)
      CALL SUPUE1(IDIM,DVE,SN,IENP,JENP,NEP,ME,MEP,NE,NP,N,
     *            FY,IUT0,IERR)
      CALL SUPUE1(IDIM,DWE,SN,IENP,JENP,NEP,ME,MEP,NE,NP,N,
     *            FZ,IUT0,IERR)
C
      IF(IERR.NE.0) THEN
          WRITE(IUT0,*)
          WRITE(IUT0,*) ERMSGC
          RETURN
      ENDIF
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
      DO 110 IP = 1 , NP
          SIJN(1,IP) = CM(IP)*FX(IP)
          SIJN(2,IP) = CM(IP)*FY(IP)
          SIJN(3,IP) = CM(IP)*FZ(IP)
  110 CONTINUE
C
C
C  (2) OFF-DIAGONAL TERMS
C
C
      DO 120 IE = 1 , NE
          DUE(IE) = DNZI(1,IE)*V(NODE(1,IE))+DNYI(1,IE)*W(NODE(1,IE))
     &             +DNZI(2,IE)*V(NODE(2,IE))+DNYI(2,IE)*W(NODE(2,IE))
     &             +DNZI(3,IE)*V(NODE(3,IE))+DNYI(3,IE)*W(NODE(3,IE))
     &             +DNZI(4,IE)*V(NODE(4,IE))+DNYI(4,IE)*W(NODE(4,IE))
     &             +DNZI(5,IE)*V(NODE(5,IE))+DNYI(5,IE)*W(NODE(5,IE))
     &             +DNZI(6,IE)*V(NODE(6,IE))+DNYI(6,IE)*W(NODE(6,IE))
     &             +DNZI(7,IE)*V(NODE(7,IE))+DNYI(7,IE)*W(NODE(7,IE))
     &             +DNZI(8,IE)*V(NODE(8,IE))+DNYI(8,IE)*W(NODE(8,IE))
C
          DVE(IE) = DNXI(1,IE)*W(NODE(1,IE))+DNZI(1,IE)*U(NODE(1,IE))
     &             +DNXI(2,IE)*W(NODE(2,IE))+DNZI(2,IE)*U(NODE(2,IE))
     &             +DNXI(3,IE)*W(NODE(3,IE))+DNZI(3,IE)*U(NODE(3,IE))
     &             +DNXI(4,IE)*W(NODE(4,IE))+DNZI(4,IE)*U(NODE(4,IE))
     &             +DNXI(5,IE)*W(NODE(5,IE))+DNZI(5,IE)*U(NODE(5,IE))
     &             +DNXI(6,IE)*W(NODE(6,IE))+DNZI(6,IE)*U(NODE(6,IE))
     &             +DNXI(7,IE)*W(NODE(7,IE))+DNZI(7,IE)*U(NODE(7,IE))
     &             +DNXI(8,IE)*W(NODE(8,IE))+DNZI(8,IE)*U(NODE(8,IE))
C
          DWE(IE) = DNYI(1,IE)*U(NODE(1,IE))+DNXI(1,IE)*V(NODE(1,IE))
     &             +DNYI(2,IE)*U(NODE(2,IE))+DNXI(2,IE)*V(NODE(2,IE))
     &             +DNYI(3,IE)*U(NODE(3,IE))+DNXI(3,IE)*V(NODE(3,IE))
     &             +DNYI(4,IE)*U(NODE(4,IE))+DNXI(4,IE)*V(NODE(4,IE))
     &             +DNYI(5,IE)*U(NODE(5,IE))+DNXI(5,IE)*V(NODE(5,IE))
     &             +DNYI(6,IE)*U(NODE(6,IE))+DNXI(6,IE)*V(NODE(6,IE))
     &             +DNYI(7,IE)*U(NODE(7,IE))+DNXI(7,IE)*V(NODE(7,IE))
     &             +DNYI(8,IE)*U(NODE(8,IE))+DNXI(8,IE)*V(NODE(8,IE))
  120 CONTINUE
C
      CALL SUPUE1(IDIM,DUE,SN,IENP,JENP,NEP,ME,MEP,NE,NP,N,
     *            FX,IUT0,IERR)
      CALL SUPUE1(IDIM,DVE,SN,IENP,JENP,NEP,ME,MEP,NE,NP,N,
     *            FY,IUT0,IERR)
      CALL SUPUE1(IDIM,DWE,SN,IENP,JENP,NEP,ME,MEP,NE,NP,N,
     *            FZ,IUT0,IERR)
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
      DO 130 IP = 1 , NP
          SIJN(4,IP) = 0.5E0*CM(IP)*FX(IP)
          SIJN(5,IP) = 0.5E0*CM(IP)*FY(IP)
          SIJN(6,IP) = 0.5E0*CM(IP)*FZ(IP)
  130 CONTINUE
C
C
C
C COMPUTE CONTRACTION OF STRAIN VELOCITY TENSOR
C
C
C
      DO 200 IP = 1 , NP
          S(IP) = SQRT(2.E0*SIJN(1,IP)*SIJN(1,IP)
     &                +2.E0*SIJN(2,IP)*SIJN(2,IP)
     &                +2.E0*SIJN(3,IP)*SIJN(3,IP)
     &                +4.E0*SIJN(4,IP)*SIJN(4,IP)
     &                +4.E0*SIJN(5,IP)*SIJN(5,IP)
     &                +4.E0*SIJN(6,IP)*SIJN(6,IP))
  200 CONTINUE
C
C
C
C TEST-FILTER VELOCITY VECTOR
C
C
C
      DO 310 I = 1 , N
          DO 300 IE = 1 , NE
              RX(I,IE) = 0.E0
              RY(I,IE) = 0.E0
              RZ(I,IE) = 0.E0
  300     CONTINUE
  310 CONTINUE
C
      DO 340 J = 1 , N
          DO 320 IE = 1 , NE
              FX(IE) = U(NODE(J,IE))
              FY(IE) = V(NODE(J,IE))
              FZ(IE) = W(NODE(J,IE))
  320     CONTINUE
C
          DO 330 IE = 1 , NE
              D1=FILTER(IE)**2*(EXX(IE,1,J)+EYY(IE,1,J)+EZZ(IE,1,J))
              D2=FILTER(IE)**2*(EXX(IE,2,J)+EYY(IE,2,J)+EZZ(IE,2,J))
              D3=FILTER(IE)**2*(EXX(IE,3,J)+EYY(IE,3,J)+EZZ(IE,3,J))
              D4=FILTER(IE)**2*(EXX(IE,4,J)+EYY(IE,4,J)+EZZ(IE,4,J))
              D5=FILTER(IE)**2*(EXX(IE,5,J)+EYY(IE,5,J)+EZZ(IE,5,J))
              D6=FILTER(IE)**2*(EXX(IE,6,J)+EYY(IE,6,J)+EZZ(IE,6,J))
              D7=FILTER(IE)**2*(EXX(IE,7,J)+EYY(IE,7,J)+EZZ(IE,7,J))
              D8=FILTER(IE)**2*(EXX(IE,8,J)+EYY(IE,8,J)+EZZ(IE,8,J))
C
              RX(1,IE)=RX(1,IE)-D1*FX(IE)
              RX(2,IE)=RX(2,IE)-D2*FX(IE)
              RX(3,IE)=RX(3,IE)-D3*FX(IE)
              RX(4,IE)=RX(4,IE)-D4*FX(IE)
              RX(5,IE)=RX(5,IE)-D5*FX(IE)
              RX(6,IE)=RX(6,IE)-D6*FX(IE)
              RX(7,IE)=RX(7,IE)-D7*FX(IE)
              RX(8,IE)=RX(8,IE)-D8*FX(IE)
C
              RY(1,IE)=RY(1,IE)-D1*FY(IE)
              RY(2,IE)=RY(2,IE)-D2*FY(IE)
              RY(3,IE)=RY(3,IE)-D3*FY(IE)
              RY(4,IE)=RY(4,IE)-D4*FY(IE)
              RY(5,IE)=RY(5,IE)-D5*FY(IE)
              RY(6,IE)=RY(6,IE)-D6*FY(IE)
              RY(7,IE)=RY(7,IE)-D7*FY(IE)
              RY(8,IE)=RY(8,IE)-D8*FY(IE)
C
              RZ(1,IE)=RZ(1,IE)-D1*FZ(IE)
              RZ(2,IE)=RZ(2,IE)-D2*FZ(IE)
              RZ(3,IE)=RZ(3,IE)-D3*FZ(IE)
              RZ(4,IE)=RZ(4,IE)-D4*FZ(IE)
              RZ(5,IE)=RZ(5,IE)-D5*FZ(IE)
              RZ(6,IE)=RZ(6,IE)-D6*FZ(IE)
              RZ(7,IE)=RZ(7,IE)-D7*FZ(IE)
              RZ(8,IE)=RZ(8,IE)-D8*FZ(IE)
  330     CONTINUE
  340 CONTINUE
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
      DO 350 IP = 1 , NP
          UI(1,IP) = U(IP)+GAM2/24.E0*CM(IP)*FX(IP)
          UI(2,IP) = V(IP)+GAM2/24.E0*CM(IP)*FY(IP)
          UI(3,IP) = W(IP)+GAM2/24.E0*CM(IP)*FZ(IP)
  350 CONTINUE
C
C
C
C TEST-FILTER VELOCITY CORRELATION TENSOR
C
C
C  (1) DIAGONAL TERMS
C
C
      DO 410 I = 1 , N
          DO 400 IE = 1 , NE
              RX(I,IE) = 0.E0
              RY(I,IE) = 0.E0
              RZ(I,IE) = 0.E0
  400     CONTINUE
  410 CONTINUE
C
      DO 440 J = 1 , N
          DO 420 IE = 1 , NE
              FX(IE) = U(NODE(J,IE))*U(NODE(J,IE))
              FY(IE) = V(NODE(J,IE))*V(NODE(J,IE))
              FZ(IE) = W(NODE(J,IE))*W(NODE(J,IE))
  420     CONTINUE
C
          DO 430 IE = 1 , NE
              D1=FILTER(IE)**2*(EXX(IE,1,J)+EYY(IE,1,J)+EZZ(IE,1,J))
              D2=FILTER(IE)**2*(EXX(IE,2,J)+EYY(IE,2,J)+EZZ(IE,2,J))
              D3=FILTER(IE)**2*(EXX(IE,3,J)+EYY(IE,3,J)+EZZ(IE,3,J))
              D4=FILTER(IE)**2*(EXX(IE,4,J)+EYY(IE,4,J)+EZZ(IE,4,J))
              D5=FILTER(IE)**2*(EXX(IE,5,J)+EYY(IE,5,J)+EZZ(IE,5,J))
              D6=FILTER(IE)**2*(EXX(IE,6,J)+EYY(IE,6,J)+EZZ(IE,6,J))
              D7=FILTER(IE)**2*(EXX(IE,7,J)+EYY(IE,7,J)+EZZ(IE,7,J))
              D8=FILTER(IE)**2*(EXX(IE,8,J)+EYY(IE,8,J)+EZZ(IE,8,J))
C
              RX(1,IE)=RX(1,IE)-D1*FX(IE)
              RX(2,IE)=RX(2,IE)-D2*FX(IE)
              RX(3,IE)=RX(3,IE)-D3*FX(IE)
              RX(4,IE)=RX(4,IE)-D4*FX(IE)
              RX(5,IE)=RX(5,IE)-D5*FX(IE)
              RX(6,IE)=RX(6,IE)-D6*FX(IE)
              RX(7,IE)=RX(7,IE)-D7*FX(IE)
              RX(8,IE)=RX(8,IE)-D8*FX(IE)
C
              RY(1,IE)=RY(1,IE)-D1*FY(IE)
              RY(2,IE)=RY(2,IE)-D2*FY(IE)
              RY(3,IE)=RY(3,IE)-D3*FY(IE)
              RY(4,IE)=RY(4,IE)-D4*FY(IE)
              RY(5,IE)=RY(5,IE)-D5*FY(IE)
              RY(6,IE)=RY(6,IE)-D6*FY(IE)
              RY(7,IE)=RY(7,IE)-D7*FY(IE)
              RY(8,IE)=RY(8,IE)-D8*FY(IE)
C
              RZ(1,IE)=RZ(1,IE)-D1*FZ(IE)
              RZ(2,IE)=RZ(2,IE)-D2*FZ(IE)
              RZ(3,IE)=RZ(3,IE)-D3*FZ(IE)
              RZ(4,IE)=RZ(4,IE)-D4*FZ(IE)
              RZ(5,IE)=RZ(5,IE)-D5*FZ(IE)
              RZ(6,IE)=RZ(6,IE)-D6*FZ(IE)
              RZ(7,IE)=RZ(7,IE)-D7*FZ(IE)
              RZ(8,IE)=RZ(8,IE)-D8*FZ(IE)
  430     CONTINUE
  440 CONTINUE
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
      DO 450 IP = 1 , NP
          UIJ(1,IP) = U(IP)*U(IP)+GAM2/24.E0*CM(IP)*FX(IP)
          UIJ(2,IP) = V(IP)*V(IP)+GAM2/24.E0*CM(IP)*FY(IP)
          UIJ(3,IP) = W(IP)*W(IP)+GAM2/24.E0*CM(IP)*FZ(IP)
  450 CONTINUE
C
C
C  (2) OFF-DIAGONAL TERMS
C
C
      DO 510 I = 1 , N
          DO 500 IE = 1 , NE
              RX(I,IE) = 0.E0
              RY(I,IE) = 0.E0
              RZ(I,IE) = 0.E0
  500     CONTINUE
  510 CONTINUE
C
      DO 540 J = 1 , N
          DO 520 IE = 1 , NE
              FX(IE) = V(NODE(J,IE))*W(NODE(J,IE))
              FY(IE) = W(NODE(J,IE))*U(NODE(J,IE))
              FZ(IE) = U(NODE(J,IE))*V(NODE(J,IE))
  520     CONTINUE
C
          DO 530 IE = 1 , NE
              D1=FILTER(IE)**2*(EXX(IE,1,J)+EYY(IE,1,J)+EZZ(IE,1,J))
              D2=FILTER(IE)**2*(EXX(IE,2,J)+EYY(IE,2,J)+EZZ(IE,2,J))
              D3=FILTER(IE)**2*(EXX(IE,3,J)+EYY(IE,3,J)+EZZ(IE,3,J))
              D4=FILTER(IE)**2*(EXX(IE,4,J)+EYY(IE,4,J)+EZZ(IE,4,J))
              D5=FILTER(IE)**2*(EXX(IE,5,J)+EYY(IE,5,J)+EZZ(IE,5,J))
              D6=FILTER(IE)**2*(EXX(IE,6,J)+EYY(IE,6,J)+EZZ(IE,6,J))
              D7=FILTER(IE)**2*(EXX(IE,7,J)+EYY(IE,7,J)+EZZ(IE,7,J))
              D8=FILTER(IE)**2*(EXX(IE,8,J)+EYY(IE,8,J)+EZZ(IE,8,J))
C
              RX(1,IE)=RX(1,IE)-D1*FX(IE)
              RX(2,IE)=RX(2,IE)-D2*FX(IE)
              RX(3,IE)=RX(3,IE)-D3*FX(IE)
              RX(4,IE)=RX(4,IE)-D4*FX(IE)
              RX(5,IE)=RX(5,IE)-D5*FX(IE)
              RX(6,IE)=RX(6,IE)-D6*FX(IE)
              RX(7,IE)=RX(7,IE)-D7*FX(IE)
              RX(8,IE)=RX(8,IE)-D8*FX(IE)
C
              RY(1,IE)=RY(1,IE)-D1*FY(IE)
              RY(2,IE)=RY(2,IE)-D2*FY(IE)
              RY(3,IE)=RY(3,IE)-D3*FY(IE)
              RY(4,IE)=RY(4,IE)-D4*FY(IE)
              RY(5,IE)=RY(5,IE)-D5*FY(IE)
              RY(6,IE)=RY(6,IE)-D6*FY(IE)
              RY(7,IE)=RY(7,IE)-D7*FY(IE)
              RY(8,IE)=RY(8,IE)-D8*FY(IE)
C
              RZ(1,IE)=RZ(1,IE)-D1*FZ(IE)
              RZ(2,IE)=RZ(2,IE)-D2*FZ(IE)
              RZ(3,IE)=RZ(3,IE)-D3*FZ(IE)
              RZ(4,IE)=RZ(4,IE)-D4*FZ(IE)
              RZ(5,IE)=RZ(5,IE)-D5*FZ(IE)
              RZ(6,IE)=RZ(6,IE)-D6*FZ(IE)
              RZ(7,IE)=RZ(7,IE)-D7*FZ(IE)
              RZ(8,IE)=RZ(8,IE)-D8*FZ(IE)
  530     CONTINUE
  540 CONTINUE
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
      DO 550 IP = 1 , NP
          UIJ(4,IP) = V(IP)*W(IP)+GAM2/24.E0*CM(IP)*FX(IP)
          UIJ(5,IP) = W(IP)*U(IP)+GAM2/24.E0*CM(IP)*FY(IP)
          UIJ(6,IP) = U(IP)*V(IP)+GAM2/24.E0*CM(IP)*FZ(IP)
  550 CONTINUE
C
C
C
C TEST-FILTER STRAIN VELOCITY TENSOR
C
C
C  (1) DIAGONAL TERMS
C
C
      DO 610 I = 1 , N
          DO 600 IE = 1 , NE
              RX(I,IE) = 0.E0
              RY(I,IE) = 0.E0
              RZ(I,IE) = 0.E0
  600     CONTINUE
  610 CONTINUE
C
      DO 640 J = 1 , N
          DO 620 IE = 1 , NE
              FX(IE) = SIJN(1,NODE(J,IE))
              FY(IE) = SIJN(2,NODE(J,IE))
              FZ(IE) = SIJN(3,NODE(J,IE))
  620     CONTINUE
C
          DO 630 IE = 1 , NE
              D1=FILTER(IE)**2*(EXX(IE,1,J)+EYY(IE,1,J)+EZZ(IE,1,J))
              D2=FILTER(IE)**2*(EXX(IE,2,J)+EYY(IE,2,J)+EZZ(IE,2,J))
              D3=FILTER(IE)**2*(EXX(IE,3,J)+EYY(IE,3,J)+EZZ(IE,3,J))
              D4=FILTER(IE)**2*(EXX(IE,4,J)+EYY(IE,4,J)+EZZ(IE,4,J))
              D5=FILTER(IE)**2*(EXX(IE,5,J)+EYY(IE,5,J)+EZZ(IE,5,J))
              D6=FILTER(IE)**2*(EXX(IE,6,J)+EYY(IE,6,J)+EZZ(IE,6,J))
              D7=FILTER(IE)**2*(EXX(IE,7,J)+EYY(IE,7,J)+EZZ(IE,7,J))
              D8=FILTER(IE)**2*(EXX(IE,8,J)+EYY(IE,8,J)+EZZ(IE,8,J))
C
              RX(1,IE)=RX(1,IE)-D1*FX(IE)
              RX(2,IE)=RX(2,IE)-D2*FX(IE)
              RX(3,IE)=RX(3,IE)-D3*FX(IE)
              RX(4,IE)=RX(4,IE)-D4*FX(IE)
              RX(5,IE)=RX(5,IE)-D5*FX(IE)
              RX(6,IE)=RX(6,IE)-D6*FX(IE)
              RX(7,IE)=RX(7,IE)-D7*FX(IE)
              RX(8,IE)=RX(8,IE)-D8*FX(IE)
C
              RY(1,IE)=RY(1,IE)-D1*FY(IE)
              RY(2,IE)=RY(2,IE)-D2*FY(IE)
              RY(3,IE)=RY(3,IE)-D3*FY(IE)
              RY(4,IE)=RY(4,IE)-D4*FY(IE)
              RY(5,IE)=RY(5,IE)-D5*FY(IE)
              RY(6,IE)=RY(6,IE)-D6*FY(IE)
              RY(7,IE)=RY(7,IE)-D7*FY(IE)
              RY(8,IE)=RY(8,IE)-D8*FY(IE)
C
              RZ(1,IE)=RZ(1,IE)-D1*FZ(IE)
              RZ(2,IE)=RZ(2,IE)-D2*FZ(IE)
              RZ(3,IE)=RZ(3,IE)-D3*FZ(IE)
              RZ(4,IE)=RZ(4,IE)-D4*FZ(IE)
              RZ(5,IE)=RZ(5,IE)-D5*FZ(IE)
              RZ(6,IE)=RZ(6,IE)-D6*FZ(IE)
              RZ(7,IE)=RZ(7,IE)-D7*FZ(IE)
              RZ(8,IE)=RZ(8,IE)-D8*FZ(IE)
  630     CONTINUE
  640 CONTINUE
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
      DO 650 IP = 1 , NP
          SIJ(1,IP) = SIJN(1,IP)+GAM2/24.E0*CM(IP)*FX(IP)
          SIJ(2,IP) = SIJN(2,IP)+GAM2/24.E0*CM(IP)*FY(IP)
          SIJ(3,IP) = SIJN(3,IP)+GAM2/24.E0*CM(IP)*FZ(IP)
  650 CONTINUE
C
C
C  (2) OFF-DIAGONAL TERMS
C
C
      DO 710 I = 1 , N
          DO 700 IE = 1 , NE
              RX(I,IE) = 0.E0
              RY(I,IE) = 0.E0
              RZ(I,IE) = 0.E0
  700     CONTINUE
  710 CONTINUE
C
      DO 740 J = 1 , N
          DO 720 IE = 1 , NE
              FX(IE) = SIJN(4,NODE(J,IE))
              FY(IE) = SIJN(5,NODE(J,IE))
              FZ(IE) = SIJN(6,NODE(J,IE))
  720     CONTINUE
C
          DO 730 IE = 1 , NE
              D1=FILTER(IE)**2*(EXX(IE,1,J)+EYY(IE,1,J)+EZZ(IE,1,J))
              D2=FILTER(IE)**2*(EXX(IE,2,J)+EYY(IE,2,J)+EZZ(IE,2,J))
              D3=FILTER(IE)**2*(EXX(IE,3,J)+EYY(IE,3,J)+EZZ(IE,3,J))
              D4=FILTER(IE)**2*(EXX(IE,4,J)+EYY(IE,4,J)+EZZ(IE,4,J))
              D5=FILTER(IE)**2*(EXX(IE,5,J)+EYY(IE,5,J)+EZZ(IE,5,J))
              D6=FILTER(IE)**2*(EXX(IE,6,J)+EYY(IE,6,J)+EZZ(IE,6,J))
              D7=FILTER(IE)**2*(EXX(IE,7,J)+EYY(IE,7,J)+EZZ(IE,7,J))
              D8=FILTER(IE)**2*(EXX(IE,8,J)+EYY(IE,8,J)+EZZ(IE,8,J))
C
              RX(1,IE)=RX(1,IE)-D1*FX(IE)
              RX(2,IE)=RX(2,IE)-D2*FX(IE)
              RX(3,IE)=RX(3,IE)-D3*FX(IE)
              RX(4,IE)=RX(4,IE)-D4*FX(IE)
              RX(5,IE)=RX(5,IE)-D5*FX(IE)
              RX(6,IE)=RX(6,IE)-D6*FX(IE)
              RX(7,IE)=RX(7,IE)-D7*FX(IE)
              RX(8,IE)=RX(8,IE)-D8*FX(IE)
C
              RY(1,IE)=RY(1,IE)-D1*FY(IE)
              RY(2,IE)=RY(2,IE)-D2*FY(IE)
              RY(3,IE)=RY(3,IE)-D3*FY(IE)
              RY(4,IE)=RY(4,IE)-D4*FY(IE)
              RY(5,IE)=RY(5,IE)-D5*FY(IE)
              RY(6,IE)=RY(6,IE)-D6*FY(IE)
              RY(7,IE)=RY(7,IE)-D7*FY(IE)
              RY(8,IE)=RY(8,IE)-D8*FY(IE)
C
              RZ(1,IE)=RZ(1,IE)-D1*FZ(IE)
              RZ(2,IE)=RZ(2,IE)-D2*FZ(IE)
              RZ(3,IE)=RZ(3,IE)-D3*FZ(IE)
              RZ(4,IE)=RZ(4,IE)-D4*FZ(IE)
              RZ(5,IE)=RZ(5,IE)-D5*FZ(IE)
              RZ(6,IE)=RZ(6,IE)-D6*FZ(IE)
              RZ(7,IE)=RZ(7,IE)-D7*FZ(IE)
              RZ(8,IE)=RZ(8,IE)-D8*FZ(IE)
  730     CONTINUE
  740 CONTINUE
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
      DO 750 IP = 1 , NP
          SIJ(4,IP) = SIJN(4,IP)+GAM2/24.E0*CM(IP)*FX(IP)
          SIJ(5,IP) = SIJN(5,IP)+GAM2/24.E0*CM(IP)*FY(IP)
          SIJ(6,IP) = SIJN(6,IP)+GAM2/24.E0*CM(IP)*FZ(IP)
  750 CONTINUE
C
C
C
C TEST-FILTER STRAIN VELOCITY TENSOR MULTIPLIED BY ITS MAGNITUDE
C
C
C  (1) DIAGONAL TERMS
C
C
      DO 810 I = 1 , N
          DO 800 IE = 1 , NE
              RX(I,IE) = 0.E0
              RY(I,IE) = 0.E0
              RZ(I,IE) = 0.E0
  800     CONTINUE
  810 CONTINUE
C
      DO 840 J = 1 , N
          DO 820 IE = 1 , NE
              FX(IE) = S(NODE(J,IE))*SIJN(1,NODE(J,IE))
              FY(IE) = S(NODE(J,IE))*SIJN(2,NODE(J,IE))
              FZ(IE) = S(NODE(J,IE))*SIJN(3,NODE(J,IE))
  820     CONTINUE
C
          DO 830 IE = 1 , NE
              D1=FILTER(IE)**2*(EXX(IE,1,J)+EYY(IE,1,J)+EZZ(IE,1,J))
              D2=FILTER(IE)**2*(EXX(IE,2,J)+EYY(IE,2,J)+EZZ(IE,2,J))
              D3=FILTER(IE)**2*(EXX(IE,3,J)+EYY(IE,3,J)+EZZ(IE,3,J))
              D4=FILTER(IE)**2*(EXX(IE,4,J)+EYY(IE,4,J)+EZZ(IE,4,J))
              D5=FILTER(IE)**2*(EXX(IE,5,J)+EYY(IE,5,J)+EZZ(IE,5,J))
              D6=FILTER(IE)**2*(EXX(IE,6,J)+EYY(IE,6,J)+EZZ(IE,6,J))
              D7=FILTER(IE)**2*(EXX(IE,7,J)+EYY(IE,7,J)+EZZ(IE,7,J))
              D8=FILTER(IE)**2*(EXX(IE,8,J)+EYY(IE,8,J)+EZZ(IE,8,J))
C
              RX(1,IE)=RX(1,IE)-D1*FX(IE)
              RX(2,IE)=RX(2,IE)-D2*FX(IE)
              RX(3,IE)=RX(3,IE)-D3*FX(IE)
              RX(4,IE)=RX(4,IE)-D4*FX(IE)
              RX(5,IE)=RX(5,IE)-D5*FX(IE)
              RX(6,IE)=RX(6,IE)-D6*FX(IE)
              RX(7,IE)=RX(7,IE)-D7*FX(IE)
              RX(8,IE)=RX(8,IE)-D8*FX(IE)
C
              RY(1,IE)=RY(1,IE)-D1*FY(IE)
              RY(2,IE)=RY(2,IE)-D2*FY(IE)
              RY(3,IE)=RY(3,IE)-D3*FY(IE)
              RY(4,IE)=RY(4,IE)-D4*FY(IE)
              RY(5,IE)=RY(5,IE)-D5*FY(IE)
              RY(6,IE)=RY(6,IE)-D6*FY(IE)
              RY(7,IE)=RY(7,IE)-D7*FY(IE)
              RY(8,IE)=RY(8,IE)-D8*FY(IE)
C
              RZ(1,IE)=RZ(1,IE)-D1*FZ(IE)
              RZ(2,IE)=RZ(2,IE)-D2*FZ(IE)
              RZ(3,IE)=RZ(3,IE)-D3*FZ(IE)
              RZ(4,IE)=RZ(4,IE)-D4*FZ(IE)
              RZ(5,IE)=RZ(5,IE)-D5*FZ(IE)
              RZ(6,IE)=RZ(6,IE)-D6*FZ(IE)
              RZ(7,IE)=RZ(7,IE)-D7*FZ(IE)
              RZ(8,IE)=RZ(8,IE)-D8*FZ(IE)
  830     CONTINUE
  840 CONTINUE
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
      DO 850 IP = 1 , NP
         SSIJ(1,IP) = S(IP)*SIJN(1,IP)+GAM2/24.E0*CM(IP)*FX(IP)
         SSIJ(2,IP) = S(IP)*SIJN(2,IP)+GAM2/24.E0*CM(IP)*FY(IP)
         SSIJ(3,IP) = S(IP)*SIJN(3,IP)+GAM2/24.E0*CM(IP)*FZ(IP)
  850 CONTINUE
C
C
C  (2) OFF-DIAGONAL TERMS
C
C
      DO 910 I = 1 , N
          DO 900 IE = 1 , NE
              RX(I,IE) = 0.E0
              RY(I,IE) = 0.E0
              RZ(I,IE) = 0.E0
  900     CONTINUE
  910 CONTINUE
C
      DO 940 J = 1 , N
          DO 920 IE = 1 , NE
              FX(IE) = S(NODE(J,IE))*SIJN(4,NODE(J,IE))
              FY(IE) = S(NODE(J,IE))*SIJN(5,NODE(J,IE))
              FZ(IE) = S(NODE(J,IE))*SIJN(6,NODE(J,IE))
  920     CONTINUE
C
          DO 930 IE = 1 , NE
              D1=FILTER(IE)**2*(EXX(IE,1,J)+EYY(IE,1,J)+EZZ(IE,1,J))
              D2=FILTER(IE)**2*(EXX(IE,2,J)+EYY(IE,2,J)+EZZ(IE,2,J))
              D3=FILTER(IE)**2*(EXX(IE,3,J)+EYY(IE,3,J)+EZZ(IE,3,J))
              D4=FILTER(IE)**2*(EXX(IE,4,J)+EYY(IE,4,J)+EZZ(IE,4,J))
              D5=FILTER(IE)**2*(EXX(IE,5,J)+EYY(IE,5,J)+EZZ(IE,5,J))
              D6=FILTER(IE)**2*(EXX(IE,6,J)+EYY(IE,6,J)+EZZ(IE,6,J))
              D7=FILTER(IE)**2*(EXX(IE,7,J)+EYY(IE,7,J)+EZZ(IE,7,J))
              D8=FILTER(IE)**2*(EXX(IE,8,J)+EYY(IE,8,J)+EZZ(IE,8,J))
C
              RX(1,IE)=RX(1,IE)-D1*FX(IE)
              RX(2,IE)=RX(2,IE)-D2*FX(IE)
              RX(3,IE)=RX(3,IE)-D3*FX(IE)
              RX(4,IE)=RX(4,IE)-D4*FX(IE)
              RX(5,IE)=RX(5,IE)-D5*FX(IE)
              RX(6,IE)=RX(6,IE)-D6*FX(IE)
              RX(7,IE)=RX(7,IE)-D7*FX(IE)
              RX(8,IE)=RX(8,IE)-D8*FX(IE)
C
              RY(1,IE)=RY(1,IE)-D1*FY(IE)
              RY(2,IE)=RY(2,IE)-D2*FY(IE)
              RY(3,IE)=RY(3,IE)-D3*FY(IE)
              RY(4,IE)=RY(4,IE)-D4*FY(IE)
              RY(5,IE)=RY(5,IE)-D5*FY(IE)
              RY(6,IE)=RY(6,IE)-D6*FY(IE)
              RY(7,IE)=RY(7,IE)-D7*FY(IE)
              RY(8,IE)=RY(8,IE)-D8*FY(IE)
C
              RZ(1,IE)=RZ(1,IE)-D1*FZ(IE)
              RZ(2,IE)=RZ(2,IE)-D2*FZ(IE)
              RZ(3,IE)=RZ(3,IE)-D3*FZ(IE)
              RZ(4,IE)=RZ(4,IE)-D4*FZ(IE)
              RZ(5,IE)=RZ(5,IE)-D5*FZ(IE)
              RZ(6,IE)=RZ(6,IE)-D6*FZ(IE)
              RZ(7,IE)=RZ(7,IE)-D7*FZ(IE)
              RZ(8,IE)=RZ(8,IE)-D8*FZ(IE)
  930     CONTINUE
  940 CONTINUE
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
      DO 950 IP = 1 , NP
          SSIJ(4,IP)=S(IP)*SIJN(4,IP)+GAM2/24.E0*CM(IP)*FX(IP)
          SSIJ(5,IP)=S(IP)*SIJN(5,IP)+GAM2/24.E0*CM(IP)*FY(IP)
          SSIJ(6,IP)=S(IP)*SIJN(6,IP)+GAM2/24.E0*CM(IP)*FZ(IP)
  950 CONTINUE
C
C
C
C COMPUTE CONTRACTION OF TEST-FILTERED STRAIN VELOCITY TENSOR
C
C
C
      DO 1000 IP = 1 , NP
          S(IP) = SQRT(2.E0*SIJ(1,IP)*SIJ(1,IP)
     &                +2.E0*SIJ(2,IP)*SIJ(2,IP)
     &                +2.E0*SIJ(3,IP)*SIJ(3,IP)
     &                +4.E0*SIJ(4,IP)*SIJ(4,IP)
     &                +4.E0*SIJ(5,IP)*SIJ(5,IP)
     &                +4.E0*SIJ(6,IP)*SIJ(6,IP))
 1000 CONTINUE
C
C
C
C LEAST-SQUARE AVERAGE LEONARD STRESS TENSOR AND SGS STRESS TENSOR
C
C
C
      DO 1100 IP = 1 , NP
          UKK     = 0.333333E0*(UIJ(1,IP)-UI(1,IP)*UI(1,IP)
     &                         +UIJ(2,IP)-UI(2,IP)*UI(2,IP)
     &                         +UIJ(3,IP)-UI(3,IP)*UI(3,IP))
C
          AML(IP) = (SSIJ(1,IP)-ALF2*S(IP)*SIJ(1,IP))
     &             *( UIJ(1,IP)-UI(1,IP)*UI(1,IP)-UKK)
     &             +(SSIJ(2,IP)-ALF2*S(IP)*SIJ(2,IP))
     &             *( UIJ(2,IP)-UI(2,IP)*UI(2,IP)-UKK)
     &             +(SSIJ(3,IP)-ALF2*S(IP)*SIJ(3,IP))
     &             *( UIJ(3,IP)-UI(3,IP)*UI(3,IP)-UKK)
     &        +2.E0*(SSIJ(4,IP)-ALF2*S(IP)*SIJ(4,IP))
     &             *( UIJ(4,IP)-UI(2,IP)*UI(3,IP))
     &        +2.E0*(SSIJ(5,IP)-ALF2*S(IP)*SIJ(5,IP))
     &             *( UIJ(5,IP)-UI(3,IP)*UI(1,IP))
     &        +2.E0*(SSIJ(6,IP)-ALF2*S(IP)*SIJ(6,IP))
     &             *( UIJ(6,IP)-UI(1,IP)*UI(2,IP))
C
          AMM(IP) = (SSIJ(1,IP)-ALF2*S(IP)*SIJ(1,IP))**2
     &             +(SSIJ(2,IP)-ALF2*S(IP)*SIJ(2,IP))**2
     &             +(SSIJ(3,IP)-ALF2*S(IP)*SIJ(3,IP))**2
     &        +2.E0*(SSIJ(4,IP)-ALF2*S(IP)*SIJ(4,IP))**2
     &        +2.E0*(SSIJ(5,IP)-ALF2*S(IP)*SIJ(5,IP))**2
     &        +2.E0*(SSIJ(6,IP)-ALF2*S(IP)*SIJ(6,IP))**2
 1100 CONTINUE
C
C
C
C RECURSIVELY TEST-FILTER LEAST-SQUARE AVERAGED LEONARD STRESS
C AND SGS STRESS FOR AVERAGING PURPOSE
C
C
C
      DO 2000 IAVDYN = 1 , NAVDYN
          DO 1210 I = 1 , N
              DO 1200 IE = 1 , NE
                  RX(I,IE) = 0.E0
                  RY(I,IE) = 0.E0
 1200         CONTINUE
 1210     CONTINUE
C
          DO 1240 J = 1 , N
              DO 1220 IE = 1 , NE
                  FX(IE) = AML(NODE(J,IE))
                  FY(IE) = AMM(NODE(J,IE))
 1220         CONTINUE
C
              DO 1230 IE = 1 , NE
                  D1=FILTER(IE)**2*(EXX(IE,1,J)+EYY(IE,1,J)+EZZ(IE,1,J))
                  D2=FILTER(IE)**2*(EXX(IE,2,J)+EYY(IE,2,J)+EZZ(IE,2,J))
                  D3=FILTER(IE)**2*(EXX(IE,3,J)+EYY(IE,3,J)+EZZ(IE,3,J))
                  D4=FILTER(IE)**2*(EXX(IE,4,J)+EYY(IE,4,J)+EZZ(IE,4,J))
                  D5=FILTER(IE)**2*(EXX(IE,5,J)+EYY(IE,5,J)+EZZ(IE,5,J))
                  D6=FILTER(IE)**2*(EXX(IE,6,J)+EYY(IE,6,J)+EZZ(IE,6,J))
                  D7=FILTER(IE)**2*(EXX(IE,7,J)+EYY(IE,7,J)+EZZ(IE,7,J))
                  D8=FILTER(IE)**2*(EXX(IE,8,J)+EYY(IE,8,J)+EZZ(IE,8,J))
C
                  RX(1,IE)=RX(1,IE)-D1*FX(IE)
                  RX(2,IE)=RX(2,IE)-D2*FX(IE)
                  RX(3,IE)=RX(3,IE)-D3*FX(IE)
                  RX(4,IE)=RX(4,IE)-D4*FX(IE)
                  RX(5,IE)=RX(5,IE)-D5*FX(IE)
                  RX(6,IE)=RX(6,IE)-D6*FX(IE)
                  RX(7,IE)=RX(7,IE)-D7*FX(IE)
                  RX(8,IE)=RX(8,IE)-D8*FX(IE)
C
                  RY(1,IE)=RY(1,IE)-D1*FY(IE)
                  RY(2,IE)=RY(2,IE)-D2*FY(IE)
                  RY(3,IE)=RY(3,IE)-D3*FY(IE)
                  RY(4,IE)=RY(4,IE)-D4*FY(IE)
                  RY(5,IE)=RY(5,IE)-D5*FY(IE)
                  RY(6,IE)=RY(6,IE)-D6*FY(IE)
                  RY(7,IE)=RY(7,IE)-D7*FY(IE)
                  RY(8,IE)=RY(8,IE)-D8*FY(IE)
 1230         CONTINUE
 1240     CONTINUE
C
          CALL SUPUE2(IDIM,RX,IENP,JENP,NEP,ME,MEP,NE,NP,N,
     *                FX,IUT0,IERR)
          CALL SUPUE2(IDIM,RY,IENP,JENP,NEP,ME,MEP,NE,NP,N,
     *                FY,IUT0,IERR)
C
          IF(IERR.EQ.1) THEN
              WRITE(IUT0,*) ERMSGC
              RETURN
          ENDIF
C
          IF(IPART.GE.1) THEN
             IDUM = 2
             CALL DDCOM3(IPART,IDUM,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *                   FX,FY,FZ,NP,IUT0,IERR,RX,RY,MAXBUF)
             IF(IERR.NE.0) THEN
                 WRITE(IUT0,*)
                 WRITE(IUT0,*) ERMSGC
                 RETURN
             ENDIF
          ENDIF
C
          DO 1250 IP = 1 , NP
              AML(IP) = AML(IP)+GAM2/24.E0*CM(IP)*FX(IP)
              AMM(IP) = AMM(IP)+GAM2/24.E0*CM(IP)*FY(IP)
 1250     CONTINUE
 2000 CONTINUE
C
C
C
C COMPUTE ELEMENT SMAGORINSKY CONSTANT
C
C
C
      DO 3000 IE = 1 , NE
          AMLE = 0.125E0*(AML(NODE(1,IE))+AML(NODE(5,IE))
     &                   +AML(NODE(2,IE))+AML(NODE(6,IE))
     &                   +AML(NODE(3,IE))+AML(NODE(7,IE))
     &                   +AML(NODE(4,IE))+AML(NODE(8,IE)))
C
          AMME = 0.125E0*(AMM(NODE(1,IE))+AMM(NODE(5,IE))
     &                   +AMM(NODE(2,IE))+AMM(NODE(6,IE))
     &                   +AMM(NODE(3,IE))+AMM(NODE(7,IE))
     &                   +AMM(NODE(4,IE))+AMM(NODE(8,IE)))
C
          IF(AMLE.GT.0.E0 .AND. AMME.GT.0.E0) THEN
              CS(IE) = SQRT(0.5E0*AMLE/AMME)/FILTER(IE)
          ELSE
              CS(IE) = 0.E0
          ENDIF
 3000 CONTINUE
C
C
C
C FINALLY, CLEAR ELEMENT SMAGORINSKY CONSTANT DOMINATED BY
C THE ROUND-OFF ERRORS
C
C
C
      DO 4000 IE = 1 , NE
          SE = 0.125E0*(S(NODE(1,IE))+S(NODE(5,IE))
     &                 +S(NODE(2,IE))+S(NODE(6,IE))
     &                 +S(NODE(3,IE))+S(NODE(7,IE))
     &                 +S(NODE(4,IE))+S(NODE(8,IE)))
C
          SGS = CLEAR*CLEAR*FILTER(IE)*FILTER(IE)*SE
          IF(SGS .LT. EPS*VISCM) CS(IE) = 0.E0
 4000 CONTINUE
C
C
      RETURN
      END
