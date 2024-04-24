C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.8.1                                   C
C                                                                      C
C  SUB ROUTINE : DYNA3D                                                C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C CONTACT ADDRESS : IIS, THE UNIVERSITY OF TOKYO, CISS                 C
C                                                                      C
C THERMO-FLUID ANALYSIS SOLVERS FOR LARGE-SCALE-ASSEMBLY               C
C                                                                      C
C  PERFORMANCE OPTIMIZATION                                            C
C                                                                      C
C                                       MODIFIED BY RIST               C
C======================================================================C
      SUBROUTINE DYNA3D(MCOLOR,MCPART,NCOLOR,NCPART,LLOOP,
     *                  VISC,ALFDYN,GAMDYN,NAVDYN,FILTER,U,V,W,
     *                  DNXI,DNYI,DNZI,SN,CM,
     *                  NODE,ME,MELM,NE,NP,N1,N2,NEX,
     *                  IPART,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *                  CS,AML,AMM,S,UI,UIJ,SIJN,SIJ,SSIJ,
     *                  RX,RY,RZ,FX,FY,FZ,
     *                  WRK00,WRK01,WRK02,WRK03, 
     *                  IVOF,NEFLD2,LEFLD2,LEFIX,
     *                  EAP3,NODP,IENP,MEP,MP,NEP,
     *                  IUT0,IERR)
C
      IMPLICIT NONE
C
      INTEGER*4 MCOLOR,MCPART
      INTEGER*4 NCOLOR(4),NCPART(MCOLOR,4),LLOOP(MCPART,MCOLOR,4)
      INTEGER*4 IPE,K
C
      REAL*4    VISC(NE),ALFDYN,GAMDYN
      INTEGER*4 NAVDYN,ME,MELM,NE,NP,N1,N2,NEX(8),MEP,MP,IUT0,IERR
      INTEGER*4 NODE(N2,NE)
C
      REAL*4    FILTER(NE),U(NP+1),V(NP+1),W(NP+1),
     *          DNXI(N1,NE),DNYI(N1,NE),DNZI(N1,NE),
     *          SN(N1,NE),CM(NP)
C
      INTEGER*4 IPART,NDOM,MBPDOM,
     *          LDOM(NDOM),NBPDOM(NDOM),
     *          IPSLF(MBPDOM,NDOM),IPSND(MBPDOM,NDOM)
C
      REAL*4    CS(NE),AML(MP),AMM(MP),S(MP),
     *          UI(3,MP),UIJ(6,MP),SIJN(6,MP),SIJ (6,MP),SSIJ(6,MP)
C
      REAL*4    RX(N1,NE),RY(N1,NE),RZ(N1,NE),
     *          FX(NP),FY(NP),FZ(NP)
C
      REAL*4    EAP3(6,N2,MEP,NP)
      INTEGER*4 NODP(N2,MEP,NP),IENP(MEP,MP),NEP(MP)
C
C
      CHARACTER*60 ERMSGC
     & /' ## SUBROUTINE DYNA3D: FATAL      ERROR REPORT   ; RETURNED' /
C
      REAL*4 CLEAR,EPS
      DATA CLEAR  / 0.15   /
      DATA EPS    / 1.0E-2 /
C
      INTEGER*4 ITIMEI,ISTEP,JPRESS
      REAL*4    WRK00(NP),WRK01(NP),WRK02(NP),WRK03(NP)
C      
      INTEGER*4 IP,IE,IB,I,J,IELM,ICOLOR,ICPART,IES,IEE,IDUM,IAVDYN,
     *          IELM1,IELM2,IELM3,IELM4,IELM5,IELM6,IELM7,IELM8,
     *          MAXBUF,IP1,NN
      REAL*4    ALF2,GAM2,DUEIE,DVEIE,DWEIE,FXIE,FYIE,FZIE,UKK,
     *          AMLE,AMME,SE,SGS,
     *          D1,RX1,RY1,RZ1
C
C     [INPUT:VOF]
      INTEGER*4 IVOF,NEFLD2,LEFLD2(NEFLD2),LEFIX(NE)
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
C          VISC        ; MOLECULAR VISCOSITY (USED TO CLEAR CS VALUES)
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
C >>>> ADDED VARIABLES BY RIST FOR PERFORMANCE OPTIMIZATION <<<<
C       INPUT
C
C
      IP=NP+1
      U  (IP)=0.0E0
      V  (IP)=0.0E0
      W  (IP)=0.0E0
      AMM(IP)=0.0E0
      AML(IP)=0.0E0
      DO I=1,6
          SIJN(I,IP)=0.0E0
      ENDDO
C
      ALF2   = ALFDYN*ALFDYN
      GAM2   = GAMDYN*GAMDYN
C
      MAXBUF = NE*N2
C
      DO 5000 IE=1,NE
         LEFIX(IE)=0
 5000 CONTINUE
C
      IF (IVOF.EQ.1) THEN
!ocl norecurrence(LEFIX)
         DO 5100 IB=1,NEFLD2
            LEFIX(LEFLD2(IB))=1
 5100    CONTINUE
      ENDIF
C
C
C
C COMPUTE NODAL VALUE OF STRAIN VELOCITY TENSOR
C
C
C  (1) DIAGONAL TERMS
C
C
      DO 100 IP = 1 , NP
          FX(IP) = 0.E0
          FY(IP) = 0.E0
          FZ(IP) = 0.E0
  100 CONTINUE
C
      DO 220  ICOLOR=1,NCOLOR(4)
!ocl norecurrence(FX,FY,FZ)
      DO 210 ICPART=1,NCPART(ICOLOR,4)
          IES=LLOOP(ICPART  ,ICOLOR,4)
          IEE=LLOOP(ICPART+1,ICOLOR,4)-1
!ocl nosimd
!ocl noswp
!CDIR NODEP
*POPTION INDEP(FX,FY,FZ)
          DO 200 IE = IES , IEE
              IF (LEFIX(IE).EQ.1) GOTO 200
              DUEIE =  DNXI(1,IE)*U(NODE(1,IE))+DNXI(5,IE)*U(NODE(5,IE))
     &                +DNXI(2,IE)*U(NODE(2,IE))+DNXI(6,IE)*U(NODE(6,IE))
     &                +DNXI(3,IE)*U(NODE(3,IE))+DNXI(7,IE)*U(NODE(7,IE))
     &                +DNXI(4,IE)*U(NODE(4,IE))+DNXI(8,IE)*U(NODE(8,IE))
C
              DVEIE =  DNYI(1,IE)*V(NODE(1,IE))+DNYI(5,IE)*V(NODE(5,IE))
     &                +DNYI(2,IE)*V(NODE(2,IE))+DNYI(6,IE)*V(NODE(6,IE))
     &                +DNYI(3,IE)*V(NODE(3,IE))+DNYI(7,IE)*V(NODE(7,IE))
     &                +DNYI(4,IE)*V(NODE(4,IE))+DNYI(8,IE)*V(NODE(8,IE))
C
              DWEIE =  DNZI(1,IE)*W(NODE(1,IE))+DNZI(5,IE)*W(NODE(5,IE))
     &                +DNZI(2,IE)*W(NODE(2,IE))+DNZI(6,IE)*W(NODE(6,IE))
     &                +DNZI(3,IE)*W(NODE(3,IE))+DNZI(7,IE)*W(NODE(7,IE))
     &                +DNZI(4,IE)*W(NODE(4,IE))+DNZI(8,IE)*W(NODE(8,IE))
C          
              FX(NODE(1,IE)) = FX(NODE(1,IE))+DUEIE*SN(1,IE)
              FX(NODE(2,IE)) = FX(NODE(2,IE))+DUEIE*SN(2,IE)
              FX(NODE(3,IE)) = FX(NODE(3,IE))+DUEIE*SN(3,IE)
              FX(NODE(4,IE)) = FX(NODE(4,IE))+DUEIE*SN(4,IE)
              FX(NODE(5,IE)) = FX(NODE(5,IE))+DUEIE*SN(5,IE)
              FX(NODE(6,IE)) = FX(NODE(6,IE))+DUEIE*SN(6,IE)
              FX(NODE(7,IE)) = FX(NODE(7,IE))+DUEIE*SN(7,IE)
              FX(NODE(8,IE)) = FX(NODE(8,IE))+DUEIE*SN(8,IE)
C
              FY(NODE(1,IE)) = FY(NODE(1,IE))+DVEIE*SN(1,IE)
              FY(NODE(2,IE)) = FY(NODE(2,IE))+DVEIE*SN(2,IE)
              FY(NODE(3,IE)) = FY(NODE(3,IE))+DVEIE*SN(3,IE)
              FY(NODE(4,IE)) = FY(NODE(4,IE))+DVEIE*SN(4,IE)
              FY(NODE(5,IE)) = FY(NODE(5,IE))+DVEIE*SN(5,IE)
              FY(NODE(6,IE)) = FY(NODE(6,IE))+DVEIE*SN(6,IE)
              FY(NODE(7,IE)) = FY(NODE(7,IE))+DVEIE*SN(7,IE)
              FY(NODE(8,IE)) = FY(NODE(8,IE))+DVEIE*SN(8,IE)
C
              FZ(NODE(1,IE)) = FZ(NODE(1,IE))+DWEIE*SN(1,IE)
              FZ(NODE(2,IE)) = FZ(NODE(2,IE))+DWEIE*SN(2,IE)
              FZ(NODE(3,IE)) = FZ(NODE(3,IE))+DWEIE*SN(3,IE)
              FZ(NODE(4,IE)) = FZ(NODE(4,IE))+DWEIE*SN(4,IE)
              FZ(NODE(5,IE)) = FZ(NODE(5,IE))+DWEIE*SN(5,IE)
              FZ(NODE(6,IE)) = FZ(NODE(6,IE))+DWEIE*SN(6,IE)
              FZ(NODE(7,IE)) = FZ(NODE(7,IE))+DWEIE*SN(7,IE)
              FZ(NODE(8,IE)) = FZ(NODE(8,IE))+DWEIE*SN(8,IE)
  200     CONTINUE
  210 CONTINUE
  220 CONTINUE
C
C
      IDUM = 3
      CALL DDCOMX(IPART,IDUM,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *            FX,FY,FZ,NP,IUT0,IERR,RX,RY,MAXBUF)
      IF(IERR.NE.0) THEN
          WRITE(IUT0,*)
          WRITE(IUT0,*) ERMSGC
          RETURN
      ENDIF
C
C
      DO 300 IP = 1 , NP
          SIJN(1,IP) = CM(IP)*FX(IP)
          SIJN(2,IP) = CM(IP)*FY(IP)
          SIJN(3,IP) = CM(IP)*FZ(IP)
  300 CONTINUE
C
C
C  (2) OFF-DIAGONAL TERMS
C
C
      DO 400 IP = 1 , NP
          FX(IP) = 0.E0
          FY(IP) = 0.E0
          FZ(IP) = 0.E0
  400 CONTINUE
C
      DO 520  ICOLOR=1,NCOLOR(4)
!ocl norecurrence(FX,FY,FZ)
      DO 510 ICPART=1,NCPART(ICOLOR,4)
          IES=LLOOP(ICPART  ,ICOLOR,4)
          IEE=LLOOP(ICPART+1,ICOLOR,4)-1
!ocl nosimd
!ocl noswp
!CDIR NODEP
*POPTION INDEP(FX,FY,FZ)
          DO 500 IE = IES , IEE
              IF (LEFIX(IE).EQ.1) GOTO 500
              DUEIE  = DNZI(1,IE)*V(NODE(1,IE))+DNYI(1,IE)*W(NODE(1,IE))
     &                +DNZI(2,IE)*V(NODE(2,IE))+DNYI(2,IE)*W(NODE(2,IE))
     &                +DNZI(3,IE)*V(NODE(3,IE))+DNYI(3,IE)*W(NODE(3,IE))
     &                +DNZI(4,IE)*V(NODE(4,IE))+DNYI(4,IE)*W(NODE(4,IE))
     &                +DNZI(5,IE)*V(NODE(5,IE))+DNYI(5,IE)*W(NODE(5,IE))
     &                +DNZI(6,IE)*V(NODE(6,IE))+DNYI(6,IE)*W(NODE(6,IE))
     &                +DNZI(7,IE)*V(NODE(7,IE))+DNYI(7,IE)*W(NODE(7,IE))
     &                +DNZI(8,IE)*V(NODE(8,IE))+DNYI(8,IE)*W(NODE(8,IE))
C
              DVEIE  = DNXI(1,IE)*W(NODE(1,IE))+DNZI(1,IE)*U(NODE(1,IE))
     &                +DNXI(2,IE)*W(NODE(2,IE))+DNZI(2,IE)*U(NODE(2,IE))
     &                +DNXI(3,IE)*W(NODE(3,IE))+DNZI(3,IE)*U(NODE(3,IE))
     &                +DNXI(4,IE)*W(NODE(4,IE))+DNZI(4,IE)*U(NODE(4,IE))
     &                +DNXI(5,IE)*W(NODE(5,IE))+DNZI(5,IE)*U(NODE(5,IE))
     &                +DNXI(6,IE)*W(NODE(6,IE))+DNZI(6,IE)*U(NODE(6,IE))
     &                +DNXI(7,IE)*W(NODE(7,IE))+DNZI(7,IE)*U(NODE(7,IE))
     &                +DNXI(8,IE)*W(NODE(8,IE))+DNZI(8,IE)*U(NODE(8,IE))
C
              DWEIE  = DNYI(1,IE)*U(NODE(1,IE))+DNXI(1,IE)*V(NODE(1,IE))
     &                +DNYI(2,IE)*U(NODE(2,IE))+DNXI(2,IE)*V(NODE(2,IE))
     &                +DNYI(3,IE)*U(NODE(3,IE))+DNXI(3,IE)*V(NODE(3,IE))
     &                +DNYI(4,IE)*U(NODE(4,IE))+DNXI(4,IE)*V(NODE(4,IE))
     &                +DNYI(5,IE)*U(NODE(5,IE))+DNXI(5,IE)*V(NODE(5,IE))
     &                +DNYI(6,IE)*U(NODE(6,IE))+DNXI(6,IE)*V(NODE(6,IE))
     &                +DNYI(7,IE)*U(NODE(7,IE))+DNXI(7,IE)*V(NODE(7,IE))
     &                +DNYI(8,IE)*U(NODE(8,IE))+DNXI(8,IE)*V(NODE(8,IE))
              FX(NODE(1,IE)) = FX(NODE(1,IE))+DUEIE*SN(1,IE)
              FX(NODE(2,IE)) = FX(NODE(2,IE))+DUEIE*SN(2,IE)
              FX(NODE(3,IE)) = FX(NODE(3,IE))+DUEIE*SN(3,IE)
              FX(NODE(4,IE)) = FX(NODE(4,IE))+DUEIE*SN(4,IE)
              FX(NODE(5,IE)) = FX(NODE(5,IE))+DUEIE*SN(5,IE)
              FX(NODE(6,IE)) = FX(NODE(6,IE))+DUEIE*SN(6,IE)
              FX(NODE(7,IE)) = FX(NODE(7,IE))+DUEIE*SN(7,IE)
              FX(NODE(8,IE)) = FX(NODE(8,IE))+DUEIE*SN(8,IE)
C     
              FY(NODE(1,IE)) = FY(NODE(1,IE))+DVEIE*SN(1,IE)
              FY(NODE(2,IE)) = FY(NODE(2,IE))+DVEIE*SN(2,IE)
              FY(NODE(3,IE)) = FY(NODE(3,IE))+DVEIE*SN(3,IE)
              FY(NODE(4,IE)) = FY(NODE(4,IE))+DVEIE*SN(4,IE)
              FY(NODE(5,IE)) = FY(NODE(5,IE))+DVEIE*SN(5,IE)
              FY(NODE(6,IE)) = FY(NODE(6,IE))+DVEIE*SN(6,IE)
              FY(NODE(7,IE)) = FY(NODE(7,IE))+DVEIE*SN(7,IE)
              FY(NODE(8,IE)) = FY(NODE(8,IE))+DVEIE*SN(8,IE)
C     
              FZ(NODE(1,IE)) = FZ(NODE(1,IE))+DWEIE*SN(1,IE)
              FZ(NODE(2,IE)) = FZ(NODE(2,IE))+DWEIE*SN(2,IE)
              FZ(NODE(3,IE)) = FZ(NODE(3,IE))+DWEIE*SN(3,IE)
              FZ(NODE(4,IE)) = FZ(NODE(4,IE))+DWEIE*SN(4,IE)
              FZ(NODE(5,IE)) = FZ(NODE(5,IE))+DWEIE*SN(5,IE)
              FZ(NODE(6,IE)) = FZ(NODE(6,IE))+DWEIE*SN(6,IE)
              FZ(NODE(7,IE)) = FZ(NODE(7,IE))+DWEIE*SN(7,IE)
              FZ(NODE(8,IE)) = FZ(NODE(8,IE))+DWEIE*SN(8,IE)
  500     CONTINUE
  510 CONTINUE
  520 CONTINUE
C
C
      IDUM = 3
      CALL DDCOMX(IPART,IDUM,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *            FX,FY,FZ,NP,IUT0,IERR,RX,RY,MAXBUF)
      IF(IERR.NE.0) THEN
          WRITE(IUT0,*)
          WRITE(IUT0,*) ERMSGC
          RETURN
      ENDIF
C
C
      DO 600 IP = 1 , NP
          SIJN(4,IP) = 0.5E0*CM(IP)*FX(IP)
          SIJN(5,IP) = 0.5E0*CM(IP)*FY(IP)
          SIJN(6,IP) = 0.5E0*CM(IP)*FZ(IP)
  600 CONTINUE
C
C
C
C COMPUTE CONTRACTION OF STRAIN VELOCITY TENSOR
C
C
C
      DO 700 IP = 1 , NP
          S(IP) = SQRT(2.E0*SIJN(1,IP)*SIJN(1,IP)
     &                +2.E0*SIJN(2,IP)*SIJN(2,IP)
     &                +2.E0*SIJN(3,IP)*SIJN(3,IP)
     &                +4.E0*SIJN(4,IP)*SIJN(4,IP)
     &                +4.E0*SIJN(5,IP)*SIJN(5,IP)
     &                +4.E0*SIJN(6,IP)*SIJN(6,IP))
  700 CONTINUE
C
C
C
C TEST-FILTER VELOCITY VECTOR
C
C
C
      DO 810 I = 1 , N2
          DO 800 IE = 1 , NE
              RX(I,IE) = 0.E0
              RY(I,IE) = 0.E0
              RZ(I,IE) = 0.E0
  800     CONTINUE
  810 CONTINUE
C
      DO 1020 IP=1,NP
          RX1 = 0.E0
          RY1 = 0.E0
          RZ1 = 0.E0
          DO 1010 IPE=1,NEP(IP)
              IE =IENP(IPE,IP)
              IF (LEFIX(IE).EQ.1) GOTO 1010
              NN = NEX(8)
C
              DO 1000 K=1,NN
                  D1=FILTER(IE)**2*(EAP3(1,K,IPE,IP)
     *                            + EAP3(2,K,IPE,IP)
     *                            + EAP3(3,K,IPE,IP))
C
                  IP1=NODP(K,IPE,IP)
                  RX1=RX1-D1*U(IP1)
                  RY1=RY1-D1*V(IP1)
                  RZ1=RZ1-D1*W(IP1)
 1000         CONTINUE
C     
 1010     CONTINUE
C
          FX(IP) = RX1
          FY(IP) = RY1
          FZ(IP) = RZ1
 1020 CONTINUE
C
C
      IDUM = 3
      CALL DDCOMX(IPART,IDUM,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *           FX,FY,FZ,NP,IUT0,IERR,RX,RY,MAXBUF)
      IF(IERR.NE.0) THEN
          WRITE(IUT0,*)
          WRITE(IUT0,*) ERMSGC
          RETURN
      ENDIF
C
C
      DO 1100 IP = 1 , NP
          UI(1,IP) = U(IP)+GAM2/24.E0*CM(IP)*FX(IP)
          UI(2,IP) = V(IP)+GAM2/24.E0*CM(IP)*FY(IP)
          UI(3,IP) = W(IP)+GAM2/24.E0*CM(IP)*FZ(IP)
 1100 CONTINUE
C
C
C
C TEST-FILTER VELOCITY CORRELATION TENSOR
C
C
C  (1) DIAGONAL TERMS
C
C
      DO 1210 I = 1 , N2
          DO 1200 IE = 1 , NE
              RX(I,IE) = 0.E0
              RY(I,IE) = 0.E0
              RZ(I,IE) = 0.E0
 1200     CONTINUE
 1210 CONTINUE
C
      DO 1300 IP = 1 , NP
          FX(IP) = 0.E0
          FY(IP) = 0.E0
          FZ(IP) = 0.E0
 1300 CONTINUE
C
      DO 1430 IP=1,NP
          RX1 = 0.E0
          RY1 = 0.E0
          RZ1 = 0.E0
          DO 1420 IPE=1,NEP(IP)
              IE =IENP(IPE,IP)
              IF (LEFIX(IE).EQ.1) GOTO 1420
              NN = NEX(8)
C
              DO 1410 K=1,NN
                  D1=FILTER(IE)**2*(EAP3(1,K,IPE,IP)
     *                            + EAP3(2,K,IPE,IP)
     *                            + EAP3(3,K,IPE,IP))
C
                  IP1  = NODP(K,IPE,IP)
                  FXIE = U(IP1)*U(IP1)
                  FYIE = V(IP1)*V(IP1)
                  FZIE = W(IP1)*W(IP1)
C
                  RX1=RX1-D1*FXIE
                  RY1=RY1-D1*FYIE
                  RZ1=RZ1-D1*FZIE
 1410         CONTINUE
C
 1420     CONTINUE
C
          FX(IP) = RX1
          FY(IP) = RY1
          FZ(IP) = RZ1
 1430 CONTINUE
C
C
      IDUM = 3
      CALL DDCOMX(IPART,IDUM,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *            FX,FY,FZ,NP,IUT0,IERR,RX,RY,MAXBUF)
      IF(IERR.NE.0) THEN
          WRITE(IUT0,*)
          WRITE(IUT0,*) ERMSGC
          RETURN
      ENDIF
C
C
      DO 1500 IP = 1 , NP
          UIJ(1,IP) = U(IP)*U(IP)+GAM2/24.E0*CM(IP)*FX(IP)
          UIJ(2,IP) = V(IP)*V(IP)+GAM2/24.E0*CM(IP)*FY(IP)
          UIJ(3,IP) = W(IP)*W(IP)+GAM2/24.E0*CM(IP)*FZ(IP)
 1500 CONTINUE
C
C
C  (2) OFF-DIAGONAL TERMS
C
C
      DO 1610 I = 1 , N2
          DO 1600 IE = 1 , NE
              RX(I,IE) = 0.E0
              RY(I,IE) = 0.E0
              RZ(I,IE) = 0.E0
 1600     CONTINUE
 1610 CONTINUE
C
      DO 1700 IP = 1 , NP
          FX(IP) = 0.E0
          FY(IP) = 0.E0
          FZ(IP) = 0.E0
 1700 CONTINUE
C
      DO 1830 IP=1,NP
          RX1 = 0.E0
          RY1 = 0.E0
          RZ1 = 0.E0
          DO 1820 IPE=1,NEP(IP)
              IE =IENP(IPE,IP)
              IF (LEFIX(IE).EQ.1) GOTO 1820
              NN = NEX(8)
C
              DO 1810 K=1,NN
                  D1=FILTER(IE)**2*(EAP3(1,K,IPE,IP)
     *                            + EAP3(2,K,IPE,IP)
     *                            + EAP3(3,K,IPE,IP))
C                  
                  IP1  = NODP(K,IPE,IP)
                  FXIE = V(IP1)*W(IP1)
                  FYIE = W(IP1)*U(IP1)
                  FZIE = U(IP1)*V(IP1)
C
                  RX1=RX1-D1*FXIE
                  RY1=RY1-D1*FYIE
                  RZ1=RZ1-D1*FZIE
 1810         CONTINUE
C     
 1820     CONTINUE
C
          FX(IP) = RX1
          FY(IP) = RY1
          FZ(IP) = RZ1
 1830 CONTINUE
C
C
      IDUM = 3
      CALL DDCOMX(IPART,IDUM,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *            FX,FY,FZ,NP,IUT0,IERR,RX,RY,MAXBUF)
      IF(IERR.NE.0) THEN
          WRITE(IUT0,*)
          WRITE(IUT0,*) ERMSGC
          RETURN
      ENDIF
C
C
      DO 1900 IP = 1 , NP
          UIJ(4,IP) = V(IP)*W(IP)+GAM2/24.E0*CM(IP)*FX(IP)
          UIJ(5,IP) = W(IP)*U(IP)+GAM2/24.E0*CM(IP)*FY(IP)
          UIJ(6,IP) = U(IP)*V(IP)+GAM2/24.E0*CM(IP)*FZ(IP)
 1900 CONTINUE
C
C
C
C TEST-FILTER STRAIN VELOCITY TENSOR
C
C
C  (1) DIAGONAL TERMS
C
C
      DO 2010 I = 1 , N2
          DO 2000 IE = 1 , NE
              RX(I,IE) = 0.E0
              RY(I,IE) = 0.E0
              RZ(I,IE) = 0.E0
 2000     CONTINUE
 2010 CONTINUE
C
      DO 2100 IP = 1 , NP
          FX(IP) = 0.E0
          FY(IP) = 0.E0
          FZ(IP) = 0.E0
 2100 CONTINUE
C
      DO 2230 IP=1,NP
          RX1 = 0.E0
          RY1 = 0.E0
          RZ1 = 0.E0
          DO 2220 IPE=1,NEP(IP)
              IE =IENP(IPE,IP)
              IF (LEFIX(IE).EQ.1) GOTO 2220
              NN = NEX(8)
C
              DO 2210 K=1,NN
                  D1=FILTER(IE)**2*(EAP3(1,K,IPE,IP)
     *                            + EAP3(2,K,IPE,IP)
     *                            + EAP3(3,K,IPE,IP))
C
                  IP1  = NODP(K,IPE,IP)
                  FXIE = SIJN(1,IP1)
                  FYIE = SIJN(2,IP1)
                  FZIE = SIJN(3,IP1)
C
                  RX1=RX1-D1*FXIE
                  RY1=RY1-D1*FYIE
                  RZ1=RZ1-D1*FZIE
 2210         CONTINUE
C     
 2220     CONTINUE
C
          FX(IP) = RX1
          FY(IP) = RY1
          FZ(IP) = RZ1
 2230 CONTINUE
C
C
      IDUM = 3
      CALL DDCOMX(IPART,IDUM,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *            FX,FY,FZ,NP,IUT0,IERR,RX,RY,MAXBUF)
      IF(IERR.NE.0) THEN
          WRITE(IUT0,*)
          WRITE(IUT0,*) ERMSGC
          RETURN
      ENDIF
C
C
      DO 2300 IP = 1 , NP
          SIJ(1,IP) = SIJN(1,IP)+GAM2/24.E0*CM(IP)*FX(IP)
          SIJ(2,IP) = SIJN(2,IP)+GAM2/24.E0*CM(IP)*FY(IP)
          SIJ(3,IP) = SIJN(3,IP)+GAM2/24.E0*CM(IP)*FZ(IP)
 2300 CONTINUE
C
C
C  (2) OFF-DIAGONAL TERMS
C
C
      DO 2410 I = 1 , N2
          DO 2400 IE = 1 , NE
              RX(I,IE) = 0.E0
              RY(I,IE) = 0.E0
              RZ(I,IE) = 0.E0
 2400     CONTINUE
 2410 CONTINUE
C
      DO 2500 IP = 1 , NP
          FX(IP) = 0.E0
          FY(IP) = 0.E0
          FZ(IP) = 0.E0
 2500 CONTINUE
C
      DO 2630 IP=1,NP
          DO 2620 IPE=1,NEP(IP)
              IE =IENP(IPE,IP)
              IF (LEFIX(IE).EQ.1) GOTO 2620
              RX1 = 0.E0
              RY1 = 0.E0
              RZ1 = 0.E0
              NN = NEX(8)
C
              DO 2610 K=1,NN
                  D1=FILTER(IE)**2*(EAP3(1,K,IPE,IP)
     *                            + EAP3(2,K,IPE,IP)
     *                            + EAP3(3,K,IPE,IP))
C
                  IP1  = NODP(K,IPE,IP)
                  FXIE = SIJN(4,IP1)
                  FYIE = SIJN(5,IP1)
                  FZIE = SIJN(6,IP1)
C
                  RX1=RX1-D1*FXIE
                  RY1=RY1-D1*FYIE
                  RZ1=RZ1-D1*FZIE
 2610         CONTINUE
C
 2620     CONTINUE
C
          FX(IP) = RX1
          FY(IP) = RY1
          FZ(IP) = RZ1
 2630 CONTINUE
C
C
      IDUM = 3
      CALL DDCOMX(IPART,IDUM,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *            FX,FY,FZ,NP,IUT0,IERR,RX,RY,MAXBUF)
      IF(IERR.NE.0) THEN
          WRITE(IUT0,*)
          WRITE(IUT0,*) ERMSGC
          RETURN
      ENDIF
C
C
      DO 2700 IP = 1 , NP
          SIJ(4,IP) = SIJN(4,IP)+GAM2/24.E0*CM(IP)*FX(IP)
          SIJ(5,IP) = SIJN(5,IP)+GAM2/24.E0*CM(IP)*FY(IP)
          SIJ(6,IP) = SIJN(6,IP)+GAM2/24.E0*CM(IP)*FZ(IP)
 2700 CONTINUE
C
C
C
C TEST-FILTER STRAIN VELOCITY TENSOR MULTIPLIED BY ITS MAGNITUDE
C
C
C  (1) DIAGONAL TERMS
C
C
      DO 2810 I = 1 , N2
          DO 2800 IE = 1 , NE
              RX(I,IE) = 0.E0
              RY(I,IE) = 0.E0
              RZ(I,IE) = 0.E0
 2800     CONTINUE
 2810 CONTINUE
C
      DO 3030 IP=1,NP
          RX1 = 0.E0
          RY1 = 0.E0
          RZ1 = 0.E0
C
          DO 3020 IPE=1,NEP(IP)
              IE =IENP(IPE,IP)
              IF (LEFIX(IE).EQ.1) GOTO 3020
              NN = NEX(8)
C
              DO 3010 K=1,NN
                  D1=FILTER(IE)**2*(EAP3(1,K,IPE,IP)
     *                            + EAP3(2,K,IPE,IP)
     *                            + EAP3(3,K,IPE,IP))
C
                  IP1  = NODP(K,IPE,IP)
                  FXIE = S(IP)*SIJN(1,IP1)
                  FYIE = S(IP)*SIJN(2,IP1)
                  FZIE = S(IP)*SIJN(3,IP1)
C
                  RX1=RX1-D1*FXIE
                  RY1=RY1-D1*FYIE
                  RZ1=RZ1-D1*FZIE
 3010         CONTINUE
C
 3020     CONTINUE
C
          FX(IP) = RX1
          FY(IP) = RY1
          FZ(IP) = RZ1
 3030 CONTINUE
C
C
      IDUM = 3
      CALL DDCOMX(IPART,IDUM,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *            FX,FY,FZ,NP,IUT0,IERR,RX,RY,MAXBUF)
      IF(IERR.NE.0) THEN
          WRITE(IUT0,*)
          WRITE(IUT0,*) ERMSGC
          RETURN
      ENDIF
C
C
      DO 3100 IP = 1 , NP
         SSIJ(1,IP) = S(IP)*SIJN(1,IP)+GAM2/24.E0*CM(IP)*FX(IP)
         SSIJ(2,IP) = S(IP)*SIJN(2,IP)+GAM2/24.E0*CM(IP)*FY(IP)
         SSIJ(3,IP) = S(IP)*SIJN(3,IP)+GAM2/24.E0*CM(IP)*FZ(IP)
 3100 CONTINUE
C
C
C  (2) OFF-DIAGONAL TERMS
C
C
      DO 3210 I = 1 , N2
          DO 3200 IE = 1 , NE
              RX(I,IE) = 0.E0
              RY(I,IE) = 0.E0
              RZ(I,IE) = 0.E0
 3200     CONTINUE
 3210 CONTINUE
C
      DO 3300 IP = 1 , NP
          FX(IP) = 0.E0
          FY(IP) = 0.E0
          FZ(IP) = 0.E0
 3300 CONTINUE
C
      DO 3430 IP=1,NP
          RX1 = 0.E0
          RY1 = 0.E0
          RZ1 = 0.E0
          DO 3420 IPE=1,NEP(IP)
              IE =IENP(IPE,IP)
              IF (LEFIX(IE).EQ.1) GOTO 3420
              NN = NEX(8)
C
              DO 3410 K=1,NN
                  D1=FILTER(IE)**2*(EAP3(1,K,IPE,IP)
     *                            + EAP3(2,K,IPE,IP)
     *                            + EAP3(3,K,IPE,IP))
C
                  IP1  = NODP(K,IPE,IP)
                  FXIE = S(IP)*SIJN(4,IP1)
                  FYIE = S(IP)*SIJN(5,IP1)
                  FZIE = S(IP)*SIJN(6,IP1)
C                  
                  RX1=RX1-D1*FXIE
                  RY1=RY1-D1*FYIE
                  RZ1=RZ1-D1*FZIE
 3410         CONTINUE
C
 3420     CONTINUE
          FX(IP) = RX1
          FY(IP) = RY1
          FZ(IP) = RZ1
 3430 CONTINUE
C
C
      IDUM = 3
      CALL DDCOMX(IPART,IDUM,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *            FX,FY,FZ,NP,IUT0,IERR,RX,RY,MAXBUF)
      IF(IERR.NE.0) THEN
          WRITE(IUT0,*)
          WRITE(IUT0,*) ERMSGC
          RETURN
      ENDIF
C
C
      DO 3500 IP = 1 , NP
          SSIJ(4,IP)=S(IP)*SIJN(4,IP)+GAM2/24.E0*CM(IP)*FX(IP)
          SSIJ(5,IP)=S(IP)*SIJN(5,IP)+GAM2/24.E0*CM(IP)*FY(IP)
          SSIJ(6,IP)=S(IP)*SIJN(6,IP)+GAM2/24.E0*CM(IP)*FZ(IP)
 3500 CONTINUE
C
C
C
C COMPUTE CONTRACTION OF TEST-FILTERED STRAIN VELOCITY TENSOR
C
C
C
      DO 3600 IP = 1 , NP
          S(IP) = SQRT(2.E0*SIJ(1,IP)*SIJ(1,IP)
     &                +2.E0*SIJ(2,IP)*SIJ(2,IP)
     &                +2.E0*SIJ(3,IP)*SIJ(3,IP)
     &                +4.E0*SIJ(4,IP)*SIJ(4,IP)
     &                +4.E0*SIJ(5,IP)*SIJ(5,IP)
     &                +4.E0*SIJ(6,IP)*SIJ(6,IP))
 3600 CONTINUE
C
C
C
C LEAST-SQUARE AVERAGE LEONARD STRESS TENSOR AND SGS STRESS TENSOR
C
C
C
      DO 3700 IP = 1 , NP
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
 3700 CONTINUE
C
C
C
C RECURSIVELY TEST-FILTER LEAST-SQUARE AVERAGED LEONARD STRESS
C AND SGS STRESS FOR AVERAGING PURPOSE
C
C
C
      DO 3820 IAVDYN = 1 , NAVDYN
          DO 3810 I = 1 , N2
              DO 3800 IE = 1 , NE
                  RX(I,IE) = 0.E0
                  RY(I,IE) = 0.E0
 3800         CONTINUE
 3810     CONTINUE
C
      DO 3900 IP = 1 , NP
          FX(IP) = 0.E0
          FY(IP) = 0.E0
 3900 CONTINUE
C
      DO 4030 IP=1,NP
          RX1 = 0.E0
          RY1 = 0.E0
          DO 4020 IPE=1,NEP(IP)
              IE =IENP(IPE,IP)
              IF (LEFIX(IE).EQ.1) GOTO 4020
              NN = NEX(8)
C
              DO 4010 K=1,NN
                  D1=FILTER(IE)**2*(EAP3(1,K,IPE,IP)
     *                            + EAP3(2,K,IPE,IP)
     *                            + EAP3(3,K,IPE,IP))
C
                  IP1  = NODP(K,IPE,IP)
                  FXIE = AML(IP1)
                  FYIE = AMM(IP1)

                  RX1=RX1-D1*FXIE
                  RY1=RY1-D1*FYIE
 4010         CONTINUE
C     
 4020     CONTINUE
          FX(IP) = RX1
          FY(IP) = RY1
 4030 CONTINUE
C
C
          IDUM = 2
          CALL DDCOMX(IPART,IDUM,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *                FX,FY,FZ,NP,IUT0,IERR,RX,RY,MAXBUF)
          IF(IERR.NE.0) THEN
              WRITE(IUT0,*)
              WRITE(IUT0,*) ERMSGC
              RETURN
          ENDIF
C
C
          DO 4100 IP = 1 , NP
              AML(IP) = AML(IP)+GAM2/24.E0*CM(IP)*FX(IP)
              AMM(IP) = AMM(IP)+GAM2/24.E0*CM(IP)*FY(IP)
 4100     CONTINUE
 3820 CONTINUE
C
C
C
C COMPUTE ELEMENT SMAGORINSKY CONSTANT
C
C
C
      DO 4200 IE = 1 , NE
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
 4200 CONTINUE
C
C
C
C FINALLY, CLEAR ELEMENT SMAGORINSKY CONSTANT DOMINATED BY
C THE ROUND-OFF ERRORS
C
C
C
      DO 4300 IE = 1 , NE
          SE = 0.125E0*(S(NODE(1,IE))+S(NODE(5,IE))
     &                 +S(NODE(2,IE))+S(NODE(6,IE))
     &                 +S(NODE(3,IE))+S(NODE(7,IE))
     &                 +S(NODE(4,IE))+S(NODE(8,IE)))
C
          SGS = CLEAR*CLEAR*FILTER(IE)*FILTER(IE)*SE
          IF(SGS .LT. EPS*VISC(IE)) CS(IE) = 0.E0
 4300 CONTINUE
C
      RETURN
      END
