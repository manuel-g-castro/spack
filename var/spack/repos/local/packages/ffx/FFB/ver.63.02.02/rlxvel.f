C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.8.2                                   C
C                                                                      C
C  SUB ROUTINE : RLXVEL                                                C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C CONTACT ADDRESS : IIS, THE UNIVERSITY OF TOKYO, CISS                 C
C                                                                      C
C THERMO-FLUID ANALYSIS SOLVERS FOR LARGE-SCALE-ASSEMBLY               C
C                                                                      C
C  PERFORMANCE OPTIMIZATION                                            C
C                                       MODIFIED BY RIST               C
C======================================================================C
      SUBROUTINE RLXVEL(MCOLOR,MCPART,NCOLOR,NCPART,LLOOP,NITR,
     *                  GAMDYN,FILTER,U,V,W,
     *                  DNXI,DNYI,DNZI,SN,CM,
     *                  NODE,ME,MELM,NE,NP,N1,N2,NEX,
     *                  IPART,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *                  UR,VR,WR,FX,FY,FZ,RX,RY,
     *                  WRK00,WRK01,WRK02,WRK03, 
     *                  IVOF,NEFLD2,LEFLD2,LEFIX,
     *                  EAP3,IENP,NEP,MEP,MP,
     *                  IUT0,IERR)
C
      IMPLICIT NONE
C
C[INPUT]
      INTEGER*4 MCOLOR,MCPART
      INTEGER*4 NCOLOR(4),NCPART(MCOLOR,4),LLOOP(MCPART,MCOLOR,4)
      INTEGER*4 NITR
C
      REAL*4    GAMDYN
      INTEGER*4 ME,MELM,NE,NP,N1,N2,NEX(8),IUT0,IERR
      INTEGER*4 NODE(N2,NE)
C
      REAL*4    FILTER(NE),U(NP),V(NP),W(NP),
     *          DNXI(N1,NE),DNYI(N1,NE),DNZI(N1,NE),
     *          SN(N1,NE),CM(NP)
C
      INTEGER*4 IPART,NDOM,MBPDOM,
     *          LDOM(NDOM),NBPDOM(NDOM),
     *          IPSLF(MBPDOM,NDOM),IPSND(MBPDOM,NDOM)
C
      INTEGER*4 MEP,MP,IPE,K,NN
      INTEGER*4 IENP(MEP,MP),NEP(MP)
      REAL*4    EAP3(6,N2,MEP,NP)
C
C[OUTPUT]
      REAL*4    UR(NP),VR(NP),WR(NP)
C
C[WORK]
      REAL*4    RX(N1,NE),RY(N1,NE),FX(NP),FY(NP),FZ(NP)
C
C
C      
C     [IN:MID NODE ]
      INTEGER*4 ITIMEI,ISTEP,JPRESS
      REAL*4    WRK00(NP),WRK01(NP),WRK02(NP),WRK03(NP)
C      
      INTEGER*4 IP,IE,IB,I,J,IELM,ICOLOR,ICPART,IES,IEE,IDUM,IAVDYN,
     *          IELM1,IELM2,IELM3,IELM4,IELM5,IELM6,IELM7,IELM8,
     *          MAXBUF,IITR
      REAL*4    GAM2,DUEIE,DVEIE,DWEIE,FXIE,FYIE,FZIE,UKK,
     *          AMLE,AMME,SE,SGS,
     *          D1,RX1,RY1,RZ1
C
C     [INPUT:VOF]
      INTEGER*4 IVOF,NEFLD2,LEFLD2(NEFLD2),LEFIX(NE)
C
      CHARACTER*60 ERMSGC
     & /' ## SUBROUTINE DYNA3D: FATAL      ERROR REPORT   ; RETURNED' /
C
      REAL*4 CLEAR,EPS
      DATA CLEAR  / 0.15   /
      DATA EPS    / 1.0E-2 /

C      RELAX VELOCITY FIELD BY TEST FILTERING 
C         ( 3-D CALCULATION : SINGLE WORD VERSION )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          GAMDYN      ; RATIO OF WIDTH OF TEST FILTER TO THAT OF GRID
C                       FILTER.
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
C          UR      (IP); FILTERED X-DIR. VELOCITY COMPONENT
C          VR      (IP); FILTERED Y-DIR. VELOCITY COMPONENT
C          WR      (IP); FILTERED Z-DIR. VELOCITY COMPONENT
C          IERR        ; RETURN CODE TO REPORT ERROR OCCURENCE
C                   0 --- NORMAL TERMINATION
C                   1 --- A FATAL ERROR HAS OCCURED
C
C       (3) WORK
C          RX    (I,IE); HOLDS X-DIR. ELEMENT RESIDUAL
C          RY    (I,IE); HOLDS Y-DIR. ELEMENT RESIDUAL
C          RZ    (I,IE); HOLDS Z-DIR. ELEMENT RESIDUAL
C          FX      (IP); HOLDS X-DIR. NODE    RESIDUAL
C          FY      (IP); HOLDS Y-DIR. NODE    RESIDUAL
C          FZ      (IP); HOLDS Z-DIR. NODE    RESIDUAL
C
C
C >>>> ADDED VARIABLES BY RIST FOR PERFORMANCE OPTIMIZATION <<<<
C       INPUT
C
C
      GAM2   = GAMDYN*GAMDYN
C
      MAXBUF = NE*N2
C
      DO 1000 IE=1,NE
         LEFIX(IE)=0
 1000 CONTINUE
C
      IF (IVOF.EQ.1) THEN
!ocl norecurrence(LEFIX)
         DO 1100 IB=1,NEFLD2
            LEFIX(LEFLD2(IB))=1
 1100    CONTINUE
      ENDIF
C
C
C TEST-FILTER VELOCITY VECTOR
C
C
C
      DO 1200 IP = 1 , NP
          UR(IP) = U(IP)
          VR(IP) = V(IP)
          WR(IP) = W(IP)
 1200 CONTINUE
C
      IF(NITR.EQ.0) RETURN
C
      IITR=0 
 2000 CONTINUE
C
      DO 2010 IP = 1 , NP
          FX(IP) = 0.E0
          FY(IP) = 0.E0
          FZ(IP) = 0.E0
 2010 CONTINUE
C
      DO 2330 IP=1,NP
          FXIE = UR(IP)
          FYIE = VR(IP)
          FZIE = WR(IP)
          RX1 = 0.E0
          RY1 = 0.E0
          RZ1 = 0.E0

          DO 2320 IPE=1,NEP(IP)
              IE =IENP(IPE,IP)
              IF (LEFIX(IE).EQ.1) GOTO 2320
              NN = NEX(8)
C
              DO 2310 K=1,NN
                  D1=FILTER(IE)**2*(EAP3(1,K,IPE,IP)
     *                             +EAP3(2,K,IPE,IP)
     *                             +EAP3(3,K,IPE,IP))
                  RX1=RX1-D1*FXIE
                  RY1=RY1-D1*FYIE
                  RZ1=RZ1-D1*FZIE
 2310         CONTINUE
C
 2320     CONTINUE
C     
          FX(IP) = RX1
          FY(IP) = RY1
          FZ(IP) = RZ1
 2330 CONTINUE
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
      DO 2600 IP = 1 , NP
          UR(IP) = UR(IP)+GAM2/24.E0*CM(IP)*FX(IP)
          VR(IP) = VR(IP)+GAM2/24.E0*CM(IP)*FY(IP)
          WR(IP) = WR(IP)+GAM2/24.E0*CM(IP)*FZ(IP)
 2600 CONTINUE
C
      IITR=IITR+1
      IF(IITR.LT.NITR) GOTO 2000
C
      RETURN
      END
