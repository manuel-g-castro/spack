      SUBROUTINE PRES3X(MCOLOR,MCPART,NCOLOR,NCPART,LLOOP,
     *                  IPRESS,JSET,JFSPRS,
     *                  PLIMIT,ALPHAP,DT,FSMACH,PSFREE,NMAX,EPS,EPSRE,
     *                  NRN,RES,
     *                  ME,MP,N1,N2,NE,NP,NEX,NITRI,
     *                  U,V,W,P,PN,DP,DPE,DT3D,NODE,IEMEDA,
     *                  DNXI,DNYI,DNZI,SN,MELM,CM,
     *                  APRS,AAA,NPP,NCRS,IPCRS,AAAPC,LTAB,APRS0,
     *                  NPFREE,LPFREE,NPSLD2,LPSLD2,NUMIP,
     *                  IPART,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *                  NPSET,LPSET1,LPSET2,LPSET3,COVER1,COVER2,COVER3,
     *                  NPSND,LPSND,NPTSND,IPSET,IPSRC,
     *                  NPRCV,LPRCV,NPTRCV,
     *                  LPFIX,LWORK,B,DIV,UG,VG,WG,WRK1,WRK2,WRK3,WRK4,
     *                  WRK5,RX,RY,MWRK,WRKN,
     *                  JUNROL,NPPMAX,NCRS2,TS,TACRS,ITPCRS,
     *                  IUT0,IERR,ICRS_T,
     *                  EAP1,EAP2,NODP,IENP,JENP,NEP,MEP)
      IMPLICIT NONE
C
      INTEGER*4 MCOLOR,MCPART
      INTEGER*4 NCOLOR(4),NCPART(MCOLOR,4),LLOOP(MCPART,MCOLOR,4)
C
      INTEGER*4 IPRESS,JSET,JFSPRS,NMAX,NRN,
     *          ME,MP,N1,N2,NE,NP,NEX,MELM,
     *          NITRI,NODE,NPP,NCRS,IPCRS,LTAB,
     *          LPFIX,NPFREE,LPFREE,NPSLD2,LPSLD2,NUMIP,
     *          LPSET1,LPSET2,LPSET3,NPSET,
     *          NPSND ,LPSND ,NPTSND,IPSET,IPSRC,
     *          NPRCV ,LPRCV ,NPTRCV,
     *          IPART ,LDOM,NBPDOM,NDOM,
     *          IPSLF,IPSND,MBPDOM,LWORK,IUT0,IERR
      REAL*4    DT,FSMACH,PSFREE,PLIMIT,ALPHAP,
     *          EPS,EPSRE,RES,U,V,W,P,PN,
     *          DNXI,DNYI,DNZI,SN,E,
     *          APRS,AAA,AAAPC,APRS0,CM,
     *          COVER1,COVER2,COVER3,
     *          RX,RY,B,DIV,UG,VG,WG,WRK1,WRK2,WRK3,WRK4,WRK5
C
      REAL*4 DP(NP),DPE(NE),DT3D(NE)
      INTEGER*4 IEMEDA(NE)
C
      DIMENSION NEX(12)
      DIMENSION U(NP),V(NP),W(NP),P(NE),PN(NP+1)
      DIMENSION NODE(N2,NE),
     *          DNXI(N1,NE), DNYI(N1,NE),DNZI(N1,NE),CM(MP),
     *          LPFIX(MP),
     *          SN(N1,NE),
     *          APRS(N1,N2,ME),APRS0(NP)
      DIMENSION AAA(N1,N2,ME)
      DIMENSION NPP(NP)
      DIMENSION IPCRS(NCRS),AAAPC(NCRS),LTAB(N1,N2,NE)
      DIMENSION LPFREE(NPFREE),LPSLD2(NPSLD2),NUMIP(NP),
     1          LDOM(NDOM),NBPDOM(NDOM),
     2          IPSLF(MBPDOM,NDOM),IPSND(MBPDOM,NDOM)
      DIMENSION LPSET1(NPSET),LPSET2(NPSET),LPSET3(NPSET),
     1          COVER1(NPSET),COVER2(NPSET),COVER3(NPSET),
     2          LPSND (NDOM),NPTSND(NDOM) ,LPRCV (NDOM) ,NPTRCV(NDOM),
     3          IPSET (MBPDOM,NDOM),IPSRC (MBPDOM,NDOM)
      DIMENSION RX(0:N2,ME),RY(0:N2,ME),
     1          B(NP),DIV(NE),UG(*),VG(*),WG(*),
     2          WRK1(NP),WRK2(NP),WRK3(NP),WRK4(NP),WRK5(NP),LWORK(NP)
      INTEGER*4 MWRK
      REAL*4    WRKN(MWRK,4)
C
      REAL*4    EAP1(N2,MEP,NP),EAP2(3,N2,MEP,NP)
      INTEGER*4 NODP(N2,MEP,NP),IENP(MEP,MP),JENP(MEP,MP),NEP(MP)
      INTEGER*4 MEP,IPE,NN,K,IPI
C
C     [FULL UNROOL]
      INTEGER*4 JUNROL
      INTEGER*4 NPPMAX,NCRS2,ITPCRS(NCRS2)
      REAL*4    TS(0:NP),TACRS(NCRS2)
C      
      INTEGER*4 ICRS_T(NP)
     
C
      REAL*4 DIJ(8,8)
      DATA DIJ / 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     &           0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     &           0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     &           0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0,
     &           0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0,
     &           0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0,
     &           0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0,
     &           0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0 /
C
      INTEGER*4 IDIMP,IMODE
      DATA IDIMP  / 1 /
      DATA IMODE  / 1 /
      INTEGER*4 IBCGS
      DATA IBCGS  / 1 /
C
      INTEGER*4 NETET,NEPRD,NEWED,NEHEX,
     *          NTET,NPRD,NWED,NHEX,
     *          NSKIP1,NSKIP2,NSKIP3,NSKIP4,IEE,IES,
     *          IES1,IEE1,IES2,IEE2,IES3,IEE3,IES4,IEE4,
     *          IELM0,
     *          IELM1,IELM2,IELM3,IELM4,
     *          IELM5,IELM6,IELM7,IELM8,
     *          MAXBUF,IP,IE,IB,IBP,ISEND,NPFIX,
     *          IP1,IP2,IP3,IP4,IP5,IP6,IP7,IP8,I,J,
     *          IITRE,NITRE,NMAXD,NRND,NB,IRES,IDIM,ICOLOR,ICPART
      REAL*4    COEF0,COEF1,AT,
     *          AT1,AT2,AT3,AT4,AT5,AT6,AT7,AT8,
     *          AC1,AC2,AC3,AC4,AC5,AC6,AC7,AC8,
     *          T1,T2,T3,T4,T5,T6,T7,T8,
     *          CRHS1,CRHS2,CRHS3,CRHS4,CRHS5,CRHS6,CRHS7,CRHS8,
     *          UU,VV,WW,COEF,COEF2,GP,EP,TP,BUF,
     *          ATI,ACI,CRHSI
C
      CHARACTER*60 ERMSGC
      
     & / ' ## SUBROUTINE PRES3X: FATAL      ERROR REPORT   ; RETURNED' /
C
C
C      COMPUTE PRESSURE FIELD BY THE FRACTIONAL STEP METHOD
C         ( 3-D CALCULATION : SINGLE WORD & MULTI ELEMENT VERSION )
C                                           CODED BASED ON 'RELX3T'
C                                              2009.01.30 Y.YAMADE
C
C       COMPUTATIONAL COSTS FOR THE OPERATION EXCEPT SOLIVING MATRIX 
C      EQUATION ARE AS FOLLOWS; 
C
C ************ COMPUTATIONAL COST EXCEPT FOR MATRIX SOLVER *******
C ************ (1) WITHOUT LOW MACH ASSUMPTIONCOMPUTATIONAL COST**
C =============================TET======================================
C          OPERATION COUNTS:     71 FLOP /ELEMENT
C          DATA LOADINGS   :      1 WORDS/ELEMENT
C                           (    52 WORDS CONTIGUOUSLY,
C                                20 WORDS BY STRIDE, AND
C                                73 WORDS BY LIST )
C
C =============================WED======================================
C          OPERATION COUNTS:    131 FLOP /ELEMENT
C          DATA LOADINGS   :    133 WORDS/ELEMENT
C                           (     1 WORDS CONTIGUOUSLY,
C                               102 WORDS BY STRIDE, AND
C                                30 WORDS BY LIST )
C
C =============================HEX======================================
C          OPERATION COUNTS:    207 FLOP /ELEMENT
C          DATA LOADINGS   :    205 WORDS/ELEMENT
C                           (     1 WORDS CONTIGUOUSLY,
C                               168 WORDS BY STRIDE, AND
C                                36 WORDS BY LIST )
C
C ************ (2) WITH    LOW MACH ASSUMPTIONCOMPUTATIONAL COST**
C =============================TET======================================
C          OPERATION COUNTS:    307 FLOP /ELEMENT
C          DATA LOADINGS   :    205 WORDS/ELEMENT
C                           (    13 WORDS CONTIGUOUSLY,
C                               136 WORDS BY STRIDE, AND
C                                56 WORDS BY LIST )
C
C =============================WED======================================
C          OPERATION COUNTS:    653 FLOP /ELEMENT
C          DATA LOADINGS   :    403 WORDS/ELEMENT
C                           (    19 WORDS CONTIGUOUSLY,
C                               288 WORDS BY STRIDE, AND
C                                96 WORDS BY LIST )
C
C =============================HEX======================================
C          OPERATION COUNTS:   1127 FLOP /ELEMENT
C          DATA LOADINGS   :    660 WORDS/ELEMENT
C                           (    24 WORDS CONTIGUOUSLY,
C                               496 WORDS BY STRIDE, AND
C                               140 WORDS BY LIST )
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          NLOOP       ;NUMBER OF LOOPS
C          LLOOP       ;POINTER FOR SPLITTED ELEMENT LIST
C          IPRESS      ; FLAG FOR PRESSURE EQUATION
C                        1---- FRACTIONAL STEP
C                        2---- FRACTIONAL STEP WITH LOW MACH-ASSUMPTION
C          FSMACH      ; MACH NUMBER OF FREE STREAM
C          DT          ; TIME INCREMENT
C          NMAX        ; MAXIMUM ITERATION NUMBER
C          EPS         ; MAXIMUM ALLOWABLE ERROR
C
C          ME          ; MAX. NUMBER OF TOTAL ELEMENTS
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          NP          ; NUMBER OF TOTAL    NODES
C          NEX(I)      ; INCLUDES NUMBER OF ELEMENTS AND NUMBER OF LOCAL NODES
C                        AS FOLOOWS
C          NEX(1)      ; NUMBER OF TET.    ELEMENTS
C          NEX(2)      ; NUMBER OF PYRAMID ELEMENTS
C          NEX(3)      ; NUMBER OF WEGDE   ELEMENTS
C          NEX(4)      ; NUMBER OF HEX.    ELEMENTS
C          NEX(5)      ; NUMBER OF LOCAL NODES IN A TET.    ELEMENT (=4)
C          NEX(6)      ; NUMBER OF LOCAL NODES IN A PYRAMID ELEMENT (=5)
C          NEX(7)      ; NUMBER OF LOCAL NODES IN A WEGDE   ELEMENT (=6)
C          NEX(8)      ; NUMBER OF LOCAL NODES IN A HEX.    ELEMENT (=8)
C
C          U       (IP); X-DIR. VELOCITY COMPONENT
C          V       (IP); Y-DIR. VELOCITY COMPONENT
C          W       (IP); Z-DIR. VELOCITY COMPONENT
C
C          NODE  (I,IE); NODE TABLE
C          NUMIP   (IP); NUMBER OF NEIGHBORING DOMAINS THAT NODE
C                        'IP' BELONG TO
C          DNXI  (I,IE); ELEMENT CENTER VALUE OF NX
C          DNYI  (I,IE); ELEMENT CENTER VALUE OF NY
C          DNZI  (I,IE); ELEMENT CENTER VALUE OF NZ
C          SN    (I,IE); ELEMENT INTEGRATED VECTOR OF N
C          E   (I,J,IE); INTEGRATED ELEMENT MATRIX OF N*NT
C
C          A   (I,J,IE); ELEMENT-WISE MATRIX COEFFICIENT FOR 
C                        PRESSURE EQUATION (ONLY POISSON OPERATER)
C          A0      (IP); DIAGONAL TERM OF MATRIX 'A'
C
C        A. FREE BOUNDARY
C          NPFREE      ; NUMBER OF FREE BOUNDARY NODES
C          LPFREE (IBP); FREE BOUNDARY NODES
C
C        B. INTER-CONNECT BOUNDARY
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
C        C. OVERSET BOUNDARY NODES
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
C          COVER1 (IBP); LOCAL COORDINATE IN INTERPOLATING ELEMENT
C          COVER2 (IBP); LOCAL COORDINATE IN INTERPOLATING ELEMENT
C          COVER3 (IBP); LOCAL COORDINATE IN INTERPOLATING ELEMENT
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
C          NPP      (IP); NUMBER OF ADJACENT NODES    TO NODE    IP
C          NCRS    ; NUMBER OF NONZERO ELEMENTS IN MATRIX OF CRS FORMAT
C          IPCRS  (ICRS); NODE NO. TABLE BASED ON CRS FORMAT
C          LTAB(J1,J2,IE); CRS INDEX TABLE FOR NODE-BASE MATRIX
C                          COEFFICIENT
C
C          IUT0        ; FILE NUMBER TO REPORT ERROR OCCURENCE
C
C       (2) OUTPUT
C          P       (IE); PRESSURE AT ELEMENTS 
C          NRN         ; CALCULATION ITERATED NUMBER
C          RES         ; RESIDUAL OF MATRIX SOLVER
C          IERR        ; RETURN CODE TO REPORT ERROR OCCURENCE
C                   0 --- NORMAL TERMINATION
C                   1 --- A FATAL ERROR HAS OCCURED
C
C       (3) INPUT-OUTPUT
C          PN      (IP); PRESSURE AT NODES
C
C       (4) WORK
C          AAA (I,J,IE); ELEMENT-WISE MATRIX COEFFICIENT FOR 
C                        PRESSURE EQUATION INCLUDING NON-LINEAR TERM
C          AAAPC (ICRS); NODE-BASE MATRIX COEFFICIENT FOR 
C                        PRESSURE EQUATION INCLUDING NON-LINEAR TERM
C          RX    (I,IE); X-DIR. ELEMENT RESIDUAL
C          RY    (I,IE); Y-DIR. ELEMENT RESIDUAL
C          DIV     (IE); ELEMENT DIVERGENT
C          B       (IP); R.H.S OF PRESSURE EQUATION
C          UG      (IE); WORK
C          VG      (IE); WORK
C          WG      (IE); WORK
C          WRK1    (IP); WORK
C          WRK2    (IP); WORK
C          WRK3    (IP); WORK
C          WRK4    (IP); WORK
C          WRK5    (IP); WORK
C
C
      IERR=0
C
CLES3X [1.] CHECK CONTROL PARAMETER
C
      NETET=NEX(1)
      NEPRD=NEX(2)
      NEWED=NEX(3)
      NEHEX=NEX(4)
      NE   =NETET+NEPRD+NEWED+NEHEX
C
      NTET=NEX(5)
      NPRD=NEX(6)
      NWED=NEX(7)
      NHEX=NEX(8)
C
C   == TET. ==  
      IES1=1
      IEE1=NETET 
C
C   == PYRAMID ==  
      IES2=NETET+1
      IEE2=NETET+NEPRD
C
C   == WEDGE ==  
      IES3=NETET+NEPRD+1
      IEE3=NETET+NEPRD+NEWED
C
C   == HEX. ==  
      IES4=NETET+NEPRD+NEWED+1
      IEE4=NETET+NEPRD+NEWED+NEHEX 
C
      IF (IPRESS.NE.1 .AND. IPRESS.NE.2 )THEN
          WRITE(IUT0,*) "INVALID VALUE FOR IPRESS"
          WRITE(IUT0,*) ERMSGC
          IERR=1
          RETURN
      ENDIF
C
      MAXBUF = NE*(N2+1)
C
C
C     MAKE FIX BOUNDARY NODES LIST (LPFIX)
C
      DO 100 IP = 1 , NP
          WRK1(IP) = 1.0E0
          LWORK(IP)=0
          LPFIX(IP)=0
  100 CONTINUE
C
*POPTION INDEP(LWORK)
C*$*ASSERT PERMUTATION ( LPFREE )
      DO 110 IB = 1 , NPFREE
          LWORK(LPFREE(IB))=1
  110 CONTINUE
C
*POPTION INDEP(LWORK)
C*$*ASSERT PERMUTATION ( LPFREE )
      DO 115 IB = 1 , NPSLD2
          LWORK(LPSLD2(IB))=1
  115 CONTINUE
C
*POPTION INDEP(LWORK)
C*$*ASSERT PERMUTATION ( LPSET1 )
      DO 120 IB = 1 , NPSET
          ISEND = LPSET3(IB)
          IF(ISEND.GT.0) GO TO 120
          LWORK(LPSET1(IB))=1
  120 CONTINUE
C
*POPTION INDEP(LPFIX)
C*$*ASSERT PERMUTATION ( LWORK )
      NPFIX=0
      DO 130 IP = 1 , NP
          IF(LWORK(IP).EQ.0) GO TO 130
          NPFIX=NPFIX+1
          WRK1(IP) = 0.0E0
          LPFIX(NPFIX) = IP
  130 CONTINUE
C
      IF(IPRESS.EQ.1)THEN
          COEF0 = 0.0E0
          COEF1 = 0.0E0
C
          DO IP=1,NP
              B(IP)=0.0E0 
          ENDDO
C
          GOTO 6000 
      ELSE
          COEF0 = FSMACH*FSMACH/DT/DT
          COEF1 = FSMACH*FSMACH/DT/2.0E0
      ENDIF
C
CLES3X [2.] CAL. VELOCITIES AT ELEMENTS (UG,VG,WG)
C
C     OPERATION COUNTS:   12 FLOP /ELEMENT
C     DATA LOADINGS   :   16 WORDS/ELEMENT
C                      (   0 WORDS CONTIGUOUSLY,
C                          4 WORDS BY STRIDE, AND
C                         12 WORDS BY LIST )
C
      COEF=1.0E0/4.0E0
      DO 210 IE = IES1 , IEE1
          IP1=NODE(1,IE)
          IP2=NODE(2,IE)
          IP3=NODE(3,IE)
          IP4=NODE(4,IE)
          UG(IE) = COEF*(U(IP1)+U(IP2)+U(IP3)+U(IP4))
          VG(IE) = COEF*(V(IP1)+V(IP2)+V(IP3)+V(IP4))
          WG(IE) = COEF*(W(IP1)+W(IP2)+W(IP3)+W(IP4))
  210 CONTINUE
C
C     OPERATION COUNTS:   15 FLOP /ELEMENT
C     DATA LOADINGS   :   20 WORDS/ELEMENT
C                      (   0 WORDS CONTIGUOUSLY,
C                          5 WORDS BY STRIDE, AND
C                         15 WORDS BY LIST )
C
      COEF=1.0E0/5.0E0
      DO 220 IE = IES2 , IEE2
          IP1=NODE(1,IE)
          IP2=NODE(2,IE)
          IP3=NODE(3,IE)
          IP4=NODE(4,IE)
          IP5=NODE(5,IE)
          UG(IE) = COEF*(U(IP1)+U(IP2)+U(IP3)+U(IP4)
     *                  +U(IP5))
          VG(IE) = COEF*(V(IP1)+V(IP2)+V(IP3)+V(IP4)
     *                  +V(IP5))
          WG(IE) = COEF*(W(IP1)+W(IP2)+W(IP3)+W(IP4)
     *                  +W(IP5))
  220 CONTINUE
C
C     OPERATION COUNTS:   18 FLOP /ELEMENT
C     DATA LOADINGS   :   24 WORDS/ELEMENT
C                      (   0 WORDS CONTIGUOUSLY,
C                          6 WORDS BY STRIDE, AND
C                         18 WORDS BY LIST )
C
      COEF=1.0E0/6.0E0
      DO 230 IE = IES3 , IEE3
          IP1=NODE(1,IE)
          IP2=NODE(2,IE)
          IP3=NODE(3,IE)
          IP4=NODE(4,IE)
          IP5=NODE(5,IE)
          IP6=NODE(6,IE)
          UG(IE) = COEF*(U(IP1)+U(IP2)+U(IP3)+U(IP4)
     *                  +U(IP5)+U(IP6))
          VG(IE) = COEF*(V(IP1)+V(IP2)+V(IP3)+V(IP4)
     *                  +V(IP5)+V(IP6))
          WG(IE) = COEF*(W(IP1)+W(IP2)+W(IP3)+W(IP4)
     *                  +W(IP5)+W(IP6))
  230 CONTINUE
C
C     OPERATION COUNTS:   24 FLOP /ELEMENT
C     DATA LOADINGS   :   32 WORDS/ELEMENT
C                      (   0 WORDS CONTIGUOUSLY,
C                          8 WORDS BY STRIDE, AND
C                         24 WORDS BY LIST )
C
      COEF=1.0E0/8.0E0
      DO 240 IE = IES4 , IEE4
          IP1=NODE(1,IE)
          IP2=NODE(2,IE)
          IP3=NODE(3,IE)
          IP4=NODE(4,IE)
          IP5=NODE(5,IE)
          IP6=NODE(6,IE)
          IP7=NODE(7,IE)
          IP8=NODE(8,IE)
          UG(IE) = COEF*(U(IP1)+U(IP2)+U(IP3)+U(IP4)
     *                  +U(IP5)+U(IP6)+U(IP7)+U(IP8))
          VG(IE) = COEF*(V(IP1)+V(IP2)+V(IP3)+V(IP4)
     *                  +V(IP5)+V(IP6)+V(IP7)+V(IP8))
          WG(IE) = COEF*(W(IP1)+W(IP2)+W(IP3)+W(IP4)
     *                  +W(IP5)+W(IP6)+W(IP7)+W(IP8))
  240 CONTINUE
C
CLES3X [3.] CAL. TIME AND CONVECTION TERM
C
      DO 300 IP=1,NP
          B(IP)=0.0E0
  300 CONTINUE
C
C     OPERATION COUNTS:  224 FLOP /ELEMENT
C     DATA LOADINGS   :  116 WORDS/ELEMENT
C                      (  12 WORDS CONTIGUOUSLY,
C                         80 WORDS BY STRIDE, AND
C                         24 WORDS BY LIST )
C
      CALL USTSTA(92)
      PN(NP+1)=0.0E0
!ocl norecurrence(AAA,WRK1,B)
!$omp parallel do  private(IPE,IE,I,K,IP1,UU,VV,WW,
!$omp+ AT,COEF2,CRHSI,AT1,AC1,CRHS1)
!$omp+ schedule(static, 4600)
      DO 310 IP=1,NP
          COEF2=WRK1(IP)
          CRHSI=0.0E0
!ocl noswp
          DO 320 IPE=1,NEP(IP)
              IE =IENP(IPE,IP)
              I  =JENP(IPE,IP)
              UU=UG(IE)
              VV=VG(IE)
              WW=WG(IE)
C
              AT =EAP1(1,IPE,IP)+EAP1(2,IPE,IP)+EAP1(3,IPE,IP)
     *           +EAP1(4,IPE,IP)+EAP1(5,IPE,IP)+EAP1(6,IPE,IP)
     *           +EAP1(7,IPE,IP)+EAP1(8,IPE,IP)
C
C NOTE THAT;
C MATRIX COEFFICINTS AT DIRECLET BOUNDARY NODES ARE 0 IN THIS LOOP
              DO 330 K=1,8
                  ATI=DIJ(I,K)*AT*COEF0
                  ACI=(UU*EAP2(1,K,IPE,IP)+VV*EAP2(2,K,IPE,IP)
     *                +WW*EAP2(3,K,IPE,IP))*COEF1
                  AAA(I,K,IE)=(ATI+ACI)*COEF2
                  IPI=NODP(K,IPE,IP)
                  CRHSI=CRHSI+(ATI-ACI)*PN(IPI)
  330         CONTINUE
  320     CONTINUE
C
          B(IP)=CRHSI
  310 CONTINUE
      CALL USTEND(92)
C
C
CLES3X [4.] CAL MATRIX COEFFICIENTS
CCCC   DIAGONAL SCALING OF LOW-MACH ASSUMPTION PART IN PRESSURE EQUATION
CCCC  MATIRIX COEFFICIENT AND SUMMATION OF TWO PARTS.   
CCC   
C
C     OPERATION COUNTS:   36 FLOP /ELEMENT
C     DATA LOADINGS   :   36 WORDS/ELEMENT
C                      (   0 WORDS CONTIGUOUSLY,
C                         32 WORDS BY STRIDE, AND
C                          4 WORDS BY LIST )
      DO 511 I=1,NTET
          DO 510 IE=IES1,IEE1
              IP=NODE(I,IE)
              COEF=1.0E0/APRS0(IP)
              AAA(I,1,IE)=AAA(I,1,IE)*COEF+APRS(I,1,IE)
              AAA(I,2,IE)=AAA(I,2,IE)*COEF+APRS(I,2,IE)
              AAA(I,3,IE)=AAA(I,3,IE)*COEF+APRS(I,3,IE)
              AAA(I,4,IE)=AAA(I,4,IE)*COEF+APRS(I,4,IE)
  510     CONTINUE
  511 CONTINUE
C
C     OPERATION COUNTS:   55 FLOP /ELEMENT
C     DATA LOADINGS   :   55 WORDS/ELEMENT
C                      (   0 WORDS CONTIGUOUSLY,
C                         50 WORDS BY STRIDE, AND
C                          5 WORDS BY LIST )
      DO 521 I=1,NPRD
          DO 520 IE=IES2,IEE2
              IP=NODE(I,IE)
              COEF=1.0E0/APRS0(IP)
              AAA(I,1,IE)=AAA(I,1,IE)*COEF+APRS(I,1,IE)
              AAA(I,2,IE)=AAA(I,2,IE)*COEF+APRS(I,2,IE)
              AAA(I,3,IE)=AAA(I,3,IE)*COEF+APRS(I,3,IE)
              AAA(I,4,IE)=AAA(I,4,IE)*COEF+APRS(I,4,IE)
              AAA(I,5,IE)=AAA(I,5,IE)*COEF+APRS(I,5,IE)
  520     CONTINUE
  521 CONTINUE
C
C     OPERATION COUNTS:   78 FLOP /ELEMENT
C     DATA LOADINGS   :   78 WORDS/ELEMENT
C                      (   0 WORDS CONTIGUOUSLY,
C                         72 WORDS BY STRIDE, AND
C                          6 WORDS BY LIST )
      DO 531 I=1,NWED
          DO 530 IE=IES3,IEE3
              IP=NODE(I,IE)
              COEF=1.0E0/APRS0(IP)
              AAA(I,1,IE)=AAA(I,1,IE)*COEF+APRS(I,1,IE)
              AAA(I,2,IE)=AAA(I,2,IE)*COEF+APRS(I,2,IE)
              AAA(I,3,IE)=AAA(I,3,IE)*COEF+APRS(I,3,IE)
              AAA(I,4,IE)=AAA(I,4,IE)*COEF+APRS(I,4,IE)
              AAA(I,5,IE)=AAA(I,5,IE)*COEF+APRS(I,5,IE)
              AAA(I,6,IE)=AAA(I,6,IE)*COEF+APRS(I,6,IE)
  530     CONTINUE
  531 CONTINUE
C
C     OPERATION COUNTS:  136 FLOP /ELEMENT
C     DATA LOADINGS   :  136 WORDS/ELEMENT
C                      (   0 WORDS CONTIGUOUSLY,
C                        128 WORDS BY STRIDE, AND
C                          8 WORDS BY LIST )
      DO 541 I=1,NHEX
          DO 540 IE=IES4,IEE4
              IP=NODE(I,IE)
              COEF=1.0E0/APRS0(IP)
              AAA(I,1,IE)=AAA(I,1,IE)*COEF+APRS(I,1,IE)
              AAA(I,2,IE)=AAA(I,2,IE)*COEF+APRS(I,2,IE)
              AAA(I,3,IE)=AAA(I,3,IE)*COEF+APRS(I,3,IE)
              AAA(I,4,IE)=AAA(I,4,IE)*COEF+APRS(I,4,IE)
              AAA(I,5,IE)=AAA(I,5,IE)*COEF+APRS(I,5,IE)
              AAA(I,6,IE)=AAA(I,6,IE)*COEF+APRS(I,6,IE)
              AAA(I,7,IE)=AAA(I,7,IE)*COEF+APRS(I,7,IE)
              AAA(I,8,IE)=AAA(I,8,IE)*COEF+APRS(I,8,IE)
  540     CONTINUE
  541 CONTINUE
C
C
CLES3X [4.] COMPUTE ELEMENT-WISE FORCE VECTOR
C
C  ************[TET]****************************
C          OPERATION COUNTS:    23 FLOP /ELEMENT
C          DATA LOADINGS   :    24 WORDS/ELEMENT
C                           (    0 WORDS CONTIGUOUSLY,
C                               12 WORDS BY STRIDE, AND
C                               12 WORDS BY LIST )
C
C  ************[WED]****************************
C          OPERATION COUNTS:    35 FLOP /ELEMENT
C          DATA LOADINGS   :    36 WORDS/ELEMENT
C                           (    0 WORDS CONTIGUOUSLY,
C                               18 WORDS BY STRIDE, AND
C                               18 WORDS BY LIST )
C
C  ************[HEX]****************************
C          OPERATION COUNTS:    47 FLOP /ELEMENT
C          DATA LOADINGS   :    48 WORDS/ELEMENT
C                           (    0 WORDS CONTIGUOUSLY,
C                               24 WORDS BY STRIDE, AND
C                               24 WORDS BY LIST )
C
 6000 CONTINUE 
C
      CALL FILD3X(IMODE,ME,NE,NP,NEX,N1,N2,
     &            U,V,W,DIV,NODE,DNXI,DNYI,DNZI,IUT0,IERR)
C
      DO 600 IE=1,NE
          IF(IEMEDA(IE).NE.0) DIV(IE)=0.0E0
  600 CONTINUE
C
C
C     OPERATION COUNTS:    12 FLOP /ELEMENT
C     DATA LOADINGS   :    13 WORDS/ELEMENT
C                      (    1 WORDS CONTIGUOUSLY,
C                           8 WORDS BY STRIDE, AND
C                           4 WORDS BY LIST )
C
C
      COEF=1.0E0/DT
C
      DO 612 ICOLOR=1,NCOLOR(1)
!ocl norecurrence(B)
         DO 611 ICPART=1,NCPART(ICOLOR,1)
            IES=LLOOP(ICPART  ,ICOLOR,1)
            IEE=LLOOP(ICPART+1,ICOLOR,1)-1
!ocl nosimd
!ocl noswp
            DO 610 IE=IES,IEE
C
               IP1=NODE(1,IE)
               IP2=NODE(2,IE)
               IP3=NODE(3,IE)
               IP4=NODE(4,IE)
C
               B(IP1)=B(IP1)-COEF*DIV(IE)*SN(1,IE)
               B(IP2)=B(IP2)-COEF*DIV(IE)*SN(2,IE)
               B(IP3)=B(IP3)-COEF*DIV(IE)*SN(3,IE)
               B(IP4)=B(IP4)-COEF*DIV(IE)*SN(4,IE)
 610        CONTINUE
 611     CONTINUE
 612  CONTINUE
C
C     OPERATION COUNTS:    15 FLOP /ELEMENT
C     DATA LOADINGS   :    16 WORDS/ELEMENT
C                      (    1 WORDS CONTIGUOUSLY,
C                          10 WORDS BY STRIDE, AND
C                           5 WORDS BY LIST )
C
      DO 622 ICOLOR=1,NCOLOR(2)
!ocl norecurrence(B)
         DO 621 ICPART=1,NCPART(ICOLOR,2)
            IES=LLOOP(ICPART  ,ICOLOR,2)
            IEE=LLOOP(ICPART+1,ICOLOR,2)-1
!ocl nosimd
!ocl noswp
            DO 620 IE=IES,IEE
C
               IP1=NODE(1,IE)
               IP2=NODE(2,IE)
               IP3=NODE(3,IE)
               IP4=NODE(4,IE)
               IP5=NODE(5,IE)
C
               B(IP1)=B(IP1)-COEF*DIV(IE)*SN(1,IE)
               B(IP2)=B(IP2)-COEF*DIV(IE)*SN(2,IE)
               B(IP3)=B(IP3)-COEF*DIV(IE)*SN(3,IE)
               B(IP4)=B(IP4)-COEF*DIV(IE)*SN(4,IE)
               B(IP5)=B(IP5)-COEF*DIV(IE)*SN(5,IE)
 620        CONTINUE
 621     CONTINUE
 622  CONTINUE
C
C     OPERATION COUNTS:    18 FLOP /ELEMENT
C     DATA LOADINGS   :    19 WORDS/ELEMENT
C                      (    1 WORDS CONTIGUOUSLY,
C                          12 WORDS BY STRIDE, AND
C                          21 WORDS BY LIST )
C
      DO 632 ICOLOR=1,NCOLOR(3)
!ocl norecurrence(B)
         DO 631 ICPART=1,NCPART(ICOLOR,3)
            IES=LLOOP(ICPART  ,ICOLOR,3)
            IEE=LLOOP(ICPART+1,ICOLOR,3)-1
!ocl nosimd
!ocl noswp
            DO 630 IE=IES,IEE
C
               IP1=NODE(1,IE)
               IP2=NODE(2,IE)
               IP3=NODE(3,IE)
               IP4=NODE(4,IE)
               IP5=NODE(5,IE)
               IP6=NODE(6,IE)
C
               B(IP1)=B(IP1)-COEF*DIV(IE)*SN(1,IE)
               B(IP2)=B(IP2)-COEF*DIV(IE)*SN(2,IE)
               B(IP3)=B(IP3)-COEF*DIV(IE)*SN(3,IE)
               B(IP4)=B(IP4)-COEF*DIV(IE)*SN(4,IE)
               B(IP5)=B(IP5)-COEF*DIV(IE)*SN(5,IE)
               B(IP6)=B(IP6)-COEF*DIV(IE)*SN(6,IE)
 630        CONTINUE
 631     CONTINUE
 632  CONTINUE
C
C     OPERATION COUNTS:    24 FLOP /ELEMENT
C     DATA LOADINGS   :    21 WORDS/ELEMENT
C                      (    1 WORDS CONTIGUOUSLY,
C                          16 WORDS BY STRIDE, AND
C                           4 WORDS BY LIST )
C
      DO 642 ICOLOR=1,NCOLOR(4)
!ocl norecurrence(B)
         DO 641 ICPART=1,NCPART(ICOLOR,4)
            IES=LLOOP(ICPART  ,ICOLOR,4)
            IEE=LLOOP(ICPART+1,ICOLOR,4)-1
!ocl nosimd
!ocl noswp
            DO 640 IE=IES,IEE
C
               IP1=NODE(1,IE)
               IP2=NODE(2,IE)
               IP3=NODE(3,IE)
               IP4=NODE(4,IE)
               IP5=NODE(5,IE)
               IP6=NODE(6,IE)
               IP7=NODE(7,IE)
               IP8=NODE(8,IE)
C
               B(IP1)=B(IP1)-COEF*DIV(IE)*SN(1,IE)
               B(IP2)=B(IP2)-COEF*DIV(IE)*SN(2,IE)
               B(IP3)=B(IP3)-COEF*DIV(IE)*SN(3,IE)
               B(IP4)=B(IP4)-COEF*DIV(IE)*SN(4,IE)
               B(IP5)=B(IP5)-COEF*DIV(IE)*SN(5,IE)
               B(IP6)=B(IP6)-COEF*DIV(IE)*SN(6,IE)
               B(IP7)=B(IP7)-COEF*DIV(IE)*SN(7,IE)
               B(IP8)=B(IP8)-COEF*DIV(IE)*SN(8,IE)
 640        CONTINUE
 641     CONTINUE
 642  CONTINUE
C
      DO 700 IP = 1 , NP
          B(IP) = B(IP)/APRS0(IP)
  700 CONTINUE
C
C
      CALL DDCOMX(IPART,IDIMP,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *            B,B,B,NP,IUT0,IERR,RX,RY,MAXBUF)
      IF(IERR.NE.0) THEN
          WRITE(IUT0,*)
          WRITE(IUT0,*) ERMSGC
          RETURN
      ENDIF
C
CLES3X [5.] STARTING A  LOOP OF B.C. SETS AND MATRIX SOLVER
C
      IF (JSET.EQ.0) THEN
          NITRE=1
          NMAXD =NMAX  
      ELSE 
          NITRE=MAX(NMAX/NITRI,1)
          NMAXD =NITRI  
      ENDIF
C
      NRN =0
      IRES=0
C
C
      DO 3000 IITRE = 0, NITRE
C
          IF (IITRE.EQ.0) GOTO 3100
C
CLES3X [5.1] SOLVE P-EQUATION
C
      IF(IPRESS.EQ.1) THEN
          CALL BCFIX2 (ME,MP,NE,NP,N1,N2,NEX,NODE,
     *                 NPFIX,LPFIX,LWORK,
     *                 APRS,B,PN,SN,CM,IUT0,IERR)
          CALL E2PMAT(MCOLOR,MCPART,NCOLOR,NCPART,LLOOP,
     *                N2,N1,NE,NEX,NCRS,APRS,AAAPC,LTAB,IUT0,IERR)
      ELSE IF(IPRESS.EQ.2) THEN
          CALL BCFIX2 (ME,MP,NE,NP,N1,N2,NEX,NODE,
     *                 NPFIX,LPFIX,LWORK,
     *                 AAA,B,PN,SN,CM,IUT0,IERR)
          CALL E2PMAT(MCOLOR,MCPART,NCOLOR,NCPART,LLOOP,
     *                N2,N1,NE,NEX,NCRS,AAA,AAAPC,LTAB,IUT0,IERR)
      ENDIF
      IF (IERR.NE.0) THEN
          WRITE(IUT0,*)
          WRITE(IUT0,*) ERMSGC
          RETURN
      ENDIF
C
C          OPERATION COUNTS:   4  FLOP /NODE/NPPAVE/ITERATION
C          DATA LOADINGS   :   6  WORDS/NODE/NPPAVE/ITERATION
C                           (  4  WORDS CONTIGUOUSLY,
C                              0  WORDS BY 4-WORD STRIDE, AND
C                              2  WORDS BY LIST )
C           NPPAVE: AVE. NUMBER OF ADJACENT NODES AT A NODE
C           --> 15 (TET), 20 (PRD,WED), 27(HEX.) 
C
          IF(JFSPRS.EQ.0) THEN
              DO 801 IP=1,NP
                  DP(IP)=PN(IP)
 801          CONTINUE
          ELSE IF(JFSPRS.EQ.1) THEN
              DO 802 IP=1,NP
                  DP(IP)=0.0E0
 802           CONTINUE
          ELSE
              IERR=51
              RETURN     
          ENDIF
C
          IF(JUNROL.EQ.1)
     *    CALL CRSCVA(NP,NPPMAX,NCRS,NCRS2,NPP,AAAPC,TACRS,ICRS_T)
C
      CALL USTSTA(93)
          CALL BCGS2X(IBCGS,IPART,NMAXD,EPS,EPSRE,ME,N2,NE,NEX,
     *                NP,NPP,NCRS,IPCRS,AAAPC,NODE,B,DP,NRND,RES,
     *                NDOM,MBPDOM,LDOM,NBPDOM,IPSLF,IPSND,NUMIP,
     *                UG,VG,WG,DIV,WRK1,WRK2,WRK3,LWORK,RX,RY,MWRK,WRKN,
     *                JUNROL,NPPMAX,NCRS2,TS,TACRS,ITPCRS,
     *                IUT0,IERR)
      CALL USTEND(93)
C
          CALL AVEPRT(NP,DP,
     *                IPART,IDIMP,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *                IUT0,IERR,RX,RY,MAXBUF,NUMIP,UG)
C
          IF(JFSPRS.EQ.0) THEN
              DO 820 IP=1,NP
                  PN(IP)=DP(IP)
 820          CONTINUE
          ELSE IF(JFSPRS.EQ.1) THEN
              DO 821 IP=1,NP
                  PN(IP)=PN(IP)+DP(IP)
 821          CONTINUE
          ELSE
              IERR=101
              RETURN
          ENDIF
C
          NRN=NRN+NRND
          IF(NRND.LT.NMAXD)IRES=1 
C
 3100     CONTINUE
C
CLES3X [5.2] SET B.C.
C
C
CCYY   A. FREE BOUNDARY CONDITIONS
C
C
*POPTION INDEP(PN)
C*$*ASSERT PERMUTATION ( LPFREE )
*POPTION INDEP(PN)
          DO 3200 IB = 1 , NPFREE
              IP     = LPFREE(IB)
              PN(IP) = PSFREE
 3200     CONTINUE
C
          DO 3250 IB = 1 , NPSLD2
              IP     = LPSLD2(IB)
              PN(IP) = 0.0E0
 3250     CONTINUE
C
          IF (JSET.GE.1) THEN
              CALL OVRST1(IPART,NPSET,N1,N2,ME,NE,NP,NEX,NODE,PN,
     *                    LPSET1,LPSET2,LPSET3,
     *                    COVER1,COVER2,COVER3,
     *                    NDOM,MBPDOM,NPSND,NPRCV,
     *                    LPSND,NPTSND,LPRCV,NPTRCV,IPSET,IPSRC,
     *                    WRK1,WRK2,RX,RY,IUT0,IERR)
          ENDIF
C
      IF(IRES.EQ.1) GOTO 4000
C
 3000 CONTINUE
C
 4000 CONTINUE
C
C
C
CLES3X [6.] CAL. PESSURE AT ELEMENTS
C
C     OPERATION COUNTS:      4 FLOP /ELEMENT
C     DATA LOADINGS   :      8 WORDS/ELEMENT
C                      (     0 WORDS CONTIGUOUSLY,
C                            4 WORDS BY STRIDE, AND
C                            4 WORDS BY LIST )
C
      COEF=1.0E0/4.0E0
      DO 910 IE = IES1 , IEE1
          IP1=NODE(1,IE)
          IP2=NODE(2,IE)
          IP3=NODE(3,IE)
          IP4=NODE(4,IE)
          P(IE) = COEF*(PN(IP1)+PN(IP2)+PN(IP3)+PN(IP4))
          DPE(IE)=COEF*(DP(IP1)+DP(IP2)+DP(IP3)+DP(IP4))
  910 CONTINUE
C
C     OPERATION COUNTS:      5 FLOP /ELEMENT
C     DATA LOADINGS   :     10 WORDS/ELEMENT
C                      (     0 WORDS CONTIGUOUSLY,
C                            5 WORDS BY STRIDE, AND
C                            5 WORDS BY LIST )
      COEF=1.0E0/5.0E0
      DO 920 IE = IES2 , IEE2
          IP1=NODE(1,IE)
          IP2=NODE(2,IE)
          IP3=NODE(3,IE)
          IP4=NODE(4,IE)
          IP5=NODE(5,IE)
          P(IE) = COEF*(PN(IP1)+PN(IP2)+PN(IP3)+PN(IP4)
     *                 +PN(IP5))
          DPE(IE)=COEF*(DP(IP1)+DP(IP2)+DP(IP3)+DP(IP4)
     *                 +DP(IP5))
  920 CONTINUE
C
C     OPERATION COUNTS:      6 FLOP /ELEMENT
C     DATA LOADINGS   :     12 WORDS/ELEMENT
C                      (     0 WORDS CONTIGUOUSLY,
C                            6 WORDS BY STRIDE, AND
C                            6 WORDS BY LIST )
C
      COEF=1.0E0/6.0E0
      DO 930 IE = IES3 , IEE3
          IP1=NODE(1,IE)
          IP2=NODE(2,IE)
          IP3=NODE(3,IE)
          IP4=NODE(4,IE)
          IP5=NODE(5,IE)
          IP6=NODE(6,IE)
          P(IE) = COEF*(PN(IP1)+PN(IP2)+PN(IP3)+PN(IP4)
     *                 +PN(IP5)+PN(IP6))
          DPE(IE)=COEF*(DP(IP1)+DP(IP2)+DP(IP3)+DP(IP4)
     *                 +DP(IP5)+DP(IP6))
  930 CONTINUE
C
C     OPERATION COUNTS:      8 FLOP /ELEMENT
C     DATA LOADINGS   :     16 WORDS/ELEMENT
C                      (     0 WORDS CONTIGUOUSLY,
C                            8 WORDS BY STRIDE, AND
C                            8 WORDS BY LIST )
C
      COEF=1.0E0/8.0E0
      DO 940 IE = IES4 , IEE4
          IP1=NODE(1,IE)
          IP2=NODE(2,IE)
          IP3=NODE(3,IE)
          IP4=NODE(4,IE)
          IP5=NODE(5,IE)
          IP6=NODE(6,IE)
          IP7=NODE(7,IE)
          IP8=NODE(8,IE)
          P(IE) = COEF*(PN(IP1)+PN(IP2)+PN(IP3)+PN(IP4)
     *                 +PN(IP5)+PN(IP6)+PN(IP7)+PN(IP8))
          DPE(IE)=COEF*(DP(IP1)+DP(IP2)+DP(IP3)+DP(IP4)
     *                 +DP(IP5)+DP(IP6)+DP(IP7)+DP(IP8))
  940 CONTINUE
C
      RETURN
      END
