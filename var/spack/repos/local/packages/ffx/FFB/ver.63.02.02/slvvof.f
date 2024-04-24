C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.8.1                                   C
C                                                                      C
C  SUB ROUTINE : SLVVOF                                                C
C                                                                      C
C                                       WRITTEN BY H.YOSHIMURA         C
C                                                                      C
C                                                                      C
C CONTACT ADDRESS : IIS, THE UNIVERSITY OF TOKYO, CISS                 C
C                                                                      C
C THERMO-FLUID ANALYSIS SOLVERS FOR LARGE-SCALE-ASSEMBLY               C
C                                                                      C
C======================================================================C
      SUBROUTINE SLVVOF(ITIME,IVOF,NSCYC,
     *                  NE,NP,NFACE,NFACE1,NFACE2,NFACE3,
     *                  N2,NEX,NSP,NS,MELM,NODE,CM,
     *                  LOCAL,LFACE,LEFACE,
     *                  MEP,MP,NEP,IENP,
     *                  U,V,W,DELTA,AVEC,DVEC,DT,
     *                  NPWALL,NPINLT,LPWALL,LPINLT,
     *                  NFWALL,NFINLT,NFFREE,NFSYMT,
     *                  LFWALL,LFINLT,LFFREE,LFSYMT,FINLT,
     *                  IPART,NDOM,MBPDOM,LDOM,NBPDOM,IPSLF,IPSND,
     *                  NDOMF,MBFDOM,LDOMF,NBFDOM,IFSLF,IFSND,
     *                  EPS,EPSRE,NMAX,
     *                  FE,FFA,FVOL,FMIN,FMAX,CMAX,FLXIN,FLXOUT,
     *                  NPC,NERR,RES,
     *                  NITR,NEFLD2,NPFLD2,LEFLD2,LPFLD2,
     *                  MWRK,LEKIND,LWRK01,LWRK02,LWRK03,LWRK04,
     *                  AD,A,FXYZ,RHS,CD,FEO,
     *                  WRK01,WRK02,WRK03,WRK04,WRK05,WRK06,WRK07,
     *                  BUFSND,BUFRCV,
     *                  FLUX,BETAF,EFLX,
     *                  FEWRKO,FEWRK,DLWRK,
     *                  FBWRK1,FBWRK2,FBWRK3,FBWRK4,
     *                  EAP1,
     *                  IUT6,IUT0,IERR)
C
      IMPLICIT NONE
C
C     [INPUT]
      INTEGER*4 ITIME,IVOF,NSCYC,
     *          NE,NP,NFACE,NFACE1,NFACE2,NFACE3,
     *          N2,NEX(12),NSP,NS,MELM
      INTEGER*4 NODE(N2,NE),LOCAL(NSP,NS,4),LFACE(5,NFACE),LEFACE(6,NE)
      INTEGER*4 MEP,MP,NEP(NP),IENP(MEP,NP)
      REAL*4    CM(NP),U(NP),V(NP),W(NP),DELTA(NE)
      REAL*4    AVEC(4,NFACE),DVEC(3,NFACE),DT
      INTEGER*4 NPWALL,NPINLT,LPWALL(NPWALL),LPINLT(NPINLT)
      INTEGER*4 NFWALL,NFINLT,NFFREE,NFSYMT
      INTEGER*4 LFWALL(NFWALL),LFINLT(NFINLT),LFFREE(NFFREE),
     *          LFSYMT(NFSYMT)
      REAL*4    FINLT(NFINLT)
      INTEGER*4 IPART,
     *          NDOM,MBPDOM,LDOM(NDOM),NBPDOM(NDOM),
     *          IPSLF(MBPDOM,NDOM),IPSND(MBPDOM,NDOM),
     *          NDOMF,MBFDOM,LDOMF(NDOMF),NBFDOM(NDOMF),
     *          IFSLF(MBFDOM,NDOMF),IFSND(MBFDOM,NDOMF)
      REAL*4    EPS,EPSRE
      INTEGER*4 NMAX
C     [IN-OUTPUT]
      REAL*4    FE(NE),FFA(NFACE),FVOL,FMIN,FMAX,CMAX,FLXIN,FLXOUT,RES
      INTEGER*4 NITR,NEFLD2,NPFLD2,LEFLD2(NE),LPFLD2(NP),
     *          IUT6,IUT0,IERR
      REAL*4    EAP1(N2,MEP,NP)
C
C     [WORK]
      INTEGER*4 MWRK,LEKIND(NE),LWRK01,LWRK02,LWRK03,LWRK04
      REAL*4    AD(NE),A(6,NE),FXYZ(3,NE),RHS(NE),CD(NE),FEO(NE)
      REAL*4    WRK01,WRK02,WRK03,WRK04,WRK05,WRK06,WRK07,BUFSND,BUFRCV
      REAL*4    FLUX(NFACE),BETAF(NFACE),EFLX(NFACE)
      REAL*4    FEWRKO(NFACE3),FEWRK (NFACE3),DLWRK (NFACE3),
     *          FBWRK1(NFACE3),FBWRK2(NFACE3),FBWRK3(NFACE3),
     *          FBWRK4(NFACE3)
C
C     [LOCAL]
      INTEGER*4 ISCYC
      INTEGER*4 IE,IE1,IE2,IEE,IED,IEA,IFACE,IFACE3,IS,IS1,IS2,IBF
      INTEGER*4 IEKIND1,IEKIND2,MAXBUF,IDIM,NERR,NPC,NPCMAX,NVAL,
     *          IERRA,I,IP,IBP,ND
      REAL*4 RK,DTS,TIMES,AX,AY,AZ,DA
      REAL*4 VAL1,VAL2,VAL3,DVX,DVY,DVZ,CDD,FXYZD1,FXYZD2,FXYZD3
      REAL*4 FED,FEA,FEU,RR,AA,BB,CC,THETA,BFLXD,BFLXA
      REAL*4 FEDN,FFAN,FFCBCN,FFUQN,GAMMAF,
     *       FEDO,FEAO,DELTAD,CF,BFC,DELF,ERF,DEPS
      REAL*4 UOLD,VOLD,WOLD
      DATA RK     /1.0E0 /
      DATA NPCMAX /2     /
C      DATA NPCMAX /10     /
      DATA DEPS   /1.0E-6/
C
      CHARACTER*60 ERMSGC
     * /' ## SUBROUTINE SLVVOF: ERROR OCCURED             ; RETURNED' /
C
C
      MAXBUF=5*NFACE
      DTS=DT/FLOAT(NSCYC)
      TIMES=0.0E0
      ISCYC=1
C
      IF (ITIME.EQ.0) GOTO 6000
C
CC
CCHY  [1] EXTRACT SURFACE ANT ITS NEIGHBOR ELEMENT
CC
      DO IE=1,NE
         LEKIND(IE)=1
      ENDDO
C
CC
CCHY  [2] CALCULATE VELOCITY AT FACE
CC
      CALL CALFLX(NE,NP,NFACE,NSP,NS,NODE,LOCAL,LFACE,
     *            U,V,W,AVEC,FLUX,IUT6,IERR)
C
CC-------------------------------------------------------------------------------
CC    SUBCYCL LOOP --- START
CC-------------------------------------------------------------------------------
 9000 CONTINUE
      IF (ISCYC.EQ.NSCYC) DTS=DT-TIMES
      TIMES=TIMES+DTS
C
CC
CCHY  [3] CALCULATE GRADIENT OF VOLUME FUNCTION AND COURANT NUMBER IN EACH ELEMENT
CC
      DO 1000 IE=1,NE
         FXYZ(1,IE)=0.0E0
         FXYZ(2,IE)=0.0E0
         FXYZ(3,IE)=0.0E0
         CD(IE)=0.0E0
 1000 CONTINUE
C
      DO 1100 IFACE=1,NFACE
C
         IE1=LFACE(1,IFACE)
         IE2=LFACE(2,IFACE)
C
         AX = AVEC(1,IFACE)
         AY = AVEC(2,IFACE)
         AZ = AVEC(3,IFACE)
         DA = AVEC(4,IFACE)
         VAL1=FFA(IFACE)*AX*DA
         VAL2=FFA(IFACE)*AY*DA
         VAL3=FFA(IFACE)*AZ*DA
C
         FXYZ(1,IE1)=FXYZ(1,IE1)+VAL1 / DELTA(IE1)
         FXYZ(2,IE1)=FXYZ(2,IE1)+VAL2 / DELTA(IE1)
         FXYZ(3,IE1)=FXYZ(3,IE1)+VAL3 / DELTA(IE1)
         CD(IE1)=CD(IE1)+AMAX1( FLUX(IFACE)*DTS/DELTA(IE1),0.0E0)
C
         IF (IE2.LE.0) GOTO 1100
C
         FXYZ(1,IE2)=FXYZ(1,IE2)-VAL1 / DELTA(IE2)
         FXYZ(2,IE2)=FXYZ(2,IE2)-VAL2 / DELTA(IE2)
         FXYZ(3,IE2)=FXYZ(3,IE2)-VAL3 / DELTA(IE2)
         CD(IE2)=CD(IE2)+AMAX1(-FLUX(IFACE)*DTS/DELTA(IE2),0.0E0)
C
 1100 CONTINUE
C
      CMAX=0.0E0
      DO 1150 IE=1,NE
         CMAX=AMAX1(CD(IE),CMAX)
 1150 CONTINUE
C
CC
CCHY  [4] COMMUNICATE FXYZ, CD AND FE
CC
      IF (IPART.NE.0) THEN
      DO 1200 IFACE3=1,NFACE3
         IFACE=NFACE1+NFACE2+IFACE3
         IE=LFACE(1,IFACE)
         FBWRK1(IFACE3)=FXYZ(1,IE)
         FBWRK2(IFACE3)=FXYZ(2,IE)
         FBWRK3(IFACE3)=FXYZ(3,IE)
         FBWRK4(IFACE3)=CD(IE)
         FEWRK(IFACE3)=FE(IE)
         DLWRK(IFACE3)=DELTA(IE)
 1200 CONTINUE
C
      IDIM=3
      CALL DDFAC1(IDIM,MBFDOM,NDOMF,LDOMF,NBFDOM,IFSLF,IFSND,NFACE3,
     *            FBWRK1,FBWRK2,FBWRK3,BUFSND,BUFRCV,MAXBUF,IUT0,IERR)
      CALL ERCHK2(IUT6,IPART,1,IERR,IERRA)
      IF(IERRA.NE.0) GOTO 9999
C
      CALL DDFAC1(IDIM,MBFDOM,NDOMF,LDOMF,NBFDOM,IFSLF,IFSND,NFACE3,
     *            FBWRK4,FEWRK,DLWRK,BUFSND,BUFRCV,MAXBUF,IUT0,IERR)
      CALL ERCHK2(IUT6,IPART,1,IERR,IERRA)
      IF(IERRA.NE.0) GOTO 9999
      ENDIF
C
      DO 1300 IE=1,NE
         FEO(IE)=FE(IE)
 1300 CONTINUE
C
      IF (IPART.NE.0) THEN
      DO 1400 IFACE3=1,NFACE3
         FEWRKO(IFACE3)=FEWRK(IFACE3)
 1400 CONTINUE
      ENDIF
C
CC
CCHY  [5] CALCULATE FLUX OF VOLUME FUNCTION AT FACE
CC
      DO 2000 IFACE=1,NFACE
C
         BETAF(IFACE)=0.0E0
         IF (FLUX(IFACE).EQ.0.0E0) GOTO 2000
C
         IE1=LFACE(1,IFACE)
         IE2=LFACE(2,IFACE)
C
         IF (IE2.EQ.0) GOTO 2000 ! BOUNDARY FACE
C
         IF (FLUX(IFACE).GT.0.0E0) THEN
C        IE1:DONOR IE2:ACCEPTOR
            FED=FE(IE1)
            FXYZD1=FXYZ(1,IE1)
            FXYZD2=FXYZ(2,IE1)
            FXYZD3=FXYZ(3,IE1)
            CDD=CD(IE1)
            IF (IE2.LT.0) THEN
               IFACE3=IFACE-NFACE1-NFACE2
               FEA=FEWRK(IFACE3)
            ELSE
               FEA=FE(IE2)
            ENDIF
            DVX= DVEC(1,IFACE)
            DVY= DVEC(2,IFACE)
            DVZ= DVEC(3,IFACE)
         ELSE
C        IE1:ACCEPTOR IE2:DONOR
            IF (IE2.LT.0) THEN
               IFACE3=IFACE-NFACE1-NFACE2
               FED=FEWRK(IFACE3)
               FXYZD1=FBWRK1(IFACE3)
               FXYZD2=FBWRK2(IFACE3)
               FXYZD3=FBWRK3(IFACE3)
               CDD=FBWRK4(IFACE3)
            ELSE
               FED=FE(IE2)
               FXYZD1=FXYZ(1,IE2)
               FXYZD2=FXYZ(2,IE2)
               FXYZD3=FXYZ(3,IE2)
               CDD=CD(IE2)
            ENDIF
            FEA=FE(IE1)
            DVX=-DVEC(1,IFACE)
            DVY=-DVEC(2,IFACE)
            DVZ=-DVEC(3,IFACE)
         ENDIF
C
         RR=FXYZD1*DVX+FXYZD2*DVY+FXYZD3*DVZ
         FEU=AMIN1( AMAX1( FEA-2.0E0*RR, 0.0E0 ), 1.0E0 )
C
         IF (ABS(FEA-FEU).LE.DEPS) GOTO 2000
         IF (ABS(RR)     .LE.DEPS) GOTO 2000
C
C     NORMALIZE F OF DONOR ELEMENT
         FEDN=(FED-FEU)/(FEA-FEU)
         IF (ABS(FEDN).LE.DEPS) FEDN=0.0E0
C
         IF (ABS(1.0E0-FEDN).LE.DEPS) GOTO 2000
C
C     CBC SCHEME
         IF (FEDN.GE.0.0E0.AND.FEDN.LE.1.0E0) THEN
            FFCBCN=FEDN/CDD
            FFCBCN=AMIN1(FFCBCN,1.0E0)
         ELSE IF (FEDN.LT.0.0E0.OR.FEDN.GT.1.0E0) THEN
            FFCBCN=FEDN
         ENDIF
C
C     UQ SCHEME
         IF (FEDN.GE.0.0E0.AND.FEDN.LE.1.0E0) THEN
            FFUQN=(8.0E0*CDD*FEDN
     *          +(1.0E0-CDD)*(6.0E0*FEDN+3.0E0))/8.0E0
            FFUQN=AMIN1(FFUQN,FFCBCN)
         ELSE IF (FEDN.LT.0.0E0.OR.FEDN.GT.1.0E0) THEN
            FFUQN=FEDN
         ENDIF
C
C     CALCLATE WIGHTING FACTOR
         AA=SQRT(FXYZD1*FXYZD1
     *          +FXYZD2*FXYZD2
     *          +FXYZD3*FXYZD3)
         BB=SQRT(DVX*DVX+DVY*DVY+DVZ*DVZ)
         THETA=ACOS( ABS(RR/(AA*BB)) )
         CC=RK*( COS(2.0E0*THETA)+1.0E0 )/2.0E0
         GAMMAF=AMIN1(CC,1.0E0)
C
         FFAN=GAMMAF*FFCBCN+(1.0E0-GAMMAF)*FFUQN
C
         BETAF(IFACE)=(FFAN-FEDN)/(1.0E0-FEDN)
 2000 CONTINUE
C
      DO 2100 IFACE=1,NFACE
         EFLX(IFACE)=0.0E0
 2100 CONTINUE
C
CC
CC
CC
      NPC=0
CC-------------------------------------------------------------------------------
CC    PREDICTOR-CORRECTOR LOOP --- START
CC-------------------------------------------------------------------------------
 2500 CONTINUE 
      NPC=NPC+1
C
CC
CCHY  [6] CALCULATE R.H.S
CC
      DO 3000 IE=1,NE
         RHS(IE)=FE(IE)
 3000 CONTINUE
C
      DO 3100 IFACE=1,NFACE
C
         IF (FLUX(IFACE).EQ.0.0E0) GOTO 3100
C
         IE1=LFACE(1,IFACE)
         IE2=LFACE(2,IFACE)
C
         IF (IE2.EQ.0) GOTO 3100
C
         IF (FLUX(IFACE).GT.0.0E0) THEN
C        IE1:DONOR IE2:ACCEPTOR
            FED=FE(IE1)
            IF (IE2.LT.0) THEN
               IFACE3=IFACE-NFACE1-NFACE2
               FEA=FEWRK(IFACE3)
            ELSE
               FEA=FE(IE2)
            ENDIF
         ELSE
C        IE1:ACCEPTOR IE2:DONOR
            IF (IE2.LT.0) THEN
               IFACE3=IFACE-NFACE1-NFACE2
               FED=FEWRK(IFACE3)
            ELSE
               FED=FE(IE2)
            ENDIF
            FEA=FE(IE1)
         ENDIF
C
         BFLXD=FLUX(IFACE)*(1.0E0-BETAF(IFACE))/2.0E0
         BFLXA=FLUX(IFACE)*(      BETAF(IFACE))/2.0E0
C
         IF (LEKIND(IE1).NE.0) THEN
            RHS(IE1)=RHS(IE1)
     *              -DTS/DELTA(IE1)*(BFLXD*FED+BFLXA*FEA+EFLX(IFACE))
         ENDIF
C
         IF (IE2.LT.0) GOTO 3100
         IF (LEKIND(IE2).NE.0) THEN
            RHS(IE2)=RHS(IE2)
     *              +DTS/DELTA(IE2)*(BFLXD*FED+BFLXA*FEA+EFLX(IFACE))
         ENDIF
C
 3100 CONTINUE
C
CC
CCHY  [7] CALCULATE COEFFICIENT MATRIX A
CC
      DO 3200 IE=1,NE
         AD(IE)=1.0E0
         DO 3300 IS=1,6
            A(IS,IE)=0.0E0
 3300    CONTINUE
 3200 CONTINUE
C
      DO 3400 IFACE=1,NFACE
C
         IF (FLUX(IFACE).EQ.0.0E0) GOTO 3400
C
         IE1=LFACE(1,IFACE)
         IE2=LFACE(2,IFACE)
         IS1=LFACE(3,IFACE)
         IS2=LFACE(4,IFACE)
C
         IF (IE2.EQ.0) GOTO 3400
C
         BFLXD=FLUX(IFACE)*(1.0E0-BETAF(IFACE))/2.0E0
         BFLXA=FLUX(IFACE)*(      BETAF(IFACE))/2.0E0
C
         IF (FLUX(IFACE).GT.0.0E0) THEN
C        IE1:DONOR IE2:ACCEPTOR
            IF (LEKIND(IE1).NE.0) THEN
               AD(    IE1)=AD(    IE1)+DTS/DELTA(IE1)*BFLXD
               A (IS1,IE1)= A(IS1,IE1)+DTS/DELTA(IE1)*BFLXA
            ENDIF
            IF (IE2.GT.0) THEN
               IF (LEKIND(IE2).NE.0) THEN
                  AD(    IE2)=AD(    IE2)-DTS/DELTA(IE2)*BFLXA
                  A (IS2,IE2)= A(IS2,IE2)-DTS/DELTA(IE2)*BFLXD
               ENDIF
            ENDIF
         ELSE
C        IE1:DONOR IE2:ACCEPTOR
            IF (LEKIND(IE1).NE.0) THEN
               AD(    IE1)=AD(    IE1)+DTS/DELTA(IE1)*BFLXA
               A (IS1,IE1)=A (IS1,IE1)+DTS/DELTA(IE1)*BFLXD
            ENDIF
            IF (IE2.GT.0) THEN
               IF (LEKIND(IE2).NE.0) THEN
                  AD(    IE2)=AD(    IE2)-DTS/DELTA(IE2)*BFLXD
                  A (IS2,IE2)=A (IS2,IE2)-DTS/DELTA(IE2)*BFLXA
               ENDIF
            ENDIF
         ENDIF
 3400 CONTINUE
C
CC
CCHY  [8] SET BOUNDARY CONDITION
CC
C     INLET BOUNDARY
      DO 3120 IBF=1,NFINLT
         IFACE=LFINLT(IBF)
         IE=LFACE(1,IFACE)
         RHS(IE)=RHS(IE)-DTS/DELTA(IE)*FLUX(IFACE)*FINLT(IBF)
 3120 CONTINUE
C
C     FREE BOUNDARY
      DO 3140 IBF=1,NFFREE
         IFACE=LFFREE(IBF)
         IE=LFACE(1,IFACE)
         IF (FLUX(IFACE).GT.0.0E0) THEN
            RHS(IE)=RHS(IE)-FE(IE)*DTS/DELTA(IE)*FLUX(IFACE)/2.0E0
            AD(IE)=AD(IE)+DTS/DELTA(IE)*FLUX(IFACE)/2.0E0
         ENDIF
 3140 CONTINUE
C
CC
CCHY  [9] DIAGONAL SCALING
CC
      DO 3500 IE=1,NE
         DO 3600 IS=1,6
            A(IS,IE)=A(IS,IE)/AD(IE)
 3600    CONTINUE
         RHS(IE)=RHS(IE)/AD(IE)
         AD(IE)=1.0E0
 3500 CONTINUE
C
      CALL BCGSVE(NE,NFACE,NFACE1,NFACE2,NFACE3,LEFACE,LFACE,
     *            A,AD,RHS,EPS,EPSRE,NMAX,NITR,FE,RES,
     *            IPART,MBFDOM,NDOMF,LDOMF,NBFDOM,IFSLF,IFSND,
     *            FEWRK,BUFSND,BUFRCV,WRK01,WRK02,WRK03,WRK04,WRK05,
     *            WRK06,IUT6,IUT0,IERR)
C
      DO 3700 IE=1,NE
         IF (FE(IE).LT.0.0E0) THEN
            ERF=AMAX1(-FE(IE),0.0E0)
            IF (ERF.LE.DEPS) THEN
               FE(IE)=0.0E0
            ENDIF
         ELSE IF (FE(IE).GT.1.0E0) THEN
            ERF=AMAX1(FE(IE)-1.0E0,0.0E0)
            IF (ERF.LE.DEPS) THEN
               FE(IE)=1.0E0
            ENDIF
         ELSE IF (FE(IE).LE.DEPS) THEN
            FE(IE)=0.0E0
         ELSE IF ((1.0E0-FE(IE)).LE.DEPS) THEN
            FE(IE)=1.0E0
         ENDIF
 3700 CONTINUE
C
CC
CCHY  [10] COMMUNICATE FE
CC
      IF (IPART.NE.0) THEN
      DO 3800 IFACE3=1,NFACE3
         IFACE=NFACE1+NFACE2+IFACE3
         IE=LFACE(1,IFACE)
         FEWRK(IFACE3)=FE(IE)
 3800 CONTINUE
C
      IDIM=1
      CALL DDFAC1(IDIM,MBFDOM,NDOMF,LDOMF,NBFDOM,IFSLF,IFSND,NFACE3,
     *            FEWRK,FEWRK,FEWRK,BUFSND,BUFRCV,MAXBUF,IUT0,IERR)
      CALL ERCHK2(IUT6,IPART,1,IERR,IERRA)
      IF(IERRA.NE.0) GOTO 9999
      ENDIF
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC
CC    2. CORRECTOR STEP
CC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      DO 3900 IFACE=1,NFACE
         EFLX (IFACE)=0.0E0
 3900 CONTINUE
C
      NERR=0
      DO 4000 IFACE=1,NFACE
C
         IF (FLUX(IFACE).EQ.0.0E0) GOTO 4000
C
         IE1=LFACE(1,IFACE)
         IE2=LFACE(2,IFACE)
C
         IF (IE2.EQ.0) GOTO 4000
C
         IF (FLUX(IFACE).GT.0.0E0) THEN
C        IE1:DONOR IE2:ACCEPTOR
            FED =FE (IE1)
            FEDO=FEO(IE1)
            DELTAD=DELTA(IE1)
            IF (IE2.LT.0) THEN
               IFACE3=IFACE-NFACE1-NFACE2
               FEA =FEWRK (IFACE3)
               FEAO=FEWRKO(IFACE3)
            ELSE
               FEA =FE (IE2)
               FEAO=FEO(IE2)
            ENDIF
         ELSE
C        IE1:ACCEPTOR IE2:DONOR
            IF (IE2.LT.0) THEN
               IFACE3=IFACE-NFACE1-NFACE2
               FED   =FEWRK (IFACE3)
               FEDO  =FEWRKO(IFACE3)
               DELTAD=DLWRK (IFACE3)
            ELSE
               FED =FE (IE2)
               FEDO=FEO(IE2)
               DELTAD=DELTA(IE2)
            ENDIF
            FEA =FE (IE1)
            FEAO=FEO(IE1)
         ENDIF
C
         IF (FED.GE.0.0E0.AND.FED.LE.1.0E0) GOTO 4000
C
         NERR=NERR+1
         DELF=0.5E0*(FEAO+FEA)-0.5E0*(FEDO+FED)
         CF=ABS(FLUX(IFACE)*DTS/DELTAD)
         BFC=0.0E0
         ERF=0.0E0
         IF (FED.LT.0.0E0) THEN
            ERF=AMAX1(-FED,0.0E0)
            IF (DELF.GT.ERF) THEN
               AA=2.0E0*CF*(DELF+ERF)
               IF (AA.NE.0.0E0) THEN
                  CC=ERF*(2.0E0-CF+2.0E0*CF*BETAF(IFACE))/AA
                  BFC=AMIN1(CC,BETAF(IFACE))
               ENDIF
            ENDIF
            BETAF(IFACE)=BETAF(IFACE)-BFC
            EFLX (IFACE)=FLUX(IFACE)*(-0.5E0+BETAF(IFACE))*ERF
         ELSE IF (FED.GT.1.0E0) THEN
            ERF=AMAX1(FED-1.0E0,0.0E0)
            IF (DELF.LT.(-ERF)) THEN
               AA=2.0E0*CF*(-DELF+ERF)
               IF (AA.NE.0.0E0) THEN
                  CC=ERF*(2.0E0-CF+2.0E0*CF*BETAF(IFACE))/AA
                  BFC=AMIN1(CC,BETAF(IFACE))
               ENDIF
            ENDIF
            BETAF(IFACE)=BETAF(IFACE)-BFC
            EFLX (IFACE)=FLUX(IFACE)*( 0.5E0-BETAF(IFACE))*ERF
         ENDIF
C
 4000 CONTINUE
C
      IF(IPART.GE.1) THEN
         VAL1=FLOAT(NERR)
         CALL DDCOM2(VAL1,VAL2)
         VAL1=VAL2
         NERR=NINT(VAL1)
      ENDIF
C
      IF (NERR.NE.0.AND.NPC.LT.NPCMAX) THEN
         DO 4100 IE=1,NE
            FE(IE)=FEO(IE)
 4100    CONTINUE
C
         IF (IPART.NE.0) THEN
            DO 4200 IFACE3=1,NFACE3
               FEWRK(IFACE3)=FEWRKO(IFACE3)
 4200       CONTINUE
         ENDIF
         GOTO 2500
      ELSE IF (NPC.EQ.NPCMAX) THEN
C         GOTO 9999
      ENDIF
CC-------------------------------------------------------------------------------
CC    PREDICTOR-CORRECTOR LOOP --- END
CC-------------------------------------------------------------------------------
C
CC
CCHY  [11] CALC VOLUME FLACTION AT FACE
CC
      DO 5100 IFACE=1,NFACE
         IE1=LFACE(1,IFACE)
         IE2=LFACE(2,IFACE)
C
         IF (IE2.EQ.0) GOTO 5100
C
         IF (FLUX(IFACE).GE.0.0E0) THEN
C        IE1:DONOR IE2:ACCEPTOR
            FED=FE(IE1)
            IF (IE2.LT.0) THEN
               IFACE3=IFACE-NFACE1-NFACE2
               FEA=FEWRK(IFACE3)
            ELSE
               FEA=FE(IE2)
            ENDIF
         ELSE
C        IE1:ACCEPTOR IE2:DONOR
            IF (IE2.LT.0) THEN
               IFACE3=IFACE-NFACE1-NFACE2
               FED=FEWRK(IFACE3)
            ELSE
               FED=FE(IE2)
            ENDIF
            FEA=FE(IE1)
         ENDIF
C
         FFA(IFACE)=(1.0E0-BETAF(IFACE))*FED+BETAF(IFACE)*FEA
 5100  CONTINUE
C
       ISCYC=ISCYC+1
       IF (ISCYC.LE.NSCYC) GOTO 9000
CC-------------------------------------------------------------------------------
CC    SUBCYCL LOOP --- END
CC-------------------------------------------------------------------------------
 6000  CONTINUE
C
CC
CCHY  [12] CALC TOTAL VOLUME OF FLUID1
CC
      FVOL= 0.0E0
      FMIN= 1.0E6
      FMAX=-1.0E6
      DO 6100 IE=1,NE
         FMIN=AMIN1(FMIN,FE(IE))
         FMAX=AMAX1(FMAX,FE(IE))
         FVOL=FVOL+FE(IE)*DELTA(IE)
 6100 CONTINUE
C
CC
CCHY  [13] CALC VOLUME FLACTION AT FACE
CC
      DO 7100 IBF=1,NFWALL
         IFACE=LFWALL(IBF)
         IE=LFACE(1,IFACE)
         FFA(IFACE)=FE(IE)
 7100 CONTINUE
C
      FLXIN=0.0E0
      DO 7200 IBF=1,NFINLT
         IFACE=LFINLT(IBF)
         IE=LFACE(1,IFACE)
         FFA(IFACE)=FINLT(IBF)
         FLXIN=FLXIN+FLUX(IFACE)*FFA(IFACE)
 7200 CONTINUE
C
      FLXOUT=0.0E0
      DO 7300 IBF=1,NFFREE
         IFACE=LFFREE(IBF)
         IE=LFACE(1,IFACE)
         FFA(IFACE)=FE(IE)
         FLXOUT=FLXOUT+FLUX(IFACE)*FFA(IFACE)
 7300 CONTINUE
C
      DO 7400 IBF=1,NFSYMT
         IFACE=LFSYMT(IBF)
         IE=LFACE(1,IFACE)
         FFA(IFACE)=FE(IE)
 7400 CONTINUE
C
      IF(IPART.GE.1) THEN
          CALL DDCOM2(FVOL,  VAL1)
          CALL DDCOM2(FLXIN, VAL2)
          CALL DDCOM2(FLXOUT,VAL3)
          FVOL  =VAL1
          FLXIN =VAL2
          FLXOUT=VAL3
      ENDIF
C
CC
CCHY  [14] MASK SECOND FLUID ELEMENT
CC
      IF (IVOF.EQ.1) THEN
         CALL MSKELM(ITIME,NE,NP,N2,NEX,MELM,NODE,MEP,MP,NEP,IENP,
     *               FE,DELTA,NPWALL,NPINLT,LPWALL,LPINLT,
     *               IPART,NDOM,MBPDOM,LDOM,NBPDOM,
     *               IPSLF,IPSND,BUFSND,BUFRCV,
     *               CM,U,V,W,NEFLD2,NPFLD2,LEFLD2,LPFLD2,
     *               MWRK,LWRK01,LWRK02,LWRK03,LWRK04,EAP1,
     *               WRK01,WRK02,WRK03,WRK04,WRK05,WRK06,WRK07,
     *               IUT0,IERR)
      ENDIF
C
      IF (ITIME.NE.0) RETURN
C
CC
CCHY  [15] CALCULATE VOLUME FRACTION AT FACE
CC
      IF (IPART.GE.1) THEN
         DO 8000 IFACE3=1,NFACE3
            IFACE=NFACE1+NFACE2+IFACE3
            IE=LFACE(1,IFACE)
            FBWRK1(IFACE3)=FE(IE)
 8000    CONTINUE
C
         IDIM=1
         CALL DDFAC1(IDIM,MBFDOM,NDOMF,LDOMF,NBFDOM,IFSLF,IFSND,NFACE,
     *             FBWRK1,FBWRK1,FBWRK1,BUFSND,BUFRCV,MAXBUF,IUT0,IERR)
         CALL ERCHK2(IUT6,IPART,1,IERR,IERRA)
         IF(IERRA.NE.0) GOTO 9999
      ENDIF
C
      DO 8100 IFACE=1,NFACE
         IE1=LFACE(1,IFACE)
         IE2=LFACE(2,IFACE)

         IF (IE2.EQ.0) GOTO 8100
C
         FED=FE(IE1)
         IF (IE2.LT.0) THEN
            IFACE3=IFACE-NFACE1-NFACE2
            FEA=FBWRK1(IFACE3)
         ELSE
            FEA=FE(IE2)
         ENDIF
C
         FFA(IFACE)=0.5E0*(FED+FEA)
 8100 CONTINUE
C
      RETURN
C
 9999 CONTINUE
      WRITE(IUT0,*) ERMSGC
      IERR=1
      RETURN
C
      END
