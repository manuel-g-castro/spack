C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.8.1                                   C
C                                                                      C
C  SUB ROUTINE : MSKELM                                                C
C                                                                      C
C                                       WRITTEN BY H.YOSHIMURA         C
C                                                                      C
C                                                                      C
C CONTACT ADDRESS : IIS, THE UNIVERSITY OF TOKYO, CISS                 C
C                                                                      C
C THERMO-FLUID ANALYSIS SOLVERS FOR LARGE-SCALE-ASSEMBLY               C
C                                                                      C
C======================================================================C
      SUBROUTINE MSKELM(ITIME,NE,NP,N2,NEX,MELM,NODE,MEP,MP,NEP,IENP,
     *                  FE,DELTA,NPWALL,NPINLT,LPWALL,LPINLT,
     *                  IPART,NDOM,MBPDOM,LDOM,NBPDOM,
     *                  IPSLF,IPSND,RX,RY,
     *                  CM,U,V,W,NEFLD2,NPFLD2,LEFLD2,LPFLD2,
     *                  MWRK,LEFIX,LPACT1,LPACT2,LWRK01,EAP1,
     *                  WRK01,WRK02,WRK03,WRK04,WRK05,WRK06,WRK07,
     *                  IUT0,IERR)
C
      IMPLICIT NONE
C
C     [INPUT]
      INTEGER*4 ITIME,NE,NP,N2,NEX(12),MELM,MP,
     *          NODE(N2,NE),MEP,IENP(MEP,NP),NEP(MP)
      REAL*4    FE(NE),DELTA(NE),EAP1(N2,MEP,NP)
      INTEGER*4 NPWALL,NPINLT,LPWALL(NPWALL),LPINLT(NPINLT)
      INTEGER*4 IPART,NDOM,MBPDOM
      INTEGER*4 LDOM(NDOM),NBPDOM(NDOM),
     *          IPSLF(MBPDOM,NDOM),IPSND(MBPDOM,NDOM),
     *          RX(NP*3),RY(NP*3)
C
C     [IN-OUTPUT]
      REAL*4    CM(NP),U(NP),V(NP),W(NP)
      INTEGER*4 NEFLD2,NPFLD2,LEFLD2(NE),LPFLD2(NP)
      INTEGER*4 IUT0,IERR
C
C     [WORK]
      INTEGER*4 MWRK,LEFIX(NE),LPACT1(NP),LPACT2(NP),LWRK01(NP)
      REAL*4    WRK01(MWRK),WRK02(MWRK),WRK03(MWRK),WRK04(MWRK),
     *          WRK05(MWRK),WRK06(MWRK),WRK07(MWRK)
C
C     [LOCAL]
      INTEGER*4 IE,IP,IBP,ND,I,IDUM,MAXBUF
      CHARACTER*60 ERMSGC
     * /' ## SUBROUTINE MSKELM: ERROR OCCURED             ; RETURNED' /
C
      MAXBUF=3*NP
C
      IF (ITIME.EQ.0) GOTO 1400
C
CC
CCHY  [1] INSERT OLD ACTIVE NODE TO LPACT1
CC
      DO 1000 IP=1,NP
         LPACT1(IP)=1
 1000 CONTINUE
C
      DO 1100 IBP=1,NPFLD2
         LPACT1(LPFLD2(IBP))=0
 1100 CONTINUE
C
      DO 1200 IBP=1,NPWALL
         LPACT1(LPWALL(IBP))=1
 1200 CONTINUE
C
      DO 1300 IBP=1,NPINLT
         LPACT1(LPINLT(IBP))=1
 1300 CONTINUE
C
 1400 CONTINUE
C       
CC
CCHY  [2] MASK NON-ACTIVE(F<0.5) ELEMENT AND NODE AT NEW TIME STEP
CC
      DO 2000 IP=1,NP
         LWRK01(IP)=0
 2000 CONTINUE
C
      NEFLD2=0
      DO 2100 IE=1,NE
         IF (FE(IE).LT.0.5) THEN
            NEFLD2=NEFLD2+1
            LEFLD2(NEFLD2)=IE
         ELSE
            DO 2200 I=1,8
               IP=NODE(I,IE)
               IF (IP.EQ.0) GOTO 2200
               LWRK01(IP)=1
 2200       CONTINUE
         ENDIF
 2100 CONTINUE
C
      DO 2300 IP=1,NP
         WRK01(IP)=FLOAT(LWRK01(IP))
 2300 CONTINUE
C
      IDUM=1
      CALL DDCOMX(IPART,IDUM,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *            WRK01,WRK01,WRK01,NP,IUT0,IERR,RX,RY,MAXBUF)
      IF(IERR.NE.0) GOTO 9999
C
      DO 2400 IP=1,NP
         IF (WRK01(IP).GT.0.0) THEN
            LWRK01(IP)=1
         ELSE
            LWRK01(IP)=0
         ENDIF
 2400 CONTINUE
C
      NPFLD2=0
      DO 2500 IP=1,NP
         IF (LWRK01(IP).EQ.0) THEN
            NPFLD2=NPFLD2+1
            LPFLD2(NPFLD2)=IP
         ENDIF
 2500 CONTINUE
C
CC
CCHY  [3] LUMP MAXX MATRIX
CC
      DO 3000 IE=1,NE
         LEFIX(IE)=0
 3000 CONTINUE
C
      DO 3100 IBP=1,NEFLD2
         LEFIX(LEFLD2(IBP))=1
 3100 CONTINUE
C
      CALL LMPEX2(N2,NE,NP,NEX,NODE,MELM,EAP1,IENP,NEP,MEP,MP,CM,LEFIX)
C
      IDUM=1
      CALL DDCOMX(IPART,IDUM,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *            CM,CM,CM,NP,IUT0,IERR,RX,RY,MAXBUF)
      IF(IERR.NE.0) GOTO 9999
C
      DO 3200 IP=1,NP
         IF (CM(IP).EQ.0.0E0) CM(IP)=1.0E0
         CM(IP)=1.0E0/CM(IP)
 3200 CONTINUE
C
      IF (ITIME.EQ.0) RETURN
C
CC
CCHY  [4] INSERT NEW ACTIVE NODE TO LPACT2
CC

      DO 4000 IP=1,NP
         LPACT2(IP)=1
 4000 CONTINUE
C
      DO 4100 IBP=1,NPFLD2
         LPACT2(LPFLD2(IBP))=0
 4100 CONTINUE
C
      DO 4200 IBP=1,NPWALL
         LPACT2(LPWALL(IBP))=1
 4200 CONTINUE
C
      DO 4300 IBP=1,NPINLT
         LPACT2(LPINLT(IBP))=1
 4300 CONTINUE
C
CC
CCHY  [5] CALC VELOCITY AT NEW ACTIVE NODE
CC
      DO 5100 IE=1,NE
         WRK01(IE)=0.0E0
         WRK02(IE)=0.0E0
         WRK03(IE)=0.0E0
         IF (LEFIX(IE).EQ.1) GOTO 5100
C
         ND=0
         DO 5200 I=1,8
            IP=NODE(I,IE)
            IF (IP.EQ.0) GOTO 5200
            IF (LPACT1(IP).EQ.0) GOTO 5200
            ND=ND+1
            WRK01(IE)=WRK01(IE)+U(IP)
            WRK02(IE)=WRK02(IE)+V(IP)
            WRK03(IE)=WRK03(IE)+W(IP)
 5200    CONTINUE
         WRK01(IE)=WRK01(IE)/FLOAT(ND)
         WRK02(IE)=WRK02(IE)/FLOAT(ND)
         WRK03(IE)=WRK03(IE)/FLOAT(ND)
 5100 CONTINUE
C
      DO 5300 IP=1,NP
         WRK04(IP)=0.0E0
         WRK05(IP)=0.0E0
         WRK06(IP)=0.0E0
         WRK07(IP)=0.0E0
         IF (LPACT1(IP).EQ.0.AND.LPACT2(IP).EQ.1) THEN
            DO 5400 I=1,NEP(IP)
               IE=IENP(I,IP)
               IF (LEFIX(IE).EQ.1) GOTO 5400
               WRK04(IP)=WRK04(IP)+WRK01(IE)*DELTA(IE)
               WRK05(IP)=WRK05(IP)+WRK02(IE)*DELTA(IE)
               WRK06(IP)=WRK06(IP)+WRK03(IE)*DELTA(IE)
               WRK07(IP)=WRK07(IP)+DELTA(IE)
 5400       CONTINUE
         ENDIF
 5300 CONTINUE
C
      IDUM=3
      CALL DDCOMX(IPART,IDUM,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *            WRK04,WRK05,WRK06,NP,IUT0,IERR,RX,RY,MAXBUF)
      IDUM=1
      CALL DDCOMX(IPART,IDUM,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *            WRK07,WRK07,WRK07,NP,IUT0,IERR,RX,RY,MAXBUF)
      IF(IERR.NE.0) GOTO 9999
C
      DO 5500 IP=1,NP
         IF (LPACT1(IP).EQ.0.AND.LPACT2(IP).EQ.1) THEN
            IF (WRK07(IP).EQ.0.0E0) THEN
               WRITE(IUT0,*)
     *'THERE ARE NODE HAVING NO ACTIVE NEIGHBOUR ELEMENT'
               GOTO 9999
            ENDIF
            U(IP)=WRK04(IP)/WRK07(IP)
            V(IP)=WRK05(IP)/WRK07(IP)
            W(IP)=WRK06(IP)/WRK07(IP)
         ENDIF
 5500 CONTINUE
C
      RETURN
C
 9999 CONTINUE
      WRITE(IUT0,*) ERMSGC
      IERR=1
      RETURN
C
      END
