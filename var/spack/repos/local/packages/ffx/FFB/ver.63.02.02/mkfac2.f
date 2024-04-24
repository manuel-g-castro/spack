C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.8.1                                   C
C                                                                      C
C  SUB ROUTINE : MKFAC2                                                C
C                                                                      C
C                                       WRITTEN BY H.YOSHIMURA         C
C                                                                      C
C                                                                      C
C CONTACT ADDRESS : IIS, THE UNIVERSITY OF TOKYO, CISS                 C
C                                                                      C
C THERMO-FLUID ANALYSIS SOLVERS FOR LARGE-SCALE-ASSEMBLY               C
C                                                                      C
C======================================================================C
      SUBROUTINE MKFAC2(IPART,NE,NP,N2,NSP,NS,MEP,MFACE,MBF,
     *                  NODE,LOCAL,NEP,IENP,X,Y,Z,FE,
     *                  NPWALL,NPINLT,NPFREE,NPSYMT,
     *                  LPWALL,LPINLT,LPFREE,LPSYMT,
     *                  NFACE,LFACE,AVEC,DVEC,
     *                  NFWALL,NFINLT,NFFREE,NFSYMT,
     *                  LFWALL,LFINLT,LFFREE,LFSYMT,FINLT,
     *                  LWRK01,LEFACE,IUT6,IUT0,IERR)
C
      IMPLICIT NONE
C
C     [INPUT]
      INTEGER*4 IPART,NE,NP,N2,NSP,NS,MEP,MFACE,MBF
      INTEGER*4 NODE(N2,NE),LOCAL(NSP,NS,4),NEP(NP),IENP(MEP,NP)
      REAL*8    X(NP),Y(NP),Z(NP)
      REAL*4    FE(NE)
      INTEGER*4 NPWALL,NPINLT,NPFREE,NPSYMT
      INTEGER*4 LPWALL(NPWALL),LPINLT(NPINLT),
     *          LPFREE(NPFREE),LPSYMT(NPSYMT)
C
C     [IN-OUTPUT]
      INTEGER*4 NFACE
      INTEGER*4 LFACE(5,MFACE)
      REAL*4    AVEC(4,MFACE),DVEC(3,MFACE)
      INTEGER*4 NFWALL,NFINLT,NFFREE,NFSYMT
      INTEGER*4 LFWALL(MBF),LFINLT(MBF),LFFREE(MBF),LFSYMT(MBF)
      REAL*4    FINLT(MBF)
      INTEGER*4 LEFACE(6,NE)
      INTEGER*4 IUT6,IUT0,IERR
C
C     [WORK]
      INTEGER*4 LWRK01(NP)
C
C     [LOCAL]
      INTEGER*4 IP,IP1,IP2,IP3,IP4
      INTEGER*4 IE,IETYPE,NLS,IS,IBF
      INTEGER*4 IBP,IB1,IB2,IB3,IB4,IBTYPE
C
      CHARACTER*60 ERMSGC
     * /' ## SUBROUTINE MKFAC1: ERROR OCCURED             ; RETURNED' /
      CHARACTER*60 EREXP1
     * / ' THE NUMBER OF FACE IS OVER MFACE                         ' /
      CHARACTER*60 EREXP2
     * / ' THE NUMBER OF WALL     BOUNDARY FACE IS OVER MBF         ' /
      CHARACTER*60 EREXP3
     * / ' THE NUMBER OF INLET    BOUNDARY FACE IS OVER MBF         ' /
      CHARACTER*60 EREXP4
     * / ' THE NUMBER OF FREE     BOUNDARY FACE IS OVER MBF         ' /
      CHARACTER*60 EREXP5
     * / ' THE NUMBER OF SYMMETRY BOUNDARY FACE IS OVER MBF         ' /
CC
CCHY [1] SET BOUNDARY TYPE
CC
      DO 1000 IP=1,NP
         LWRK01(IP)=0
 1000 CONTINUE
C
      DO 1100 IBP=1,NPWALL
         IP=LPWALL(IBP)
         LWRK01(IP)=1
 1100 CONTINUE
      DO 1200 IBP=1,NPINLT
         IP=LPINLT(IBP)
         LWRK01(IP)=2
 1200 CONTINUE
      DO 1300 IBP=1,NPFREE
         IP=LPFREE(IBP)
         LWRK01(IP)=3
 1300 CONTINUE
      DO 1400 IBP=1,NPSYMT
         IP=LPSYMT(IBP)
         LWRK01(IP)=4
 1400 CONTINUE
CC
CCHY [2] MAKE BOUNDARY SURFACE LIST
CC
      DO 2000 IBF=1,MBF
         LFWALL(IBF)=0
         LFINLT(IBF)=0
         LFFREE(IBF)=0
         LFSYMT(IBF)=0
 2000 CONTINUE
C
      NFWALL=0
      NFINLT=0
      NFFREE=0
      NFSYMT=0
      DO 2100 IE=1,NE
         IF(     NODE(8,IE).GE.1) THEN ! HEX
            IETYPE=4
            NLS=6
         ELSE IF(NODE(6,IE).GE.1) THEN ! PRS
            IETYPE=3
            NLS=5
         ELSE IF(NODE(5,IE).GE.1) THEN ! PYR
            IETYPE=2
            NLS=5
         ELSE                          ! TET
            IETYPE=1
            NLS=4
         ENDIF
         DO 2200 IS=1,NLS
            IF (LEFACE(IS,IE).NE.0) GOTO 2200
C
            IBTYPE=0
            IP1=NODE(LOCAL(1,IS,IETYPE),IE)
            IB1=LWRK01(IP1)
            IF (IB1.EQ.0) GOTO 2200
            IBTYPE=MAX(IBTYPE,IB1)
C
            IP2=NODE(LOCAL(2,IS,IETYPE),IE)
            IB2=LWRK01(IP2)
            IF (IB2.EQ.0) GOTO 2200
            IBTYPE=MAX(IBTYPE,IB2)
C
            IP3=NODE(LOCAL(3,IS,IETYPE),IE)
            IB3=LWRK01(IP3)
            IF (IB3.EQ.0) GOTO 2200
            IBTYPE=MAX(IBTYPE,IB3)
C
            IF ((IETYPE.EQ.1            ).OR. ! TRI
     *          (IETYPE.EQ.2.AND.IS.LE.4).OR.
     *          (IETYPE.EQ.3.AND.IS.LE.2)) THEN
               IP4=0
            ELSE                              ! QUAD
               IP4=NODE(LOCAL(4,IS,IETYPE),IE)
               IB4=LWRK01(IP4)
               IF (IB4.EQ.0) GOTO 2200
               IBTYPE=MAX(IBTYPE,IB4)
            ENDIF
C
            NFACE=NFACE+1
            IF (NFACE.GT.MFACE) THEN
               WRITE(IUT0,*) EREXP1,MFACE
               GOTO 9999
            ENDIF
            LEFACE(IS,IE)=IE
            LFACE(1,NFACE)=IE
            LFACE(2,NFACE)=0
            LFACE(3,NFACE)=IS
            LFACE(4,NFACE)=0
            LFACE(5,NFACE)=0
            CALL CALAVC(IE,IS,NE,NP,N2,NSP,NS,NODE,LOCAL,
     *                  X,Y,Z,AVEC(1,NFACE))
C
            IF      (IBTYPE.EQ.1) THEN
               NFWALL=NFWALL+1
               IF (NFWALL.GT.MBF) THEN
                  WRITE(IUT0,*) EREXP2,MBF
                  GOTO 9999
               ENDIF
               LFWALL(NFWALL)=NFACE
            ELSE IF (IBTYPE.EQ.2) THEN
               NFINLT=NFINLT+1
               IF (NFINLT.GT.MBF) THEN
                  WRITE(IUT0,*) EREXP3,MBF
                  GOTO 9999
               ENDIF
               LFINLT(NFINLT)=NFACE
            ELSE IF (IBTYPE.EQ.3) THEN
               NFFREE=NFFREE+1
               IF (NFFREE.GT.MBF) THEN
                  WRITE(IUT0,*) EREXP4,MBF
                  GOTO 9999
               ENDIF
               LFFREE(NFFREE)=NFACE
            ELSE IF (IBTYPE.EQ.4) THEN
               NFSYMT=NFSYMT+1
               IF (NFSYMT.GT.MBF) THEN
                  WRITE(IUT0,*) EREXP5,MBF
                  GOTO 9999
               ENDIF
               LFSYMT(NFSYMT)=NFACE
            ENDIF
C
 2200    CONTINUE
 2100 CONTINUE
C
      WRITE(IUT6,*) 'NUMBER OF WALL     BOUNDARY FACE : ',NFWALL
      WRITE(IUT6,*) 'NUMBER OF INLET    BOUNDARY FACE : ',NFINLT
      WRITE(IUT6,*) 'NUMBER OF FREE     BOUNDARY FACE : ',NFFREE
      WRITE(IUT6,*) 'NUMBER OF SYMMETRY BOUNDARY FACE : ',NFSYMT
C
CC
CCHY [3] SET VOLUME FRACTION OF INLET BOUNDARY FACE
CC
      DO 3000 IBF=1,NFINLT
         IE=LFACE(1,LFINLT(IBF))
         IF (FE(IE).GE.0.5E0) THEN
            FINLT(IBF)=1.0E0
         ELSE
            FINLT(IBF)=0.0E0
         ENDIF
 3000 CONTINUE
C
      RETURN
C
 9999 CONTINUE
      WRITE(IUT0,*)
      WRITE(IUT0,*) ERMSGC
      IERR=1
C
      RETURN
      END
