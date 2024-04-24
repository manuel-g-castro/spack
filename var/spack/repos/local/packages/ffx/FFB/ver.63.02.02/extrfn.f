      SUBROUTINE EXTRFN(NGRID,NLAY1,NLAY2,IRFBOX,
     *                  ME,N2,NE,NP,NODE,X,Y,Z,
     *                  NPBODY,LPBODY,NPINLT,LPINLT,NPTGT,LPTGT,
     *                  XRFMIN,YRFMIN,ZRFMIN,XRFMAX,YRFMAX,ZRFMAX,
     *                  LERFN,LERFN2,LPRFN,LEACNV,
     *                  NE1,NE2,NDORG,NODEBK,
     *                  AWRK,IPART,NDOM,MBPDOM,LDOM,NBPDOM,
     *                  IPSLF,IPSND,RX,RY,MAXBUF,
     *                  IUT0,IERR)
C
      IMPLICIT NONE
C
      INTEGER*4 NGRID,NLAY1,NLAY2,IRFBOX
      INTEGER*4 ME,N2,NE,NP,NODE(N2,NE)
      INTEGER*4 NPBODY,NPINLT,NPTGT
      INTEGER*4 LPBODY(NPBODY),LPINLT(NPINLT),LPTGT(NPTGT)
      REAL*8    X(NP),Y(NP),Z(NP)
      REAL*4    XRFMIN,YRFMIN,ZRFMIN,XRFMAX,YRFMAX,ZRFMAX
      INTEGER*4 LERFN(NE),LPRFN(NP),LERFN2(NE),LEACNV(ME)
      INTEGER*4 IPART,NDOM,MBPDOM
      INTEGER*4 LDOM(NDOM),NBPDOM(NDOM),
     *          IPSLF(MBPDOM,NDOM),IPSND(MBPDOM,NDOM),
     *          RX(ME*8),RY(ME*8)
      INTEGER*4 MAXBUF,IUT0,IERR
      REAL*4    AWRK(NP)
C     OUT
      INTEGER*4 NE1,NE2
      INTEGER*4 NDORG(NE*8),NODEBK(8,NE)

C     WORK
      INTEGER*4 I,J,IE,IP,IBP,IDUM,IEBUF,IER,NDUM,NNPE
      REAL*4    XP,YP,ZP    
      CHARACTER*60 ERMSGC
     & /' ## SUBROUTINE EXTRFN: ERROR OCCURED             ; RETURNED' /
      CHARACTER*60 ERMSG1
     & /' ## SUBROUTINE EXTRFN: FAILED TO REORDER NODE TBL; RETURNED' /
C
CC
CCHY[1] MAKE ELEMENT LIST TO BE REFINED
CC
      DO 1000 IE=1,NE
         LERFN (IE)=0
         LERFN2(IE)=0
 1000 CONTINUE
C
      DO 1100 IP=1,NP
         LPRFN(IP)=0
 1100 CONTINUE
C
      DO 1200 IBP=1,NPBODY
         IP=LPBODY(IBP)
         LPRFN(IP)=1
 1200 CONTINUE
C
      DO 1300 IE=1,ME
         LEACNV(IE)=0
 1300 CONTINUE
C
CC
CC    // GLOBAL REFINE : NGRID = -1 //
CC
      IF(NGRID.EQ.-1) THEN
         DO 1500 IE=1,NE
             LERFN(IE)=1
 1500    CONTINUE   
         GOTO 2950
      ENDIF
C
CC
CC    // LOCAL REFINE : NGRID > 0 OR NGRID = -2 //
CC
CC
C     << FOR LERFN >>
      DO 2000 J=1,NLAY1
CC
CC    FIND ELEMENTS ATTACHED TO REFINE BOUNDARY NODE
CC
         DO 2100 IE=1,NE
            IF(LERFN(IE).EQ.1) GOTO 2100 
            DO 2200 I=1,N2
               IP=NODE(I,IE)
               IF(IP.EQ.0)        GOTO 2200
               IF(LPRFN(IP).EQ.0) GOTO 2200
               LERFN(IE)=1
               GOTO 2100
 2200       CONTINUE
 2100    CONTINUE
CC
CC    UPDATE REFINE BOUNDARY NODE
CC
         DO 2300 IE=1,NE
            IF(LERFN(IE).EQ.0) GOTO 2300
            DO 2400 I=1,N2
               IP=NODE(I,IE)
               IF(IP.EQ.0) GOTO 2400
               LPRFN(IP)=1
 2400       CONTINUE
 2300    CONTINUE
C
         DO 2500 IP=1,NP
            AWRK(IP)=FLOAT(LPRFN(IP))
 2500    CONTINUE
C
         IDUM=1
         CALL DDCOMX(IPART,IDUM,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *               AWRK,AWRK,AWRK,NP,IUT0,IERR,RX,RY,MAXBUF)
         IF(IERR.NE.0) THEN
            WRITE(IUT0,*)
            WRITE(IUT0,*) ERMSGC
            RETURN
         ENDIF
C
         DO 2600 IP=1,NP
            IF (AWRK(IP).GT.0.0) THEN
               LPRFN(IP)=1
            ELSE
               LPRFN(IP)=0
            ENDIF
 2600    CONTINUE
C
 2000 CONTINUE
C
      IF (NGRID.NE.-2) GOTO 2750
C
      DO 2007 IP=1,NP
         LPRFN(IP)=0
 2007 CONTINUE
C
      DO 2008 IBP=1,NPTGT
         IP=LPTGT(IBP)
         LPRFN(IP)=1
 2008 CONTINUE
C
C     << FOR LERFN2 (NGRID = -2)>>
      DO 2010 J=1,NLAY2
         DO 2110 IE=1,NE
            IF(LERFN2(IE).EQ.1) GOTO 2110 
            DO 2210 I=1,N2
               IP=NODE(I,IE)
               IF(IP.EQ.0)        GOTO 2210
               IF(LPRFN(IP).EQ.0) GOTO 2210
               LERFN2(IE)=1
               GOTO 2110
 2210       CONTINUE
 2110    CONTINUE
C
         DO 2310 IE=1,NE
            IF(LERFN2(IE).EQ.0) GOTO 2310
            DO 2410 I=1,N2
               IP=NODE(I,IE)
               IF(IP.EQ.0) GOTO 2410
               LPRFN(IP)=1
 2410       CONTINUE
 2310    CONTINUE
C
         DO 2510 IP=1,NP
            AWRK(IP)=FLOAT(LPRFN(IP))
 2510    CONTINUE
C
         IDUM=1
         CALL DDCOMX(IPART,IDUM,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *               AWRK,AWRK,AWRK,NP,IUT0,IERR,RX,RY,MAXBUF)
         IF(IERR.NE.0) THEN
            WRITE(IUT0,*)
            WRITE(IUT0,*) ERMSGC
            RETURN
         ENDIF
C
         DO 2610 IP=1,NP
            IF (AWRK(IP).GT.0.0) THEN
               LPRFN(IP)=1
            ELSE
               LPRFN(IP)=0
            ENDIF
 2610    CONTINUE
C
 2010 CONTINUE
C
 2750 CONTINUE
C
      IF (IRFBOX.EQ.0) GOTO 2850
C
C     << ADD ELEMENTS IN BOX TO BE REFINED >>
      DO 2800 IE=1, NE
         XP=0.0E0
         YP=0.0E0
         ZP=0.0E0
         IF(     NODE(8,IE).GE.1)THEN
            NNPE=8              ! Hex
         ELSE IF(NODE(6,IE).GE.1)THEN
            NNPE=6              ! Prism
         ELSE IF(NODE(5,IE).GE.1)THEN
            NNPE=5              ! Pyramid
         ELSE
            NNPE=4              ! Tetra
         END IF
         DO 2810 I=1, NNPE
            XP=XP+REAL(X(NODE(I,IE))/FLOAT(NNPE))
            YP=YP+REAL(Y(NODE(I,IE))/FLOAT(NNPE))
            ZP=ZP+REAL(Z(NODE(I,IE))/FLOAT(NNPE))
 2810    CONTINUE
         IF(XP.GT.XRFMIN.AND.XP.LT.XRFMAX.AND.
     *      YP.GT.YRFMIN.AND.YP.LT.YRFMAX.AND.
     *      ZP.GT.ZRFMIN.AND.ZP.LT.ZRFMAX) THEN
            LERFN(IE)=1
         ENDIF
 2800 CONTINUE
C
 2850 CONTINUE
C
C     << MERGE LERFN >>
      DO 2900 IE=1, NE
         IF(LERFN2(IE).GT.0) LERFN(IE)=1
 2900 CONTINUE
C
 2950 CONTINUE
C
CC
CCHY [2] COUNT NUM. OF ELEMENTS TO BE REFINERD
CC
      NE1=0
      IER=0
      DO 3000 IE=1,NE
         IF(LERFN(IE).EQ.0) GOTO 3000
         NE1=NE1+1
C
         IF(NODE(6,IE).EQ.0.AND.NODE(5,IE).NE.0) THEN
             NDUM=10
             ELSE
             NDUM=8 
         ENDIF
C
         DO 3100 I=1,NDUM
            IER=IER+1
            LEACNV(IER)=IE
 3100    CONTINUE
C 
 3000 CONTINUE
C
CC
CCHY [3] REORDER NODE TABLE
CC
      DO 4000 IE=1,NE
         DO 4100 I=1,8
            NDORG((IE-1)*8+I)=0
 4100    CONTINUE
 4000 CONTINUE
CC
CC    SET NODE TABLE AT ELEMENTS TO BE REFINED
CC
      IEBUF=0
      DO 4200 IE=1,NE
         IF(LERFN(IE).EQ.0) GOTO 4200
         IEBUF=IEBUF+1
         DO 4300 I=1,8
            NDORG((IEBUF-1)*8+I)=NODE(I,IE)
 4300    CONTINUE
 4200 CONTINUE
C
      IF(IEBUF.NE.NE1) THEN
         WRITE(IUT0,*)
         WRITE(IUT0,*) ERMSG1
         IERR=1
         RETURN
      ENDIF
CC
CC    SET NODE TABLE AT ELEMENTS NOT TO BE REFINED
CC
      NE2=0
      DO 4400 IE=1,NE
         IF(LERFN(IE).EQ.1) GOTO 4400
         NE2=NE2+1
         IER=IER+1
         LEACNV(IER)=IE
         DO 4500 I=1,8
            NDORG((NE1+NE2-1)*8+I)=NODE(I,IE)
            NODEBK(I,NE2)=NODE(I,IE)
 4500    CONTINUE
 4400 CONTINUE
C
      IF(NE1+NE2.NE.NE) THEN
         WRITE(IUT0,*)
         WRITE(IUT0,*) ERMSG1
         IERR=1
         RETURN
      ENDIF
C
      DO 4600 IE=1,NE
         DO 4700 I=1,8
            NODE(I,IE)=NDORG((IE-1)*8+I)
 4700    CONTINUE
 4600 CONTINUE
C
      RETURN
      END
