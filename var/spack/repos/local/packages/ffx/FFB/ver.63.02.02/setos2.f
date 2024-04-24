      SUBROUTINE SETOS2(IPART,NPART,N2,NE,NP,MFRM,MDOM,
     *                  MCOMM,
     *                  NPSET,LPSET1,LPSET4,
     *                  NESET,LESET1,LESET4,
     *                  X,Y,Z,NODE, 
     *                  OMEGA,TIME,IPATTR,IEATTR,IFATTR,
     *                  NFRMS,LFRMS,NFRMR,LFRMR,
     *                  NDOMS,LISTS,LDOMS,LOSFMS,NBDOMS,LOSPS,LTYPES,
     *                  XOVERS,YOVERS,ZOVERS,
     *                  NDOMR,LISTR,LDOMR,LOSFMR,NBDOMR,LOSPR,LTYPER,
     *                  XOVERR,YOVERR,ZOVERR,
     *                  MMRF,OMGMRF,AMRF,BNDBOX,OSBOXG,
     *                  IUT6,IUT0,IERR)
      IMPLICIT NONE
C
C[INPUT]
      INTEGER*4 IPART,NPART,N2,NE,NP,MFRM,MDOM,NODE(N2,NE)
      INTEGER*4 MCOMM
      INTEGER*4 NPSET,LPSET1(NPSET),LPSET4(NPSET)
      INTEGER*4 NESET,LESET1(NESET),LESET4(NESET)
      INTEGER*4 IPATTR(NP),IEATTR(NE),IFATTR(*)
      INTEGER*4 NFRMS(NPART),LFRMS(MFRM,NPART),
     *          NFRMR(NPART),LFRMR(MFRM,NPART)
      REAL*4    X(NP),Y(NP),Z(NP),OMEGA,TIME,
     *          BNDBOX(6,NPART),OSBOXG(6,NPART)
      INTEGER*4 MMRF
      REAL*4    OMGMRF(MMRF),AMRF(3,MMRF)
C
C[OUTPUT]
      INTEGER*4 NDOMS,NDOMR
      INTEGER*4 LDOMS (MDOM),LISTS(MDOM),LOSFMS(MCOMM),
     *          NBDOMS(MDOM),LOSPS(MCOMM),LTYPES(MCOMM),
     *          LDOMR (MDOM),LISTR(MDOM),LOSFMR(MCOMM),
     *          NBDOMR(MDOM),LOSPR(MCOMM),LTYPER(MCOMM)
      REAL*4    XOVERS(MCOMM),YOVERS(MCOMM),
     *          ZOVERS(MCOMM),XOVERR(MCOMM),
     *          YOVERR(MCOMM),ZOVERR(MCOMM)
      INTEGER*4 IUT6,IUT0,IERR
C
C[LOCAL]
      INTEGER*4 JPART,I,J,IFRM,JFRM,IP,IE,IBP,IBE,INEW,LWRK(MDOM)
      INTEGER*4 NUM1,IERRA,ICHECK,II
      INTEGER*4 IP1,IP2,IP3,IP4,IP5,IP6,IP7,IP8
      DATA NUM1 /1/
      REAL*4    XBUF0,YBUF0,ZBUF0
      REAL*4    XBUF ,YBUF ,ZBUF
      REAL*4    TH,COSTH,SINTH
      REAL*4 O1,O2,C0,S0,C1,S1,C2,S2,AX,AY
      REAL*4 UBUF,VBUF,WBUF
      REAL*4 UR3,VR3,WR3
      REAL*4 XR1,YR1,ZR1
      REAL*4 XR2,YR2,ZR2
C
C
C     VERSION 2011.07.15 WRITTEN BY Y.YAMADE   
C
C     ARGUMENT LIST
C
C[INPUT]
C IPART          : DOMAIN NUMBER (1 - NPART) 
C NPART          : NUMBER OF DOMAINS
C NE             : NUMBER OF ELEMENTS
C NP             : NUMBER OF NODES
C MFRM           : MAX. NUMBER OF FRAMES
C MDOM           : MAX. NUMBER OF DOMAIN TO BE COMMUNICATED 
C NPSET          : NUMBER OF OVERSET NOUSES
C LPSET1(IBP)    : OVERSET NODES LIST
C NFRMS(  IPART) : NUMBER OF FRAMES FOR SENDING   IN THE DOMAIN OF IPART
C LFRMS(I,IPART) : FRAME LIST       FOR SENDING   IN THE DOMAIN OF IPART
C NFRMR(  IPART) : NUMBER OF FRAMES FOR RECEIVING IN THE DOMAIN OF IPART
C LFRMR(I,IPART) : FRAME LST        FOR RECEIVING IN THE DOMAIN OF IPART
C X(IP)      : X COORDINATE
C Y(IP)      : Y COORDINATE
C Z(IP)      : Z COORDINATE
C
C[OUTPUT]
C NDOMS          : NUMBER OF DOMAIN  FOR SENDING
C LDOMS (  IDOM) : DOMAIN LIST       FOR SENDING 
C NBDOMS(I,IDOM) : NUMBER OF NODES   FOR SENDING TO THE DOMAIN OF IDOM
C LOSFMS(I,IDOM) : FRAME NUMBER LIST FOR SENDING TO THE DOMAIN OF IDOM
C LOSPS (I,IDOM) : NODE  NUMBER LIST FOR SENDING TO THE DOMAIN OF IDOM
C XOVERS(I,IDOM) : X-COORDINATE      FOR SENDING TO THE DOMAIN OF IDOM
C YOVERS(I,IDOM) : Y-COORDINATE      FOR SENDING TO THE DOMAIN OF IDOM
C ZOVERS(I,IDOM) : Z-COORDINATE      FOR SENDING TO THE DOMAIN OF IDOM
C NDOMR          : NUMBER OF DOMAIN  FOR RECEIVING
C LDOMR (  IDOM) : DOMAIN LIST       FOR RECEIVING FROM THE DOMAIN OF IDOM
C NBDOMR(I,IDOM) : NUMBER OF NODES   FOR RECEIVING FROM THE DOMAIN OF IDOM 
C LOSFMR(I,IDOM) : FRAME NUMBER LIST FOR RECEIVING FROM THE DOMAIN OF IDOM 
C LOSPR (I,IDOM) : NODE  NUMBER LIST FOR RECEIVING FROM THE DOMAIN OF IDOM  
C XOVERR(I,IDOM) : X-COORDINATE      FOR RECEIVING FROM THE DOMAIN OF IDOM  
C YOVERR(I,IDOM) : Y-COORDINATE      FOR RECEIVING FROM THE DOMAIN OF IDOM  
C ZOVERR(I,IDOM) : Z-COORDINATE      FOR RECEIVING FROM THE DOMAIN OF IDOM  
C
C
      IERR=0
      TH=OMEGA*TIME
      COSTH=COS(TH)
      SINTH=SIN(TH)
C      
      IF(IPART.EQ.0) THEN
          NDOMS=1
          NDOMR=1
          LDOMS(1)=0
          LDOMR(1)=0
          LISTS(1)=1
          LISTR(1)=1
          NBDOMS(1)=NPSET+NESET
          NBDOMR(1)=NPSET+NESET
C
          DO 100 IBP=1,NPSET
              IP  =LPSET1(IBP)
              IFRM=LPSET4(IBP)
              LOSFMS(IBP)=IFRM
              LOSFMR(IBP)=IFRM
              LOSPS (IBP)=IBP
              LOSPR (IBP)=IBP
              LTYPES(IBP)=1
              LTYPER(IBP)=1
C
              XBUF=X(IP)
              YBUF=Y(IP)
              ZBUF=Z(IP)
C
              O1=OMEGA
              C1=COS(O1*TIME)
              S1=SIN(O1*TIME)
C
              IF(IPATTR(IP)         .GE. 0  .AND.
     *           IFATTR(LPSET4(IBP)).EQ.-1       )THEN
CC            [ROTATE COORDINATES AROUND Z-AXIS WITH THE ANGLE -A1]
                 XBUF= C1*X(IP)+S1*Y(IP)
                 YBUF=-S1*X(IP)+C1*Y(IP)
              ENDIF
C
              IF(IPATTR(IP)         .EQ.-1  .AND.
     *           IFATTR(LPSET4(IBP)).GE. 0       )THEN
CC            [ROTATE COORDINATES AROUND Z-AXIS WITH THE ANGLE A1]
                 XBUF=C1*X(IP)-S1*Y(IP)
                 YBUF=S1*X(IP)+C1*Y(IP)
              ENDIF
C
              XOVERS(IBP)=XBUF
              XOVERR(IBP)=XBUF
              YOVERS(IBP)=YBUF
              YOVERR(IBP)=YBUF
              ZOVERS(IBP)=ZBUF
              ZOVERR(IBP)=ZBUF
  100     CONTINUE
C
          DO 110 IBE=1,NESET
              IE  =LESET1(IBE)
              IFRM=LESET4(IBE)
              LOSFMS(NPSET+IBE)=IFRM
              LOSFMR(NPSET+IBE)=IFRM
              LOSPS (NPSET+IBE)=IBE
              LOSPR (NPSET+IBE)=IBE
              LTYPES(NPSET+IBE)=2
              LTYPER(NPSET+IBE)=2
C
              IP1=NODE(1,IE) 
              IP2=NODE(2,IE) 
              IP3=NODE(3,IE) 
              IP4=NODE(4,IE) 
              IP5=NODE(5,IE) 
              IP6=NODE(6,IE) 
              IP7=NODE(7,IE) 
              IP8=NODE(8,IE) 
              IF(IP8.NE.0) THEN
                  XBUF0=( X(IP1)+X(IP2)+X(IP3)+X(IP4)
     *                   +X(IP5)+X(IP6)+X(IP7)+X(IP8))/8.0E0
                  YBUF0=( Y(IP1)+Y(IP2)+Y(IP3)+Y(IP4)
     *                   +Y(IP5)+Y(IP6)+Y(IP7)+Y(IP8))/8.0E0
                  ZBUF0=( Z(IP1)+Z(IP2)+Z(IP3)+Z(IP4)
     *                   +Z(IP5)+Z(IP6)+Z(IP7)+Z(IP8))/8.0E0
              ELSE IF(IP6.NE.0) THEN
                  XBUF0=( X(IP1)+X(IP2)+X(IP3)+X(IP4)
     *                   +X(IP5)+X(IP6)              )/6.0E0
                  YBUF0=( Y(IP1)+Y(IP2)+Y(IP3)+Y(IP4)
     *                   +Y(IP5)+Y(IP6)              )/6.0E0
                  ZBUF0=( Z(IP1)+Z(IP2)+Z(IP3)+Z(IP4)
     *                   +Z(IP5)+Z(IP6)              )/6.0E0
              ELSE IF(IP5.NE.0) THEN
                  XBUF0=( X(IP1)+X(IP2)+X(IP3)+X(IP4)
     *                   +X(IP5)                     )/5.0E0
                  YBUF0=( Y(IP1)+Y(IP2)+Y(IP3)+Y(IP4)
     *                   +Y(IP5)                     )/5.0E0
                  ZBUF0=( Z(IP1)+Z(IP2)+Z(IP3)+Z(IP4)
     *                   +Z(IP5)                     )/5.0E0
              ELSE IF(IP4.NE.0) THEN
                  XBUF0=( X(IP1)+X(IP2)+X(IP3)+X(IP4))/4.0E0
                  YBUF0=( Y(IP1)+Y(IP2)+Y(IP3)+Y(IP4))/4.0E0
                  ZBUF0=( Z(IP1)+Z(IP2)+Z(IP3)+Z(IP4))/4.0E0
              ENDIF
C
              XBUF=XBUF0
              YBUF=YBUF0
              ZBUF=ZBUF0
C
              O1=OMEGA
              C1=COS(O1*TIME)
              S1=SIN(O1*TIME)
C
              IF(IEATTR(IE)         .GE. 0  .AND.
     *           IFATTR(LESET4(IBE)).EQ.-1       )THEN
CC            [ROTATE COORDINATES AROUND Z-AXIS WITH THE ANGLE -A1]
                 XBUF= C1*XBUF0+S1*YBUF0
                 YBUF=-S1*XBUF0+C1*YBUF0
              ENDIF
C
              IF(IEATTR(IE)         .EQ.-1  .AND.
     *           IFATTR(LESET4(IBE)).GE. 0       )THEN
CC            [ROTATE COORDINATES AROUND Z-AXIS WITH THE ANGLE A1]
                 XBUF=C1*XBUF0-S1*YBUF0
                 YBUF=S1*XBUF0+C1*YBUF0
              ENDIF
C
              XOVERS(NPSET+IBE)=XBUF
              XOVERR(NPSET+IBE)=XBUF
              YOVERS(NPSET+IBE)=YBUF
              YOVERR(NPSET+IBE)=YBUF
              ZOVERS(NPSET+IBE)=ZBUF
              ZOVERR(NPSET+IBE)=ZBUF
  110     CONTINUE
C
          RETURN
      ENDIF
CC
CCYY
CCYY[1] SEARCH DOMAIN FOR SENDING AND RECEIVING
CCYY
      NDOMS=0
      NDOMR=0
      LISTS(1)=1
      LISTR(1)=1
C
      DO 500 I=1,MDOM
          NBDOMS(I)=0
          NBDOMR(I)=0
  500 CONTINUE
C
      II=0
      DO 1000 JPART=1,NPART
CC
CC BOUNDING BOX FILTERING
CC
          ICHECK=1
          CALL CHKBOX(OSBOXG(1,IPART),BNDBOX(1,JPART),ICHECK)
          IF(ICHECK.EQ.0) GOTO 1000
CC
CC SEACH DOMAINS FOR SENDING
CC
         INEW=0
         DO 1100 I=1,NFRMS(IPART)
              IFRM=LFRMS(I,IPART)
              DO 1110 J=1,NFRMR(JPART)
                  JFRM=LFRMR(J,JPART)
                  IF(IFRM.NE.JFRM) GOTO 1110
                  IF(INEW.EQ.0) THEN
                      NDOMS=NDOMS+1
                      IF(NDOMS.EQ.1) THEN
                          II=0
                      ELSE
                          II=II+NBDOMS(NDOMS-1)
                      ENDIF
                      IF(NDOMS.GT.MDOM) THEN
                          IERR=1
                          WRITE(IUT0,*) 
     *                    'INSUFFICIENT MEMORY: SET LARGER MDOM'
                          GOTO 3000
                      ENDIF
                      LDOMS(NDOMS)=JPART
                      INEW=1
                  ENDIF
C
                  DO 1120 IBP=1,NPSET
                      IP=LPSET1(IBP)
                      IF(LPSET4(IBP).NE.IFRM) GOTO 1120
C
C
                      XBUF=X(IP)
                      YBUF=Y(IP)
                      ZBUF=Z(IP)
C
                      O1=OMEGA
                      IF(IPATTR(IP).GE.0) THEN
                          O2=0.0
                          AX=1.0
                          AY=0.0
                      ELSE 
                          O2=OMGMRF(-IPATTR(IP))
                          AX=AMRF(1,-IPATTR(IP))
                          AY=AMRF(2,-IPATTR(IP))
                      ENDIF
                      C0=AX/SQRT(AX*AX+AY*AY)
                      S0=AY/SQRT(AX*AX+AY*AY)
                      C1  = COS(O1*TIME)
                      S1  = SIN(O1*TIME)
                      C2  = COS(O2*TIME)
                      S2  = SIN(O2*TIME)
C
                      IF(IPATTR(IP)         .GE. 0  .AND.
     *                   IFATTR(LPSET4(IBP)).EQ.-1       )THEN
CC                    [ROTATE COORDINATES AROUND Z-AXIS WITH THE ANGLE -A1]
                         XBUF= C1*X(IP)+S1*Y(IP)
                         YBUF=-S1*X(IP)+C1*Y(IP)
                      ENDIF
C
                      IF(IPATTR(IP)         .EQ.-1  .AND.
     *                   IFATTR(LPSET4(IBP)).GE. 0       )THEN
CC                    [ROTATE COORDINATES AROUND Z-AXIS WITH THE ANGLE A1]
                         XBUF=C1*X(IP)-S1*Y(IP)
                         YBUF=S1*X(IP)+C1*Y(IP)
                      ENDIF
C
                      IF(IPATTR(IP)         .LT.-1  .AND.
     *                   IFATTR(LPSET4(IBP)).EQ.-1       )THEN
CC                    [ROTATE COORDINATES AROUND Z-AXIS WITH THE ANGLE A0]
                          XR1 = C0*X(IP)+S0*Y(IP)
                          YR1 =-S0*X(IP)+C0*Y(IP)
                          ZR1 =    Z(IP)
CC                    [ROTATE COORDINATES AROUND X-AXIS WITH THE ANGLE A2]
                          XR2 = XR1
                          YR2 = C2*YR1-S2*ZR1
                          ZR2 = S2*YR1+C2*ZR1
CC                    [ROTATE COORDINATES AROUND Z-AXIS WITH THE ANGLE -A0]
                          XBUF= C0*XR2-S0*YR2
                          YBUF= S0*XR2+C0*YR2
                          ZBUF=    ZR2
                      ENDIF
C  
                      IF(IPATTR(IP)         .EQ.-1  .AND.
     *                   IFATTR(LPSET4(IBP)).LT.-1       )THEN
                          O2=OMGMRF(-IFATTR(LPSET4(IBP)))
                          AX=AMRF(1,-IFATTR(LPSET4(IBP)))
                          AY=AMRF(2,-IFATTR(LPSET4(IBP)))
                          C0=AX/SQRT(AX*AX+AY*AY)
                          S0=AY/SQRT(AX*AX+AY*AY)
                          C1  = COS(O1*TIME)
                          S1  = SIN(O1*TIME)
                          C2  = COS(O2*TIME)
                          S2  = SIN(O2*TIME)
C                    [ROTATE COORDINATES AROUND Z-AXIS WITH THE ANGLE A0]
                          XR1 = C0*X(IP)+S0*Y(IP)
                          YR1 =-S0*X(IP)+C0*Y(IP)
                          ZR1 =    Z(IP)
C                    [ROTATE COORDINATES AROUND X-AXIS WITH THE ANGLE -A2]
                          XR2 = XR1
                          YR2 = C2*YR1+S2*ZR1
                          ZR2 =-S2*YR1+C2*ZR1
C                    [ROTATE COORDINATES AROUND Z-AXIS WITH THE ANGLE -A0]
                          XBUF= C0*XR2-S0*YR2
                          YBUF= S0*XR2+C0*YR2
                          ZBUF=    ZR2
                      ENDIF
C
                      IF(XBUF.LT.BNDBOX(1,JPART)) GOTO 1120
                      IF(YBUF.LT.BNDBOX(2,JPART)) GOTO 1120
                      IF(ZBUF.LT.BNDBOX(3,JPART)) GOTO 1120
                      IF(XBUF.GT.BNDBOX(4,JPART)) GOTO 1120
                      IF(YBUF.GT.BNDBOX(5,JPART)) GOTO 1120
                      IF(ZBUF.GT.BNDBOX(6,JPART)) GOTO 1120
C
                      NBDOMS(NDOMS           )=NBDOMS(NDOMS)+1
                      LOSFMS(II+NBDOMS(NDOMS))=IFRM
                      LOSPS (II+NBDOMS(NDOMS))=IBP
                      LTYPES(II+NBDOMS(NDOMS))=1
                      XOVERS(II+NBDOMS(NDOMS))=XBUF
                      YOVERS(II+NBDOMS(NDOMS))=YBUF
                      ZOVERS(II+NBDOMS(NDOMS))=ZBUF
C
 1120             CONTINUE
C
C
                  DO 1130 IBE=1,NESET
                      IE=LESET1(IBE)
                      IF(LESET4(IBE).NE.IFRM) GOTO 1130
C
                      IP1=NODE(1,IE) 
                      IP2=NODE(2,IE) 
                      IP3=NODE(3,IE) 
                      IP4=NODE(4,IE) 
                      IP5=NODE(5,IE) 
                      IP6=NODE(6,IE) 
                      IP7=NODE(7,IE) 
                      IP8=NODE(8,IE) 
                      IF(IP8.NE.0) THEN
                          XBUF0=( X(IP1)+X(IP2)+X(IP3)+X(IP4)
     *                           +X(IP5)+X(IP6)+X(IP7)+X(IP8))/8.0E0
                          YBUF0=( Y(IP1)+Y(IP2)+Y(IP3)+Y(IP4)
     *                           +Y(IP5)+Y(IP6)+Y(IP7)+Y(IP8))/8.0E0
                          ZBUF0=( Z(IP1)+Z(IP2)+Z(IP3)+Z(IP4)
     *                           +Z(IP5)+Z(IP6)+Z(IP7)+Z(IP8))/8.0E0
                      ELSE IF(IP6.NE.0) THEN
                          XBUF0=( X(IP1)+X(IP2)+X(IP3)+X(IP4)
     *                           +X(IP5)+X(IP6)              )/6.0E0
                          YBUF0=( Y(IP1)+Y(IP2)+Y(IP3)+Y(IP4)
     *                           +Y(IP5)+Y(IP6)              )/6.0E0
                          ZBUF0=( Z(IP1)+Z(IP2)+Z(IP3)+Z(IP4)
     *                           +Z(IP5)+Z(IP6)              )/6.0E0
                      ELSE IF(IP5.NE.0) THEN
                          XBUF0=( X(IP1)+X(IP2)+X(IP3)+X(IP4)
     *                           +X(IP5)                     )/5.0E0
                          YBUF0=( Y(IP1)+Y(IP2)+Y(IP3)+Y(IP4)
     *                           +Y(IP5)                     )/5.0E0
                          ZBUF0=( Z(IP1)+Z(IP2)+Z(IP3)+Z(IP4)
     *                           +Z(IP5)                     )/5.0E0
                      ELSE IF(IP4.NE.0) THEN
                          XBUF0=( X(IP1)+X(IP2)+X(IP3)+X(IP4))/4.0E0
                          YBUF0=( Y(IP1)+Y(IP2)+Y(IP3)+Y(IP4))/4.0E0
                          ZBUF0=( Z(IP1)+Z(IP2)+Z(IP3)+Z(IP4))/4.0E0
                      ENDIF
C
                      XBUF=XBUF0
                      YBUF=YBUF0
                      ZBUF=ZBUF0
C
                      O1=OMEGA
                      IF(IEATTR(IE).GE.0) THEN
                          O2=0.0
                          AX=1.0
                          AY=0.0
                      ELSE 
                          O2=OMGMRF(-IEATTR(IE))
                          AX=AMRF(1,-IEATTR(IE))
                          AY=AMRF(2,-IEATTR(IE))
                      ENDIF
                      C0=AX/SQRT(AX*AX+AY*AY)
                      S0=AY/SQRT(AX*AX+AY*AY)
                      C1  = COS(O1*TIME)
                      S1  = SIN(O1*TIME)
                      C2  = COS(O2*TIME)
                      S2  = SIN(O2*TIME)
C
                      IF(IEATTR(IE)         .GE. 0  .AND.
     *                   IFATTR(LESET4(IBE)).EQ.-1       )THEN
CC                    [ROTATE COORDINATES AROUND Z-AXIS WITH THE ANGLE -A1]
                         XBUF= C1*XBUF0+S1*YBUF0
                         YBUF=-S1*XBUF0+C1*YBUF0
                      ENDIF
C
                      IF(IEATTR(IE)         .EQ.-1  .AND.
     *                   IFATTR(LESET4(IBE)).GE. 0       )THEN
CC                    [ROTATE COORDINATES AROUND Z-AXIS WITH THE ANGLE A1]
                         XBUF=C1*XBUF0-S1*YBUF0
                         YBUF=S1*XBUF0+C1*YBUF0
                      ENDIF
C
                      IF(IEATTR(IE)         .LT.-1  .AND.
     *                   IFATTR(LESET4(IBE)).EQ.-1       )THEN
CC                    [ROTATE COORDINATES AROUND Z-AXIS WITH THE ANGLE A0]
                          XR1 = C0*XBUF0+S0*YBUF0
                          YR1 =-S0*XBUF0+C0*YBUF0
                          ZR1 =    ZBUF0
CC                    [ROTATE COORDINATES AROUND X-AXIS WITH THE ANGLE A2]
                          XR2 = XR1
                          YR2 = C2*YR1-S2*ZR1
                          ZR2 = S2*YR1+C2*ZR1
CC                    [ROTATE COORDINATES AROUND Z-AXIS WITH THE ANGLE -A0]
                          XBUF= C0*XR2-S0*YR2
                          YBUF= S0*XR2+C0*YR2
                          ZBUF=    ZR2
                      ENDIF
C  
                      IF(IEATTR(IE)         .EQ.-1  .AND.
     *                   IFATTR(LESET4(IBE)).LT.-1       )THEN
                          O2=OMGMRF(-IFATTR(LESET4(IBE)))
                          AX=AMRF(1,-IFATTR(LESET4(IBE)))
                          AY=AMRF(2,-IFATTR(LESET4(IBE)))
                          C0=AX/SQRT(AX*AX+AY*AY)
                          S0=AY/SQRT(AX*AX+AY*AY)
                          C1  = COS(O1*TIME)
                          S1  = SIN(O1*TIME)
                          C2  = COS(O2*TIME)
                          S2  = SIN(O2*TIME)
C                    [ROTATE COORDINATES AROUND Z-AXIS WITH THE ANGLE A0]
                          XR1 = C0*XBUF0+S0*YBUF0
                          YR1 =-S0*XBUF0+C0*YBUF0
                          ZR1 =    ZBUF0
C                    [ROTATE COORDINATES AROUND X-AXIS WITH THE ANGLE -A2]
                          XR2 = XR1
                          YR2 = C2*YR1+S2*ZR1
                          ZR2 =-S2*YR1+C2*ZR1
C                    [ROTATE COORDINATES AROUND Z-AXIS WITH THE ANGLE -A0]
                          XBUF= C0*XR2-S0*YR2
                          YBUF= S0*XR2+C0*YR2
                          ZBUF=    ZR2
                      ENDIF
C
                      IF(XBUF.LT.BNDBOX(1,JPART)) GOTO 1130
                      IF(YBUF.LT.BNDBOX(2,JPART)) GOTO 1130
                      IF(ZBUF.LT.BNDBOX(3,JPART)) GOTO 1130
                      IF(XBUF.GT.BNDBOX(4,JPART)) GOTO 1130
                      IF(YBUF.GT.BNDBOX(5,JPART)) GOTO 1130
                      IF(ZBUF.GT.BNDBOX(6,JPART)) GOTO 1130
C
                      NBDOMS(NDOMS           )=NBDOMS(NDOMS)+1
                      LOSFMS(II+NBDOMS(NDOMS))=IFRM
                      LOSPS (II+NBDOMS(NDOMS))=IBE
                      LTYPES(II+NBDOMS(NDOMS))=2
                      XOVERS(II+NBDOMS(NDOMS))=XBUF
                      YOVERS(II+NBDOMS(NDOMS))=YBUF
                      ZOVERS(II+NBDOMS(NDOMS))=ZBUF
C
 1130             CONTINUE
C
                  IF(NDOMS.GT.1) 
     *            LISTS(NDOMS)=LISTS(NDOMS-1)+NBDOMS(NDOMS-1)
C
                  GOTO 1100
C
 1110         CONTINUE   
 1100     CONTINUE
 1000 CONTINUE
C
C
CC
CC SEACH DOMAINS FOR RECEIVING
CC
      DO 2000 JPART=1,NPART
C
CC
CC BOUNDING BOX FILTERING
CC
          ICHECK=1
          CALL CHKBOX(OSBOXG(1,JPART),BNDBOX(1,IPART),ICHECK)
          IF(ICHECK.EQ.0) GOTO 2000
C
          INEW=0
          DO 2100 I=1,NFRMR(IPART)
              IFRM=LFRMR(I,IPART)
              DO 2110 J=1,NFRMS(JPART)
                  JFRM=LFRMS(J,JPART)
                  IF(IFRM.NE.JFRM) GOTO 2110
                  IF(INEW.EQ.0) THEN
                      NDOMR=NDOMR+1
                      IF(NDOMR.GT.MDOM) THEN
                          IERR=1
                          WRITE(IUT0,*) 
     *                    'INSUFFICIENT MEMORY: SET LARGER MDOM'
                          GOTO 3000
                      ENDIF
                      LDOMR(NDOMR)=JPART
                      INEW=1
                  ENDIF
                  GOTO 2100
 2110        CONTINUE   
 2100     CONTINUE
C
 2000 CONTINUE
C
 3000 CONTINUE
C
      CALL ERRCHK(IUT6,IPART,1,IERR,IERRA)
      IF(IERRA.NE.0) THEN
          IERR=1
          RETURN
      ENDIF
CCYY
CCYY[2] COMMUNICATE NUM. OF DATA ( NBDOMS --> NNDOMR )
CCYY
      DO 4000 I=1,MDOM
          LWRK(I)=1
 4000 CONTINUE
      CALL DDSET5(IPART,NUM1,
     *            NDOMS,LDOMS,LWRK,NBDOMS,
     *            NDOMR,LDOMR,LWRK,NBDOMR,IERR)
C
      IF(NDOMR.GE.2) THEN
          DO 4100 I=2,NDOMR
              LISTR(I)=LISTR(I-1)+NBDOMR(I-1)
 4100     CONTINUE
      ENDIF
CCYY
CCYY[3] COMMUNICATE   LOSFMS,LOSP,[X,Y,Z]OVERS
CCYY
      CALL DDSET6(IPART,MCOMM,
     *            NDOMS,LDOMS,NBDOMS,LISTS,LOSFMS,
     *            NDOMR,LDOMR,NBDOMR,LISTR,LOSFMR,IERR)
C
      CALL DDSET6(IPART,MCOMM,
     *            NDOMS,LDOMS,NBDOMS,LISTS,LOSPS,
     *            NDOMR,LDOMR,NBDOMR,LISTR,LOSPR,IERR)
C
      CALL DDSET6(IPART,MCOMM,
     *            NDOMS,LDOMS,NBDOMS,LISTS,LTYPES,
     *            NDOMR,LDOMR,NBDOMR,LISTR,LTYPER,IERR)
C
      CALL DDSET6(IPART,MCOMM,
     *            NDOMS,LDOMS,NBDOMS,LISTS,XOVERS,
     *            NDOMR,LDOMR,NBDOMR,LISTR,XOVERR,IERR)
C
      CALL DDSET6(IPART,MCOMM,
     *            NDOMS,LDOMS,NBDOMS,LISTS,YOVERS,
     *            NDOMR,LDOMR,NBDOMR,LISTR,YOVERR,IERR)
C
      CALL DDSET6(IPART,MCOMM,
     *            NDOMS,LDOMS,NBDOMS,LISTS,ZOVERS,
     *            NDOMR,LDOMR,NBDOMR,LISTR,ZOVERR,IERR)
C     
      RETURN
      END
