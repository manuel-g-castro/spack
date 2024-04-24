      SUBROUTINE SETOS3(N2,NE,NP,IPART,NPART,MFRM,
     *                  NODE,LEFRM,LPFRM,NPSET,LPSET1,LPSET4,
     *                  NFRMS,LFRMS,NFRMR,LFRMR,
     *                  XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,
     *                  BNDBOX,OSBOX,OSBOXG,  
     *                  IUT6,IERR)
      IMPLICIT NONE
C
C[INPUT]
      INTEGER*4 N2,NE,NP,IPART,NPART,MFRM,
     *          NODE(N2,NE),LEFRM(NE),LPFRM(NP)
      INTEGER*4 NPSET,LPSET1(NPSET),LPSET4(NPSET)
      REAL*4    XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,OSBOX(6)
C
C[OUTPUT]
      INTEGER*4 NFRMS(NPART),LFRMS(MFRM,NPART),
     *          NFRMR(NPART),LFRMR(MFRM,NPART),IUT6,IERR
      REAL*4    BNDBOX(6,NPART),OSBOXG(6,NPART)
C
C[LOCAL]
       INTEGER*4 IP,IE,I,J,IBP,IFRM,NUM1,NUM6
       DATA NUM1 /1/
       DATA NUM6 /6/
C
C
C     VERSION 2011.07.15 WRITTEN BY Y.YAMADE   
C
C     ARGUMENT LIST
C
C[INPUT]
C N2         : NUMBER OF NODES IN A ELEMENT (=8)
C NE         : NUMBER OF ELEMENTS
C NP         : NUMBER OF NODES
C IPART      : DOMAIN NUMBER (1 - NPART) 
C NPART      : NUMBER OF DOMAINS
C MFRM       :
C NODE(I,IE) : NODE TABLE
C LEFRM(IE)  : FRAME NUMBER AT ELEMENTS
C LPFRM(IP)  : FRAME NUMBER AT NODES
C
C[OUTPUT]
C NFRMS(  IPART) : NUMBER OF FRAMES FOR SENDING   IN THE DOMAIN OF IPART
C LFRMS(I,IPART) : FRAME LIST       FOR SENDING   IN THE DOMAIN OF IPART
C NFRMR(  IPART) : NUMBER OF FRAMES FOR RECEIVING IN THE DOMAIN OF IPART
C LFRMR(I,IPART) : FRAME LST        FOR RECEIVING IN THE DOMAIN OF IPART
C
      IERR=0
CCYY
CCYY[1] MAKING FRAME LIST FOR SEND (NPFRMS,LPFRMS)
CCYY
      DO 1000 IP=1,NP
          LPFRM(IP)=0
 1000 CONTINUE
C
      DO 1100 IE=1,NE
          DO 1200 I=1,8
              IP=NODE(I,IE)
              IF(IP.EQ.0) GOTO 1100 
              LPFRM(IP)= LEFRM(IE)
 1200     CONTINUE
 1100 CONTINUE
C
C
      IF(IPART.EQ.0) RETURN
C
      DO 1300 I=1,NPART
          NFRMS(I)=0
          DO 1301 J=1,MFRM
              LFRMS(J,I)=0
 1301     CONTINUE
 1300 CONTINUE
C
      DO 1400 IBP =1,NPSET
          IP  =LPSET1(IBP)
          IFRM=LPSET4(IBP)
          DO 1410 I=1,NFRMS(IPART)
              IF(IFRM.EQ.LFRMS(I,IPART)) GOTO 1400
 1410     CONTINUE         
          NFRMS(IPART)=NFRMS(IPART)+1 
          LFRMS(NFRMS(IPART),IPART)=IFRM
 1400 CONTINUE
CCYY
CCYY[2] MAKING FRAME LIST FOR RECEIVING (NPFRMR,LPFRMR)
CCYY
      DO 2000 I=1,NPART
          NFRMR(I)=0
          DO 2001 J=1,MFRM
              LFRMR(J,I)=0 
 2001     CONTINUE
 2000 CONTINUE
      DO 2100 IE=1,NE
          IFRM=LEFRM(IE)
          DO 2110 I=1,NFRMR(IPART)
              IF(IFRM.EQ.LFRMR(I,IPART)) GOTO 2100
 2110     CONTINUE         
          NFRMR(IPART)=NFRMR(IPART)+1 
          LFRMR(NFRMR(IPART),IPART)=IFRM
 2100 CONTINUE
CCYY
CCYY[3] COMMUNICAT OVERSET TABLE-3
CCYY
      CALL DDSET4(IPART,NPART,NUM1,NFRMR,IERR)
      CALL DDSET4(IPART,NPART,NUM1,NFRMS,IERR)
      CALL DDSET4(IPART,NPART,MFRM,LFRMR,IERR)
      CALL DDSET4(IPART,NPART,MFRM,LFRMS,IERR)
CCYY
CCYY[4] BOUNDING BOX DATA
CCYY
      BNDBOX(1,IPART) = XMIN
      BNDBOX(2,IPART) = YMIN
      BNDBOX(3,IPART) = ZMIN
      BNDBOX(4,IPART) = XMAX
      BNDBOX(5,IPART) = YMAX
      BNDBOX(6,IPART) = ZMAX
      CALL DDSET4(IPART,NPART,NUM6,BNDBOX,IERR)
CCYY
CCYY[5] BOUNDING BOX DATA FOR  OVERSET NODE 
CCYY
      OSBOXG(1,IPART) = OSBOX(1)
      OSBOXG(2,IPART) = OSBOX(2)
      OSBOXG(3,IPART) = OSBOX(3)
      OSBOXG(4,IPART) = OSBOX(4)
      OSBOXG(5,IPART) = OSBOX(5)
      OSBOXG(6,IPART) = OSBOX(6)
      CALL DDSET4(IPART,NPART,NUM6,OSBOXG,IERR)
C
      RETURN
      END
