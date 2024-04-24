      SUBROUTINE FNDOS2(N,NEG,NE,LIST,
     *                  NP,NEX,X,Y,Z,NODE,IFRM,LEFRM,
     *                  XP,YP,ZP,EPSOS,
     *                  EN,GP,EP,TP,ERR,IUT0,IERR)
      IMPLICIT NONE
C
***** DEFINE ARGUMENTS *****
      INTEGER N,NEG,NE,NP,NEX,NODE,IFRM,LEFRM(NEG)
      INTEGER LIST(NE) 
      REAL X,Y,Z
      REAL XP,YP,ZP,EPSOS
C
      INTEGER EN
      REAL GP,EP,TP,ERR
C
      DIMENSION NODE(N,NEG),NEX(8),X(NP),Y(NP),Z(NP)
C      
***** OBJECTS *****
      INTEGER IUT0,IERR
      INTEGER IN,IE,IE0,IP,I
      REAL*4 XE,YE,ZE,DX,DY,DZ,RR,RRMIN
C
      INTEGER NHEX,NPRD,NWED,NTET,IEBUF     
      IERR =0
C
      NTET=NEX(5)
      NPRD=NEX(6)
      NWED=NEX(7)
      NHEX=NEX(8)
C      
************************
***** ELEMENT LOOP *****
************************
C
      IEBUF=1
      RRMIN=1.0E10
C
      DO 1000 IE0=1,NE
C
          IE=LIST(IE0)
          IF(LEFRM(IE).NE.IFRM) GOTO 1000 
C
          DO 1100 I=1,8
              IP=NODE(I,IE)
              IF(IP.EQ.0) GOTO 1100
              XE=X(IP)
              YE=Y(IP)
              ZE=Z(IP)
C
              DX=XE-XP
              DY=YE-YP
              DZ=ZE-ZP
              RR=DX*DX+DY*DY+DZ*DZ
C
              IF(RR.EQ.0.0E0) THEN
                  RR=0.0E0
              ELSE
                  RR=SQRT(RR)
              ENDIF
C
              IF(RR.LE.RRMIN) THEN
                  IEBUF=IE
                  RRMIN=RR
              ENDIF 
C
 1100     CONTINUE   
 1000 CONTINUE   
C
      IF(RRMIN.GT.EPSOS) THEN
          EN=0
          ERR=1.0
      ELSE
          EN=IEBUF
          ERR=RRMIN
C
          IF(NODE(NHEX,IE).NE.0) THEN
              GP=0.0
              EP=0.0
              TP=0.0
          ELSE IF(NODE(NWED,IE).NE.0) THEN
              GP=1./3.
              EP=1./3.
              TP=0.0
          ELSE IF(NODE(NPRD,IE).NE.0) THEN
              GP=0.0
              EP=0.0
              TP=1./4.
          ELSE IF(NODE(NTET,IE).NE.0) THEN
              GP=1./4.
              EP=1./4.
              TP=1./4.
          ENDIF
      ENDIF
C      
      RETURN
      END
