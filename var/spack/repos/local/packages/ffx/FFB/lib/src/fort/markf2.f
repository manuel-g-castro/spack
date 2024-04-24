C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    MARKF2                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE MARKF2(IMODE,JVALID,DT,X,Y,UM,VM,NODE,N,IENE,NEE,MAXEE,
     *                  EX1,EX2,EX3,EY1,EY2,EY3,DET1,DET2,
     *                  XMINMK,YMINMK,XMAXMK,YMAXMK,XM,YM,IEM,NM,
     *                  LOVER,LLOST,MOVER,MLOST,IUT0,IERR)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X(*),Y(*),UM(NM),VM(NM),NODE(N,*),IENE(MAXEE,*),NEE(*),
     1          EX1 (*),EX2 (*),EX3(*),EY1(*),EY2(*),EY3(*),
     2          DET1(*),DET2(*),XM(NM),YM(NM),IEM(NM),
     3          LOVER(MOVER),LLOST(MLOST)
C
      CHARACTER*72 ERMSG
     & /' *** SUBROUTINE MARKF2 REPORTS A FATAL ERROR OCCURENCE ***' /
      CHARACTER*72 EREXP1
     & /' TOO MANY PARTICLES HAVE PASSED ELEMENT BOUNDARIES' /
      CHARACTER*72 EREXP2
     & /' TOO MANY PARTICLES HAVE BEEN LOST FROM THE FIELD ' /
C
      D = 1.D-3
C
C
C      MOVE MARKER AND FIND NEW ELEMENT ; MARKER OPERATION 2
C         ( 2-D CALCULATION )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          IMODE       ; SPECIFIES LOST MARKER TRANSACTION AS FOLLOWS
C                   1 --- CONDENSE THE DIMENSIONS     FOR LOST MARKERS
C                   2 --- CLEAR    THE ELEMENT NUMBER FOR LOST MARKERS
C                 NOTE   ; ONLY IN MODE 2
C                         MARKER IDENTIFICATION IS POSSIBLE
C          JVALID      ; VALIDITY FLAG FOR MARKER TRACING ZONE
C                   0 --- TRACING ZONE INVALID
C                   1 --- TRACING ZONE   VALID
C          DT          ; TIME INCREMENT
C          X       (IP); X-DIR. COORDINATE         OF NODE
C          Y       (IP); Y-DIR. COORDINATE         OF NODE
C          UM      (IM); X-DIR. VELOCITY COMPONENT OF MARKER
C          VM      (IM); Y-DIR. VELOCITY COMPONENT OF MARKER
C          NODE  (I,IE); NODE TABLE
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT
C          IENE(IEE,IE); ADJACENT ELEMENT NUMBER TO ELEMENT IE
C          NEE     (IE); NUMBER OF ADJACENT ELEMENTS TO ELEMENT IE
C          MAXEE       ; THE FIRST DIMENSION OF ARRAY IENE
C          EX1     (IE); ELEMENT VECTOR  X(NODE(2,IE))-X(NODE(1,IE))
C          EX2     (IE); ELEMENT VECTOR  X(NODE(3,IE))-X(NODE(1,IE))
C          EX3     (IE); ELEMENT VECTOR  X(NODE(4,IE))-X(NODE(1,IE))
C          EY1     (IE); ELEMENT VECTOR  Y(NODE(2,IE))-Y(NODE(1,IE))
C          EY2     (IE); ELEMENT VECTOR  Y(NODE(3,IE))-Y(NODE(1,IE))
C          EY3     (IE); ELEMENT VECTOR  Y(NODE(4,IE))-Y(NODE(1,IE))
C          DET1(IE)    ; DETERMINANT ( EX1 , EY1 , EX2 , EY2 )
C          DET2(IE)    ; DETERMINANT ( EX2 , EY2 , EX3 , EY3 )
C          XMINMK      ; MIN. X-COOR. OF THE TRACING   ZONE   (JVALID=1)
C          XMAXMK      ; MAX. X-COOR. OF THE TRACING   ZONE   (JVALID=1)
C          YMINMK      ; MIN. Y-COOR. OF THE TRACING   ZONE   (JVALID=1)
C          YMAXMK      ; MAX. Y-COOR. OF THE TRACING   ZONE   (JVALID=1)
C          MOVER       ; THE DIMENSION OF ARRAY LOVER
C          MLOST       ; THE DIMENSION OF ARRAY LLOST
C          IUT0        ; DEVICE NUMBER TO REPORT ERROR OCCURENCE
C
C       (2) OUTPUT
C          IERR        ; RETURN CODE TO REPORT ERROR OCCURENCE
C                   0 --- NORMAL TERMINATION
C                   1 --- A FATAL ERROR HAS OCCURED
C
C       (3) INPUT-OUTPUT
C          XM      (IM); LOCATION    OF PARTICLES EXISTING IN THE FIELD
C          YM      (IM); LOCATION    OF PARTICLES EXISTING IN THE FIELD
C          IEM     (IM); ELEMENT NO. OF PARTICLES EXISTING IN THE FIELD
C          NM          ; NUMBER      OF PARTICLES EXISTING IN THE FIELD
C
C       (4) WORK
C          LOVER(IOVER); STORE THE PARTICLE NO.
C                       WHICH HAVE PASSED ELEMENT BOUNDARIES
C          LLOST(ILOST); STORE THE PARTICLE NO.
C                       WHICH HAVE BEEN LOST FROM THE FIELD
C
C
      IERR = 0
C
C      (1) MOVE ALL THE PARTICLES
C
      DO 100 IM = 1 , NM
          IF(IEM(IM).EQ.0) GO TO 100
          XM(IM) = XM(IM)+DT*UM(IM)
          YM(IM) = YM(IM)+DT*VM(IM)
  100 CONTINUE
C
C      (2) FIND PARTICLES WHICH HAVE PASSED ELEMENT BOUNDARIES
C
      NOVER = 0
      DO 200 IM = 1 , NM
          IF(IEM(IM).EQ.0) GO TO 200
          IE = IEM(IM)
          XF = XM(IM)-X(NODE(1,IE))
          YF = YM(IM)-Y(NODE(1,IE))
C
          A1 = ( EY2(IE)*XF-EX2(IE)*YF)/DET1(IE)
          B1 = (-EY1(IE)*XF+EX1(IE)*YF)/DET1(IE)
          A2 = ( EY3(IE)*XF-EX3(IE)*YF)/DET2(IE)
          B2 = (-EY2(IE)*XF+EX2(IE)*YF)/DET2(IE)
C
          IF((A1.LT.-D .OR. B1.LT.-D .OR. A1+B1.GT. 1.D0+D) .AND.
     &       (A2.LT.-D .OR. B2.LT.-D .OR. A2+B2.GT. 1.D0+D)) THEN
              NOVER = NOVER+1
C TEMORARY DELETED 88/03/19
C             IF(NOVER.LE.MOVER) THEN
C TEMORARY DELETED 88/03/19
                  LOVER(NOVER) = IM
C TEMORARY DELETED 88/03/19
C             ENDIF
C TEMORARY DELETED 88/03/19
          ENDIF
  200 CONTINUE
C
      IF(NOVER.GT.MOVER) THEN
          WRITE(IUT0,6300) ERMSG
          WRITE(IUT0,6300) EREXP1 
          IERR = 1 
          RETURN
      ENDIF
C
C      (3) OVER PARTICLES TRANSACTION
C
      DO 310 IEE = 1 , MAXEE
*VOPTION VEC
          DO 300 IOVER = 1 , NOVER
              IF(LOVER(IOVER).EQ.0) GO TO 300
              IM = LOVER(IOVER) 
              IE = IEM(IM)
              IF(IEE.GT.NEE(IE)) GO TO 300
              IER = IENE(IEE,IE)
              XF = XM(IM)-X(NODE(1,IER))
              YF = YM(IM)-Y(NODE(1,IER))
              A1 = ( EY2(IER)*XF-EX2(IER)*YF)/DET1(IER)
              B1 = (-EY1(IER)*XF+EX1(IER)*YF)/DET1(IER)
              A2 = ( EY3(IER)*XF-EX3(IER)*YF)/DET2(IER)
              B2 = (-EY2(IER)*XF+EX2(IER)*YF)/DET2(IER)
C
              IF(A1.GE.-D .AND. B1.GE.-D .AND. A1+B1.LE.1.D0+D  .OR.
     &           A2.GE.-D .AND. B2.GE.-D .AND. A2+B2.LE.1.D0+D) THEN
                  IEM(IM) = IER 
                  LOVER(IOVER) = 0
              ENDIF
  300     CONTINUE
  310 CONTINUE
C
      NLOST = 0
      DO 400 IOVER = 1 , NOVER
          IF(LOVER(IOVER).NE.0) THEN
              NLOST = NLOST+1
C TEMORARY DELETED 88/03/19
C             IF(NLOST.LE.MLOST) THEN
C TEMORARY DELETED 88/03/19
                  LLOST(NLOST) = LOVER(IOVER)
C TEMORARY DELETED 88/03/19
C             ENDIF
C TEMORARY DELETED 88/03/19
          ENDIF
  400 CONTINUE
C
      IF(JVALID.EQ.1) THEN
          DO 450 IM = 1 , NM
              IF(IEM(IM).EQ.0) GO TO 450
              IF(XM(IM).LT.XMINMK .OR. XM(IM).GT.XMAXMK. OR.
     &           YM(IM).LT.YMINMK .OR. YM(IM).GT.YMAXMK) THEN
                  NLOST = NLOST+1
C TEMORARY DELETED 88/03/19
C                 IF(NLOST.LE.MLOST) THEN
C TEMORARY DELETED 88/03/19
                      LLOST(NLOST) = IM
C TEMORARY DELETED 88/03/19
C                 ENDIF
C TEMORARY DELETED 88/03/19
              ENDIF
  450     CONTINUE
      ENDIF
C
      IF(NLOST.GT.MLOST) THEN
          WRITE(IUT0,6300) ERMSG
          WRITE(IUT0,6300) EREXP2 
          IERR = 1 
          RETURN
      ENDIF
C
C      (4) LOST PARTICLES TRANSACTION
C
      IF(IMODE.EQ.1) THEN
          DO 510 ILOST = NLOST , 1 , -1
              NM = NM-1
              DO 500 IM = LLOST(ILOST) , NM
                  XM(IM) =  XM(IM+1)
                  YM(IM) =  YM(IM+1)
                  IEM(IM) = IEM(IM+1)
  500         CONTINUE
  510     CONTINUE
      ELSE
          DO 520 ILOST = 1 , NLOST
              IEM(LLOST(ILOST)) = 0
  520     CONTINUE
      ENDIF
C
C
      RETURN
 6300 FORMAT(A72)
      END
