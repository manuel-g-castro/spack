C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    THROW1                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE THROW1(INTRAN,XMIN,YMIN,XMAX,YMAX,NMARK,
     *                  X,Y,NODE,NE,NP,N,
     *                  EX1,EX2,EX3,EY1,EY2,EY3,DET1,DET2,
     *                  XM,YM,IEM,IUT0,IERR)
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*4   RAN
      DIMENSION X(NP),Y(NP),NODE(N,NE),
     1          EX1 (NE),EX2 (NE),EX3(NE),EY1(NE),EY2(NE),EY3(NE),
     2          DET1(NE),DET2(NE),XM(NMARK),YM(NMARK),IEM(NMARK)
C
      DIMENSION RAN(2)
C
      CHARACTER*72 ERMSG
     & /' *** SUBROUTINE THROW1 REPORTS A FATAL ERROR OCCURENCE ***  '/
      CHARACTER*72 EREXP1
     & /' THROWING IMPOSSIBLE ; NO ELEMENT FOUND IN THE SPECIFID ZONE'/
C
      DATA D / 1.D-1 /
      NRAN = 2
C
C
C      THROW MARKER PARTICLES AT RANDOM INTO THE SPECIFIED ZONE
C         ( 2-D CALCULATION )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          XMIN        ; MIN. X-COOR. OF THE THROWING ZONE
C          YMIN        ; MIN. Y-COOR. OF THE THROWING ZONE
C          XMAX        ; MAX. X-COOR. OF THE THROWING ZONE
C          YMAX        ; MAX. Y-COOR. OF THE THROWING ZONE
C          NMARK       ; NUMBER OF MARKERS TO BE THROWN
C
C          X (IP)      ; X-DIR. COORDINATE         OF NODE
C          Y (IP)      ; Y-DIR. COORDINATE         OF NODE
C          NODE(I,IE)  ; NODE TABLE
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          NP          ; NUMBER OF TOTAL NODES
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT
C          EX1 (IE)    ; ELEMENT VECTOR  X(NODE(2,IE))-X(NODE(1,IE))
C          EX2 (IE)    ; ELEMENT VECTOR  X(NODE(3,IE))-X(NODE(1,IE))
C          EX3 (IE)    ; ELEMENT VECTOR  X(NODE(4,IE))-X(NODE(1,IE))
C          EY1 (IE)    ; ELEMENT VECTOR  Y(NODE(2,IE))-Y(NODE(1,IE))
C          EY2 (IE)    ; ELEMENT VECTOR  Y(NODE(3,IE))-Y(NODE(1,IE))
C          EY3 (IE)    ; ELEMENT VECTOR  Y(NODE(4,IE))-Y(NODE(1,IE))
C          DET1(IE)    ; DETERMINANT ( EX1 , EY1 , EX2 , EY2 )
C          DET2(IE)    ; DETERMINANT ( EX2 , EY2 , EX3 , EY3 )
C
C          IUT0        ; DEVICE NO. TO REPORT ERROR OCCURENCE
C
C       (2) OUTPUT
C          XM      (IM); X-DIR. POSITIONS OF MARKERS
C          YM      (IM); Y-DIR. POSITIONS OF MARKERS
C          IEM     (IM); ELEMENT NUMBER   OF MARKERS
C          IERR        ; RETURN CODE TO REPORT ERROR OCCURENCE
C                   0 --- NORMAL TERMINATION
C                   1 --- A FATAL ERROR HAS OCCURED
C
C       (3) INPUT-OUTPUT
C          INTRAN      ; INITIAL AND FINAL RANDAM NUMBER
C
C
      IERR = 0
C
C      (1) CHECK IF THE THROWING IS POSSIBLE
C
      DO 10 IP = 1 , NP
          IF(X(IP).GE.XMIN .AND. X(IP).LE.XMAX .AND.
     &       Y(IP).GE.YMIN .AND. Y(IP).LE.YMAX) GO TO 20
   10 CONTINUE
          WRITE(IUT0,6300) ERMSG
          WRITE(IUT0,6300) EREXP1 
          IERR = 1 
          RETURN
   20 CONTINUE
C
C      (2) CONTINUE THROWING UNTILL MEETING THE REQUIRED NUMBER
C
      NM = 0
   30 CONTINUE
C
#ifdef VOS
          CALL HSRU1M(NRAN,INTRAN,RAN,IERRDM)
#else
          CALL LSGU1M(NRAN,INTRAN,RAN,IERRDM)
#endif
          XP = XMIN+RAN(1)*(XMAX-XMIN)
          YP = YMIN+RAN(2)*(YMAX-YMIN)
          IFOUND = 0
          DO 100 IE = 1 , NE
              XF = XP-X(NODE(1,IE))
              YF = YP-Y(NODE(1,IE))
              A1 = ( EY2(IE)*XF-EX2(IE)*YF)/DET1(IE)
              B1 = (-EY1(IE)*XF+EX1(IE)*YF)/DET1(IE)
              A2 = ( EY3(IE)*XF-EX3(IE)*YF)/DET2(IE)
              B2 = (-EY2(IE)*XF+EX2(IE)*YF)/DET2(IE)
C
              IF(A1.GE.-D .AND. B1.GE.-D .AND. A1+B1.LE.1.D0+D  .OR.
     &           A2.GE.-D .AND. B2.GE.-D .AND. A2+B2.LE.1.D0+D) THEN
                  IFOUND = IE
              ENDIF
  100     CONTINUE
C
          IF(IFOUND.NE.0) THEN
              NM = NM+1
              XM (NM) = XP 
              YM (NM) = YP 
              IEM(NM) = IFOUND
              IF(NM.EQ.NMARK) RETURN
          ENDIF
      GO TO 30
C
C
 6300 FORMAT(A72)
      END
