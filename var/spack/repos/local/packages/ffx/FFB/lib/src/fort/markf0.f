C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    MARKF0                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE MARKF0(X,Y,NODE,NE,N,
     *                  EX1,EX2,EX3,EY1,EY2,EY3,DET1,DET2)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X(*),Y(*),NODE(N,NE),
     1          EX1 (NE),EX2 (NE),EX3(NE),EY1(NE),EY2(NE),EY3(NE),
     2          DET1(NE),DET2(NE)
C
C
C      CALCULATE ELEMENT CONSTANTS ; MARKER OPERATION 0 ( PREPARATION )
C         ( 2-D CALCULATION )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          X (IP)      ; X-DIR. COORDINATE         OF NODE
C          Y (IP)      ; Y-DIR. COORDINATE         OF NODE
C          NODE(I,IE)  ; NODE TABLE
C          NE          ; NUMBER OF TOTAL NODES
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT
C
C       (2) OUTPUT
C          EX1 (IE)    ; ELEMENT VECTOR  X(NODE(2,IE))-X(NODE(1,IE))
C          EX2 (IE)    ; ELEMENT VECTOR  X(NODE(3,IE))-X(NODE(1,IE))
C          EX3 (IE)    ; ELEMENT VECTOR  X(NODE(4,IE))-X(NODE(1,IE))
C          EY1 (IE)    ; ELEMENT VECTOR  Y(NODE(2,IE))-Y(NODE(1,IE))
C          EY2 (IE)    ; ELEMENT VECTOR  Y(NODE(3,IE))-Y(NODE(1,IE))
C          EY3 (IE)    ; ELEMENT VECTOR  Y(NODE(4,IE))-Y(NODE(1,IE))
C          DET1(IE)    ; DETERMINANT ( EX1 , EY1 , EX2 , EY2 )
C          DET2(IE)    ; DETERMINANT ( EX2 , EY2 , EX3 , EY3 )
C
C
      DO 100 IE = 1 , NE
          EX1 (IE) = X(NODE(2,IE))-X(NODE(1,IE))
          EX2 (IE) = X(NODE(3,IE))-X(NODE(1,IE))
          EX3 (IE) = X(NODE(4,IE))-X(NODE(1,IE))
C
          EY1 (IE) = Y(NODE(2,IE))-Y(NODE(1,IE))
          EY2 (IE) = Y(NODE(3,IE))-Y(NODE(1,IE))
          EY3 (IE) = Y(NODE(4,IE))-Y(NODE(1,IE))
  100 CONTINUE
C
      DO 200 IE = 1 , NE
          DET1(IE) = EX1(IE)*EY2(IE)-EX2(IE)*EY1(IE)
          DET2(IE) = EX2(IE)*EY3(IE)-EX3(IE)*EY2(IE)
  200 CONTINUE
C
C
      RETURN
      END
