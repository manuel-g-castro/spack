C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    INCLUD                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE INCLUD(IE,X,Y,NODE,NE,NP,N,XP,YP,IRN)
      IMPLICIT REAL*4 (A-H,O-Z)
      DIMENSION X(NP),Y(NP),NODE(N,NE)
      DATA DELT / 1.E-20 /
C
C
C      JUDGE IF SPECIFIED POINT IS INCLUDED IN THE SPECIFIED ELEMENT
C         ( 2-D GRAPHICS )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          IE          ; SPECIFIED ELEMENT NO.
C          X     (IP)  ; X-DIR. COORDINATE         OF NODE
C          Y     (IP)  ; Y-DIR. COORDINATE         OF NODE
C          NODE(I,IE)  ; NODE NUMBER TABLE
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          NP          ; NUMBER OF TOTAL NODES
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT
C          XP          ; X-DIR. COORDINATE OF SPECIFIED POINT
C          YP          ; Y-DIR. COORDINATE OF SPECIFIED POINT
C
C       (2) OUTPUT
C          IRN         ; RETURN CODE TO REPORT RESULT
C                   0 --- NOT INCLUDED
C                   1 ---     INCLUDED
C
C
      IRN = 0
C
      DO 100 IF = 1 , 2
          X1  = X(NODE(IF+1,IE))-X(NODE(1,IE))
          Y1  = Y(NODE(IF+1,IE))-Y(NODE(1,IE))
          X2  = X(NODE(IF+2,IE))-X(NODE(1,IE))
          Y2  = Y(NODE(IF+2,IE))-Y(NODE(1,IE))
          DET = X1*Y2-X2*Y1
C
          XF  = XP              -X(NODE(1,IE))
          YF  = YP              -Y(NODE(1,IE))
C
          A   = ( Y2*XF-X2*YF)/DET
          B   = (-Y1*XF+X1*YF)/DET
C
          IF(A       .LT.    -DELT) GO TO 100
          IF(B       .LT.    -DELT) GO TO 100
          IF(A+B     .GT. 1.0+DELT) GO TO 100
          IRN = 1
          RETURN
  100 CONTINUE
C
C
      RETURN
      END
