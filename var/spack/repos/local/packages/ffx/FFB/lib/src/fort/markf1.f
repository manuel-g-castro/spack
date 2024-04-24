C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    MARKF1                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE MARKF1(X,Y,F,NODE,N,XM,YM,IEM,NM,FM)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X(*),Y(*),F(*),NODE(N,*),XM(NM),YM(NM),IEM(NM),FM(NM)
C
      E = 1.D-50
C
C
C      INTERPOLATE FIELD DATA ; MARKER OPERATION 1 ( INTERPOLATION )
C         ( 2-D CALCULATION )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          X     (IP)  ; X-DIR. COORDINATE         OF NODE
C          Y     (IP)  ; Y-DIR. COORDINATE         OF NODE
C          F     (IP)  ; FIELD DATA NODALLY DEFINED
C          NODE(I,IE)  ; NODE TABLE BASED ON ELEMENT
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT
C          XM    (IM)  ; LOCATION    OF INTERPOLATION
C          YM    (IM)  ; LOCATION    OF INTERPOLATION
C          IEM   (IM)  ; ELEMENT NO. OF INTERPOLATION
C          NM          ; NUMBER      OF INTERPOLATION
C
C       (2) OUTPUT
C          FM    (IM)  ; INTERPOLATED VALUE
C
C
      DO 100 IM = 1 , NM
       IE  = IEM(IM)
       IF(IE.EQ.0) GO TO 100
       R1 = 1.D0/((XM(IM)-X(NODE(1,IE)))**2+(YM(IM)-Y(NODE(1,IE)))**2+E)
       R2 = 1.D0/((XM(IM)-X(NODE(2,IE)))**2+(YM(IM)-Y(NODE(2,IE)))**2+E)
       R3 = 1.D0/((XM(IM)-X(NODE(3,IE)))**2+(YM(IM)-Y(NODE(3,IE)))**2+E)
       R4 = 1.D0/((XM(IM)-X(NODE(4,IE)))**2+(YM(IM)-Y(NODE(4,IE)))**2+E)
C
       FM(IM) = (R1*F(NODE(1,IE))+R2*F(NODE(2,IE))
     &          +R3*F(NODE(3,IE))+R4*F(NODE(4,IE)))/(R1+R2+R3+R4)
  100 CONTINUE
C
C
      RETURN
      END
