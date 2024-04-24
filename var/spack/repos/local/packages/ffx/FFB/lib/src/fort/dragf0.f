C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    DRAGF0                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE DRAGF0(X,Y,NODE,N,LEB,LESB,NEB,
     *                  DSB,ASB,ACB,DNXB,DNYB,DXG,DXE,DYG,DYE,DET)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X(*),Y(*),NODE(N,*),LEB(NEB),LESB(NEB),
     1          DSB(NEB),ASB(NEB),ACB(NEB),DNXB(N,NEB),DNYB(N,NEB),
     2          DXG(NEB),DXE(NEB),DYG(NEB),DYE(NEB),DET(NEB)
C
      DIMENSION G(4),E(4),DNG(4,4),DNE(4,4)
      DATA      G /  0.D0 ,  1.D0 ,  0.D0 , -1.D0 /
      DATA      E / -1.D0 ,  0.D0 ,  1.D0 ,  0.D0 /
C
C
C      CALCULATE BOUNDARY ELEMENT CONSTANTS ; DRAG CALCULATION 0
C         ( 2-D CALCULATION )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          X     (IP)  ; X-COORDINATE OF NODE ' IP '
C          Y     (IP)  ; Y-COORDINATE OF NODE ' IP '
C          NODE(I,IE)  ; NODE NO. TABLE BASED ON ELEMENT
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT
C          LEB   (IB)  ; ELEMENT  SPECIFYING LIST VECTOR
C          LESB  (IB)  ; SIDE NO. SPECIFYING LIST VECTOR
C          NEB         ; NUMBER OF THE SIDES
C
C       (2) OUTPUT
C          DSB   (IB)  ; LENGTH      OF BOUNDARY SIDE
C          ASB   (IB)  ; ANGLE BETWEEN THE SIDE AND X-AXIS (   SINE )
C          ACB   (IB)  ; ANGLE BETWEEN THE SIDE AND X-AXIS ( COSINE )
C          DNXB(I,IB)  ; X-DIRIVERTIVE  AT THE SIDE'S CENTER POINT
C          DNYB(I,IB)  ; Y-DIRIVERTIVE  AT THE SIDE'S CENTER POINT
C
C       (3) WORK
C          DET   (IB)  ; DETERMINANT OF JACOBIAN MATRIX
C          DXG   (IB)  ; G-DIR. DERIVERTIVE OF X
C          DXE   (IB)  ; E-DIR. DERIVERTIVE OF X
C          DYG   (IB)  ; G-DIR. DERIVERTIVE OF Y
C          DYE   (IB)  ; E-DIR. DERIVERTIVE OF Y
C
C      (1) CALCULATE  DSB(IB) , ASB(IB) , ACB(IB)
C
      DO 100 IB = 1 , NEB
          IE = LEB (IB) 
          IS = LESB(IB)
          X1 = X(NODE(MOD(IS  ,N)+1,IE))-X(NODE(IS,IE))
          Y1 = Y(NODE(MOD(IS  ,N)+1,IE))-Y(NODE(IS,IE))
          X2 = X(NODE(MOD(IS+1,N)+1,IE))-X(NODE(IS,IE))
          Y2 = Y(NODE(MOD(IS+1,N)+1,IE))-Y(NODE(IS,IE))

          DSB(IB) = DSQRT(X1*X1+Y1*Y1)
          ANG     = DATAN2(Y1,X1)
          ASB(IB) = DSIN(ANG) 
          ACB(IB) = DCOS(ANG)
          DIRECT  = -X2*ASB(IB)+Y2*ACB(IB)
          IF(DIRECT.LT.0.D0) THEN
              ASB(IB) = -ASB(IB) 
              ACB(IB) = -ACB(IB)
          ENDIF
  100 CONTINUE
C
C      (2) CALCULATE  DNXB(IB) DNYB(IB)
C
C          SETTING OF  G , E  DERIVERTIVES OF SHAPE FUNCTION
C
      DO 110 IS = 1 , N
          DNG(1,IS) = SHAPE2(1,G(IS),E(IS),2)
          DNG(2,IS) = SHAPE2(2,G(IS),E(IS),2)
          DNG(3,IS) = SHAPE2(3,G(IS),E(IS),2)
          DNG(4,IS) = SHAPE2(4,G(IS),E(IS),2)
C
          DNE(1,IS) = SHAPE2(1,G(IS),E(IS),3)
          DNE(2,IS) = SHAPE2(2,G(IS),E(IS),3)
          DNE(3,IS) = SHAPE2(3,G(IS),E(IS),3)
          DNE(4,IS) = SHAPE2(4,G(IS),E(IS),3)
  110 CONTINUE
C
C          CALCULATION  OF  G , E  DERIVERTIVES OF X , Y
C
      DO 200 IB = 1 , NEB
          IE  = LEB(IB) 
          IS = LESB(IB)
          DXG(IB) = X(NODE(1,IE))*DNG(1,IS)+X(NODE(2,IE))*DNG(2,IS)
     &             +X(NODE(3,IE))*DNG(3,IS)+X(NODE(4,IE))*DNG(4,IS)
          DYG(IB) = Y(NODE(1,IE))*DNG(1,IS)+Y(NODE(2,IE))*DNG(2,IS)
     &             +Y(NODE(3,IE))*DNG(3,IS)+Y(NODE(4,IE))*DNG(4,IS)
C
          DXE(IB) = X(NODE(1,IE))*DNE(1,IS)+X(NODE(2,IE))*DNE(2,IS)
     &             +X(NODE(3,IE))*DNE(3,IS)+X(NODE(4,IE))*DNE(4,IS)
          DYE(IB) = Y(NODE(1,IE))*DNE(1,IS)+Y(NODE(2,IE))*DNE(2,IS)
     &             +Y(NODE(3,IE))*DNE(3,IS)+Y(NODE(4,IE))*DNE(4,IS)
  200 CONTINUE
C
C          CALCULATION OF DETERMINANT OF JACOBIAN MATRIX
C
      DO 300 IB = 1 , NEB
          DET(IB) = DXG(IB)*DYE(IB)-DYG(IB)*DXE(IB)
  300 CONTINUE
C
C          CALCULATION OF X , Y DERIVERTIVES OF SHAPE FUNCTION
C
      DO 400 IB = 1 , NEB
          IS = LESB(IB)
          DNXB(1,IB) =( DYE(IB)*DNG(1,IS)-DYG(IB)*DNE(1,IS))/DET(IB)
          DNXB(2,IB) =( DYE(IB)*DNG(2,IS)-DYG(IB)*DNE(2,IS))/DET(IB)
          DNXB(3,IB) =( DYE(IB)*DNG(3,IS)-DYG(IB)*DNE(3,IS))/DET(IB)
          DNXB(4,IB) =( DYE(IB)*DNG(4,IS)-DYG(IB)*DNE(4,IS))/DET(IB)
C
          DNYB(1,IB) =(-DXE(IB)*DNG(1,IS)+DXG(IB)*DNE(1,IS))/DET(IB)
          DNYB(2,IB) =(-DXE(IB)*DNG(2,IS)+DXG(IB)*DNE(2,IS))/DET(IB)
          DNYB(3,IB) =(-DXE(IB)*DNG(3,IS)+DXG(IB)*DNE(3,IS))/DET(IB)
          DNYB(4,IB) =(-DXE(IB)*DNG(4,IS)+DXG(IB)*DNE(4,IS))/DET(IB)
  400 CONTINUE
C
C
      RETURN
      END
