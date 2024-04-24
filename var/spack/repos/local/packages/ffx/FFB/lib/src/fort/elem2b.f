C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    ELEM2B                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE ELEM2B(X,Y,NODE,N,LEB,LSB,NEB,
     *                  DSB,DCOSB,SNIB,DNXIB,DNYIB,DXG,DXE,DYG,DYE,DET)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X(*),Y(*),NODE(N,*),LEB(NEB),LSB(NEB),DSB(NEB),
     1          DCOSB(2,NEB),SNIB(N,NEB),DNXIB(N,NEB),DNYIB(N,NEB),
     2          DXG(NEB),DXE(NEB),DYG(NEB),DYE(NEB),DET(NEB)
C
      DIMENSION G(4),E(4),SN(4,4),DNG(4,4),DNE(4,4)
      DATA      G /  0.D0 ,  1.D0 ,  0.D0 , -1.D0 /
      DATA      E / -1.D0 ,  0.D0 ,  1.D0 ,  0.D0 /
C
C
C      CALCULATE LENGTH, OUTWARD DIRECTIONAL COSINE, SHAPE FUNCTION
C     AND ITS X,Y DERIVATIVES ON SPECIFIED SIDE OF SPECIFIED ELEMENTS
C         ( 2-D CALCULATION )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          X      (IP) ; X-COORDINATE OF NODE ' IP '
C          Y      (IP) ; Y-COORDINATE OF NODE ' IP '
C          NODE (I,IE) ; NODE NO. TABLE BASED ON ELEMENT
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT
C          LEB    (IB) ; ELEMENT  SPECIFYING LIST VECTOR
C          LSB    (IB) ; SIDE NO. SPECIFYING LIST VECTOR
C          NEB         ; NUMBER OF THE SIDES
C
C       (2) OUTPUT
C          DSB    (IB) ; LENGTH              OF BOUNDARY SIDE
C          DCOSB(I,IB) ; DIRECTIONAL COSINES OF BOUNDARY SIDE
C          SNIB (I,IB) ; SHAPE FUNCTION AT THE SIDE'S CENTER POINT
C          DNXIB(I,IB) ; X-DIRIVERTIVE  AT THE SIDE'S CENTER POINT
C          DNYIB(I,IB) ; Y-DIRIVERTIVE  AT THE SIDE'S CENTER POINT
C
C       (3) WORK
C          DET   (IB)  ; DETERMINANT OF JACOBIAN MATRIX
C          DXG   (IB)  ; G-DIR. DERIVERTIVE OF X
C          DXE   (IB)  ; E-DIR. DERIVERTIVE OF X
C          DYG   (IB)  ; G-DIR. DERIVERTIVE OF Y
C          DYE   (IB)  ; E-DIR. DERIVERTIVE OF Y
C
C      (1) CALCULATE  DSB(IB) , DCOSB(I,IB)
C
      DO 100 IB = 1 , NEB
          IE  = LEB(IB) 
          IS = LSB(IB)
          X1 = X(NODE(MOD(IS  ,N)+1,IE))-X(NODE(IS,IE))
          Y1 = Y(NODE(MOD(IS  ,N)+1,IE))-Y(NODE(IS,IE))
          X2 = X(NODE(MOD(IS+1,N)+1,IE))-X(NODE(IS,IE))
          Y2 = Y(NODE(MOD(IS+1,N)+1,IE))-Y(NODE(IS,IE))

          DSB(IB) = DSQRT(X1*X1+Y1*Y1)
C
          DCOSB(1,IB) = Y1/DSB(IB)
          DCOSB(2,IB) =-X1/DSB(IB)
          CHECK  = X2*DCOSB(1,IB)+Y2*DCOSB(2,IB)
          IF(CHECK.GT.0.D0) THEN
              DCOSB(1,IB) =-DCOSB(1,IB)
              DCOSB(2,IB) =-DCOSB(2,IB)
          ENDIF
  100 CONTINUE
C
C      (2) CALCULATE  DNXIB(IB) DNYIB(IB)
C
C          SETTING SHAPE FUNCTION AND ITS DERIVERTIVES AT ALL SIDES
C
      DO 110 IS = 1 , N
          SN (1,IS) = SHAPE2(1,G(IS),E(IS),1)
          SN (2,IS) = SHAPE2(2,G(IS),E(IS),1)
          SN (3,IS) = SHAPE2(3,G(IS),E(IS),1)
          SN (4,IS) = SHAPE2(4,G(IS),E(IS),1)
C
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
C          CALCULATION  OF SHAPE FUNCTTION
C
      DO 200 IB = 1 , NEB
          IS = LSB(IB)
          SNIB(1,IB) = SN(1,IS) 
          SNIB(2,IB) = SN(2,IS)
          SNIB(3,IB) = SN(3,IS) 
          SNIB(4,IB) = SN(4,IS)
  200 CONTINUE
C
C          CALCULATION  OF  G , E  DERIVERTIVES OF X , Y
C
      DO 300 IB = 1 , NEB
          IE  = LEB(IB) 
          IS = LSB(IB)
          DXG(IB) = X(NODE(1,IE))*DNG(1,IS)+X(NODE(2,IE))*DNG(2,IS)
     &             +X(NODE(3,IE))*DNG(3,IS)+X(NODE(4,IE))*DNG(4,IS)
          DYG(IB) = Y(NODE(1,IE))*DNG(1,IS)+Y(NODE(2,IE))*DNG(2,IS)
     &             +Y(NODE(3,IE))*DNG(3,IS)+Y(NODE(4,IE))*DNG(4,IS)
C
          DXE(IB) = X(NODE(1,IE))*DNE(1,IS)+X(NODE(2,IE))*DNE(2,IS)
     &             +X(NODE(3,IE))*DNE(3,IS)+X(NODE(4,IE))*DNE(4,IS)
          DYE(IB) = Y(NODE(1,IE))*DNE(1,IS)+Y(NODE(2,IE))*DNE(2,IS)
     &             +Y(NODE(3,IE))*DNE(3,IS)+Y(NODE(4,IE))*DNE(4,IS)
  300 CONTINUE
C
C          CALCULATION OF DETERMINANT OF JACOBIAN MATRIX
C
      DO 400 IB = 1 , NEB
          DET(IB) = DXG(IB)*DYE(IB)-DYG(IB)*DXE(IB)
  400 CONTINUE
C
C          CALCULATION OF X , Y DERIVERTIVES OF SHAPE FUNCTION
C
      DO 500 IB = 1 , NEB
          IS = LSB(IB)
          DNXIB(1,IB) =( DYE(IB)*DNG(1,IS)-DYG(IB)*DNE(1,IS))/DET(IB)
          DNXIB(2,IB) =( DYE(IB)*DNG(2,IS)-DYG(IB)*DNE(2,IS))/DET(IB)
          DNXIB(3,IB) =( DYE(IB)*DNG(3,IS)-DYG(IB)*DNE(3,IS))/DET(IB)
          DNXIB(4,IB) =( DYE(IB)*DNG(4,IS)-DYG(IB)*DNE(4,IS))/DET(IB)
C
          DNYIB(1,IB) =(-DXE(IB)*DNG(1,IS)+DXG(IB)*DNE(1,IS))/DET(IB)
          DNYIB(2,IB) =(-DXE(IB)*DNG(2,IS)+DXG(IB)*DNE(2,IS))/DET(IB)
          DNYIB(3,IB) =(-DXE(IB)*DNG(3,IS)+DXG(IB)*DNE(3,IS))/DET(IB)
          DNYIB(4,IB) =(-DXE(IB)*DNG(4,IS)+DXG(IB)*DNE(4,IS))/DET(IB)
  500 CONTINUE
C
C
      RETURN
      END
