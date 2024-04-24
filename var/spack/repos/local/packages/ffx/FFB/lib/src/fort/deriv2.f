C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    DERIV2                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE DERIV2(G,E,X,Y,NODE,NE,N,SNI,DNXI,DNYI,DET,
     *                  DXG,DYG,DXE,DYE)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X(*),Y(*),NODE(N,NE),SNI(N,NE),DNXI(N,NE),DNYI(N,NE),
     1          DET(NE),DXG(NE),DYG(NE),DXE(NE),DYE(NE)
C
      DIMENSION DN(4),DNG(4),DNE(4)
C
C
C      CALCULATE X,Y DERIVERTIVES OF SHAPE FUNCTION AND DETERMINANT
C         ( 2-D CALCULATION )
C
C
C     NOTE ; 1. CALCULATION IS CARRIED OUT AT A GIVEN LOCATION(G,E)
C              IN ALL THE ELEMENTS.
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          G,E         ; GZAI, EATA COORDINATE WHERE CALCULATION DONE
C          X     (IP)  ; X-COORDINATE OF NODE
C          Y     (IP)  ; Y-COORDINATE OF NODE
C          NODE(I,IE)  ; NODE NO. TABLE
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT
C
C       (2) OUTPUT
C          SNI (I,IE)  ; SHAPE FUNCTION
C          DNXI(I,IE)  ; X-DERIVERTIVES OF SHAPE FUNCTION N(I)
C          DNYI(I,IE)  ; Y-DERIVERTIVES OF SHAPE FUNCTION N(I)
C          DET   (IE)  ; DETERMINANT
C
C       (3) WORK
C          DXG   (IE)  ; DX/DG OF ELEMENT
C          DYG   (IE)  ; DY/DG OF ELEMENT
C          DXE   (IE)  ; DX/DE OF ELEMENT
C          DYE   (IE)  ; DY/DE OF ELEMENT
C
C
C
      DO 10 I = 1 , N
            DN (I) = SHAPE2(I,G,E,1)
            DNG(I) = SHAPE2(I,G,E,2)
            DNE(I) = SHAPE2(I,G,E,3)
   10 CONTINUE
C
      DO 100 IE = 1 , NE
          DXG(IE) = DNG(1)*X(NODE(1,IE))+DNG(2)*X(NODE(2,IE))
     &             +DNG(3)*X(NODE(3,IE))+DNG(4)*X(NODE(4,IE))
          DYG(IE) = DNG(1)*Y(NODE(1,IE))+DNG(2)*Y(NODE(2,IE))
     &             +DNG(3)*Y(NODE(3,IE))+DNG(4)*Y(NODE(4,IE))
          DXE(IE) = DNE(1)*X(NODE(1,IE))+DNE(2)*X(NODE(2,IE))
     &             +DNE(3)*X(NODE(3,IE))+DNE(4)*X(NODE(4,IE))
          DYE(IE) = DNE(1)*Y(NODE(1,IE))+DNE(2)*Y(NODE(2,IE))
     &             +DNE(3)*Y(NODE(3,IE))+DNE(4)*Y(NODE(4,IE))
  100 CONTINUE
C
      DO 200 IE = 1 , NE
          DET(IE) = DXG(IE)*DYE(IE)-DYG(IE)*DXE(IE)
  200 CONTINUE
C
      DO 310 I = 1 , N
          DO 300 IE = 1 , NE
              SNI (I,IE) = DN(I)
              DNXI(I,IE) = ( DYE(IE)*DNG(I)-DYG(IE)*DNE(I))/DET(IE)
              DNYI(I,IE) = (-DXE(IE)*DNG(I)+DXG(IE)*DNE(I))/DET(IE)
  300     CONTINUE
  310 CONTINUE
C
C
      RETURN
      END
