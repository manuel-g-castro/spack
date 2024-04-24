C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    DERIV3                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE DERIV3(G,E,T,X,Y,Z,NODE,NE,NP,N,
     *                  SNI,DNXI,DNYI,DNZI,DET,
     *                  DXG,DYG,DZG,DXE,DYE,DZE,DXT,DYT,DZT,DGET)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION X(NP),Y(NP),Z(NP),NODE(N,NE),
     1          SNI(N,NE),DNXI(N,NE),DNYI(N,NE),DNZI(N,NE),DET(NE),
     2          DXG(NE),DYG(NE),DZG(NE),
     3          DXE(NE),DYE(NE),DZE(NE),
     4          DXT(NE),DYT(NE),DZT(NE),DGET(N,NE)
C
      DIMENSION DN(8),DNG(8),DNE(8),DNT(8)
C
C
C      CALCULATE X,Y,Z DERIVERTIVES OF SHAPE FUNCTION AND DETERMINANT
C         ( 3-D CALCULATION : SINGLE WORD VERSION )
C
C
C     NOTE ; 1. CALCULATION IS CARRIED OUT AT A GIVEN LOCATION(G,E,T)
C              IN ALL THE ELEMENTS.
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          G,E,T       ; GZAI, EATA, TETA COORDINATE
C                       WHERE CALCULATION DONE
C          X     (IP)  ; X-DIR. COORDINATE         OF NODE
C          Y     (IP)  ; Y-DIR. COORDINATE         OF NODE
C          Z     (IP)  ; Y-DIR. COORDINATE         OF NODE
C          NODE(I,IE)  ; NODE TABLE
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          NP          ; NUMBER OF TOTAL    NODES
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT
C
C       (2) OUTPUT
C          SNI (I,IE)  ; SHAPE FUNCTION
C          DNXI(I,IE)  ; X-DERIVERTIVES OF SHAPE FUNCTION N(I)
C          DNYI(I,IE)  ; Y-DERIVERTIVES OF SHAPE FUNCTION N(I)
C          DNZI(I,IE)  ; Z-DERIVERTIVES OF SHAPE FUNCTION N(I)
C          DET   (IE)  ; DETERMINANT
C
C       (3) WORK
C          DXG   (IE)  ; DX/DG             OF ELEMENT
C          DYG   (IE)  ; DY/DG             OF ELEMENT
C          DZG   (IE)  ; DZ/DG             OF ELEMENT
C          DXE   (IE)  ; DX/DE             OF ELEMENT
C          DYE   (IE)  ; DY/DE             OF ELEMENT
C          DZE   (IE)  ; DZ/DE             OF ELEMENT
C          DXT   (IE)  ; DX/DT             OF ELEMENT
C          DYT   (IE)  ; DY/DT             OF ELEMENT
C          DZT   (IE)  ; DZ/DT             OF ELEMENT
C          DGET(K,IE)  ; D(G,E,T)/D(X,Y,Z) OF ELEMENT
C
C
C      (1) D(X,Y,Z)/D(G,E,T)
C
      DO 10 I = 1 , N
            DN (I) = SHAPE3(I,G,E,T,1)
            DNG(I) = SHAPE3(I,G,E,T,2)
            DNE(I) = SHAPE3(I,G,E,T,3)
            DNT(I) = SHAPE3(I,G,E,T,4)
   10 CONTINUE
C
      DO 100 IE = 1 , NE
          DXG(IE) = 0.E0
          DYG(IE) = 0.E0
          DZG(IE) = 0.E0
          DXE(IE) = 0.E0
          DYE(IE) = 0.E0
          DZE(IE) = 0.E0
          DXT(IE) = 0.E0
          DYT(IE) = 0.E0
          DZT(IE) = 0.E0
  100 CONTINUE
C
      DO 210 I = 1 , N
          DO 200 IE = 1 , NE
             DXG(IE) = DXG(IE)+DNG(I)*X(NODE(I,IE))
             DYG(IE) = DYG(IE)+DNG(I)*Y(NODE(I,IE))
             DZG(IE) = DZG(IE)+DNG(I)*Z(NODE(I,IE))
             DXE(IE) = DXE(IE)+DNE(I)*X(NODE(I,IE))
             DYE(IE) = DYE(IE)+DNE(I)*Y(NODE(I,IE))
             DZE(IE) = DZE(IE)+DNE(I)*Z(NODE(I,IE))
             DXT(IE) = DXT(IE)+DNT(I)*X(NODE(I,IE))
             DYT(IE) = DYT(IE)+DNT(I)*Y(NODE(I,IE))
             DZT(IE) = DZT(IE)+DNT(I)*Z(NODE(I,IE))
  200     CONTINUE
  210 CONTINUE
C
C      (2) DETERMINANT OF D(X,Y,Z)/D(G,E,T)
C
      DO 300 IE = 1 , NE
          DET(IE) = DXG(IE)*(DYE(IE)*DZT(IE)-DZE(IE)*DYT(IE))
     &             +DYG(IE)*(DZE(IE)*DXT(IE)-DXE(IE)*DZT(IE))
     &             +DZG(IE)*(DXE(IE)*DYT(IE)-DYE(IE)*DXT(IE))
  300 CONTINUE
C
C      (3) SHAPE FUNCTION
C
      DO 400 IE = 1 , NE
          SNI(1,IE) = DN(1)
          SNI(5,IE) = DN(5)
          SNI(2,IE) = DN(2)
          SNI(6,IE) = DN(6)
          SNI(3,IE) = DN(3)
          SNI(7,IE) = DN(7)
          SNI(4,IE) = DN(4)
          SNI(8,IE) = DN(8)
  400 CONTINUE
C
C      (4) D(G,E,T)/DX
C
      DO 500 IE = 1 , NE
          DGET(1,IE) = (DYE(IE)*DZT(IE)-DYT(IE)*DZE(IE))/DET(IE)
          DGET(2,IE) = (DYT(IE)*DZG(IE)-DYG(IE)*DZT(IE))/DET(IE)
          DGET(3,IE) = (DYG(IE)*DZE(IE)-DYE(IE)*DZG(IE))/DET(IE)
  500 CONTINUE
C
C      (5) X-DERIVERTIVE OF SHAPE FUNCTION
C
      DO 610 I = 1 , N
          DO 600 IE = 1 , NE
              DNXI(I,IE) = DGET(1,IE)*DNG(I)
     &                    +DGET(2,IE)*DNE(I)
     &                    +DGET(3,IE)*DNT(I)
  600     CONTINUE
  610 CONTINUE
C
C      (6) D(G,E,T)/DY
C
      DO 700 IE = 1 , NE
          DGET(1,IE) = (DZE(IE)*DXT(IE)-DZT(IE)*DXE(IE))/DET(IE)
          DGET(2,IE) = (DZT(IE)*DXG(IE)-DZG(IE)*DXT(IE))/DET(IE)
          DGET(3,IE) = (DZG(IE)*DXE(IE)-DZE(IE)*DXG(IE))/DET(IE)
  700 CONTINUE
C
C      (7) Y-DERIVERTIVE OF SHAPE FUNCTION
C
      DO 810 I = 1 , N
          DO 800 IE = 1 , NE
              DNYI(I,IE) = DGET(1,IE)*DNG(I)
     &                    +DGET(2,IE)*DNE(I)
     &                    +DGET(3,IE)*DNT(I)
  800     CONTINUE
  810 CONTINUE
C
C      (8) D(G,E,T)/DZ
C
      DO 900 IE = 1 , NE
          DGET(1,IE) = (DXE(IE)*DYT(IE)-DXT(IE)*DYE(IE))/DET(IE)
          DGET(2,IE) = (DXT(IE)*DYG(IE)-DXG(IE)*DYT(IE))/DET(IE)
          DGET(3,IE) = (DXG(IE)*DYE(IE)-DXE(IE)*DYG(IE))/DET(IE)
  900 CONTINUE
C
C      (9) Z-DERIVERTIVE OF SHAPE FUNCTION
C
      DO 1010 I = 1 , N
          DO 1000 IE = 1 , NE
              DNZI(I,IE) = DGET(1,IE)*DNG(I)
     &                    +DGET(2,IE)*DNE(I)
     &                    +DGET(3,IE)*DNT(I)
 1000     CONTINUE
 1010 CONTINUE
C
C
      RETURN
      END
