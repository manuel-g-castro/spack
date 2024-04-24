C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    ELEM3B                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE ELEM3B(X,Y,Z,NODE,NE,NP,N,LEB,NB,
     *                  SNB ,DNXB,DNYB,DNZB,H,DET,
     *                  DXG,DYG,DZG,
     *                  DXE,DYE,DZE,
     *                  DXT,DYT,DZT,DGET)
      IMPLICIT REAL*4(A-H,O-Z)
      PARAMETER ( P1 = -0.577350E0 , U1 = -1.E0 )
      PARAMETER ( P2 =  0.577350E0 , U2 =  1.E0 )
      DIMENSION X(NP),Y(NP),Z(NP),NODE(N,NE),LEB(2,NB),
     1          SNB(N,NB),DNXB(N,NB),DNYB(N,NB),DNZB(N,NB),H(NB),
     2          DET(NB),DXG(NB),DYG(NB),DZG(NB),DXE(NB),DYE(NB),DZE(NB),
     3          DXT(NB),DYT(NB),DZT(NB),DGET(N,NB)
C
      DIMENSION DN(8,4,6),DNG(8,4,6),DNE(8,4,6),DNT(8,4,6)
      DIMENSION GP(3,4,6),FL(3,6)
C
      DATA GP /U1, P1, P1,    U1, P1, P2,    U1, P2, P1,    U1, P2, P2,
     &         U2, P1, P1,    U2, P1, P2,    U2, P2, P1,    U2, P2, P2,
     &         P1, U1, P1,    P2, U1, P1,    P1, U1, P2,    P2, U1, P2,
     &         P1, U2, P1,    P2, U2, P1,    P1, U2, P2,    P2, U2, P2,
     &         P1, P1, U1,    P1, P2, U1,    P2, P1, U1,    P2, P2, U1,
     &         P1, P1, U2,    P1, P2, U2,    P2, P1, U2,    P2, P2, U2/
      DATA FL /1.E0, 0.E0, 0.E0, 1.E0, 0.E0, 0.E0,
     &         0.E0, 1.E0, 0.E0, 0.E0, 1.E0, 0.E0,
     &         0.E0, 0.E0, 1.E0, 0.E0, 0.E0, 1.E0/
      DATA W / 1.E0 /
C
C
C      INTEGRAL SHAPE FUNCTION AND ITS X, Y, Z DERIVATIVES ON SPECIFIED
C     SURFACE OF SPECIFIED ELEMENTS
C         ( 3-D CALCULATION : SINGLE WORD & SAME ELEMENT VERSION )
C
C
C     NOTES ; SURFACE INTEGRATION WILL BE DONE USING 4 GAUSS POINTS.
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          X      (IP) ; X-DIR. COORDINATE         OF NODE
C          Y      (IP) ; Y-DIR. COORDINATE         OF NODE
C          Z      (IP) ; Y-DIR. COORDINATE         OF NODE
C          NODE (I,IE) ; NODE TABLE
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          NP          ; NUMBER OF TOTAL    NODES
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT
C          LEB  (I,IB) ; ELEMENT(1, & SURFACE(2, NUMBER SPECIFYING LIST
C          NB          ; NUMBER OF SURFACES
C
C       (2) OUTPUT
C          SNB  (I,IB) ; INTEGRATED SHAPE FUNCTION
C          DNXB (I,IB) ; INTEGRATED X DERIVATIVE OF SHAPE FUNCTION
C          DNYB (I,IB) ; INTEGRATED Y DERIVATIVE OF SHAPE FUNCTION
C          DNZB (I,IB) ; INTEGRATED Z DERIVATIVE OF SHAPE FUNCTION
C
C       (3) WORK
C          H      (IB) ; SURFACE INTEGRAL FACTOR
C          DET    (IB) ; DETERMINANT OF JACOBIAN MATRIX
C          DXG    (IB) ; G-DIR. DERIVERTIVE OF X
C          DYG    (IB) ; G-DIR. DERIVERTIVE OF Y
C          DZG    (IB) ; G-DIR. DERIVERTIVE OF Z
C          DXE    (IB) ; E-DIR. DERIVERTIVE OF X
C          DYE    (IB) ; E-DIR. DERIVERTIVE OF Y
C          DZE    (IB) ; E-DIR. DERIVERTIVE OF Z
C          DXT    (IB) ; T-DIR. DERIVERTIVE OF X
C          DYT    (IB) ; T-DIR. DERIVERTIVE OF Y
C          DZT    (IB) ; T-DIR. DERIVERTIVE OF Z
C          DGET (K,IB) ; D(G,E,T)/D(X,Y,Z) OF ELEMENT
C
C
C      (1) PREPARE GZAI, EATA, ZEATA DERIVATIVES AT ALL THE GAUSS POINTS
C
      DO 30 IS = 1 , 6
          DO 20 IG = 1 , 4
              GG = GP(1,IG,IS)
              EE = GP(2,IG,IS)
              TT = GP(3,IG,IS)
              DO 10 I = 1 , N
                  DN (I,IG,IS) = SHAPE3(I,GG,EE,TT,1)
                  DNG(I,IG,IS) = SHAPE3(I,GG,EE,TT,2)
                  DNE(I,IG,IS) = SHAPE3(I,GG,EE,TT,3)
                  DNT(I,IG,IS) = SHAPE3(I,GG,EE,TT,4)
   10         CONTINUE
   20     CONTINUE
   30 CONTINUE
C
C      (2) CLEAR ARRAYS
C
      DO 110 I = 1 , N
          DO 100 IB = 1 , NB
              SNB (I,IB) = 0.E0
              DNXB(I,IB) = 0.E0
              DNYB(I,IB) = 0.E0
              DNZB(I,IB) = 0.E0
  100     CONTINUE
  110 CONTINUE
C
C      (3) INTEGRAL USING GAUSS POINTS
C
      DO 1220 IG = 1 , 4
C
C CALCULATE GZAI, EATA, ZEATA DERIVATIVES OF X, Y, Z
C
          DO 200 IB = 1 , NB
              DXG(IB) = 0.E0
              DYG(IB) = 0.E0
              DZG(IB) = 0.E0
              DXE(IB) = 0.E0
              DYE(IB) = 0.E0
              DZE(IB) = 0.E0
              DXT(IB) = 0.E0
              DYT(IB) = 0.E0
              DZT(IB) = 0.E0
  200     CONTINUE
C
          DO 310 I = 1 , N
              DO 300 IB = 1 , NB
                  IE = LEB(1,IB)
                  IS = LEB(2,IB)
                  DXG(IB) = DXG(IB)+DNG(I,IG,IS)*X(NODE(I,IE))
                  DYG(IB) = DYG(IB)+DNG(I,IG,IS)*Y(NODE(I,IE))
                  DZG(IB) = DZG(IB)+DNG(I,IG,IS)*Z(NODE(I,IE))
                  DXE(IB) = DXE(IB)+DNE(I,IG,IS)*X(NODE(I,IE))
                  DYE(IB) = DYE(IB)+DNE(I,IG,IS)*Y(NODE(I,IE))
                  DZE(IB) = DZE(IB)+DNE(I,IG,IS)*Z(NODE(I,IE))
                  DXT(IB) = DXT(IB)+DNT(I,IG,IS)*X(NODE(I,IE))
                  DYT(IB) = DYT(IB)+DNT(I,IG,IS)*Y(NODE(I,IE))
                  DZT(IB) = DZT(IB)+DNT(I,IG,IS)*Z(NODE(I,IE))
  300         CONTINUE
  310     CONTINUE
C
C SURFACE INTEGRAL FACTOR H
C
          DO 400 IB = 1 , NB
              IS = LEB(2,IB)
            E=FL(1,IS)*(DXE(IB)*DXE(IB)+DYE(IB)*DYE(IB)+DZE(IB)*DZE(IB))
     &       +FL(2,IS)*(DXT(IB)*DXT(IB)+DYT(IB)*DYT(IB)+DZT(IB)*DZT(IB))
     &       +FL(3,IS)*(DXG(IB)*DXG(IB)+DYG(IB)*DYG(IB)+DZG(IB)*DZG(IB))
            F=FL(1,IS)*(DXT(IB)*DXT(IB)+DYT(IB)*DYT(IB)+DZT(IB)*DZT(IB))
     &       +FL(2,IS)*(DXG(IB)*DXG(IB)+DYG(IB)*DYG(IB)+DZG(IB)*DZG(IB))
     &       +FL(3,IS)*(DXE(IB)*DXE(IB)+DYE(IB)*DYE(IB)+DZE(IB)*DZE(IB))
            G=FL(1,IS)*(DXE(IB)*DXT(IB)+DYE(IB)*DYT(IB)+DZE(IB)*DZT(IB))
     &       +FL(2,IS)*(DXT(IB)*DXG(IB)+DYT(IB)*DYG(IB)+DZT(IB)*DZG(IB))
     &       +FL(3,IS)*(DXG(IB)*DXE(IB)+DYG(IB)*DYE(IB)+DZG(IB)*DZE(IB))
              H(IB) = SQRT(E*F-G*G)
  400     CONTINUE
C
C DETERMINANT
C
          DO 500 IB = 1 , NB
              DET(IB) = DXG(IB)*(DYE(IB)*DZT(IB)-DZE(IB)*DYT(IB))
     &                 +DYG(IB)*(DZE(IB)*DXT(IB)-DXE(IB)*DZT(IB))
     &                 +DZG(IB)*(DXE(IB)*DYT(IB)-DYE(IB)*DXT(IB))
  500     CONTINUE
C
C SHAPE FUNCTION
C
          DO 610 I = 1 , N
              DO 600 IB = 1 , NB
                  IE = LEB(1,IB)
                  IS = LEB(2,IB)
                  SNB(I,IB) = SNB(I,IB)+W*H(IB)*DN(I,IG,IS)
  600         CONTINUE
  610     CONTINUE
C
C X-DERIVERTIVE OF SHAPE FUNCTION
C
          DO 700 IB = 1 , NB
              DGET(1,IB) = (DYE(IB)*DZT(IB)-DYT(IB)*DZE(IB))/DET(IB)
              DGET(2,IB) = (DYT(IB)*DZG(IB)-DYG(IB)*DZT(IB))/DET(IB)
              DGET(3,IB) = (DYG(IB)*DZE(IB)-DYE(IB)*DZG(IB))/DET(IB)
  700     CONTINUE
          DO 810 I = 1 , N
              DO 800 IB = 1 , NB
                  IS = LEB(2,IB)
                  DNXB(I,IB) = DNXB(I,IB)
     &                        +W*H(IB)*(DGET(1,IB)*DNG(I,IG,IS)
     &                                 +DGET(2,IB)*DNE(I,IG,IS)
     &                                 +DGET(3,IB)*DNT(I,IG,IS))
  800         CONTINUE
  810     CONTINUE
C
C Y-DERIVERTIVE OF SHAPE FUNCTION
C
          DO 900 IB = 1 , NB
              DGET(1,IB) = (DZE(IB)*DXT(IB)-DZT(IB)*DXE(IB))/DET(IB)
              DGET(2,IB) = (DZT(IB)*DXG(IB)-DZG(IB)*DXT(IB))/DET(IB)
              DGET(3,IB) = (DZG(IB)*DXE(IB)-DZE(IB)*DXG(IB))/DET(IB)
  900     CONTINUE
C
          DO 1010 I = 1 , N
              DO 1000 IB = 1 , NB
                  IS = LEB(2,IB)
                  DNYB(I,IB) = DNYB(I,IB)
     &                        +W*H(IB)*(DGET(1,IB)*DNG(I,IG,IS)
     &                                 +DGET(2,IB)*DNE(I,IG,IS)
     &                                 +DGET(3,IB)*DNT(I,IG,IS))
 1000         CONTINUE
 1010     CONTINUE
C
C Z-DERIVERTIVE OF SHAPE FUNCTION
C
          DO 1100 IB = 1 , NB
              DGET(1,IB) = (DXE(IB)*DYT(IB)-DXT(IB)*DYE(IB))/DET(IB)
              DGET(2,IB) = (DXT(IB)*DYG(IB)-DXG(IB)*DYT(IB))/DET(IB)
              DGET(3,IB) = (DXG(IB)*DYE(IB)-DXE(IB)*DYG(IB))/DET(IB)
 1100     CONTINUE
C
          DO 1210 I = 1 , N
              DO 1200 IB = 1 , NB
                  IS = LEB(2,IB)
                  DNZB(I,IB) = DNZB(I,IB)
     &                        +W*H(IB)*(DGET(1,IB)*DNG(I,IG,IS)
     &                                 +DGET(2,IB)*DNE(I,IG,IS)
     &                                 +DGET(3,IB)*DNT(I,IG,IS))
 1200         CONTINUE
 1210     CONTINUE
 1220 CONTINUE
C
C
      RETURN
      END
