C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    COSIN3                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE COSIN3(LOCAL,X,Y,Z,NODE,NE,NP,N,LEB,NB,
     *                  XNB,YNB,ZNB,A,AOUT)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION X(NP),Y(NP),Z(NP),NODE(N,NE),
     1          LEB(2,NB),XNB(NB),YNB(NB),ZNB(NB),A(NB),AOUT(NB)
C
      DIMENSION LOCAL(4,6)
C
C
C      CALCULATE ELEMENT'S SURFACE INWARD NORMAL VECTOR WITH UNIT LENGTH
C     AND SURFACE AREA
C         ( 3-D ; SINGLE PRECISION, REFERING ELEMENT BY TYPE LIST )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          LOCAL (I,IS); NODE NUMBER TABLE  DEFINING ELEMENT'S SURFACES
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
C          XNB    (IB) ; X COMPONENT OF THE NORMAL VECTOR OF SURFACE IB
C          YNB    (IB) ; Y COMPONENT OF THE NORMAL VECTOR OF SURFACE IB
C          ZNB    (IB) ; X COMPONENT OF THE NORMAL VECTOR OF SURFACE IB
C          A      (IB) ; AREA                             OF SURFACE IB
C
C       (4) WORK
C          AOUT   (IB) ; ABSOLUTE OF NORMAL VECTOR ( AVOID VECTOR AREA )
C
C
      DO 100 IB = 1 , NB
          XNB(IB) = 0.E0
          YNB(IB) = 0.E0
          ZNB(IB) = 0.E0
          A  (IB) = 0.E0
  100 CONTINUE
C
      DO 210 IMEAN = 1 , 3 , 2
          DO 200 IB = 1 , NB
              IE = LEB(1,IB)
              IS = LEB(2,IB)
C
              AX  = X(NODE(LOCAL(IMEAN+1,IS),IE))
     &             -X(NODE(LOCAL(IMEAN  ,IS),IE))
              AY  = Y(NODE(LOCAL(IMEAN+1,IS),IE))
     &             -Y(NODE(LOCAL(IMEAN  ,IS),IE))
              AZ  = Z(NODE(LOCAL(IMEAN+1,IS),IE))
     &             -Z(NODE(LOCAL(IMEAN  ,IS),IE))
C
              BX  = X(NODE(LOCAL(MOD(IMEAN+2,4),IS),IE))
     &             -X(NODE(LOCAL(IMEAN+1,       IS),IE))
              BY  = Y(NODE(LOCAL(MOD(IMEAN+2,4),IS),IE))
     &             -Y(NODE(LOCAL(IMEAN+1,       IS),IE))
              BZ  = Z(NODE(LOCAL(MOD(IMEAN+2,4),IS),IE))
     &             -Z(NODE(LOCAL(IMEAN+1,       IS),IE))
C
              XNB (IB) = XNB(IB)+AY*BZ-AZ*BY
              YNB (IB) = YNB(IB)+AZ*BX-AX*BZ
              ZNB (IB) = ZNB(IB)+AX*BY-AY*BX
              AOUT(IB) = (AY*BZ-AZ*BY)**2+(AZ*BX-AX*BZ)**2
     &                  +(AX*BY-AY*BX)**2
              A   (IB) = A  (IB)+0.5E0*SQRT(AOUT(IB))
  200     CONTINUE
  210 CONTINUE
C
      DO 300 IB = 1 , NB
          AOUT(IB) = XNB(IB)*XNB(IB)+YNB(IB)*YNB(IB)+ZNB(IB)*ZNB(IB)
          XNB(IB) = XNB(IB)/SQRT(AOUT(IB))
          YNB(IB) = YNB(IB)/SQRT(AOUT(IB))
          ZNB(IB) = ZNB(IB)/SQRT(AOUT(IB))
  300 CONTINUE
C
C
      RETURN
      END
