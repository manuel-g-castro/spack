C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    FORCE3                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE FORCE3(IMODE,U,V,W,P,VISC,NODE,NE,NP,N,
     *                  A,XNB,YNB,ZNB,DNXB,DNYB,DNZB,LEB,NB,
     *                  FX,FY,FZ)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION U(NP),V(NP),W(NP),P(NE),VISC(NE),NODE(N,NE),
     1          A(NB),XNB(NB),YNB(NB),ZNB(NB),
     2          DNXB(N,NB),DNYB(N,NB),DNZB(N,NB),LEB(2,NB)
C
C
C      CALCULATE FLUID FORCE ACTING ON A SPECIFIED BODY
C         ( 3-D CALCULATION : SINGLE WORD & SAME ELEMENT VERSION )
C
C
C     NOTES ; CALL SUBROUTINES 'ELEM3B' AND 'COSIN3' FIRST
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          IMODE       ; SPECIFIES THE TYPE OF CALCULATION AS FOLLOWS
C                   1 --- CALCULATE PRESSURE FORCE
C                   2 --- CALCULATE VISCOUS  FORCE
C                   3 --- CALCULATE TOTAL    FORCE
C          U       (IP); X-DIR. VELOCITY COMPONENT
C          V       (IP); Y-DIR. VELOCITY COMPONENT
C          W       (IP); Z-DIR. VELOCITY COMPONENT
C          P       (IE); ELEMENT PRESSURE
C          VISC    (IE); ELEMENT VISCOSITY
C          NODE  (I,IE); NODE NO. TABLE BASED ON ELEMENT
C          NE          ; NUMBER OF TOTAL     ELEMENTS
C          NP          ; NUMBER OF TOTAL     NODES
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT
C          A       (IB); AREA                             OF SURFACE IB
C          XNB     (IB); X COMPONENT OF THE NORMAL VECTOR OF SURFACE IB
C          YNB     (IB); Y COMPONENT OF THE NORMAL VECTOR OF SURFACE IB
C          ZNB     (IB); X COMPONENT OF THE NORMAL VECTOR OF SURFACE IB
C          DNXB (I,IB) ; INTEGRATED X DERIVATIVE OF SHAPE FUNCTION
C          DNYB (I,IB) ; INTEGRATED Y DERIVATIVE OF SHAPE FUNCTION
C          DNZB (I,IB) ; INTEGRATED Z DERIVATIVE OF SHAPE FUNCTION
C          LEB  (I,IB) ; ELEMENT(1, & SURFACE(2, NUMBER SPECIFYING LIST
C          NB          ; NUMBER OF SURFACES
C
C       (2) OUTPUT
C          FX           ; X COMPONENT OF FLUID FORCE ACTING ON THE BODY
C          FY           ; Y COMPONENT OF FLUID FORCE ACTING ON THE BODY
C          FZ           ; Z COMPONENT OF FLUID FORCE ACTING ON THE BODY
C
C
      FX = 0.E0
      FY = 0.E0
      FZ = 0.E0
C
C      CALCULATE PRESSURE FORCE
C
      IF(IMODE.EQ.1 .OR. IMODE.EQ.3) THEN
      DO 100 IB = 1 , NB
          FX = FX-XNB(IB)*A(IB)*P(LEB(1,IB))
          FY = FY-YNB(IB)*A(IB)*P(LEB(1,IB))
          FZ = FZ-ZNB(IB)*A(IB)*P(LEB(1,IB))
  100 CONTINUE
      ENDIF
C
C      CALCULATE VISCOUS  FORCE
C
      IF(IMODE.EQ.2 .OR. IMODE.EQ.3) THEN
      DO 210 I = 1 , N
          DO 200 IB = 1 , NB
              IE = LEB(1,IB)
              IP = NODE(I,IE)
              FX = FX+VISC(IE)
     &            *(XNB(IB)*U(IP)*DNXB(I,IB)+XNB(IB)*U(IP)*DNXB(I,IB)
     &             +YNB(IB)*U(IP)*DNYB(I,IB)+YNB(IB)*V(IP)*DNXB(I,IB)
     &             +ZNB(IB)*U(IP)*DNZB(I,IB)+ZNB(IB)*W(IP)*DNXB(I,IB))
              FY = FY+VISC(IE)
     &            *(XNB(IB)*V(IP)*DNXB(I,IB)+XNB(IB)*U(IP)*DNYB(I,IB)
     &             +YNB(IB)*V(IP)*DNYB(I,IB)+YNB(IB)*V(IP)*DNYB(I,IB)
     &             +ZNB(IB)*V(IP)*DNZB(I,IB)+ZNB(IB)*W(IP)*DNYB(I,IB))
              FZ = FZ+VISC(IE)
     &            *(XNB(IB)*W(IP)*DNXB(I,IB)+XNB(IB)*U(IP)*DNZB(I,IB)
     &             +YNB(IB)*W(IP)*DNYB(I,IB)+YNB(IB)*V(IP)*DNZB(I,IB)
     &             +ZNB(IB)*W(IP)*DNZB(I,IB)+ZNB(IB)*W(IP)*DNZB(I,IB))
  200     CONTINUE
  210 CONTINUE
      ENDIF
C
C
      RETURN
      END
