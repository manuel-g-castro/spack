C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.8.1                                   C
C                                                                      C
C  SUB ROUTINE : FORC3X                                                C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C CONTACT ADDRESS : IIS, THE UNIVERSITY OF TOKYO, CISS                 C
C                                                                      C
C THERMO-FLUID ANALYSIS SOLVERS FOR LARGE-SCALE-ASSEMBLY               C
C                                                                      C
C======================================================================C
      SUBROUTINE FORC3X(IMODE,U,V,W,P,VISC,NODE,NE,NP,N1,N2,
     *                  A,XNB,YNB,ZNB,DNXB,DNYB,DNZB,LEB,NB,
     *                  FX,FY,FZ,IVOF,NEFLD2,LEFLD2,LEFIX,
     *                  FXVIS,FYVIS,FZVIS)
C
      IMPLICIT NONE
C
      INTEGER*4 IMODE,NODE,NE,NP,N1,N2,LEB,NB
      REAL*4    U,V,W,P,VISC,
     *          A,XNB,YNB,ZNB,DNXB,DNYB,DNZB,
     *          FX,FY,FZ,FXVIS,FYVIS,FZVIS
C
      INTEGER*4 IE,IB,I,IP
      REAL*4    FXBUF,FYBUF,FZBUF
C
      DIMENSION U(NP),V(NP),W(NP),P(NE),VISC(NE),NODE(N2,NE),
     1          A(NB),XNB(NB),YNB(NB),ZNB(NB),
     2          LEB(2,NB),
     3          DNXB(N1,NB),DNYB(N1,NB),DNZB(N1,NB),
     4          FXVIS(NB),FYVIS(NB),FZVIS(NB)
C
C     [INPUT:VOF]
      INTEGER*4 IVOF,NEFLD2,LEFLD2(NEFLD2)
C
C     [WORK]
      INTEGER*4 LEFIX(NE)
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
      DO 1000 IE=1,NE
         LEFIX(IE)=0
 1000 CONTINUE
C
      IF (IVOF.EQ.1) THEN
!ocl norecurrence(LEFIX)
         DO 1100 IB=1,NEFLD2
            LEFIX(LEFLD2(IB))=1
 1100    CONTINUE
      ENDIF
C
      FX = 0.E0
      FY = 0.E0
      FZ = 0.E0
C
C      CALCULATE PRESSURE FORCE
C
      IF(IMODE.EQ.1 .OR. IMODE.EQ.3) THEN
      DO 100 IB = 1 , NB
          IE = LEB(1,IB)
          IF(LEFIX(IE).EQ.1) GOTO 100
          FX = FX-XNB(IB)*A(IB)*P(LEB(1,IB))
          FY = FY-YNB(IB)*A(IB)*P(LEB(1,IB))
          FZ = FZ-ZNB(IB)*A(IB)*P(LEB(1,IB))
  100 CONTINUE
      ENDIF
C
C      CALCULATE VISCOUS  FORCE
C
      IF(IMODE.EQ.2 .OR. IMODE.EQ.3) THEN
C
      DO 150 IB=1,NB 
          FXVIS(IB)=0.0E0
          FYVIS(IB)=0.0E0
          FZVIS(IB)=0.0E0
  150 CONTINUE     
C
      DO 210 I = 1 , N2
          DO 200 IB = 1 , NB
              IE = LEB(1,IB)
              IP = NODE(I,IE)
              IF(LEFIX(IE).EQ.1) GOTO 200
              IF(IP.GE.1) THEN
                 FXBUF=VISC(IE)
     &              *(XNB(IB)*U(IP)*DNXB(I,IB)+XNB(IB)*U(IP)*DNXB(I,IB)
     &               +YNB(IB)*U(IP)*DNYB(I,IB)+YNB(IB)*V(IP)*DNXB(I,IB)
     &               +ZNB(IB)*U(IP)*DNZB(I,IB)+ZNB(IB)*W(IP)*DNXB(I,IB))
                 FYBUF=VISC(IE)
     &              *(XNB(IB)*V(IP)*DNXB(I,IB)+XNB(IB)*U(IP)*DNYB(I,IB)
     &               +YNB(IB)*V(IP)*DNYB(I,IB)+YNB(IB)*V(IP)*DNYB(I,IB)
     &               +ZNB(IB)*V(IP)*DNZB(I,IB)+ZNB(IB)*W(IP)*DNYB(I,IB))
                 FZBUF=VISC(IE)
     &              *(XNB(IB)*W(IP)*DNXB(I,IB)+XNB(IB)*U(IP)*DNZB(I,IB)
     &               +YNB(IB)*W(IP)*DNYB(I,IB)+YNB(IB)*V(IP)*DNZB(I,IB)
     &               +ZNB(IB)*W(IP)*DNZB(I,IB)+ZNB(IB)*W(IP)*DNZB(I,IB))
C
                 FX=FX+FXBUF
                 FY=FY+FYBUF
                 FZ=FZ+FZBUF
C
                 FXVIS(IB)=FXVIS(IB)+FXBUF
                 FYVIS(IB)=FYVIS(IB)+FYBUF
                 FZVIS(IB)=FZVIS(IB)+FZBUF
              END IF   
  200     CONTINUE
  210 CONTINUE
      ENDIF
C
C
      RETURN
      END
