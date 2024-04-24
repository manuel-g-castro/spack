C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    PROKE3                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE PROKE3(IPRDWL,VISC,VISCM,U,V,W,PD,NEWALL,LEWALL,
     *                  DNXI,DNYI,DNZI,NODE,NE,NP,N,
     *                  DUX,DUY,DUZ,DVX,DVY,DVZ,DWX,DWY,DWZ)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION VISC(NE),U(NP),V(NP),W(NP),PD(NE),LEWALL(2,NEWALL),
     1          DNXI(N,NE),DNYI(N,NE),DNZI(N,NE),NODE(N,NE),
     2          DUX(NE),DUY(NE),DUZ(NE),DVX(NE),DVY(NE),DVZ(NE),
     3          DWX(NE),DWY(NE),DWZ(NE)
C
      DATA EPS / 1.0E-30 /
C
C
C      CALCULATE PRODUCTION TERM OF K-EQUATION
C         ( 3-D CALCULATION )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          IPRDWL      ; CONTROLS PRODUCTION IN WALL-ADJACENT ELEMENTS
C                   0 --- CLEAR     PRODUCTION
C                   1 --- CALCULATE PRODUCTION AS IT IS
C          VISC    (IE); ELEMENT VISCOSITY ( MOLECULAR+TURBULENT )
C          VISCM       ; MOLECULAR VISCOSITY
C          U       (IP); X-DIR. VELOCITY COMPONENT
C          V       (IP); Y-DIR. VELOCITY COMPONENT
C          W       (IP); Z-DIR. VELOCITY COMPONENT
C          NEWALL      ; NUMBER OF WALL BOUNDARY SURFACES
C          LEWALL(I,IBE); WALL BOUNDARY ELEMENT AND ITS SURFACE
C          DNXI  (I,IE); ELEMENT CENTER VALUE OF NX
C          DNYI  (I,IE); ELEMENT CENTER VALUE OF NY
C          DNZI  (I,IE); ELEMENT CENTER VALUE OF NZ
C          NODE  (I,IE); NODE NO. TABLE BASED ON ELEMENT
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          NP          ; NUMBER OF TOTAL     NODES
C          N           ; NUMBER OF NODE ASSIGNED TO ONE ELEMENT
C
C       (2) OUTPUT
C          PD      (IE); PRODUCTION TERM FOR K-EQUATION
C
C       (3) WORK
C           NOTES ; PREPARE THE FOLLOWING ARRAYS FOR MAX(IE) = NE.
C          DUX     (IE); DU/DX
C          DUX     (IE); DU/DY
C          DUX     (IE); DU/DZ
C          DVX     (IE); DV/DX
C          DVY     (IE); DV/DY
C          DVZ     (IE); DV/DZ
C          DWX     (IE); DW/DX
C          DWY     (IE); DW/DY
C          DWZ     (IE); DW/DZ
C
C
      DO 100 IE = 1 , NE
          DUX(IE) = 0.E0
          DUY(IE) = 0.E0
          DUZ(IE) = 0.E0
C
          DVX(IE) = 0.E0
          DVY(IE) = 0.E0
          DVZ(IE) = 0.E0
C
          DWX(IE) = 0.E0
          DWY(IE) = 0.E0
          DWZ(IE) = 0.E0
  100 CONTINUE
C
      DO 210 I = 1 , N
          DO 200 IE = 1 , NE
              DUX(IE) = DUX(IE)+DNXI(I,IE)*U(NODE(I,IE))
              DUY(IE) = DUY(IE)+DNYI(I,IE)*U(NODE(I,IE))
              DUZ(IE) = DUZ(IE)+DNZI(I,IE)*U(NODE(I,IE))
C
              DVX(IE) = DVX(IE)+DNXI(I,IE)*V(NODE(I,IE))
              DVY(IE) = DVY(IE)+DNYI(I,IE)*V(NODE(I,IE))
              DVZ(IE) = DVZ(IE)+DNZI(I,IE)*V(NODE(I,IE))
C
              DWX(IE) = DWX(IE)+DNXI(I,IE)*W(NODE(I,IE))
              DWY(IE) = DWY(IE)+DNYI(I,IE)*W(NODE(I,IE))
              DWZ(IE) = DWZ(IE)+DNZI(I,IE)*W(NODE(I,IE))
  200     CONTINUE
  210 CONTINUE
C
      DO 300 IE = 1 , NE
          VISCT = VISC(IE)-VISCM
          IF(VISCT.LT.EPS) VISCT = EPS
          PD(IE) = VISCT
     &          *(2.E0*(DUX(IE)*DUX(IE)+DVY(IE)*DVY(IE)+DWZ(IE)*DWZ(IE))
     &           +2.E0*(DWY(IE)*DVZ(IE)+DUZ(IE)*DWX(IE)+DVX(IE)*DUY(IE))
     &                 +DWY(IE)*DWY(IE)+DVZ(IE)*DVZ(IE)+DUZ(IE)*DUZ(IE)
     &                 +DWX(IE)*DWX(IE)+DVX(IE)*DVX(IE)+DUY(IE)*DUY(IE))
  300 CONTINUE
C
      IF(IPRDWL.EQ.0) THEN
          DO 400 IEWALL = 1 , NEWALL
              PD(LEWALL(1,IEWALL)) = 0.E0
  400     CONTINUE
      ENDIF
C
C
      RETURN
      END
