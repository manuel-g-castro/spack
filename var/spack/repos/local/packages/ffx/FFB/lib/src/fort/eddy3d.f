C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    EDDY3D                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE EDDY3D(CS,FILTER,U,V,W,DNXI,DNYI,DNZI,NODE,NE,NP,N,
     *                  IPRDWL,DAMPWL,LEWALL,NEWALL,NEAR,DSNEAR,UTAU,AP,
     *                  VISCM,VISC,VISCAV,UPPER,DT,
     *                  DUX,DUY,DUZ,DVX,DVY,DVZ,DWX,DWY,DWZ)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION CS(NE),FILTER(NE),U(NP),V(NP),W(NP),LEWALL(2,NEWALL),
     1          NEAR(NE),DSNEAR(NE),UTAU(NEWALL),
     2          DNXI(N,NE),DNYI(N,NE),DNZI(N,NE),NODE(N,NE),
     3          VISC(NE),DUX(NE),DUY(NE),DUZ(NE),DVX(NE),
     4                   DVY(NE),DVZ(NE),DWX(NE),DWY(NE),DWZ(NE)
C
C
C      CALCULATE SUBGRID-SCALE EDDY VISCOSITY BY SMAGORINSKY MODEL
C     WITH VAN-DRIEST DAMPING FUNCTION
C         ( 3-D CALCULATION )
C
C
C     NOTE ; SET ARGUMENT 'NEWALL' TO ZERO TO AVOID THE VAN-DRIEST
C           DAMPING FUNCTION BEING APPLIED. OTHERWISE, SUBGRID-SCALE
C           EDDY VISCOSITY WILL BE DAMPED FOR ALL THE ELEMENTS BASED ON
C           THIS FUNCTION.
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          CS      (IE); ELEMENT SMAGORINSKY CONSTANT
C          FILTER  (IE); ELEMENT GRID FILTER WIDTH
C          U       (IP); X-DIR. VELOCITY COMPONENT
C          V       (IP); Y-DIR. VELOCITY COMPONENT
C          W       (IP); Z-DIR. VELOCITY COMPONENT
C          DNXI  (I,IE); ELEMENT CENTER VALUE OF NX
C          DNYI  (I,IE); ELEMENT CENTER VALUE OF NY
C          DNZI  (I,IE); ELEMENT CENTER VALUE OF NZ
C          NODE  (I,IE); NODE NO. TABLE BASED ON ELEMENT
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          NP          ; NUMBER OF TOTAL    NODES
C          N           ; NUMBER OF NODE ASSIGNED TO ONE ELEMENT
C
C          IPRDWL      ; CONTROLS PRODUCTION IN WALL-ADJACENT ELEMENTS
C                   0 --- CLEAR     PRODUCTION
C                   1 --- CALCULATE PRODUCTION AS IT IS
C                   2 --- DAMP PRODUCTION BY PRI-ORI PARAMETER 'DAMPWL'
C          DAMPWL      ; DAMP PARAMETER FOR WALL PRODUCTION (IPRDWL=2)
C          LEWALL(I,IBE); WALL BOUNDARY ELEMENT AND ITS SURFACE
C          NEWALL      ; NUMBER OF WALL BOUNDARY SURFACES
C          NEAR    (IE); NEAREST WALL SURFACE
C          DSNEAR  (IE); DISTANCE TO THE NEAREST WALL SURFACE
C          UTAU   (IBE); FRICTION VELOCITY AT WALL SURFACES
C          AP          ; VAN-DRIEST DAMPING CONSTANT
C          VISCM       ; MOLECULAR VISCOSITY
C
C          UPPER       ; IF SET TO A POSITIVE VALUE, ELEMENT SUBGRID-
C                       SCALE EDDY VISCOSITY WILL BE UPPER-BOUND SUCH
C                       THAT THE DIFFUSION NUMBER VIOLATION BE AVOIDED
C                       FOR EACH ELEMENT WITH A MARGIN '1.0-UPPER'.
C          DT          ; TIME INCREMENT
C
C       (2) OUTPUT
C          VISC    (IE); ELEMENT VISCOSITY ( MOLECULAR+TURBULENT )
C          VISCAV      ; SPATIALLY AVERAGED EDDY VISCOSITY
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
C
C COMPUTE ELEMENT SUBGRID-SCALE EDDY VISCOSITY
C
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
          VISC(IE) =
     &        2.E0*(DUX(IE)*DUX(IE)+DVY(IE)*DVY(IE)+DWZ(IE)*DWZ(IE))
     &       +2.E0*(DWY(IE)*DVZ(IE)+DUZ(IE)*DWX(IE)+DVX(IE)*DUY(IE))
     &             +DWY(IE)*DWY(IE)+DVZ(IE)*DVZ(IE)+DUZ(IE)*DUZ(IE)
     &             +DWX(IE)*DWX(IE)+DVX(IE)*DVX(IE)+DUY(IE)*DUY(IE)
  300 CONTINUE
C
      DO 400 IE = 1 , NE
          IF(VISC(IE) .LT. 0.E0) VISC(IE) = 0.E0
          VISC(IE) = CS(IE)*CS(IE)*FILTER(IE)*FILTER(IE)*SQRT(VISC(IE))
  400 CONTINUE
C
C
C
C DAMP NEAR-WALL SUBGRID-SCALE EDDY VISCOSITY
C
C
C
      IF(NEWALL.GE.1) THEN
          DO 500 IE = 1 , NE
              YP = UTAU(NEAR(IE))*DSNEAR(IE)/VISCM
              VISC(IE) = VISC(IE)*(1.E0-EXP(-YP/AP))**2
  500     CONTINUE
      ENDIF
C
C
      IF(IPRDWL.EQ.0) THEN
          DO 600 IEWALL = 1 , NEWALL
              VISC(LEWALL(1,IEWALL)) = 0.E0
  600     CONTINUE
      ENDIF
C
      IF(IPRDWL.EQ.2) THEN
          DO 700 IEWALL = 1 , NEWALL
              VISC(LEWALL(1,IEWALL)) = DAMPWL*VISC(LEWALL(1,IEWALL))
  700     CONTINUE
      ENDIF
C
C
C
C UPPER-BOUND SUBGRID-SCALE EDDY VISCOSITY SUCH THAT THE
C DIFFUSION NUMBER CRITERIA BE MET
C
C
C
      IF(UPPER.GT.0.E0) THEN
          DO 800 IE = 1 , NE
              VISC(IE) = AMIN1(VISC(IE),
     &                         0.5E0*UPPER*FILTER(IE)*FILTER(IE)/DT)
  800     CONTINUE
      ENDIF
C
C
C
C CALCULATE EFFECTIVE (TOTAL) ELEMENT VISCOSITY
C
C
C
      VISCAV = 0.E0
      DO 900 IE = 1 , NE
          VISCAV   = VISCAV+VISC(IE)/NE
          VISC(IE) = VISCM +VISC(IE)
  900 CONTINUE
C
C
      RETURN
      END
