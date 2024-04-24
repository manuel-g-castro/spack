C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.8.1                                   C
C                                                                      C
C  SUB ROUTINE : EDY3DX                                                C
C                                                                      C
C                                       WRITTEN BY Y.YAMADE            C
C                                                                      C
C                                                                      C
C CONTACT ADDRESS : IIS, THE UNIVERSITY OF TOKYO, CISS                 C
C                                                                      C
C THERMO-FLUID ANALYSIS SOLVERS FOR LARGE-SCALE-ASSEMBLY               C
C                                                                      C
C======================================================================C
      SUBROUTINE EDY3DX(CS,FILTER,U,V,W,NODE,ME,NE,NP,N1,N2,NEX,
     *                  IPRDWL,DAMPWL,LEWALL,NEWALL,NEAR,DSNEAR,UTAU,AP,
     *                  VISC,VISCAV,UPPER,DT,
     *                  DNXI,DNYI,DNZI,VISC2,
     *                  DUX,DUY,DUZ,DVX,DVY,DVZ,DWX,DWY,DWZ,
     *                  IVOF,NEFLD2,LEFLD2,LEFIX)
      IMPLICIT NONE
C
      INTEGER*4 NODE,ME,NE,NP,N1,N2,NEX,
     *          IPRDWL,LEWALL,NEWALL,NEAR
C
      REAL*4    CS,FILTER,U,V,W,DAMPWL,DSNEAR,UTAU,AP,
     *          VISC,VISCAV,UPPER,DT,DNXI,DNYI,DNZI,VISC2,
     *          DUX,DUY,DUZ,DVX,DVY,DVZ,DWX,DWY,DWZ
C
      INTEGER*4 NETET,NEPRD,NEWED,NEHEX,NTET,NPRD,NWED,NHEX,
     *          IES1,IEE1,IES2,IEE2,IES3,IEE3,IES4,IEE4,IE,I,
     *          IEWALL,IBP,NUM
C
      REAL*4    YP
C
      DIMENSION CS(NE),FILTER(NE),U(NP),V(NP),W(NP),LEWALL(2,NEWALL),
     *          NEAR(NE),DSNEAR(NE),UTAU(NEWALL),VISC(NE),
     *          NODE(N2,NE),NEX(8),
     *          DNXI(N1,ME),DNYI(N1,ME),DNZI(N1,ME),
     *          VISC2(NE),DUX(NE),DUY(NE),DUZ(NE),DVX(NE),
     *                   DVY(NE),DVZ(NE),DWX(NE),DWY(NE),DWZ(NE)
C     [INPUT:VOF]
      INTEGER*4 IVOF,NEFLD2
      INTEGER*4 LEFLD2(NEFLD2),LEFIX(NE)
C
C
C      CALCULATE SUBGRID-SCALE EDDY VISCOSITY BY SMAGORINSKY MODEL
C     WITH VAN-DRIEST DAMPING FUNCTION
C         ( 3-D CALCULATION : SINGLE WORD & MULTI ELEMENT VERSION )
C                                           CODED BASED ON 'EDDY3D'
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
C          VISC2   (IE); TURBULENT VISCOSITY
C
C
      NETET=NEX(1)
      NEPRD=NEX(2)
      NEWED=NEX(3)
      NEHEX=NEX(4)
      NE   =NETET+NEPRD+NEWED+NEHEX
C
      NTET =NEX(5)
      NPRD =NEX(6)
      NWED =NEX(7)
      NHEX =NEX(8)
C
C     * TET *
      IES1=1
      IEE1=NETET
C
C     * PYRAMID *
      IES2=NETET+1
      IEE2=NETET+NEPRD
C
C     * WEDGE *
      IES3=NETET+NEPRD+1
      IEE3=NETET+NEPRD+NEWED
C
C     * HEX *
      IES4=NETET+NEPRD+NEWED+1
      IEE4=NETET+NEPRD+NEWED+NEHEX
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
C     * TET *
      DO 200 IE = IES1,IEE1
          DO 210 I = 1 , NTET
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
  210     CONTINUE
  200 CONTINUE
C
C     * PYRAMID *
      DO 220 IE = IES2,IEE2
          DO 230 I = 1 , NPRD
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
  230     CONTINUE
  220 CONTINUE
C
C     * WEDGE *
      DO 240 IE = IES3,IEE3
          DO 250 I = 1 , NWED
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
  250     CONTINUE
  240 CONTINUE
C
C     * HEX *
      DO 260 IE = IES4,IEE4
          DO 270 I = 1 , NHEX
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
  270     CONTINUE
  260 CONTINUE
C
      DO 300 IE = 1 , NE
          VISC2(IE) =
     &        2.E0*(DUX(IE)*DUX(IE)+DVY(IE)*DVY(IE)+DWZ(IE)*DWZ(IE))
     &       +2.E0*(DWY(IE)*DVZ(IE)+DUZ(IE)*DWX(IE)+DVX(IE)*DUY(IE))
     &             +DWY(IE)*DWY(IE)+DVZ(IE)*DVZ(IE)+DUZ(IE)*DUZ(IE)
     &             +DWX(IE)*DWX(IE)+DVX(IE)*DVX(IE)+DUY(IE)*DUY(IE)
  300 CONTINUE
C
      DO 400 IE = 1 , NE
          IF(VISC2(IE) .LT. 0.E0) VISC2(IE) = 0.E0
          VISC2(IE)=CS(IE)*CS(IE)*FILTER(IE)*FILTER(IE)*SQRT(VISC2(IE))
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
              YP = UTAU(NEAR(IE))*DSNEAR(IE)/VISC(IE)
              VISC2(IE) = VISC2(IE)*(1.E0-EXP(-YP/AP))**2
  500     CONTINUE
      ENDIF
C
C
      IF(IPRDWL.EQ.0) THEN
          DO 600 IEWALL = 1 , NEWALL
              VISC2(LEWALL(1,IEWALL)) = 0.E0
  600     CONTINUE
      ENDIF
C
      IF(IPRDWL.EQ.2) THEN
          DO 700 IEWALL = 1 , NEWALL
              VISC2(LEWALL(1,IEWALL)) = DAMPWL*VISC2(LEWALL(1,IEWALL))
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
              VISC2(IE) = AMIN1(VISC2(IE),
     &                          0.5E0*UPPER*FILTER(IE)*FILTER(IE)/DT)
  800     CONTINUE
      ENDIF
C
C
C
C CALCULATE EFFECTIVE (TOTAL) ELEMENT VISCOSITY
C
C
C
      DO 900 IE=1,NE
         LEFIX(IE)=0
 900  CONTINUE
C
      IF (IVOF.EQ.1) THEN
         DO 950 IBP=1,NEFLD2
            LEFIX(LEFLD2(IBP))=1
 950     CONTINUE
      ENDIF
C
      VISCAV = 0.E0
      NUM=0
      DO 1000 IE = 1 , NE
          IF (LEFIX(IE).EQ.1) GOTO 1000
          NUM=NUM+1
          VISCAV   = VISCAV  +VISC2(IE)
          VISC(IE) = VISC(IE)+VISC2(IE)
 1000 CONTINUE
      VISCAV=VISCAV/FLOAT(NUM)
C
C
      RETURN
      END
