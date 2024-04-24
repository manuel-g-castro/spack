C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    PRED2D                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE PRED2D(IWEI,IMASS,DT,VISC,U,V,P,AU,AV,SX,SY,
     *                  SN,DNX,DNY,EX,EY,EXX,EYY,EXY,ACCELX,ACCELY,
     *                  IENP,JENP,NEP,IPNP,NPP,
     *                  MAXE,MAXP,MAXEP,MAXPP,NE,NP,N,
     *                  AC,CM,LBU,LBV,NBU,NBV,
     *                  LEN,SNIN,DNXIN,DNYIN,FLUXN,NEN,
     *                  RX,RY,FX,FY,DF,S,IUT0,IERR)
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*4   SN,DNX,DNY,EX,EY,EXX,EYY,EXY,AC,CM
      DIMENSION VISC(NE),U(NP),V(NP),P(NE),AU(N,NE),AV(N,NE),
     1          SX(NE),SY(NE),SN(N,NE),DNX(N,NE),DNY(N,NE),
     2          EX (N,N,NE),EY (N,N,NE),
     3          EXX(N,N,NE),EYY(N,N,NE),EXY(N,N,NE),
     4          IENP(MAXEP,NP),JENP(MAXEP,NP),NEP(NP),
     5          IPNP(MAXPP,NP),NPP(NP),
     5          AC  (MAXPP,NP),CM(NP),LBU(NBU),LBV(NBV),
     6          LEN  (NEN),SNIN(N,NEN),DNXIN(N,NEN),DNYIN(N,NEN),
     7          FLUXN(NEN),RX(N,NE),RY(N,NE),FX(NP),FY(NP),DF(NP),S(NP)
C
      CHARACTER*72 ERMSG
     & /' FATAL ERROR REPORTED !! PRED2D TERMINATES ALL THE PROCESS '/
C
      IDIM = 2
C
C
C      CALCULATE VELOCITY PREDICTOR
C         ( 2-D CALCULATION )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          IWEI        ; SPECIFIES THE TYPE OF WEIGHTING FUNCTIONS
C                       FOR THE ADVECTION TERMS AS FOLLOWS
C                   1 --- GALERKIN             TYPE WEIGHTING FUNCTION
C                   2 --- STREAMLINE UPWINDING TYPE WEIGHTING FUNCTION
C          IMASS       ; SPECIFIES MASS MATRIX TREATMENT AS FOLLOWS
C                   1 --- LUMPED MASS REPRESENTATION
C                   2,3,- MULTI-PASS ALGORITHEM WILL BE TAKEN
C          DT          ; TIME INCTREMENT
C          VISC    (IE); ELEMENT VISCOSITY
C          P       (IE); ELEMENT PRESSURE
C          AU    (I,IE); ELEMENT VECTOR OF U(IP)
C          AV    (I,IE); ELEMENT VECTOR OF V(IP)
C          SX      (IE); STREAMLINE UPWINDING SHIFT VECTOR ( X-DIR. )
C          SY      (IE); STREAMLINE UPWINDING SHIFT VECTOR ( Y-DIR. )
C
C          SN    (I,IE); ELEMENT INTEGRATED VECTOR OF N
C          DNX   (I,IE); INTEGRATED ELEMENT VECTOR OF NX
C          DNY   (I,IE); INTEGRATED ELEMENT VECTOR OF NY
C          EX  (J,I,IE); INTEGRATED ELEMENT MATRIX OF N*NXT
C          EY  (J,I,IE); INTEGRATED ELEMENT MATRIX OF N*NYT
C          EXX (J,I,IE); INTEGRATED ELEMENT MATRIX OF NX*NXT
C          EYY (J,I,IE); INTEGRATED ELEMENT MATRIX OF NY*NYT
C          EXY (J,I,IE); INTEGRATED ELEMENT MATRIX OF NX*NYT
C
C          ACCELX      ; X-DIR. BODY FORCE ( UNIFORM )
C          ACCELY      ; Y-DIR. BODY FORCE ( UNIFORM )
C
C          IENP(IEP,IP); ADJACENT ELEMENT NUMBER TO NODE IP
C                      ( IF NEP(IP).LT.MAXEP , THEN IENP(NEP(IP)+1,IP),
C                       IENP(MAXEP,IP) MUST BE SET TO AN IMAGINARY
C                       ELEMENT NO. BETWEEN NE+1,MAXE.)
C          JENP(IEP,IP); LOCAL NODE NUMBER OF IP IN ELEMENT IENP(IEP,IP)
C          NEP     (IP); NUMBER OF ADJACENT ELEMENTS TO NODE IP
C          IPNP(IPP,IP); ADJACENT NODE    NUMBER TO NODE    IP
C                      ( IF NPP(IP).LT.MAXPP , THEN IPNP(NPP(IP)+1,IP),
C                       IPNP(MAXPP,IP) MUST BE SET TO AN IMAGINARY
C                       NODE    NO. BETWEEN NP+1,MAXP.)
C          NPP     (IP); NUMBER OF ADJACENT NODES    TO NODE    IP
C          MAXE        ; THE MAXIMUM NUMBER  OF ELEMETS
C          MAXP        ; THE MAXIMUM NUMBER  OF   NODES
C          MAXEP       ; THE FIRST DIMENSION OF ARRAY IENP,JENP
C          MAXPP       ; THE FIRST DIMENSION OF ARRAY IPNP
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          NP          ; NUMBER OF TOTAL    NODES
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT
C
C          AC  (IPP,IP); CONDENSED MASS MATRIX
C          CM      (IP); LUMPED    MASS MATRIX
C          LBU     (IB); U-PRESCRIBED NODE SPECIFYING LIST VECTOR
C          LBV     (IB); V-PRESCRIBED NODE SPECIFYING LIST VECTOR
C          NBU         ; NUMBER OF U-PRESCRIBED NODES
C          NBV         ; NUMBER OF V-PRESCRIBED NODES
C
C          LEN     (IB); ELEMENT NUMBERS FACING ON OUTFLOW BOUNDARIES
C          SNIN  (I,IB); SHAPE FUNCTION AT MID POINT OF BOUNDARY SIDES
C          DNXIN (I,IB); X-DERIVATIVE   AT MID POINT OF BOUNDARY SIDES
C          DNYIN (I,IB); Y-DERIVATIVE   AT MID POINT OF BOUNDARY SIDES
C          FLUXN   (IB); HALF VOLUME FLOW LEAVING OUTFLOW BOUNDARY SIDES
C                       IN ONE TIME INTEGRATION
C          NEN         ; NUMBER OF SIDES FACING ON OUTFLOW BOUNDARIES
C
C          IUT0        ; DEVICE NO. TO REPORT ERROR OCCURENCE
C
C       (2) OUTPUT
C          IERR        ; RETURN CODE TO REPORT ERROR OCCURENCE
C                   0 --- NORMAL TERMINATION
C                   1 --- A FATAL ERROR HAS OCCURED
C
C       (3) INPUT-OUTPUT
C          U   (IP)   ; X-DIR. VELOCITY COMPONENT
C          V   (IP)   ; Y-DIR. VELOCITY COMPONENT
C
C       (4) WORK
C          RX  (I,IE) ; ELEMENT MOMENTUM RESIDUAL ( X-DIR. )
C          RY  (I,IE) ; ELEMENT MOMENTUM RESIDUAL ( Y-DIR. )
C          FX  (IP)   ; X-DIR. GLOBAL MOMENTUM RESIDUAL
C          FY  (IP)   ; Y-DIR. GLOBAL MOMENTUM RESIDUAL
C          DF  (IP)   ; WORKING  VECTOR OF MULTI-PASS ALGORITHM
C          S   (IP)   ; SOLUTION VECTOR OF MULTI-PASS ALGORITHM
C
C
C      ADVECTION TERM CALCULATION ( GALERKIN             TYPE )
C
      IF(IWEI.EQ.1) THEN
      DO 110 I = 1 , N
          DO 100 IE = 1 , NE
             AX1 = EX(1,I,IE)
             AX2 = EX(2,I,IE)
             AX3 = EX(3,I,IE)
             AX4 = EX(4,I,IE)
C
             AY1 = EY(1,I,IE)
             AY2 = EY(2,I,IE)
             AY3 = EY(3,I,IE)
             AY4 = EY(4,I,IE)
C
             RX(I,IE) = -AX1*AU(1,IE)*AU(1,IE)-AY1*AV(1,IE)*AU(1,IE)
     &                  -AX2*AU(2,IE)*AU(2,IE)-AY2*AV(2,IE)*AU(2,IE)
     &                  -AX3*AU(3,IE)*AU(3,IE)-AY3*AV(3,IE)*AU(3,IE)
     &                  -AX4*AU(4,IE)*AU(4,IE)-AY4*AV(4,IE)*AU(4,IE)
C
             RY(I,IE) = -AX1*AU(1,IE)*AV(1,IE)-AY1*AV(1,IE)*AV(1,IE)
     &                  -AX2*AU(2,IE)*AV(2,IE)-AY2*AV(2,IE)*AV(2,IE)
     &                  -AX3*AU(3,IE)*AV(3,IE)-AY3*AV(3,IE)*AV(3,IE)
     &                  -AX4*AU(4,IE)*AV(4,IE)-AY4*AV(4,IE)*AV(4,IE)
  100     CONTINUE
  110 CONTINUE
      ENDIF
C
C      ADVECTION TERM CALCULATION ( STREAMLINE UPWINDING TYPE )
C
      IF(IWEI.EQ.2) THEN
      DO 210 I = 1 , N
          DO 200 IE = 1 , NE
             AX1 = EX(1,I,IE)+SX(IE)*EXX(1,I,IE)+SY(IE)*EXY(I,1,IE)
             AX2 = EX(2,I,IE)+SX(IE)*EXX(2,I,IE)+SY(IE)*EXY(I,2,IE)
             AX3 = EX(3,I,IE)+SX(IE)*EXX(3,I,IE)+SY(IE)*EXY(I,3,IE)
             AX4 = EX(4,I,IE)+SX(IE)*EXX(4,I,IE)+SY(IE)*EXY(I,4,IE)
C
             AY1 = EY(1,I,IE)+SX(IE)*EXY(1,I,IE)+SY(IE)*EYY(1,I,IE)
             AY2 = EY(2,I,IE)+SX(IE)*EXY(2,I,IE)+SY(IE)*EYY(2,I,IE)
             AY3 = EY(3,I,IE)+SX(IE)*EXY(3,I,IE)+SY(IE)*EYY(3,I,IE)
             AY4 = EY(4,I,IE)+SX(IE)*EXY(4,I,IE)+SY(IE)*EYY(4,I,IE)
C
             RX(I,IE) = -AX1*AU(1,IE)*AU(1,IE)-AY1*AV(1,IE)*AU(1,IE)
     &                  -AX2*AU(2,IE)*AU(2,IE)-AY2*AV(2,IE)*AU(2,IE)
     &                  -AX3*AU(3,IE)*AU(3,IE)-AY3*AV(3,IE)*AU(3,IE)
     &                  -AX4*AU(4,IE)*AU(4,IE)-AY4*AV(4,IE)*AU(4,IE)
C
             RY(I,IE) = -AX1*AU(1,IE)*AV(1,IE)-AY1*AV(1,IE)*AV(1,IE)
     &                  -AX2*AU(2,IE)*AV(2,IE)-AY2*AV(2,IE)*AV(2,IE)
     &                  -AX3*AU(3,IE)*AV(3,IE)-AY3*AV(3,IE)*AV(3,IE)
     &                  -AX4*AU(4,IE)*AV(4,IE)-AY4*AV(4,IE)*AV(4,IE)
  200     CONTINUE
  210 CONTINUE
      ENDIF
C
C      STRESS TERM CALCULATION
C
      DO 310 I = 1 , N
          DO 300 IE = 1 , NE
              RX(I,IE) = RX(I,IE)
     &             -VISC(IE)*(EXX(1,I,IE)*AU(1,IE)+EXX(1,I,IE)*AU(1,IE)
     &                       +EXX(2,I,IE)*AU(2,IE)+EXX(2,I,IE)*AU(2,IE)
     &                       +EXX(3,I,IE)*AU(3,IE)+EXX(3,I,IE)*AU(3,IE)
     &                       +EXX(4,I,IE)*AU(4,IE)+EXX(4,I,IE)*AU(4,IE)
     &                       +EYY(1,I,IE)*AU(1,IE)+EXY(I,1,IE)*AV(1,IE)
     &                       +EYY(2,I,IE)*AU(2,IE)+EXY(I,2,IE)*AV(2,IE)
     &                       +EYY(3,I,IE)*AU(3,IE)+EXY(I,3,IE)*AV(3,IE)
     &                       +EYY(4,I,IE)*AU(4,IE)+EXY(I,4,IE)*AV(4,IE))
C
              RY(I,IE) = RY(I,IE)
     &             -VISC(IE)*(EXX(1,I,IE)*AV(1,IE)+EXY(1,I,IE)*AU(1,IE)
     &                       +EXX(2,I,IE)*AV(2,IE)+EXY(2,I,IE)*AU(2,IE)
     &                       +EXX(3,I,IE)*AV(3,IE)+EXY(3,I,IE)*AU(3,IE)
     &                       +EXX(4,I,IE)*AV(4,IE)+EXY(4,I,IE)*AU(4,IE)
     &                       +EYY(1,I,IE)*AV(1,IE)+EYY(1,I,IE)*AV(1,IE)
     &                       +EYY(2,I,IE)*AV(2,IE)+EYY(2,I,IE)*AV(2,IE)
     &                       +EYY(3,I,IE)*AV(3,IE)+EYY(3,I,IE)*AV(3,IE)
     &                       +EYY(4,I,IE)*AV(4,IE)+EYY(4,I,IE)*AV(4,IE))
  300     CONTINUE
  310 CONTINUE
C
      DO 400 IE = 1 , NE
          RX(1,IE) = RX(1,IE)+P(IE)*DNX(1,IE)+ACCELX*SN(1,IE)
          RX(2,IE) = RX(2,IE)+P(IE)*DNX(2,IE)+ACCELX*SN(2,IE)
          RX(3,IE) = RX(3,IE)+P(IE)*DNX(3,IE)+ACCELX*SN(3,IE)
          RX(4,IE) = RX(4,IE)+P(IE)*DNX(4,IE)+ACCELX*SN(4,IE)
C
          RY(1,IE) = RY(1,IE)+P(IE)*DNY(1,IE)+ACCELY*SN(1,IE)
          RY(2,IE) = RY(2,IE)+P(IE)*DNY(2,IE)+ACCELY*SN(2,IE)
          RY(3,IE) = RY(3,IE)+P(IE)*DNY(3,IE)+ACCELY*SN(3,IE)
          RY(4,IE) = RY(4,IE)+P(IE)*DNY(4,IE)+ACCELY*SN(4,IE)
  400 CONTINUE
C
C      UPWINDING ON OUTFLOW BOUNDARIES
C
      DO 500 IB = 1 , NEN
         IE = LEN(IB)
         UPX=DNXIN(1,IB)*AU(1,IE)*AU(1,IE)+DNYIN(1,IB)*AU(1,IE)*AV(1,IE)
     &      +DNXIN(2,IB)*AU(2,IE)*AU(2,IE)+DNYIN(2,IB)*AU(2,IE)*AV(2,IE)
     &      +DNXIN(3,IB)*AU(3,IE)*AU(3,IE)+DNYIN(3,IB)*AU(3,IE)*AV(3,IE)
     &      +DNXIN(4,IB)*AU(4,IE)*AU(4,IE)+DNYIN(4,IB)*AU(4,IE)*AV(4,IE)
         RX(1,IE) = RX(1,IE)+FLUXN(IB)*UPX*SNIN(1,IB)
         RX(2,IE) = RX(2,IE)+FLUXN(IB)*UPX*SNIN(2,IB)
         RX(3,IE) = RX(3,IE)+FLUXN(IB)*UPX*SNIN(3,IB)
         RX(4,IE) = RX(4,IE)+FLUXN(IB)*UPX*SNIN(4,IB)
  500 CONTINUE
C
      DO 600 IB = 1 , NEN
         IE = LEN(IB)
         UPY=DNXIN(1,IB)*AU(1,IE)*AV(1,IE)+DNYIN(1,IB)*AV(1,IE)*AV(1,IE)
     &      +DNXIN(2,IB)*AU(2,IE)*AV(2,IE)+DNYIN(2,IB)*AV(2,IE)*AV(2,IE)
     &      +DNXIN(3,IB)*AU(3,IE)*AV(3,IE)+DNYIN(3,IB)*AV(3,IE)*AV(3,IE)
     &      +DNXIN(4,IB)*AU(4,IE)*AV(4,IE)+DNYIN(4,IB)*AV(4,IE)*AV(4,IE)
         RY(1,IE) = RY(1,IE)+FLUXN(IB)*UPY*SNIN(1,IB)
         RY(2,IE) = RY(2,IE)+FLUXN(IB)*UPY*SNIN(2,IB)
         RY(3,IE) = RY(3,IE)+FLUXN(IB)*UPY*SNIN(3,IB)
         RY(4,IE) = RY(4,IE)+FLUXN(IB)*UPY*SNIN(4,IB)
  600 CONTINUE
C
C      SUPERPOSITION
C
      CALL SUPER2(IDIM,RX,IENP,JENP,NEP,MAXE,MAXEP,NE,NP,N,
     *            FX,IUT0,IERR)
      CALL SUPER2(IDIM,RY,IENP,JENP,NEP,MAXE,MAXEP,NE,NP,N,
     *            FY,IUT0,IERR)
C
      IF(IERR.EQ.1) THEN
          WRITE(IUT0,6300) ERMSG 
          RETURN
      ENDIF
C
C      SOLVE EQUATIONS
C
      CALL SOLVEF(IDIM,IMASS,AC,CM,IPNP,NPP,MAXP,MAXPP,NP,
     *            FX,LBU,NBU,S,DF,IUT0,IERR)
C
      IF(IERR.EQ.1) THEN
          WRITE(IUT0,6300) ERMSG 
          RETURN
      ENDIF
C
      DO 700 IP = 1 , NP
          U(IP) = U(IP)+DT*S(IP)
  700 CONTINUE
C
      CALL SOLVEF(IDIM,IMASS,AC,CM,IPNP,NPP,MAXP,MAXPP,NP,
     *            FY,LBV,NBV,S,DF,IUT0,IERR)
C
      IF(IERR.EQ.1) THEN
          WRITE(IUT0,6300) ERMSG 
          RETURN
      ENDIF
C
      DO 800 IP = 1 , NP
          V(IP) = V(IP)+DT*S(IP)
  800 CONTINUE
C
C
      RETURN
 6300 FORMAT(A72)
      END
