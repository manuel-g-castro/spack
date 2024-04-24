C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    SCAL2D                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE SCAL2D(IWEI,IMASS,DT,VKAP,T,Q,AU,AV,AT,SX,SY,
     *                  SN,DNX,DNY,EX,EY,EXX,EYY,EXY,
     *                  IENP,JENP,NEP,IPNP,NPP,
     *                  MAXE,MAXP,MAXEP,MAXPP,NE,NP,N,
     *                  AC,CM,LBT,NBT,
     *                  RT,FT,DF,S,IUT0,IERR)
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*4   SN,DNX,DNY,EX,EY,EXX,EYY,EXY,AC,CM
      DIMENSION VKAP(NE),T(NP),Q(NE),AU(N,NE),AV(N,NE),AT(N,NE),
     1          SX(NE),SY(NE),SN(N,NE),DNX(N,NE),DNY(N,NE),
     2          EX (N,N,NE),EY(N,N,NE),
     3          EXX(N,N,NE),EYY(N,N,NE),EXY(N,N,NE),
     4          IENP(MAXEP,NP),JENP(MAXEP,NP),NEP(NP),
     5          IPNP(MAXPP,NP),NPP(NP),
     6          AC  (MAXPP,NP),CM (NP),LBT(NBT),
     7          RT(N,NE),FT(NP),DF(NP),S(NP)
C
      CHARACTER*72 ERMSG
     & /' FATAL ERROR REPORTED !! SCAL2D TERMINATES ALL THE PROCESS '/
C
      IDIM = 2
C
C
C      INTEGRATE AN ADVECTION-DIFFUTION EQUATION FOR A PASSIVE SCALAR
C         ( 2-D CALCULATION )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          IWEI        ; SPECIFIES THE TYPE OF WEIGHTING FUNCTION
C                       FOR THE ADVECTION AND SOURCE TERMS AS FOLLOWS
C                   1 --- GALERKIN             TYPE WEIGHTING FUNCTION
C                   2 --- STREAMLINE UPWINDING TYPE WEIGHTING FUNCTION
C          IMASS       ; SPECIFIES MASS MATRIX TREATMENT AS FOLLOWS
C                   1 --- LUMPED MASS REPRESENTATION
C                   2,3,- MULTI-PASS ALGORITHEM WILL BE TAKEN
C          DT          ; TIME INCTREMENT
C          VKAP    (IE); ELEMENT EFFECTIVE DIFFUSIVITY
C          Q       (IE); ELEMENT SOURCE
C          AU    (I,IE); ELEMENT VECTOR OF U(IP)
C          AV    (I,IE); ELEMENT VECTOR OF V(IP)
C          AT    (I,IE); ELEMENT VECTOR OF T(IP)
C          SX      (IE); STREAMLINE UPWINDING SHIFT VECTOR ( X-DIR. )
C          SY      (IE); STREAMLINE UPWINDING SHIFT VECTOR ( Y-DIR. )
C
C          SN    (I,IE); INTEGRATED ELEMENT VECTOR OF N
C          DNX   (I,IE); INTEGRATED ELEMENT VECTOR OF NX
C          DNY   (I,IE); INTEGRATED ELEMENT VECTOR OF NY
C          EX  (J,I,IE); INTEGRATED ELEMENT MATRIX OF N*NXT
C          EY  (J,I,IE); INTEGRATED ELEMENT MATRIX OF N*NYT
C          EXX (J,I,IE); INTEGRATED ELEMENT MATRIX OF NX*NXT
C          EYY (J,I,IE); INTEGRATED ELEMENT MATRIX OF NY*NYT
C          EXY (J,I,IE); INTEGRATED ELEMENT MATRIX OF NX*NYT
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
C          LBT     (IB); T-PRESCRIBED NODE SPECIFYING LIST VECTOR
C          NBT         ; NUMBER OF T-PRESCRIBED NODES
C          IUT0        ; DEVICE NO. TO REPORT ERROR OCCURENCE
C
C       (2) OUTPUT
C          IERR        ; RETURN CODE TO REPORT ERROR OCCURENCE
C                   0 --- NORMAL TERMINATION
C                   1 --- A FATAL ERROR HAS OCCURED
C
C       (3) INPUT-OUTPUT
C          T   (IP)   ; PASSIVE SCALAR INTEGRATED
C
C       (4) WORK
C          RT  (I,IE) ; ELEMENT ENERGY RESIDUAL
C          FT  (IP)   ; GLOBAL  ENERGY RESIDUAL
C          DF  (IP)   ; WORKING  VECTOR OF MULTI-PASS ALGORITHM
C          S   (IP)   ; SOLUTION VECTOR OF MULTI-PASS ALGORITHM
C
C
C      ADVECTION TERM CALCULATION
C
      IF(IWEI.EQ.1) THEN
      DO 110 I = 1 , N
          DO 100 IE = 1 , NE
             AX1 = EX(1,I,IE) 
             AY1 = EY(1,I,IE)
             AX2 = EX(2,I,IE) 
             AY2 = EY(2,I,IE)
             AX3 = EX(3,I,IE) 
             AY3 = EY(3,I,IE)
             AX4 = EX(4,I,IE) 
             AY4 = EY(4,I,IE)
             RT(I,IE) = -AX1*AU(1,IE)*AT(1,IE)-AY1*AV(1,IE)*AT(1,IE)
     &                  -AX2*AU(2,IE)*AT(2,IE)-AY2*AV(2,IE)*AT(2,IE)
     &                  -AX3*AU(3,IE)*AT(3,IE)-AY3*AV(3,IE)*AT(3,IE)
     &                  -AX4*AU(4,IE)*AT(4,IE)-AY4*AV(4,IE)*AT(4,IE)
  100     CONTINUE
  110 CONTINUE
      ELSE
C
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
             RT(I,IE) = -AX1*AU(1,IE)*AT(1,IE)-AY1*AV(1,IE)*AT(1,IE)
     &                  -AX2*AU(2,IE)*AT(2,IE)-AY2*AV(2,IE)*AT(2,IE)
     &                  -AX3*AU(3,IE)*AT(3,IE)-AY3*AV(3,IE)*AT(3,IE)
     &                  -AX4*AU(4,IE)*AT(4,IE)-AY4*AV(4,IE)*AT(4,IE)
  200     CONTINUE
  210 CONTINUE
      ENDIF
C
C      DIFFUSION TERM CALCULATION
C
      DO 310 I = 1 , N
          DO 300 IE = 1 , NE
              RT(I,IE) = RT(I,IE)
     &             -VKAP(IE)*((EXX(1,I,IE)+EYY(1,I,IE))*AT(1,IE)
     &                       +(EXX(2,I,IE)+EYY(2,I,IE))*AT(2,IE)
     &                       +(EXX(3,I,IE)+EYY(3,I,IE))*AT(3,IE)
     &                       +(EXX(4,I,IE)+EYY(4,I,IE))*AT(4,IE))
  300     CONTINUE
  310 CONTINUE
C
C      SOURCE TERM CALCULATION
C
      IF(IWEI.EQ.1) THEN
      DO 400 IE = 1 , NE
          RT(1,IE) = RT(1,IE)+Q(IE)*SN(1,IE)
          RT(2,IE) = RT(2,IE)+Q(IE)*SN(2,IE)
          RT(3,IE) = RT(3,IE)+Q(IE)*SN(3,IE)
          RT(4,IE) = RT(4,IE)+Q(IE)*SN(4,IE)
  400 CONTINUE
      ELSE
      DO 500 IE = 1 , NE
          W1 = SN(1,IE)+SX(IE)*DNX(1,IE)+SY(IE)*DNY(1,IE)
          W2 = SN(2,IE)+SX(IE)*DNX(2,IE)+SY(IE)*DNY(2,IE)
          W3 = SN(3,IE)+SX(IE)*DNX(3,IE)+SY(IE)*DNY(3,IE)
          W4 = SN(4,IE)+SX(IE)*DNX(4,IE)+SY(IE)*DNY(4,IE)
          RT(1,IE) = RT(1,IE)+W1*Q(IE)
          RT(2,IE) = RT(2,IE)+W2*Q(IE)
          RT(3,IE) = RT(3,IE)+W3*Q(IE)
          RT(4,IE) = RT(4,IE)+W4*Q(IE)
  500 CONTINUE
      ENDIF
C
C      SUPERPOSITION
C
      CALL SUPER2(IDIM,RT,IENP,JENP,NEP,MAXE,MAXEP,NE,NP,N,
     *            FT,IUT0,IERR)
C
      IF(IERR.EQ.1) THEN
          WRITE(IUT0,6300) ERMSG 
          RETURN
      ENDIF
C
C      MATRIX SOLUTION
C
      CALL SOLVEF(IDIM,IMASS,AC,CM,IPNP,NPP,MAXP,MAXPP,NP,
     *            FT,LBT,NBT,S,DF,IUT0,IERR)
C
      IF(IERR.EQ.1) THEN
          WRITE(IUT0,6300) ERMSG 
          RETURN
      ENDIF
C
      DO 600 IP = 1 , NP
          T(IP) = T(IP)+DT*S(IP)
  600 CONTINUE
C
C
      RETURN
 6300 FORMAT(A72)
      END
