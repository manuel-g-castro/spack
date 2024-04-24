C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    RELAX2                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE RELAX2(DT,ALF,EPS,NMAX,U,V,P,DNXI,DNYI,DELTA,
     *                  DNX,DNY,CM,LBU,LBV,NBU,NBV,
     *                  NODE,IENP,JENP,NEP,MAXE,MAXEP,NE,NP,N,
     *                  IRN,NRN,DIVMAX,DIVAR,
     *                  DIV,DP,FX,FY,IUT0,IERR)
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*4   DNX,DNY,CM
      DIMENSION U(NP),V(NP),P(NE),DNXI(N,NE),DNYI(N,NE),DELTA(NE),
     1          DNX(N,NE),DNY(N,NE),CM(NP),LBU(NBU),LBV(NBV),
     2          NODE(N,NE),IENP(MAXEP,NP),JENP(MAXEP,NP),NEP(NP),
     3          DIV(NE),DP(NE),FX(NP),FY(NP)
C
      CHARACTER*72 ERMSG
     & /' FATAL ERROR REPORTED !! RELAX2 TERMINATES ALL THE PROCESS '/
C
      IDIM  = 2
      IMODE = 1
C
C
C      CORRECT PRESSURE FIELD AND VELOCITY FIELD
C     BY A SIMULTANEOUS PENALTY RELAXATION METHOD
C         ( 2-D CALCULATION )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          DT          ; TIME INCREMENT
C          ALF         ; RELAXATION COEFFICIENT
C          EPS         ; CONVERGENCE JUDGING VALUE
C          NMAX        ; MAXIMUN ITERATION NUMBER
C          DNXI  (I,IE); ELEMENT CENTER VALUE OF NX
C          DNYI  (I,IE); ELEMENT CENTER VALUE OF NY
C          DELTA   (IE); ELEMENT CHARACTERISTIC DIMENSION
C
C          DNX   (I,IE); INTEGRATED ELEMENT VECTOR OF NX
C          DNY   (I,IE); INTEGRATED ELEMENT VECTOR OF NY
C          CM      (IP); LUMPED    MASS MATRIX
C          LBU     (IB); U-PRESCRIBED NODE SPECIFYING LIST VECTOR
C          LBV     (IB); V-PRESCRIBED NODE SPECIFYING LIST VECTOR
C          NBU         ; NUMBER OF U-PRESCRIBED NODES
C          NBV         ; NUMBER OF V-PRESCRIBED NODES
C
C          NODE  (I,IE); NODE TABLE
C          IENP(IEP,IP); ADJACENT ELEMENT NUMBER TO NODE IP
C                      ( IF NEP(IP).LT.MAXEP , THEN IENP(NEP(IP)+1,IP),
C                       IENP(MAXEP,IP) MUST BE SET TO AN IMAGINARY
C                       ELEMENT NO. BETWEEN NE+1,MAXE.)
C          JENP(IEP,IP); LOCAL NODE NUMBER OF IP IN ELEMENT IENP(IEP,IP)
C          NEP     (IP); NUMBER OF ADJACENT ELEMENTS TO NODE IP
C          MAXE        ; THE MAXIMUM NUMBER  OF ELEMETS
C          MAXEP       ; THE FIRST DIMENSION OF ARRAY IENP,JENP
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          NP          ; NUMBER OF TOTAL    NODES
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT
C          IUT0        ; DEVICE NO. TO REPORT ERROR OCCURENCE
C
C       (2) OUTPUT
C          IRN         ; RETURN CODE TO REPORT CONVERGENCE
C                   0 --- NOT CONVERGED
C                   1 ---     CONVERGED
C          NRN         ; CALCULATION ITERATED NUMBER
C          DIVMAX      ; MAXIMUN ABSOLUTE DIVERGENT
C          DIVAR       ; SPATIAL AVERAGED ABSOLUTE DIVERGENT
C          IERR        ; RETURN CODE TO REPORT ERROR OCCURENCE
C                   0 --- NORMAL TERMINATION
C                   1 --- A FATAL ERROR HAS OCCURED
C
C
C       (3) INPUT-OUTPUT
C          U       (IP); X-DIR. VELOCITY COMPONENT
C          V       (IP); Y-DIR. VELOCITY COMPONENT
C          P       (IE); ELEMENT PRESSURE
C
C       (4) WORK
C          DIV     (IE); ELEMENT DIVERGENT
C          DP      (IE); PRESSURE CORRECTOR
C          FX      (IE); X-DIR. RESIDUAL FORCE VECTOR
C          FY      (IE); Y-DIR. RESIDUAL FORCE VECTOR
C
C
C
      IRN = 0 
      NRN = 0
C
   10 CONTINUE
      NRN = NRN+1
C
C      CALCULATION ELEMENT DIVERGENT
C
      CALL FIELD2(IMODE,U,V,DNXI,DNYI,NODE,NE,N,DIV)
C
      DIVMAX = 0.D0 
      DIVAR = 0.D0
      DO 100 IE = 1 , NE
          ABSDIV = DABS(DIV(IE))
          DIVAR  = DIVAR+ABSDIV
          DIVMAX = DMAX1(ABSDIV,DIVMAX)
  100 CONTINUE
      DIVAR = DIVAR/NE
      IF(NRN.GT.NMAX) GO TO 1000
      IF(DIVMAX.LE.EPS) THEN 
          IRN = 1 
          GO TO 1000 
          ENDIF
C
C      PRESSURE CORRECTION
C
CT    COEFF = -ALF/(3.D0*DT)
      COEFF = -ALF/      DT
      DO 200 IE = 1 , NE
          DP(IE) = COEFF*DIV(IE)*DELTA(IE)*DELTA(IE)
  200 CONTINUE
C
      DO 300 IE = 1 , NE
          P(IE) = P(IE)+DP(IE)
  300 CONTINUE
C
C      VELOCITY CORRECTION
C
      CALL SUPER1(IDIM,DP,DNX,IENP,JENP,NEP,MAXE,MAXEP,NE,NP,N,
     *            FX,IUT0,IERR)
      CALL SUPER1(IDIM,DP,DNY,IENP,JENP,NEP,MAXE,MAXEP,NE,NP,N,
     *            FY,IUT0,IERR)
C
      IF(IERR.EQ.1) THEN
          WRITE(IUT0,6300) ERMSG 
          RETURN
      ENDIF
C
      DO 400 IB = 1 , NBU
          FX(LBU(IB)) = 0.D0
  400 CONTINUE
C
      DO 500 IB = 1 , NBV
          FY(LBV(IB)) = 0.D0
  500 CONTINUE
C
CT    COEFF = DT/ALF
      COEFF = DT
      DO 600 IP = 1 , NP
          U(IP) = U(IP)+COEFF*CM(IP)*FX(IP)
          V(IP) = V(IP)+COEFF*CM(IP)*FY(IP)
  600 CONTINUE
C
C      ITERATION
C
      GO TO 10
C
C
 1000 CONTINUE
      NRN = NRN-1
C
C
      RETURN
 6300 FORMAT(A72)
      END
