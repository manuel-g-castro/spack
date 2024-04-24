C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    FIND31                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE FIND31(IMODE,ELM,NE,XP,YP,ZP,NITER,
     *                  XMINE,YMINE,ZMINE,XMAXE,YMAXE,ZMAXE,
     *                  IFOUND,GP,EP,TP,JCHECK,DELTA,ERR,G,E,T,IWRK)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION ELM(24,NE),DELTA(NE),G(NE),E(NE),T(NE),IWRK(NE),
     1          XMINE(NE),YMINE(NE),ZMINE(NE),
     2          XMAXE(NE),YMAXE(NE),ZMAXE(NE)
C
      DATA EPS    / 1.E-3  /
      DATA FINITE / 1.E-20 /
C
C
C      FIND AN ELEMENT INCLUDING A SPECIFIED POINT. RETURN THE
C     ELEMENT NUMBER AND LOCAL COORDINATES OF THE POINT. IF NO ELEMENT
C     IS FOUND, ELEMENT NUMBER OF ZERO WILL BE RETURNED.
C         ( 3-D CALCULATION )
C
C
C     NOTE ; 1. INCLUSION IN AN ELEMENT OF A POINT WILL BE JUDGED
C              BASED ON ITS LOCAL GZAI, EATA, AND THETA COORDINATES,
C              CALCULATED BY THE NEWTON LAPSON METHODS.
C
C     NOTE ; 2. TOTAL OF 'NITER' ITERATIONS WILL BE DONE WITH THE
C              NEWTON LAPSON METHOD, REGARDLESS TO ITS CONVERGENCE.
C              BUT, TWO OR THREE ITERATIONS ARE, IN GENERAL, FOUND
C              ENOUGH TO OBTAIN THE LOCAL COORDINATES WITH REASONABLE
C              ACCURACY UNLESS THE ELEMENT IS STRONGLY SKEWED.
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          IMODE       ; PASS ARGUMENTS 'XMINE'-'ZMAXE' WITH THIS FLAG
C                       BEING SET TO ONE, TO ACTIVATE FAST ELEMENT
C                       SEARCH MODE
C
C           NOTES; 'FIND31' RESTRICTS THOSE ELEMENTS TO BE SEARCHED
C                 BASED ON THE PASSED ELEMENT'S MINIMUM & MAXIMUM
C                 COORDINATES IF 'IMODE' FLAG IS BEING SET TO ONE.
C                 NOTE THAT VECTOR OPERATIONS WILL BE SUBSTANTIALLY
C                 SUPPRESSED IF 'IMODE' FLAG IS BEING SET TO ONE,
C                 BECAUSE THE NUMBER OF ELEMENTS TO BE SEARCHED WILL BE
C                 SMALL DUE TO THE RESTRICTION.
C
C          ELM( 1,IE)  ; 0.125*SUM OF X(NODE(I,IE))
C          ELM( 2,IE)  ; 0.125*SUM OF Y(NODE(I,IE))
C          ELM( 3,IE)  ; 0.125*SUM OF Z(NODE(I,IE))
C
C          ELM( 4,IE)  ; 0.125*SUM OF X(NODE(I,IE))*GI(I)
C          ELM( 5,IE)  ; 0.125*SUM OF Y(NODE(I,IE))*GI(I)
C          ELM( 6,IE)  ; 0.125*SUM OF Z(NODE(I,IE))*GI(I)
C          ELM( 7,IE)  ; 0.125*SUM OF X(NODE(I,IE))*EI(I)
C          ELM( 8,IE)  ; 0.125*SUM OF Y(NODE(I,IE))*EI(I)
C          ELM( 9,IE)  ; 0.125*SUM OF Z(NODE(I,IE))*EI(I)
C          ELM(10,IE)  ; 0.125*SUM OF X(NODE(I,IE))*TI(I)
C          ELM(11,IE)  ; 0.125*SUM OF Y(NODE(I,IE))*TI(I)
C          ELM(12,IE)  ; 0.125*SUM OF Z(NODE(I,IE))*TI(I)
C
C          ELM(13,IE)  ; 0.125*SUM OF X(NODE(I,IE))*GI(I)*EI(I)
C          ELM(14,IE)  ; 0.125*SUM OF Y(NODE(I,IE))*GI(I)*EI(I)
C          ELM(15,IE)  ; 0.125*SUM OF Z(NODE(I,IE))*GI(I)*EI(*)
C          ELM(16,IE)  ; 0.125*SUM OF X(NODE(I,IE))*EI(I)*TI(I)
C          ELM(17,IE)  ; 0.125*SUM OF Y(NODE(I,IE))*EI(I)*TI(I)
C          ELM(18,IE)  ; 0.125*SUM OF Z(NODE(I,IE))*EI(I)*TI(*)
C          ELM(19,IE)  ; 0.125*SUM OF X(NODE(I,IE))*TI(I)*GI(I)
C          ELM(20,IE)  ; 0.125*SUM OF Y(NODE(I,IE))*TI(I)*GI(I)
C          ELM(21,IE)  ; 0.125*SUM OF Z(NODE(I,IE))*TI(I)*GI(*)
C
C          ELM(22,IE)  ; 0.125*SUM OF X(NODE(I,IE))*GI(I)*EI(I)*TI(I)
C          ELM(23,IE)  ; 0.125*SUM OF Y(NODE(I,IE))*GI(I)*EI(I)*TI(I)
C          ELM(24,IE)  ; 0.125*SUM OF Z(NODE(I,IE))*GI(I)*EI(I)*TI(I)
C
C          NE          ; NUMBER OF TOTAL NODES
C          XP          ; X-DIR. COORDINATE OF THE POINT
C          YP          ; Y-DIR. COORDINATE OF THE POINT
C          ZP          ; Z-DIR. COORDINATE OF THE POINT
C          NITER       ; NUMBER OF ITERATIVE CALCULATI NS TO BE DONE
C
C           NOTES; THE FOLLOWING MIN & MAX VALUES WILL BE USED TO
C                 ACTIVATE FAST ELEMENT SEARCH MODE AND ARE NEEDED ONLY
C                 WHEN IMODE = 1 IS SPECIFIED.
C          XMINE   (IE); MINIMUM X-DIR. COORDINATE OF ELEMENT
C          YMINE   (IE); MINIMUM Y-DIR. COORDINATE OF ELEMENT
C          ZMINE   (IE); MINIMUM Z-DIR. COORDINATE OF ELEMENT
C          XMAXE   (IE); MAXIMUM X-DIR. COORDINATE OF ELEMENT
C          YMAXE   (IE); MAXIMUM Y-DIR. COORDINATE OF ELEMENT
C          ZMAXE   (IE); MAXIMUM Z-DIR. COORDINATE OF ELEMENT
C
C          JCHECK      ; RESIDUAL ERROR CHECK WILL BE DONE IF THIS IS
C                        SET TO ONE
C          DELTA (IE)  ; CHARACATERISTIC ELEMENT DIMENSION USED FOR
C                        THE RESIDUAL ERROR CHECK
C                        DUMMY ARGUMENT FOR JCHECK = 0
C
C       (2) OUTPUT
C          IFOUND      ; ELEMENT NUMBER INCLUDING THE POINT
C          GP          ; GZAI  COORDINATE OF THE POINT FOR ELEMENT FOUND
C          EP          ; EATA  COORDINATE OF THE POINT FOR ELEMENT FOUND
C          TP          ; THETA COORDINATE OF THE POINT FOR ELEMENT FOUND
C          ERR         ; RELATIVE RESIDUAL ERROR IN GP,EP,ZP CALCULATION
C
C       (4) WORK
C          G     (IE)  ; PREPARE FOR ALL ELEMENTS
C          E     (IE)  ; PREPARE FOR ALL ELEMENTS
C          T     (IE)  ; PREPARE FOR ALL ELEMENTS
C          IWRK  (IE)  ; PREPARE FOR ALL ELEMENTS
C
C
      IF(IMODE.EQ.0) THEN
          DO 50 IE = 1 , NE
              IWRK(IE) = IE
  50      CONTINUE
          NES = NE
      ELSE
          NES = 0
          DO 60 IE = 1 , NE
              IF(XMINE(IE).GT.XP) GO TO 60
              IF(YMINE(IE).GT.YP) GO TO 60
              IF(ZMINE(IE).GT.ZP) GO TO 60
              IF(XMAXE(IE).LT.XP) GO TO 60
              IF(YMAXE(IE).LT.YP) GO TO 60
              IF(ZMAXE(IE).LT.ZP) GO TO 60
              NES = NES+1
              IWRK(NES) = IE
  60      CONTINUE
      ENDIF
C
      DO 210 ITER = 1 , NITER
          IF(ITER.EQ.1) THEN
*VOPTION VEC
C*$*ASSERT PERMUTATION ( IWRK )
          DO 100 IES = 1 , NES
            IE = IWRK(IES)
            DFG=ELM( 4,IE)
            DGG=ELM( 5,IE)
            DHG=ELM( 6,IE)
            DFE=ELM( 7,IE)
            DGE=ELM( 8,IE)
            DHE=ELM( 9,IE)
            DFT=ELM(10,IE)
            DGT=ELM(11,IE)
            DHT=ELM(12,IE)
C
            FV =ELM( 1,IE)-XP
            GV =ELM( 2,IE)-YP
            HV =ELM( 3,IE)-ZP
C
            DET = DFG*(DGE*DHT-DGT*DHE)
     &           +DFE*(DGT*DHG-DGG*DHT)
     &           +DFT*(DGG*DHE-DGE*DHG)
C
            DET = DET+SIGN(FINITE,DET)
C
            A11 = (DGE*DHT-DGT*DHE)/DET
            A21 = (DGT*DHG-DGG*DHT)/DET
            A31 = (DGG*DHE-DGE*DHG)/DET
            A12 = (DHE*DFT-DHT*DFE)/DET
            A22 = (DHT*DFG-DHG*DFT)/DET
            A32 = (DHG*DFE-DHE*DFG)/DET
            A13 = (DFE*DGT-DFT*DGE)/DET
            A23 = (DFT*DGG-DFG*DGT)/DET
            A33 = (DFG*DGE-DFE*DGG)/DET
C
            G(IE) =      -A11*FV-A12*GV-A13*HV
            E(IE) =      -A21*FV-A22*GV-A23*HV
            T(IE) =      -A31*FV-A32*GV-A33*HV
  100     CONTINUE
C
          ELSE
*VOPTION VEC
C*$*ASSERT PERMUTATION ( IWRK )
          DO 200 IES = 1 , NES
            IE = IWRK(IES)
            DFG=ELM( 4,IE)
     &         +ELM(13,IE)*E(IE)+ELM(19,IE)*T(IE)+ELM(22,IE)*E(IE)*T(IE)
            DGG=ELM( 5,IE)
     &         +ELM(14,IE)*E(IE)+ELM(20,IE)*T(IE)+ELM(23,IE)*E(IE)*T(IE)
            DHG=ELM( 6,IE)
     &         +ELM(15,IE)*E(IE)+ELM(21,IE)*T(IE)+ELM(24,IE)*E(IE)*T(IE)
            DFE=ELM( 7,IE)
     &         +ELM(16,IE)*T(IE)+ELM(13,IE)*G(IE)+ELM(22,IE)*T(IE)*G(IE)
            DGE=ELM( 8,IE)
     &         +ELM(17,IE)*T(IE)+ELM(14,IE)*G(IE)+ELM(23,IE)*T(IE)*G(IE)
            DHE=ELM( 9,IE)
     &         +ELM(18,IE)*T(IE)+ELM(15,IE)*G(IE)+ELM(24,IE)*T(IE)*G(IE)
            DFT=ELM(10,IE)
     &         +ELM(19,IE)*G(IE)+ELM(16,IE)*E(IE)+ELM(22,IE)*G(IE)*E(IE)
            DGT=ELM(11,IE)
     &         +ELM(20,IE)*G(IE)+ELM(17,IE)*E(IE)+ELM(23,IE)*G(IE)*E(IE)
            DHT=ELM(12,IE)
     &         +ELM(21,IE)*G(IE)+ELM(18,IE)*E(IE)+ELM(24,IE)*G(IE)*E(IE)
C
            FV =ELM( 1,IE)-XP
     &         +ELM( 4,IE)*G(IE)+ELM( 7,IE)*E(IE)+ELM(10,IE)*T(IE)
     &         +ELM(13,IE)*G(IE)*E(IE)
     &         +ELM(16,IE)*E(IE)*T(IE)
     &         +ELM(19,IE)*T(IE)*G(IE)+ELM(22,IE)*G(IE)*E(IE)*T(IE)
            GV =ELM( 2,IE)-YP
     &         +ELM( 5,IE)*G(IE)+ELM( 8,IE)*E(IE)+ELM(11,IE)*T(IE)
     &         +ELM(14,IE)*G(IE)*E(IE)
     &         +ELM(17,IE)*E(IE)*T(IE)
     &         +ELM(20,IE)*T(IE)*G(IE)+ELM(23,IE)*G(IE)*E(IE)*T(IE)
            HV =ELM( 3,IE)-ZP
     &         +ELM( 6,IE)*G(IE)+ELM( 9,IE)*E(IE)+ELM(12,IE)*T(IE)
     &         +ELM(15,IE)*G(IE)*E(IE)
     &         +ELM(18,IE)*E(IE)*T(IE)
     &         +ELM(21,IE)*T(IE)*G(IE)+ELM(24,IE)*G(IE)*E(IE)*T(IE)
C
            DET = DFG*(DGE*DHT-DGT*DHE)
     &           +DFE*(DGT*DHG-DGG*DHT)
     &           +DFT*(DGG*DHE-DGE*DHG)
C
            DET = DET+SIGN(FINITE,DET)
C
            A11 = (DGE*DHT-DGT*DHE)/DET
            A21 = (DGT*DHG-DGG*DHT)/DET
            A31 = (DGG*DHE-DGE*DHG)/DET
            A12 = (DHE*DFT-DHT*DFE)/DET
            A22 = (DHT*DFG-DHG*DFT)/DET
            A32 = (DHG*DFE-DHE*DFG)/DET
            A13 = (DFE*DGT-DFT*DGE)/DET
            A23 = (DFT*DGG-DFG*DGT)/DET
            A33 = (DFG*DGE-DFE*DGG)/DET
C
            G(IE) = G(IE)-A11*FV-A12*GV-A13*HV
            E(IE) = E(IE)-A21*FV-A22*GV-A23*HV
            T(IE) = T(IE)-A31*FV-A32*GV-A33*HV
  200     CONTINUE
          ENDIF
  210 CONTINUE
C
      IFOUND = 0
      DO 300 IES = 1 , NES
          IE = IWRK(IES)
          IF(ABS(G(IE)) .GT. 1.0+EPS .OR.
     &       ABS(E(IE)) .GT. 1.0+EPS .OR.
     &       ABS(T(IE)) .GT. 1.0+EPS) GO TO 300
          IFOUND = IE
  300 CONTINUE
C
      IF(IFOUND.GT.0) THEN
          GP = G(IFOUND)
          EP = E(IFOUND)
          TP = T(IFOUND)
          IF(JCHECK.EQ.1) THEN
              IE = IFOUND
              FV =ELM( 1,IE)-XP
     &           +ELM( 4,IE)*GP+ELM( 7,IE)*EP+ELM(10,IE)*TP
     &           +ELM(13,IE)*GP*EP
     &           +ELM(16,IE)*EP*TP
     &           +ELM(19,IE)*TP*GP+ELM(22,IE)*GP*EP*TP
              GV =ELM( 2,IE)-YP
     &           +ELM( 5,IE)*GP+ELM( 8,IE)*EP+ELM(11,IE)*TP
     &           +ELM(14,IE)*GP*EP
     &           +ELM(17,IE)*EP*TP
     &           +ELM(20,IE)*TP*GP+ELM(23,IE)*GP*EP*TP
              HV =ELM( 3,IE)-ZP
     &           +ELM( 6,IE)*GP+ELM( 9,IE)*EP+ELM(12,IE)*TP
     &           +ELM(15,IE)*GP*EP
     &           +ELM(18,IE)*EP*TP
     &           +ELM(21,IE)*TP*GP+ELM(24,IE)*GP*EP*TP
C
              ERR = SQRT(FV*FV+GV*GV+HV*HV+FINITE)/(DELTA(IE)+FINITE)
          ENDIF
      ENDIF
C
C
      RETURN
      END
