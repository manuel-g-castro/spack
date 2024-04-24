C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    FIND33                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE FIND33(ELM,NE,NM,XM,YM,ZM,IEM,LIST,NLIST,NITER,
     *                  GM,EM,TM,JCHECK,DELTA,ERRMAX)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION ELM(24,NE),XM(NM),YM(NM),ZM(NM),IEM(NM),LIST(NLIST),
     1          GM(NM),EM(NM),TM(NM),DELTA(NE)
C
      DATA EPS    / 1.E-3 /
      DATA FINITE / 1.E-30 /
C
C
C      CALCULATE LOCAL COORDINATES AT SPECIFIED POINTS AS TO SPECIFIED
C     ELEMENTS. RETURN (-1)*ORIGINAL ELEMENT NUMBER FOR THOSE POINTS NOT
C     INCLUDED IN THE SPECIFIED ELEMENT.
C         ( 3-D CALCULATION , LIST OPERATION VERSION )
C
C
C     NOTE ; 1. LOCAL GZAI, EATA, AND THETA COORDINATES WILL BE
C              CALCULATED ONLY FOR POINTS SPECIFIED BY LIST(ILIST),
C              AND WITH POSITIVE ELEMENT NUMBER, BY THE NEWTON LAPSON
C              METHODS.
C
C     NOTE ; 2. TOTAL OF 'NITER' ITERATIONS WILL BE DONE WITH THE
C              NEWTON LAPSON METHOD, REGARDLESS TO ITS CONVERGENCE.
C              BUT, TWO OR THREE ITERATIONS ARE, IN GENERAL, FOUND
C              ENOUGH TO OBTAIN THE LOCAL COORDINATES WITH REASONABLE
C              ACCURACY UNLESS THE ELEMENT IS STRONGLY SKEWED.
C
C     ARGUMENT LISTINGS
C       (1) INPUT
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
C          NE          ; NUMBER OF TOTAL     ELEMENTS
C          NM          ; NUMBER OF POINTS
C
C          XM    (IM)  ; X-DIR. COORDINATE OF THE POINTS
C          YM    (IM)  ; Y-DIR. COORDINATE OF THE POINTS
C          ZM    (IM)  ; Z-DIR. COORDINATE OF THE POINTS
C          IEM   (IM)  ; ELEMENT NUMBER    OF THE POINTS
C           NOTES ; IEM(IM) WILL BE RETURNED WITH THE SIGN CHANGED FOR
C                  THOSE POINTS NOT INCLUDED IN THE SPECIFIED ELEMENT
C          LIST(ILIST) ; SPECIFIES POINT NUMBERS FOR WHICH TRANSACTIONS
C                        WILL BE DONE
C          NLIST       ; NUMBER OF POINTS        FOR WHICH TRANSACTIONS
C                        WILL BE DONE
C          NITER       ; NUMBER OF ITERATIVE CALCULATIONS TO BE DONE
C          JCHECK      ; MAXIMUM RESIDUAL ERROR CHECK WILL BE DONE
C                        IF THIS IS SET TO ONE
C          DELTA (IE)  ; CHARACATERISTIC ELEMENT DIMENSION USED FOR
C                        THE RESIDUAL ERROR CHECK
C                        DUMMY ARGUMENT FOR JCHECK = 0
C
C       (2) OUTPUT
C          GM    (IM)  ; GZAI  COORDINATE OF THE POINT FOR THE ELEMENT
C          EM    (IM)  ; EATA  COORDINATE OF THE POINT FOR THE ELEMENT
C          TM    (IM)  ; THETA COORDINATE OF THE POINT FOR THE ELEMENT
C          ERRMAX      ; MAXIMUM RELATIVE RESIDUAL ERROR
C                        IN GZAI, EATA, THETA CALCULATION FOR POINTS
C                        INCLUDED IN THE SPECIFIED ELEMENT ( JCHECK=1 )
C
C
      DO 210 ITER = 1 , NITER
          IF(ITER.EQ.1) THEN
C*$*ASSERT PERMUTATION ( LIST )
          DO 100 ILIST = 1 , NLIST
            IM = LIST(ILIST)
            IF(IM.LE.0) GO TO 100
            IE = IEM(IM)
            IF(IE.LE.0) GO TO 100
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
            FV =ELM( 1,IE)-XM(IM)
            GV =ELM( 2,IE)-YM(IM)
            HV =ELM( 3,IE)-ZM(IM)
C
            DET = DFG*(DGE*DHT-DGT*DHE)
     &           +DFE*(DGT*DHG-DGG*DHT)
     &           +DFT*(DGG*DHE-DGE*DHG)
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
            GM(IM) =      -A11*FV-A12*GV-A13*HV
            EM(IM) =      -A21*FV-A22*GV-A23*HV
            TM(IM) =      -A31*FV-A32*GV-A33*HV
  100     CONTINUE
C
          ELSE
*VOPTION VEC
C*$*ASSERT PERMUTATION ( LIST )
          DO 200 ILIST = 1 , NLIST
            IM = LIST(ILIST)
            IF(IM.LE.0) GO TO 200
            IE = IEM(IM)
            IF(IE.LE.0) GO TO 200
            DFG=ELM( 4,IE)
     &     +ELM(13,IE)*EM(IM)+ELM(19,IE)*TM(IM)+ELM(22,IE)*EM(IM)*TM(IM)
            DGG=ELM( 5,IE)
     &     +ELM(14,IE)*EM(IM)+ELM(20,IE)*TM(IM)+ELM(23,IE)*EM(IM)*TM(IM)
            DHG=ELM( 6,IE)
     &     +ELM(15,IE)*EM(IM)+ELM(21,IE)*TM(IM)+ELM(24,IE)*EM(IM)*TM(IM)
            DFE=ELM( 7,IE)
     &     +ELM(16,IE)*TM(IM)+ELM(13,IE)*GM(IM)+ELM(22,IE)*TM(IM)*GM(IM)
            DGE=ELM( 8,IE)
     &     +ELM(17,IE)*TM(IM)+ELM(14,IE)*GM(IM)+ELM(23,IE)*TM(IM)*GM(IM)
            DHE=ELM( 9,IE)
     &     +ELM(18,IE)*TM(IM)+ELM(15,IE)*GM(IM)+ELM(24,IE)*TM(IM)*GM(IM)
            DFT=ELM(10,IE)
     &     +ELM(19,IE)*GM(IM)+ELM(16,IE)*EM(IM)+ELM(22,IE)*GM(IM)*EM(IM)
            DGT=ELM(11,IE)
     &     +ELM(20,IE)*GM(IM)+ELM(17,IE)*EM(IM)+ELM(23,IE)*GM(IM)*EM(IM)
            DHT=ELM(12,IE)
     &     +ELM(21,IE)*GM(IM)+ELM(18,IE)*EM(IM)+ELM(24,IE)*GM(IM)*EM(IM)
C
            FV =ELM( 1,IE)-XM(IM)
     &         +ELM( 4,IE)*GM(IM)+ELM( 7,IE)*EM(IM)+ELM(10,IE)*TM(IM)
     &         +ELM(13,IE)*GM(IM)*EM(IM)
     &         +ELM(16,IE)*EM(IM)*TM(IM)
     &         +ELM(19,IE)*TM(IM)*GM(IM)+ELM(22,IE)*GM(IM)*EM(IM)*TM(IM)
            GV =ELM( 2,IE)-YM(IM)
     &         +ELM( 5,IE)*GM(IM)+ELM( 8,IE)*EM(IM)+ELM(11,IE)*TM(IM)
     &         +ELM(14,IE)*GM(IM)*EM(IM)
     &         +ELM(17,IE)*EM(IM)*TM(IM)
     &         +ELM(20,IE)*TM(IM)*GM(IM)+ELM(23,IE)*GM(IM)*EM(IM)*TM(IM)
            HV =ELM( 3,IE)-ZM(IM)
     &         +ELM( 6,IE)*GM(IM)+ELM( 9,IE)*EM(IM)+ELM(12,IE)*TM(IM)
     &         +ELM(15,IE)*GM(IM)*EM(IM)
     &         +ELM(18,IE)*EM(IM)*TM(IM)
     &         +ELM(21,IE)*TM(IM)*GM(IM)+ELM(24,IE)*GM(IM)*EM(IM)*TM(IM)
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
            GM(IM) = GM(IM)-A11*FV-A12*GV-A13*HV
            EM(IM) = EM(IM)-A21*FV-A22*GV-A23*HV
            TM(IM) = TM(IM)-A31*FV-A32*GV-A33*HV
  200     CONTINUE
          ENDIF
  210 CONTINUE
C
*VOPTION VEC
C*$*ASSERT PERMUTATION ( LIST )
      DO 300 ILIST = 1 , NLIST
          IM = LIST(ILIST)
          IF(IM.LE.0) GO TO 300
          IF(IEM(IM).LE.0             .OR.
     &       ABS(GM(IM)) .LE. 1.0+EPS .AND.
     &       ABS(EM(IM)) .LE. 1.0+EPS .AND.
     &       ABS(TM(IM)) .LE. 1.0+EPS) GO TO 300
          IEM(IM) = -IEM(IM)
  300 CONTINUE
C
      IF(JCHECK.EQ.0) RETURN
      ERRMAX = 0.E0
C*$*ASSERT PERMUTATION ( LIST )
      DO 400 ILIST = 1 , NLIST
          IM = LIST(ILIST)
          IF(IM.LE.0) GO TO 400
          IE = IEM(IM)
          IF(IE.LE.0) GO TO 400
            FV =ELM( 1,IE)-XM(IM)
     &         +ELM( 4,IE)*GM(IM)+ELM( 7,IE)*EM(IM)+ELM(10,IE)*TM(IM)
     &         +ELM(13,IE)*GM(IM)*EM(IM)
     &         +ELM(16,IE)*EM(IM)*TM(IM)
     &         +ELM(19,IE)*TM(IM)*GM(IM)+ELM(22,IE)*GM(IM)*EM(IM)*TM(IM)
            GV =ELM( 2,IE)-YM(IM)
     &         +ELM( 5,IE)*GM(IM)+ELM( 8,IE)*EM(IM)+ELM(11,IE)*TM(IM)
     &         +ELM(14,IE)*GM(IM)*EM(IM)
     &         +ELM(17,IE)*EM(IM)*TM(IM)
     &         +ELM(20,IE)*TM(IM)*GM(IM)+ELM(23,IE)*GM(IM)*EM(IM)*TM(IM)
            HV =ELM( 3,IE)-ZM(IM)
     &         +ELM( 6,IE)*GM(IM)+ELM( 9,IE)*EM(IM)+ELM(12,IE)*TM(IM)
     &         +ELM(15,IE)*GM(IM)*EM(IM)
     &         +ELM(18,IE)*EM(IM)*TM(IM)
     &         +ELM(21,IE)*TM(IM)*GM(IM)+ELM(24,IE)*GM(IM)*EM(IM)*TM(IM)
C
          ERR =  SQRT(FV*FV+GV*GV+HV*HV)/DELTA(IE)
          ERRMAX = AMAX1(ERR,ERRMAX)
  400 CONTINUE
C
C
      RETURN
      END
