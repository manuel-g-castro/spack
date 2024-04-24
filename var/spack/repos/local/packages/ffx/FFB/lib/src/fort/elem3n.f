C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    ELEM3N                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE ELEM3N(X,Y,Z,NODE,NES,NE,NP,N,GI,EI,TI,ELM)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION X(NP),Y(NP),Z(NP),NODE(N,NE),
     1          GI(N),EI(N),TI(N),ELM(24,NE)
C
C
C      CALCULATE ELEMENT POSITION AND SHAPE DEPENDENT CONSTANTS
C     NEEDED FOR FIND3N
C         ( 3-D CALCULATION )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          X     (IP)  ; X-DIR. COORDINATE         OF NODE
C          Y     (IP)  ; Y-DIR. COORDINATE         OF NODE
C          Z     (IP)  ; Z-DIR. COORDINATE         OF NODE
C          NODE(I,IE)  ; NODE TABLE
C          NES         ; FIRST ELEMENT NUMBER
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          NP          ; NUMBER OF TOTAL     NODES
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT
C          GI  (I)     ; GZAI  COORDINATES OF ELEMENTWISE NODE I
C          EI  (I)     ; EATA  COORDINATES OF ELEMENTWISE NODE I
C          TI  (I)     ; THETA COORDINATES OF ELEMENTWISE NODE I
C
C       (2) OUTPUT
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
C          ELM(15,IE)  ; 0.125*SUM OF Z(NODE(I,IE))*GI(I)*EI(I)
C          ELM(16,IE)  ; 0.125*SUM OF X(NODE(I,IE))*EI(I)*TI(I)
C          ELM(17,IE)  ; 0.125*SUM OF Y(NODE(I,IE))*EI(I)*TI(I)
C          ELM(18,IE)  ; 0.125*SUM OF Z(NODE(I,IE))*EI(I)*TI(I)
C          ELM(19,IE)  ; 0.125*SUM OF X(NODE(I,IE))*TI(I)*GI(I)
C          ELM(20,IE)  ; 0.125*SUM OF Y(NODE(I,IE))*TI(I)*GI(I)
C          ELM(21,IE)  ; 0.125*SUM OF Z(NODE(I,IE))*TI(I)*GI(I)
C
C          ELM(22,IE)  ; 0.125*SUM OF X(NODE(I,IE))*GI(I)*EI(I)*TI(I)
C          ELM(23,IE)  ; 0.125*SUM OF Y(NODE(I,IE))*GI(I)*EI(I)*TI(I)
C          ELM(24,IE)  ; 0.125*SUM OF Z(NODE(I,IE))*GI(I)*EI(I)*TI(I)
C
C
      DO 110 K = 1 , 24
          DO 100 IE = NES , NE
              ELM(K,IE) = 0.E0
  100     CONTINUE
  110 CONTINUE
C
      DO 210 I = 1 , N
          DO 200 IE = NES , NE
              ELM( 1,IE) = ELM( 1,IE)+X(NODE(I,IE))
              ELM( 2,IE) = ELM( 2,IE)+Y(NODE(I,IE))
              ELM( 3,IE) = ELM( 3,IE)+Z(NODE(I,IE))
  200     CONTINUE
  210 CONTINUE
C
      DO 310 I = 1 , N
          DO 300 IE = NES , NE
              ELM( 4,IE) = ELM( 4,IE)+X(NODE(I,IE))*GI(I)
              ELM( 5,IE) = ELM( 5,IE)+Y(NODE(I,IE))*GI(I)
              ELM( 6,IE) = ELM( 6,IE)+Z(NODE(I,IE))*GI(I)
              ELM( 7,IE) = ELM( 7,IE)+X(NODE(I,IE))*EI(I)
              ELM( 8,IE) = ELM( 8,IE)+Y(NODE(I,IE))*EI(I)
              ELM( 9,IE) = ELM( 9,IE)+Z(NODE(I,IE))*EI(I)
              ELM(10,IE) = ELM(10,IE)+X(NODE(I,IE))*TI(I)
              ELM(11,IE) = ELM(11,IE)+Y(NODE(I,IE))*TI(I)
              ELM(12,IE) = ELM(12,IE)+Z(NODE(I,IE))*TI(I)
  300     CONTINUE
  310 CONTINUE
C
      DO 410 I = 1 , N
          DO 400 IE = NES , NE
              ELM(13,IE) = ELM(13,IE)+X(NODE(I,IE))*GI(I)*EI(I)
              ELM(14,IE) = ELM(14,IE)+Y(NODE(I,IE))*GI(I)*EI(I)
              ELM(15,IE) = ELM(15,IE)+Z(NODE(I,IE))*GI(I)*EI(I)
              ELM(16,IE) = ELM(16,IE)+X(NODE(I,IE))*EI(I)*TI(I)
              ELM(17,IE) = ELM(17,IE)+Y(NODE(I,IE))*EI(I)*TI(I)
              ELM(18,IE) = ELM(18,IE)+Z(NODE(I,IE))*EI(I)*TI(I)
              ELM(19,IE) = ELM(19,IE)+X(NODE(I,IE))*TI(I)*GI(I)
              ELM(20,IE) = ELM(20,IE)+Y(NODE(I,IE))*TI(I)*GI(I)
              ELM(21,IE) = ELM(21,IE)+Z(NODE(I,IE))*TI(I)*GI(I)
  400     CONTINUE
  410 CONTINUE
C
      DO 510 I = 1 , N
          DO 500 IE = NES , NE
              ELM(22,IE) = ELM(22,IE)+X(NODE(I,IE))*GI(I)*EI(I)*TI(I)
              ELM(23,IE) = ELM(23,IE)+Y(NODE(I,IE))*GI(I)*EI(I)*TI(I)
              ELM(24,IE) = ELM(24,IE)+Z(NODE(I,IE))*GI(I)*EI(I)*TI(I)
  500     CONTINUE
  510 CONTINUE
C
      DO 610 K = 1 , 24
          DO 600 IE = NES , NE
              ELM(K,IE) = 0.125E0*ELM(K,IE)
  600     CONTINUE
  610 CONTINUE
C
C
      RETURN
      END
