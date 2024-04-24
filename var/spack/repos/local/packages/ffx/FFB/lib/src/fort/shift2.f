C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    SHIFT2                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE SHIFT2(IMODE,DELTA,VISC,DT,BETA,AU,AV,NE,N,
     *                  NSHIFT,LSHIFT,SX,SY,VABS,PEC,GZAI,TAU)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION DELTA(NE),VISC(NE),AU(N,NE),AV(N,NE),LSHIFT(NSHIFT),
     1          SX(NE),SY(NE),VABS(NE),PEC(NE),GZAI(NE),TAU(NE)
      DATA D / 1.D-10 /
C
C
C      CALCULATE ELEMENT SHIFT VECTOR
C         ( 2-D CALCULATION )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          IMODE       ; SPECIFIES THE TYPE OF SHIFT VECTOR AS FOLLOWS
C                   1 --- ELEMENT PECLET NO. DEPENDENT SHIFT VECTOR
C                   2 --- TIME INCREMENT     DEPENDENT SHIFT VECTOR
C          DELTA(IE)   ; ELEMENT CHARACTERISTIC DIMENSION ( IMODE = 1 )
C          VISC (IE)   ; ELEMENT DIFFUSION COEFFICIENT    ( IMODE = 1 )
C          DT          ; TIME INCREMENT                   ( IMODE = 2 )
C          BETA        ; UPWINDING COEFFICIENT
C          AU (I,IE)   ; ELEMENT VECTOR OF U(IP)
C          AV (I,IE)   ; ELEMENT VECTOR OF V(IP)
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT ( N=4 )
C          NSHIFT      ; NUMBER OF ELEMENTS TO ADD ELEMENT SIZE BASED
C                       SHIFTING AMOUNT IN ADDITION TO THE SHIFTING
C                       AMOUNT SPECIFIED BY 'IMODE' ARGUMENT
C          LSHIFT(ISHIFT);ELEMENTS NUMBER TO ADD ELEMENT SIZE BASED
C                       SHIFTING AMOUNT IN ADDITION TO THE SHIFTING
C                       AMOUNT SPECIFIED BY 'IMODE' ARGUMENT
C
C       (2) OUTPUT
C          SX   (IE)   ; X-DIR. SHIFT VECTOR
C          SY   (IE)   ; Y-DIR. SHIFT VECTOR
C
C       (3) WORK
C          VABS (IE)   ; VELOCITY MAGNITUDE CALCULATION   ( IMODE = 1 )
C          PEC  (IE)   ; ELEMENT PECLET NUMBER            ( IMODE = 1 )
C          GZAI (IE)   ; UPWINDING PARAMETER              ( IMODE = 1 )
C          TAU  (IE)   ; UPWINDING PARAMETER              ( IMODE = 1 )
C
C
C      (1) SHIFT VECTOR DEPENDENT ON ELEMENT PECLET NO.
C
      IF(IMODE.EQ.1) THEN
          DO 100 IE = 1 , NE
              SX(IE) = AU(1,IE)+AU(2,IE)+AU(3,IE)+AU(4,IE)
              SY(IE) = AV(1,IE)+AV(2,IE)+AV(3,IE)+AV(4,IE)
  100     CONTINUE
          DO 200 IE = 1 , NE
              SX(IE) = 0.25D0*SX(IE)
              SY(IE) = 0.25D0*SY(IE)
  200     CONTINUE
C
          DO 300 IE = 1 , NE
              VABS(IE) = SX(IE)*SX(IE)+SY(IE)*SY(IE)+D
              VABS(IE) = DSQRT(VABS(IE))
              PEC (IE) = VABS(IE)*DELTA(IE)/(VISC(IE)+D)+D
  300     CONTINUE
C
          DO 400 IE = 1 , NE
              GZAI(IE) = DTANH(PEC(IE))
              GZAI(IE) = 1.D0/GZAI(IE)-1.D0/PEC(IE)
              TAU (IE) = BETA*0.5D0*GZAI(IE)*DELTA(IE)/(VABS(IE))
C
              SX(IE) = TAU(IE)*SX(IE)
              SY(IE) = TAU(IE)*SY(IE)
  400     CONTINUE
      ENDIF
C
C      (2) SHIFT VECTOR DEPENDENT ON THE TIME INCREMENT
C
      IF(IMODE.EQ.2) THEN
          COEFF = 0.25D0*0.5D0*BETA*DT
          DO 500 IE = 1 , NE
              SX(IE) = AU(1,IE)+AU(2,IE)+AU(3,IE)+AU(4,IE)
              SY(IE) = AV(1,IE)+AV(2,IE)+AV(3,IE)+AV(4,IE)
  500     CONTINUE
C
          DO 600 IE = 1 , NE
              SX(IE) = COEFF*SX(IE)
              SY(IE) = COEFF*SY(IE)
  600     CONTINUE
      ENDIF
C
C      (3) ADD ELEMENT SIZE BASED SHIFTING TO SPECIFIED ELEMENTS
C
*VOPTION VEC
      DO 700 ISHIFT = 1 , NSHIFT
          IE = LSHIFT(ISHIFT)
          UP     = 0.25D0*(AU(1,IE)+AU(2,IE)+AU(3,IE)+AU(4,IE))
          VP     = 0.25D0*(AV(1,IE)+AV(2,IE)+AV(3,IE)+AV(4,IE))
          SX(IE) = SX(IE)+0.5D0*DELTA(IE)*UP/DSQRT(UP*UP+VP*VP+D)
          SY(IE) = SY(IE)+0.5D0*DELTA(IE)*VP/DSQRT(UP*UP+VP*VP+D)
  700 CONTINUE
C
C
      RETURN
      END
