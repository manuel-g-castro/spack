C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    EDDY2D                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE EDDY2D(C,DELTA,AU,AV,DNXI,DNYI,NE,N,VISCT,
     *                  JDAMP,NDAMP,LDAMP,FACT)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION DELTA(NE),AU(N,NE),AV(N,NE),
     1          DNXI(N,NE),DNYI(N,NE),VISCT(NE),LDAMP(NDAMP),FACT(NDAMP)
C
C
C      CALCULATE ELEMENT EDDY VISCOSITY ; SMAGORINSKY MODEL
C         ( 2-D CALCULATION )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          C           ; SMAGORINSKY CONSTANT
C          DELTA (IE)  ; ELEMENT CHARACTERISTIC DIMENSION
C          AU  (I,IE)  ; ELEMENT VECTOR OF U(IP)
C          AV  (I,IE)  ; ELEMENT VECTOR OF V(IP)
C          DNXI(I,IE)  ; ELEMENT CENTER VALUE OF NX
C          DNYI(I,IE)  ; ELEMENT CENTER VALUE OF NY
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          N           ; NUMBER OF NODE ASSIGNED TO ONE ELEMENT
C          JDAMP       ; DAMP EDDY VISCOSITY IF THIS IS SET TO 1
C          NDAMP       ; NUMBER OF ELEMENTS TO DAMP VISCOSITY  (JDAMP=1)
C          LDAMP(IDAMP); ELEMENTS NUMBER    TO DAMP VISCOSITY  (JDAMP=1)
C          FACT (IDAMP); DAMPING FACTOR     TO DAMP VISCOSITY  (JDAMP=1)
C
C       (2) OUTPUT
C          VISCT (IE)  ; ELEMENT EDDY VISCOSITY
C
C
      DO 100 IE = 1 , NE
          DUX = DNXI(1,IE)*AU(1,IE)+DNXI(2,IE)*AU(2,IE)
     &         +DNXI(3,IE)*AU(3,IE)+DNXI(4,IE)*AU(4,IE)
C
          DUY = DNYI(1,IE)*AU(1,IE)+DNYI(2,IE)*AU(2,IE)
     &         +DNYI(3,IE)*AU(3,IE)+DNYI(4,IE)*AU(4,IE)
C
          DVX = DNXI(1,IE)*AV(1,IE)+DNXI(2,IE)*AV(2,IE)
     &         +DNXI(3,IE)*AV(3,IE)+DNXI(4,IE)*AV(4,IE)
C
          DVY = DNYI(1,IE)*AV(1,IE)+DNYI(2,IE)*AV(2,IE)
     &         +DNYI(3,IE)*AV(3,IE)+DNYI(4,IE)*AV(4,IE)
C
          VISCT(IE)=2.D0*DUX*DUX+2.D0*DVY*DVY
     &             +DUY*DUY+DVX*DVX+2.D0*DUY*DVX
  100 CONTINUE
C
      DO 200 IE = 1 , NE
          VISCT(IE) = DSQRT(VISCT(IE))
  200 CONTINUE
C
      C2 = C*C
      DO 300 IE = 1 , NE
          VISCT(IE) = C2*DELTA(IE)*DELTA(IE)*VISCT(IE)
  300 CONTINUE
C
      IF(JDAMP.EQ.1) THEN
          DO 400 IDAMP = 1 , NDAMP
              VISCT(LDAMP(IDAMP)) = FACT(IDAMP)**2*VISCT(LDAMP(IDAMP))
  400     CONTINUE
      ENDIF
C
C
      RETURN
      END
