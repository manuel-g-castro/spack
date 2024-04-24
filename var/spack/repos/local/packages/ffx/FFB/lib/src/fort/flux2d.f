C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    FLUX2D                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE FLUX2D(IMODE,DT,AU,AV,N,DELTA,LEB,DSB,DCOSB,SNIB,NEB,
     *                  FLUXB)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION AU(N,*),AV(N,*),DELTA(*),
     1          LEB(NEB),DSB(NEB),DCOSB(2,NEB),SNIB(N,NEB),FLUXB(NEB)
C
C
C      CALCULATE HALF VOLUME FLOW LEAVING OUTFLOW BOUNDARY SIDES
C     IN ONE TIME INTEGRATION
C         ( 2-D CALCULATION )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          DT          ; TIME INCREMENT
C          AU (I,IE)   ; ELEMENT VECTOR OF U(IP)
C          AV (I,IE)   ; ELEMENT VECTOR OF V(IP)
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT ( N=4 )
C
C          LEB     (IB); ELEMENT NUMBERS FACING ON BOUNDARIES
C          DSB     (IB); LENGTH OF SIDES FACING ON BOUNDARIES
C          DCOSB (I,IB); DIRECTIONAL COSINE          OF BOUNDARY SIDES
C          SNIB  (I,IB); SHAPE FUNCTION AT MID POINT OF BOUNDARY SIDES
C          NEB         ; NUMBER OF SIDES FACING ON BOUNDARIES
C
C       (2) OUTPUT
C          FLUXB   (IB); HALF VOLUME FLOW LEAVING OUTFLOW BOUNDARY SIDES
C                       IN ONE TIME INTEGRATION
C
C
      IF(IMODE.EQ.1) THEN
      DO 100 IB = 1 , NEB
          IE = LEB(IB)
          FLUXB(IB) = 0.5D0*DELTA(IE)*DSB(IB)
  100 CONTINUE
      ELSE
      DO 200 IB = 1 , NEB
          IE = LEB(IB)
          UB = SNIB(1,IB)*AU(1,IE)+SNIB(2,IB)*AU(2,IE)
     &        +SNIB(3,IB)*AU(3,IE)+SNIB(4,IB)*AU(4,IE)
          VB = SNIB(1,IB)*AV(1,IE)+SNIB(2,IB)*AV(2,IE)
     &        +SNIB(3,IB)*AV(3,IE)+SNIB(4,IB)*AV(4,IE)
          FLUXB(IB) = 0.5D0*DT*(DCOSB(1,IB)*UB+DCOSB(2,IB)*VB)*DSB(IB)
  200 CONTINUE
      ENDIF
C
C
      RETURN
      END
