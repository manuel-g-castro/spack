C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    FINDLT                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE FINDLT(ELM,NE,IELIST,NEL,XP,YP,ZP,NITR,CONV,
     *                  IE,GP,EP,TP,ERR)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION ELM(24,NE),IELIST(NEL)
C
C
C      COMPUTE LOCAL GZAI, ETA AND THETA COORDINATES OF A SPECIFIED
C     POINT FOR ALL THE LISTED ELEMENTS, AND RETURN THE ELEMENT NUMBER
C     WHICH INCLUDES, OR IS THE NEAREST TO, THE SPECIFIED POINT AND
C     THEIR LOCAL GZAI, EATA, AND THETA COORDINATES
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          ELM   (K,IE); ELEMENT POSITION AND SHAPE DEPENDENT CONSTANTS
C           (SEE ELEM3E FOR THE MEANING OF EACH COMPONENT IN ELM(K,IE))
C          NE          ; NUMBER OF TOTAL ELEMENTS
C
C          IELIST (IEL); ELEMENT LIST TO BE SEARCHED
C          NEL         ; NUMBER OF ELEMENTS TO BE SEARCHED
C
C          XP          ; X-COORDINATE OF THE SPECIFIED POINT
C          YP          ; Y-COORDINATE OF THE SPECIFIED POINT
C          ZP          ; Z-COORDINATE OF THE SPECIFIED POINT
C          NITR        ; MAXIMUM NUMBER OF NEWTON-RAPHSON ITERATIONS
C          CONV        ; CONVERGENCE CRITERIA IN L2-NORM OF THE LOCAL
C                       COORDINATES FOR THE NEWTON-RAPHSON ITERATION
C
C           NOTES ; IF 'NITR' IS SET TO ZERO, NO NEWTON-RAPHSON
C                  ITERATION WILL BE DONE AND THIS ARGUMENT WILL BE
C                  IGNORED.
C
C       (2) OUTPUT
C          IE          ; ELEMENT INCLUDING OR NEAREST TO SPECIFIED POINT
C
C           NOTES ; IF RETURNED WITH THIS ARGUMENT SET TO ZERO, IT
C                  INDICATES THAT COMPUTATION OF THE LOCAL COORDINATES
C                  CONVERGED, WITHIN THE GIVEN MAXIMUM ITERATIONS, FOR
C                  NO ELEMENTS GIVEN IN THE INPUT ELEMENT LIST.
C
C          GP          ; LOCAL GZAI -COORDINATE OF THE SPECIFIED POINT
C          EP          ; LOCAL EATA -COORDINATE OF THE SPECIFIED POINT
C          TP          ; LOCAL THETA-COORDINATE OF THE SPECIFIED POINT
C          ERR         ; IF RETURNED AS ZERO, INDICATES THAT THE
C                       SPECIFIED POINT IS IN THE RETURNED ELEMENT.
C                        IF RETURNED WITH A POSITIVE VALUE, INDICATES
C                       THE DISTANCE (IN LOCAL COORDINATE) BETWEEN THE
C                       SPECIFIED POINT TO THE NEAREST SURFACE OF THE
C                       RETURNED ELEMENT.
C
C
C
C COMPUTE LOCAL COORDINATES FOR THE LISTED ELEMENTS
C
C
C
      IEF  = 0
CCYYAD.2010.02.10---
      ERRF = 1.0E0 
CCYYAD.2010.02.10---
C
      DO 130 IEL = 1 , NEL
          IE = IELIST(IEL)
C
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
     &         +DFE*(DGT*DHG-DGG*DHT)
     &         +DFT*(DGG*DHE-DGE*DHG)
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
          GP = -A11*FV-A12*GV-A13*HV
          EP = -A21*FV-A22*GV-A23*HV
          TP = -A31*FV-A32*GV-A33*HV
C
          IF(NITR.EQ.0) GO TO 120
C
          DO 110 IITR = 1 , NITR
             DFG=ELM( 4,IE)+ELM(13,IE)*EP+ELM(19,IE)*TP+ELM(22,IE)*EP*TP
             DGG=ELM( 5,IE)+ELM(14,IE)*EP+ELM(20,IE)*TP+ELM(23,IE)*EP*TP
             DHG=ELM( 6,IE)+ELM(15,IE)*EP+ELM(21,IE)*TP+ELM(24,IE)*EP*TP
             DFE=ELM( 7,IE)+ELM(16,IE)*TP+ELM(13,IE)*GP+ELM(22,IE)*TP*GP
             DGE=ELM( 8,IE)+ELM(17,IE)*TP+ELM(14,IE)*GP+ELM(23,IE)*TP*GP
             DHE=ELM( 9,IE)+ELM(18,IE)*TP+ELM(15,IE)*GP+ELM(24,IE)*TP*GP
             DFT=ELM(10,IE)+ELM(19,IE)*GP+ELM(16,IE)*EP+ELM(22,IE)*GP*EP
             DGT=ELM(11,IE)+ELM(20,IE)*GP+ELM(17,IE)*EP+ELM(23,IE)*GP*EP
             DHT=ELM(12,IE)+ELM(21,IE)*GP+ELM(18,IE)*EP+ELM(24,IE)*GP*EP
C
             FV =ELM( 1,IE)-XP+ELM( 4,IE)*GP+ELM( 7,IE)*EP+ELM(10,IE)*TP
     &          +ELM(13,IE)*GP*EP+ELM(16,IE)*EP*TP+ELM(19,IE)*TP*GP
     &          +ELM(22,IE)*GP*EP*TP
             GV =ELM( 2,IE)-YP+ELM( 5,IE)*GP+ELM( 8,IE)*EP+ELM(11,IE)*TP
     &          +ELM(14,IE)*GP*EP+ELM(17,IE)*EP*TP+ELM(20,IE)*TP*GP
     &          +ELM(23,IE)*GP*EP*TP
             HV =ELM( 3,IE)-ZP+ELM( 6,IE)*GP+ELM( 9,IE)*EP+ELM(12,IE)*TP
     &          +ELM(15,IE)*GP*EP+ELM(18,IE)*EP*TP+ELM(21,IE)*TP*GP
     &          +ELM(24,IE)*GP*EP*TP
C
             DET = DFG*(DGE*DHT-DGT*DHE)
     &            +DFE*(DGT*DHG-DGG*DHT)
     &            +DFT*(DGG*DHE-DGE*DHG)
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
             GPOLD = GP
             EPOLD = EP
             TPOLD = TP
C
             GP = GP-A11*FV-A12*GV-A13*HV
             EP = EP-A21*FV-A22*GV-A23*HV
             TP = TP-A31*FV-A32*GV-A33*HV
C
             IF((GP-GPOLD)**2+(EP-EPOLD)**2+(TP-TPOLD)**2.LE.CONV)
     &       GO TO 120
  110     CONTINUE
          GO TO 130
C
  120     CONTINUE
C
          ERR = AMAX1(ABS(GP)-1.E0, ABS(EP)-1.E0, ABS(TP)-1.E0, 0.E0)
          IF(IEF.EQ.0 .OR. ERR.LE.ERRF) THEN
              IEF  = IE
              GPF  = GP
              EPF  = EP
              TPF  = TP
              ERRF = ERR
          ENDIF
  130 CONTINUE
C
      IE  = IEF
      GP  = GPF
      EP  = EPF
      TP  = TPF
      ERR = ERRF
C
C
      RETURN
      END
