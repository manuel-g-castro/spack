C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    VISCOS                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE VISCOS(VISCM,VISCT,NE,VISC)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION VISCT(NE),VISC(NE)
C
C
C      CALCULATE ELEMENT EFFECTIVE VISCOSITY
C     BY ADDING ELEMENT EDDY VISCOSITY TO THE MOLECULAR VISCOSITY
C         ( 2-D , 3-D CALCULATION )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          VISCM       ; MOLECULAR VISCOSITY
C          VISCT   (IE); ELEMENT EDDY VISCOSITY
C          NE          ; NUMBER OF TOTAL ELEMENTS
C
C       (2) OUTPUT
C          VISC    (IE); ELEMENT EFFECTIVE VISCOSITY
C
C
      DO 100 IE = 1 , NE
          VISC(IE) = VISCM+VISCT(IE)
  100 CONTINUE
C
C
      RETURN
      END
