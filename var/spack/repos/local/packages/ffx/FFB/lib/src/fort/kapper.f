C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    KAPPER                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE KAPPER(VKAPM,VISCT,PRT,NE,VKAP)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION VISCT(NE),VKAP(NE)
C
C
C      CALCULATE ELEMENT EFFECTIVE DIFFUSIVITY
C     BY ADDING ELEMENT EDDY DIFFUSIVITY TO THE MOLECULAR DIFFUSIVITY
C         ( 2-D , 3-D CALCULATION )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          VKAPM       ; MOLECULAR DIFFUSIVITY
C          VISCT   (IE); ELEMENT EDDY VISCOSITY
C          PRT         ; TURBULENT PRANDTL NUMBER
C          NE          ; NUMBER OF TOTAL ELEMENTS
C
C       (2) OUTPUT
C          VKAP    (IE); ELEMENT EFFECTIVE DIFFUSIVITY
C
C
      DO 100 IE = 1 , NE
          VKAP(IE) = VKAPM+VISCT(IE)/PRT
  100 CONTINUE
C
C
      RETURN
      END
