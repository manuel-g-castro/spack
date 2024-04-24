C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    ORDERG                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE ORDERG(Z,N,NORDER)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION Z(N),NORDER(N)
C
C
C      MAKE RE-ORDERING TABLE
C         ( 2-D GRAPHICS )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          Z(I)        ; CONSIDERED VARIABLE
C          N           ; LENGTH OF DIMENSION Z(I)
C
C       (2) OUTPUT
C          NORDER(I)   ; RE-ORDERING TABLE
C
C
      DO 100 I = 1 , N
          NORDER(I) = I
  100 CONTINUE
 1000 CONTINUE
          JUG = 0
          DO 1010 I = 1 , N-1
              I1 = NORDER(I)
              I2 = NORDER(I+1)
              IF(Z(I2).LT.Z(I1)) THEN
                  NORDER(I)   = I2
                  NORDER(I+1) = I1
                  JUG = 1
              ENDIF
 1010     CONTINUE
      IF(JUG.EQ.1) GO TO 1000
C
C
      RETURN
      END
