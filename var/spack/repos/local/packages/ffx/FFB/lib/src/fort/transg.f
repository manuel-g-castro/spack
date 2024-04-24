C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    TRANSG                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE TRANSG(RAD1,RAD2,TMAT)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION TMAT(3,1)
      DIMENSION EMAT(3,3),N(5)
      DATA N   / 1 , 2 , 3 , 1 , 2 /
      DATA EPS / 1.E-12 /
C
C
C      CALCULATE COORDINATE TRANSVERSE MATRIX
C         ( 2-D GRAPHICS )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          RAD1        ; ANGLE BETWEEN TRANSVERSED Z-AXIS AND ORIGINAL
C                      X-AXIS IN ORIGINAL X-Y PLANE IN RADIANS
C          RAD2        ; ANGLE BETWEEN TRANSVERSED Z-AXIS AND ORIGINAL
C                      Z-AXIS                       IN RADIANS
C
C       (2) OUTPUT
C          TMAT(J,I)   ; TRANSVERSE MATRIX
C
C     INTRINSIC  VARIABLES
C          EMAT(1,I)   ; BASE VECTOR ON TRANSVERSED X AXIS
C          EMAT(2,I)   ; BASE VECTOR ON TRANSVERSED Y AXIS
C          EMAT(3,I)   ; BASE VECTOR ON TRANSVERSED Z AXIS
C                      ( I CORRESPONDING TO X,Y,Z COMPONENT )
C
C
C      BASE VECTOR CALCULATION
C
C
C  (1) AS TO Z-AXIS
C
      EMAT(3,1) = SIN(RAD2)*COS(RAD1)
      EMAT(3,2) = SIN(RAD2)*SIN(RAD1)
      EMAT(3,3) = COS(RAD2)
C
C  (2) AS TO X-AXIS
C
      ABS = EMAT(3,1)**2+EMAT(3,2)**2
      IF(ABS.GT.EPS) THEN
          A = ATAN2(-EMAT(3,1),EMAT(3,2))
          EMAT(1,1) =-COS(A)
          EMAT(1,2) =-SIN(A)
          EMAT(1,3) = 0.E0
      ELSE
          EMAT(1,1) = 1.E0
          EMAT(1,2) = 0.E0
          EMAT(1,3) = 0.E0
      ENDIF
C
C  (3) AS TO Y-AXIS
C
      EMAT(2,1) = EMAT(3,2)*EMAT(1,3)-EMAT(3,3)*EMAT(1,2)
      EMAT(2,2) = EMAT(3,3)*EMAT(1,1)-EMAT(3,1)*EMAT(1,3)
      EMAT(2,3) = EMAT(3,1)*EMAT(1,2)-EMAT(3,2)*EMAT(1,1)
C
C
C      INVERSE MATRIX CALCULATION
C
C
      DET = 0.E0
      DO 10 J = 1 , 3
          DET = DET+EMAT(J,1)*( EMAT(N(J+1),2)*EMAT(N(J+2),3)
     &                         -EMAT(N(J+2),2)*EMAT(N(J+1),3))
   10 CONTINUE
      DO 30 I = 1 , 3
          DO 20 J = 1 , 3
              TMAT(I,J) = EMAT(N(J+1),N(I+1))*EMAT(N(J+2),N(I+2))
     &                   -EMAT(N(J+2),N(I+1))*EMAT(N(J+1),N(I+2))
              TMAT(I,J) = TMAT(I,J)/DET
   20     CONTINUE
   30 CONTINUE
C
C
      RETURN
      END
