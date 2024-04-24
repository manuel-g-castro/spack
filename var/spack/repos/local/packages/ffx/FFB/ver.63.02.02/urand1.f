      SUBROUTINE URAND1(N,X,IR)
      IMPLICIT NONE
      INTEGER*4 N,M,LAMBDA,MU,I,IR
      REAL*4    X(N),DINVM
      DATA M      / 1664501 /
      DATA LAMBDA /    1229 /
      DATA MU     /  351750 /
C
      DINVM = 1.0E0 / M
      DO 100 I=1, N
          IR = MOD(LAMBDA*IR + MU, M)
          X(I) = IR*DINVM
 100  CONTINUE
      RETURN
      END
