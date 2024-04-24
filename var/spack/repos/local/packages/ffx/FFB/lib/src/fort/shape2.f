C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    SHAPE2                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      FUNCTION SHAPE2(I,G,E,J)
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION GI(4),EI(4)
      DATA GI /-1.D0 , 1.D0 , 1.D0 ,-1.D0 /
      DATA EI /-1.D0 ,-1.D0 , 1.D0 , 1.D0 /
      DATA U  / 1.D0 /
C
C
C      CALCULATE SHAPE FUNCTION AND ITS DERIVERTIVES AT A GIVEN POINT
C         ( 2-D CALCULATION )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          I           ; SPECIFIES ELEMENT-WISE NODE NO.
C          G           ; GZI -COORDINATE
C          E           ; ETA -COORDINATE
C          J           ; SPECIFIES FUNCTION AS FOLLOWS
C                   1 --- SHAPE FUNCTION
C                   2 --- GZI  DERIVERTIVE OF SHAPE FUNCTION
C                   3 --- ETA  DERIVERTIVE OF SHAPE FUNCTION
C
C       (2) OUTPUT
C          SHAPE2
C
C
      GO TO (100,200,300) , J
C
C      J = 1 ----- SHAPE FUNCTION N
C
  100 CONTINUE
          SHAPE2 = .25D0*(U+GI(I)*G)*(U+EI(I)*E)
      RETURN
C
C      J = 2 ----- DN/DG
C
  200 CONTINUE
          SHAPE2 = .25D0*GI(I)*(U+EI(I)*E)
      RETURN
C
C      J = 3 ----- DN/DE
C
  300 CONTINUE
          SHAPE2 = .25D0*EI(I)*(U+GI(I)*G)
      RETURN
C
C
      END
