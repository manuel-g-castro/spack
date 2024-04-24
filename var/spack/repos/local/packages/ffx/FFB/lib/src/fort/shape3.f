C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    SHAPE3                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      FUNCTION SHAPE3(I,G,E,T,J)
      IMPLICIT REAL*4 (A-H,O-Z)
      DIMENSION GI(8),EI(8),TI(8)
      DATA GI /-1.0E0, 1.0E0, 1.0E0,-1.0E0,-1.0E0, 1.0E0, 1.0E0,-1.0E0/
      DATA EI /-1.0E0,-1.0E0, 1.0E0, 1.0E0,-1.0E0,-1.0E0, 1.0E0, 1.0E0/
      DATA TI /-1.0E0,-1.0E0,-1.0E0,-1.0E0, 1.0E0, 1.0E0, 1.0E0, 1.0E0/
      DATA U  / 1.0E0 /
C
C
C      CALCULATE SHAPE FUNCTION AND ITS DERIVERTIVES AT A GIVEN POINT
C         ( 3-D CALCULATION : SINGLE WORD VERSION )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          I           ; SPECIFIES ELEMENT-WISE NODE NO.
C          G           ; GZI -COORDINATE
C          E           ; ETA -COORDINATE
C          T           ; TETA-COORDINATE
C          J           ; SPECIFIES FUNCTION AS FOLLOWS
C                   1 --- SHAPE FUNCTION
C                   2 --- GZI  DERIVERTIVE OF SHAPE FUNCTION
C                   3 --- ETA  DERIVERTIVE OF SHAPE FUNCTION
C                   4 --- TETA DERIVERTIVE OF SHAPE FUNCTION
C
C       (2) OUTPUT
C          SHAPE3
C
C
      GO TO ( 100 , 200 , 300 , 400 ) , J
C
C      J = 1 ; SHAPE FUNCTION
C
  100 CONTINUE
          SHAPE3 = 0.125E0*(U+GI(I)*G)*(U+EI(I)*E)*(U+TI(I)*T)
      RETURN
C
C      J = 2 ; GZAI DERIVERTIVE
C
  200 CONTINUE
          SHAPE3 = 0.125E0*GI(I)*(U+EI(I)*E)*(U+TI(I)*T)
      RETURN
C
C      J = 3 ; EATA DERIVERTIVE
C
  300 CONTINUE
          SHAPE3 = 0.125E0*EI(I)*(U+TI(I)*T)*(U+GI(I)*G)
      RETURN
C
C      J = 4 ; TETA DERIVERTIVE
C
  400 CONTINUE
          SHAPE3 = 0.125E0*TI(I)*(U+GI(I)*G)*(U+EI(I)*E)
      RETURN
C
C
      END
