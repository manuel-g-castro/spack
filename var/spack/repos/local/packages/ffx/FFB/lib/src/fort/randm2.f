C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    RANDM2                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE RANDM2(LTABLE,MAX,LIST,NLIST)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION LTABLE(MAX),LIST(NLIST)
C
C
C      TRANSFORM CONTENT OF A LIST BASED ON GIVEN PERMUTATION TABLE
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          LTABLE   (I); PERMUTATION TABLE
C          MAX         ; NUMBER OF CONTENTS OF PERMUTATION TABLE
C          NLIST       ; NUMBER OF CONTENTS OF LIST
C
C       (3) INPUT-OUTPUT
C          LIST (ILIST); LIST
C
C
      DO 100 ILIST = 1 , NLIST
          LIST(ILIST) = LTABLE(LIST(ILIST))
  100 CONTINUE
C
C     
      RETURN
      END
