C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    VBSETE                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE VBSETE(LB,BB,NB,NP,VN)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION LB(NB),BB(NB),VN(NP)
C
C
C      GIVE PRESCRIBED BOUNDARY VALUES TO VN(IP)
C         ( 2-D & 3-D CALCULATION : SINGLE WORD VERSION )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          LB   (IB)   ; BOUNDARY NODE SPECIFYING LIST VECTOR
C          BB   (IB)   ; PRESCRIBED BOUNDARY VALUES
C          NB          ; NUMBER OF PRESCRIBED BOUNDARY NODES
C          NP          ; NUMBER OF TOTAL     NODES
C
C       (3) INPUT-OUTPUT
C          VN   (IP)   ; NODALLY DEFINED VARIABLE GIVEN B.C.
C
C
C*$*ASSERT PERMUTATION ( LB )
      DO 100 IB = 1 , NB
          VN(LB(IB)) = BB(IB)
  100 CONTINUE
C
C
      RETURN
      END
