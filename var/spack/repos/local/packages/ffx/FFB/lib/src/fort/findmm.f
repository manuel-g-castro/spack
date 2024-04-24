C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    FINDMM                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE FINDMM(S,NP,SMIN,SMAX)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION S(NP)
C
C
C      FIND MAXIMUM AND MINIMUM VALUE AMONG GIVEN ARRAY ELEMENTS
C         ( 2-D , 3-D GRAPHICS )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          S(IP)       ; GIVEN ARRAY
C          NP          ; NUMBER OF ARRAY ELEMENTS
C       (2) OUTPUT
C          SMIN        ; MINIMUM VALUE OF AMONG THE ARRAY ELEMENTS
C          SMAX        ; MAXIMUM VALUE OF AMONG THE ARRAY ELEMENTS
C
C
      DO 100 IP = 1 , NP
          IF(IP.EQ.1 .OR. S(IP).LE.SMIN) THEN
             SMIN = S(IP)
          ENDIF
          IF(IP.EQ.1 .OR. S(IP).GE.SMAX) THEN
             SMAX = S(IP)
          ENDIF
  100 CONTINUE
C
C
      RETURN
      END
