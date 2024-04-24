C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    RANDOM                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE RANDOM(INIT,NRAN,LRAN,NWALK1,NWALK2)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION LRAN(NRAN)
C
      REAL*4 RAN(1)
C
C
C      GENERATE A SET OF UNIQUE RANDOM INTEGER NUMBERS
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          NRAN        ; NUMBER OF RANDOM INTEGERS TO BE GENERATED
C
C       (2) OUTPUT
C          LRAN (IRAN) ; RANDOM INTEGER NUMBERS GENERATED
C          NWALK1      ; NUMBER OF TIMES  WALKED AROUND
C          NWALK2      ; AVERAGE DISTANCE WALKED AROUND

C       (3) INPUT-OUTPUT
C          INIT        ; INITIAL AND FINAL RANDOM NUMBER
C
C
      NWALK1 = 0
      NWALK2 = 0
C
      DO 100 IRAN = 1 , NRAN
          LRAN(IRAN) = 0
  100 CONTINUE
C
      DO 130 IRAN = 1 , NRAN
#ifdef VOS
          CALL HSRU1M(1,INIT,RAN,IERRDM)
#else
          CALL LSGU1M(1,INIT,RAN,IERRDM)
#endif
          N = RAN(1)*NRAN+1
          IF(N.GT.NRAN) N = NRAN
C
          IF(LRAN(N).EQ.0) THEN
              LRAN(N) = IRAN
          ELSE
              DO 110 IWALK = 1 , NRAN-1
                  NUP = N+IWALK
                  IF(NUP.LE.NRAN) THEN
                      IF(LRAN(NUP).EQ.0) THEN
                          LRAN(NUP) = IRAN
                          NWALK1 = NWALK1+1 
                          NWALK2 = NWALK2+IWALK
                          GO TO 120
                      ENDIF
                  ENDIF
C
                  NDOWN = N-IWALK
                  IF(NDOWN.GE.1) THEN
                      IF(LRAN(NDOWN).EQ.0) THEN
                          LRAN(NDOWN) = IRAN
                          NWALK1 = NWALK1+1 
                          NWALK2 = NWALK2+IWALK
                          GO TO 120
                      ENDIF
                  ENDIF
  110         CONTINUE
C
  120         CONTINUE
          ENDIF
  130 CONTINUE
C
      IF(NWALK1.GE.1) NWALK2 = NWALK2/NWALK1
C
C     
      RETURN
      END
