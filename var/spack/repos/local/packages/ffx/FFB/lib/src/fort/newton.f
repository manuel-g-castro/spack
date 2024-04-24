C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    NEWTON                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE NEWTON(A,SIG,N,RTINIT,EPS,NMAX,RT,IRN,NRN,ERR)
      IMPLICIT REAL*4 (A-H,O-Z)
C
C
C      CALCULATE ISO-RATIO BY NEWTON-LAPSON METHOD
C         ( 2-D , 3-D GRAPHICS )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          A            ; INITIAL TERM
C          SIG          ; SIGMA
C          N            ; NUMBER OF TERMS
C          RTINIT       ; INITIAL VALUE OF ISO-RATIO
C          EPS          ; CONVERGENCE JUDGING PARAMETER
C          NMAX         ; MAX. ITERATION NO.
C
C       (2) OUTPUT
C          RT           ; ISO-RATIO
C          IRN          ; RETURN CODE ( IRN = 1 CONVERGED , = 0 NOT )
C          NRN          ; CALCULATION ITERATED NO.
C          ERR          ; RESIDUAL ERROR
C
C
C
      IRN = 0
      NRN = NMAX
      RT  = RTINIT
      DO 100 INL = 1 , NMAX
          F    = RT**N-SIG/A*RT+SIG/A-1.D0
          DFDR = N*RT**(N-1)-SIG/A
          DR   =-F/DFDR
          RT   = RT + DR
          SIGE = A*(RT**N-1.D0)/(RT-1.D0)
          ERR  = (SIGE-SIG)/SIG
          IF(ABS(ERR).LE.EPS) THEN
              IRN = 1
              NRN = INL
              RETURN
          ENDIF
  100 CONTINUE
C
C
      RETURN
      END
