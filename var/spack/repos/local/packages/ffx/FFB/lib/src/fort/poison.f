C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    POISON                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE POISON(IDIM,NMAX,NMIN,EPS,ALF,
     *                  AP,IPNP,NPP,MAXP,MAXPP,NP,F,S,
     *                  DF,IRN,NRN,ERR,IUT0,IERR)
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*4   AP
      DIMENSION AP(MAXPP,NP),IPNP(MAXPP,NP),NPP(NP),F(NP),S(NP),DF(NP)
C
      CHARACTER*72 ERMSG
     & /' *** SUBROUTINE POISON REPORTS A FATAL ERROR OCCURENCE ***' /
      CHARACTER*72 EREXP1
     & /' NO ROOM LEFT IN S(MAXP) FOR CONDUCTING POISON OPERATIONS' /
      CHARACTER*72 EREXP2
     & /' MAXPP IS TOO SMALL TO CONDUCT POISON OPERATIONS' /
C
C
C      SOLVE POISSON EQUATION BY THE JACOBI ITERATION ALGORITHM
C         ( 2-D , 3-D CALCULATION )
C
C
C     NOTE ; INTRINSIC INITIAL CONDITION IS GIVEN FOR THE ITERATION
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          IDIM        ; SPECIFIES THE SPACE DIMENSION AS FOLLOWS
C                   2 --- TWO   DIMENSIONAL PROBLEM
C                   3 --- THREE DIMENSIONAL PROBLEM
C          NMAX        ; MAX. ITERATION NO. FOR JACOBI ITERATION
C          NMIN        ; MIN. ITERATION NO. FOR JACOBI ITERATION
C          EPS         ; ALLOWABLE MAXIMUM RESIDUAL ERROR
C          ALF         ; RELAXATION COEFFICIENT
C          AP  (IPP,IP); CONDENSED DIFFUSION MATRIX
C          IPNP(IPP,IP); ADJACENT NODE NUMBER TO NODE IP
C                      ( IF NPP(IP).LT.MINPP , THEN IPNP(NPP(IP)+1,IP),
C                       IPNP(MINPP,IP) MUST BE SET TO AN IMAGINARY
C                       NODE NO. BETWEEN NP+1,MAXP.)
C          NPP     (IP); NUMBER OF ADJACENT NODES TO NODE IP
C          MAXP        ; THE       DIMENSION OF ARRAY S
C          MAXPP       ; THE FIRST DIMENSION OF ARRAY IPNP
C          NP          ; NUMBER OF TOTAL NODES
C          F       (IP); GLOBAL SOURCE VECTOR
C          IUT0        ; DEVICE NO. TO REPORT ERROR OCCURENCE
C
C       (2) OUTPUT
C          S       (IP); SOLUTION VECTOR
C          IRN         ; RETURN CODE TO REPORT THE CONVERGENCE
C                   0 --- NOT CONVERGED
C                   1 ---     CONVERGED
C          NRN         ; CALCULATION ITERATED NUMBER
C          ERR         ; RESIDUAL ERROR
C          IERR        ; RETURN CODE TO REPORT ERROR OCCURENCE
C                   0 --- NORMAL TERMINATION
C                   1 --- A FATAL ERROR HAS OCCURED
C
C       (4) WORK
C          DF      (IP); USED FOR RESIDUAL FORCE VECTOR
C
C
C
C           ( ALL TO BE ZERO )
C
C
      IF(IDIM.EQ.2) THEN
          MINPP = 9
      ELSE
          MINPP = 27
      ENDIF
C
      IERR = 0
C
      IF(MAXP.LE.NP) THEN
          WRITE(IUT0,6300) ERMSG
          WRITE(IUT0,6300) EREXP1 
          IERR = 1 
          RETURN
      ENDIF
C
      IF(MAXPP.LT.MINPP) THEN
          WRITE(IUT0,6300) ERMSG
          WRITE(IUT0,6300) EREXP2  
          IERR = 1 
          RETURN
      ENDIF
C
C      CLEAR THE ARRAY S(IP) FOR POISON OPERATIONS
C
      DO 100 IP = NP+1 , MAXP
          S(IP) = 0.D0
  100 CONTINUE
C
C      SET INITIAL VALUES
C
      DO 200 IP = 1 , NP
          S(IP) = 0.D0
  200 CONTINUE
C
C      JACOBI ITERATION LOOP START
C
      IRN = 0 
      NRN = NMAX
C
      DO 2000 ITER = 1 , NMAX
C
C      CALCULATION OF RESIDUAL FORCE VECTOR
C
          DO 400 IP = 1 , NP
              DF(IP) = AP(2,IP)*S(IPNP(2,IP))+AP(3,IP)*S(IPNP(3,IP))
     &                +AP(4,IP)*S(IPNP(4,IP))+AP(5,IP)*S(IPNP(5,IP))
  400     CONTINUE
          DO 500 IP = 1 , NP
              DF(IP) = DF(IP)
     &                +AP(6,IP)*S(IPNP(6,IP))+AP(7,IP)*S(IPNP(7,IP))
     &                +AP(8,IP)*S(IPNP(8,IP))+AP(9,IP)*S(IPNP(9,IP))
  500     CONTINUE
C
          IF(IDIM.EQ.3) THEN
          DO 600 IP = 1 , NP
              DF(IP) = DF(IP)
     &                +AP(10,IP)*S(IPNP(10,IP))+AP(11,IP)*S(IPNP(11,IP))
     &                +AP(12,IP)*S(IPNP(12,IP))+AP(13,IP)*S(IPNP(13,IP))
     &                +AP(14,IP)*S(IPNP(14,IP))+AP(15,IP)*S(IPNP(15,IP))
  600     CONTINUE
          DO 700 IP = 1 , NP
              DF(IP) = DF(IP)
     &                +AP(16,IP)*S(IPNP(16,IP))+AP(17,IP)*S(IPNP(17,IP))
     &                +AP(18,IP)*S(IPNP(18,IP))+AP(19,IP)*S(IPNP(19,IP))
     &                +AP(20,IP)*S(IPNP(20,IP))+AP(21,IP)*S(IPNP(21,IP))
  700     CONTINUE
          DO 800 IP = 1 , NP
              DF(IP) = DF(IP)
     &                +AP(22,IP)*S(IPNP(22,IP))+AP(23,IP)*S(IPNP(23,IP))
     &                +AP(24,IP)*S(IPNP(24,IP))+AP(25,IP)*S(IPNP(25,IP))
     &                +AP(26,IP)*S(IPNP(26,IP))+AP(27,IP)*S(IPNP(27,IP))
  800     CONTINUE
          ENDIF
C
          DO 910 IPP = MINPP+1 , MAXPP
              DO 900 IP = 1 , NP
                  IF(NPP(IP).GE.IPP) THEN
                      DF(IP) = DF(IP)
     &                        +AP(IPP,IP)*S(IPNP(IPP,IP))
                  ENDIF
  900         CONTINUE
  910     CONTINUE
C
C      CALCULATE NEW VALUE WHILE CALCULATING RESIDUAL ERROR
C
          ERR = 0.D0
          DO 1000 IP = 1 , NP
              SNEW  = (F(IP)-DF(IP))/AP(1,IP)
              ERR   = ERR+DABS(SNEW-S(IP))
              S(IP) = ALF*SNEW+(1.D0-ALF)*S(IP)
 1000     CONTINUE
C
C      RETURN IF CONVERGED
C
          ERR = ERR/NP
          IF(ITER.LT.NMIN) GO TO 2000
          IF(ERR .LE.EPS ) THEN 
              IRN = 1 
              NRN = ITER 
              RETURN 
           ENDIF
C
C      JACOBI ITERATION LOOP END
C
 2000 CONTINUE
C
C
      RETURN
 6300 FORMAT(A72)
      END
