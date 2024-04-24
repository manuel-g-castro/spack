C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    SOLVEE                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE SOLVEE(IDIM,IMASS,AC,CM,IPNP,NPP,MAXP,MAXPP,NP,
     *                  F,LB,NB,S,DF,IUT0,IERR)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION AC(MAXPP,NP),CM(NP),IPNP(MAXPP,NP),NPP(NP),F(NP),LB(NB),
     1          S(NP),DF(NP)
C
      CHARACTER*60 ERMSGB
     & / ' ## SUBROUTINE SOLVEE: FATAL      ERROR OCCURENCE; RETURNED' /
      CHARACTER*60 EREXP1
     & / ' NO ROOM LEFT IN S(MAXP) FOR DOING SOLVEE OPERATIONS       ' /
      CHARACTER*60 EREXP2
     & / ' MAXPP IS TOO SMALL TO DO SOLVEE OPERATIONS                ' /
C
C
C      SOLVE MATRIX-EQUATION ' MV = F ' BY MULTIPASS ALGORITHM
C         ( 2-D & 3-D CALCULATION : SINGLE WORD VERSION )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          IDIM        ; SPECIFIES THE SPACE DIMENSION AS FOLLOWS
C                   2 --- TWO   DIMENSIONAL PROBLEM
C                   3 --- THREE DIMENSIONAL PROBLEM
C          IMASS       ; ITERATION NUMBER OF MULTI-PASS ALGORITHM
C                      ( FOR LUMPED MASS CALCULATION , SET IMASS TO 1 )
C          AC  (IPP,IP); CONDENSED MASS     MATRIX
C          CM      (IP); GLOBAL LUMPED MASS MATRIX ( INVERSED )
C          IPNP(IPP,IP); ADJACENT NODE NUMBER TO NODE IP
C                      ( IF NPP(IP).LT.MINPP , THEN IPNP(NPP(IP)+1,IP),
C                       IPNP(MINPP,IP) MUST BE SET TO AN IMAGINARY
C                       NODE NO. BETWEEN NP+1,MAXP.)
C          NPP     (IP); NUMBER OF ADJACENT NODES TO NODE IP
C          MAXP        ; THE       DIMENSION OF ARRAY S
C          MAXPP       ; THE FIRST DIMENSION OF ARRAY IPNP
C          NP          ; NUMBER OF TOTAL NODES
C          F       (IP); GLOBAL FORCE VECTOR
C          LB      (IB); PRESCRIBED NODES LIST VECTOR
C          NB          ; NUMBER OF NODE PRESCRIBED TO ZERO
C          IUT0        ; FILE NUMBER TO REPORT ERROR OCCURENCE
C
C       (2) OUTPUT
C          S       (IP); SOLUTION OF M*V = F
C          IERR        ; RETURN CODE TO REPORT ERROR OCCURENCE
C                   0 --- NORMAL TERMINATION
C                   1 --- A FATAL ERROR HAS OCCURED
C
C       (3) WORK
C          DF(IP)      ; GLOBAL FORCE VECTOR M*V
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
          WRITE(IUT0,*) ERMSGB
          WRITE(IUT0,*) EREXP1
          IERR = 1
          RETURN
      ENDIF
C
      IF(MAXPP.LT.MINPP) THEN
          WRITE(IUT0,*) ERMSGB
          WRITE(IUT0,*) EREXP2
          IERR = 1
          RETURN
      ENDIF
C
C      CLEAR THE ARRAY S(IP) FOR SOLVEE OPERATIONS
C
      DO 100 IP = NP+1 , MAXP
          S(IP) = 0.E0
  100 CONTINUE
C
C      SET INITIAL VALUES
C
      DO 200 IP = 1 , NP
          S(IP) = CM(IP)*F(IP)
  200 CONTINUE
C
C*$*ASSERT PERMUTATION ( LB )
      DO 300 IB = 1 , NB
          S(LB(IB)) = 0.E0
  300 CONTINUE
C
C      MULTIPASS LOOP START
C
      DO 2000 ITER = 2 , IMASS
C
C      CALCULATION OF  DF = M*V
C
          DO 400 IP = 1 , NP
              DF(IP) = AC(1,IP)*S(IPNP(1,IP))+AC(2,IP)*S(IPNP(2,IP))
     &                +AC(3,IP)*S(IPNP(3,IP))+AC(4,IP)*S(IPNP(4,IP))
     &                +AC(5,IP)*S(IPNP(5,IP))
  400     CONTINUE
          DO 500 IP = 1 , NP
              DF(IP) = DF(IP)
     &                +AC(6,IP)*S(IPNP(6,IP))+AC(7,IP)*S(IPNP(7,IP))
     &                +AC(8,IP)*S(IPNP(8,IP))+AC(9,IP)*S(IPNP(9,IP))
  500     CONTINUE
C
          IF(IDIM.EQ.3) THEN
          DO 600 IP = 1 , NP
              DF(IP) = DF(IP)
     &                +AC(10,IP)*S(IPNP(10,IP))+AC(11,IP)*S(IPNP(11,IP))
     &                +AC(12,IP)*S(IPNP(12,IP))+AC(13,IP)*S(IPNP(13,IP))
     &                +AC(14,IP)*S(IPNP(14,IP))+AC(15,IP)*S(IPNP(15,IP))
  600     CONTINUE
          DO 700 IP = 1 , NP
              DF(IP) = DF(IP)
     &                +AC(16,IP)*S(IPNP(16,IP))+AC(17,IP)*S(IPNP(17,IP))
     &                +AC(18,IP)*S(IPNP(18,IP))+AC(19,IP)*S(IPNP(19,IP))
     &                +AC(20,IP)*S(IPNP(20,IP))+AC(21,IP)*S(IPNP(21,IP))
  700     CONTINUE
          DO 800 IP = 1 , NP
              DF(IP) = DF(IP)
     &                +AC(22,IP)*S(IPNP(22,IP))+AC(23,IP)*S(IPNP(23,IP))
     &                +AC(24,IP)*S(IPNP(24,IP))+AC(25,IP)*S(IPNP(25,IP))
     &                +AC(26,IP)*S(IPNP(26,IP))+AC(27,IP)*S(IPNP(27,IP))
  800     CONTINUE
          ENDIF
C
          DO 910 IPP = MINPP+1 , MAXPP
              DO 900 IP = 1 , NP
                  IF(IPP.LE.NPP(IP)) THEN
                      DF(IP) = DF(IP)
     &                        +AC(IPP,IP)*S(IPNP(IPP,IP))
                  ENDIF
  900         CONTINUE
  910     CONTINUE
C
C      SOLVE
C
          DO 1000 IP = 1 , NP
              S(IP) = S(IP)+CM(IP)*(F(IP)-DF(IP))
 1000     CONTINUE
C
C     GIVE DERICLET B.C.
C
C*$*ASSERT PERMUTATION ( LB )
          DO 1100 IB = 1 , NB
              S(LB(IB)) = 0.E0
 1100     CONTINUE
C
C      MULTIPASS LOOP END
C
 2000 CONTINUE
C
C
      RETURN
      END
