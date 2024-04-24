C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    SUPUE2                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE SUPUE2(IDIM,R,IENP,JENP,NEP,MAXE,MAXEP,NE,NP,N,
     *                  F,IUT0,IERR)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION R(N,MAXE),IENP(MAXEP,NP),JENP(MAXEP,NP),NEP(NP),F(NP)
C
      CHARACTER*60 ERMSGB
     & / ' ## SUBROUTINE SUPUE2: FATAL      ERROR OCCURENCE; RETURNED' /
      CHARACTER*60 EREXP1
     & / ' NO ROOM LEFT IN ARRAY R(ME) FOR DOING SUPUE2 OPERATIONS   ' /
      CHARACTER*60 EREXP2
     & / ' MAXEP IS TOO SMALL TO DO SUPUE2 OPERATIONS                ' /
C
C
C      SUPERPOSE ELEMENT'S FORCES VECTOR TO THE GLOBAL FORCE VECTOR
C         ( 3-D CALCULATION : SINGLE WORD VERSION )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          IDIM        ; SPECIFIES THE SPACE DIMENSION AS FOLLOWS
C                   2 --- TWO   DIMENSIONAL PROBLEM
C                   3 --- THREE DIMENSIONAL PROBLEM
C          R     (I,IE); ELEMENT FORCE VECTOR
C          IENP(IEP,IP); ADJACENT ELEMENT NUMBER TO NODE IP
C                      ( IF NEP(IP).LT.MINEP , THEN IENP(NEP(IP)+1,IP),
C                       IENP(MINEP,IP) MUST BE SET TO AN IMAGINARY
C                       ELEMENT NO. BETWEEN NE+1,MAXE.)
C          JENP(IEP,IP); LOCAL NODE NUMBER OF IP IN ELEMENT IENP(IEP,IP)
C          NEP     (IP); NUMBER OF ADJACENT ELEMENTS TO NODE IP
C          MAXE        ; THE SECOND DIMENSION OF ARRAY R
C          MAXEP       ; THE FIRST  DIMENSION OF ARRAY IENP,JENP
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          NP          ; NUMBER OF TOTAL    NODES
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT
C          IUT0        ; FILE NUMBER TO REPORT ERROR OCCURENCE
C
C       (2) OUTPUT
C          F       (IP); GLOBAL FORCE VECTOR
C          IERR        ; RETURN CODE TO REPORT ERROR OCCURENCE
C                   0 --- NORMAL TERMINATION
C                   1 --- A FATAL ERROR HAS OCCURED
C
C
      IF(IDIM.EQ.2) THEN
          MINEP = 4
      ELSE
          MINEP = 8
      ENDIF
C
      IERR = 0
C
      IF(MAXE.LE.NE) THEN
          WRITE(IUT0,*) ERMSGB
          WRITE(IUT0,*) EREXP1
          IERR = 1
          RETURN
      ENDIF
C
      IF(MAXEP.LT.MINEP) THEN
          WRITE(IUT0,*) ERMSGB
          WRITE(IUT0,*) EREXP2
          IERR = 1
          RETURN
      ENDIF
C
      DO 100 IE = NE+1 , MAXE
          R(1,IE) = 0.E0
          R(2,IE) = 0.E0
          R(3,IE) = 0.E0
          R(4,IE) = 0.E0
  100 CONTINUE
C
      IF(IDIM.EQ.3) THEN
          DO 200 IE = NE+1 , MAXE
              R(5,IE) = 0.E0
              R(6,IE) = 0.E0
              R(7,IE) = 0.E0
              R(8,IE) = 0.E0
  200     CONTINUE
      ENDIF
C
      DO 300 IP = 1 , NP
          F(IP) = R(JENP(1,IP),IENP(1,IP))
     &           +R(JENP(2,IP),IENP(2,IP))
     &           +R(JENP(3,IP),IENP(3,IP))
     &           +R(JENP(4,IP),IENP(4,IP))
  300 CONTINUE
C
      IF(IDIM.EQ.3) THEN
          DO 400 IP = 1 , NP
              F(IP) = F(IP)
     &               +R(JENP(5,IP),IENP(5,IP))
     &               +R(JENP(6,IP),IENP(6,IP))
     &               +R(JENP(7,IP),IENP(7,IP))
     &               +R(JENP(8,IP),IENP(8,IP))
  400     CONTINUE
      ENDIF
C
      DO 510 IEP = MINEP+1 , MAXEP
          DO 500 IP = 1 , NP
              IF(IEP.LE.NEP(IP)) THEN
                  F(IP) = F(IP)+R(JENP(IEP,IP),IENP(IEP,IP))
              ENDIF
  500     CONTINUE
  510 CONTINUE
C
C
      RETURN
      END
