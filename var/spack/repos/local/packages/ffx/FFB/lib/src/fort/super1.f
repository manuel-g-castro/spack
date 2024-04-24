C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    SUPER1                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE SUPER1(IDIM,VELM,SN,IENP,JENP,NEP,MAXE,MAXEP,NE,NP,N,
     *                  F,IUT0,IERR)
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*4 SN
      DIMENSION VELM(NE),SN(N,NE),
     1          IENP(MAXEP,NP),JENP(MAXEP,NP),NEP(NP),F(NP)
C
      CHARACTER*72 ERMSG
     & /' *** SUBROUTINE SUPER1 REPORTS A FATAL ERROR OCCURENCE ***' /
      CHARACTER*72 EREXP1
     & /' NO ROOM LEFT IN VELM(MAXE) FOR CONDUCTING SUPER1 OPERATIONS' /
      CHARACTER*72 EREXP2
     & /' MAXEP IS TOO SMALL TO CONDUCT SUPER1 OPERATIONS' /
C
C
C      SUPERPOSE ELEMENT'S ASSIGNED VALUES TO THE GLOBAL FORCE VECTOR
C         ( 2-D , 3-D CALCULATION )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          IDIM        ; SPECIFIES THE SPACE DIMENSION AS FOLLOWS
C                   2 --- TWO   DIMENSIONAL PROBLEM
C                   3 --- THREE DIMENSIONAL PROBLEM
C          VELM    (IE); ELEMENT ASSIGNED VARIABLE
C          SN    (I,IE); INTEGRATED ELEMENT VECTOR
C          IENP(IEP,IP); ADJACENT ELEMENT NUMBER TO NODE IP
C                      ( IF NEP(IP).LT.MINEP , THEN IENP(NEP(IP)+1,IP),
C                       IENP(MINEP,IP) MUST BE SET TO AN IMAGINARY
C                       ELEMENT NO. BETWEEN NE+1,MAXE.)
C          JENP(IEP,IP); LOCAL NODE NUMBER OF IP IN ELEMENT IENP(IEP,IP)
C          NEP     (IP); NUMBER OF ADJACENT ELEMENTS TO NODE IP
C          MAXE        ; THE       DIMENSION OF ARRAY VELM
C          MAXEP       ; THE FIRST DIMENSION OF ARRAY IENP,JENP
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          NP          ; NUMBER OF TOTAL    NODES
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT
C          IUT0        ; DEVICE NO. TO REPORT ERROR OCCURENCE
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
          WRITE(IUT0,6300) ERMSG
          WRITE(IUT0,6300) EREXP1 
          IERR = 1 
          RETURN
      ENDIF
C
      IF(MAXEP.LT.MINEP) THEN
          WRITE(IUT0,6300) ERMSG
          WRITE(IUT0,6300) EREXP2 
          IERR = 1 
          RETURN
      ENDIF
C
      DO 100 IE = NE+1 , MAXE
          VELM(IE) = 0.D0
  100 CONTINUE
C
      DO 200 IP = 1 , NP
          F(IP) = VELM(IENP(1,IP))*SN(JENP(1,IP),IENP(1,IP))
     &           +VELM(IENP(2,IP))*SN(JENP(2,IP),IENP(2,IP))
     &           +VELM(IENP(3,IP))*SN(JENP(3,IP),IENP(3,IP))
     &           +VELM(IENP(4,IP))*SN(JENP(4,IP),IENP(4,IP))
  200 CONTINUE
C
      IF(IDIM.EQ.3) THEN
          DO 300 IP = 1 , NP
              F(IP) = F(IP)
     &               +VELM(IENP(5,IP))*SN(JENP(5,IP),IENP(5,IP))
     &               +VELM(IENP(6,IP))*SN(JENP(6,IP),IENP(6,IP))
     &               +VELM(IENP(7,IP))*SN(JENP(7,IP),IENP(7,IP))
     &               +VELM(IENP(8,IP))*SN(JENP(8,IP),IENP(8,IP))
  300     CONTINUE
      ENDIF
C
      DO 410 IEP = MINEP+1 , MAXEP
          DO 400 IP = 1 , NP
              IF(NEP(IP).GE.IEP) THEN
                  F(IP) = F(IP)
     &                 +VELM(IENP(IEP,IP))*SN(JENP(IEP,IP),IENP(IEP,IP))
              ENDIF
  400     CONTINUE
  410 CONTINUE
C
C
      RETURN
 6300 FORMAT(A72)
      END
