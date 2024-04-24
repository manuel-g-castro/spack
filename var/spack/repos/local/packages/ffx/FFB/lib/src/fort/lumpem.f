C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    LUMPEM                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE LUMPEM(IDIM,E,IENP,JENP,NEP,MAXEP,NP,N,CM)
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*4   E,CM
      DIMENSION E(N,N,*),IENP(MAXEP,NP),JENP(MAXEP,NP),NEP(NP),CM(NP)
C
C
C      LUMP ELEMENT MATRIX
C         ( 2-D , 3-D CALCULATION )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          IDIM        ; SPECIFIES THE SPACE DIMENSION AS FOLLOWS
C                   2 --- TWO   DIMENSIONAL PROBLEM
C                   3 --- THREE DIMENSIONAL PROBLEM
C          E   (J,I,IE); ELEMENT MATRIX
C          IENP(IEP,IP); ADJACENT ELEMENT NUMBER TO NODE IP
C          JENP(IEP,IP); LOCAL NODE NUMBER OF IP IN ELEMENT IENP(IEP,IP)
C          NEP     (IP); NUMBER OF ADJACENT ELEMENTS TO NODE IP
C          MAXEP       ; THE FIRST DIMENSION OF ARRAY IENP,JENP
C          NP          ; NUMBER OF TOTAL    NODES
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT
C
C       (2) OUTPUT
C          CM(IP)      ; GLOBAL LUMPED MATRIX ( INVERSED )
C
C
C      CLEAR ARRAY
C
      DO 100 IP = 1 , NP
          CM(IP) = 0.D0
  100 CONTINUE
C
C      LUMP MATRIX
C
      DO 310 IEP = 1 , MAXEP
C
          IF(IDIM.EQ.2) THEN
              DO 200 IP = 1 , NP
                  IF(NEP(IP).GE.IEP) THEN
                      CM(IP) = CM(IP)+E(1,JENP(IEP,IP),IENP(IEP,IP))
     &                               +E(2,JENP(IEP,IP),IENP(IEP,IP))
     &                               +E(3,JENP(IEP,IP),IENP(IEP,IP))
     &                               +E(4,JENP(IEP,IP),IENP(IEP,IP))
                  ENDIF
  200         CONTINUE
          ELSE
              DO 300 IP = 1 , NP
                  IF(NEP(IP).GE.IEP) THEN
                      CM(IP) = CM(IP)+E(1,JENP(IEP,IP),IENP(IEP,IP))
     &                               +E(2,JENP(IEP,IP),IENP(IEP,IP))
     &                               +E(3,JENP(IEP,IP),IENP(IEP,IP))
     &                               +E(4,JENP(IEP,IP),IENP(IEP,IP))
     &                               +E(5,JENP(IEP,IP),IENP(IEP,IP))
     &                               +E(6,JENP(IEP,IP),IENP(IEP,IP))
     &                               +E(7,JENP(IEP,IP),IENP(IEP,IP))
     &                               +E(8,JENP(IEP,IP),IENP(IEP,IP))
                  ENDIF
  300         CONTINUE
          ENDIF
  310 CONTINUE
C
C      (3) INVERSE LUMPED MATRIX
C
      DO 400 IP = 1 , NP
          CM(IP) = 1.D0/CM(IP)
  400 CONTINUE
C
C
      RETURN
      END
