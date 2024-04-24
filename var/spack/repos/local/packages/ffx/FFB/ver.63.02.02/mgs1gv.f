      SUBROUTINE MGS1GV(NP,NS,XM,YM,WEIGHT)
C
      IMPLICIT NONE
C
C   INPUTS
      INTEGER*4 NP,NS
      REAL*4 XM(NP,NS),WEIGHT(NP)
C      
C   OUTPUTS
      REAL*4 YM(NS,NP)
C
C   WORKS
      INTEGER*4 I,J,IP
      REAL*4 U(NP),V(NP)
      REAL*4 SIGII,SIGIJ,SBUF(2),RBUF(2),ZZ
      INTEGER*4 IERR
C
C     
      DO 100 J=1, NS
          DO 110 IP=1, NP
              YM(J,IP) = XM(IP,J)
 110      CONTINUE
C
          DO 200 I=1, J-1
              DO 210 IP=1, NP
                  U(IP) = YM(I,IP)
                  V(IP) = YM(J,IP)
 210          CONTINUE
C
              CALL DOTWVC(NP,U,V,WEIGHT,SIGIJ)
              CALL DOTWVC(NP,U,U,WEIGHT,SIGII)
C
              DO 220 IP=1, NP
                  YM(J,IP) = YM(J,IP) - (SIGIJ / SIGII) * YM(I,IP)
 220          CONTINUE
 200      CONTINUE
C
          DO 120 IP=1, NP
              U(IP) = YM(J,IP)
 120      CONTINUE
C
          CALL DOTWVC(NP,U,U,WEIGHT,ZZ)
C
          DO 130 IP=1, NP
              YM(J,IP) = YM(J,IP) / SQRT(ZZ)
 130      CONTINUE
C
 100  CONTINUE
C
      RETURN
      END
C
