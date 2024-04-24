      SUBROUTINE MGSGVC(NP,NS,XM,YM,WEIGHT)
C
      IMPLICIT NONE
C
C   INPUTS
      INTEGER*4 NP,NS
      REAL*4    XM(NP,NS),WEIGHT(NP)
C      
C   OUTPUTS
      REAL*4 YM(NP,NS)
C
C   WORKS
      INTEGER*4 I,J,IP,IERR
      REAL*4 U(NP),V(NP)
      REAL*4 SIGII,SIGIJ,SBUF(2),RBUF(2)
C
C     
      DO 100 J=1, NS
          DO 110 IP=1, NP
              YM(IP,J) = XM(IP,J)
 110      CONTINUE
C
          DO 200 I=1, J-1
              DO 210 IP=1, NP
                  U(IP) = YM(IP,I)
                  V(IP) = YM(IP,J)
 210          CONTINUE
C
              CALL DOTWVC(NP,U,V,WEIGHT,SIGIJ)
              CALL DOTWVC(NP,U,U,WEIGHT,SIGII)
C
              DO 220 IP=1, NP
                  YM(IP,J) = YM(IP,J) - (SIGIJ / SIGII) * YM(IP,I)
 220          CONTINUE
 200      CONTINUE
 100  CONTINUE
C
      RETURN
      END
C
