      SUBROUTINE MATMLS(NP,NS,XM,Y,XMY)
C
      IMPLICIT NONE
C
C   INPUTS
      INTEGER*4 NS,NP
      REAL*4    XM(NP,NS),Y(NS)
C
C   OUTPUTS
      REAL*4    XMY(NP)
C
C   WORKS
      REAL*4    BUF
      INTEGER*4 IS,IP
C
C
C
      DO 100 IP=1, NP
          BUF = 0.0E0
          DO 200 IS = 1, NS
              BUF = BUF + XM(IP,IS) * Y(IS)
 200      CONTINUE
          XMY(IP) = BUF
 100  CONTINUE
C
      RETURN
      END
C
