      SUBROUTINE MATMLW(NP,NS,XM,Y,XMY,WEIGHT)
C
      IMPLICIT NONE
C
C   INPUTS
      INTEGER*4 NP,NS
      REAL*4    WEIGHT(NP),XM(NS,NP),Y(NP)
C
C   OUTPUTS
      REAL*4 XMY(NS)
C
C   WORKS
      INTEGER*4 IS,IP
      REAL*4    BUF
C
C
C
      DO 100 IS=1, NS
          BUF = 0.0E0
          DO 200 IP = 1, NP
              BUF = BUF + XM(IS,IP) * Y(IP) * WEIGHT(IP)
 200      CONTINUE
          XMY(IS) = BUF
 100  CONTINUE
C
      RETURN
      END
C
