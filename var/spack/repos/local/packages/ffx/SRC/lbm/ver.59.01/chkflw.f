      SUBROUTINE CHKFLW(NG,NG3,NP,CVEL,F,RHOMAX,VELMAX)
      IMPLICIT NONE
      INTEGER*4 NG,NG3,NP
C
      REAL*8 CVEL(3,NP),F(NG3,NP),
     *       RHOMAX,VELMAX
C  
      INTEGER*4 IG,IP
      REAL*8    RHO,U0,V0,W0,UU,FBUF
CCCC
CCCC
      CALL USTSTA(21)
C
      RHOMAX=0.0D0
      VELMAX=0.0D0
C
      DO 1000 IG=1,NG3
          RHO=0.0E0
          U0 =0.0E0
          V0 =0.0E0
          W0 =0.0E0
C
          FBUF=F(IG,1)
          RHO=RHO+FBUF
C
          FBUF=F(IG,2)
          RHO=RHO+FBUF
          U0 =U0 +FBUF
C
          FBUF=F(IG,3)
          RHO=RHO+FBUF
          V0 =V0 +FBUF
C
          FBUF=F(IG,4)
          RHO=RHO+FBUF
          U0 =U0 -FBUF
C
          FBUF=F(IG,5)
          RHO=RHO+FBUF
          V0 =V0 -FBUF
C
          FBUF=F(IG,6)
          RHO=RHO+FBUF
          W0 =W0 +FBUF
C
          FBUF=F(IG,7)
          RHO=RHO+FBUF
          W0 =W0 -FBUF
C
          FBUF=F(IG,8)
          RHO=RHO+FBUF
          U0 =U0 +FBUF
          V0 =V0 +FBUF
          W0 =W0 +FBUF
C
          FBUF=F(IG,9)
          RHO=RHO+FBUF
          U0 =U0 -FBUF
          V0 =V0 +FBUF
          W0 =W0 +FBUF
C
          FBUF=F(IG,10)
          RHO=RHO+FBUF
          U0 =U0 -FBUF
          V0 =V0 -FBUF
          W0 =W0 +FBUF
C
          FBUF=F(IG,11)
          RHO=RHO+FBUF
          U0 =U0 +FBUF
          V0 =V0 -FBUF
          W0 =W0 +FBUF
C
          FBUF=F(IG,12)
          RHO=RHO+FBUF
          U0 =U0 +FBUF
          V0 =V0 +FBUF
          W0 =W0 -FBUF
C
          FBUF=F(IG,13)
          RHO=RHO+FBUF
          U0 =U0 -FBUF
          V0 =V0 +FBUF
          W0 =W0 -FBUF
C
          FBUF=F(IG,14)
          RHO=RHO+FBUF
          U0 =U0 -FBUF
          V0 =V0 -FBUF
          W0 =W0 -FBUF
C
          FBUF=F(IG,15)
          RHO=RHO+FBUF
          U0 =U0 +FBUF
          V0 =V0 -FBUF
          W0 =W0 -FBUF
C
          IF(NP.EQ.15) GOTO 1100
C
          FBUF=F(IG,16)
          RHO=RHO+FBUF
          U0 =U0 +FBUF
          V0 =V0 +FBUF
C
          FBUF=F(IG,17)
          RHO=RHO+FBUF
          U0 =U0 -FBUF
          V0 =V0 +FBUF
C
          FBUF=F(IG,18)
          RHO=RHO+FBUF
          U0 =U0 -FBUF
          V0 =V0 -FBUF
C
          FBUF=F(IG,19)
          RHO=RHO+FBUF
          U0 =U0 +FBUF
          V0 =V0 -FBUF
C
          FBUF=F(IG,20)
          RHO=RHO+FBUF
          U0 =U0 +FBUF
          W0 =W0 +FBUF
C
          FBUF=F(IG,21)
          RHO=RHO+FBUF
          U0 =U0 -FBUF
          W0 =W0 +FBUF
C
          FBUF=F(IG,22)
          RHO=RHO+FBUF
          U0 =U0 -FBUF
          W0 =W0 -FBUF
C
          FBUF=F(IG,23)
          RHO=RHO+FBUF
          U0 =U0 +FBUF
          W0 =W0 -FBUF
C
          FBUF=F(IG,24)
          RHO=RHO+FBUF
          V0 =V0 +FBUF
          W0 =W0 +FBUF
C
          FBUF=F(IG,25)
          RHO=RHO+FBUF
          V0 =V0 -FBUF
          W0 =W0 +FBUF
C
          FBUF=F(IG,26)
          RHO=RHO+FBUF
          V0 =V0 -FBUF
          W0 =W0 -FBUF
C
          FBUF=F(IG,27)
          RHO=RHO+FBUF
          V0 =V0 +FBUF
          W0 =W0 -FBUF
C
 1100     CONTINUE
C
          U0 =U0/RHO
          V0 =V0/RHO
          W0 =W0/RHO
          UU =U0*U0+V0*V0+W0*W0
          RHOMAX=MAX(RHOMAX,RHO)
          VELMAX=MAX(VELMAX,UU )
 1000 CONTINUE
      IF(VELMAX.NE.0.0D0) VELMAX=SQRT(VELMAX)
      CALL USTEND(21)
C
      RETURN
      END
