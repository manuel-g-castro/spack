      SUBROUTINE SETSRF(NPSURF,LSURF,CSURF,NG,NC,NP,
     *                  RHO000,VSCALE,F,RHOSRF)
      IMPLICIT NONE
      INTEGER*4 NPSURF,LSURF(4,NPSURF),NG,NC,NP
      REAL*8    RHO000,VSCALE
      REAL*8    CSURF(3,NPSURF)
      REAL*8    F(0:NG+2,0:NG+2,0:NG+2,NP,NC)
      REAL*4    RHOSRF(NPSURF)
C
      INTEGER*4 ISURF,IC,I,J,K,IP,II,JJ,KK,IG
      REAL*8    RHO0(8),RHO1,FBUF,DRHO
      REAL*8    CSUM,COEF(8),DX,DY,DZ,C1X,C2X,C1Y,C2Y,C1Z,C2Z
      REAL*8    EPS
C
      DATA EPS /1.0E-4/
C
      DO 1000 ISURF=1,NPSURF
          IC  =LSURF(1,ISURF)
          I   =LSURF(2,ISURF)
          J   =LSURF(3,ISURF)
          K   =LSURF(4,ISURF)
C
          DX=CSURF(1,ISURF)
          DY=CSURF(2,ISURF)
          DZ=CSURF(3,ISURF)
          C1X=(1.0E0-DX)
          C2X=       DX
          C1Y=(1.0E0-DY)
          C2Y=       DY
          C1Z=(1.0E0-DZ)
          C2Z=       DZ
          COEF(1) = C1X*C1Y*C1Z
          COEF(2) = C2X*C1Y*C1Z
          COEF(3) = C1X*C2Y*C1Z
          COEF(4) = C2X*C2Y*C1Z
          COEF(5) = C1X*C1Y*C2Z
          COEF(6) = C2X*C1Y*C2Z
          COEF(7) = C1X*C2Y*C2Z
          COEF(8) = C2X*C2Y*C2Z
C
          IG=0
          CSUM=0.0D0
          DO 1100 KK=0,1
          DO 1200 JJ=0,1
          DO 1300 II=0,1
              IG=IG+1
              RHO0(IG)=0.0D0
              DO 1400 IP=1,NP
                  FBUF=F(I+II,J+JJ,K+KK,IP,IC)
                  RHO0(IG)=RHO0(IG)+FBUF
 1400         CONTINUE
              DRHO=RHO0(IG)-1.0D0
              IF(ABS(DRHO).LT.EPS) COEF(IG)=0.0D0
              CSUM=CSUM+COEF(IG)
 1300     CONTINUE
 1200     CONTINUE
 1100     CONTINUE
C
          IF(CSUM.EQ.0.0D0) THEN
              DO 1500 IG=1,8
                  COEF(IG)=1.0D0/8.0D0
 1500         CONTINUE
          ELSE
              DO 1510 IG=1,8
                  COEF(IG)=COEF(IG)/CSUM
 1510         CONTINUE
          ENDIF
C
          RHO1 = COEF(1)*RHO0(1)+COEF(2)*RHO0(2)
     *          +COEF(3)*RHO0(3)+COEF(4)*RHO0(4)
     *          +COEF(5)*RHO0(5)+COEF(6)*RHO0(6)
     *          +COEF(7)*RHO0(7)+COEF(8)*RHO0(8)
          RHOSRF(ISURF)=REAL(RHO1)
 1000 CONTINUE   
C
      RETURN  
      END
