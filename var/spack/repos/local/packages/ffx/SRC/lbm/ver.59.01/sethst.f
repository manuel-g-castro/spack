      SUBROUTINE SETHST(MSAMPL,MRESV,NSAMPL,LSAMPL,CSAMPL,NG,NC,NP,
     *                  CVEL,F,TIME,RHOMAX,VELMAX,VSCALE,DSCALE,HIST,
     *                  FX,FY,FZ)
      IMPLICIT NONE
      INTEGER*4 MSAMPL,MRESV,NSAMPL,LSAMPL(5,NSAMPL),NG,NC,NP
      REAL*8    CSAMPL(3,NSAMPL)
      REAL*8    CVEL(3,NP),F(0:NG+2,0:NG+2,0:NG+2,NP,NC)
      REAL*8    TIME,RHOMAX,VELMAX,VSCALE,DSCALE
      REAL*4    HIST(MSAMPL)
      REAL*4    FX,FY,FZ,COEFF
C
      INTEGER*4 ISAMPL,IVAR,IC,I,J,K,IP,II,JJ,KK,IG
      REAL*8    RHO0(8),U0(8),V0(8),W0(8),RHO1,U1,V1,W1,FBUF,BUF
      REAL*8    COEF(8),DX,DY,DZ,C1X,C2X,C1Y,C2Y,C1Z,C2Z
C
      COEFF = REAL(DSCALE)/(REAL(VSCALE)*REAL(NG))
      COEFF = COEFF*COEFF
C
      HIST(1)=REAL(TIME)
      HIST(2)=REAL(RHOMAX)
      HIST(3)=REAL(VELMAX/VSCALE)
      HIST(4)=FX*COEFF
      HIST(5)=FY*COEFF
      HIST(6)=FZ*COEFF
      IF(NSAMPL.EQ.0) RETURN
C
      DO 1000 ISAMPL=1,NSAMPL
          IVAR=LSAMPL(1,ISAMPL)
          IC  =LSAMPL(2,ISAMPL)
          I   =LSAMPL(3,ISAMPL)
          J   =LSAMPL(4,ISAMPL)
          K   =LSAMPL(5,ISAMPL)
C
          DX=CSAMPL(1,ISAMPL)
          DY=CSAMPL(2,ISAMPL)
          DZ=CSAMPL(3,ISAMPL)
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
          DO 1100 KK=0,1
          DO 1200 JJ=0,1
          DO 1300 II=0,1
              IG=IG+1
              RHO0(IG)=0.0D0
              U0  (IG)=0.0D0
              V0  (IG)=0.0D0
              W0  (IG)=0.0D0
              DO 1400 IP=1,NP
                  FBUF=F(I+II,J+JJ,K+KK,IP,IC)
                  RHO0(IG)=RHO0(IG)+FBUF
                  U0  (IG)=U0  (IG)+FBUF*CVEL(1,IP)
                  V0  (IG)=V0  (IG)+FBUF*CVEL(2,IP)
                  W0  (IG)=W0  (IG)+FBUF*CVEL(3,IP)
 1400         CONTINUE
              U0(IG)=U0(IG)/RHO0(IG) 
              V0(IG)=V0(IG)/RHO0(IG) 
              W0(IG)=W0(IG)/RHO0(IG) 
 1300     CONTINUE
 1200     CONTINUE
 1100     CONTINUE
C
          RHO1 = COEF(1)*RHO0(1)+COEF(2)*RHO0(2)
     *          +COEF(3)*RHO0(3)+COEF(4)*RHO0(4)
     *          +COEF(5)*RHO0(5)+COEF(6)*RHO0(6)
     *          +COEF(7)*RHO0(7)+COEF(8)*RHO0(8)
          U1   = COEF(1)*U0(1)+COEF(2)*U0(2)
     *          +COEF(3)*U0(3)+COEF(4)*U0(4)
     *          +COEF(5)*U0(5)+COEF(6)*U0(6)
     *          +COEF(7)*U0(7)+COEF(8)*U0(8)
          V1   = COEF(1)*V0(1)+COEF(2)*V0(2)
     *          +COEF(3)*V0(3)+COEF(4)*V0(4)
     *          +COEF(5)*V0(5)+COEF(6)*V0(6)
     *          +COEF(7)*V0(7)+COEF(8)*V0(8)
          W1   = COEF(1)*W0(1)+COEF(2)*W0(2)
     *          +COEF(3)*W0(3)+COEF(4)*W0(4)
     *          +COEF(5)*W0(5)+COEF(6)*W0(6)
     *          +COEF(7)*W0(7)+COEF(8)*W0(8)
C
          IF(IVAR.EQ.1) BUF=RHO1
          IF(IVAR.EQ.2) BUF=U1/VSCALE
          IF(IVAR.EQ.3) BUF=V1/VSCALE
          IF(IVAR.EQ.4) BUF=W1/VSCALE
C
          HIST(MRESV+ISAMPL)=REAL(BUF)
 1000 CONTINUE   
C
      RETURN  
      END
