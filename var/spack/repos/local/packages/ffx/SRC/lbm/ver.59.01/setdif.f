      SUBROUTINE SETDIF(IMODEL,ICALSS,NG,NP,LEVEL,VISCM,
     *                  CS,TAUMIN,WF,CVEL,
     *                  DSCALE,VSCALE,F,TAU,
     *                  LPOSI,NTFIX,LTFIX,TAUFIX,
     *                  LCBOUN,TAINLT,TAFREE,VAL3D,FEQ,CVISC,TAUTGT)
      IMPLICIT NONE
      INTEGER*4 IMODEL,ICALSS,ICNSTD,NG,NP,LEVEL,LPOSI(3),LCBOUN(4)
      REAL*8    VISCM,CS,TAUMIN,WF(NP),CVEL(3,NP),DSCALE,VSCALE
      REAL*8    TAINLT,TAFREE    
      REAL*8    F(0:NG+2,0:NG+2,0:NG+2,NP)
      REAL*8    TAU(0:NG+2,0:NG+2,0:NG+2) 
      REAL*8    VAL3D(4,0:NG+2,0:NG+2,0:NG+2)
      REAL*8    FEQ(0:NG+2,0:NG+2,0:NG+2,NP)
      REAL*8    CVISC(0:NG+2,0:NG+2,0:NG+2),TAUTGT
C
      INTEGER*4 NTFIX,LTFIX(2,NTFIX)
      REAL*8    TAUFIX(NTFIX) 
C
C  IMODEL=0: NON-SGS MODEL 
C  IMODEL=1: STANDARD SMAGORINSKY MODEL
C
C  ICALSS=0: CAL. STRAIN BY MACRO VARIAVLES
C  ICALSS=1: CAL. STRAIN BY DISRIBUTION FUNCTAION 
C
C  ICNSTD=0: USE LOCAL   GRID SCALE FOR COMPUTING TURBULENT VISCOCITY  
C  ICNSTD=1: USE MINIMUM GRID SCALE FOR COMPUTING TURBULENT VISCOCITY  
C
C  MTFIX       :MAX. NUMBER OF TAU-FIX INPUT
C  NTFIX       :     NUMBER OF TAU-FIX INPUT
C  LTFIX(1,I)  :SPECIFY DIRECTION OF TAU-FIX (1:X, 2:Y, 3:Z)
C  LTFIX(2,I)  :POSITION OF TAU-FIX CELL (1--> NC[X,Y,Z])
C  TAUFIX(I)   :RELAXATION COEF. TAU OF TAU-FIX CELL 
C
      INTEGER*4 I,J,K,IFIX,IDIR,IPOSI,IP
      REAL*8    DX,TAU0,VIS0,VISSGS,C1,C2
      REAL*8    RHO,FBUF,U0,V0,W0,UU,CU
      REAL*8    S11,S12,S13,S21,S22,S23,S31,S32,S33,SS,BUF,FNEQ
      REAL*8    P11,P12,P13,P21,P22,P23,P31,P32,P33,PP
      REAL*8    DUDX,DUDY,DUDZ,DVDX,DVDY,DVDZ,DWDX,DWDY,DWDZ
C      
      CALL USTSTA(22)
C
      DX=2.0E0**FLOAT(LEVEL)/FLOAT(NG) 
C
      VIS0= VISCM*(VSCALE/DSCALE)*DBLE(NG)/(2.0E0**FLOAT(LEVEL-1))
      TAU0= 0.5D0+3.0D0*VIS0
C     
      DO 1000 IFIX=1,NTFIX
          IDIR =LTFIX(1,IFIX)
          IPOSI=LTFIX(2,IFIX)
          IF(LPOSI(IDIR).EQ.IPOSI) TAU0=TAUFIX(IFIX)
 1000 CONTINUE    
C
      IF(LCBOUN(4).EQ.1 .AND. TAFREE.GT.0.5D0) TAU0=TAFREE
      IF(LCBOUN(2).EQ.1 .AND. TAINLT.GT.0.5D0) TAU0=TAINLT
C
      DO 1100 K =0,NG+2
      DO 1200 J =0,NG+2
      DO 1300 I =0,NG+2
          TAU(I,J,K)=TAU0
 1300 CONTINUE
 1200 CONTINUE
 1100 CONTINUE

      IF(IMODEL.EQ.1) THEN
C
          DO 2000 K =0,NG+2
          DO 2100 J =0,NG+2
          DO 2200 I =0,NG+2
              RHO=0.0D0
              U0 =0.0D0
              V0 =0.0D0
              W0 =0.0D0
              DO 2300 IP=1,NP
                  FBUF=F(I,J,K,IP)
                  RHO=RHO+FBUF
                  U0 =U0 +FBUF*CVEL(1,IP)
                  V0 =V0 +FBUF*CVEL(2,IP)
                  W0 =W0 +FBUF*CVEL(3,IP)
 2300         CONTINUE
              U0=U0/RHO 
              V0=V0/RHO 
              W0=W0/RHO 
              VAL3D(1,I,J,K)=RHO
              VAL3D(2,I,J,K)=U0
              VAL3D(3,I,J,K)=V0
              VAL3D(4,I,J,K)=W0
C
              IF(ICALSS.EQ.1) THEN
              DO 2400 IP=1,NP
                  CU= U0*CVEL(1,IP)+V0*CVEL(2,IP)+W0*CVEL(3,IP)
                  UU=U0*U0+V0*V0+W0*W0
                  FEQ(I,J,K,IP)=WF(IP)*RHO*
     *            (1.0D0+3.0D0*CU+4.5D0*CU*CU-1.5D0*UU)
 2400         CONTINUE
              ENDIF
C
 2200     CONTINUE
 2100     CONTINUE
 2000     CONTINUE
C
          IF(ICALSS.EQ.0) THEN
              DO 3000 K =1,NG+1
              DO 3100 J =1,NG+1
              DO 3200 I =1,NG+1
                  DUDX=(VAL3D(2,I+1,J  ,K  )-VAL3D(2,I-1,J  ,K  ))/2.0D0
                  DUDY=(VAL3D(2,I  ,J+1,K  )-VAL3D(2,I  ,J-1,K  ))/2.0D0
                  DUDZ=(VAL3D(2,I  ,J  ,K+1)-VAL3D(2,I  ,J  ,K-1))/2.0D0
                  DVDX=(VAL3D(3,I+1,J  ,K  )-VAL3D(3,I-1,J  ,K  ))/2.0D0
                  DVDY=(VAL3D(3,I  ,J+1,K  )-VAL3D(3,I  ,J-1,K  ))/2.0D0
                  DVDZ=(VAL3D(3,I  ,J  ,K+1)-VAL3D(3,I  ,J  ,K-1))/2.0D0
                  DWDX=(VAL3D(4,I+1,J  ,K  )-VAL3D(4,I-1,J  ,K  ))/2.0D0
                  DWDY=(VAL3D(4,I  ,J+1,K  )-VAL3D(4,I  ,J-1,K  ))/2.0D0
                  DWDZ=(VAL3D(4,I  ,J  ,K+1)-VAL3D(4,I  ,J  ,K-1))/2.0D0
                  S11 =(DUDX+DUDX)/2.0D0 
                  S12 =(DUDY+DVDX)/2.0D0 
                  S13 =(DUDZ+DWDX)/2.0D0 
                  S21 =(DVDX+DUDY)/2.0D0 
                  S22 =(DVDY+DVDY)/2.0D0 
                  S23 =(DVDZ+DWDY)/2.0D0 
                  S31 =(DWDX+DUDZ)/2.0D0 
                  S32 =(DWDY+DVDZ)/2.0D0 
                  S33 =(DWDZ+DWDZ)/2.0D0 
                  SS=2.0D0*(S11*S11+S12*S12+S13*S13
     *                     +S21*S21+S22*S22+S23*S23
     *                     +S31*S31+S32*S32+S33*S33)
                  IF(SS .LT. 0.D0) THEN
                      SS = 0.D0
                  ELSE
                      SS = SQRT(SS)
                  ENDIF
                  VISSGS=CS*CS*SS
                  TAU(I,J,K)=TAU(I,J,K)+3.0D0*VISSGS
 3200         CONTINUE
 3100         CONTINUE
 3000         CONTINUE
          ELSE IF(ICALSS.EQ.1) THEN 
              DO 4000 K =0,NG+2
              DO 4100 J =0,NG+2
              DO 4200 I =0,NG+2
                  S11=0.0D0
                  S22=0.0D0
                  S33=0.0D0
                  S12=0.0D0
                  S13=0.0D0
                  S23=0.0D0
                  DO 4300 IP=1,NP
                      FNEQ=F(I,J,K,IP)-FEQ(I,J,K,IP)
                      BUF=(1.5D0/TAU(I,J,K))*FNEQ
                      S11=S11-BUF*CVEL(1,IP)*CVEL(1,IP)
                      S22=S22-BUF*CVEL(2,IP)*CVEL(2,IP)
                      S33=S33-BUF*CVEL(3,IP)*CVEL(3,IP)
                      S12=S12-BUF*CVEL(1,IP)*CVEL(2,IP)
                      S13=S13-BUF*CVEL(1,IP)*CVEL(3,IP)
                      S23=S23-BUF*CVEL(2,IP)*CVEL(3,IP)
 4300             CONTINUE
                  S21=S12
                  S31=S13
                  S32=S23
                  SS=2.0D0*(S11*S11+S12*S12+S13*S13
     *                     +S21*S21+S22*S22+S23*S23
     *                     +S31*S31+S32*S32+S33*S33)
                  IF(SS .LT. 0.D0) THEN
                      SS = 0.D0
                  ELSE
                      SS = SQRT(SS)
                  ENDIF
                  VISSGS=CS*CS*SS
                  TAU(I,J,K)=TAU(I,J,K)+3.0D0*VISSGS
 4200         CONTINUE
 4100         CONTINUE
 4000         CONTINUE
          ELSE
              DO 5000 K =0,NG+2
              DO 5100 J =0,NG+2
              DO 5200 I =0,NG+2
                  P11=0.0D0
                  P22=0.0D0
                  P33=0.0D0
                  P12=0.0D0
                  P13=0.0D0
                  P23=0.0D0
                  DO 5300 IP=1,NP
                      FNEQ=F(I,J,K,IP)-FEQ(I,J,K,IP)
                      P11=P11-FNEQ*CVEL(1,IP)*CVEL(1,IP)
                      P22=P22-FNEQ*CVEL(2,IP)*CVEL(2,IP)
                      P33=P33-FNEQ*CVEL(3,IP)*CVEL(3,IP)
                      P12=P12-FNEQ*CVEL(1,IP)*CVEL(2,IP)
                      P13=P13-FNEQ*CVEL(1,IP)*CVEL(3,IP)
                      P23=P23-FNEQ*CVEL(2,IP)*CVEL(3,IP)
 5300             CONTINUE
                  P21=P12
                  P31=P13
                  P32=P23
                  PP=2.0D0*(P11*P11+P12*P12+P13*P13
     *                     +P21*P21+P22*P22+P23*P23
     *                     +P31*P31+P32*P32+P33*P33)
                  IF(PP .LT. 0.D0) THEN
                      PP = 0.D0
                  ELSE
                      PP = SQRT(PP)
                  ENDIF
                  BUF=4.5D0*CS*CS*PP
                  TAU(I,J,K)=TAU0/2.0D0+(TAU0*TAU0/4.0D0+BUF)**0.5D0
 5200         CONTINUE
 5100         CONTINUE
 5000         CONTINUE
          ENDIF
      ENDIF
C
      IF(TAUMIN.GT.0.5D0) THEN
          DO 6000 K =0,NG+2
          DO 6100 J =0,NG+2
          DO 6200 I =0,NG+2
              TAU(I,J,K)=MAX(TAU(I,J,K),TAUMIN)
 6200     CONTINUE
 6100     CONTINUE
 6000     CONTINUE
      ENDIF
C
      DO 7000 K =0,NG+2
      DO 7100 J =0,NG+2
      DO 7200 I =0,NG+2
          C1=1.0D0-CVISC(I,J,K)
          C2=      CVISC(I,J,K)
          TAU(I,J,K)=C1*TAU(I,J,K)+C2*TAUTGT
 7200 CONTINUE
 7100 CONTINUE
 7000 CONTINUE
C
      CALL USTEND(22)
C
      RETURN
      END
