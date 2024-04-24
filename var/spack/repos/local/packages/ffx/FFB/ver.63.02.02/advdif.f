      SUBROUTINE ADVDIF(N1,N2,NE,NP,MELM,NEX,NODE,IDIAGV,
     *                  UG,VG,WG,DT,VISC,CEB,
     *                  EAP1,EAP2,EAP3,EBP,AP,MEP,MP,IENP,JENP,NEP,
     *                  A,X,RHS,AD,
     *                  AR,RHOCP,DELTA,IERR)
      IMPLICIT NONE
      INTEGER*4 N1,N2,NE,NP,MELM,NEX(12)
      INTEGER*4 NODE(N2,NE)
      REAL*4    UG(NE),VG(NE),WG(NE),DT
      REAL*4    VISC(NE),CEB
      REAL*4    EAP1(N2,MEP,NP),EAP2(3,N2,MEP,NP),EAP3(6,N2,MEP,NP)
      REAL*4    EBP(3,N2,MEP,NP),AP(N2,MEP,NP)
      INTEGER*4 MEP,MP
      INTEGER*4 IENP(MEP,MP),JENP(MEP,MP),NEP(MP)
      REAL*4    A(N1,N2,NE),X(NP),RHS(NP),AD(NP),AR(NP)
      REAL*4    RHOCP(NE),DELTA(NE)
C
      INTEGER*4 NETET,NEPRD,NEWED,NEHEX
      INTEGER*4 NTET,NPRD,NWED,NHEX
      INTEGER*4 NSKIP1,NSKIP2,NSKIP3,NSKIP4
      INTEGER*4 IE,IP,IPE,I,J,K,IERR,
     *          IEE1,IEE2,IEE3,IEE4,
     *          IDIAGV,NN
      REAL*4    UU,VV,WW,AT,DTH,UABS,
     *          AD1,AC1,AT1,AUPWD1,CRHS1
C 
      REAL*4    DIJ(8,8)
      DATA DIJ /1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     &          0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     &          0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     &          0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0,
     &          0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0,
     &          0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0,
     &          0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0,
     &          0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0/
C
      REAL*4 CUPWD
      DATA CUPWD /1.0E0/
C
      NETET =NEX( 1)
      NEPRD =NEX( 2)
      NEWED =NEX( 3)
      NEHEX =NEX( 4)
      NTET  =NEX( 5)
      NPRD  =NEX( 6)
      NWED  =NEX( 7)
      NHEX  =NEX( 8)
      NSKIP1=NEX( 9)
      NSKIP2=NEX(10)
      NSKIP3=NEX(11)
      NSKIP4=NEX(12)
C
C   == TET. ==  
      IEE1=NETET 
C
C   == PYRAMID ==  
      IEE2=NETET+NEPRD
C
C   == WEDGE ==  
      IEE3=NETET+NEPRD+NEWED
C
C   == HEX. ==  
      IEE4=NETET+NEPRD+NEWED+NEHEX 
C
      DTH = DT*0.5E0
C
      DO 100 IP=1,NP
          AD (IP)=0.0E0
          AR(IP)=0.0E0
          RHS(IP)=0.0E0
  100 CONTINUE
C
      DO 1040 IP=1,NP
          DO 1030 IPE=1,NEP(IP)
              IE =IENP(IPE,IP)
              I  =JENP(IPE,IP)
              UU =UG(IE)
              VV =VG(IE)
              WW =WG(IE)
              UABS =SQRT(UU*UU+VV*VV+WW*WW+1.0E-10)
C
              IF(IE.LE.IEE1) THEN
                  NN = NEX(5)
              ELSE IF (IE.LE.IEE2) THEN
                  NN = NEX(6)
              ELSE IF (IE.LE.IEE3) THEN
                  NN = NEX(7)
              ELSE
                  NN = NEX(8)
              ENDIF
C
              AT = 0.0E0
              DO 1010 K=1,NN
                  AT = AT + EAP1(K,IPE,IP)
 1010         CONTINUE
C
              DO 1020 K=1,NN
C
CC [HEX. TIME-TERM]
                  AT  = AT*RHOCP(IE)
                  AT1 = AT*DIJ(K,1)
C
CC [HEX. COVECTION-TERM]
                  AC1=(UU*EAP2(1,K,IPE,IP)+VV*EAP2(2,K,IPE,IP)
     *                +WW*EAP2(3,K,IPE,IP))*DTH
                  AC1 = AC1*RHOCP(IE)
C
CC [HEX. UPWIND-TERM]
                  AUPWD1 = DTH*CUPWD
     *              *(UU*(UU*EAP3(1,K,IPE,IP)+VV*EAP3(2,K,IPE,IP)
     *               +WW*EAP3(3,K,IPE,IP))
     *               +VV*(UU*EBP (1,K,IPE,IP)+VV*EAP3(2,K,IPE,IP)
     *                   +WW*EAP3(5,K,IPE,IP))
     *               +WW*(UU*EBP (3,K,IPE,IP)+VV*EBP (2,K,IPE,IP)
     *                   +WW*EAP3(3,K,IPE,IP)))*DTH
                  AUPWD1 = AUPWD1*RHOCP(IE)*(DELTA(IE)**0.33333E0)
     *                     /(UABS*DT)
C
CC [HEX. DIFFUSION-TERM]
                  AD1 = (EAP3(1,K,IPE,IP)+EAP3(2,K,IPE,IP)
     *                  +EAP3(3,K,IPE,IP))*VISC(IE)*DTH
C
                  AP(K,IPE,IE) = AT1+AC1*(1.0E0+CEB)+AD1*(1.0E0+CEB)
     *                           +AUPWD1
C
                  CRHS1 = AT1-AC1*(1.0E0-CEB)-AD1*(1.0E0-CEB)-AUPWD1
C
                  RHS(IP)=RHS(IP)+CRHS1*X(IP)
 1020         CONTINUE
 1030     CONTINUE
          AD(IP)=AD(IP)+AP(I,IPE,IP)
 1040 CONTINUE   
C
      DO 5000 IP=1,NP
          AR(IP)=AD(IP)
 5000 CONTINUE
C
      RETURN
      END

