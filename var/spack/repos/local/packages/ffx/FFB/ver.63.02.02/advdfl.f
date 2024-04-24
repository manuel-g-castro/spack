      SUBROUTINE ADVDFL(N1,N2,NE,NP,MEP,MELM,NEX,NODE,
     *                  UG,VG,WG,DT,SCT,VISC,CEB,
     *                  EAP1,EAP2,EAP3,EBP,IENP,NODP,NEP,MP,
     *                  AP,A,X,RHS,AD,
     *                  DELTA,IERR)
      IMPLICIT NONE
C
C[INTPUT]
      INTEGER*4 N1,N2,NE,NP,MEP,MELM,NEX(12)
      INTEGER*4 NODE(N2,NE)
      INTEGER*4 IENP(MEP,MP),NODP(N2,MEP,NP),NEP(MP),MP
      REAL*4    UG(NE),VG(NE),WG(NE),DT
      REAL*4    VISC(NE),SCT,CEB,X(NP+1),DELTA(NE)
      REAL*4    EAP1(N2,MEP,NP),EAP2(3,N2,MEP,NP),EAP3(6,N2,MEP,NP)
      REAL*4    EBP(3,N2,MEP,NP),AP(N2,MEP,NP)
C
C[OUTPUT]
      REAL*4    A(N1,N2,NE),RHS(NP),AD(NP)
C
C[LOCAL]
      INTEGER*4 NETET,NEPRD,NEWED,NEHEX
      INTEGER*4 IE,IP,I,IERR,
     *          IEE1,IEE2,IEE3,IEE4,IPE,NN,K,IPK,
     *          IDIAGV,J
      REAL*4    UU,VV,WW,AT,DTH,UABS,
     *          ADI,ACI,ATI,AUPWDI,CRHSI
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
      REAL*4 COEF
C
C          SCT         ; SGS SCHMIDT NUMBER FOR VOLUMETRIC FRACTION
C
      X(NP+1)=0.0E0
C
      NETET =NEX(1)
      NEPRD =NEX(2)
      NEWED =NEX(3)
      NEHEX =NEX(4)
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
      DO 100 IP=1,NP
        AD (IP)=0.0E0
        RHS(IP)=0.0E0
  100 CONTINUE
C
      DTH = DT*0.5E0
C
      DO 1040 IP=1,NP
          CRHSI = 0.0E0
          DO 1030 IPE=1,NEP(IP)
              IE =IENP(IPE,IP)
              UU=UG(IE)
              VV=VG(IE)
              WW=WG(IE)
              UABS=SQRT(UU*UU+VV*VV+WW*WW+1.0E-10)
              COEF= VISC(IE)*DTH/SCT 
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
                  ATI = AT*DIJ(K,I)
C
CC [HEX. COVECTION-TERM]
                  ACI =(UU*EAP2(1,K,IPE,IP)+VV*EAP2(2,K,IPE,IP)
     *                 +WW*EAP2(3,K,IPE,IP))*DTH
C
CC [HEX. UPWIND-TERM]
                  AUPWDI = DTH*CUPWD
     *               *(UU*(UU*EAP3(1,K,IPE,IP)+VV*EAP3(4,K,IPE,IP)
     *                +WW*EAP3(6,K,IPE,IP))
     *                +VV*(UU*EBP(1,K,IPE,IP)+VV*EAP3(2,K,IPE,IP)
     *                +WW*EAP3(5,K,IPE,IP))
     *                +WW*(UU*EBP(3,K,IPE,IP)+VV*EBP(2,K,IPE,IP)
     *                +WW*EAP3(3,K,IPE,IP)))*DTH
                  AUPWDI = AUPWDI*(DELTA(IE)**0.33333E0)/(UABS*DT)
C
CC [HEX. DIFFUSION-TERM]

                  ADI = (EAP3(1,K,IPE,IP)+EAP3(2,K,IPE,IP)
     *                  +EAP3(3,K,IPE,IP))*COEF
C
                  AP(K,IPE,IP) = ATI+ACI*(1.0E0+CEB)+ADI*(1.0E0+CEB)
     *                           +AUPWDI
C
                  IPK=NODP(K,IPE,IP)
                  CRHSI = CRHSI 
     *             + (ATI-ACI*(1.0E0-CEB)-ADI*(1.0E0-CEB)-AUPWDI)*X(IPK)
C
 1020         CONTINUE
 1030     CONTINUE
C
          RHS(IP) = CRHSI
          AD(IP)=AD(IP)+AP(I,IPE,IP)
 1040 CONTINUE   
C
      RETURN
      END

