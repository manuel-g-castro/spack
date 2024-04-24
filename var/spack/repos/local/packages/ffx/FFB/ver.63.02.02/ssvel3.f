      SUBROUTINE SSVEL3(N2,NEX,NP,NE,NODE,U,V,W,
     *                  NPASRC,LEASRC,COEAS1,COEAS2,COEAS3,
     *                  UASRC,VASRC,WASRC,
     *                  IUT0,IERR)
      IMPLICIT NONE 
C
C[INPUT]
      INTEGER*4 N2,NEX(8),NP,NE,NODE(N2,NE),
     *          NPASRC,LEASRC(NPASRC),IUT0
      REAL*4    U(NP),V(NP),W(NP),
     *          COEAS1(NPASRC),COEAS2(NPASRC),COEAS3(NPASRC)
C
C[LOCAL]
      INTEGER*4 IERR
      REAL*4    UASRC(NPASRC),VASRC(NPASRC),WASRC(NPASRC)
C
C[LOCAL]
      INTEGER*4 IPASRC,NHEX,NWED,NPRD,NTET,IE,
     *          IP1,IP2,IP3,IP4,IP5,IP6,IP7,IP8
      REAL*4    GP,EP,TP,T1,T2,T3,T4,T5,T6,T7,T8
C                 
      IERR=0
      NTET=NEX(5)
      NPRD=NEX(6) 
      NWED=NEX(7) 
      NHEX=NEX(8) 
C
      DO 1000 IPASRC=1,NPASRC
          IE = LEASRC(IPASRC)  
          GP = COEAS1(IPASRC)
          EP = COEAS2(IPASRC)
          TP = COEAS3(IPASRC)
C
          IF(NODE(NHEX,IE).NE.0) THEN
              T1=0.125E0*(1.0E0-GP)*(1.0E0-EP)*(1.0E0-TP)
              T2=0.125E0*(1.0E0+GP)*(1.0E0-EP)*(1.0E0-TP)
              T3=0.125E0*(1.0E0+GP)*(1.0E0+EP)*(1.0E0-TP)
              T4=0.125E0*(1.0E0-GP)*(1.0E0+EP)*(1.0E0-TP)
              T5=0.125E0*(1.0E0-GP)*(1.0E0-EP)*(1.0E0+TP)
              T6=0.125E0*(1.0E0+GP)*(1.0E0-EP)*(1.0E0+TP)
              T7=0.125E0*(1.0E0+GP)*(1.0E0+EP)*(1.0E0+TP)
              T8=0.125E0*(1.0E0-GP)*(1.0E0+EP)*(1.0E0+TP)
          ELSE IF(NODE(NWED,IE).NE.0) THEN
              T1=0.5E0*GP           *(1.0E0-TP)
              T2=0.5E0*EP           *(1.0E0-TP)
              T3=0.5E0*(1.0E0-GP-EP)*(1.0E0-TP)
              T4=0.5E0*GP           *(1.0E0+TP)
              T5=0.5E0*EP           *(1.0E0+TP)
              T6=0.5E0*(1.0E0-GP-EP)*(1.0E0+TP)
          ELSE IF(NODE(NPRD,IE).NE.0) THEN
              T1=0.25E0*((1.0E0-GP)*(1.0E0-EP)-TP+GP*EP*TP/(1.0E0-TP))
              T2=0.25E0*((1.0E0+GP)*(1.0E0-EP)-TP-GP*EP*TP/(1.0E0-TP))
              T3=0.25E0*((1.0E0+GP)*(1.0E0+EP)-TP+GP*EP*TP/(1.0E0-TP))
              T4=0.25E0*((1.0E0-GP)*(1.0E0+EP)-TP-GP*EP*TP/(1.0E0-TP))
              T5= TP
          ELSE IF(NODE(NTET,IE).NE.0) THEN
              T1=GP
              T2=EP
              T3=TP
              T4=1.0E0-(GP+EP+TP)
          ENDIF
C
          IP1=NODE(1,IE)
          IP2=NODE(2,IE)
          IP3=NODE(3,IE)
          IP4=NODE(4,IE)
          IP5=NODE(5,IE)
          IP6=NODE(6,IE)
          IP7=NODE(7,IE)
          IP8=NODE(8,IE)
C
          IF(NODE(NHEX,IE).NE.0) THEN
              UASRC(IPASRC)=
     *         T1*U(IP1)+T2*U(IP2)+T3*U(IP3)+T4*U(IP4)
     *        +T5*U(IP5)+T6*U(IP6)+T7*U(IP7)+T8*U(IP8)
              VASRC(IPASRC)=
     *         T1*V(IP1)+T2*V(IP2)+T3*V(IP3)+T4*V(IP4)
     *        +T5*V(IP5)+T6*V(IP6)+T7*V(IP7)+T8*V(IP8)
              WASRC(IPASRC)=
     *         T1*W(IP1)+T2*W(IP2)+T3*W(IP3)+T4*W(IP4)
     *        +T5*W(IP5)+T6*W(IP6)+T7*W(IP7)+T8*W(IP8)
          ELSE IF(NODE(NWED,IE).NE.0) THEN
              UASRC(IPASRC)=
     *         T1*U(IP1)+T2*U(IP2)+T3*U(IP3)+T4*U(IP4)
     *        +T5*U(IP5)+T6*U(IP6)
              VASRC(IPASRC)=
     *         T1*V(IP1)+T2*V(IP2)+T3*V(IP3)+T4*V(IP4)
     *        +T5*V(IP5)+T6*V(IP6)
              WASRC(IPASRC)=
     *         T1*W(IP1)+T2*W(IP2)+T3*W(IP3)+T4*W(IP4)
     *        +T5*W(IP5)+T6*W(IP6)
          ELSE IF(NODE(NPRD,IE).NE.0) THEN
              UASRC(IPASRC)=
     *         T1*U(IP1)+T2*U(IP2)+T3*U(IP3)+T4*U(IP4)
     *        +T5*U(IP5)
              VASRC(IPASRC)=
     *         T1*V(IP1)+T2*V(IP2)+T3*V(IP3)+T4*V(IP4)
     *        +T5*V(IP5)
              WASRC(IPASRC)=
     *         T1*W(IP1)+T2*W(IP2)+T3*W(IP3)+T4*W(IP4)
     *        +T5*W(IP5)
          ELSE IF(NODE(NTET,IE).NE.0) THEN
              UASRC(IPASRC)=
     *         T1*U(IP1)+T2*U(IP2)+T3*U(IP3)+T4*U(IP4)
              VASRC(IPASRC)=
     *         T1*V(IP1)+T2*V(IP2)+T3*V(IP3)+T4*V(IP4)
              WASRC(IPASRC)=
     *         T1*W(IP1)+T2*W(IP2)+T3*W(IP3)+T4*W(IP4)
          ENDIF
C
 1000 CONTINUE   
C
      RETURN
      END
