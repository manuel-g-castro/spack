      SUBROUTINE MRTLBM(NG,NG3,NP,LVEL,WF,CVEL,LREV,LEVEL,
     *                  F,TAU,FWRK,FWRK2,
     *                  MPBOUN,NPBOUN,LPBOUN,QBOUN,
     *                  UINLT,VINLT,WINLT,UWALL,VWALL,WWALL,
     *                  V1,V2,V3,V4,MASK,FX,FY,FZ,FXB,FYB,FZB)
      IMPLICIT NONE
      INTEGER*4 NG,NG3,NP,LVEL(3,NP),LREV(NP),LEVEL
C
      REAL*8 WF(NP),CVEL(3,NP),
     *       F(NG3,NP),TAU(NG3),FWRK(NG3,NP),FWRK2(NG3,NP),
     *       V1(NG3),V2(NG3),V3(NG3),V4(NG3)
      INTEGER*4 MPBOUN,NPBOUN,LPBOUN(5,NPBOUN)
      REAL*4    QBOUN(MPBOUN)
      REAL*8    UINLT,VINLT,WINLT,UWALL,VWALL,WWALL
      REAL*8    FX,FY,FZ
      REAL*8    FXB(MPBOUN),FYB(MPBOUN),FZB(MPBOUN)
      INTEGER*4 MASK(NG3)
      REAL*8    FWRK3(NP),SD(NP),S02,S03,S05,S10,S12,S15
      DATA S02 /1.6D0/
      DATA S03 /1.2D0/
      DATA S05 /1.6D0/
      DATA S10 /2.0D0/
      DATA S12 /2.0D0/
      DATA S15 /1.2D0/
C
      INTEGER*4 LSHIFT(3,26)
      DATA LSHIFT/ -1, 0, 0,
     *              1, 0, 0, 
     *              0,-1, 0,
     *              0, 1, 0, 
     *              0, 0,-1,
     *              0, 0, 1,
     *              0,-1,-1, 0, 1,-1, 0, 1, 1, 0,-1, 1, 
     *             -1, 0,-1, 1, 0,-1, 1, 0, 1,-1, 0, 1, 
     *             -1,-1, 0, 1,-1, 0, 1, 1, 0,-1, 1, 0, 
     *             -1,-1,-1, 1,-1,-1, 1, 1,-1,-1, 1,-1, 
     *             -1,-1, 1, 1,-1, 1, 1, 1, 1,-1, 1, 1 /
C
      INTEGER*4 LMAT(15,15)
      DATA LMAT /  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
     *            -2,-1,-1,-1,-1,-1,-1, 1, 1, 1, 1, 1, 1, 1, 1, 
     *            16,-4,-4,-4,-4,-4,-4, 1, 1, 1, 1, 1, 1, 1, 1, 
     *             0, 1, 0,-1, 0, 0, 0, 1,-1,-1, 1, 1,-1,-1, 1, 
     *             0,-4, 0, 4, 0, 0, 0, 1,-1,-1, 1, 1,-1,-1, 1, 
     *             0, 0, 1, 0,-1, 0, 0, 1, 1,-1,-1, 1, 1,-1,-1, 
     *             0, 0,-4, 0, 4, 0, 0, 1, 1,-1,-1, 1, 1,-1,-1, 
     *             0, 0, 0, 0, 0, 1,-1, 1, 1, 1, 1,-1,-1,-1,-1, 
     *             0, 0, 0, 0, 0,-4, 4, 1, 1, 1, 1,-1,-1,-1,-1, 
     *             0, 2,-1, 2,-1,-1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 
     *             0, 0, 1, 0, 1,-1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 
     *             0, 0, 0, 0, 0, 0, 0, 1,-1, 1,-1, 1,-1, 1,-1, 
     *             0, 0, 0, 0, 0, 0, 0, 1, 1,-1,-1,-1,-1, 1, 1, 
     *             0, 0, 0, 0, 0, 0, 0, 1,-1,-1, 1,-1, 1, 1,-1, 
     *             0, 0, 0, 0, 0, 0, 0, 1,-1, 1,-1,-1, 1,-1, 1 / 
C  
      INTEGER*4 I,J,K,IP,IP1,IP2,IB,ITYPE,IPR,
     *          I1,I2,I3,J1,J2,J3,K1,K2,K3,ID,IG,NG1,NG2,
     *          IG01,IG02,IG03,IG04,IG05,IG06,IG07,IG08,IG09,IG10,
     *          IG11,IG12,IG13,IG14,IG15,II,JJ
      REAL*8    RHO,U0,V0,W0,UU,CU,JX,JY,JZ,EE,
     *          F1,F1R,F2R,FBUF,Q,Q1,Q2,COEF1,COEF2
C
C
CCCC
CCCC[1] MACRO VARIABLES
CCCC
CCCC (48-FLOP/GRID, 15-WORDS LOAD, 4-WORDS STORE)
      CALL USTSTA(32)
C
      CALL USTSTA(33)
      DO 1000 IG=1,NG3
          RHO= F(IG, 1)+F(IG, 2)+F(IG, 3)+F(IG, 4)
     *        +F(IG, 5)+F(IG, 6)+F(IG, 7)+F(IG, 8)
     *        +F(IG, 9)+F(IG,10)+F(IG,11)+F(IG,12)
     *        +F(IG,13)+F(IG,14)+F(IG,15)
          JX = F(IG, 2)-F(IG, 4)
     *        +F(IG, 8)-F(IG, 9)-F(IG,10)+F(IG,11)
     *        +F(IG,12)-F(IG,13)-F(IG,14)+F(IG,15)
          JY = F(IG, 3)-F(IG, 5)
     *        +F(IG, 8)+F(IG, 9)-F(IG,10)-F(IG,11)
     *        +F(IG,12)+F(IG,13)-F(IG,14)-F(IG,15)
          JZ = F(IG, 6)-F(IG, 7)
     *        +F(IG, 8)+F(IG, 9)+F(IG,10)+F(IG,11)
     *        -F(IG,12)-F(IG,13)-F(IG,14)-F(IG,15)
         V1(IG)=RHO
         V2(IG)=JX
         V3(IG)=JY
         V4(IG)=JZ
 1000 CONTINUE
      CALL USTEND(33)
CCCC
CCCC[2] EQUILIBRIAM FUNCTION
CCCC
      CALL USTSTA(34)
      DO 1100 IG=1,NG3
          RHO=V1(IG)
          JX =V2(IG)
          JY =V3(IG)
          JZ =V4(IG)
          EE =JX*JX+JY*JY+JZ*JZ
          FWRK(IG, 1)= RHO
          FWRK(IG, 2)=-RHO+EE
          FWRK(IG, 3)=-RHO
          FWRK(IG, 4)= JX
          FWRK(IG, 5)=-JX*7.0D0/3.0D0
          FWRK(IG, 6)= JY
          FWRK(IG, 7)=-JY*7.0D0/3.0D0
          FWRK(IG, 8)= JZ
          FWRK(IG, 9)=-JZ*7.0D0/3.0D0
          FWRK(IG,10)= 2.0D0*JX*JX-JY*JY-JZ*JZ
          FWRK(IG,11)=             JY*JY-JZ*JZ
          FWRK(IG,12)= JX*JY
          FWRK(IG,13)= JY*JZ
          FWRK(IG,14)= JZ*JX
          FWRK(IG,15)= 0.0D0
 1100 CONTINUE
      CALL USTEND(34)
CCCC
CCCC[3] B.C. FOR EQUILIBRIAM FUNCTION
CCCC
      CALL USTSTA(35)
      CALL USTEND(35)
CCCC
CCCC[4] COLLISION
CCCC
CCCC (45-FLOP/GRID, 31-WORDS LOAD, 15-WORD STORE)
      SD( 1)=0.0D0/1.5D1
      SD( 2)=  S02/1.8D1
      SD( 3)=  S03/3.6D2
      SD( 4)=0.0D0/1.0D1
      SD( 5)=  S05/4.0D1
      SD( 6)=0.0D0/1.0D1
      SD( 7)=  S05/4.0D1
      SD( 8)=0.0D0/1.0D1
      SD( 9)=  S05/4.0D1
      SD(10)=  S10/1.2D1
      SD(11)=  S10/4.0D0
      SD(12)=  S12/8.0D0
      SD(13)=  S12/8.0D0
      SD(14)=  S12/8.0D0
      SD(15)=  S15/8.0D0
      CALL USTSTA(36)
!ocl norecurrence (FWRK)
      DO 3000 IG = 1, NG3
          FWRK2(IG, 1)= F(IG, 1)+F(IG, 2)+F(IG, 3)+F(IG, 4)
     *                 +F(IG, 5)+F(IG, 6)+F(IG, 7)+F(IG, 8)
     *                 +F(IG, 9)+F(IG,10)+F(IG,11)+F(IG,12)
     *                 +F(IG,13)+F(IG,14)+F(IG,15)
          FWRK2(IG, 2)=-2.0D0*F(IG, 1)
     *                 -F(IG, 2)-F(IG, 3)-F(IG, 4)
     *                 -F(IG, 5)-F(IG, 6)-F(IG, 7)
     *                 +F(IG, 8)+F(IG, 9)+F(IG,10)+F(IG,11)
     *                 +F(IG,12)+F(IG,13)+F(IG,14)+F(IG,15)
          FWRK2(IG, 3)= 1.6D1*F(IG, 1)
     *                 -4.0D0*( F(IG, 2)+F(IG, 3)+F(IG, 4)
     *                       +F(IG, 5)+F(IG, 6)+F(IG, 7))
     *                 +F(IG, 8)+F(IG, 9)+F(IG,10)+F(IG,11)
     *                 +F(IG,12)+F(IG,13)+F(IG,14)+F(IG,15)
          FWRK2(IG, 4)= F(IG, 2)-F(IG, 4)
     *                 +F(IG, 8)-F(IG, 9)-F(IG,10)+F(IG,11)
     *                 +F(IG,12)-F(IG,13)-F(IG,14)+F(IG,15)
          FWRK2(IG, 5)= 4.0D0*( -F(IG, 2)+F(IG, 4))
     *                 +F(IG, 8)-F(IG, 9)-F(IG,10)+F(IG,11)
     *                 +F(IG,12)-F(IG,13)-F(IG,14)+F(IG,15)
          FWRK2(IG, 6)= F(IG, 3)-F(IG, 5)
     *                 +F(IG, 8)+F(IG, 9)-F(IG,10)-F(IG,11)
     *                 +F(IG,12)+F(IG,13)-F(IG,14)-F(IG,15)
          FWRK2(IG, 7)= 4.0D0*( -F(IG, 3)+F(IG, 5))
     *                 +F(IG, 8)+F(IG, 9)-F(IG,10)-F(IG,11)
     *                 +F(IG,12)+F(IG,13)-F(IG,14)-F(IG,15)
          FWRK2(IG, 8)= F(IG, 6)-F(IG, 7)
     *                 +F(IG, 8)+F(IG, 9)+F(IG,10)+F(IG,11)
     *                 -F(IG,12)-F(IG,13)-F(IG,14)-F(IG,15)
          FWRK2(IG, 9)= 4.0D0*( -F(IG, 6)+F(IG, 7))
     *                 +F(IG, 8)+F(IG, 9)+F(IG,10)+F(IG,11)
     *                 -F(IG,12)-F(IG,13)-F(IG,14)-F(IG,15)
          FWRK2(IG,10)= F(IG, 2)+F(IG, 2)-F(IG, 3)
     *                 +F(IG, 4)+F(IG, 4)-F(IG, 5)
     *                 -F(IG, 6)-F(IG, 7)
          FWRK2(IG,11)= F(IG, 3)+F(IG, 5)-F(IG, 6)-F(IG, 7)
          FWRK2(IG,12)= F(IG, 8)-F(IG, 9)+F(IG,10)-F(IG,11)
     *                 +F(IG,12)-F(IG,13)+F(IG,14)-F(IG,15)  
          FWRK2(IG,13)= F(IG, 8)+F(IG, 9)-F(IG,10)-F(IG,11)
     *                 -F(IG,12)-F(IG,13)+F(IG,14)+F(IG,15)  
          FWRK2(IG,14)= F(IG, 8)-F(IG, 9)-F(IG,10)+F(IG,11)
     *                 -F(IG,12)+F(IG,13)+F(IG,14)-F(IG,15)  
          FWRK2(IG,15)= F(IG, 8)-F(IG, 9)+F(IG,10)-F(IG,11)
     *                 -F(IG,12)+F(IG,13)-F(IG,14)+F(IG,15)  
C
          FWRK2(IG, 1)=(FWRK2(IG, 1)-FWRK(IG, 1))*(0.0D0/1.5D1)
          FWRK2(IG, 2)=(FWRK2(IG, 2)-FWRK(IG, 2))*(  S02/1.8D1)
          FWRK2(IG, 3)=(FWRK2(IG, 3)-FWRK(IG, 3))*(  S03/3.6D2)
          FWRK2(IG, 4)=(FWRK2(IG, 4)-FWRK(IG, 4))*(0.0D0/1.0D1)
          FWRK2(IG, 5)=(FWRK2(IG, 5)-FWRK(IG, 5))*(  S05/4.0D1)
          FWRK2(IG, 6)=(FWRK2(IG, 6)-FWRK(IG, 6))*(0.0D0/1.0D1)
          FWRK2(IG, 7)=(FWRK2(IG, 7)-FWRK(IG, 7))*(  S05/4.0D1)
          FWRK2(IG, 8)=(FWRK2(IG, 8)-FWRK(IG, 8))*(0.0D0/1.0D1)
          FWRK2(IG, 9)=(FWRK2(IG, 9)-FWRK(IG, 9))*(  S05/4.0D1)
          FWRK2(IG,10)=(FWRK2(IG,10)-FWRK(IG,10))/(1.2D1*TAU(IG))
          FWRK2(IG,11)=(FWRK2(IG,11)-FWRK(IG,11))/(4.0D0*TAU(IG))
          FWRK2(IG,12)=(FWRK2(IG,12)-FWRK(IG,12))/(8.0D0*TAU(IG))
          FWRK2(IG,13)=(FWRK2(IG,13)-FWRK(IG,13))/(8.0D0*TAU(IG))
          FWRK2(IG,14)=(FWRK2(IG,14)-FWRK(IG,14))/(8.0D0*TAU(IG))
          FWRK2(IG,15)=(FWRK2(IG,15)-FWRK(IG,15))*(  S15)/8.0D0
C
 3000 CONTINUE
C
!ocl norecurrence (FWRK)
      DO 3100 IG = 1, NG3
CC IP=1
          FBUF= FWRK2(IG,1)-2.0D0*FWRK2(IG,2)+1.6D1*FWRK2(IG,3)
          FWRK(IG, 1)= F(IG, 1)-FBUF
CC IP=2
          FBUF= FWRK2(IG,1)-FWRK2(IG, 2)-4.0D0*FWRK2(IG, 3)
     *                     +FWRK2(IG, 4)-4.0D0*FWRK2(IG, 5)
     *         +2.0D0*FWRK2(IG,10)
          FWRK(IG, 2)= F(IG, 2)-FBUF
CC IP=3
          FBUF= FWRK2(IG,1)-FWRK2(IG, 2)-4.0D0*FWRK2(IG, 3)
     *                      +FWRK2(IG, 6)-4.0D0*FWRK2(IG, 7)
     *                      -FWRK2(IG,10)+FWRK2(IG,11)
          FWRK(IG, 3)= F(IG, 3)-FBUF
CC IP=4
          FBUF= FWRK2(IG,1)-FWRK2(IG, 2)-4.0D0*FWRK2(IG, 3)
     *                     -FWRK2(IG, 4)+4.0D0*FWRK2(IG, 5)
     *                     +2.0D0*FWRK2(IG,10)
          FWRK(IG, 4)= F(IG, 4)-FBUF
CC IP=5
          FBUF= FWRK2(IG,1)-FWRK2(IG, 2)-4.0D0*FWRK2(IG, 3)
     *                     -FWRK2(IG, 6)+4.0D0*FWRK2(IG, 7)
     *                     -FWRK2(IG,10)+FWRK2(IG,11)
          FWRK(IG, 5)= F(IG, 5)-FBUF
CC IP=6
          FBUF= FWRK2(IG,1)-FWRK2(IG, 2)-4.0D0*FWRK2(IG, 3)
     *                     +FWRK2(IG, 8)-4.0D0*FWRK2(IG, 9)
     *                     -FWRK2(IG,10)-FWRK2(IG,11)
          FWRK(IG, 6)= F(IG, 6)-FBUF
CC IP=7
          FBUF= FWRK2(IG,1)-FWRK2(IG, 2)-4.0D0*FWRK2(IG, 3)
     *                     -FWRK2(IG, 8)+4.0D0*FWRK2(IG, 9)
     *                     -FWRK2(IG,10)-FWRK2(IG,11)
          FWRK(IG, 7)= F(IG, 7)-FBUF
CC IP=8
          FBUF= FWRK2(IG, 1)
     *         +FWRK2(IG, 2)+FWRK2(IG, 3)+FWRK2(IG, 4)+FWRK2(IG, 5)
     *         +FWRK2(IG, 6)+FWRK2(IG, 7)+FWRK2(IG, 8)+FWRK2(IG, 9)
     *         +FWRK2(IG,12)+FWRK2(IG,13)+FWRK2(IG,14)+FWRK2(IG,15)
          FWRK(IG, 8)= F(IG, 8)-FBUF
CC IP=9
          FBUF=FWRK2(IG, 1)
     *         +FWRK2(IG, 2)+FWRK2(IG, 3)-FWRK2(IG, 4)-FWRK2(IG, 5)
     *         +FWRK2(IG, 6)+FWRK2(IG, 7)+FWRK2(IG, 8)+FWRK2(IG, 9)
     *         -FWRK2(IG,12)+FWRK2(IG,13)-FWRK2(IG,14)-FWRK2(IG,15)
          FWRK(IG, 9)= F(IG, 9)-FBUF
CC IP=10
          FBUF=FWRK2(IG, 1)
     *         +FWRK2(IG, 2)+FWRK2(IG, 3)-FWRK2(IG, 4)-FWRK2(IG, 5)
     *         -FWRK2(IG, 6)-FWRK2(IG, 7)+FWRK2(IG, 8)+FWRK2(IG, 9)
     *         +FWRK2(IG,12)-FWRK2(IG,13)-FWRK2(IG,14)+FWRK2(IG,15)
          FWRK(IG,10)= F(IG,10)-FBUF
CC IP=11
          FBUF=FWRK2(IG, 1)
     *         +FWRK2(IG, 2)+FWRK2(IG, 3)+FWRK2(IG, 4)+FWRK2(IG, 5)
     *         -FWRK2(IG, 6)-FWRK2(IG, 7)+FWRK2(IG, 8)+FWRK2(IG, 9)
     *         -FWRK2(IG,12)-FWRK2(IG,13)+FWRK2(IG,14)-FWRK2(IG,15)
          FWRK(IG,11)= F(IG,11)-FBUF
CC IP=12
          FBUF=FWRK2(IG, 1)
     *         +FWRK2(IG, 2)+FWRK2(IG, 3)+FWRK2(IG, 4)+FWRK2(IG, 5)
     *         +FWRK2(IG, 6)+FWRK2(IG, 7)-FWRK2(IG, 8)-FWRK2(IG, 9)
     *         +FWRK2(IG,12)-FWRK2(IG,13)-FWRK2(IG,14)-FWRK2(IG,15)
          FWRK(IG,12)= F(IG,12)-FBUF
CC IP=13
          FBUF=FWRK2(IG, 1)
     *         +FWRK2(IG, 2)+FWRK2(IG, 3)-FWRK2(IG, 4)-FWRK2(IG, 5)
     *         +FWRK2(IG, 6)+FWRK2(IG, 7)-FWRK2(IG, 8)-FWRK2(IG, 9)
     *         -FWRK2(IG,12)-FWRK2(IG,13)+FWRK2(IG,14)+FWRK2(IG,15)
          FWRK(IG,13)= F(IG,13)-FBUF
CC IP=14
          FBUF=FWRK2(IG, 1)
     *         +FWRK2(IG, 2)+FWRK2(IG, 3)-FWRK2(IG, 4)-FWRK2(IG, 5)
     *         -FWRK2(IG, 6)-FWRK2(IG, 7)-FWRK2(IG, 8)-FWRK2(IG, 9)
     *         +FWRK2(IG,12)+FWRK2(IG,13)+FWRK2(IG,14)-FWRK2(IG,15)
          FWRK(IG,14)= F(IG,14)-FBUF
CC IP=15
          FBUF=FWRK2(IG, 1)
     *         +FWRK2(IG, 2)+FWRK2(IG, 3)+FWRK2(IG, 4)+FWRK2(IG, 5)
     *         -FWRK2(IG, 6)-FWRK2(IG, 7)-FWRK2(IG, 8)-FWRK2(IG, 9)
     *         -FWRK2(IG,12)+FWRK2(IG,13)-FWRK2(IG,14)+FWRK2(IG,15)
          FWRK(IG,15)= F(IG,15)-FBUF
 3100 CONTINUE
      CALL USTEND(36)
CCCC
CCCC[5] TRANSLATION
CCCC
CCCC (0 FLOP/GRID,  15-WORD LOAD, 15-WORD STORE)
      CALL USTSTA(37)
      NG1=NG+3
      NG2=NG1*NG1
!ocl loop_fission_target
      DO 4000 IG=1,NG3
          IF(MASK(IG).EQ.0) CYCLE
          IG01= IG
          IG02= IG - 1 
          IG03= IG     - NG1
          IG04= IG + 1 
          IG05= IG     + NG1
          IG06= IG           - NG2
          IG07= IG           + NG2
          IG08= IG - 1 - NG1 - NG2
          IG09= IG + 1 - NG1 - NG2
          IG10= IG + 1 + NG1 - NG2
          IG11= IG - 1 + NG1 - NG2
          IG12= IG - 1 - NG1 + NG2
          IG13= IG + 1 - NG1 + NG2
          IG14= IG + 1 + NG1 + NG2
          IG15= IG - 1 + NG1 + NG2
C
          F(IG, 1) = FWRK(IG01, 1)
          F(IG, 2) = FWRK(IG02, 2)
          F(IG, 3) = FWRK(IG03, 3)
          F(IG, 4) = FWRK(IG04, 4)
          F(IG, 5) = FWRK(IG05, 5)
          F(IG, 6) = FWRK(IG06, 6)
          F(IG, 7) = FWRK(IG07, 7)
          F(IG, 8) = FWRK(IG08, 8)
          F(IG, 9) = FWRK(IG09, 9)
          F(IG,10) = FWRK(IG10,10)
          F(IG,11) = FWRK(IG11,11)
          F(IG,12) = FWRK(IG12,12)
          F(IG,13) = FWRK(IG13,13)
          F(IG,14) = FWRK(IG14,14)
          F(IG,15) = FWRK(IG15,15)
 4000 CONTINUE
      CALL USTEND(37)
C
CCCC
CCCC[5]  B.C. FOR DISRIBUTION FUNCTION
CCCC
      SD( 1)=1.0D0/1.5D1
      SD( 2)=1.0D0/1.8D1
      SD( 3)=1.0D0/3.6D2
      SD( 4)=1.0D0/1.0D1
      SD( 5)=1.0D0/4.0D1
      SD( 6)=1.0D0/1.0D1
      SD( 7)=1.0D0/4.0D1
      SD( 8)=1.0D0/1.0D1
      SD( 9)=1.0D0/4.0D1
      SD(10)=1.0D0/1.2D1
      SD(11)=1.0D0/4.0D0
      SD(12)=1.0D0/8.0D0
      SD(13)=1.0D0/8.0D0
      SD(14)=1.0D0/8.0D0
      SD(15)=1.0D0/8.0D0
      FXB=0.0D0
      FYB=0.0D0
      FZB=0.0D0
      CALL USTSTA(38)
!ocl norecurrence (F)
      DO 5000 IB=1,NPBOUN
C
          I     = LPBOUN(1,IB)
          J     = LPBOUN(2,IB)
          K     = LPBOUN(3,IB)
          IG=I+1+(NG+3)*J+(NG+3)*(NG+3)*K
          IP    = LPBOUN(4,IB)
          ITYPE = LPBOUN(5,IB)
          IPR   = LREV (IP)
          Q     = DBLE(QBOUN(IB))
          I1=I
          J1=J
          K1=K
          IG01=I+1+(NG+3)*J+(NG+3)*(NG+3)*K
          I2=I1 + LVEL(1,IP)
          J2=J1 + LVEL(2,IP)
          K2=K1 + LVEL(3,IP)
          IG02=I2+1+(NG+3)*J2+(NG+3)*(NG+3)*K2
C
          I3=I1 + LVEL(1,IPR)
          J3=J1 + LVEL(2,IPR)
          K3=K1 + LVEL(3,IPR)
          IG03=I3+1+(NG+3)*J3+(NG+3)*(NG+3)*K3
C
          IF(ITYPE.EQ.1 .OR. ITYPE.EQ.11) THEN
              F1 =FWRK(IG01,IP )
              F1R=FWRK(IG01,IPR)
              F2R=FWRK(IG02,IPR)
C---------[WALL]------------C
              IF(Q.LT.0.5D0) THEN
                  COEF1= 2.0D0*Q
                  COEF2= 1.0D0-COEF1
                  FBUF =COEF1*F1R+COEF2*F2R
              ELSE
                  COEF1= 0.5D0/Q
                  COEF2= 1.0D0-COEF1
                  FBUF =COEF1*F1R+COEF2*F1
              ENDIF
C CAL. FLUID FORCE
              IF(ITYPE.EQ.11.AND.MASK(IG03).EQ.1) THEN
                  FXB(IB)=CVEL(1,IPR)*F1R-CVEL(1,IP)*FBUF
                  FYB(IB)=CVEL(2,IPR)*F1R-CVEL(2,IP)*FBUF
                  FZB(IB)=CVEL(3,IPR)*F1R-CVEL(3,IP)*FBUF
              ENDIF    
              F(IG01,IP)=FBUF
          ELSE IF(ITYPE.EQ.2) THEN 
C---------[INLET]-----------C
              F1R=FWRK(IG01,IPR)
              FBUF=F1R
              RHO=1.0D0
              U0=UINLT
              V0=VINLT
              W0=WINLT
              CU= U0*CVEL(1,IP)+V0*CVEL(2,IP)+W0*CVEL(3,IP)
              F(IG01,IP)=FBUF+6.0D0*WF(IP)*CU 
          ELSE IF(ITYPE.EQ.3) THEN 
C---------[Moving-INLET]-----------C
              F1R=FWRK(IG01,IPR)
              FBUF=F1R
              RHO=1.0D0
              U0=UWALL
              V0=VWALL
              W0=WWALL
              CU= U0*CVEL(1,IP)+V0*CVEL(2,IP)+W0*CVEL(3,IP)
              F(IG01,IP)=FBUF+6.0D0*WF(IP)*CU 
          ENDIF
C
 5000 CONTINUE
C
!ocl norecurrence (F)
      DO 6000 IB=1,NPBOUN
C
          I     = LPBOUN(1,IB)
          J     = LPBOUN(2,IB)
          K     = LPBOUN(3,IB)
          IG=I+1+(NG+3)*J+(NG+3)*(NG+3)*K
          IP    = LPBOUN(4,IB)
          ITYPE = LPBOUN(5,IB)
          IPR   = LREV (IP)
          Q     = DBLE(QBOUN(IB))
          I1=I
          J1=J
          K1=K
          IG01=I+1+(NG+3)*J+(NG+3)*(NG+3)*K
          I2=I1 + LVEL(1,IP)
          J2=J1 + LVEL(2,IP)
          K2=K1 + LVEL(3,IP)
          IG02=I2+1+(NG+3)*J2+(NG+3)*(NG+3)*K2
C
          IF(ITYPE.EQ.3) THEN  
C---------[MOVING-WALL]-----C
              ID=LPBOUN(4,IB)
              I2=I-LSHIFT(1,ID)
              J2=J-LSHIFT(2,ID)
              K2=K-LSHIFT(3,ID)
              IG02=I2+1+(NG+3)*J2+(NG+3)*(NG+3)*K2
              RHO=V1(IG02)
              U0=UWALL
              V0=VWALL
              W0=WWALL
              JX = RHO*U0
              JY = RHO*V0
              JZ = RHO*W0
              EE =JX*JX+JY*JY+JZ*JZ
              FWRK3( 1)= RHO
              FWRK3( 2)=-RHO+EE
              FWRK3( 3)=-RHO
              FWRK3( 4)= JX
              FWRK3( 5)=-JX*7.0D0/3.0D0
              FWRK3( 6)= JY
              FWRK3( 7)=-JY*7.0D0/3.0D0
              FWRK3( 8)= JZ
              FWRK3( 9)=-JZ*7.0D0/3.0D0
              FWRK3(10)= 2.0D0*JX*JX-JY*JY-JZ*JZ
              FWRK3(11)=             JY*JY-JZ*JZ
              FWRK3(12)= JX*JY
              FWRK3(13)= JY*JZ
              FWRK3(14)= JZ*JX
              FWRK3(15)= 0.0D0
!ocl UNROLL
              DO 6200 JJ = 1,15
                  FBUF= SD( 1)*DBLE(LMAT(JJ, 1))*FWRK3( 1)
     *                 +SD( 2)*DBLE(LMAT(JJ, 2))*FWRK3( 2)
     *                 +SD( 3)*DBLE(LMAT(JJ, 3))*FWRK3( 3)
     *                 +SD( 4)*DBLE(LMAT(JJ, 4))*FWRK3( 4)
     *                 +SD( 5)*DBLE(LMAT(JJ, 5))*FWRK3( 5)
     *                 +SD( 6)*DBLE(LMAT(JJ, 6))*FWRK3( 6)
     *                 +SD( 7)*DBLE(LMAT(JJ, 7))*FWRK3( 7)
     *                 +SD( 8)*DBLE(LMAT(JJ, 8))*FWRK3( 8)
     *                 +SD( 9)*DBLE(LMAT(JJ, 9))*FWRK3( 9)
     *                 +SD(10)*DBLE(LMAT(JJ,10))*FWRK3(10)
     *                 +SD(11)*DBLE(LMAT(JJ,11))*FWRK3(11)
     *                 +SD(12)*DBLE(LMAT(JJ,12))*FWRK3(12)
     *                 +SD(13)*DBLE(LMAT(JJ,13))*FWRK3(13)
     *                 +SD(14)*DBLE(LMAT(JJ,14))*FWRK3(14)
     *                 +SD(15)*DBLE(LMAT(JJ,15))*FWRK3(15)
                  F(IG01,JJ)= FBUF
 6200         CONTINUE
          ENDIF
C     
 6000 CONTINUE
C
!ocl norecurrence (F)
      DO 7000 IB=1,NPBOUN
          I     = LPBOUN(1,IB)
          J     = LPBOUN(2,IB)
          K     = LPBOUN(3,IB)
          IG=I+1+(NG+3)*J+(NG+3)*(NG+3)*K
          IP    = LPBOUN(4,IB)
          ITYPE = LPBOUN(5,IB)
          IPR   = LREV (IP)
          Q     = DBLE(QBOUN(IB))
          IG01=I+1+(NG+3)*J+(NG+3)*(NG+3)*K
          IF(ITYPE.EQ.4) THEN 
C---------[FREE]-----------C
              RHO= FWRK(IG01, 1)+FWRK(IG01, 2)
     *            +FWRK(IG01, 3)+FWRK(IG01, 4)
     *            +FWRK(IG01, 5)+FWRK(IG01, 6)
     *            +FWRK(IG01, 7)+FWRK(IG01, 8)
     *            +FWRK(IG01, 9)+FWRK(IG01,10)
     *            +FWRK(IG01,11)+FWRK(IG01,12)
     *            +FWRK(IG01,13)+FWRK(IG01,14)+FWRK(IG01,15)
              JX = FWRK(IG01, 2)-FWRK(IG01, 4)
     *            +FWRK(IG01, 8)-FWRK(IG01, 9)
     *            -FWRK(IG01,10)+FWRK(IG01,11)
     *            +FWRK(IG01,12)-FWRK(IG01,13)
     *            -FWRK(IG01,14)+FWRK(IG01,15)
              JY = FWRK(IG01, 3)-FWRK(IG01, 5)
     *            +FWRK(IG01, 8)+FWRK(IG01, 9)
     *            -FWRK(IG01,10)-FWRK(IG01,11)
     *            +FWRK(IG01,12)+FWRK(IG01,13)
     *            -FWRK(IG01,14)-FWRK(IG01,15)
              JZ = FWRK(IG01, 6)-FWRK(IG01, 7)
     *            +FWRK(IG01, 8)+FWRK(IG01, 9)
     *            +FWRK(IG01,10)+FWRK(IG01,11)
     *            -FWRK(IG01,12)-FWRK(IG01,13)
     *            -FWRK(IG01,14)-FWRK(IG01,15)
              EE =JX*JX+JY*JY+JZ*JZ
              FWRK3( 1)= RHO
              FWRK3( 2)=-RHO+EE
              FWRK3( 3)=-RHO
              FWRK3( 4)= JX
              FWRK3( 5)=-JX*7.0D0/3.0D0
              FWRK3( 6)= JY
              FWRK3( 7)=-JY*7.0D0/3.0D0
              FWRK3( 8)= JZ
              FWRK3( 9)=-JZ*7.0D0/3.0D0
              FWRK3(10)= 2.0D0*JX*JX-JY*JY-JZ*JZ
              FWRK3(11)=             JY*JY-JZ*JZ
              FWRK3(12)= JX*JY
              FWRK3(13)= JY*JZ
              FWRK3(14)= JZ*JX
              FWRK3(15)= 0.0D0
              FBUF= SD( 1)*DBLE(LMAT(IP, 1))*FWRK3( 1)
     *             +SD( 2)*DBLE(LMAT(IP, 2))*FWRK3( 2)
     *             +SD( 3)*DBLE(LMAT(IP, 3))*FWRK3( 3)
     *             +SD( 4)*DBLE(LMAT(IP, 4))*FWRK3( 4)
     *             +SD( 5)*DBLE(LMAT(IP, 5))*FWRK3( 5)
     *             +SD( 6)*DBLE(LMAT(IP, 6))*FWRK3( 6)
     *             +SD( 7)*DBLE(LMAT(IP, 7))*FWRK3( 7)
     *             +SD( 8)*DBLE(LMAT(IP, 8))*FWRK3( 8)
     *             +SD( 9)*DBLE(LMAT(IP, 9))*FWRK3( 9)
     *             +SD(10)*DBLE(LMAT(IP,10))*FWRK3(10)
     *             +SD(11)*DBLE(LMAT(IP,11))*FWRK3(11)
     *             +SD(12)*DBLE(LMAT(IP,12))*FWRK3(12)
     *             +SD(13)*DBLE(LMAT(IP,13))*FWRK3(13)
     *             +SD(14)*DBLE(LMAT(IP,14))*FWRK3(14)
     *             +SD(15)*DBLE(LMAT(IP,15))*FWRK3(15)
              F(IG01,IP)= FBUF
          ENDIF
 7000 CONTINUE
C
      FX=0.0D0
      FY=0.0D0
      FZ=0.0D0
      DO 8000 IB=1,NPBOUN
          FX=FX+FXB(IB)
          FY=FY+FYB(IB)
          FZ=FZ+FZB(IB)
 8000 CONTINUE
C
      CALL USTEND(38)
C
      CALL USTEND(32)
C
      RETURN
      END
