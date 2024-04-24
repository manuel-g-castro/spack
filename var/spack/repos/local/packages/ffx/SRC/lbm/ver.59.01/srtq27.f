      SUBROUTINE SRTQ27(NG,NG3,NP,LVEL,WF,CVEL,LREV,LEVEL,
     *                  LPOSI,IINLTV,DSCALE,DBLAS,VISCM,
     *                  F,TAU,FWRK,
     *                  MPBOUN,NPBOUN,LPBOUN,QBOUN,
     *                  UINLT,VINLT,WINLT,UWALL,VWALL,WWALL,
     *                  V1,V2,V3,V4,MASK,FX,FY,FZ,FXB,FYB,FZB,
     *                  JDUMP,FDUMP,CDUMP)
      IMPLICIT NONE
      INTEGER*4 NG,NG3,NP,LVEL(3,NP),LREV(NP),LEVEL,LPOSI(3),IINLTV
C
      REAL*8 WF(NP),CVEL(3,NP),DSCALE,DBLAS,VISCM,
     *       F(NG3,NP),TAU(NG3),FWRK(NG3,NP),
     *       V1(NG3),V2(NG3),V3(NG3),V4(NG3)
      INTEGER*4 MPBOUN,NPBOUN,LPBOUN(5,NPBOUN)
      REAL*4    QBOUN(MPBOUN)
      REAL*8    UINLT,VINLT,WINLT,UWALL,VWALL,WWALL
      REAL*8    FX,FY,FZ
      REAL*8    FXB(MPBOUN),FYB(MPBOUN),FZB(MPBOUN)
      INTEGER*4 JDUMP
      REAL*8    FDUMP(NP),CDUMP(NG3)
      INTEGER*4 MASK(NG3,2)
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
      INTEGER*4 I,J,K,IP,IP1,IP2,IB,ITYPE,IPR,
     *          I1,I2,I3,J1,J2,J3,K1,K2,K3,ID,IG,NG1,NG2,
     *          IG01,IG02,IG03,IG04,IG05,IG06,IG07,IG08,IG09,IG10,
     *          IG11,IG12,IG13,IG14,IG15,IG16,IG17,IG18,IG19,IG20,
     *          IG21,IG22,IG23,IG24,IG25,IG26,IG27
      REAL*8    RHO,U0,V0,W0,UU,CU,JX,JY,JZ,
     *          F1,F1R,F2R,FBUF,Q,Q1,Q2,COEF1,COEF2
C
      REAL*8   CBLAS(0:5),ETA,DD,DG,X0,Y0,Z0
      DATA CBLAS /2.08D-3, 3.02D-1, 5.13D-2,
     *           -2.95D-2, 3.81D-3,-1.59D-4/ 
C
      DG=(DSCALE*2.0E0**(LEVEL-1))/FLOAT(NG)
      X0=DSCALE*LPOSI(1)
      Y0=DSCALE*LPOSI(2)
      Z0=DSCALE*LPOSI(3)
CCCC
CCCC[1] MACRO VARIABLES
CCCC
CCCC (90-FLOP/GRID, 27-WORDS LOAD, 3-WORDS STORE)
      CALL USTSTA(02)
C
      CALL USTSTA(03)
      DO 1000 IG=1,NG3
          RHO= F(IG, 1)+F(IG, 2)+F(IG, 3)+F(IG, 4)
     *        +F(IG, 5)+F(IG, 6)+F(IG, 7)+F(IG, 8)
     *        +F(IG, 9)+F(IG,10)+F(IG,11)+F(IG,12)
     *        +F(IG,13)+F(IG,14)+F(IG,15)
     *        +F(IG,16)+F(IG,17)+F(IG,18)+F(IG,19)
     *        +F(IG,20)+F(IG,21)+F(IG,22)+F(IG,23)
     *        +F(IG,24)+F(IG,25)+F(IG,26)+F(IG,27)
          JX = F(IG, 2)-F(IG, 4)
     *        +F(IG, 8)-F(IG, 9)-F(IG,10)+F(IG,11)
     *        +F(IG,12)-F(IG,13)-F(IG,14)+F(IG,15)
     *        +F(IG,16)-F(IG,17)-F(IG,18)+F(IG,19)
     *        +F(IG,20)-F(IG,21)-F(IG,22)+F(IG,23)
          JY = F(IG, 3)-F(IG, 5)
     *        +F(IG, 8)+F(IG, 9)-F(IG,10)-F(IG,11)
     *        +F(IG,12)+F(IG,13)-F(IG,14)-F(IG,15)
     *        +F(IG,16)+F(IG,17)-F(IG,18)-F(IG,19)
     *        +F(IG,24)-F(IG,25)-F(IG,26)+F(IG,27)
          JZ = F(IG, 6)-F(IG, 7)
     *        +F(IG, 8)+F(IG, 9)+F(IG,10)+F(IG,11)
     *        -F(IG,12)-F(IG,13)-F(IG,14)-F(IG,15)
     *        +F(IG,20)+F(IG,21)-F(IG,22)-F(IG,23)
     *        +F(IG,24)+F(IG,25)-F(IG,26)-F(IG,27)
         V1(IG)=RHO
         V2(IG)=JX/RHO
         V3(IG)=JY/RHO
         V4(IG)=JZ/RHO
 1000 CONTINUE
      CALL USTEND(03)
CCCC
CCCC[2] EQUILIBRIAM FUNCTION
CCCC
CCCC (266-FLOP/GRID, 4-WORDS LOAD, 27-WORDS STORE)
      CALL USTSTA(04)
      DO 1100 IG=1,NG3
         RHO=V1(IG)
         U0 =V2(IG)
         V0 =V3(IG)
         W0 =V4(IG)
         UU =U0*U0+V0*V0+W0*W0

         FWRK(IG,1)= WF(1)*RHO*(1.0D0-1.5D0*UU)

         FBUF=WF(2)*RHO*(1.0D0+3.0D0*U0+4.5D0*U0*U0-1.5D0*UU)
         FWRK(IG,2)= FBUF

         FBUF=WF(3)*RHO*(1.0D0+3.0D0*V0+4.5D0*V0*V0-1.5D0*UU)
         FWRK(IG,3)= FBUF

         FBUF=WF(4)*RHO*(1.0D0-3.0D0*U0+4.5D0*U0*U0-1.5D0*UU)
         FWRK(IG,4)= FBUF

         FBUF=WF(5)*RHO*(1.0D0-3.0D0*V0+4.5D0*V0*V0-1.5D0*UU)
         FWRK(IG,5)= FBUF

         FBUF=WF(6)*RHO*(1.0D0+3.0D0*W0+4.5D0*W0*W0-1.5D0*UU)
         FWRK(IG,6)= FBUF

         FBUF=WF(7)*RHO*(1.0D0-3.0D0*W0+4.5D0*W0*W0-1.5D0*UU)
         FWRK(IG,7)= FBUF

         CU =U0+V0+W0
         FBUF=WF(8)*RHO*(1.0D0+3.0D0*CU+4.5D0*CU*CU-1.5D0*UU)
         FWRK(IG,8)= FBUF

         CU =-U0+V0+W0
         FBUF=WF(9)*RHO*(1.0D0+3.0D0*CU+4.5D0*CU*CU-1.5D0*UU)
         FWRK(IG,9)= FBUF

         CU =-U0-V0+W0
         FBUF=WF(10)*RHO*(1.0D0+3.0D0*CU+4.5D0*CU*CU-1.5D0*UU)
         FWRK(IG,10)= FBUF

         CU =U0-V0+W0
         FBUF=WF(11)*RHO*(1.0D0+3.0D0*CU+4.5D0*CU*CU-1.5D0*UU)
         FWRK(IG,11)= FBUF

         CU =U0+V0-W0
         FBUF=WF(12)*RHO*(1.0D0+3.0D0*CU+4.5D0*CU*CU-1.5D0*UU)
         FWRK(IG,12)= FBUF

         CU =-U0+V0-W0
         FBUF=WF(13)*RHO*(1.0D0+3.0D0*CU+4.5D0*CU*CU-1.5D0*UU)
         FWRK(IG,13)= FBUF

         CU =-U0-V0-W0
         FBUF=WF(14)*RHO*(1.0D0+3.0D0*CU+4.5D0*CU*CU-1.5D0*UU)
         FWRK(IG,14)= FBUF

         CU =U0-V0-W0
         FBUF=WF(15)*RHO*(1.0D0+3.0D0*CU+4.5D0*CU*CU-1.5D0*UU)
         FWRK(IG,15)= FBUF

         CU =U0+V0
         FBUF=WF(16)*RHO*(1.0D0+3.0D0*CU+4.5D0*CU*CU-1.5D0*UU)
         FWRK(IG,16)= FBUF

         CU =-U0+V0
         FBUF=WF(17)*RHO*(1.0D0+3.0D0*CU+4.5D0*CU*CU-1.5D0*UU)
         FWRK(IG,17)= FBUF

         CU =-U0-V0
         FBUF=WF(18)*RHO*(1.0D0+3.0D0*CU+4.5D0*CU*CU-1.5D0*UU)
         FWRK(IG,18)= FBUF

         CU =U0-V0
         FBUF=WF(19)*RHO*(1.0D0+3.0D0*CU+4.5D0*CU*CU-1.5D0*UU)
         FWRK(IG,19)= FBUF

         CU =U0+W0
         FBUF=WF(20)*RHO*(1.0D0+3.0D0*CU+4.5D0*CU*CU-1.5D0*UU)
         FWRK(IG,20)= FBUF

         CU =-U0+W0
         FBUF=WF(21)*RHO*(1.0D0+3.0D0*CU+4.5D0*CU*CU-1.5D0*UU)
         FWRK(IG,21)= FBUF

         CU =-U0-W0
         FBUF=WF(22)*RHO*(1.0D0+3.0D0*CU+4.5D0*CU*CU-1.5D0*UU)
         FWRK(IG,22)= FBUF

         CU =U0-W0
         FBUF=WF(23)*RHO*(1.0D0+3.0D0*CU+4.5D0*CU*CU-1.5D0*UU)
         FWRK(IG,23)= FBUF

         CU =V0+W0
         FBUF=WF(24)*RHO*(1.0D0+3.0D0*CU+4.5D0*CU*CU-1.5D0*UU)
         FWRK(IG,24)= FBUF

         CU =-V0+W0
         FBUF=WF(25)*RHO*(1.0D0+3.0D0*CU+4.5D0*CU*CU-1.5D0*UU)
         FWRK(IG,25)= FBUF

         CU =-V0-W0
         FBUF=WF(26)*RHO*(1.0D0+3.0D0*CU+4.5D0*CU*CU-1.5D0*UU)
         FWRK(IG,26)= FBUF

         CU =V0-W0
         FBUF=WF(27)*RHO*(1.0D0+3.0D0*CU+4.5D0*CU*CU-1.5D0*UU)
         FWRK(IG,27)= FBUF
 1100 CONTINUE
      CALL USTEND(04)
CCCC
CCCC[3] B.C. FOR EQUILIBRIAM FUNCTION
CCCC
      CALL USTSTA(05)
!ocl norecurrence
!NEC$ ivdep
      DO 2000 IB=1,NPBOUN
          I     = LPBOUN(1,IB)
          J     = LPBOUN(2,IB)
          K     = LPBOUN(3,IB)
          IG=I+1+(NG+3)*J+(NG+3)*(NG+3)*K
          IP    = LPBOUN(4,IB)
          ITYPE = LPBOUN(5,IB)
          Q     = DBLE(QBOUN(IB))
          IF(ITYPE.EQ.1 .OR. ITYPE.EQ.11) THEN
C---------[WALL]------------C
              RHO=V1(IG)
              U0 = 0.0D0
              V0 = 0.0D0
              W0 = 0.0D0
              CU= U0*CVEL(1,IP)+V0*CVEL(2,IP)+W0*CVEL(3,IP)
              UU =U0*U0+V0*V0+W0*W0
              FBUF=WF(IP)*RHO*(1.0D0+3.0D0*CU+4.5D0*CU*CU-1.5D0*UU)
              IF(Q.LE.0.25D0) THEN
                  Q1=0.0D0
              ELSE IF(Q.LE.0.75D0) THEN
                  Q1=2.0D0*Q-0.5D0
              ELSE
                  Q1=1.0D0
              ENDIF
              Q2=1.0D0-Q1
              FWRK(IG,IP)=Q1*FWRK(IG,IP)+Q2*FBUF
          ELSE IF(ITYPE.EQ.2) THEN
C---------[INLET]-----------C
C
          ELSE IF(ITYPE.EQ.3) THEN
C---------[MOVING-WALL]-----C
              ID=LPBOUN(4,IB)
              I2=I-LSHIFT(1,ID)
              J2=J-LSHIFT(2,ID)
              K2=K-LSHIFT(3,ID)
              IG02=I2+1+(NG+3)*J2+(NG+3)*(NG+3)*K2
              RHO=V1(IG02)
              U0 =UWALL
              V0 =VWALL
              W0 =WWALL
              UU =U0*U0+V0*V0+W0*W0
              DO 2200 IP2=1,NP
                  CU= U0*CVEL(1,IP2)+V0*CVEL(2,IP2)+W0*CVEL(3,IP2)
                  FBUF=WF(IP2)*RHO*(1.0D0+3.0D0*CU+4.5D0*CU*CU-1.5D0*UU)
                  FWRK(IG,IP2)=FBUF
 2200         CONTINUE
              ELSE IF(ITYPE.EQ.4) THEN
C---------[FREE]-----------C
C
          ENDIF
C
 2000 CONTINUE
      CALL USTEND(05)
CCCC
CCCC[4] COLLISION
CCCC
CCCC (81-FLOP/GRID, 55-WORDS LOAD, 27-WORD STORE)
      CALL USTSTA(06)
      IF(JDUMP.EQ.0) THEN
          DO 3000 IP=1,NP
              DO 3100 IG = 1, NG3
                  FWRK(IG,IP)= F(IG,IP)-(F(IG,IP)-FWRK(IG,IP))/TAU(IG)
 3100         CONTINUE
 3000     CONTINUE
      ELSE
          DO 3200 IP=1,NP
              DO 3300 IG = 1, NG3
                  FWRK(IG,IP)= F(IG,IP)-(F(IG,IP)-FWRK(IG,IP))/TAU(IG)
     *                         -CDUMP(IG)*(FWRK(IG,IP)-FDUMP(IP))
 3300         CONTINUE
 3200     CONTINUE
      ENDIF
      CALL USTEND(06)
CCCC
CCCC[5] TRANSLATION
CCCC
CCCC (0 FLOP/GRID,  15-WORD LOAD, 15-WORD STORE)
      CALL USTSTA(07)
      NG1=NG+3
      NG2=NG1*NG1
!ocl loop_fission_target
      DO 4000 IG=1,NG3
          IF(MASK(IG,1).EQ.0) CYCLE
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
          IG16= IG - 1 - NG1
          IG17= IG + 1 - NG1
          IG18= IG + 1 + NG1
          IG19= IG - 1 + NG1
          IG20= IG - 1       - NG2
          IG21= IG + 1       - NG2
          IG22= IG + 1       + NG2
          IG23= IG - 1       + NG2
          IG24= IG     - NG1 - NG2
          IG25= IG     + NG1 - NG2
          IG26= IG     + NG1 + NG2
          IG27= IG     - NG1 + NG2
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
          F(IG,16) = FWRK(IG16,16)
          F(IG,17) = FWRK(IG17,17)
          F(IG,18) = FWRK(IG18,18)
          F(IG,19) = FWRK(IG19,19)
          F(IG,20) = FWRK(IG20,20)
          F(IG,21) = FWRK(IG21,21)
          F(IG,22) = FWRK(IG22,22)
          F(IG,23) = FWRK(IG23,23)
          F(IG,24) = FWRK(IG24,24)
          F(IG,25) = FWRK(IG25,25)
          F(IG,26) = FWRK(IG26,26)
          F(IG,27) = FWRK(IG27,27)
 4000 CONTINUE
      CALL USTEND(07)
C
CCCC
CCCC[5]  B.C. FOR DISRIBUTION FUNCTION
CCCC
      FXB=0.0D0
      FYB=0.0D0
      FZB=0.0D0
      CALL USTSTA(08)
!ocl norecurrence
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
              IF(ITYPE.EQ.11.AND.MASK(IG03,2).EQ.1) THEN
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
              IF(IINLTV.EQ.0) THEN
                  COEF1=1.0D0
              ELSE
                  IF(IINLTV.EQ.1) DD=X0+DG*I
                  IF(IINLTV.EQ.2) DD=Y0+DG*J
                  IF(IINLTV.EQ.3) DD=Z0+DG*K
                  ETA  =DD/SQRT(VISCM*DBLAS)
                  IF(ETA.LE.7.0D0) THEN
                      COEF1= CBLAS(0)
     *                      +CBLAS(1)*ETA
     *                      +CBLAS(2)*ETA**2.0
     *                      +CBLAS(3)*ETA**3.0
     *                      +CBLAS(4)*ETA**4.0
     *                      +CBLAS(5)*ETA**5.0
                  ELSE
                      COEF1=1.0D0
                  ENDIF
              ENDIF
              U0=UINLT*COEF1
              V0=VINLT*COEF1
              W0=WINLT*COEF1
              UU =U0*U0+V0*V0+W0*W0
              CU= U0*CVEL(1,IP)+V0*CVEL(2,IP)+W0*CVEL(3,IP)
              F(IG01,IP)=FBUF+6.0D0*WF(IP)*CU 
          ELSE IF(ITYPE.EQ.3) THEN  
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
              UU =U0*U0+V0*V0+W0*W0
!NEC$ unroll_completely
              DO 5200 IP=1,NP
                  CU= U0*CVEL(1,IP)+V0*CVEL(2,IP)+W0*CVEL(3,IP)
                  FBUF=WF(IP)*RHO*(1.0D0+3.0D0*CU+4.5D0*CU*CU-1.5D0*UU)
                  F(IG01,IP)= FBUF
 5200         CONTINUE
          ELSE IF(ITYPE.EQ.4) THEN 
C---------[FREE]-----------C
              RHO=0.0D0
              U0 =0.0D0
              V0 =0.0D0
              W0 =0.0D0
!NEC$ unroll_completely
              DO 5300 IP2=1,NP
                  FBUF=FWRK(IG01,IP2)
                  RHO=RHO+FBUF
                  U0 =U0 +FBUF*CVEL(1,IP2)
                  V0 =V0 +FBUF*CVEL(2,IP2)
                  W0 =W0 +FBUF*CVEL(3,IP2)
 5300         CONTINUE
              U0=U0/RHO 
              V0=V0/RHO 
              W0=W0/RHO 
              UU =U0*U0+V0*V0+W0*W0
              CU= U0*CVEL(1,IP)+V0*CVEL(2,IP)+W0*CVEL(3,IP)
              FBUF=WF(IP)*RHO*(1.0D0+3.0D0*CU+4.5D0*CU*CU-1.5D0*UU)
              F(IG01,IP)=FBUF
          ENDIF
C
 5000 CONTINUE
C
      FX=0.0D0
      FY=0.0D0
      FZ=0.0D0
      DO 6000 IB=1,NPBOUN
          FX=FX+FXB(IB)
          FY=FY+FYB(IB)
          FZ=FZ+FZB(IB)
 6000 CONTINUE
C     
      CALL USTEND(08)
C
      CALL USTEND(02)
C
      RETURN
      END
