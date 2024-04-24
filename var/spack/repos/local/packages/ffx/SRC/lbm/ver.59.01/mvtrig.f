      SUBROUTINE MVTRIG(NG,NPTRG,DD,RATIO,X,Y,Z)
      IMPLICIT NONE
      INTEGER*4 NG,NPTRG
      REAL*8    DD,RATIO
      REAL*4    X(NPTRG),Y(NPTRG),Z(NPTRG)
C
      INTEGER*4 NE,ITRG,IP1,IP2,IP3
      REAL*4    DG,DMV,X1,X2,X3,Y1,Y2,Y3,Z1,Z2,Z3,
     *          AX,AY,AZ,BX,BY,BZ,CX,CY,CZ,CABS
 
C
      DG  =REAL(DD)/DBLE(NG)
      DMV =DG*REAL(RATIO)
      NE  =NPTRG/3
C
      DO 1000 ITRG=1,NE
          IP1=3*(ITRG-1)+1
          IP2=3*(ITRG-1)+2
          IP3=3*(ITRG-1)+3
          X1 =X(IP1)
          X2 =X(IP2)
          X3 =X(IP3)
          Y1 =Y(IP1)
          Y2 =Y(IP2)
          Y3 =Y(IP3)
          Z1 =Z(IP1)
          Z2 =Z(IP2)
          Z3 =Z(IP3)
C
          AX = X2-X1
          AY = Y2-Y1
          AZ = Z2-Z1
          BX = X3-X2
          BY = Y3-Y2
          BZ = Z3-Z2
          CX = AY*BZ-AZ*BY
          CY = AZ*BX-AX*BZ
          CZ = AX*BY-AY*BX 
          CABS = SQRT(CX*CX+CY*CY+CZ*CZ)
          IF(CABS.NE.0.0D0) THEN
              CX=CX/CABS 
              CY=CY/CABS 
              CZ=CZ/CABS 
          ENDIF
C
          X(IP1)=X(IP1)+CX*DMV
          Y(IP1)=Y(IP1)+CY*DMV
          Z(IP1)=Z(IP1)+CZ*DMV
          X(IP2)=X(IP2)+CX*DMV
          Y(IP2)=Y(IP2)+CY*DMV
          Z(IP2)=Z(IP2)+CZ*DMV
          X(IP3)=X(IP3)+CX*DMV
          Y(IP3)=Y(IP3)+CY*DMV
          Z(IP3)=Z(IP3)+CZ*DMV
C
 1000 CONTINUE
C
      RETURN
      END
