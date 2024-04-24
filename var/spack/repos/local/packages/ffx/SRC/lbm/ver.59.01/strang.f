      SUBROUTINE STRANG(COORD,DD,YY,NN,X1,X2,IERR)
      IMPLICIT NONE
C
C     FUNCTION: CAL INTERSECT BETWEEN LINE(Y=YY) 
C               AND TRIANGLE IN X-Y PLAIN
C
C
C
C[INPUT]
      REAL*4    COORD(3,2),DD,YY
      INTEGER*4 NN
C[OUTPUT]
      INTEGER*4 IERR
      REAL*4    X1,X2
C[LOCAL]
      INTEGER*4 IMAX,I1,I2
      REAL*4    XA,XB,XC,YA,YB,YC,XD,XE,XF
      REAL*4    DXAB,DXAC,DXBC,DYAB,DYAC,DYBC,DMAX
      REAL*4    C1,C2,C3
      REAL*4    EPS0,EPS
      DATA EPS0 /1.0E-4/ 
C
      EPS=DD/1.0E2
CC
CCYY [1]
CC
      XA=COORD(1,1)
      YA=COORD(1,2)
      XB=COORD(2,1)
      YB=COORD(2,2)
      XC=COORD(3,1)
      YC=COORD(3,2)
C
      DXAB=XA-XB
      DXAC=XA-XC
      DXBC=XB-XC
      DYAB=YA-YB
      DYAC=YA-YC
      DYBC=YB-YC
      DMAX=MAX(DXAB,DXAC,DXBC,DYAB,DYAC,DYBC)
      IF(DMAX.LE.DD*FLOAT(NN)) THEN
          IF(ABS(YY-YA).LE.DD*FLOAT(NN)) THEN
              X1=MIN(XA,XB,XC)
              X2=MAX(XA,XB,XC)
          ELSE
              X1= 1.0E2
              X2=-1.0E2
          ENDIF
          GOTO 1000
      ENDIF
C
      IF(    (ABS(DYAB).LE.EPS0 .AND. ABS(DYAC).LE.EPS0)
     *   .OR.(ABS(DYAB).LE.EPS0 .AND. ABS(DYBC).LE.EPS0)
     *   .OR.(ABS(DYAC).LE.EPS0 .AND. ABS(DYBC).LE.EPS0)
     *   ) THEN
          IF(ABS(YY-YA).LE.DD*FLOAT(NN)) THEN
              X1=MIN(XA,XB,XC)
              X2=MAX(XA,XB,XC)
          ELSE
              X1= 1.0E2
              X2=-1.0E2
          ENDIF
          GOTO 1000
      ENDIF   
C
      C1=1.0E2
      C2=1.0E2
      C3=1.0E2
C
      IF(ABS(YA-YB).GE.EPS) C1=(YA-YY)/(YA-YB)
      IF(ABS(YA-YC).GE.EPS) C2=(YA-YY)/(YA-YC)
      IF(ABS(YB-YC).GE.EPS) C3=(YB-YY)/(YB-YC)
C
      XD=XA+C1*(XB-XA)
      XE=XA+C2*(XC-XA)
      XF=XB+C3*(XC-XB)
CC
CCYY [2]CAL. XAB, XAC
CC
      IF(       -EPS.LE.C1 .AND. C1.LE.1.0+EPS 
     *    .AND. -EPS.LE.C2 .AND. C2.LE.1.0+EPS ) THEN
          X1=MIN(XD,XE)
          X2=MAX(XD,XE)
C
      ELSE IF(  -EPS.LE.C1 .AND. C1.LE.1.0+EPS 
     *    .AND. -EPS.LE.C3 .AND. C3.LE.1.0+EPS ) THEN
          X1=MIN(XD,XF)
          X2=MAX(XD,XF)
C
      ELSE IF(  -EPS.LE.C2 .AND. C2.LE.1.0E0+EPS 
     *    .AND. -EPS.LE.C3 .AND. C3.LE.1.0E0+EPS ) THEN
          X1=MIN(XE,XF)
          X2=MAX(XE,XF)
C
      ELSE
          X1= 1.0E2
          X2=-1.0E2
      ENDIF
C
 1000 CONTINUE
      X1=X1-DD*FLOAT(NN)
      X2=X2+DD*FLOAT(NN)
C
      RETURN
      END
