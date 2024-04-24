      SUBROUTINE SETGRD(IMODE,MP,NP,A,B,C,D,XP,YP,ZP)
      IMPLICIT NONE
      INTEGER*4 IMODE,MP,NP 
      REAL*4 A(3),B(3),C(3),D
      REAL*4 XP(MP),YP(MP),ZP(MP)
C
      INTEGER*4 I,J,NPX,NPY,NPZ
      REAL*4    VN(3),VNABS,AB(3),AC(3),XMIN,YMIN,ZMIN,XLEN,YLEN,ZLEN, 
     *          XL,YL,ZL,PX,PY,PZ,X12,X23,X31,Y12,Y23,Y31,Z12,Z23,Z31,
     *          CP1,CP2,CP3 
C
      XMIN = MIN(A(1), B(1), C(1))
      YMIN = MIN(A(2), B(2), C(2))
      ZMIN = MIN(A(3), B(3), C(3))
      X12  = A(1) - B(1)
      X23  = B(1) - C(1)
      X31  = C(1) - A(1)
      XLEN = MAX(ABS(X12), ABS(X23), ABS(X31))
      Y12  = A(2) - B(2)
      Y23  = B(2) - C(2)
      Y31  = C(2) - A(2)
      YLEN = MAX(ABS(Y12), ABS(Y23), ABS(Y31))
      Z12  = A(3) - B(3)
      Z23  = B(3) - C(3)
      Z31  = C(3) - A(3)
      ZLEN = MAX(ABS(Z12), ABS(Z23), ABS(Z31))
C
      AB(1)=B(1)-A(1) 
      AB(2)=B(2)-A(2) 
      AB(3)=B(3)-A(3) 
      AC(1)=C(1)-A(1) 
      AC(2)=C(2)-A(2) 
      AC(3)=C(3)-A(3) 
      VN(1)=AB(2)*AC(3)-AB(3)*AC(2)
      VN(2)=AB(3)*AC(1)-AB(1)*AC(3)
      VN(3)=AB(1)*AC(2)-AB(2)*AC(1)
      VNABS=VN(1)*VN(1)+VN(2)*VN(2)+VN(3)*VN(3)
      IF(VNABS.NE.0.0E0) THEN
          VNABS=SQRT(VNABS)
          VN(1)=VN(1)/VNABS
          VN(2)=VN(2)/VNABS
          VN(3)=VN(3)/VNABS
      ENDIF  
C
      NP=0
      IF (VN(2).EQ.1.0 .OR. VN(2).EQ.-1.0) THEN
          XL  = D
          YL  = 0
          ZL  = D
          NPX = NINT(XLEN/XL)
          NPZ = NINT(ZLEN/ZL)
          DO 1000 I = 0, NPX
              PX = XMIN + I*XL
              DO 1100 J = 0, NPZ
                  PZ = ZMIN + J*ZL
                  CP1 = X12*(PZ-A(3)) - Z12*(PX-A(1))
                  CP2 = X23*(PZ-B(3)) - Z23*(PX-B(1))
                  CP3 = X31*(PZ-C(3)) - Z31*(PX-C(1))
                  IF ((CP1.GE.0 .AND. CP2.GE.0 .AND. CP3.GE.0).OR.
     *                (CP1.LE.0 .AND. CP2.LE.0 .AND. CP3.LE.0)) THEN
                      PY = A(2)
                      NP = NP + 1
                      IF(IMODE.EQ.2) THEN
                          XP(NP) = PX
                          YP(NP) = PY
                          ZP(NP) = PZ
                      ENDIF
                  ENDIF
 1100          CONTINUE
 1000      CONTINUE
      ELSE IF (VN(1).EQ.1.0 .OR. VN(1).EQ.-1.0) THEN
          XL  = 0
          YL  = D
          ZL  = D
          NPY = NINT(YLEN/YL)
          NPZ = NINT(ZLEN/ZL)
          DO 2000 I = 0, NPY
              PY = YMIN + I*YL
              DO 2100 J = 0, NPZ
                  PZ = ZMIN + J*ZL
                  CP1 = Z12*(PY-A(2)) - Y12*(PZ-A(3))
                  CP2 = Z23*(PY-B(2)) - Y23*(PZ-B(3))
                  CP3 = Z31*(PY-C(2)) - Y31*(PZ-C(3))
                  IF ((CP1.GE.0 .AND. CP2.GE.0 .AND. CP3.GE.0).OR.
     *                (CP1.LE.0 .AND. CP2.LE.0 .AND. CP3.LE.0)) THEN
                      PX = A(1)
                      NP = NP + 1
                      IF(IMODE.EQ.2) THEN
                          XP(NP) = PX
                          YP(NP) = PY
                          ZP(NP) = PZ
                      ENDIF 
                  ENDIF
 2100         CONTINUE
 2000     CONTINUE
      ELSE
          XL  = ABS( (D*VN(3))/SQRT(VN(1)*VN(1)+VN(3)*VN(3)) )
          YL  = ABS( (D*VN(3))/SQRT(VN(2)*VN(2)+VN(3)*VN(3)) )
          NPX = NINT(XLEN/XL)
          NPY = NINT(YLEN/YL)
          DO 3000 I = 0, NPX
              PX = XMIN + I*XL
              DO 3100 J = 0, NPY
                  PY  = YMIN + J*YL
                  CP1 = Y12*(PX-A(1)) - X12*(PY-A(2))
                  CP2 = Y23*(PX-B(1)) - X23*(PY-B(2))
                  CP3 = Y31*(PX-C(1)) - X31*(PY-C(2))
                  IF ((CP1.GE.0 .AND. CP2.GE.0 .AND. CP3.GE.0).OR.
     *                (CP1.LE.0 .AND. CP2.LE.0 .AND. CP3.LE.0)) THEN
                      PZ = A(3) - ((VN(1)*(PX-A(1)) 
     *                            + VN(2)*(PY-A(2)))/VN(3))
                      NP = NP + 1
                      IF(IMODE.EQ.2) THEN
                          XP(NP) = PX
                          YP(NP) = PY
                          ZP(NP) = PZ
                      ENDIF
                  ENDIF
 3100         CONTINUE
 3000     CONTINUE
      ENDIF
C
      NP=NP+1
      IF(IMODE.EQ.2) THEN
          XP(NP) = A(1)
          YP(NP) = A(2)
          ZP(NP) = A(3)
      ENDIF 
      NP=NP+1
      IF(IMODE.EQ.2) THEN
          XP(NP) = B(1)
          YP(NP) = B(2)
          ZP(NP) = B(3)
      ENDIF 
      NP=NP+1
      IF(IMODE.EQ.2) THEN
          XP(NP) = C(1)
          YP(NP) = C(2)
          ZP(NP) = C(3)
      ENDIF 
C
      RETURN
      END
