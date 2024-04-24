      SUBROUTINE FNDBLK(IMODE,NG,NC,LLEVEL,LPOSI,D0,
     *                  NPBLK,NUMBLK,CORBLK,
     *                  MPBLK,LCBLK,LPBLK1,LPBLK2,CBLK,IUT6)
      IMPLICIT NONE
      INTEGER*4 IMODE
      INTEGER*4 NG,NC,LLEVEL(NC),LPOSI(3,NC),NPBLK,NUMBLK(3)
      REAL*8    D0,D1,CORBLK(6)
      INTEGER*4 MPBLK,LCBLK(MPBLK),LPBLK1(3,MPBLK),LPBLK2(3,MPBLK)
      REAL*8    CBLK(3,MPBLK)
      INTEGER*4 IUT6
C
      INTEGER*4 I,J,K,IC,II,JJ,KK,NFOUND,NX,NY,NZ
      REAL*8    X0,Y0,Z0,X1,Y1,Z1,DX,DY,DZ
      REAL*8    X0BLK,Y0BLK,Z0BLK,X1BLK,Y1BLK,Z1BLK,
     *          DXBLK,DYBLK,DZBLK,XX,YY,ZZ
C      
      NX=NUMBLK(1)
      NY=NUMBLK(2)
      NZ=NUMBLK(3)
      X0BLK=CORBLK(1)
      Y0BLK=CORBLK(2)
      Z0BLK=CORBLK(3)
      X1BLK=CORBLK(4)
      Y1BLK=CORBLK(5)
      Z1BLK=CORBLK(6)
      DXBLK=(X1BLK-X0BLK)
      DYBLK=(Y1BLK-Y0BLK)
      DZBLK=(Z1BLK-Z0BLK)
      IF(NX.GT.0) DXBLK=DXBLK/DBLE(NX)
      IF(NY.GT.0) DYBLK=DYBLK/DBLE(NY)
      IF(NZ.GT.0) DZBLK=DZBLK/DBLE(NZ)
C
      NFOUND=0
      DO 1000 K=0,NZ
      DO 1100 J=0,NY
      DO 1200 I=0,NX
           XX=X0BLK+DXBLK*DBLE(I)
           YY=Y0BLK+DYBLK*DBLE(J)
           ZZ=Z0BLK+DZBLK*DBLE(K)
           DO 1300 IC=1,NC
               D1=D0*2.0**(LLEVEL(IC)-1)               
               X0=D0*REAL(LPOSI(1,IC))
               Y0=D0*REAL(LPOSI(2,IC))
               Z0=D0*REAL(LPOSI(3,IC))
               X1=X0+D1
               Y1=Y0+D1
               Z1=Z0+D1
               IF(     X0.LE.XX .AND. XX.LE.X1
     *           .AND. Y0.LE.YY .AND. YY.LE.Y1
     *           .AND. Z0.LE.ZZ .AND. ZZ.LE.Z1 ) THEN
C
                   NFOUND=NFOUND+1
C
                   IF(IMODE.EQ.0) GOTO 1200
C
                   II=INT(REAL(NG)*(XX-X0)/D1)+1
                   JJ=INT(REAL(NG)*(YY-Y0)/D1)+1
                   KK=INT(REAL(NG)*(ZZ-Z0)/D1)+1
                   DX=   (REAL(NG)*(XX-X0)/D1)-REAL(II-1)
                   DY=   (REAL(NG)*(YY-Y0)/D1)-REAL(JJ-1)
                   DZ=   (REAL(NG)*(ZZ-Z0)/D1)-REAL(KK-1)
                   LCBLK (  NFOUND) = IC
                   LPBLK1(1,NFOUND) = II
                   LPBLK1(2,NFOUND) = JJ
                   LPBLK1(3,NFOUND) = KK
                   CBLK  (1,NFOUND) = DX
                   CBLK  (2,NFOUND) = DY
                   CBLK  (3,NFOUND) = DZ
                   LPBLK2(1,NFOUND) = I
                   LPBLK2(2,NFOUND) = J
                   LPBLK2(3,NFOUND) = K
C
                   GOTO 1200
               ENDIF  
 1300      CONTINUE
 1200 CONTINUE   
 1100 CONTINUE   
 1000 CONTINUE   
C
      NPBLK=NFOUND
C
      RETURN
      END
