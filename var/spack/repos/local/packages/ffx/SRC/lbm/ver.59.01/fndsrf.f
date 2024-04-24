C
      SUBROUTINE FNDSRF(NG,NC,LLEVEL,LPOSI,D0,
     *                  NP,XX,YY,ZZ,
     *                  MPG,NPSURF,LSURF,LPSURF,
     *                  CSURF,IUT6)
      IMPLICIT NONE
      INTEGER*4 NG,NC,LLEVEL(NC),LPOSI(3,NC),NP
      REAL*8    D0,D1
      REAL*4    XX(NP),YY(NP),ZZ(NP)
      INTEGER*4 MPG,NPSURF,LSURF(4,MPG),LPSURF(MPG)
      REAL*8    CSURF(3,MPG)
      INTEGER*4 IUT6
C
      INTEGER*4 I,IC,II,JJ,KK,NFOUND
      REAL*8    X0,Y0,Z0,X1,Y1,Z1,DX,DY,DZ
C      
      NFOUND=0
      DO 1000 I=1,NP
           LSURF(1,I) = 0
           LSURF(2,I) = 0
           LSURF(3,I) = 0
           LSURF(4,I) = 0
           CSURF(1,I) = 99.0
           CSURF(2,I) = 99.0
           CSURF(3,I) = 99.0
           DO 1100 IC=1,NC
               D1=D0*2.0**(LLEVEL(IC)-1)               
               X0=D0*REAL(LPOSI(1,IC))
               Y0=D0*REAL(LPOSI(2,IC))
               Z0=D0*REAL(LPOSI(3,IC))
               X1=X0+D1
               Y1=Y0+D1
               Z1=Z0+D1
               IF(     X0.LE.XX(I) .AND. XX(I).LT.X1
     *           .AND. Y0.LE.YY(I) .AND. YY(I).LT.Y1
     *           .AND. Z0.LE.ZZ(I) .AND. ZZ(I).LT.Z1 ) THEN
C
                   NFOUND=NFOUND+1
C
                   II=INT(REAL(NG)*(XX(I)-X0)/D1)+1
                   JJ=INT(REAL(NG)*(YY(I)-Y0)/D1)+1
                   KK=INT(REAL(NG)*(ZZ(I)-Z0)/D1)+1
                   DX=   (REAL(NG)*(XX(I)-X0)/D1)-REAL(II-1)
                   DY=   (REAL(NG)*(YY(I)-Y0)/D1)-REAL(JJ-1)
                   DZ=   (REAL(NG)*(ZZ(I)-Z0)/D1)-REAL(KK-1)
                   LSURF (1,NFOUND) = IC
                   LSURF (2,NFOUND) = II
                   LSURF (3,NFOUND) = JJ
                   LSURF (4,NFOUND) = KK
                   LPSURF(  NFOUND) = I
                   CSURF (1,NFOUND) = DX
                   CSURF (2,NFOUND) = DY
                   CSURF (3,NFOUND) = DZ
C
                   GOTO 1000
               ENDIF  
 1100      CONTINUE
 1000 CONTINUE   
C
      NPSURF=NFOUND
C
      RETURN
      END
