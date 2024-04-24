C
      SUBROUTINE FNDCUB(NG,NC,LLEVEL,LPOSI,D0,
     *                  MSAMPL,NSAMPL,XX,YY,ZZ,LSAMPL,CSAMPL,
     *                  MRESV,LHIST,COMHST,IUT6)
      IMPLICIT NONE
      INTEGER*4 NG,NC,LLEVEL(NC),LPOSI(3,NC)
      REAL*8    D0,D1
      INTEGER*4 MSAMPL,NSAMPL,LSAMPL(5,MSAMPL),MRESV,LHIST(MSAMPL)
      REAL*8    XX(NSAMPL),YY(NSAMPL),ZZ(NSAMPL),CSAMPL(3,NSAMPL)
      CHARACTER*60 COMHST(MSAMPL)
      INTEGER*4 IUT6
C
      CHARACTER*15 COMDAT(4)
      DATA COMDAT(1) / ' DENSITY      ;' /
      DATA COMDAT(2) / ' VELOCITY-U   ;' /
      DATA COMDAT(3) / ' VELOCITY-V   ;' /
      DATA COMDAT(4) / ' VELOCITY-W   ;' /
C
      INTEGER*4 I,IC,II,JJ,KK,NFOUND
      REAL*8    X0,Y0,Z0,X1,Y1,Z1,DX,DY,DZ
C      
      NFOUND=0
      DO 1000 I=1,NSAMPL
           LSAMPL(2,I) = 0
           LSAMPL(3,I) = 0
           LSAMPL(4,I) = 0
           LSAMPL(5,I) = 0
           CSAMPL(1,I) = 99.0
           CSAMPL(2,I) = 99.0
           CSAMPL(3,I) = 99.0
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
                   LSAMPL(1,NFOUND) = LSAMPL(1,I)
                   LSAMPL(2,NFOUND) = IC
                   LSAMPL(3,NFOUND) = II
                   LSAMPL(4,NFOUND) = JJ
                   LSAMPL(5,NFOUND) = KK
                   CSAMPL(1,NFOUND) = DX
                   CSAMPL(2,NFOUND) = DY
                   CSAMPL(3,NFOUND) = DZ
                   LHIST (MRESV+NFOUND)   = I
C
                   WRITE(IUT6,600) COMDAT(LSAMPL(1,I)),XX(I),YY(I),ZZ(I)
C
                   WRITE(COMHST(MRESV+NFOUND),701)
     *             COMDAT(LSAMPL(1,I)),XX(I),YY(I),ZZ(I)
C
                   GOTO 1000
               ENDIF  
 1100      CONTINUE
 1000 CONTINUE   
C
      NSAMPL=NFOUND
CC    WRITE(IUT6,601)  NSAMPL
C
      RETURN
C
 600  FORMAT( 'FOUND::'  , A15 , 
     *        ' XSMPL =' , 1PE12.5,
     *        ' YSMPL =' , 1PE12.5,
     *        ' ZSMPL =' , 1PE12.5  )
 601  FORMAT( '   FNDCUB NSAMPL   ::', I6 ) 
 701  FORMAT(A15 , '  X=' , F11.5 , '  Y=', F11.5 , '  Z=', F11.5 )
      END
