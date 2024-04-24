      SUBROUTINE FND3EX(N2,NE,NP,NEX,X,Y,Z,NODE,
     *                  XMINE,YMINE,ZMINE,XMAXE,YMAXE,ZMAXE,
     *                  XP,YP,ZP,EN,GP,EP,TP,IUT0,IERR)
      IMPLICIT NONE
C
***** DEFINE ARGUMENTS *****
      INTEGER N2,NE,NP,NEX,NODE
      REAL X,Y,Z
      REAL XMINE,YMINE,ZMINE
      REAL XMAXE,YMAXE,ZMAXE
      REAL XP,YP,ZP
      INTEGER EN
      REAL GP,EP,TP
C
      DIMENSION NODE(N2,NE),NEX(8),X(NP),Y(NP),Z(NP),
     *          XMINE(NE),YMINE(NE),ZMINE(NE),
     *          XMAXE(NE),YMAXE(NE),ZMAXE(NE)
C      
***** OBJECTS *****
      INTEGER JHEX,JWED,JPRD,JTET
      INTEGER IFOUND
      REAL XI(3),DXI(3),XPV(3),DX(3),X1,X2,X3
      REAL XE(3,8),N(8),PSI(3,8),J(3,3),INVJ(3,3)
      INTEGER IUT0,IERR
      INTEGER IE,IT
      INTEGER ITMAX
      INTEGER II,JJ,KK

      REAL*4 TINY
      REAL*4 TMP,DETJ
C
      INTEGER NTMP,NHEX,NPRD,NWED,NTET     
C
***** DATA *****
      DATA IFOUND  /0/
      DATA ITMAX  /10/
C
      DATA TINY/1.0E-4/
C      
      IFOUND=0
      IERR =0
C
      NTET=NEX(5)
      NPRD=NEX(6)
      NWED=NEX(7)
      NHEX=NEX(8)
C
***** MAKE XPV  *****
      XPV(1)=XP
      XPV(2)=YP
      XPV(3)=ZP
C      
************************
***** ELEMENT LOOP *****
************************
      DO 1000 IE=1,NE
C
          IF(     (XP.LT.XMINE(IE)) .OR. (XP.GT.XMAXE(IE))
     *       .OR. (YP.LT.YMINE(IE)) .OR. (YP.GT.YMAXE(IE))
     *       .OR. (ZP.LT.ZMINE(IE)) .OR. (ZP.GT.ZMAXE(IE)) )
     *    GOTO 1000 
C
***** DETERMIN ELEMENT TYPE *****
          JHEX=0
          JWED=0
          JPRD=0
          JTET=0
          IF(NODE(NHEX,IE).NE.0) THEN
              JHEX=1
              XI(1)=0.0
              XI(2)=0.0
              XI(3)=0.0
              NTMP=NHEX
          ELSE IF(NODE(NWED,IE).NE.0) THEN
              JWED=1
              XI(1)=1./3.
              XI(2)=1./3.
              XI(3)=0.0
              NTMP=NWED
          ELSE IF(NODE(NPRD,IE).NE.0) THEN
              JPRD=1
              XI(1)=0.0
              XI(2)=0.0
              XI(3)=1./4.
              NTMP=NPRD
          ELSE IF(NODE(NTET,IE).NE.0) THEN
              JTET=1
              XI(1)=1./4.
              XI(2)=1./4.
              XI(3)=1./4.
              NTMP=NTET
          ELSE
              WRITE(IUT0,*)'INVALID NODE TABLE:ERROR'
              IERR=1      
              RETURN
          ENDIF
C
          DO 1100 II=1,NTMP
              XE(1,II) =X(NODE(II,IE))
              XE(2,II) =Y(NODE(II,IE))
              XE(3,II) =Z(NODE(II,IE))
 1100     CONTINUE
C            
************HEX************
          DO 1200 IT=1,ITMAX    
              IF( JHEX.EQ.1 ) THEN
                   N(1)=0.125*(1.-XI(1))*(1.-XI(2))*(1.-XI(3))
                   N(2)=0.125*(1.+XI(1))*(1.-XI(2))*(1.-XI(3))
                   N(3)=0.125*(1.+XI(1))*(1.+XI(2))*(1.-XI(3))
                   N(4)=0.125*(1.-XI(1))*(1.+XI(2))*(1.-XI(3))
                   N(5)=0.125*(1.-XI(1))*(1.-XI(2))*(1.+XI(3))
                   N(6)=0.125*(1.+XI(1))*(1.-XI(2))*(1.+XI(3))
                   N(7)=0.125*(1.+XI(1))*(1.+XI(2))*(1.+XI(3))
                   N(8)=0.125*(1.-XI(1))*(1.+XI(2))*(1.+XI(3))
              ELSE IF(JWED.EQ.1) THEN
                   N(1)=0.5*XI(1)           *(1.-XI(3))
                   N(2)=0.5*XI(2)           *(1.-XI(3))
                   N(3)=0.5*(1.-XI(1)-XI(2))*(1.-XI(3))
                   N(4)=0.5*XI(1)           *(1.+XI(3))
                   N(5)=0.5*XI(2)           *(1.+XI(3))
                   N(6)=0.5*(1.-XI(1)-XI(2))*(1.+XI(3))
              ELSE IF(JPRD.EQ.1) THEN
                   X1=XI(1)
                   X2=XI(2)
                   X3=XI(3)
                   N(1)=0.25*((1.-X1)*(1.-X2)-X3+X1*X2*X3/(1.-X3))
                   N(2)=0.25*((1.+X1)*(1.-X2)-X3-X1*X2*X3/(1.-X3))
                   N(3)=0.25*((1.+X1)*(1.+X2)-X3+X1*X2*X3/(1.-X3))
                   N(4)=0.25*((1.-X1)*(1.+X2)-X3-X1*X2*X3/(1.-X3))
                   N(5)= X3
              ELSE IF(JTET.EQ.1) THEN
                   N(1)=1.-XI(1)-XI(2)-XI(3)
                   N(2)=XI(1)
                   N(3)=XI(2)
                   N(4)=XI(3)
              ENDIF
C
              DO 1300 II=1,3
                  TMP=0.
                   DO 1310 JJ=1,NTMP
                       TMP =TMP +XE(II,JJ)*N(JJ)
 1310              CONTINUE
                   DX (II)=XPV(II)-TMP
 1300         CONTINUE     
C
              IF( JHEX.EQ.1 ) THEN
                  PSI(1,1)=-0.125*(1.-XI(2))*(1.-XI(3))
                  PSI(1,2)=+0.125*(1.-XI(2))*(1.-XI(3))
                  PSI(1,3)=+0.125*(1.+XI(2))*(1.-XI(3))
                  PSI(1,4)=-0.125*(1.+XI(2))*(1.-XI(3))
                  PSI(1,5)=-0.125*(1.-XI(2))*(1.+XI(3))
                  PSI(1,6)=+0.125*(1.-XI(2))*(1.+XI(3))
                  PSI(1,7)=+0.125*(1.+XI(2))*(1.+XI(3))
                  PSI(1,8)=-0.125*(1.+XI(2))*(1.+XI(3))
C
                  PSI(2,1)=-0.125*(1.-XI(3))*(1.-XI(1))
                  PSI(2,2)=-0.125*(1.-XI(3))*(1.+XI(1))
                  PSI(2,3)=+0.125*(1.-XI(3))*(1.+XI(1))
                  PSI(2,4)=+0.125*(1.-XI(3))*(1.-XI(1))
                  PSI(2,5)=-0.125*(1.+XI(3))*(1.-XI(1))
                  PSI(2,6)=-0.125*(1.+XI(3))*(1.+XI(1))
                  PSI(2,7)=+0.125*(1.+XI(3))*(1.+XI(1))
                  PSI(2,8)=+0.125*(1.+XI(3))*(1.-XI(1))
C
                  PSI(3,1)=-0.125*(1.-XI(1))*(1.-XI(2))
                  PSI(3,2)=-0.125*(1.+XI(1))*(1.-XI(2))
                  PSI(3,3)=-0.125*(1.+XI(1))*(1.+XI(2))
                  PSI(3,4)=-0.125*(1.-XI(1))*(1.+XI(2))
                  PSI(3,5)=+0.125*(1.-XI(1))*(1.-XI(2))
                  PSI(3,6)=+0.125*(1.+XI(1))*(1.-XI(2))
                  PSI(3,7)=+0.125*(1.+XI(1))*(1.+XI(2))
                  PSI(3,8)=+0.125*(1.-XI(1))*(1.+XI(2))
              ELSE IF(JWED.EQ.1) THEN
                  PSI(1,1)=+0.5*(1.-XI(3))
                  PSI(1,2)=0.
                  PSI(1,3)=-0.5*(1.-XI(3))
                  PSI(1,4)=+0.5*(1.+XI(3))
                  PSI(1,5)=0.
                  PSI(1,6)=-0.5*(1.+XI(3))
C
                  PSI(2,1)=0.
                  PSI(2,2)=+0.5*(1.-XI(3))
                  PSI(2,3)=-0.5*(1.-XI(3))
                  PSI(2,4)=0.
                  PSI(2,5)=+0.5*(1.+XI(3))
                  PSI(2,6)=-0.5*(1.+XI(3))
C
                  PSI(3,1)=-0.5*XI(1)
                  PSI(3,2)=-0.5*XI(2)
                  PSI(3,3)=-0.5*(1.-XI(1)-XI(2))
                  PSI(3,4)=+0.5*XI(1)
                  PSI(3,5)=+0.5*XI(2)
                  PSI(3,6)=+0.5*(1.-XI(1)-XI(2))
              ELSE IF(JPRD.EQ.1) THEN
                  PSI(1,1)=0.25*( -1. +XI(2)/(1.-XI(3)) )
                  PSI(1,2)=0.25*( +1. -XI(2)/(1.-XI(3)) )
                  PSI(1,3)=0.25*( +1. +XI(2)/(1.-XI(3)) )
                  PSI(1,4)=0.25*( -1. -XI(2)/(1.-XI(3)) )
                  PSI(1,5)=0.
C
                  PSI(2,1)=0.25*( -1. +XI(1)/(1.-XI(3)) )
                  PSI(2,2)=0.25*( -1. -XI(1)/(1.-XI(3)) )
                  PSI(2,3)=0.25*( +1. +XI(1)/(1.-XI(3)) )
                  PSI(2,4)=0.25*( +1. -XI(1)/(1.-XI(3)) )
                  PSI(2,5)=0.
C
                  PSI(3,1)=0.25*( -1. +XI(1)*XI(2)/(1.-XI(3))**2 )
                  PSI(3,2)=0.25*( -1. -XI(1)*XI(2)/(1.-XI(3))**2 )
                  PSI(3,3)=0.25*( -1. +XI(1)*XI(2)/(1.-XI(3))**2 )
                  PSI(3,4)=0.25*( -1. -XI(1)*XI(2)/(1.-XI(3))**2 )
                  PSI(3,5)=1.
              ELSE IF(JTET.EQ.1) THEN
                  PSI(1,1)=-1.0
                  PSI(1,2)= 1.0
                  PSI(1,3)= 0.0
                  PSI(1,4)= 0.0
C
                  PSI(2,1)=-1.0
                  PSI(2,2)= 0.0
                  PSI(2,3)= 1.0
                  PSI(2,4)= 0.0
C
                  PSI(3,1)=-1.0
                  PSI(3,2)= 0.0
                  PSI(3,3)= 0.0
                  PSI(3,4)= 1.0
              ENDIF
C
              DO 1400 II=1,3
                  DO 1410 JJ=1,3
                      TMP=0.
                      DO 1420 KK=1,NTMP
                          TMP =TMP +PSI(II,KK)*XE(JJ,KK)
 1420                 CONTINUE
                      J(II,JJ)=TMP
 1410             CONTINUE
 1400         CONTINUE
C
              DETJ =J(1,1)*J(2,2)*J(3,3) -J(1,1)*J(2,3)*J(3,2)
     *             +J(1,2)*J(2,3)*J(3,1) -J(1,2)*J(2,1)*J(3,3)
     *             +J(1,3)*J(2,1)*J(3,2) -J(1,3)*J(2,2)*J(3,1)
              INVJ(1,1) =(J(2,2)*J(3,3)-J(2,3)*J(3,2))/DETJ
              INVJ(1,2) =(J(1,3)*J(3,2)-J(1,2)*J(3,3))/DETJ
              INVJ(1,3) =(J(1,2)*J(2,3)-J(1,3)*J(2,2))/DETJ
              INVJ(2,1) =(J(2,3)*J(3,1)-J(2,1)*J(3,3))/DETJ
              INVJ(2,2) =(J(1,1)*J(3,3)-J(1,3)*J(3,1))/DETJ
              INVJ(2,3) =(J(1,3)*J(2,1)-J(1,1)*J(2,3))/DETJ
              INVJ(3,1) =(J(2,1)*J(3,2)-J(2,2)*J(3,1))/DETJ
              INVJ(3,2) =(J(1,2)*J(3,1)-J(1,1)*J(3,2))/DETJ
              INVJ(3,3) =(J(1,1)*J(2,2)-J(1,2)*J(2,1))/DETJ
C
              DO 1500 II=1,3
                  TMP=0.
                  DO 1510 JJ=1,3
                      TMP =TMP +INVJ(JJ,II)*DX(JJ)
 1510             CONTINUE
                  DXI(II)=TMP
                   XI(II)=XI(II)+DXI(II)
 1500         CONTINUE     
C
              IF( JHEX.EQ.1
     *         .AND. (XI(1).GE.-1-TINY) 
     *         .AND. (XI(1).LE.+1+TINY)
     *         .AND. (XI(2).GE.-1-TINY) 
     *         .AND. (XI(2).LE.+1+TINY)
     *         .AND. (XI(3).GE.-1-TINY) 
     *         .AND. (XI(3).LE.+1+TINY) ) THEN
                  IFOUND=1
              ELSE IF(JWED.EQ.1
     *         .AND. (XI(1).GE. 0-TINY) 
     *         .AND. (XI(1).LE.+1+TINY)
     *         .AND. (XI(2).GE. 0-TINY) 
     *         .AND. (XI(2).LE.+1+TINY)
     *         .AND. (XI(3).GE.-1-TINY) 
     *         .AND. (XI(3).LE.+1+TINY)
     *         .AND. (XI(1)+XI(2).LE.+1+TINY) ) THEN
                  IFOUND=1
              ELSE IF(JPRD.EQ.1
     *         .AND. (XI(1).GE.-1.+XI(3)-TINY) 
     *         .AND. (XI(1).LE. 1.-XI(3)+TINY)
     *         .AND. (XI(2).GE.-1.+XI(3)-TINY) 
     *         .AND. (XI(2).LE. 1.-XI(3)+TINY)
     *         .AND. (XI(3).GE. 0.-TINY) 
     *         .AND. (XI(3).LE. 1.+TINY) ) THEN
                  IFOUND=1
              ELSE IF(JTET.EQ.1
     *         .AND. (XI(1).GE.0.-TINY) 
     *         .AND. (XI(1).LE.1.+TINY)
     *         .AND. (XI(2).GE.0.-TINY) 
     *         .AND. (XI(2).LE.1.+TINY)
     *         .AND. (XI(3).GE.0.-TINY) 
     *         .AND. (XI(3).LE.1.+TINY)
     *         .AND. (XI(1)+XI(2)+XI(3).LE.1+TINY) ) THEN
                  IFOUND=1
              ENDIF
C
***** CHECK FOUND *****
          IF( IFOUND.EQ.1 ) THEN
              EN =IE
              GP =XI(1)
              EP =XI(2)
              TP =XI(3)
              RETURN
          ENDIF
C            
 1200     CONTINUE
 1000 CONTINUE
C      
*********************
***** NOT FOUND *****
*********************
      EN=0
      RETURN
      END
