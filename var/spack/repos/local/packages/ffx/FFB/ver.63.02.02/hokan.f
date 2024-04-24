      SUBROUTINE HOKAN
     &         ( MP,NP,ME,NE,N2,NODE,NEX,IUT0,
     &           VAL,IE,XI1,XI2,XI3,DUM)
C
      IMPLICIT NONE
C
      INTEGER*4 MP,NP,ME,NE,N2
      INTEGER*4 NODE(N2,NE)
      INTEGER*4 NEX(8)
      INTEGER*4 IE,IUT0
      REAL*4    VAL(NP),XI1,XI2,XI3,DUM
C
      INTEGER*4 NETET,NEPRD,NEWED,NEHEX
      INTEGER*4  NTET, NPRD, NWED, NHEX
C
      REAL*4    XI(3)
      REAL*4    N(8)
C
      INTEGER*4 IN
C
C     - START -
C
      NETET=NEX(1)
      NEPRD=NEX(2)
      NEWED=NEX(3)
      NEHEX=NEX(4)
      NTET =NEX(5)
      NPRD =NEX(6)
      NWED =NEX(7)
      NHEX =NEX(8)
C
      DUM=0.0E0
      XI(1)=XI1
      XI(2)=XI2
      XI(3)=XI3
C
      IF(     IE.GE.      1       .AND. IE.LE.NETET            ) THEN
C     - TETRA -
          N(1)=1.-XI(1)-XI(2)-XI(3)
          N(2)=XI(1)
          N(3)=XI(2)
          N(4)=XI(3)
          DO 100 IN=1,NTET
              DUM=DUM+N(IN)*VAL(NODE(IN,IE))
  100     CONTINUE
      ELSE IF(IE.GE.NETET+1       .AND. IE.LE.NETET+NEPRD      ) THEN
C     - PYRAMID -
          N(1) =0.25*( (1.-XI(1))*(1.-XI(2))
     *         -XI(3) +XI(1)*XI(2)*XI(3)/(1.-XI(3)) )
          N(2) =0.25*( (1.+XI(1))*(1.-XI(2))
     *         -XI(3) -XI(1)*XI(2)*XI(3)/(1.-XI(3)) )
          N(3) =0.25*( (1.+XI(1))*(1.+XI(2))
     *         -XI(3) +XI(1)*XI(2)*XI(3)/(1.-XI(3)) )
          N(4) =0.25*( (1.-XI(1))*(1.+XI(2))
     *         -XI(3) -XI(1)*XI(2)*XI(3)/(1.-XI(3)) )
          N(5) = XI(3)
          DO 200 IN=1,NPRD
              DUM=DUM+N(IN)*VAL(NODE(IN,IE))
  200     CONTINUE
      ELSE IF(IE.GE.NETET+NEPRD+1 .AND. IE.LE.NETET+NEPRD+NEWED) THEN   
C     - WEDGE(PRYSM) -
          N(1)=0.5*XI(1)           *(1.-XI(3))
          N(2)=0.5*XI(2)           *(1.-XI(3))
          N(3)=0.5*(1.-XI(1)-XI(2))*(1.-XI(3))
          N(4)=0.5*XI(1)           *(1.+XI(3))
          N(5)=0.5*XI(2)           *(1.+XI(3))
          N(6)=0.5*(1.-XI(1)-XI(2))*(1.+XI(3))
          DO 300 IN=1,NWED
              DUM=DUM+N(IN)*VAL(NODE(IN,IE))
  300     CONTINUE
      ELSE IF(IE.GE.NETET+NEPRD+NEWED+1 .AND. IE.LE.NE         ) THEN
C     - HEX -
          N(1)=0.125*(1.-XI(1))*(1.-XI(2))*(1.-XI(3))
          N(2)=0.125*(1.+XI(1))*(1.-XI(2))*(1.-XI(3))
          N(3)=0.125*(1.+XI(1))*(1.+XI(2))*(1.-XI(3))
          N(4)=0.125*(1.-XI(1))*(1.+XI(2))*(1.-XI(3))
          N(5)=0.125*(1.-XI(1))*(1.-XI(2))*(1.+XI(3))
          N(6)=0.125*(1.+XI(1))*(1.-XI(2))*(1.+XI(3))
          N(7)=0.125*(1.+XI(1))*(1.+XI(2))*(1.+XI(3))
          N(8)=0.125*(1.-XI(1))*(1.+XI(2))*(1.+XI(3))
          DO 400 IN=1,NHEX
              DUM=DUM+N(IN)*VAL(NODE(IN,IE))
  400     CONTINUE
      ELSE
C     - NOT AVAILABLE
          WRITE(IUT0,'(A15)') "ERROR IN HOKAN!"
      END IF
C
      RETURN
      END
