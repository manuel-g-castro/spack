      SUBROUTINE BCFIX2
     &   (ME,MP,NE,NP,N1,N2,NEX,NODE,NPFIX,LPFIX,LWORK,
     &    AAA,B,PN,SN,CM,IUT0,IERR)
C
      IMPLICIT NONE
C
      INTEGER*4 ME,MP,NE,NP,N1,N2,NEX,NODE,NPFIX,LPFIX,LWORK,IUT0,IERR
C
      REAL*4    AAA,B,PN,SN,CM
C
      INTEGER*4 NETET,NEPRD,NEWED,NEHEX,NTET,NPRD,NWED,NHEX,
     *          IES1,IEE1,IES2,IEE2,IES3,IEE3,IES4,IEE4,IP,IB,I,IE
C
      DIMENSION NODE(N2,ME),NEX(8),LPFIX(MP),LWORK(MP)
      DIMENSION AAA(N1,N2,ME)
      DIMENSION B(NP),PN(NP),SN(N1,NE),CM(MP)
C
      NETET=NEX(1)
      NEPRD=NEX(2)
      NEWED=NEX(3)
      NEHEX=NEX(4)
      NE   =NETET+NEPRD+NEWED+NEHEX
      NTET =NEX(5)
      NPRD =NEX(6)
      NWED =NEX(7)
      NHEX =NEX(8)
C     * TET *
      IES1=1
      IEE1=NETET 
C     * PRD *
      IES2=NETET+1
      IEE2=NETET+NEPRD
C     * WED *
      IES3=NETET+NEPRD+1
      IEE3=NETET+NEPRD+NEWED
C     * HEX *
      IES4=NETET+NEPRD+NEWED+1
      IEE4=NETET+NEPRD+NEWED+NEHEX 
C
      DO 500 IP=1,NP
         LWORK(IP)=0
 500  CONTINUE
      DO 501 IB=1,NPFIX
         IP=LPFIX(IB)
         IF(IP.LE.0.OR.IP.GT.NP)THEN
            WRITE(IUT0,*) 'BCFIX : ERROR OCCURED'
            IERR=1
            RETURN
         END IF
         LWORK(IP)=1
 501  CONTINUE   
C
C     === DIRICHLET BOUNDARY CONDITION FOR MATRIX ===
C     * TET *
      DO 101 I=1,NTET
         DO 100 IE=IES1,IEE1
            IP=NODE(I,IE)
            IF(LWORK(IP).GE.1)THEN
               AAA(I,1,IE)=0.0E0
               AAA(I,2,IE)=0.0E0
               AAA(I,3,IE)=0.0E0
               AAA(I,4,IE)=0.0E0
               AAA(I,I,IE)=SN(I,IE)
            END IF
 100     CONTINUE
 101  CONTINUE   
C     * PRD *
      DO 201 I=1,NPRD
         DO 200 IE=IES2,IEE2
            IP=NODE(I,IE)
            IF(LWORK(IP).GE.1)THEN
               AAA(I,1,IE)=0.0E0
               AAA(I,2,IE)=0.0E0
               AAA(I,3,IE)=0.0E0
               AAA(I,4,IE)=0.0E0
               AAA(I,5,IE)=0.0E0
               AAA(I,I,IE)=SN(I,IE)
            END IF
 200     CONTINUE
 201  CONTINUE   
C     * WED *
      DO 301 I=1,NWED
         DO 300 IE=IES3,IEE3
            IP=NODE(I,IE)
            IF(LWORK(IP).GE.1)THEN
               AAA(I,1,IE)=0.0E0
               AAA(I,2,IE)=0.0E0
               AAA(I,3,IE)=0.0E0
               AAA(I,4,IE)=0.0E0
               AAA(I,5,IE)=0.0E0
               AAA(I,6,IE)=0.0E0
               AAA(I,I,IE)=SN(I,IE)
            END IF
 300     CONTINUE
 301  CONTINUE   
C     * HEX *
      DO 401 I=1,NHEX
         DO 400 IE=IES4,IEE4
            IP=NODE(I,IE)
            IF(LWORK(IP).GE.1)THEN
               AAA(I,1,IE)=0.0E0
               AAA(I,2,IE)=0.0E0
               AAA(I,3,IE)=0.0E0
               AAA(I,4,IE)=0.0E0
               AAA(I,5,IE)=0.0E0
               AAA(I,6,IE)=0.0E0
               AAA(I,7,IE)=0.0E0
               AAA(I,8,IE)=0.0E0
               AAA(I,I,IE)=SN(I,IE)
            END IF
 400     CONTINUE
 401  CONTINUE   
C
C     === DIRICHLET BOUNDARY CONDITION FOR RIGHT HAND SIDE VECTOR ===
      DO I=1,NP
         IF(LWORK(I).GE.1)THEN
            B(I)=PN(I)/CM(I)
         END IF
      END DO
C
      RETURN
      END
