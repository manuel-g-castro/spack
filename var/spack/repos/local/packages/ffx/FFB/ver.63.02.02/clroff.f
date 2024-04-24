      SUBROUTINE CLROFF(MCOLOR,MCPART,NCOLOR,NCPART,LLOOP,
     *                  ME,NE,NP,NEX,NODE,A,
     *                  N1,N2,NBOUN,LBOUN,LWORK,IUT0,IERR)
      IMPLICIT NONE
C
      INTEGER*4 MCOLOR,MCPART,NCOLOR,NCPART,LLOOP,
     *          ME,NE,NP,NEX,NODE,
     *          N1,N2,NBOUN,LBOUN,LWORK,IUT0,IERR
      REAL*4    A,DIJ
C
      INTEGER*4 NETET,NEPRD,NEWED,NEHEX,NTET,NPRD,NWED,NHEX,
     *          IES1,IEE1,IES2,IEE2,IES3,IEE3,IES4,IEE4,IP,IB,I,IE
C
      DIMENSION NCOLOR(4),NCPART(MCOLOR,4),LLOOP(MCPART,MCOLOR,4)
      DIMENSION NODE(N2,NE),A(N1,N2,ME),NEX(8),
     *          LBOUN(NBOUN),LWORK(NP)
C
C          NEX(I)      ; INCLUDES NUMBER OF ELEMENTS AND NUMBER OF LOCAL NODES
C                        AS FOLOOWS
C          NEX(1)      ; NUMBER OF TET.    ELEMENTS
C          NEX(2)      ; NUMBER OF PYRAMID ELEMENTS
C          NEX(3)      ; NUMBER OF WEGDE   ELEMENTS
C          NEX(4)      ; NUMBER OF HEX.    ELEMENTS
C          NEX(5)      ; NUMBER OF LOCAL NODES IN A TET.    ELEMENT (=4)
C          NEX(6)      ; NUMBER OF LOCAL NODES IN A PYRAMID ELEMENT (=5)
C          NEX(7)      ; NUMBER OF LOCAL NODES IN A WEGDE   ELEMENT (=6)
C          NEX(8)      ; NUMBER OF LOCAL NODES IN A HEX.    ELEMENT (=8)
C
      DIMENSION  DIJ(8,8)
      DATA DIJ / 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     &           0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     &           0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     &           0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0,
     &           0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0,
     &           0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0,
     &           0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0,
     &           0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0 /
C
      IERR=0
      NETET=NEX(1)
      NEPRD=NEX(2)
      NEWED=NEX(3)
      NEHEX=NEX(4)
      NE   =NETET+NEPRD+NEWED+NEHEX
C
      NTET=NEX(5)
      NPRD=NEX(6)
      NWED=NEX(7)
      NHEX=NEX(8)
C
C   == TET. ==  
      IES1=1
      IEE1=NETET 
C
C   == PYRAMID ==  
      IES2=NETET+1
      IEE2=NETET+NEPRD
C
C   == WEDGE ==  
      IES3=NETET+NEPRD+1
      IEE3=NETET+NEPRD+NEWED
C
C   == HEX. ==  
      IES4=NETET+NEPRD+NEWED+1
      IEE4=NETET+NEPRD+NEWED+NEHEX 

C
      DO 100 IP=1,NP
          LWORK(IP)=0
  100 CONTINUE
C
      DO 200 IB=1,NBOUN
          IP=LBOUN(IB)
C
          IF(IP.LE.0.OR.IP.GT.NP) THEN
              WRITE(IUT0,*) 'CLROFF : ERROR OCCURED'
              IERR=1
              RETURN
          ENDIF
C
          LWORK(IP)=1
  200 CONTINUE
C
C
CC==== TET. ====     
      DO 311 I=1,NTET
          DO 310 IE=IES1,IEE1
          IP=NODE(I,IE)
          IF(LWORK(IP).EQ.0) GOTO 310
C
          A(I,1,IE)=A(I,1,IE)*DIJ(I,1)
          A(I,2,IE)=A(I,2,IE)*DIJ(I,2)
          A(I,3,IE)=A(I,3,IE)*DIJ(I,3)
          A(I,4,IE)=A(I,4,IE)*DIJ(I,4)
  310     CONTINUE     
  311 CONTINUE     
C
C
CC==== PYRAMID ====     
      DO 321 I=1,NPRD
          DO 320 IE=IES2,IEE2
          IP=NODE(I,IE)
          IF(LWORK(IP).EQ.0) GOTO 320
C
          A(I,1,IE)=A(I,1,IE)*DIJ(I,1)
          A(I,2,IE)=A(I,2,IE)*DIJ(I,2)
          A(I,3,IE)=A(I,3,IE)*DIJ(I,3)
          A(I,4,IE)=A(I,4,IE)*DIJ(I,4)
          A(I,5,IE)=A(I,5,IE)*DIJ(I,5)
  320     CONTINUE     
  321 CONTINUE     
C
C
CC==== PYRAMID ====     
      DO 331 I=1,NWED
          DO 330 IE=IES3,IEE3
          IP=NODE(I,IE)
          IF(LWORK(IP).EQ.0) GOTO 330
C
          A(I,1,IE)=A(I,1,IE)*DIJ(I,1)
          A(I,2,IE)=A(I,2,IE)*DIJ(I,2)
          A(I,3,IE)=A(I,3,IE)*DIJ(I,3)
          A(I,4,IE)=A(I,4,IE)*DIJ(I,4)
          A(I,5,IE)=A(I,5,IE)*DIJ(I,5)
          A(I,6,IE)=A(I,6,IE)*DIJ(I,6)
  330     CONTINUE     
  331 CONTINUE     
C
C
CC==== HEX. ====     
      DO 341 I=1,NHEX
          DO 340 IE=IES4,IEE4
          IP=NODE(I,IE)
          IF(LWORK(IP).EQ.0) GOTO 340
C
          A(I,1,IE)=A(I,1,IE)*DIJ(I,1)
          A(I,2,IE)=A(I,2,IE)*DIJ(I,2)
          A(I,3,IE)=A(I,3,IE)*DIJ(I,3)
          A(I,4,IE)=A(I,4,IE)*DIJ(I,4)
          A(I,5,IE)=A(I,5,IE)*DIJ(I,5)
          A(I,6,IE)=A(I,6,IE)*DIJ(I,6)
          A(I,7,IE)=A(I,7,IE)*DIJ(I,7)
          A(I,8,IE)=A(I,8,IE)*DIJ(I,8)
  340     CONTINUE     
  341 CONTINUE     
C
      RETURN
      END
