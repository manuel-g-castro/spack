      SUBROUTINE SETBC3(NG,NP,NC,MPBDOM,F,BUFRCV,IUT6)
      IMPLICIT NONE
C
      INTEGER*4 NG,NP,NC,MPBDOM,IUT6
      REAL*8    F(0:NG+2,0:NG+2,0:NG+2,NP,NC)
      REAL*8    BUFRCV(MPBDOM)
C
      INTEGER*4 IB,NB,II,JC,ID,ILEVEL,IPOSI
C
C     ====FUNCTION====
C     UNPACK DISTRIBUTION FUNCTION TO CUBE BOUNDARY, WHICH ARE RECIEVED
C     FROM OTHE DOMAIN
C
C     ====VARIABLES LIST====
C[IN]
C     NG             :CUBE SIZE (=2^N)
C     NC             :NUMBER OF CUBES IN SUB-DOMAIN
C     NP             :NUMBER OF PARTICLES (=15)
C     MPB             NUMBER OF DATA TO BE RECEIVED FROM THE NEIGHBORING 
C                     SUB-DOMAINS
C    NDOM            :NUMBER OF THE NEIGHBORING SUB-DOMAINS
C    LDOM(IDOM)      :NEIGHBORING SUB-DOMAIN NUMBER
C    WRKSND(IPB,IDOM):WORK REGION FOR RECEIVING THE PARTICLE DISTRIBUTION FUNCTION
C 
C[OUT]
C     F(I,J,K,IP,IC) :PARTICLE DISTRIBUTION FUNCTION
C
      NB=BUFRCV(1)
      II=1
C
      DO 1000 IB=1,NB
          JC    =INT(BUFRCV(II+1))
          ID    =INT(BUFRCV(II+2))
          ILEVEL=INT(BUFRCV(II+3))
          IPOSI =INT(BUFRCV(II+4))
          II=II+4
C
C         FACE-CONNECTION
          IF(ID .EQ.1)
     *    CALL CBC101(NG,NP,ILEVEL,IPOSI,
     *                F(0,0,0,1,JC),BUFRCV(II+1))
          IF(ID.EQ. 2)
     *    CALL CBC102(NG,NP,ILEVEL,IPOSI,
     *                F(0,0,0,1,JC),BUFRCV(II+1))
          IF(ID.EQ. 3)
     *    CALL CBC103(NG,NP,ILEVEL,IPOSI,
     *                F(0,0,0,1,JC),BUFRCV(II+1))
          IF(ID.EQ. 4)
     *    CALL CBC104(NG,NP,ILEVEL,IPOSI,
     *                F(0,0,0,1,JC),BUFRCV(II+1))
          IF(ID.EQ. 5)
     *    CALL CBC105(NG,NP,ILEVEL,IPOSI,
     *                F(0,0,0,1,JC),BUFRCV(II+1))
          IF(ID.EQ. 6)
     *    CALL CBC106(NG,NP,ILEVEL,IPOSI,
     *                F(0,0,0,1,JC),BUFRCV(II+1))
C
C         EDGE-CONNECTION
          IF(ID.EQ. 7)
     *    CALL CBC107(NG,NP,ILEVEL,IPOSI,
     *                F(0,0,0,1,JC),BUFRCV(II+1))
          IF(ID.EQ. 8)
     *    CALL CBC108(NG,NP,ILEVEL,IPOSI,
     *                F(0,0,0,1,JC),BUFRCV(II+1))
          IF(ID.EQ. 9)
     *    CALL CBC109(NG,NP,ILEVEL,IPOSI,
     *                F(0,0,0,1,JC),BUFRCV(II+1))
          IF(ID.EQ.10)
     *    CALL CBC110(NG,NP,ILEVEL,IPOSI,
     *                F(0,0,0,1,JC),BUFRCV(II+1))
          IF(ID.EQ.11)
     *    CALL CBC111(NG,NP,ILEVEL,IPOSI,
     *                F(0,0,0,1,JC),BUFRCV(II+1))
          IF(ID.EQ.12)
     *    CALL CBC112(NG,NP,ILEVEL,IPOSI,
     *                F(0,0,0,1,JC),BUFRCV(II+1))
          IF(ID.EQ.13)
     *    CALL CBC113(NG,NP,ILEVEL,IPOSI,
     *                F(0,0,0,1,JC),BUFRCV(II+1))
          IF(ID.EQ.14)
     *    CALL CBC114(NG,NP,ILEVEL,IPOSI,
     *                F(0,0,0,1,JC),BUFRCV(II+1))
          IF(ID.EQ.15)
     *    CALL CBC115(NG,NP,ILEVEL,IPOSI,
     *                F(0,0,0,1,JC),BUFRCV(II+1))
          IF(ID.EQ.16)
     *    CALL CBC116(NG,NP,ILEVEL,IPOSI,
     *                F(0,0,0,1,JC),BUFRCV(II+1))
          IF(ID.EQ.17)
     *    CALL CBC117(NG,NP,ILEVEL,IPOSI,
     *                F(0,0,0,1,JC),BUFRCV(II+1))
          IF(ID.EQ.18)
     *    CALL CBC118(NG,NP,ILEVEL,IPOSI,
     *                F(0,0,0,1,JC),BUFRCV(II+1))
C
C         POINT-CONNECTION
          IF(ID.EQ.19) 
     *    CALL CBC119(NG,NP,ILEVEL,IPOSI,F(0,0,0,1,JC),BUFRCV(II+1))
          IF(ID.EQ.20) 
     *    CALL CBC120(NG,NP,ILEVEL,IPOSI,F(0,0,0,1,JC),BUFRCV(II+1))
          IF(ID.EQ.21) 
     *    CALL CBC121(NG,NP,ILEVEL,IPOSI,F(0,0,0,1,JC),BUFRCV(II+1))
          IF(ID.EQ.22)
     *    CALL CBC122(NG,NP,ILEVEL,IPOSI,F(0,0,0,1,JC),BUFRCV(II+1))
          IF(ID.EQ.23) 
     *    CALL CBC123(NG,NP,ILEVEL,IPOSI,F(0,0,0,1,JC),BUFRCV(II+1))
          IF(ID.EQ.24) 
     *    CALL CBC124(NG,NP,ILEVEL,IPOSI,F(0,0,0,1,JC),BUFRCV(II+1))
          IF(ID.EQ.25) 
     *    CALL CBC125(NG,NP,ILEVEL,IPOSI,F(0,0,0,1,JC),BUFRCV(II+1))
          IF(ID.EQ.26) 
     *    CALL CBC126(NG,NP,ILEVEL,IPOSI,F(0,0,0,1,JC),BUFRCV(II+1))
C
 1100 CONTINUE      
C
          IF(ID.LE.6) THEN
              II=II+NP*(NG+1)*(NG+1)
          ELSE IF (ID.GE.7 .AND. ID.LE.18) THEN
              II=II+NP*(NG+1)
          ELSE
              II=II+NP
          ENDIF
C
 1000 CONTINUE      
C
      RETURN
      END
