      SUBROUTINE LUMPEX(N2,NE,NP,NEX,MELM,EAP1,IENP,NEP,MEP,MP,CM)
      IMPLICIT NONE
      
***** DEFINE ARGUMENTS *****
      INTEGER N2,NE,NP,NEX(12),MELM,IENP,NEP,MEP,MP
      REAL EAP1, CM
      DIMENSION CM(NP)
      DIMENSION EAP1(N2,MEP,NP)
      DIMENSION IENP(MEP,MP),NEP(MP)
      
***** OBJECTS *****
      INTEGER IP,IN, JN, NN, IE, IEBEG, IEEND
      INTEGER NEHEX, NEWED, NEPRD, NETET
      INTEGER IEE1,IEE2,IEE3,IEE4,IPE,K
      REAL    CM1
C
      NETET=NEX(1)
      NEPRD=NEX(2)
      NEWED=NEX(3)
      NEHEX=NEX(4)
C
      DO 100 IP=1,NP
          CM(IP)=0.0
  100 CONTINUE     
C
C   == TET. ==  
      IEE1=NETET 
C
C   == PYRAMID ==  
      IEE2=NETET+NEPRD
C
C   == WEDGE ==  
      IEE3=NETET+NEPRD+NEWED
C
C   == HEX. ==  
      IEE4=NETET+NEPRD+NEWED+NEHEX 
C
      DO IP=1,NP
          CM1 = 0.0E0
          DO IPE=1,NEP(IP)
              IE =IENP(IPE,IP)
              IF(IE.LE.IEE1) THEN
                  NN=NEX(5)
              ELSE IF (IE.LE.IEE2) THEN
                  NN=NEX(6)
              ELSE IF (IE.LE.IEE3) THEN
                  NN=NEX(7)
              ELSE
                  NN=NEX(8)
              ENDIF
C
              DO K=1,NN
                  CM1 = CM1 + EAP1(K,IPE,IP)
              ENDDO
              CM(IP) = CM1
          ENDDO
      ENDDO
      
      RETURN
      END
