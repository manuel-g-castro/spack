      SUBROUTINE NNLUMP
     *   ( CTYPE,MCOLOR,MCPART,NCOLOR,NCPART,LLOOP,
     *     ME,MP,NE,NP,NODE,N2,NEX,MELM,
     *     FX,FY,FZ,AX,AY,AZ,BX,BY,BZ,
     *     EAP1,IENP,NEP,MEP )
C
      IMPLICIT NONE
C
CCCC  [INPUT:LOOP]
      INTEGER*4 MCOLOR,MCPART
      INTEGER*4 NCOLOR(4),NCPART(MCOLOR,4),LLOOP(MCPART,MCOLOR,4)
C
      CHARACTER*3 CTYPE
      INTEGER*4 ME,MP,NE,NP,N2,MEP
      INTEGER*4 NODE(N2,NE),NEX(12)
      INTEGER*4 MELM
      REAL*4 EAP1(N2,MEP,NP)
      INTEGER*4 IENP(MEP,MP),NEP(MP)
      REAL*4 FX(NP),FY(NP),FZ(NP)
      REAL*4 AX(NP),AY(NP),AZ(NP),BX(NP),BY(NP),BZ(NP)
C
      INTEGER*4 NETET,NEPRD,NEWED,NEHEX
      REAL*4 RLUMP,FX1,FY1,FZ1
C
      INTEGER*4 I,J,IE,IP,IPE,IP1,K,NN
      INTEGER*4 IEE1,IEE2,IEE3,IEE4
C
C     * START *
C
      NETET=NEX(1)
      NEPRD=NEX(2)
      NEWED=NEX(3)
      NEHEX=NEX(4)
      NE   =NETET+NEPRD+NEWED+NEHEX
C
C     * ZERO *
      DO 10 IP=1,NP
          FX(IP)=0.0E0
          FY(IP)=0.0E0
          FZ(IP)=0.0E0
   10 CONTINUE   
C
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
      IF(CTYPE(1:3).EQ."ONE") THEN
C
          DO IP=1,NP
              FX1 = 0.0E0
              FY1 = 0.0E0
              FZ1 = 0.0E0
              DO IPE=1,NEP(IP)
                  IE =IENP(IPE,IP)
C
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
                  RLUMP=0.0E0
                  DO K=1,NN
                      RLUMP = RLUMP + EAP1(K,IPE,IP)
                  ENDDO
C
                  FX1 = FX1 + RLUMP*AX(IP)
                  FY1 = FY1 + RLUMP*AY(IP)
                  FZ1 = FZ1 + RLUMP*AZ(IP)
              ENDDO
C
              FX(IP) = FX1
              FY(IP) = FY1
              FZ(IP) = FZ1
          ENDDO
C   
      ELSE IF(CTYPE(1:3).EQ."TWO") THEN
C
          DO IP=1,NP
              FX1 = 0.0E0
              FY1 = 0.0E0
              FZ1 = 0.0E0
              DO IPE=1,NEP(IP)
                  IE =IENP(IPE,IP)
C
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
                  RLUMP=0.0E0
                  DO K=1,NN
                      RLUMP = RLUMP + EAP1(K,IPE,IP)
                  ENDDO
C
                  FX1 = FX1 + RLUMP*AX(IP)*BX(IP)
                  FY1 = FY1 + RLUMP*AY(IP)*BY(IP)
                  FZ1 = FZ1 + RLUMP*AZ(IP)*BZ(IP)
              ENDDO
C
              FX(IP) = FX1
              FY(IP) = FY1
              FZ(IP) = FZ1
          ENDDO
C
      ENDIF
C
      RETURN
      END


