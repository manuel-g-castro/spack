      SUBROUTINE NNXYZ
     *   ( CTYPE,MCOLOR,MCPART,NCOLOR,NCPART,LLOOP,
     *     ME,MP,NE,NP,NODE,N2,NEX,
     *     MELM,EAP2,IENP,JENP,NEP,MEP,
     *     FX,FY,FZ,VX,VY,VZ )
C
      IMPLICIT NONE
C
CCCC  [INPUT:LOOP]
      INTEGER*4 MCOLOR,MCPART
      INTEGER*4 NCOLOR(4),NCPART(MCOLOR,4),LLOOP(MCPART,MCOLOR,4)
C
      CHARACTER*12 CTYPE
      INTEGER*4 ME,MP,NE,NP,N2
      INTEGER*4 NODE(N2,NE),NEX(12)
      INTEGER*4 MELM,MEP
      REAL*4 EAP2(3,N2,MEP,NP)
      REAL*4 FX(NP),FY(NP),FZ(NP),VX(NP),VY(NP),VZ(NP)
C
      INTEGER*4 IENP(MEP,MP),JENP(MEP,MP),NEP(MP)
      INTEGER*4 NETET,NEPRD,NEWED,NEHEX
      INTEGER*4 NTET ,NPRD, NWED, NHEX
C
      INTEGER*4 I,J,IE,IP,IPE,K,NN
      INTEGER*4 IEE1,IEE2,IEE3,IEE4
C
C     * START *
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
C     * ZERO *
      DO 10 IP=1,NP
          FX(IP)=0.0E0
          FY(IP)=0.0E0
          FZ(IP)=0.0E0
  10  CONTINUE   
C
      IF(CTYPE(1:12).EQ."DIAGONAL    "    ) THEN
C
C        * FX = (N*NX) * VX
C        * FY = (N*NY) * VY
C        * FZ = (N*NZ) * VZ
C
          DO IP=1,NP
!ocl nosimd
!ocl noswp
              DO IPE=1,NEP(IP)
                  IE =IENP(IPE,IP)
                  I  =JENP(IPE,IP)
                  IF(IE.LE.IEE1) THEN
                      NN = NTET
                  ELSE IF (IE.LE.IEE2) THEN
                      NN = NPRD
                  ELSE IF (IE.LE.IEE3) THEN
                      NN = NWED
                  ELSE 
                      NN = NHEX
                  ENDIF
C
                  DO K=1,NN
                      FX(IP) = FX(IP) + EAP2(1,K,IPE,IP)*VX(NODE(K,IE))
                      FY(IP) = FY(IP) + EAP2(2,K,IPE,IP)*VY(NODE(K,IE))
                      FZ(IP) = FZ(IP) + EAP2(3,K,IPE,IP)*VZ(NODE(K,IE))
                  ENDDO
              ENDDO
          ENDDO
C
      ELSE IF(CTYPE(1:12).EQ."OFF-DIAGONAL") THEN
C
C        * FX = ( (N*NY) * VZ + (N*NZ) * VY )/2
C        * FY = ( (N*NZ) * VX + (N*NX) * VZ )/2
C        * FZ = ( (N*NX) * VY + (N*NY) * VX )/2
C
          DO IP=1,NP
!ocl nosimd
!ocl noswp
              DO IPE=1,NEP(IP)
                  IE =IENP(IPE,IP)
                  I  =JENP(IPE,IP)
                  IF(IE.LE.IEE1) THEN
                      NN = NTET
                  ELSE IF (IE.LE.IEE2) THEN
                      NN = NPRD
                  ELSE IF (IE.LE.IEE3) THEN
                      NN = NWED
                  ELSE 
                      NN = NHEX
                  ENDIF
C
                  DO K=1,NN
                      FX(IP) = FX(IP)
     *                     + 0.5E0
     *                * (EAP2(2,K,IPE,IP)*VZ(NODE(K,IE)) 
     *                 + EAP2(3,K,IPE,IP)*VY(NODE(K,IE)))
                      FY(IP) = FY(IP)
     *                     + 0.5E0
     *                * (EAP2(3,K,IPE,IP)*VX(NODE(K,IE)) 
     *                 + EAP2(1,K,IPE,IP)*VZ(NODE(K,IE)))
                      FZ(IP) = FZ(IP)
     *                     + 0.5E0
     *                * (EAP2(1,K,IPE,IP)*VY(NODE(K,IE)) 
     *                 + EAP2(2,K,IPE,IP)*VX(NODE(K,IE)))
                  ENDDO
              ENDDO
          ENDDO
C
      END IF
C
      RETURN
      END
