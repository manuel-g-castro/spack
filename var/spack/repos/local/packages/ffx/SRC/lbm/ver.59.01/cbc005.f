      SUBROUTINE CBC005(ITIME,NG,NP,ILVL1,ILVL2,IPOSI,F,FEQ,
     *                  TAU3,LVEL,FBC1,FBC1W)
      IMPLICIT NONE
      INTEGER*4 ITIME,NG,NP,ILVL1,ILVL2,IPOSI,LVEL(3,NP)
      REAL*8    F  (0:NG+2,0:NG+2,0:NG+2,NP),
     *          FEQ(0:NG+2,0:NG+2,0:NG+2,NP),
     *          FBC1(NP,NG+1,NG+1),FBC1W(NP,NG+1,NG+1),TAU3(3)
      INTEGER*4 I,J,K,I1,J1,K1,I2,J2,K2,I3,J3,K3,
     *          IMODE,IOFFST,JOFFST,KOFFST,N1,IP,JP
      REAL*8    BUF(NP),COEF,FBUF,FBUF2
C
C     ====FUNCTION====
C     SET B.C. DATA TO FACE-TYPE B.C. WITH B.C. FLAG-TYPE OF 005
C
C     ====VARIABLES LIST====
C[IN]
C     ITIME         :TIME STEP
C     NG            :GRID SIZE (2^N)
C     NP            :NUMBER OF PARTICLES (=15)
C     ILVL1         :LEVEL (RESOLUTION) OF SELF-CUBE
C     ILVL2         :RELATIVE LEVEL (RESOLUTION) OF ADJACENT CUBE
C     IPOSI         :RELATIVE POSITION OF  ADJACENT CUBE  
C     F  (I,J,K,IP) :PARTICLE DISTRIBUTION FUNCTION
C     FEQ(I,J,K,IP) :PARTICLE EQUILIBRIUM DISTRIBUTION FUNCTION
C     TAU3(1)       :RELAXTATION TIME OF A FINER   CUBE  
C     TAU3(2)       :RELAXTATION TIME OF A SELF    CUBE  
C     TAU3(3)       :RELAXTATION TIME OF A COARSER CUBE  
C
C[OUT]
C     FBC1 (IP,J,K) :WORK REGION FOR FACE-TYPE  B.C. GROUPS
C     FBC1W(IP,J,K) :WORK REGION FOR FACE-TYPE  B.C. GROUPS, TIME INTERPOLATION 
C
C
      IF(ILVL2.EQ.0) THEN
          K=2
          DO 1000 J=1,NG+1
          DO 1100 I=1,NG+1
              FBC1(1:NP,I,J) = F(I,J,K,1:NP)
 1100     CONTINUE
 1000     CONTINUE
      ELSE IF(ILVL2.EQ. 1) THEN
          K=3
          COEF=2.0D0*(TAU3(3)/TAU3(2))/DBLE(NP)
          DO 2000 J=1,NG+1,2
          DO 2100 I=1,NG+1,2
          DO 2200 IP=1,NP
              FBUF=0.0D0
              DO 2300 JP=1,NP
                  I2=MIN( MAX(I+LVEL(1,JP),1),NG+1 )
                  J2=MIN( MAX(J+LVEL(2,JP),1),NG+1 )
                  K2=MIN( MAX(K+LVEL(3,JP),1),NG+1 )
                  FBUF2=F(I2,J2,K2,IP)-FEQ(I2,J2,K2,IP)
                  FBUF=FBUF+FBUF2
 2300         CONTINUE
              FBC1(IP,(I+1)/2,(J+1)/2) = FEQ(I,J,K,IP)+COEF*FBUF
 2200     CONTINUE
 2100     CONTINUE
 2000     CONTINUE
      ELSE IF(ILVL2.EQ.-1) THEN
          N1=2**(ILVL1-1)
          IMODE=0
          IF(MOD(ITIME+1,N1).EQ.1) THEN
              IMODE=1
          ELSE IF(MOD(ITIME,N1).EQ.1) THEN
              IMODE=2
          ENDIF
C
          IF(IPOSI.EQ.1) THEN
              IOFFST=0
              JOFFST=0
          ELSE IF (IPOSI.EQ.2) THEN
              IOFFST=NG/2
              JOFFST=0
          ELSE IF (IPOSI.EQ.3) THEN
              IOFFST=NG/2
              JOFFST=NG/2
          ELSE IF (IPOSI.EQ.4) THEN
              IOFFST=0
              JOFFST=NG/2
          ENDIF
C
          K1=1
          K2=2
          K3=3
          COEF=0.5D0*(TAU3(1)/TAU3(2))
          DO 3200 J=1,NG+1
          DO 3300 I=1,NG+1
              IF(MOD(I,2).EQ.1) THEN
                  I1=IOFFST+(I+1)/2
                  I2=I1
              ELSE
                  I1=IOFFST+(I+0)/2
                  I2=I1+1
              ENDIF
              IF(MOD(J,2).EQ.1) THEN
                  J1=JOFFST+(J+1)/2
                  J2=J1
              ELSE
                  J1=JOFFST+(J+0)/2
                  J2=J1+1
              ENDIF
              DO 3400 IP=1,NP
                  FBUF
     *            = ( FEQ(I1,J1,K1,IP)+FEQ(I2,J1,K1,IP)
     *               +FEQ(I1,J2,K1,IP)+FEQ(I2,J2,K1,IP))/4.0E0
                  FBUF2
     *            = ( F(I1,J1,K1,IP)+F(I2,J1,K1,IP)
     *               +F(I1,J2,K1,IP)+F(I2,J2,K1,IP))/4.0E0
                  BUF(IP)=FBUF+COEF*(FBUF2-FBUF) 
 3400         CONTINUE   
              IF(IMODE.EQ.1) THEN
                  FBC1 (1:NP,I,J) = BUF(1:NP)
                  FBC1W(1:NP,I,J) = BUF(1:NP)
              ELSE IF(IMODE.EQ.2) THEN
                  FBC1 (1:NP,I,J) = 0.5D0*(BUF+FBC1W(1:NP,I,J))
              ENDIF
 3300     CONTINUE
 3200     CONTINUE
      ENDIF
C
 4000 CONTINUE
C
      RETURN
      END
