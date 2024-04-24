      SUBROUTINE CBC024(ITIME,NG,NP,ILVL1,ILVL2,IPOSI,F,FEQ,
     *                  TAU3,LVEL,FBC3,FBC3W)
      IMPLICIT NONE
      INTEGER*4 ITIME,NG,NP,ILVL1,ILVL2,IPOSI,LVEL(3,NP)
      REAL*8    F  (0:NG+2,0:NG+2,0:NG+2,NP),
     *          FEQ(0:NG+2,0:NG+2,0:NG+2,NP),
     *          FBC3(NP),FBC3W(NP),TAU3(3)
      INTEGER*4 I,J,K,I1,J1,K1,I2,J2,K2,I3,J3,K3,
     *          IMODE,IOFFST,JOFFST,KOFFST,N1,IP,JP
      REAL*8    BUF(NP),COEF,FBUF,FBUF2
C
C     ====FUNCTION====
C     SET B.C. DATA TO POINT-TYPE B.C. WITH B.C. FLAG-TYPE OF 024
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
C     FBC3 (IP)     :WORK REGION FOR FACE-TYPE  B.C. GROUPS
C     FBC3W(IP)     :WORK REGION FOR FACE-TYPE  B.C. GROUPS, TIME INTERPOLATION 
C
C
      IF(ILVL2.EQ.0) THEN
          I=NG
          J=2
          K=NG
          FBC3(1:NP) = F(I,J,K,1:NP)
      ELSE IF(ILVL2.EQ. 1) THEN
          I=NG-1
          J=3
          K=NG-1
          COEF=2.0D0*(TAU3(3)/TAU3(2))/DBLE(NP)
          DO 2000 IP=1,NP
              FBUF=0.0D0
              DO 2100 JP=1,NP
                  I2=MIN( MAX(I+LVEL(1,JP),1),NG+1 )
                  J2=MIN( MAX(J+LVEL(2,JP),1),NG+1 )
                  K2=MIN( MAX(K+LVEL(3,JP),1),NG+1 )
                  FBUF2=F(I2,J2,K2,IP)-FEQ(I2,J2,K2,IP)
                  FBUF=FBUF+FBUF2
 2100         CONTINUE
              FBC3(IP) = FEQ(I,J,K,IP)+COEF*FBUF
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
              I1=NG+1
              J1=1
              K1=NG+1
          ELSE IF(IPOSI.EQ.2) THEN
              I1=NG/2+1
              J1=1
              K1=NG/2+1
          ELSE IF(IPOSI.EQ.3) THEN
              I1=NG+1
              J1=1
              K1=NG/2+1
          ELSE IF(IPOSI.EQ.4) THEN
              I1=NG+1
              J1=NG/2+1
              K1=NG/2+1 
          ELSE IF(IPOSI.EQ.5) THEN
              I1=NG/2+1
              J1=1
              K1=NG+1 
          ELSE IF(IPOSI.EQ.6) THEN
              I1=NG+1
              J1=NG/2+1
              K1=NG+1 
          ELSE IF(IPOSI.EQ.7) THEN
              I1=NG/2+1
              J1=NG/2+1
              K1=NG+1 
          ENDIF
C
          COEF=0.5D0*(TAU3(1)/TAU3(2))
          DO 3000 IP=1,NP
              FBUF = FEQ(I1,J1,K1,IP)
              FBUF2= F  (I1,J1,K1,IP)
              BUF(IP)=FBUF+COEF*(FBUF2-FBUF) 
 3000     CONTINUE   
          IF(IMODE.EQ.1) THEN
              FBC3 (1:NP) = BUF(1:NP)
              FBC3W(1:NP) = BUF(1:NP)
          ELSE IF(IMODE.EQ.2) THEN
              FBC3 (1:NP) = 0.5D0*(BUF+FBC3W(1:NP))
          ENDIF
      ENDIF
C
 4000 CONTINUE
C
      RETURN
      END
