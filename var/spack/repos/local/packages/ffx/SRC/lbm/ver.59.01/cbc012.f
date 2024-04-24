      SUBROUTINE CBC012(ITIME,NG,NP,ILVL1,ILVL2,IPOSI,F,FEQ,
     *                  TAU3,LVEL,FBC2,FBC2W)
      IMPLICIT NONE
      INTEGER*4 ITIME,NG,NP,ILVL1,ILVL2,IPOSI,LVEL(3,NP)
      REAL*8    F  (0:NG+2,0:NG+2,0:NG+2,NP),
     *          FEQ(0:NG+2,0:NG+2,0:NG+2,NP),
     *          FBC2(NP,NG+1),FBC2W(NP,NG+1),TAU3(3)
      INTEGER*4 I,J,K,I1,J1,K1,I2,J2,K2,I3,J3,K3,
     *          IMODE,IOFFST,JOFFST,KOFFST,N1,IP,JP
      REAL*8    BUF(NP),COEF,FBUF,FBUF2
C
C     ====FUNCTION====
C     SET B.C. DATA TO EDGE-TYPE B.C. WITH B.C. FLAG-TYPE OF 012
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
C     FBC2 (IP,I)   :WORK REGION FOR FACE-TYPE  B.C. GROUPS
C     FBC2W(IP,I)   :WORK REGION FOR FACE-TYPE  B.C. GROUPS, TIME INTERPOLATION 
C
C
      IF(ILVL2.EQ.0) THEN
          I=NG
          K=2
          DO 1000 J=1,NG+1
              FBC2(1:NP,J) = F(I,J,K,1:NP)
 1000     CONTINUE
      ELSE IF(ILVL2.EQ. 1) THEN
          I=NG-1
          K=3
          COEF=2.0D0*(TAU3(3)/TAU3(2))/DBLE(NP)
          DO 2000 J=1,NG+1,2
          DO 2100 IP=1,NP
              FBUF=0.0D0
              DO 2200 JP=1,NP
                  I2=MIN( MAX(I+LVEL(1,JP),1),NG+1 )
                  J2=MIN( MAX(J+LVEL(2,JP),1),NG+1 )
                  K2=MIN( MAX(K+LVEL(3,JP),1),NG+1 )
                  FBUF2=F(I2,J2,K2,IP)-FEQ(I2,J2,K2,IP)
                  FBUF=FBUF+FBUF2
 2200         CONTINUE
              FBC2(IP,(J+1)/2) = FEQ(I,J,K,IP)+COEF*FBUF
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
              JOFFST=0
              I1=NG+1
              K1=1
          ELSE IF (IPOSI.EQ.2) THEN
              JOFFST=NG/2
              I1=NG+1
              K1=1
          ELSE IF (IPOSI.EQ.3) THEN
              JOFFST=0
              I1=NG/2+1
              K1=1
          ELSE IF (IPOSI.EQ.4) THEN
              JOFFST=NG/2
              I1=NG/2+1
              K1=1
          ELSE IF (IPOSI.EQ.5) THEN
              JOFFST=0
              I1=NG+1
              K1=NG/2+1
          ELSE IF (IPOSI.EQ.6) THEN
              JOFFST=NG/2
              I1=NG+1
              K1=NG/2+1
          ENDIF
C
          I2=I1-1
          I3=I2-1
          K2=K1+1
          K3=K2+1
          COEF=0.5D0*(TAU3(1)/TAU3(2))
          DO 3100 J=1,NG+1
              IF(MOD(J,2).EQ.1) THEN
                  J1=JOFFST+(J+1)/2
                  J2=J1
              ELSE
                  J1=JOFFST+(J+0)/2
                  J2=J1+1
              ENDIF
              DO 3400 IP=1,NP
                  FBUF
     *            = 0.5E0*(FEQ(I1,J1,K1,IP)+FEQ(I1,J2,K1,IP))
                  FBUF2
     *            = 0.5E0*(F(I1,J1,K1,IP)+F(I1,J2,K1,IP))
                  BUF(IP)=FBUF+COEF*(FBUF2-FBUF) 
 3400         CONTINUE   
              IF(IMODE.EQ.1) THEN
                  FBC2 (1:NP,J) = BUF(1:NP)
                  FBC2W(1:NP,J) = BUF(1:NP)
              ELSE IF(IMODE.EQ.2) THEN
                  FBC2 (1:NP,J) = 0.5D0*(BUF+FBC2W(1:NP,J))
              ENDIF
 3100     CONTINUE
      ENDIF
C
 4000 CONTINUE
C
      RETURN
      END
