      SUBROUTINE CBC102(NG,NP,ILEVEL,IPOSI,F,FBC1)
      IMPLICIT NONE
      INTEGER*4 NG,NP,ILEVEL,IPOSI
      REAL*8    F(0:NG+2,0:NG+2,0:NG+2,NP),FBC1(NP,NG+1,NG+1)
      INTEGER*4 I,J,K,I1,J1,K1,I2,J2,K2
C
C     ====FUNCTION====
C     SET B.C. DATA TO F FROM FACE-TYPE B.C. BUFFER WITH B.C. FLAG-TYPE OF 002
C     NOTE THAT B.C. TYPE IS DEFINED AT SEND-SIDE CUBE
C
C     ====VARIABLES LIST====
C
C[IN]
C     NG            :GRID SIZE (2^N)
C     NP            :NUMBER OF PARTICLES (=15)
C     ILEVEL        :LEVEL (RESOLUTION) OF RECIEVE SIDE CUBE
C     FBC1 (II,I,J) WORK REGION FOR FACE-TYPE  B.C. GROUPS
C
C[OUT]
C     F(I,J,K,IP)   :PARTICLE DISTRIBUTION FUNCTION
C
      I=0
      IF(ILEVEL.EQ.-1) I=1
      IF(ILEVEL.EQ.0 .OR. ILEVEL.EQ.-1) THEN
          DO 1000 K=1,NG+1
          DO 1100 J=1,NG+1
              F(I,J,K,1:NP)=FBC1(1:NP,J,K)
 1100     CONTINUE
 1000     CONTINUE         
      ELSE
C
          IF(IPOSI.EQ.1) THEN
              J1=0
              K1=0
          ELSE IF(IPOSI.EQ.2) THEN
              J1=NG/2
              K1=0
          ELSE IF(IPOSI.EQ.3) THEN
              J1=NG/2
              K1=NG/2
          ELSE
              J1=0
              K1=NG/2
          ENDIF
C
          DO 2000 K=1,NG/2+1
          DO 2100 J=1,NG/2+1
              K2=K1+K
              J2=J1+J
              F(I,J2,K2,1:NP)=FBC1(1:NP,J,K)
 2100     CONTINUE
 2000     CONTINUE
C
      ENDIF

      RETURN
      END
