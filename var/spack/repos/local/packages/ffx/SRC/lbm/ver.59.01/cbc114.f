      SUBROUTINE CBC114(NG,NP,ILEVEL,IPOSI,F,FBC2)
      IMPLICIT NONE
      INTEGER*4 NG,NP,ILEVEL,IPOSI
      REAL*8    F(0:NG+2,0:NG+2,0:NG+2,NP),FBC2(NP,NG+1)
      INTEGER*4 I,J,K,I1,J1,K1,I2,J2,K2
C
C     ====FUNCTION====
C     SET B.C. DATA TO F FROM EDGE-TYPE B.C. BUFFER WITH B.C. FLAG-TYPE OF 014
C     NOTE THAT B.C. TYPE IS DEFINED AT SEND-SIDE CUBE
C
C     ====VARIABLES LIST====
C
C[IN]
C     NG            :GRID SIZE (2^N)
C     NP            :NUMBER OF PARTICLES (=15)
C     ILEVEL        :LEVEL (RESOLUTION) OF RECIEVE SIDE CUBE
C     FBC2 (II,I)   :WORK REGION FOR FACE-TYPE  B.C. GROUPS
C
C[OUT]
C     F(I,J,K,IP)   :PARTICLE DISTRIBUTION FUNCTION
C
      I=NG+2
      K=0
      IF(ILEVEL.EQ.-1) I=NG+1
      IF(ILEVEL.EQ.-1) K=1
      IF(ILEVEL.EQ.0 .OR. ILEVEL.EQ.-1) THEN
          DO 1000 J=1,NG+1
              F(I,J,K,1:NP)=FBC2(1:NP,J)
 1000     CONTINUE         
      ELSE
C
          IF(IPOSI.EQ.1) THEN
              J1=0
          ELSE IF(IPOSI.EQ.2) THEN
              J1=NG/2
          ELSE
              RETURN
          ENDIF
C
          DO 2000 J=1,NG/2+1
              J2=J1+J
              F(I,J2,K,1:NP)=FBC2(1:NP,J)
 2000     CONTINUE
C
      ENDIF

      RETURN
      END
