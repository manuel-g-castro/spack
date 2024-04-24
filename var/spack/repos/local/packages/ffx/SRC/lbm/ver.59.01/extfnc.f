      SUBROUTINE EXTFNC(NG,NP,MBC,NBC,LBC,F)
      IMPLICIT NONE
      INTEGER*4 NG,NP
      INTEGER*4 MBC,NBC,LBC(5,MBC)
      REAL*8    F(0:NG+2,0:NG+2,0:NG+2,NP)
C
C    LBC(II,IBC)   ATTRIBUTE DATA OF B.C. GROUPS
C                   II=1 B.C. GROUP ID (1-26) IN AN ADJACENT CUBE
C                   II=2 SUB-DOMAIN NUMBER OF AN ADJACENT CUBE
C                   II=3 CUBE NUMBER OF AN ADJACENT CUBE IN A DOMAIN
C                   II=4 RELATIVE LEVEL OF AN ADJACENT CUBE
C                    (-1: FINE, 0:SAME, 1:COARSE)
C                   II=5 POSITION IN COARSER CUBE
C
      INTEGER*4 LSHIFT(3,26)
      DATA LSHIFT /-1,0,0, 1,0,0, 0,-1,0, 0,1,0, 0,0,-1, 0,0,1,   
C
     *             0,-1,-1, 0,1,-1, 0,1,1, 0,-1,1,
     *             -1,0,-1, 1,0,-1, 1,0,1, -1,0,1,
     *             -1,-1,0, 1,-1,0, 1,1,0, -1,1,0,
C
     *             -1,-1,-1, 1,-1,-1, 1,1,-1, -1,1,-1,
     *             -1,-1, 1, 1,-1, 1, 1,1, 1, -1,1, 1/
C
C
      INTEGER*4 I,J,K,I2,J2,K2,IBC,IP,ID,ILEVEL,NS,NE,IS,JS,KS,IE,JE,KE
C
      NS=1
      NE=NG+1
      DO 1000 IBC=1,NBC
          ID    =LBC(1,IBC)
          ILEVEL=LBC(4,IBC)
          IF(ILEVEL.NE.1) GOTO 1000
C
          IF(ID.EQ.1) THEN
              IS=NS;IE=NS;JS=NS;JE=NE;KS=NS;KE=NE
          ELSE IF(ID.EQ.2) THEN
              IS=NE;IE=NE;JS=NS;JE=NE;KS=NS;KE=NE
          ELSE IF(ID.EQ.3) THEN
              IS=NS;IE=NE;JS=NS;JE=NS;KS=NS;KE=NE
          ELSE IF(ID.EQ.4) THEN
              IS=NS;IE=NE;JS=NE;JE=NE;KS=NS;KE=NE
          ELSE IF(ID.EQ.5) THEN
              IS=NS;IE=NE;JS=NS;JE=NE;KS=NS;KE=NS
          ELSE IF(ID.EQ.6) THEN
              IS=NS;IE=NE;JS=NS;JE=NE;KS=NE;KE=NE
          ELSE IF(ID.EQ.7) THEN
              IS=NS;IE=NE;JS=NS;JE=NS;KS=NS;KE=NS
          ELSE IF(ID.EQ.8) THEN
              IS=NS;IE=NE;JS=NE;JE=NE;KS=NS;KE=NS
          ELSE IF(ID.EQ.9) THEN
              IS=NS;IE=NE;JS=NE;JE=NE;KS=NE;KE=NE
          ELSE IF(ID.EQ.10) THEN
              IS=NS;IE=NE;JS=NS;JE=NS;KS=NE;KE=NE
          ELSE IF(ID.EQ.11) THEN
              IS=NS;IE=NS;JS=NS;JE=NE;KS=NS;KE=NS
          ELSE IF(ID.EQ.12) THEN
              IS=NE;IE=NE;JS=NS;JE=NE;KS=NS;KE=NS
          ELSE IF(ID.EQ.13) THEN
              IS=NE;IE=NE;JS=NS;JE=NE;KS=NE;KE=NE
          ELSE IF(ID.EQ.14) THEN
              IS=NS;IE=NS;JS=NS;JE=NE;KS=NE;KE=NE
          ELSE IF(ID.EQ.15) THEN
              IS=NS;IE=NS;JS=NS;JE=NS;KS=NS;KE=NE
          ELSE IF(ID.EQ.16) THEN
              IS=NE;IE=NE;JS=NS;JE=NS;KS=NS;KE=NE
          ELSE IF(ID.EQ.17) THEN
              IS=NE;IE=NE;JS=NE;JE=NE;KS=NS;KE=NE
          ELSE IF(ID.EQ.18) THEN
              IS=NS;IE=NS;JS=NE;JE=NE;KS=NS;KE=NE
          ELSE IF(ID.EQ.19) THEN
              IS=NS;IE=NS;JS=NS;JE=NS;KS=NS;KE=NS
          ELSE IF(ID.EQ.20) THEN
              IS=NE;IE=NE;JS=NS;JE=NS;KS=NS;KE=NS
          ELSE IF(ID.EQ.21) THEN
              IS=NE;IE=NE;JS=NE;JE=NE;KS=NS;KE=NS
          ELSE IF(ID.EQ.22) THEN
              IS=NS;IE=NS;JS=NE;JE=NE;KS=NS;KE=NS
          ELSE IF(ID.EQ.23) THEN
              IS=NS;IE=NS;JS=NS;JE=NS;KS=NE;KE=NE
          ELSE IF(ID.EQ.24) THEN
              IS=NE;IE=NE;JS=NS;JE=NS;KS=NE;KE=NE
          ELSE IF(ID.EQ.25) THEN
              IS=NE;IE=NE;JS=NE;JE=NE;KS=NE;KE=NE
          ELSE IF(ID.EQ.26) THEN
              IS=NS;IE=NS;JS=NE;JE=NE;KS=NE;KE=NE
          ENDIF

          DO 1100 IP=1,NP
          DO 1200 K =KS,KE
          DO 1300 J =JS,JE
          DO 1400 I =IS,IE
              I2=I+LSHIFT(1,ID)
              J2=J+LSHIFT(2,ID)
              K2=K+LSHIFT(3,ID)
              F(I2,J2,K2,IP)=F(I,J,K,IP)
 1400     CONTINUE
 1300     CONTINUE
 1200     CONTINUE
 1100     CONTINUE
C
 1000 CONTINUE
C
      RETURN
      END
