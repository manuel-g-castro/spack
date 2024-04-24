      SUBROUTINE SETMSK(NG,MASK,MBC,NBC,LBC)
      IMPLICIT NONE
      INTEGER*4 NG,MASK(0:NG+2,0:NG+2,0:NG+2,2)
      INTEGER*4 MBC,NBC,LBC(5,MBC)
C
C    LBC(II,IBC)   ATTRIBUTE DATA OF B.C. GROUPS
C                   II=1 B.C. GROUP ID (1-26) IN AN ADJACENT CUBE
C                   II=2 SUB-DOMAIN NUMBER OF AN ADJACENT CUBE
C                   II=3 CUBE NUMBER OF AN ADJACENT CUBE IN A DOMAIN
C                   II=4 RELATIVE LEVEL OF AN ADJACENT CUBE
C                    (-1: FINE, 0:SAME, 1:COARSE)
C                   II=5 POSITION IN COARSER CUBE
C
      INTEGER*4 I,J,K,ID,ILEVEL,NS,NE,IS,JS,KS,IE,JE,KE,IBC
C
      DO 1000 K =0,NG+2
      DO 1100 J =0,NG+2
      DO 1200 I =0,NG+2
          MASK(I,J,K,1)=0
          MASK(I,J,K,2)=0
 1200 CONTINUE
 1100 CONTINUE
 1000 CONTINUE
C
      DO 2000 K =1,NG+1
      DO 2100 J =1,NG+1
      DO 2200 I =1,NG+1
          MASK(I,J,K,1)=1
          MASK(I,J,K,2)=1
 2200 CONTINUE
 2100 CONTINUE
 2000 CONTINUE
C
      NS=1
      NE=NG+1
      DO 3000 IBC=1,NBC
          ID    =LBC(1,IBC)
          ILEVEL=LBC(4,IBC)
          IF(ILEVEL.NE.1) GOTO 3000
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

          DO 3100 K =KS,KE
          DO 3200 J =JS,JE
          DO 3300 I =IS,IE
              MASK(I,J,K,1)=0
 3300     CONTINUE
 3200     CONTINUE
 3100     CONTINUE
C
 3000 CONTINUE
C
      RETURN
      END
