      SUBROUTINE PARTCL(NP,LVEL,CVEL,LREV,WF)
      IMPLICIT NONE
      INTEGER*4 NP,LVEL(3,NP),LREV(NP)
      REAL*8    CVEL(3,NP),WF(NP)
      INTEGER*4 I,J
C
C[IN]
C  NP       : NUMBER OF PARTICLES     
C
C[OUT]
C  LVEL(NP) : NORMALIZED VELOCITY OF PARTCLES (INTEGER)
C  CVEL(NP) : NORMALIZED VELOCITY OF PARTCLES (FLOAT)
C  LREV(NP) : INVERSE LIST OF PARTICLES
C  WF  (NP) : COEEFICIENTS TO CALCULATE PARTICLE DISTRIBUTION FUNCTION
C
      INTEGER*4 IP
      REAL*8    C1,C2,C3
C
      LVEL(1, 1)= 0; LVEL(2, 1)= 0; LVEL(3, 1)= 0; LREV( 1)= 1
      LVEL(1, 2)= 1; LVEL(2, 2)= 0; LVEL(3, 2)= 0; LREV( 2)= 4
      LVEL(1, 3)= 0; LVEL(2, 3)= 1; LVEL(3, 3)= 0; LREV( 3)= 5
      LVEL(1, 4)=-1; LVEL(2, 4)= 0; LVEL(3, 4)= 0; LREV( 4)= 2
      LVEL(1, 5)= 0; LVEL(2, 5)=-1; LVEL(3, 5)= 0; LREV( 5)= 3
      LVEL(1, 6)= 0; LVEL(2, 6)= 0; LVEL(3, 6)= 1; LREV( 6)= 7
      LVEL(1, 7)= 0; LVEL(2, 7)= 0; LVEL(3, 7)=-1; LREV( 7)= 6
      LVEL(1, 8)= 1; LVEL(2, 8)= 1; LVEL(3, 8)= 1; LREV( 8)=14 
      LVEL(1, 9)=-1; LVEL(2, 9)= 1; LVEL(3, 9)= 1; LREV( 9)=15 
      LVEL(1,10)=-1; LVEL(2,10)=-1; LVEL(3,10)= 1; LREV(10)=12 
      LVEL(1,11)= 1; LVEL(2,11)=-1; LVEL(3,11)= 1; LREV(11)=13 
      LVEL(1,12)= 1; LVEL(2,12)= 1; LVEL(3,12)=-1; LREV(12)=10 
      LVEL(1,13)=-1; LVEL(2,13)= 1; LVEL(3,13)=-1; LREV(13)=11 
      LVEL(1,14)=-1; LVEL(2,14)=-1; LVEL(3,14)=-1; LREV(14)= 8 
      LVEL(1,15)= 1; LVEL(2,15)=-1; LVEL(3,15)=-1; LREV(15)= 9
C
      IF(NP.EQ.27) THEN
      LVEL(1,16)= 1; LVEL(2,16)= 1; LVEL(3,16)= 0; LREV(16)=18
      LVEL(1,17)=-1; LVEL(2,17)= 1; LVEL(3,17)= 0; LREV(17)=19
      LVEL(1,18)=-1; LVEL(2,18)=-1; LVEL(3,18)= 0; LREV(18)=16
      LVEL(1,19)= 1; LVEL(2,19)=-1; LVEL(3,19)= 0; LREV(19)=17
      LVEL(1,20)= 1; LVEL(2,20)= 0; LVEL(3,20)= 1; LREV(20)=22
      LVEL(1,21)=-1; LVEL(2,21)= 0; LVEL(3,21)= 1; LREV(21)=23
      LVEL(1,22)=-1; LVEL(2,22)= 0; LVEL(3,22)=-1; LREV(22)=20
      LVEL(1,23)= 1; LVEL(2,23)= 0; LVEL(3,23)=-1; LREV(23)=21
      LVEL(1,24)= 0; LVEL(2,24)= 1; LVEL(3,24)= 1; LREV(24)=26
      LVEL(1,25)= 0; LVEL(2,25)=-1; LVEL(3,25)= 1; LREV(25)=27
      LVEL(1,26)= 0; LVEL(2,26)=-1; LVEL(3,26)=-1; LREV(26)=24 
      LVEL(1,27)= 0; LVEL(2,27)= 1; LVEL(3,27)=-1; LREV(27)=25 
      ENDIF
C
      DO 1000 IP=1,NP
          C1=DBLE(LVEL(1,IP))
          C2=DBLE(LVEL(2,IP))
          C3=DBLE(LVEL(3,IP))
          CVEL(1,IP)=C1
          CVEL(2,IP)=C2
          CVEL(3,IP)=C3
C         CVEL(4,IP)=C1*C1
C         CVEL(5,IP)=C2*C2
C         CVEL(6,IP)=C3*C3
C         CVEL(7,IP)=C2*C3
C         CVEL(8,IP)=C3*C1
C         CVEL(9,IP)=C1*C2
C
          IF(NP.EQ.15) THEN
              IF(IP.EQ.1) THEN
                  WF(IP)=2.0D0/9.0D0
              ELSE IF (2.LE.IP .AND. IP.LE.7) THEN 
                  WF(IP)=1.0D0/9.0D0
              ELSE IF (8.LE.IP .AND. IP.LE.15) THEN 
                  WF(IP)=1.0D0/72.0D0
              ENDIF
          ELSE IF(NP.EQ.27) THEN
              IF(IP.EQ.1) THEN
                  WF(IP)=64.0D0/216.0D0
              ELSE IF (2.LE.IP .AND. IP.LE.7) THEN 
                  WF(IP)=16.0D0/216.0D0
              ELSE IF (8.LE.IP .AND. IP.LE.15) THEN 
                  WF(IP)=1.0D0/216.0D0
              ELSE IF (16.LE.IP .AND. IP.LE.27) THEN 
                  WF(IP)=4.0D0/216.0D0
              ENDIF
          ENDIF
 1000 CONTINUE
C
      RETURN
      END
