      SUBROUTINE MODIFQ(NP,NG,NPB1,NPB,LPBOUN,QBOUN,LWORK)
      IMPLICIT NONE
      INTEGER*4 NP,NG,NPB1,NPB,LPBOUN(5,NPB),
     *          LWORK(NP,NG+1,NG+1,NG+1)
      REAL*4    QBOUN(NPB)
      INTEGER*4 IPB,ID,II,JJ,KK,I,J,K,
     *          ID02,ID03,ID04,ID05,ID06,ID07,ID08,
     *          ID09,ID10,ID11,ID12,ID13,ID14,ID15,
     *          ID16,ID17,ID18,ID19,ID20,ID21,ID22,
     *          ID23,ID24,ID25,ID26,ID27
C
      DO 1000 K =1,NG+1
      DO 1100 J =1,NG+1
      DO 1200 I =1,NG+1
      DO 1300 ID=1,NP
          LWORK(ID,I,J,K)=0
 1300 CONTINUE
 1200 CONTINUE
 1100 CONTINUE
 1000 CONTINUE
C
      DO 2000 IPB=NPB1+1,NPB
          II=LPBOUN(1,IPB)
          JJ=LPBOUN(2,IPB)
          KK=LPBOUN(3,IPB)
          ID=LPBOUN(4,IPB)
          LWORK(ID,II,JJ,KK)=IPB
 2000 CONTINUE
C
      DO 3000 K =1,NG+1
      DO 3100 J =1,NG+1
      DO 3200 I =1,NG+1
          ID02=LWORK( 2,I,J,K)
          ID03=LWORK( 3,I,J,K)
          ID04=LWORK( 4,I,J,K)
          ID05=LWORK( 5,I,J,K)
          ID06=LWORK( 6,I,J,K)
          ID07=LWORK( 7,I,J,K)
          ID08=LWORK( 8,I,J,K)
          ID09=LWORK( 9,I,J,K)
          ID10=LWORK(10,I,J,K)
          ID11=LWORK(11,I,J,K)
          ID12=LWORK(12,I,J,K)
          ID13=LWORK(13,I,J,K)
          ID14=LWORK(14,I,J,K)
          ID15=LWORK(15,I,J,K)
C
          IF(ID02.NE.0 .AND. ID04.NE.0) THEN
              QBOUN(ID02)=1.0E0  
              QBOUN(ID04)=1.0E0  
          ENDIF 
C
          IF(ID03.NE.0 .AND. ID05.NE.0) THEN
              QBOUN(ID03)=1.0E0  
              QBOUN(ID05)=1.0E0  
          ENDIF 
C
          IF(ID06.NE.0 .AND. ID07.NE.0) THEN
              QBOUN(ID06)=1.0E0  
              QBOUN(ID07)=1.0E0  
          ENDIF 
C
          IF(ID08.NE.0 .AND. ID14.NE.0) THEN
              QBOUN(ID08)=1.0E0  
              QBOUN(ID14)=1.0E0  
          ENDIF 
C
          IF(ID09.NE.0 .AND. ID15.NE.0) THEN
              QBOUN(ID09)=1.0E0  
              QBOUN(ID15)=1.0E0  
          ENDIF 
C
          IF(ID10.NE.0 .AND. ID12.NE.0) THEN
              QBOUN(ID10)=1.0E0  
              QBOUN(ID12)=1.0E0  
          ENDIF 
C
          IF(ID11.NE.0 .AND. ID13.NE.0) THEN
              QBOUN(ID11)=1.0E0  
              QBOUN(ID13)=1.0E0  
          ENDIF 
C
          IF(NP.EQ.15) GOTO 3200
C
          ID16=LWORK(16,I,J,K)
          ID17=LWORK(17,I,J,K)
          ID18=LWORK(18,I,J,K)
          ID19=LWORK(19,I,J,K)
          ID20=LWORK(20,I,J,K)
          ID21=LWORK(21,I,J,K)
          ID22=LWORK(22,I,J,K)
          ID23=LWORK(23,I,J,K)
          ID24=LWORK(24,I,J,K)
          ID25=LWORK(25,I,J,K)
          ID26=LWORK(26,I,J,K)
          ID27=LWORK(27,I,J,K)
C
          IF(ID16.NE.0 .AND. ID18.NE.0) THEN
              QBOUN(ID16)=1.0E0  
              QBOUN(ID18)=1.0E0  
          ENDIF 
C
          IF(ID17.NE.0 .AND. ID19.NE.0) THEN
              QBOUN(ID17)=1.0E0  
              QBOUN(ID19)=1.0E0  
          ENDIF 
C
          IF(ID20.NE.0 .AND. ID22.NE.0) THEN
              QBOUN(ID20)=1.0E0  
              QBOUN(ID22)=1.0E0  
          ENDIF 
C
          IF(ID21.NE.0 .AND. ID23.NE.0) THEN
              QBOUN(ID21)=1.0E0  
              QBOUN(ID23)=1.0E0  
          ENDIF 
C
          IF(ID24.NE.0 .AND. ID26.NE.0) THEN
              QBOUN(ID24)=1.0E0  
              QBOUN(ID26)=1.0E0  
          ENDIF 
C
          IF(ID25.NE.0 .AND. ID27.NE.0) THEN
              QBOUN(ID25)=1.0E0  
              QBOUN(ID27)=1.0E0  
          ENDIF 
C 
 3200 CONTINUE
 3100 CONTINUE
 3000 CONTINUE
C
      RETURN
      END      
