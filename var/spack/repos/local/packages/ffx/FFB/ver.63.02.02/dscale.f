      SUBROUTINE DSCALE(N1,N2,NE,NP,NEX,NODE,A,AD,RHS,IERR)
      IMPLICIT NONE
C
      INTEGER*4 N1,N2,NE,NP,NEX(12),NODE(N2,NE)
      REAL*4    A(N1,N2,NE),AD(NP),RHS(NP)
      INTEGER*4 IERR
C
      INTEGER*4 NETET,NEPRD,NEWED,NEHEX
      INTEGER*4 NTET,NPRD,NWED,NHEX
      INTEGER*4 I,IE,IP,
     *          IES1,IEE1,IES2,IEE2,IES3,IEE3,IES4,IEE4
      REAL*4    COEF

C
      NETET =NEX( 1)
      NEPRD =NEX( 2)
      NEWED =NEX( 3)
      NEHEX =NEX( 4)
      NTET  =NEX( 5)
      NPRD  =NEX( 6)
      NWED  =NEX( 7)
      NHEX  =NEX( 8)
C
C   == TET. ==  
      IES1=1
      IEE1=NETET 
C
C   == PYRAMID ==  
      IES2=NETET+1
      IEE2=NETET+NEPRD
C
C   == WEDGE ==  
      IES3=NETET+NEPRD+1
      IEE3=NETET+NEPRD+NEWED
C
C   == HEX. ==  
      IES4=NETET+NEPRD+NEWED+1
      IEE4=NETET+NEPRD+NEWED+NEHEX 
C
C
C
C          OPERATION COUNTS:  20 FLOP /ELEMENT
C          DATA LOADINGS   :  24 WORDS/ELEMENT
C                           (  0 WORDS CONTIGUOUSLY,
C                             20 WORDS BY STRIDE, AND
C                              4 WORDS BY LIST )
      DO 1000 I=1,NTET
          DO 1100 IE=IES1,IEE1
              IP=NODE(I,IE)
              COEF=AD(IP)
              IF(COEF.EQ.0.0E0) THEN
                  IERR=1
                  RETURN
              ENDIF
              COEF=1.0E0/COEF
              A(I,1,IE)=A(I,1,IE)*COEF
              A(I,2,IE)=A(I,2,IE)*COEF
              A(I,3,IE)=A(I,3,IE)*COEF
              A(I,4,IE)=A(I,4,IE)*COEF
 1100     CONTINUE
 1000 CONTINUE
C
C
C          OPERATION COUNTS:  30 FLOP /ELEMENT
C          DATA LOADINGS   :  35 WORDS/ELEMENT
C                           (  0 WORDS CONTIGUOUSLY,
C                             30 WORDS BY STRIDE, AND
C                              5 WORDS BY LIST )
C
      DO 2000 I=1,NPRD
          DO 2100 IE=IES2,IEE2
              IP=NODE(I,IE)
              COEF=AD(IP)
              IF(COEF.EQ.0.0E0) THEN
                  IERR=1
                  RETURN
              ENDIF
              COEF=1.0E0/COEF
              A(I,1,IE)=A(I,1,IE)*COEF
              A(I,2,IE)=A(I,2,IE)*COEF
              A(I,3,IE)=A(I,3,IE)*COEF
              A(I,4,IE)=A(I,4,IE)*COEF
              A(I,5,IE)=A(I,5,IE)*COEF
 2100     CONTINUE
 2000 CONTINUE
C
C
C          OPERATION COUNTS:  42 FLOP /ELEMENT
C          DATA LOADINGS   :  48 WORDS/ELEMENT
C                           (  0 WORDS CONTIGUOUSLY,
C                             42 WORDS BY STRIDE, AND
C                              6 WORDS BY LIST )
C
      DO 3000 I=1,NWED
          DO 3100 IE=IES3,IEE3
              IP=NODE(I,IE)
              COEF=AD(IP)
              IF(COEF.EQ.0.0E0) THEN
                  IERR=1
                  RETURN
              ENDIF
              COEF=1.0E0/COEF
              A(I,1,IE)=A(I,1,IE)*COEF
              A(I,2,IE)=A(I,2,IE)*COEF
              A(I,3,IE)=A(I,3,IE)*COEF
              A(I,4,IE)=A(I,4,IE)*COEF
              A(I,5,IE)=A(I,5,IE)*COEF
              A(I,6,IE)=A(I,6,IE)*COEF
 3100     CONTINUE
 3000 CONTINUE
C
C
C          OPERATION COUNTS:  72 FLOP /ELEMENT
C          DATA LOADINGS   :  80 WORDS/ELEMENT
C                           (  0 WORDS CONTIGUOUSLY,
C                             72 WORDS BY STRIDE, AND
C                              8 WORDS BY LIST )
C
      DO 4000 I=1,NHEX
          DO 4100 IE=IES4,IEE4
              IP=NODE(I,IE)
              COEF=AD(IP)
              IF(COEF.EQ.0.0E0) THEN
                  IERR=1
                  RETURN
              ENDIF
              COEF=1.0E0/COEF
              A(I,1,IE)=A(I,1,IE)*COEF
              A(I,2,IE)=A(I,2,IE)*COEF
              A(I,3,IE)=A(I,3,IE)*COEF
              A(I,4,IE)=A(I,4,IE)*COEF
              A(I,5,IE)=A(I,5,IE)*COEF
              A(I,6,IE)=A(I,6,IE)*COEF
              A(I,7,IE)=A(I,7,IE)*COEF
              A(I,8,IE)=A(I,8,IE)*COEF
 4100     CONTINUE
 4000 CONTINUE
C
      DO 5000 IP = 1 , NP
          COEF=AD(IP)
          IF(COEF.EQ.0.0E0) THEN
              IERR=1
              RETURN
          ENDIF
          COEF=1.0E0/COEF
          RHS(IP)=RHS(IP)*COEF
 5000 CONTINUE   
C
      RETURN
      END

 
