C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.8.1                                   C
C                                                                      C
C  SUB ROUTINE : CALAXV                                                C
C                                                                      C
C                                       WRITTEN BY H.YOSHIMURA         C
C                                                                      C
C                                                                      C
C CONTACT ADDRESS : IIS, THE UNIVERSITY OF TOKYO, CISS                 C
C                                                                      C
C THERMO-FLUID ANALYSIS SOLVERS FOR LARGE-SCALE-ASSEMBLY               C
C                                                                      C
C======================================================================C
      SUBROUTINE CALAXV(NE,NFACE3,LEFACE,A,AD,S,AS,SWRK,IUT6,IUT0,IERR)
C
      IMPLICIT NONE
C
C     [INPUT]
      INTEGER*4 NE,NFACE3,LEFACE(6,NE)
      REAL*4    A(6,NE),AD(NE),S(NE),SWRK(NFACE3)
C
C     [IN-OUTPUT]
      REAL*4    AS(NE)
      INTEGER*4 IUT6,IUT0,IERR
C
C     [LOCAL]
      REAL*4    S1
      INTEGER*4 IE,IE2,IS,IFACE
C
      DO 1000 IE=1,NE
         AS(IE)=AD(IE)*S(IE)
         DO 1100 IS=1,6
            IE2=LEFACE(IS,IE)
            IF (IE2.EQ.0) GOTO 1100
            IF (IE2.LT.0) THEN
               S1=SWRK(-IE2)
            ELSE
               S1=S(IE2)
            ENDIF
            AS(IE)=AS(IE)+A(IS,IE)*S1
 1100    CONTINUE
 1000 CONTINUE
C
      RETURN
      END
