C=======================================================================
      SUBROUTINE ALE_CLRCRS(A,NP,NCRS,IPCRS,NPP,
     *                      LFIXX,LFIXY,LFIXZ,NUMIP,WEIGHT)
C=======================================================================
      IMPLICIT NONE
      INTEGER*4 NP,NCRS
      REAL*4  A(NCRS*9), WEIGHT(NP)
      INTEGER*4 IPCRS(NCRS), NPP(NP),NUMIP(NP)
      INTEGER*4 IP,K,ICRS,IP2
      INTEGER*4 LFIXX(NP),LFIXY(NP),LFIXZ(NP)
C
C      CHARACTER*60 ERMSGC
C     & /' ## SUBROUTINE RELAXM: FATAL      ERROR REPORT   ; RETURNED'/
C
C
C
CCC   1. SET WEIGHTING FUNCTION FOR COMPUTING AN INNER PRODUCT
C
C
      DO 1100 IP = 1 , NP
          WEIGHT(IP) = 1.E0/(FLOAT(NUMIP(IP))+1.E0)
 1100 CONTINUE
C
      ICRS=0
      DO 2000 IP=1,NP
          DO 2100 K=1,NPP(IP)
              ICRS=ICRS+1
              IP2 =IPCRS(ICRS)
C
              IF(LFIXX(IP).EQ.1) THEN
                 IF (IP.NE.IP2) THEN
                    A((ICRS-1)*9+1)=0.0E0
                    A((ICRS-1)*9+2)=0.0E0
                    A((ICRS-1)*9+3)=0.0E0
                 ELSE IF (IP.EQ.IP2) THEN
                    A((ICRS-1)*9+1)=WEIGHT(IP)
                    A((ICRS-1)*9+2)=0.0E0
                    A((ICRS-1)*9+3)=0.0E0
                 ENDIF
              ENDIF
              IF(LFIXY(IP).EQ.1) THEN
                 IF (IP.NE.IP2) THEN
                    A((ICRS-1)*9+4)=0.0E0
                    A((ICRS-1)*9+5)=0.0E0
                    A((ICRS-1)*9+6)=0.0E0
                 ELSE IF (IP.EQ.IP2) THEN
                    A((ICRS-1)*9+4)=0.0E0
                    A((ICRS-1)*9+5)=WEIGHT(IP)
                    A((ICRS-1)*9+6)=0.0E0
                 ENDIF
              ENDIF
              IF(LFIXZ(IP).EQ.1) THEN
                 IF (IP.NE.IP2) THEN
                    A((ICRS-1)*9+7)=0.0E0
                    A((ICRS-1)*9+8)=0.0E0
                    A((ICRS-1)*9+9)=0.0E0
                 ELSE IF (IP.EQ.IP2) THEN
                    A((ICRS-1)*9+7)=0.0E0
                    A((ICRS-1)*9+8)=0.0E0
                    A((ICRS-1)*9+9)=WEIGHT(IP)
                 ENDIF
              ENDIF
C
 2100     CONTINUE
 2000 CONTINUE   
C
      RETURN
      END
