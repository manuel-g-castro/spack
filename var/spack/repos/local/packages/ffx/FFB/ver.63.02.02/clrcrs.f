C=======================================================================
      SUBROUTINE CLRCRS(IPART,A,NP,NCRS,IPCRS,NPP,
     *                  AR,LFIX3D,NUMIP,WEIGHT)
C=======================================================================
      IMPLICIT NONE
      INTEGER*4 IPART,NP,NCRS
      REAL*4  A(NCRS), AR(NP),WEIGHT(NP)
      INTEGER*4 IPCRS(NCRS), NPP(NP),NUMIP(NP)
      INTEGER*4 IP,K,ICRS,IP2,ICRDIG
      INTEGER*4 LFIX3D(NP)
C
      CHARACTER*60 ERMSGC
     & /' ## SUBROUTINE RELAXM: FATAL      ERROR REPORT   ; RETURNED'/
C
C
C
CCC   1. SET WEIGHTING FUNCTION FOR COMPUTING AN INNER PRODUCT
C
C
      CALL USTSTA(71)
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
              IF(LFIX3D(IP).EQ.1.AND.IP.NE.IP2) A(ICRS)=0.0E0
              IF(LFIX3D(IP).EQ.1.AND.IP.EQ.IP2) A(ICRS)=WEIGHT(IP)
C
              ICRDIG=0
              IF(LFIX3D(IP).NE.1.AND.IP.EQ.IP2) THEN
                  ICRDIG = ICRS
              ENDIF
 2100     CONTINUE
 2000 CONTINUE
C
      CALL USTEND(71)
C
      RETURN
      END
C
C=======================================================================
      SUBROUTINE STLCLR(LSTCLR,LSTDGN,NP,NCRS,IPCRS,NPP,LFIX3D)
C=======================================================================
      IMPLICIT NONE
      INTEGER*4 NP,NCRS
      INTEGER*4 LSTCLR(NCRS),LSTDGN(NCRS)
      INTEGER*4 IPCRS(NCRS), NPP(NP),LFIX3D(NP)
C
      INTEGER*4 IP,K,ICRS,IP2
C
      ICRS=0
      DO 1000 IP=1,NP
          DO 1100 K=1,NPP(IP)
              ICRS=ICRS+1
              IP2 =IPCRS(ICRS)
              LSTCLR(ICRS)=0
              IF(LFIX3D(IP).EQ.1.AND.IP.NE.IP2) LSTCLR(ICRS)=-1
              IF(LFIX3D(IP).EQ.1.AND.IP.EQ.IP2) LSTCLR(ICRS)=IP
              LSTDGN(ICRS)=IP
 1100     CONTINUE
 1000 CONTINUE   
C
      RETURN
      END
C
C=======================================================================
      SUBROUTINE CLRCRS2(A,NP,NCRS,LSTCLR,NUMIP)
C=======================================================================
      IMPLICIT NONE
      INTEGER*4 NP,NCRS
      REAL*4  A(NCRS)
      INTEGER*4 LSTCLR(NCRS),NUMIP(NP)
      INTEGER*4 IP,ICRS
C
      CHARACTER*60 ERMSGC
     & /' ## SUBROUTINE RELAXM: FATAL      ERROR REPORT   ; RETURNED'/
C
C
      CALL USTSTA(75)
C
      DO 1000 ICRS=1,NCRS
          IP=LSTCLR(ICRS)  
          IF(IP.EQ.-1) A(ICRS)=0.0E0
          IF(IP.GT. 0) A(ICRS)=1.E0/(FLOAT(NUMIP(IP))+1.E0)
 1000 CONTINUE
C
      CALL USTEND(75)
C
      RETURN
      END
