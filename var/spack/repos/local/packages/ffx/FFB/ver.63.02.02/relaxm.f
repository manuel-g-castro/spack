C=======================================================================
      SUBROUTINE RELAXM(IPART,A,ALPHA,NP,NCRS,IPCRS,NPP,AR,
     *                  IUT0,IERR,NUMIP,WEIGHT)
C=======================================================================
      IMPLICIT NONE
      INTEGER IDIAG,NP,NCRS,NE,N
      REAL*4  A(NCRS), AR(NP), ALPHA
      INTEGER IPCRS(NCRS), NPP(NP), NUMIP(NP)
      INTEGER IP, K, ICRS, IP2
      REAL*4  BUF, BUFMAX, BUFSUM, TMPBUF,WEIGHT(NP)
      INTEGER MAXBUF,ICRDIA
C
      INTEGER IPART,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM
C
      INTEGER IDUM,ICRDIG
      INTEGER IUT0,IERR
C
C [INPUT]
C IDIAG : FLAG FOR DIAGONAL TERM (0:MAX, 1:SUMMATION)
C
      CHARACTER*60 ERMSGC
     & /' ## SUBROUTINE RELAXM: FATAL      ERROR REPORT   ; RETURNED'/
C
      IERR=0
C
      DO 1100 IP = 1 , NP
          WEIGHT(IP) = 1.E0/(FLOAT(NUMIP(IP))+1.E0)
 1100 CONTINUE
C
      ICRS=0
      DO 110 IP=1,NP
          ICRDIA = 0
          BUFMAX = 0.0E0
          BUFSUM = 0.0E0
C
          DO 100 K=1,NPP(IP)
              ICRS = ICRS+1
              IP2 = IPCRS(ICRS)
C
              IF(IP.EQ.IP2) THEN
                  ICRDIA = ICRS
              ENDIF
  100     CONTINUE
C          A(ICRDIA) = AR(IP)*WEIGHT(IP)
          IF(ICRDIA.NE.0) A(ICRDIA) = AR(IP)
C          A(ICRDIA) = AR(IP)
  110 CONTINUE   
C
      RETURN
      END
