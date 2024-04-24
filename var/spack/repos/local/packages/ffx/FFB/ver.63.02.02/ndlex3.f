      SUBROUTINE NDLEX3
     *   (ME,NE,NP,MEP,SNP,IENP,NEP,
     *    IPART,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *    VALELM,VALNOD,CM,IUT0,IERR,BUFSND,BUFRCV,MAXBUF)
C
      IMPLICIT NONE
C
      INTEGER*4 ME,NE,NP,MEP,IENP(MEP,NP),NEP(NP)
      REAL*4    SNP(MEP,NP)
      INTEGER*4 IPART,NDOM,MBPDOM
      INTEGER*4 LDOM(NDOM),NBPDOM(NDOM)
      INTEGER*4 IPSLF(MBPDOM,NDOM),IPSND(MBPDOM,NDOM)
      REAL*4    VALELM(ME),VALNOD(NP),CM(NP)
      INTEGER*4 IUT0,IERR,MAXBUF
      REAL*4    BUFSND(MAXBUF),BUFRCV(MAXBUF)
C
      CHARACTER*60 ERMSGC
     & /' ## SUBROUTINE NODLEX: FATAL      ERROR REPORT   ; RETURNED' /
C
      INTEGER*4 I,IE,IP,IDUM
      REAL*4    BUF
C
C     * START *
C
      CALL USTSTA(76)
C
#ifndef __AIX__
!ocl CACHE_SECTOR_SIZE(1,11)
!ocl CACHE_SUBSECTOR_ASSIGN(VALELM)
#endif
      DO 1000 IP = 1 , NP 
          VALNOD(IP)=0.0E0
          BUF=0.0E0
          DO 1100 I = 1 , 8
              IE=IENP(I,IP)
              BUF=BUF+SNP(I,IP)*VALELM(IE)
 1100     CONTINUE
          VALNOD(IP)=BUF
 1000 CONTINUE
C
      DO 1200 IP = 1 , NP 
          BUF=0.0E0
          IF(NEP(IP).GT.8) THEN
              DO 1300 I = 9 , NEP(IP)
                  IE=IENP(I,IP)
                  BUF=BUF+SNP(I,IP)*VALELM(IE)
 1300         CONTINUE
              VALNOD(IP)=VALNOD(IP)+BUF
          ENDIF 
 1200 CONTINUE
C
C     *** DATA COMMUNICATION ***
C
C
      IDUM=1
      CALL DDCOMX(IPART,IDUM,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     &            VALNOD,VALNOD,VALNOD,NP,IUT0,IERR,
     &            BUFSND,BUFRCV,MAXBUF)
      IF(IERR.NE.0) THEN
         WRITE(IUT0,*)
         WRITE(IUT0,*) ERMSGC
         RETURN
      END IF   
C
C
C     *** INVERSE MASS MATRIX ***
C
C
      DO 200 IP=1,NP
         VALNOD(IP)=VALNOD(IP)*CM(IP)
 200  CONTINUE
C
      CALL USTEND(76)
C
      RETURN
      END
