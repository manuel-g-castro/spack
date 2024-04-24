      Subroutine MULTI_GRAD3X
     *                 (NS,ME,N1,N,NE,NP,NODE,MEP,NEP,IENP,
     *                  S,DNXYZP,CM,FXYZ,RX,RY,MWRK,WRKN,
     *                  NPFIX,LPFIX,LEFIX,NPFREE,LPFREE,
     *                  NPSYMT,LPSYMT,XPSYMT,YPSYMT,ZPSYMT,
     *                  IPART,NDOM,MBPDOM,LDOM,NBPDOM,IPSLF,IPSND,
     *                  IUT0,IERR)
      IMPLICIT NONE
C
CCCC  [INPUT]
      INTEGER*4 NS,ME,N1,N,NE,NP,NODE(N1,NE),IUT0
      INTEGER*4 MEP,NEP(NP),IENP(MEP,NP)
      REAL*4    S(NE,NS),DNXYZP(MEP,3,NP),CM(NP)
C
CCC [INPUT:B.C. NODES]
      INTEGER*4 NPFIX,LPFIX(NPFIX),LEFIX(NE),NPFREE,LPFREE(NPFREE),
     *          NPSYMT,LPSYMT(NPSYMT)
      REAL*4    XPSYMT(NPSYMT),YPSYMT(NPSYMT),ZPSYMT(NPSYMT)
C
CCC [INPUT:INTER CONNECT NODES]
      INTEGER*4 IPART,NDOM,MBPDOM,
     *          LDOM(NDOM),NBPDOM(NDOM),
     *          IPSLF(MBPDOM,NDOM),IPSND(MBPDOM,NDOM)
C    
CCC [OUTPUT]
      INTEGER*4 IERR
      REAL*4    FXYZ(3,NP,NS)
C
CCC [WORK]    
      REAL*4    RX(0:N,ME),RY(0:N,ME)
      INTEGER*4 MWRK
      REAL*4    WRKN(MWRK,3)
C
CCC [LOCAL]    
      INTEGER*4 MAXBUF,IP,IE,IBP,I,J,IS
      REAL*4    SWRK(NS),FXBUF(NS),FYBUF(NS),FZBUF(NS)
      REAL*4    COEF,COEFX,COEFY,COEFZ
C
      INTEGER*4 IDIM
      DATA IDIM    / 3 /
C
      CHARACTER*60 ERMSGC
     & /' ## SUBROUTINE GRAD3X: FATAL      ERROR REPORT   ; RETURNED' /
C
      IERR=0
      MAXBUF = NE*(N+1)
C
#ifndef __AIX__
!ocl CACHE_SECTOR_SIZE(1,11)
!ocl CACHE_SUBSECTOR_ASSIGN(S)
#endif
      DO 1000 IP = 1 , NP 
          FXBUF=0.0E0
          FYBUF=0.0E0
          FZBUF=0.0E0
          DO 1100 I = 1 , 8
              IE=IENP(I,IP)
              IF(IE.LT.0 .OR. IE.GT.NE) GOTO 1100
              DO 1101 IS= 1 , NS
                  SWRK(IS) = S(IE,IS)
                  FXBUF(IS)=FXBUF(IS)-SWRK(IS)*DNXYZP(I,1,IP)
                  FYBUF(IS)=FYBUF(IS)-SWRK(IS)*DNXYZP(I,2,IP)
                  FZBUF(IS)=FZBUF(IS)-SWRK(IS)*DNXYZP(I,3,IP)
 1101         CONTINUE
 1100     CONTINUE
C
          DO 1102 IS= 1 , NS
              FXYZ(1,IP,IS)=FXBUF(IS)
              FXYZ(2,IP,IS)=FYBUF(IS)
              FXYZ(3,IP,IS)=FZBUF(IS)
 1102     CONTINUE
 1000 CONTINUE
C
C    
C
CC
CC NOTE THAT 
CC INPUT DATA WITH NEP(IP)>=9 IS NOT SUPPORTED IN THIS CODE
CC                                     2020.03.06 Y.YAMADE 
CC
CC      DO 1200 IP = 1 , NP 
CC          FXBUF=0.0E0
CC          FYBUF=0.0E0
CC          FZBUF=0.0E0
CC          IF(NEP(IP).GT.8) THEN
CC              DO 1300 I = 9 , NEP(IP)
CC                  IE=IENP(I,IP)
CC                  SWRK = S(IE)
CC                  FXBUF=FXBUF-SWRK*DNXYZP(1,I,IP)
CC                  FYBUF=FYBUF-SWRK*DNXYZP(2,I,IP)
CC                  FZBUF=FZBUF-SWRK*DNXYZP(3,I,IP)
CC 1300         CONTINUE
CC              FXYZ(1,IP)=FXYZ(1,IP)+FXBUF
CC              FXYZ(2,IP)=FXYZ(2,IP)+FYBUF
CC              FXYZ(3,IP)=FXYZ(3,IP)+FZBUF
CC          ENDIF 
CC 1200 CONTINUE
C
C     
C
C
C SUPERIMPOSE NEIBERING ELEMENT CONTRIBUTIONS
C
      DO 1500 IS=1,NS
          CALL DDCOMY(IPART,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *                FXYZ(1,1,IS),NP,IUT0,IERR,RX,RY,MAXBUF)
 1500 CONTINUE
      IF(IERR.NE.0) THEN
          WRITE(IUT0,*)
          WRITE(IUT0,*) ERMSGC
          RETURN
      ENDIF
C
      DO 2100 IP=1,NP
          DO 2101 IS = 1 , NS
              FXYZ(1,IP,IS)=FXYZ(1,IP,IS)*CM(IP)
              FXYZ(2,IP,IS)=FXYZ(2,IP,IS)*CM(IP)
              FXYZ(3,IP,IS)=FXYZ(3,IP,IS)*CM(IP)
 2101     CONTINUE
 2100 CONTINUE
C
!ocl norecurrence(FXYZ)      
      DO 3000 IBP=1,NPFIX
          IP=LPFIX(IBP)
          DO 3001 IS = 1 , NS
              FXYZ(1,IP,IS)=0.0E0
              FXYZ(2,IP,IS)=0.0E0
              FXYZ(3,IP,IS)=0.0E0
 3001     CONTINUE   
 3000 CONTINUE   
C
!ocl norecurrence(FXYZ)      
      DO 3100 IBP = 1 , NPSYMT
          DO 3101 IS = 1 , NS
              COEF= XPSYMT(IBP)*FXYZ(1,LPSYMT(IBP),IS)
     *             +YPSYMT(IBP)*FXYZ(2,LPSYMT(IBP),IS)
     *             +ZPSYMT(IBP)*FXYZ(3,LPSYMT(IBP),IS)
              COEFX=COEF*XPSYMT(IBP)
              COEFY=COEF*XPSYMT(IBP)
              COEFZ=COEF*XPSYMT(IBP)
              FXYZ(1,LPSYMT(IBP),IS) = FXYZ(1,LPSYMT(IBP),IS)-COEFX
              FXYZ(2,LPSYMT(IBP),IS) = FXYZ(2,LPSYMT(IBP),IS)-COEFY
              FXYZ(3,LPSYMT(IBP),IS) = FXYZ(3,LPSYMT(IBP),IS)-COEFZ
 3101     CONTINUE
 3100 CONTINUE
C
      RETURN
      END
