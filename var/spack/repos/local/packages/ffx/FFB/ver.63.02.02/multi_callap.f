      SUBROUTINE MULTI_CALLAP
     *                 (NS,IPRESS,ME,N1,N2,NE,NP,NEX,NODE,
     *                  MEP,NEP,IENP,JENP,
     *                  S,AS,PG,DNXYZP,DNXI,DNYI,DNZI,
     *                  CM,DT,FSMACH,
     *                  IPART,NDOM,MBPDOM,LDOM,NBPDOM,IPSLF,IPSND,
     *                  NPFIX,LPFIX,LEFIX,NPFREE,LPFREE,
     *                  NPSYMT,LPSYMT,XPSYMT,YPSYMT,ZPSYMT,
     *                  FXYZ,RX,RY,MWRK,WRKN,
     *                  IUT0,IERR)
      IMPLICIT NONE
C
      REAL*4    FESRC(NE),FLE(NE)
C
CCCC  [INPUT]
      INTEGER*4 NS,IPRESS,
     *          ME,N1,N2,NE,NP,NEX(8),NODE(N2,NE),IUT0
      INTEGER*4 MEP,NEP(NP),IENP(MEP,NP),JENP(MEP,NP)
      REAL*4    S(NE,NS),PG(NE),DNXYZP(MEP,3,NP),
     *          DNXI(N1,ME),DNYI(N1,ME),DNZI(N1,ME),CM(NP),
     *          DT,FSMACH
C
CCC [INPUT:INTER CONNECT NODES]
      INTEGER*4 IPART,NDOM,MBPDOM,
     *          LDOM(NDOM),NBPDOM(NDOM),
     *          IPSLF(MBPDOM,NDOM),IPSND(MBPDOM,NDOM)
C
CCC [INPUT:B.C. NODES]
      INTEGER*4 NPFIX,LPFIX(NPFIX),LEFIX(NE),NPFREE,LPFREE(NPFREE),
     *          NPSYMT,LPSYMT(NPSYMT)
      REAL*4    XPSYMT(NPSYMT),YPSYMT(NPSYMT),ZPSYMT(NPSYMT)
C
CCC [OUTPUT]
      REAL*4    AS(NE,NS)
C
CCC [WORK]    
      REAL*4    RX(0:N2,ME),RY(0:N2,ME)
      INTEGER*4 MWRK
      REAL*4    WRKN(MWRK,3,NS)
      INTEGER*4 IERR
      REAL*4    FXYZ(3,NP,NS)
C
CCC [LOCAL]    
      INTEGER*4 IP,IE,IS
      REAL*4    COEF
C
      INTEGER*4 IMODE
      DATA IMODE   / 1 /
C
      CHARACTER*60 ERMSGC
     & /' ## SUBROUTINE CALLAP: FATAL      ERROR REPORT   ; RETURNED' /
C
#ifdef cputime
      include 'mpif.h'
C
      INTEGER*4 NUMLAP
      REAL*4 DTLAPA,DTLAPR,DTLAP2 
      COMMON /CPULAP/ NUMLAP,DTLAPA,DTLAPR,DTLAP2
      REAL*4 DTCPU,TBUF1,TBUF2,TBUF3
#endif
C
C     CAL. LAPLASIAN (DIVERGENCE OF GRADIENT) 
C     OF VARIABLE DEFINED AT ELEMENTS 
C 
C     WRITTEN BY Y.YAMADE 2012.07.18
C
#ifdef cputime
      NUMLAP=NUMLAP+1
C     CALL CPU_TIME( TBUF1 )
      TBUF1 = MPI_WTIME()
#endif
      IERR=0
C
C
C  [1]  CAL. GRADIENT
C
      CALL USTSTA(25)
      CALL MULTI_GRAD3X
     *           (NS,ME,N1,N2,NE,NP,NODE,MEP,NEP,IENP,
     *            S,DNXYZP,CM,FXYZ,RX,RY,MWRK,WRKN,
     *            NPFIX,LPFIX,LEFIX,NPFREE,LPFREE,
     *            NPSYMT,LPSYMT,XPSYMT,YPSYMT,ZPSYMT,
     *            IPART,NDOM,MBPDOM,LDOM,NBPDOM,IPSLF,IPSND,
     *            IUT0,IERR)
      CALL USTEND(25)
C
C
C  [2]  CAL. DIVERGENCE
C
      CALL USTSTA(26)
      CALL MULTI_FLD3X2
     *          (NS,IMODE,ME,N2,NE,NP,NEX,N1,N2,
     *           FXYZ,AS,NODE,DNXI,DNYI,DNZI, 
     *           IUT0,IERR)
      CALL USTEND(26)
C
C
C
C
C  [3]  LOW MACH ASSUMPTION
C
C          OPERATION COUNTS:    FLOP /ELEMENT
C          DATA LOADINGS   :    WORDS/ELEMENT
C                           (    WORDS CONTIGUOUSLY,
C                                WORDS BY STRIDE, AND
C                                WORDS BY LIST )
C
      COEF=FSMACH*FSMACH/(DT*DT)
      IF (IPRESS.EQ.2) THEN
          DO 1000 IE=1,NE
              DO 1001 IS=1,NS
                  AS(IE,IS)=AS(IE,IS)-COEF*S(IE,IS)
 1001         CONTINUE
 1000     CONTINUE
      ENDIF
c
      DO 1100 IE=1, NE
          DO 1101 IS=1,NS
              IF (LEFIX(IE).EQ.1) THEN
                  AS(IE,IS)=0.0E0
              ELSE
                  AS(IE,IS)=AS(IE,IS)/PG(IE)
              ENDIF
 1101     CONTINUE
 1100 CONTINUE
C
#ifdef cputime
      TBUF2 = MPI_WTIME()
      DTCPU=TBUF2-TBUF1
      DTLAPA=DTLAPA+DTCPU
      DTLAPR=DTLAPR+DTCPU*DTCPU
#endif
C
      RETURN
      END
