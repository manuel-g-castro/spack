      SUBROUTINE  CALDNR(ITIME,EPSSU,EPSSP,NP,
     *                   U,V,W,PN,UPREV,VPREV,WPREV,PNPREV,
     *                   DNRU,DNRP,
     *                   JCONVG,IUT0,IUT6,IERR)
      IMPLICIT NONE
C
      INTEGER*4 ITIME,NP,JCONVG,IUT0,IUT6,IERR
C
      REAL*4    EPSSU,EPSSP,
     *          U,V,W,PN,UPREV,VPREV,WPREV,PNPREV,DNRU,DNRP
C
      INTEGER*4 IP,IOP,IFLAG
C
      REAL*4    TMAGU,TMAGV,TMAGW,TMAGP,TMAGDU,TMAGDV,TMAGDW,TMAGDP,
     *          TMGVA,TMGDVA,TMGPA,TMGDPA
C
      DIMENSION U(NP),V(NP),W(NP),PN(NP),
     *          UPREV(NP),VPREV(NP),WPREV(NP),PNPREV(NP)
#ifdef USE_TIMER
      real*8 ts0, te0

      include 'timer.h'
      include 'mpif.h'

      ncaldnr = ncaldnr + 1
      tstart = MPI_WTIME()
#endif      
C
C // AT FIRST STEP, SKIP EVALUATION OF L2-NORM OF DIFFERENCE
      IF (ITIME.LE.1) THEN
          GOTO 1000
      ENDIF
C
C // INITIALIZE
C
C     // IN ORDER TO AVOID POSSIBLE DIVISION BY ZERO,
C     // INITIALIZED BY A SMALL POSITIVE VALUE.
      TMAGU=1.0E-30
      TMAGV=1.0E-30
      TMAGW=1.0E-30
      TMAGP=1.0E-30
C
      TMAGDU=0.0E0
      TMAGDV=0.0E0
      TMAGDW=0.0E0
      TMAGDP=0.0E0
C
      DO 100 IP = 1, NP
          TMAGU = TMAGU + U (IP)**2
          TMAGV = TMAGV + V (IP)**2
          TMAGW = TMAGW + W (IP)**2
          TMAGP = TMAGP + PN(IP)**2
C
          TMAGDU = TMAGDU + (U (IP) - UPREV (IP))**2
          TMAGDV = TMAGDV + (V (IP) - VPREV (IP))**2
          TMAGDW = TMAGDW + (W (IP) - WPREV (IP))**2
          TMAGDP = TMAGDP + (PN(IP) - PNPREV(IP))**2
 100  CONTINUE
C
C     
      TMAGV = TMAGU+TMAGV+TMAGW
      TMAGDV = TMAGDU+TMAGDV+TMAGDW
C
      TMGVA  = 0.0
      TMGDVA = 0.0
      TMGPA  = 0.0
      TMGDPA = 0.0
C
C // SUMMALIZE ALL CPU-REGIONS
      IOP=1
#ifdef USE_TIMER
      tend = MPI_WTIME()
      tcaldnr = tcaldnr + (tend - tstart)
#endif      
      CALL DDALLD(TMAGV ,TMGVA ,IOP,IUT0,IERR)
      CALL DDALLD(TMAGDV,TMGDVA,IOP,IUT0,IERR)
      CALL DDALLD(TMAGP ,TMGPA ,IOP,IUT0,IERR)
      CALL DDALLD(TMAGDP,TMGDPA,IOP,IUT0,IERR)
#ifdef USE_TIMER
      tstart = MPI_WTIME()
#endif      
C
      TMAGV  = TMGVA
      TMAGDV = TMGDVA
      TMAGP  = TMGPA
      TMAGDP = TMGDPA
C
C // CALCULATE L2-NORM
      DNRU = TMAGDV / TMAGV
      DNRP = TMAGDP / TMAGP
C
C // CONVERGENCE CHECK FOR STATIC CALCULATION
      IFLAG = 0
CC    WRITE(IUT6,*) "***CALDNR***"
CC    WRITE(IUT6,*) DNRU,DNRV,DNRW,DNRP
C
 1000 CONTINUE
C
      DO 2000 IP = 1, NP
         UPREV (IP) = U (IP)
         VPREV (IP) = V (IP)
         WPREV (IP) = W (IP)
         PNPREV(IP) = PN(IP)
 2000 CONTINUE
C
#ifdef USE_TIMER
      tend = MPI_WTIME()
      tcaldnr = tcaldnr + (tend - tstart)
#endif      
      RETURN
      END
