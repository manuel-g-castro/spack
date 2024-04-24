      SUBROUTINE PRES3E(MCOLOR,MCPART,NCOLOR,NCPART,LLOOP,
     *                  IPMODE,ME,MP,N1,N2,NEX,NE,NP,ISTEP,
     *                  MRCM,NMAX,NMAXB,ISOLP,NSIDR,NLIDR,
     *                  EPS,EPSRE,DT3D,
     *                  NODE,CM,SN,DNXYZ,DNXYZP,DNXI,DNYI,DNZI,
     *                  U,V,W,NPINLT,LPINLT,NPMWAL,LPMWAL,
     *                  NPSYMT,LPSYMT,XPSYMT,YPSYMT,ZPSYMT,
     *                  NPSLD1,LPSLD1,
     *                  NITR,RES,PE,DPE,PN,
     *                  IPART,NDOM,MBPDOM,LDOM,NBPDOM,IPSLF,IPSND,
     *                  LPFIX,LFIX3D,FXYZ,WRK01,WRK02,WRK03,WRK04,
     *                  WRK05,WRK06,WRK07,WRK08,WRK09,B,PG,
     *                  ADIAG,ALPHAP,
     *                  PRCM,APRCM,RX,RY,RZ,MWRK,WRKN,
     *                  JSET,NFRAME,IEATTR,IPATTR,
     *                  X,Y,Z,OMEGA,TIMER,UFRAME,VFRAME,WFRAME,
     *                  NPSET,LPSET1,LPSET2,LPSET3,LPSET4,
     *                  COVER1,COVER2,COVER3,
     *                  NPSND,LPSND,NPTSND,NPRCV,LPRCV,NPTRCV,
     *                  IPSET,IPSRC,
     *                  NESET,LESET1,LESET2,LESET3,
     *                  EOVER1,EOVER2,EOVER3,
     *                  NESND,LESND,NETSND,NERCV,LERCV,NETRCV,
     *                  IESET,IESRC,
     *                  IPRES,DT,FSMACH,
     *                  NMRF,IFATTR,OMGMRF,AMRF,
     *                  MEP,NEP,IENP,JENP,
     *                  IVOF,IMASS,RHO3D,LEFIX,NSP,NS,LOCAL,
     *                  NFACE,LFACE,AVEC,FFA,NFINLT,LFINLT,
     *                  NFFREE,LFFREE,NPFLD2,LPFLD2,NEFLD2,LEFLD2,
     *                  NPFREE,LPFREE,XPFREE,YPFREE,ZPFREE,
     *                  NBESET,LBESET,
     *                  SNESET,OSBCOE,XNESET,YNESET,ZNESET,
     *                  SOSP,SOSWRK,WRKOS1,WRKOS2,
     *                  COSBIN,COSBFR,ICAVI,FESRC,FLE,SIGMA,IUT0,IERR,
     *                  DNXYZT,NUMVALID,LSTVALID)
      
      IMPLICIT NONE
C
CCC   [INPUT]
      INTEGER*4 ICAVI
      REAL*4    FESRC(NE),FLE(NE),SIGMA
      INTEGER*4 MCOLOR,MCPART
      INTEGER*4 NCOLOR(4),NCPART(MCOLOR,4),LLOOP(MCPART,MCOLOR,4)
      INTEGER*4 IPMODE,ME,MP,N1,N2,NEX(8),NE,NP,NMAX,IUT0,ISTEP
      INTEGER*4 MRCM,NMAXB,ISOLP,NSIDR,NLIDR
      REAL*4    EPS,EPSRE,DT3D(NE)
      INTEGER*4 NODE(N2,NE),
     *          NPINLT,LPINLT(NPINLT),NPSYMT,LPSYMT(NPSYMT),
     *          NPMWAL,LPMWAL(NPMWAL),NPSLD1,LPSLD1(NPSLD1)
      REAL*4    CM(NP),SN(N1,ME),
     *          DNXYZ(3,N1,ME),DNXYZP(MEP,3,MP),
     *          DNXI(N1,ME),DNYI(N1,ME),DNZI(N1,ME),
     *          U(NP),V(NP),W(NP)
      REAL*4    XPSYMT(NPSYMT),YPSYMT(NPSYMT),ZPSYMT(NPSYMT)
      INTEGER*4 IPART,NDOM,MBPDOM,
     *          LDOM(NDOM),NBPDOM(NDOM),
     *          IPSLF(MBPDOM,NDOM),IPSND(MBPDOM,NDOM)
C
      INTEGER*4 IPRES,JSET,NFRAME,IEATTR(NE),IPATTR(NP)
      REAL*4    DT,FSMACH,X(NP),Y(NP),Z(NP),OMEGA,TIMER,
     *          UFRAME(NFRAME),VFRAME(NFRAME),WFRAME(NFRAME)
C
CCCC  [INPUT:OVERSET NODE DATA]
      INTEGER*4 NPSET,NPSND,NPRCV,
     *          LPSET1(NPSET),LPSET2(NPSET),
     *          LPSET3(NPSET),LPSET4(NPSET),
     *          LPSND(NDOM),NPTSND(NDOM),LPRCV(NDOM),NPTRCV(NDOM),
     *          IPSET(MBPDOM,NDOM),IPSRC(MBPDOM,NDOM)
      INTEGER*4 NESET,NESND,NERCV,
     *          LESET1(NPSET),LESET2(NPSET),LESET3(NPSET),
     *          LESND(NDOM),NETSND(NDOM),LERCV(NDOM),NETRCV(NDOM),
     *          IESET(MBPDOM,NDOM),IESRC(MBPDOM,NDOM)
      REAL*4    COVER1(NPSET),COVER2(NPSET),COVER3(NPSET)
      REAL*4    EOVER1(NPSET),EOVER2(NPSET),EOVER3(NPSET)
      REAL*4    COSBIN,COSBFR
C
CCCC  [INPUT:OVERSET PRESSURE B.C.]
      INTEGER*4 NBESET,LBESET(2,NBESET)
      REAL*4    SNESET(N1,NE),OSBCOE(NP),
     *          XNESET(NBESET),YNESET(NBESET),ZNESET(NBESET),
     *          SOSP(NP),SOSWRK(NE),WRKOS1(NE),WRKOS2(NE)
C
      INTEGER*4 MEP,NEP(NP),IENP(MEP,NP),JENP(MEP,NP)
C
C     [INPUT:VOF]
      INTEGER*4 IVOF,IMASS,NFACE,NSP,NS,NFINLT,NFFREE,NPFLD2,
     *          NEFLD2,NPFREE
      INTEGER*4 LEFIX(NE),LOCAL,LFACE,LFINLT,LFFREE,
     *          LPFLD2(NPFLD2),LEFLD2(NEFLD2),LPFREE
      REAL*4    RHO3D,AVEC,FFA,XPFREE,YPFREE,ZPFREE
C
C     [INPUT:MRF]
      INTEGER*4 NMRF
      INTEGER*4 IFATTR(*)
      REAL*4    OMGMRF(NMRF),AMRF(3,NMRF)
C
CCC [INPUT/OUTPUT]
      REAL*4    PE(NE)
C
CCC [OUTPUT]
      INTEGER*4 NITR,IERR
      REAL*4    RES,DPE(NE),PN(NP)
C
CCC [WORK]
      REAL*4    WRKSCT(NP)
      INTEGER*4 LPFIX(NP),LFIX3D(NP)
      REAL*4    RX(0:N2,ME),RY(0:N2,ME),RZ(0:N2,ME),
     *          B(NE),PG(NE),FXYZ(3,NP),
     *          WRK01(*),WRK02(*),WRK03(*),WRK04(*),
     *          WRK05(*),WRK06(*),WRK07(*),WRK08(*),
     *          WRK09(*)
      REAL*4    PRCM(ME,MRCM),APRCM(ME,MRCM)
      INTEGER*4 MWRK
      REAL*4    WRKN(MWRK,9)
C      
      REAL*4    DNXYZT(8,3,ME)
C
      INTEGER*4 LSTVALID(MP)
      INTEGER*4 NUMVALID
C
CCCC  [LOCAL]
      INTEGER*4 IBP,NPFIX,MAXBUF,IP,IE,IEP,I,J,ISEND,
     *          NETET,NEPRD,NEWED,NEHEX,NTET,NPRD,NWED,NHEX,
     *          IES1,IEE1,IES2,IEE2,IES3,IEE3,IES4,IEE4,
     *          IP1,IP2,IP3,IP4,IP5,IP6,IP7,IP8
      REAL*4    RX1,RX2,RX3,RX4,RX5,RX6,RX7,RX8,
     *          RY1,RY2,RY3,RY4,RY5,RY6,RY7,RY8,
     *          RZ1,RZ2,RZ3,RZ4,RZ5,RZ6,RZ7,RZ8,
     *          GRDPX,GRDPY,GRDPZ,UE,VE,WE,PDIAG,PCONV,COE
C
      INTEGER*4 IMODE
      DATA IMODE /1/
      REAL*4    EPS0 
      DATA EPS0 / 1.E-30 /
C
      CHARACTER*60 ERMSGC
     & /' ## SUBROUTINE PRES3E: FATAL      ERROR REPORT   ; RETURNED' /
C
CCYY---WORK FOR IDR---
      REAL*4 RESR
      REAL*4 RESV(NE,0:NLIDR+1),UMAT(NE,NSIDR,0:NLIDR+1),
     *       TR0T(NSIDR,NE),WRKS01(NE,NSIDR),WRKS02(NE,NSIDR)
CCYY---WORK FOR IDR---
C
CCTT 110317 ADDITION OF ARGUMENTS AR
      REAL*4    ADIAG(NP),ALPHAP
CCTT
C
      REAL*4    DELTAP
C
C
C     SOLVE CONTINUITY EQUATION DEFINED AT ELEMENTS 
C 
C     WRITTEN BY Y.YAMADE 2011.01.20
C    
C     NOTE THAT 
C     CURRENT VERSION DOES NOT SUPPORT OVERSET AND MID-NODES
C
C     ARGUMENT LISTINGS
C (1) INPUT
C INT *4 IPMODE       ;CONTROL PARAMETER FOR PRESSURE EQUATION
C                   0:SOLVE PRESSURE            : PE
C                   1:SOLVE PRESSURE DIFFERENCE :DPE  
C        NOTE THAT
C        IPMODE MUST BE SET TO 2 WHEN PRS. TERM IS CALCULATED IN MOMENTUM EQ.
C  
C INT *4 MP           ; MAX. NUMBER OF TOTAL NODES
C INT *4 N            ; MAX. NUMBER OF NODES ASSIGNED TO ONE ELEMENT (=8)
C INT *4 N1           ; THE DIMENSION SIZE OF THE FIRST ELEMENTS OF THE 
C                       PASSED ARRAYS 'NODET'
C INT *4 NE           ; NUMBER OF ELEMENTS
C INT *4 NP           ; NUMBER OF NODES
C INT *4 NEX       (I); INCLUDES NUMBER OF ELEMENTS AND NUMBER OF LOCAL NODES
C INT *4 NE           ; NUMBER OF ELEMENTS
C INT *4 NP           ; NUMBER OF NODES
C INT *4 NMAX         ; NUMBER OF MATRIX SOLVER ITERATIONS
C INT *4 IUT0         ; FILE NUMBER TO REPORT ERROR OCCURENCE
C REAL*4 EPS          ; MAXIMUM ALLOWABLE ERROR
C REAL*4 DT           ; TIME INCREMENT  
C INT *4 NODE   (I,IE); NODE TABLE
C INT *4 NPINLT       ; NUMBER OF INLET BOUNDARY NODES
C INT *4 LPINLT   (IB); INLET BOUNDARY NODES
C INT *4 NPMWAL       ; NUMBER OF MOVING-WALL BOUNDARY NODES
C INT *4 LPMWAL   (IB); MOVING-WAL BOUNDARU NODES
C REAL*4 CM       (IP); INVERSED LUMPED MASS MATRIX
C REAL*4 SN     (I,IE); INTEGRATED ELEMENT VECTOR OF N
C REAL*4 DNX    (I,IE); ELEMENT CENTER VALUE OF NX
C REAL*4 DNY    (I,IE); ELEMENT CENTER VALUE OF NY
C REAL*4 DNZ    (I,IE); ELEMENT CENTER VALUE OF NZ
C REAL*4 DNXI   (I,IE); ELEMENT CENTER VALUE OF NX
C REAL*4 DNYI   (I,IE); ELEMENT CENTER VALUE OF NX
C REAL*4 DNZI   (I,IE); ELEMENT CENTER VALUE OF NX
C REAL*4 U        (IP); X-DIR. VELOCITY COMPONENT
C REAL*4 V        (IP); Y-DIR. VELOCITY COMPONENT
C REAL*4 W        (IP); Z-DIR. VELOCITY COMPONENT
C 
C (2) INPUT/OUTPUT
C REAL*4 PE       (IE); PRESSURE AT ELEMENTS
C
C (3) OUTPUT
C INT *4 NITR         ; ITERATION NUMBER OF MATRIX SOLVER
C REAL*4 RES          ; L2-NORM RESIDUAL OF THE FINAL SOLUTION VECTOR
C REAL*4 DPE      (IE); PRESSURE INCREMENT AT ELEMENTS
C REAL*4 PN       (IP); PRESSURE AT NODES
C INT *4 IERR         ; RETURN CODE TO REPORT ERROR OCCURENCE
C                   0 --- NORMAL TERMINATION
C                   1 --- A FATAL ERROR HAS OCCURED
C
C (4) WORK
C REAL*4 RX     (I,IE); WORK REGION PASSED FOR MATRIX SOLVER
C REAL*4 RY     (I,IE); WORK REGION PASSED FOR MATRIX SOLVER
C REAL*4 B        (IE); WORK REGION PASSED FOR R.H.S. VECTOR 
C REAL*4 WRK01    (IE); WORK REGION PASSED FOR MATRIX SOLVER
C REAL*4 WRK02    (IE); WORK REGION PASSED FOR MATRIX SOLVER
C REAL*4 WRK03    (IE); WORK REGION PASSED FOR MATRIX SOLVER
C REAL*4 WRK04    (IE); WORK REGION PASSED FOR MATRIX SOLVER
C REAL*4 WRK05    (IE); WORK REGION PASSED FOR MATRIX SOLVER
C REAL*4 WRK06    (IE); WORK REGION PASSED FOR MATRIX SOLVER
C REAL*4 WRK07    (IE); WORK REGION PASSED FOR MATRIX SOLVER
C REAL*4 WRK08    (IE); WORK REGION PASSED FOR MATRIX SOLVER
C REAL*4 WRK09    (IE); WORK REGION PASSED FOR MATRIX SOLVER
C
#ifdef USE_TIMER
      real*8 ts0, te0

      include 'timer.h'
      include 'mpif.h'

      npres3e = npres3e + 1
      tstart = MPI_WTIME()
#endif      
      IERR=0
      MAXBUF=NE*(N2+1)
C
      IF(IPMODE.NE.0.AND.IPMODE.NE.1) THEN
          WRITE(IUT0,*) ERMSGC
          IERR=1
#ifdef USE_TIMER
          tend = MPI_WTIME()
          tpres3e = tpres3e + (tend - tstart)
#endif          
          RETURN
      ENDIF
C
      DO 1000 IP=1,NP
          LFIX3D(IP)=0
 1000 CONTINUE   
C
!ocl norecurrence(LFIX3D)
      DO 1100 IBP=1,NPINLT
          IP=LPINLT(IBP)
          LFIX3D(IP)=1
 1100 CONTINUE   
C
!ocl norecurrence(LFIX3D)
      DO 1200 IBP=1,NPMWAL
          IP=LPMWAL(IBP)
          LFIX3D(IP)=1
 1200 CONTINUE   
C
!ocl norecurrence(LFIX3D)
      DO 1250 IBP=1,NPSLD1
          IP=LPSLD1(IBP)
          LFIX3D(IP)=1
 1250 CONTINUE   
C
      DO 1300 IE=1,NE
         LEFIX(IE)=0
 1300 CONTINUE
C
      IF (IVOF.EQ.1) THEN
!ocl norecurrence(LEFIX)
         DO 1350 IBP=1,NEFLD2          
            LEFIX(LEFLD2(IBP))=1
 1350    CONTINUE
C
!ocl norecurrence(LFIX3D)
          DO 1400 IBP=1,NPFLD2
              IP=LPFLD2(IBP)
              LFIX3D(IP)=1
 1400     CONTINUE
      ENDIF
C
      NPFIX=0
      DO 1450 IP=1,NP
          IF(LFIX3D(IP).EQ.0) GOTO 1450
          NPFIX=NPFIX+1
          LPFIX(NPFIX)=IP
 1450 CONTINUE 
C
      DO 10 IE=1, NE
          PG(IE) = 1.0
 10   CONTINUE
C
      NETET=NEX(1)
      NEPRD=NEX(2)
      NEWED=NEX(3)
      NEHEX=NEX(4)
C
      NTET=NEX(5)
      NPRD=NEX(6)
      NWED=NEX(7)
      NHEX=NEX(8)
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
      DO 110 IE=IES1,IEE1
          IF (LEFIX(IE).EQ.1) GOTO 110
          RX(1,IE) = DNXYZ(1,1,IE)*CM(NODE(1,IE))
          RX(2,IE) = DNXYZ(1,2,IE)*CM(NODE(2,IE))
          RX(3,IE) = DNXYZ(1,3,IE)*CM(NODE(3,IE))
          RX(4,IE) = DNXYZ(1,4,IE)*CM(NODE(4,IE))
C
          RY(1,IE) = DNXYZ(2,1,IE)*CM(NODE(1,IE))
          RY(2,IE) = DNXYZ(2,2,IE)*CM(NODE(2,IE))
          RY(3,IE) = DNXYZ(2,3,IE)*CM(NODE(3,IE))
          RY(4,IE) = DNXYZ(2,4,IE)*CM(NODE(4,IE))
C
          RZ(1,IE) = DNXYZ(3,1,IE)*CM(NODE(1,IE))
          RZ(2,IE) = DNXYZ(3,2,IE)*CM(NODE(2,IE))
          RZ(3,IE) = DNXYZ(3,3,IE)*CM(NODE(3,IE))
          RZ(4,IE) = DNXYZ(3,4,IE)*CM(NODE(4,IE))
  110 CONTINUE
C
      DO 120 IE=IES2,IEE2
          IF (LEFIX(IE).EQ.1) GOTO 120
          RX(1,IE) = DNXYZ(1,1,IE)*CM(NODE(1,IE))
          RX(2,IE) = DNXYZ(1,2,IE)*CM(NODE(2,IE))
          RX(3,IE) = DNXYZ(1,3,IE)*CM(NODE(3,IE))
          RX(4,IE) = DNXYZ(1,4,IE)*CM(NODE(4,IE))
          RX(5,IE) = DNXYZ(1,5,IE)*CM(NODE(5,IE))
C
          RY(1,IE) = DNXYZ(2,1,IE)*CM(NODE(1,IE))
          RY(2,IE) = DNXYZ(2,2,IE)*CM(NODE(2,IE))
          RY(3,IE) = DNXYZ(2,3,IE)*CM(NODE(3,IE))
          RY(4,IE) = DNXYZ(2,4,IE)*CM(NODE(4,IE))
          RY(5,IE) = DNXYZ(2,5,IE)*CM(NODE(5,IE))
C
          RZ(1,IE) = DNXYZ(3,1,IE)*CM(NODE(1,IE))
          RZ(2,IE) = DNXYZ(3,2,IE)*CM(NODE(2,IE))
          RZ(3,IE) = DNXYZ(3,3,IE)*CM(NODE(3,IE))
          RZ(4,IE) = DNXYZ(3,4,IE)*CM(NODE(4,IE))
          RZ(5,IE) = DNXYZ(3,5,IE)*CM(NODE(5,IE))
  120 CONTINUE
C
      DO 130 IE=IES3,IEE3
          IF (LEFIX(IE).EQ.1) GOTO 130
          RX(1,IE) = DNXYZ(1,1,IE)*CM(NODE(1,IE))
          RX(2,IE) = DNXYZ(1,2,IE)*CM(NODE(2,IE))
          RX(3,IE) = DNXYZ(1,3,IE)*CM(NODE(3,IE))
          RX(4,IE) = DNXYZ(1,4,IE)*CM(NODE(4,IE))
          RX(5,IE) = DNXYZ(1,5,IE)*CM(NODE(5,IE))
          RX(6,IE) = DNXYZ(1,6,IE)*CM(NODE(6,IE))
C
          RY(1,IE) = DNXYZ(2,1,IE)*CM(NODE(1,IE))
          RY(2,IE) = DNXYZ(2,2,IE)*CM(NODE(2,IE))
          RY(3,IE) = DNXYZ(2,3,IE)*CM(NODE(3,IE))
          RY(4,IE) = DNXYZ(2,4,IE)*CM(NODE(4,IE))
          RY(5,IE) = DNXYZ(2,5,IE)*CM(NODE(5,IE))
          RY(6,IE) = DNXYZ(2,6,IE)*CM(NODE(6,IE))
C
          RZ(1,IE) = DNXYZ(3,1,IE)*CM(NODE(1,IE))
          RZ(2,IE) = DNXYZ(3,2,IE)*CM(NODE(2,IE))
          RZ(3,IE) = DNXYZ(3,3,IE)*CM(NODE(3,IE))
          RZ(4,IE) = DNXYZ(3,4,IE)*CM(NODE(4,IE))
          RZ(5,IE) = DNXYZ(3,5,IE)*CM(NODE(5,IE))
          RZ(6,IE) = DNXYZ(3,6,IE)*CM(NODE(6,IE))
  130 CONTINUE
C
      DO 140 IE=IES4,IEE4
          IF (LEFIX(IE).EQ.1) GOTO 140
          RX(1,IE) = DNXYZ(1,1,IE)*CM(NODE(1,IE))
          RX(2,IE) = DNXYZ(1,2,IE)*CM(NODE(2,IE))
          RX(3,IE) = DNXYZ(1,3,IE)*CM(NODE(3,IE))
          RX(4,IE) = DNXYZ(1,4,IE)*CM(NODE(4,IE))
          RX(5,IE) = DNXYZ(1,5,IE)*CM(NODE(5,IE))
          RX(6,IE) = DNXYZ(1,6,IE)*CM(NODE(6,IE))
          RX(7,IE) = DNXYZ(1,7,IE)*CM(NODE(7,IE))
          RX(8,IE) = DNXYZ(1,8,IE)*CM(NODE(8,IE))
C
          RY(1,IE) = DNXYZ(2,1,IE)*CM(NODE(1,IE))
          RY(2,IE) = DNXYZ(2,2,IE)*CM(NODE(2,IE))
          RY(3,IE) = DNXYZ(2,3,IE)*CM(NODE(3,IE))
          RY(4,IE) = DNXYZ(2,4,IE)*CM(NODE(4,IE))
          RY(5,IE) = DNXYZ(2,5,IE)*CM(NODE(5,IE))
          RY(6,IE) = DNXYZ(2,6,IE)*CM(NODE(6,IE))
          RY(7,IE) = DNXYZ(2,7,IE)*CM(NODE(7,IE))
          RY(8,IE) = DNXYZ(2,8,IE)*CM(NODE(8,IE))
C
          RZ(1,IE) = DNXYZ(3,1,IE)*CM(NODE(1,IE))
          RZ(2,IE) = DNXYZ(3,2,IE)*CM(NODE(2,IE))
          RZ(3,IE) = DNXYZ(3,3,IE)*CM(NODE(3,IE))
          RZ(4,IE) = DNXYZ(3,4,IE)*CM(NODE(4,IE))
          RZ(5,IE) = DNXYZ(3,5,IE)*CM(NODE(5,IE))
          RZ(6,IE) = DNXYZ(3,6,IE)*CM(NODE(6,IE))
          RZ(7,IE) = DNXYZ(3,7,IE)*CM(NODE(7,IE))
          RZ(8,IE) = DNXYZ(3,8,IE)*CM(NODE(8,IE))
  140 CONTINUE
C
      DO 141 IEP = 1 , MEP
!ocl norecurrence(RX,RY,RZ)
          DO 142  IBP = 1 , NPINLT
              IP = LPINLT(IBP)
              IF(IEP.LE.NEP(IP)) THEN
                  RX(JENP(IEP,IP),IENP(IEP,IP)) = 0.E0
                  RY(JENP(IEP,IP),IENP(IEP,IP)) = 0.E0
                  RZ(JENP(IEP,IP),IENP(IEP,IP)) = 0.E0
              ENDIF
  142     CONTINUE
  141 CONTINUE
C
C
      DO 143 IEP = 1 , MEP
!ocl norecurrence(RX,RY,RZ)
          DO 144  IBP = 1 , NPMWAL
              IP = LPMWAL(IBP)
              IF(IEP.LE.NEP(IP)) THEN
                  RX(JENP(IEP,IP),IENP(IEP,IP)) = 0.E0
                  RY(JENP(IEP,IP),IENP(IEP,IP)) = 0.E0
                  RZ(JENP(IEP,IP),IENP(IEP,IP)) = 0.E0
              ENDIF
  144     CONTINUE
  143 CONTINUE
C
      DO 145 IEP = 1 , MEP
!ocl norecurrence(RX,RY,RZ)
          DO 146 IBP = 1 , NPSYMT
              IP = LPSYMT(IBP)
              IF(IEP.LE.NEP(IP)) THEN
                  I  = JENP(IEP,IP)
                  IE = IENP(IEP,IP)
                  COE = XPSYMT(IBP)*RX(I,IE)
     &                 +YPSYMT(IBP)*RY(I,IE)
     &                 +ZPSYMT(IBP)*RZ(I,IE)
                  RX(I,IE) = RX(I,IE)-COE*XPSYMT(IBP)
                  RY(I,IE) = RY(I,IE)-COE*YPSYMT(IBP)
                  RZ(I,IE) = RZ(I,IE)-COE*ZPSYMT(IBP)
              ENDIF
 146      CONTINUE
 145  CONTINUE
C
      IF(JSET.GE.1) THEN
          DO 147 IEP = 1 , MEP
!ocl norecurrence(RX,RY,RZ)
              DO 148 IBP = 1 , NPSET
                  ISEND = LPSET3(IBP)
                  IF(ISEND.GE.1) GO TO 148
                  IP = LPSET1(IBP)
                  IF(IEP.LE.NEP(IP)) THEN
                      RX(JENP(IEP,IP),IENP(IEP,IP)) = 0.E0
                      RY(JENP(IEP,IP),IENP(IEP,IP)) = 0.E0
                      RZ(JENP(IEP,IP),IENP(IEP,IP)) = 0.E0
                  ENDIF
 148          CONTINUE
 147      CONTINUE
      ENDIF
C 
      DO 151 IE=IES1,IEE1
         IF (LEFIX(IE).EQ.1) GOTO 151
         PG(IE) =
     *        DNXI(1,IE)*RX(1,IE)+DNXI(2,IE)*RX(2,IE)
     *       +DNXI(3,IE)*RX(3,IE)+DNXI(4,IE)*RX(4,IE)
     *       +DNYI(1,IE)*RY(1,IE)+DNYI(2,IE)*RY(2,IE)
     *       +DNYI(3,IE)*RY(3,IE)+DNYI(4,IE)*RY(4,IE)
     *       +DNZI(1,IE)*RZ(1,IE)+DNZI(2,IE)*RZ(2,IE)
     *       +DNZI(3,IE)*RZ(3,IE)+DNZI(4,IE)*RZ(4,IE)
         IF(PG(IE).EQ.0.0E0) PG(IE)=1.0E0
 151  CONTINUE
C 
      DO 152 IE=IES2,IEE2
         IF (LEFIX(IE).EQ.1) GOTO 152
         PG(IE) =
     *        DNXI(1,IE)*RX(1,IE)+DNXI(2,IE)*RX(2,IE)
     *       +DNXI(3,IE)*RX(3,IE)+DNXI(4,IE)*RX(4,IE)
     *       +DNXI(5,IE)*RX(5,IE)
     *       +DNYI(1,IE)*RY(1,IE)+DNYI(2,IE)*RY(2,IE)
     *       +DNYI(3,IE)*RY(3,IE)+DNYI(4,IE)*RY(4,IE)
     *       +DNYI(5,IE)*RY(5,IE)
     *       +DNZI(1,IE)*RZ(1,IE)+DNZI(2,IE)*RZ(2,IE)
     *       +DNZI(3,IE)*RZ(3,IE)+DNZI(4,IE)*RZ(4,IE)
     *       +DNZI(5,IE)*RZ(5,IE)
         IF(PG(IE).EQ.0.0E0) PG(IE)=1.0E0
 152  CONTINUE
C 
      DO 153 IE=IES3,IEE3
         IF (LEFIX(IE).EQ.1) GOTO 153
         PG(IE) =
     *        DNXI(1,IE)*RX(1,IE)+DNXI(2,IE)*RX(2,IE)
     *       +DNXI(3,IE)*RX(3,IE)+DNXI(4,IE)*RX(4,IE)
     *       +DNXI(5,IE)*RX(5,IE)+DNXI(6,IE)*RX(6,IE)
     *       +DNYI(1,IE)*RY(1,IE)+DNYI(2,IE)*RY(2,IE)
     *       +DNYI(3,IE)*RY(3,IE)+DNYI(4,IE)*RY(4,IE)
     *       +DNYI(5,IE)*RY(5,IE)+DNYI(6,IE)*RY(6,IE)
     *       +DNZI(1,IE)*RZ(1,IE)+DNZI(2,IE)*RZ(2,IE)
     *       +DNZI(3,IE)*RZ(3,IE)+DNZI(4,IE)*RZ(4,IE)
     *       +DNZI(5,IE)*RZ(5,IE)+DNZI(6,IE)*RZ(6,IE)
         IF(PG(IE).EQ.0.0E0) PG(IE)=1.0E0
 153  CONTINUE
C 
      DO 154 IE=IES4,IEE4
         IF (LEFIX(IE).EQ.1) GOTO 154
         PG(IE) =
     *        DNXI(1,IE)*RX(1,IE)+DNXI(2,IE)*RX(2,IE)
     *       +DNXI(3,IE)*RX(3,IE)+DNXI(4,IE)*RX(4,IE)
     *       +DNXI(5,IE)*RX(5,IE)+DNXI(6,IE)*RX(6,IE)
     *       +DNXI(7,IE)*RX(7,IE)+DNXI(8,IE)*RX(8,IE)
     *       +DNYI(1,IE)*RY(1,IE)+DNYI(2,IE)*RY(2,IE)
     *       +DNYI(3,IE)*RY(3,IE)+DNYI(4,IE)*RY(4,IE)
     *       +DNYI(5,IE)*RY(5,IE)+DNYI(6,IE)*RY(6,IE)
     *       +DNYI(7,IE)*RY(7,IE)+DNYI(8,IE)*RY(8,IE)
     *       +DNZI(1,IE)*RZ(1,IE)+DNZI(2,IE)*RZ(2,IE)
     *       +DNZI(3,IE)*RZ(3,IE)+DNZI(4,IE)*RZ(4,IE)
     *       +DNZI(5,IE)*RZ(5,IE)+DNZI(6,IE)*RZ(6,IE)
     *       +DNZI(7,IE)*RZ(7,IE)+DNZI(8,IE)*RZ(8,IE)
         IF(PG(IE).EQ.0.0E0) PG(IE)=1.0E0
 154  CONTINUE
C
      IF(IPMODE.EQ.1) THEN
#ifdef USE_TIMER
          tend = MPI_WTIME()
          tpres3e = tpres3e + (tend - tstart)
#endif         
          CALL GRAD3X(MCOLOR,MCPART,NCOLOR,NCPART,LLOOP,
     *                JSET,ICAVI,
     *                ME,MP,N1,N2,NE,NP,NEX,NODE,IEATTR,IPATTR,
     *                PE,DNXYZP,CM,X,Y,Z,OMEGA,TIMER,ADIAG,
     *                NFRAME,UFRAME,VFRAME,WFRAME,
     *                IPART,NDOM,MBPDOM,LDOM,NBPDOM,IPSLF,IPSND,
     *                NPFIX,LPFIX,LEFIX,
     *                NPSYMT,LPSYMT,XPSYMT,YPSYMT,ZPSYMT,
     *                NPSET,LPSET1,LPSET2,LPSET3,LPSET4,
     *                COVER1,COVER2,COVER3,
     *                NPSND,LPSND,NPTSND,
     *                NPRCV,LPRCV,NPTRCV,IPSET,IPSRC,
     *                FXYZ,RX,RY,MWRK,WRKN,
     *                NMRF,IFATTR,OMGMRF,AMRF,
     *                IVOF,IMASS,RHO3D,NSP,NS,LOCAL,
     *                NFACE,LFACE,AVEC,FFA,NFINLT,LFINLT,
     *                NFFREE,LFFREE,NPFREE,LPFREE,
     *                XPFREE,YPFREE,ZPFREE,
     *                NESET,NESND,NERCV,NBESET,LBESET,
     *                LESET1,LESET2,LESET3,
     *                EOVER1,EOVER2,EOVER3,
     *                LESND,NETSND,LERCV,NETRCV,IESET,IESRC,
     *                SN,SNESET,OSBCOE,XNESET,YNESET,ZNESET,
     *                SOSP,SOSWRK,WRKOS1,WRKOS2,
     *                MEP,NEP,IENP,JENP,
     *                IUT0,IERR,NUMVALID,LSTVALID)
#ifdef USE_TIMER
          tstart = MPI_WTIME()
#endif          
C
          DO 1500 IP=1,NP
              U(IP)=U(IP)+FXYZ(1,IP)*DT
              V(IP)=V(IP)+FXYZ(2,IP)*DT
              W(IP)=W(IP)+FXYZ(3,IP)*DT
 1500     CONTINUE   
      ENDIF
C
#ifdef USE_TIMER
      tend = MPI_WTIME()
      tpres3e = tpres3e + (tend - tstart)
#endif      
      CALL FILD3X(IMODE,ME,NE,NP,NEX,N1,N2,
     *            U,V,W,B,NODE,DNXI,DNYI,DNZI,IUT0,IERR)
#ifdef USE_TIMER
      tstart = MPI_WTIME()
#endif          
      IF(IERR.NE.0) THEN
          WRITE(IUT0,*) ERMSGC
#ifdef USE_TIMER
          tend = MPI_WTIME()
          tpres3e = tpres3e + (tend - tstart)
#endif          
          RETURN
      ENDIF
C
      DO 1550 IE=1,NE
          B(IE)=B(IE)/DT3D(IE)
 1550 CONTINUE
C
      IF(IPRES.EQ.3) THEN
          DO 1560 IE=1,NE
              B(IE)=B(IE)*FLE(IE)
 1560     CONTINUE
C
          DO 1570 IE=1,NE
              DELTAP=PE(IE)+0.5E0*SIGMA
              IF(ICAVI.EQ.1 .OR. ICAVI.EQ.2) THEN
                  IF(DELTAP.EQ.0.0E0) THEN
                      FESRC(IE)=0.0
                  ELSE
                      FESRC(IE)=FESRC(IE)/DELTAP
                      B(IE)=B(IE)+0.5E0*FESRC(IE)*SIGMA/DT
                  ENDIF  
              ELSE IF(ICAVI.EQ.3) THEN
                  IF(DELTAP.LE.0.0E0) THEN
                      FESRC(IE)=0.0E0
                  ELSE IF(DELTAP.GT.0.0E0) THEN
                      FESRC(IE)=FESRC(IE)/DELTAP
                      B(IE)=B(IE)+0.5E0*FESRC(IE)*SIGMA/DT
                  ELSE 
                      B(IE)=B(IE)+FESRC(IE)/DT
                      FESRC(IE)=0.0E0
                  ENDIF 
              ENDIF 
           
 1570     CONTINUE
      ENDIF
C
      IF(IPRES.GE.2) THEN
#ifdef USE_TIMER
          tend = MPI_WTIME()
          tpres3e = tpres3e + (tend - tstart)
#endif         
          CALL GRAD3X(MCOLOR,MCPART,NCOLOR,NCPART,LLOOP,
     *                JSET,ICAVI,
     *                ME,MP,N1,N2,NE,NP,NEX,NODE,IEATTR,IPATTR,
     *                PE,DNXYZP,CM,X,Y,Z,OMEGA,TIMER,ADIAG,
     *                NFRAME,UFRAME,VFRAME,WFRAME,
     *                IPART,NDOM,MBPDOM,LDOM,NBPDOM,IPSLF,IPSND,
     *                NPFIX,LPFIX,LEFIX,
     *                NPSYMT,LPSYMT,XPSYMT,YPSYMT,ZPSYMT,
     *                NPSET,LPSET1,LPSET2,LPSET3,LPSET4,
     *                COVER1,COVER2,COVER3,
     *                NPSND,LPSND,NPTSND,
     *                NPRCV,LPRCV,NPTRCV,IPSET,IPSRC,
     *                FXYZ,RX,RY,MWRK,WRKN,
     *                NMRF,IFATTR,OMGMRF,AMRF,
     *                IVOF,IMASS,RHO3D,NSP,NS,LOCAL,
     *                NFACE,LFACE,AVEC,FFA,NFINLT,LFINLT,
     *                NFFREE,LFFREE,NPFREE,LPFREE,
     *                XPFREE,YPFREE,ZPFREE,
     *                NESET,NESND,NERCV,NBESET,LBESET,
     *                LESET1,LESET2,LESET3,
     *                EOVER1,EOVER2,EOVER3,
     *                LESND,NETSND,LERCV,NETRCV,IESET,IESRC,
     *                SN,SNESET,OSBCOE,XNESET,YNESET,ZNESET,
     *                SOSP,SOSWRK,WRKOS1,WRKOS2,
     *                MEP,NEP,IENP,JENP,
     *                IUT0,IERR,NUMVALID,LSTVALID)
#ifdef USE_TIMER
          tstart = MPI_WTIME()
#endif          
C
          COE = FSMACH*FSMACH/(DT*DT)
          DO 1600 IE=1,NE
              IP1=NODE(1,IE) 
              IP2=NODE(2,IE) 
              IP3=NODE(3,IE) 
              IP4=NODE(4,IE) 
              IP5=NODE(5,IE) 
              IP6=NODE(6,IE) 
              IP7=NODE(7,IE) 
              IP8=NODE(8,IE) 
C
              IF(IP8.NE.0) THEN
                  GRDPX = ( FXYZ(1,IP1)+FXYZ(1,IP2)
     *                     +FXYZ(1,IP3)+FXYZ(1,IP4)
     *                     +FXYZ(1,IP5)+FXYZ(1,IP6)
     *                     +FXYZ(1,IP7)+FXYZ(1,IP8))/8.0E0
                  GRDPY = ( FXYZ(2,IP1)+FXYZ(2,IP2)
     *                     +FXYZ(2,IP3)+FXYZ(2,IP4)
     *                     +FXYZ(2,IP5)+FXYZ(2,IP6)
     *                     +FXYZ(2,IP7)+FXYZ(2,IP8))/8.0E0
                  GRDPZ = ( FXYZ(3,IP1)+FXYZ(3,IP2)
     *                     +FXYZ(3,IP3)+FXYZ(3,IP4)
     *                     +FXYZ(3,IP5)+FXYZ(3,IP6)
     *                     +FXYZ(3,IP7)+FXYZ(3,IP8))/8.0E0
                  UE = ( U(IP1)+U(IP2)+U(IP3)+U(IP4)
     *                  +U(IP5)+U(IP6)+U(IP7)+U(IP8))/8.0E0
                  VE = ( V(IP1)+V(IP2)+V(IP3)+V(IP4)
     *                  +V(IP5)+V(IP6)+V(IP7)+V(IP8))/8.0E0
                  WE = ( W(IP1)+W(IP2)+W(IP3)+W(IP4)
     *                  +W(IP5)+W(IP6)+W(IP7)+W(IP8))/8.0E0
              ELSE IF(IP6.NE.0) THEN
                  GRDPX = ( FXYZ(1,IP1)+FXYZ(1,IP2)
     *                     +FXYZ(1,IP3)+FXYZ(1,IP4)
     *                     +FXYZ(1,IP5)+FXYZ(1,IP6))/6.0E0
                  GRDPY = ( FXYZ(2,IP1)+FXYZ(2,IP2)
     *                     +FXYZ(2,IP3)+FXYZ(2,IP4)
     *                     +FXYZ(2,IP5)+FXYZ(2,IP6))/6.0E0
                  GRDPZ = ( FXYZ(3,IP1)+FXYZ(3,IP2)
     *                     +FXYZ(3,IP3)+FXYZ(3,IP4)
     *                     +FXYZ(3,IP5)+FXYZ(3,IP6))/6.0E0
                  UE = ( U(IP1)+U(IP2)+U(IP3)+U(IP4)
     *                  +U(IP5)+U(IP6)              )/6.0E0
                  VE = ( V(IP1)+V(IP2)+V(IP3)+V(IP4)
     *                  +V(IP5)+V(IP6)              )/6.0E0
                  WE = ( W(IP1)+W(IP2)+W(IP3)+W(IP4)
     *                  +W(IP5)+W(IP6)              )/6.0E0
              ELSE IF(IP5.NE.0) THEN
                  GRDPX = ( FXYZ(1,IP1)+FXYZ(1,IP2)
     *                     +FXYZ(1,IP3)+FXYZ(1,IP4)
     *                     +FXYZ(1,IP5)           )/5.0E0
                  GRDPY = ( FXYZ(2,IP1)+FXYZ(2,IP2)
     *                     +FXYZ(2,IP3)+FXYZ(2,IP4)
     *                     +FXYZ(2,IP5)          )/5.0E0
                  GRDPZ = ( FXYZ(3,IP1)+FXYZ(3,IP2)
     *                     +FXYZ(3,IP3)+FXYZ(3,IP4)
     *                     +FXYZ(3,IP5)          )/5.0E0
                  UE = ( U(IP1)+U(IP2)+U(IP3)+U(IP4)
     *                  +U(IP5)                     )/5.0E0
                  VE = ( V(IP1)+V(IP2)+V(IP3)+V(IP4)
     *                  +V(IP5)                     )/5.0E0
                  WE = ( W(IP1)+W(IP2)+W(IP3)+W(IP4)
     *                  +W(IP5)                     )/5.0E0
              ELSE IF(IP4.NE.0) THEN
                  GRDPX = ( FXYZ(1,IP1)+FXYZ(1,IP2)
     *                     +FXYZ(1,IP3)+FXYZ(1,IP4))/4.0E0
                  GRDPY = ( FXYZ(2,IP1)+FXYZ(2,IP2)
     *                     +FXYZ(2,IP3)+FXYZ(2,IP4))/4.0E0
                  GRDPZ = ( FXYZ(3,IP1)+FXYZ(3,IP2)
     *                     +FXYZ(3,IP3)+FXYZ(3,IP4))/4.0E0
                  UE = ( U(IP1)+U(IP2)+U(IP3)+U(IP4))/4.0E0
                  VE = ( V(IP1)+V(IP2)+V(IP3)+V(IP4))/4.0E0
                  WE = ( W(IP1)+W(IP2)+W(IP3)+W(IP4))/4.0E0
              ELSE
                  IERR=1
              ENDIF
C               
              PCONV=UE*GRDPX+VE*GRDPY+WE*GRDPZ
              PCONV=PCONV*(FSMACH*FSMACH)/DT
              PDIAG=PE(IE)*(FSMACH*FSMACH)/(DT*DT)
C
              PCONV=PCONV*FLE(IE)
              PDIAG=PDIAG*FLE(IE)
C
              PG(IE) = PG(IE)+COE
              B(IE)=B(IE)-PDIAG+PCONV
 1600     CONTINUE   
C
      ENDIF
C
      DO 2000 IE=1,NE
          IF (LEFIX(IE).EQ.1) THEN
             B(IE)=0.0E0
          ELSE
             B(IE)=B(IE)/PG(IE)
          ENDIF
 2000 CONTINUE
C
      DO 3000 IE=1,NE
          DPE(IE)=PE(IE)
 3000 CONTINUE   
C
      IF(ISOLP.EQ.1) THEN
#ifdef USE_TIMER
         tend = MPI_WTIME()
         tpres3e = tpres3e + (tend - tstart)
#endif         
          CALL BCGSXE(MCOLOR,MCPART,NCOLOR,NCPART,LLOOP,
     *                ME,MP,N1,N2,NEX,NE,NP,NMAX,EPS,EPSRE,
     *                NODE,CM,DNXYZP,DNXI,DNYI,DNZI,
     *                B,PG,NPFIX,LPFIX,LEFIX,
     *                NPSYMT,LPSYMT,XPSYMT,YPSYMT,ZPSYMT,
     *                NITR,RES,DPE,
     *                IPART,NDOM,MBPDOM,LDOM,NBPDOM,IPSLF,IPSND,
     *                WRK01,WRK02,WRK03,WRK04,WRK05,WRK06,
     *                FXYZ,RX,RY,MWRK,WRKN,
     *                ADIAG,
     *                JSET,NFRAME,IEATTR,IPATTR,
     *                X,Y,Z,OMEGA,TIMER,UFRAME,VFRAME,WFRAME,
     *                NPSET,LPSET1,LPSET2,LPSET3,LPSET4,
     *                COVER1,COVER2,COVER3,
     *                NPSND,LPSND,NPTSND,NPRCV,LPRCV,NPTRCV,
     *                IPSET,IPSRC,IPRES,DT,FSMACH,
     *                NMRF,IFATTR,OMGMRF,AMRF,
     *                IVOF,IMASS,RHO3D,NSP,NS,LOCAL,
     *                NFACE,LFACE,AVEC,FFA,NFINLT,LFINLT,
     *                NFFREE,LFFREE,NPFREE,LPFREE,
     *                XPFREE,YPFREE,ZPFREE,
     *                NESET,NESND,NERCV,NBESET,LBESET,
     *                LESET1,LESET2,LESET3,
     *                EOVER1,EOVER2,EOVER3,
     *                LESND,NETSND,LERCV,NETRCV,IESET,IESRC,
     *                SN,SNESET,OSBCOE,XNESET,YNESET,ZNESET,
     *                SOSP,SOSWRK,WRKOS1,WRKOS2,FESRC,FLE,
     *                MEP,NEP,IENP,JENP,
     *                IUT0,IERR, DNXYZT,NUMVALID,LSTVALID)
#ifdef USE_TIMER
          tstart = MPI_WTIME()
#endif
      ELSE IF(ISOLP.EQ.2) THEN
#ifdef USE_TIMER
          tend = MPI_WTIME()
          tpres3e = tpres3e + (tend - tstart)
#endif         
          CALL USTSTA(22)
          CALL RCMELM(MCOLOR,MCPART,NCOLOR,NCPART,LLOOP,
     *                ME,MP,N1,N2,NEX,NE,NP,NMAX,EPS,EPSRE,MRCM,NMAXB,
     *                NODE,CM,DNXYZP,DNXI,DNYI,DNZI,
     *                B,PG,NPFIX,LPFIX,LEFIX,
     *                NPSYMT,LPSYMT,XPSYMT,YPSYMT,ZPSYMT,
     *                NITR,RES,DPE,
     *                IPART,NDOM,MBPDOM,LDOM,NBPDOM,IPSLF,IPSND,
     *                FXYZ,WRK01,WRK02,WRK03,WRK04,
     *                WRK05,WRK06,WRK07,WRK08,WRK09,PRCM,APRCM,
     *                RX,RY,MWRK,WRKN,
     *                ADIAG,
     *                JSET,NFRAME,IEATTR,IPATTR,
     *                X,Y,Z,OMEGA,TIMER,UFRAME,VFRAME,WFRAME,
     *                NPSET,LPSET1,LPSET2,LPSET3,LPSET4,
     *                COVER1,COVER2,COVER3,
     *                NPSND,LPSND,NPTSND,NPRCV,LPRCV,NPTRCV,
     *                IPSET,IPSRC,IPRES,DT,FSMACH,
     *                NMRF,IFATTR,OMGMRF,AMRF,
     *                IVOF,IMASS,RHO3D,NSP,NS,LOCAL,
     *                NFACE,LFACE,AVEC,FFA,NFINLT,LFINLT,
     *                NFFREE,LFFREE,NPFREE,LPFREE,
     *                XPFREE,YPFREE,ZPFREE,
     *                NESET,NESND,NERCV,NBESET,LBESET,
     *                LESET1,LESET2,LESET3,
     *                EOVER1,EOVER2,EOVER3,
     *                LESND,NETSND,LERCV,NETRCV,IESET,IESRC,
     *                SN,SNESET,OSBCOE,XNESET,YNESET,ZNESET,
     *                SOSP,SOSWRK,WRKOS1,WRKOS2,FESRC,FLE,
     *                MEP,NEP,IENP,JENP,
     *                IUT0,IERR, DNXYZT,NUMVALID,LSTVALID)
          CALL USTEND(22)
#ifdef USE_TIMER
          tstart = MPI_WTIME()
#endif          
      ELSE IF(ISOLP.EQ.3) THEN
#ifdef USE_TIMER
          tend = MPI_WTIME()
          tpres3e = tpres3e + (tend - tstart)
#endif         
          CALL IDRLAP(MCOLOR,MCPART,NCOLOR,NCPART,LLOOP,
     *                NSIDR,NLIDR,NMAX,EPS,NITR,JSET,
     *                IPRES,OMEGA,TIMER,DT,FSMACH,RES,RESR,
     *                ME,MP,N1,N2,NE,NP,NEX,NODE,B,DPE,PG,
     *                IEATTR,IPATTR,DNXYZP,
     *                DNXI,DNYI,DNZI,CM,X,Y,Z,ADIAG,
     *                NFRAME,UFRAME,VFRAME,WFRAME,NPFIX,LPFIX,LEFIX,
     *                NPSYMT,LPSYMT,XPSYMT,YPSYMT,ZPSYMT,
     *                NPSET,LPSET1,LPSET2,LPSET3,LPSET4,
     *                COVER1,COVER2,COVER3,
     *                NPSND,LPSND,NPTSND,
     *                NPRCV,LPRCV,NPTRCV,IPSET,IPSRC,
     *                IPART,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *                MWRK,WRKN,
     *                WRK01,RESV,UMAT,TR0T,WRKS01,WRKS02,WRK02,
     *                RX,RY,
     *                NMRF,IFATTR,OMGMRF,AMRF,
     *                IVOF,IMASS,RHO3D,NSP,NS,LOCAL,
     *                NFACE,LFACE,AVEC,FFA,NFINLT,LFINLT,
     *                NFFREE,LFFREE,NPFREE,LPFREE,
     *                XPFREE,YPFREE,ZPFREE,
     *                NESET,NESND,NERCV,NBESET,LBESET,
     *                LESET1,LESET2,LESET3,
     *                EOVER1,EOVER2,EOVER3,
     *                LESND,NETSND,LERCV,NETRCV,IESET,IESRC,
     *                SN,SNESET,OSBCOE,XNESET,YNESET,ZNESET,
     *                SOSP,SOSWRK,WRKOS1,WRKOS2,FESRC,FLE,
     *                MEP,NEP,IENP,JENP,
     *                IUT0,IERR,DNXYZT,NUMVALID,LSTVALID)
#ifdef USE_TIMER
          tstart = MPI_WTIME()
#endif          
      ELSE
          IERR=1
      ENDIF
C
      IF(IERR.NE.0) THEN
          WRITE(IUT0,*) ERMSGC
#ifdef USE_TIMER
          tend = MPI_WTIME()
          tpres3e = tpres3e + (tend - tstart)
#endif          
          RETURN
      ENDIF
C
      DO IE=1,NE
      WRK05(IE)=DPE(IE)
      ENDDO
C
      DO 3200 IE=1,NE
          PE(IE)=DPE(IE)
 3200 CONTINUE   
C
      IF (IVOF.EQ.1) THEN
!ocl norecurrence(PE)
         DO 3400 IBP=1,NEFLD2
            PE(LEFLD2(IBP))=0.0E0
 3400    CONTINUE
      ENDIF
C
      IF(JSET.GE.1) THEN
#ifdef USE_TIMER
          tend = MPI_WTIME()
          tpres3e = tpres3e + (tend - tstart)
#endif          
          CALL NDLEX2(MCOLOR,MCPART,NCOLOR,NCPART,LLOOP,
     *                NODE,ME,MP,NE,NP,N1,N2,NEX,SN,
     *                IPART,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *                PE,PN,CM,IUT0,IERR,RX,RY,MAXBUF,LEFIX)
#ifdef USE_TIMER
          tstart = MPI_WTIME()
#endif              
C
          IF(COSBIN.EQ.1.0E0 .AND. COSBIN.EQ.1.0E0) THEN
#ifdef USE_TIMER                 
             tend = MPI_WTIME()
             t pres3e = tpres3e + (tend - tstart)
#endif                 
              CALL OVRSTE(IPART,NESET,N1,N2,ME,NE,NP,NEX,NODE,PE,PN,
     *                    LESET1,LESET2,LESET3,
     *                    EOVER1,EOVER2,EOVER3,
     *                    NDOM,MBPDOM,NESND,NERCV,
     *                    LESND,NETSND,LERCV,NETRCV,IESET,IESRC,
     *                    WRK01,WRK02,RX,RY,IUT0,IERR)
#ifdef USE_TIMER
              tstart = MPI_WTIME()
#endif              
C
              ELSE
                  DO 4000 IE=1,NE
                      IP1=NODE(1,IE)
                      IP2=NODE(2,IE)
                      IP3=NODE(3,IE)
                      IP4=NODE(4,IE)
                      IP5=NODE(5,IE)
                      IP6=NODE(6,IE)
                      IP7=NODE(7,IE)
                      IP8=NODE(8,IE)
                      IF (NODE(NHEX,IE).NE.0) THEN
                         PE(IE)=(PN(IP1)+PN(IP2)+PN(IP3)+PN(IP4)
     *                         + PN(IP5)+PN(IP6)+PN(IP7)+PN(IP8))
     *                         /8.0E0
                      ELSE IF (NODE(NWED,IE).NE.0) THEN
                         PE(IE)=(PN(IP1)+PN(IP2)+PN(IP3)+PN(IP4)
     *                         + PN(IP5)+PN(IP6))
     *                         /FLOAT(NWED)
                      ELSE IF (NODE(NPRD,IE).NE.0) THEN
                         PE(IE)=(PN(IP1)+PN(IP2)+PN(IP3)+PN(IP4)
     *                         + PN(IP5))
     *                         /FLOAT(NPRD)
                      ELSE IF (NODE(NTET,IE).NE.0) THEN
                         PE(IE)=(PN(IP1)+PN(IP2)+PN(IP3)+PN(IP4))
     *                         /FLOAT(NTET)
                      ENDIF
 4000         CONTINUE
          ENDIF
      ENDIF
C  
C
#ifdef USE_TIMER
      tend = MPI_WTIME()
      tpres3e = tpres3e + (tend - tstart)
#endif          
      RETURN
      END
