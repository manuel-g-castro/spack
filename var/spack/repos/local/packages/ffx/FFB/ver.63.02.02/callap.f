      SUBROUTINE CALLAP(MCOLOR,MCPART,NCOLOR,NCPART,LLOOP,
     *                  JSET,IPRESS,
     *                  ME,MP,N1,N2,NE,NP,NEX,NODE,IEATTR,IPATTR,
     *                  S,AS,PG,DNXYZP,DNXI,DNYI,DNZI,
     *                  CM,X,Y,Z,OMEGA,TIMER,DT,FSMACH,ADIAG,
     *                  NFRAME,UFRAME,VFRAME,WFRAME,
     *                  IPART,NDOM,MBPDOM,LDOM,NBPDOM,IPSLF,IPSND,
     *                  NPFIX,LPFIX,LEFIX,
     *                  NPSYMT,LPSYMT,XPSYMT,YPSYMT,ZPSYMT,
     *                  NPSET,LPSET1,LPSET2,LPSET3,LPSET4,
     *                  COVER1,COVER2,COVER3,
     *                  NPSND,LPSND,NPTSND,
     *                  NPRCV,LPRCV,NPTRCV,IPSET,IPSRC,
     *                  FXYZ,RX,RY,MWRK,WRKN,
     *                  NMRF,IFATTR,OMGMRF,AMRF,
     *                  IVOF,IMASS,RHO3D,NSP,NS,LOCAL,
     *                  NFACE,LFACE,AVEC,FFA,NFINLT,LFINLT,
     *                  NFFREE,LFFREE,NPFREE,LPFREE,
     *                  XPFREE,YPFREE,ZPFREE,
     *                  NESET,NESND,NERCV,NBESET,LBESET,
     *                  LESET1,LESET2,LESET3,
     *                  EOVER1,EOVER2,EOVER3,
     *                  LESND,NETSND,LERCV,NETRCV,IESET,IESRC,
     *                  SN,SNESET,OSBCOE,XNESET,YNESET,ZNESET,
     *                  SOSP,SOSWRK,WRKOS1,WRKOS2,FESRC,FLE,
     *                  MEP,NEP,IENP,JENP,
     *                  IUT0,IERR,DNXYZ,NUMVALID,LSTVALID)
      IMPLICIT NONE
C
      INTEGER*4 ICAVI
      REAL*4    FESRC(NE),FLE(NE)
C
CCCC  [INPUT:LOOP]
      INTEGER*4 MCOLOR,MCPART
      INTEGER*4 NCOLOR(4),NCPART(MCOLOR,4),LLOOP(MCPART,MCOLOR,4)
C
CCCC  [INPUT]
      INTEGER*4 JSET,IPRESS,
     *          ME,MP,N1,N2,NE,NP,NEX(8),NODE(N2,NE),
     *          NFRAME,IEATTR(NE),IPATTR(NP),IUT0
      INTEGER*4 MEP,NEP(NP),IENP(MEP,NP),JENP(MEP,NP)
      REAL*4    S(NE),AS(NE),PG(NE),DNXYZP(MEP,3,NP),
     *          DNXI(N1,ME),DNYI(N1,ME),DNZI(N1,ME),CM(NP),
     *          X(NP),Y(NP),Z(NP),OMEGA,TIMER,DT,FSMACH,ADIAG(NP), 
     *          UFRAME(NFRAME),VFRAME(NFRAME),WFRAME(NFRAME)
C
CCCC  [INPUT:OVERSET NODE DATA]
      INTEGER*4  NPSET,NPSND,NPRCV,
     *           LPSET1(NPSET),LPSET2(NPSET),
     *           LPSET3(NPSET),LPSET4(NPSET),
     *           LPSND(NDOM),NPTSND(NDOM),LPRCV(NDOM),NPTRCV(NDOM),
     *           IPSET(MBPDOM,NDOM),IPSRC(MBPDOM,NDOM)
      REAL*4     COVER1(NPSET),COVER2(NPSET),COVER3(NPSET)
C
CCCC  [INPUT:OVERSET ELEMENT DATA]
      INTEGER*4 NESET,NESND,NERCV,NBESET,LBESET(2,NBESET),
     *          LESET1(NESET),LESET2(NESET),LESET3(NESET),
     *          LESND(NDOM),NETSND(NDOM),LERCV(NDOM),NETRCV(NDOM),
     *          IESET(MBPDOM,NDOM),IESRC(MBPDOM,NDOM)
      REAL*4    EOVER1(NESET),EOVER2(NESET),EOVER3(NESET),
     *          SN(N1,NE),SNESET(N1,NE),OSBCOE(NP),
     *          XNESET(NBESET),YNESET(NBESET),ZNESET(NBESET),
     *          SOSP(NP),SOSWRK(NE),WRKOS1(NE),WRKOS2(NE)
C
CCC [INPUT:INTER CONNECT NODES]
      INTEGER*4 IPART,NDOM,MBPDOM,
     *          LDOM(NDOM),NBPDOM(NDOM),
     *          IPSLF(MBPDOM,NDOM),IPSND(MBPDOM,NDOM)
C
CCC [INPUT:B.C. NODES]
      INTEGER*4 NPFIX,LPFIX(NPFIX),LEFIX(NE),
     *          NPSYMT,LPSYMT(NPSYMT)
      REAL*4    XPSYMT(NPSYMT),YPSYMT(NPSYMT),ZPSYMT(NPSYMT)
C
C     [INPUT:VOF]
      INTEGER*4 IVOF,IMASS,NFACE,NSP,NS,NFINLT,NFFREE,NPFREE
      INTEGER*4 LOCAL,LFACE,LFINLT,LFFREE,LPFREE
      REAL*4    RHO3D,AVEC,FFA,
     *          XPFREE,YPFREE,ZPFREE
C
C     [INPUT:MRF]
      INTEGER*4 NMRF
      INTEGER*4 IFATTR(*)
      REAL*4    OMGMRF(NMRF),AMRF(3,NMRF)
C
CCC [OUTPUT]
      INTEGER*4 IERR
      REAL*4    FXYZ(3,NP)
C
CCC [WORK]    
      REAL*4    RX(0:N2,ME),RY(0:N2,ME)
      INTEGER*4 MWRK
      REAL*4    WRKN(MWRK,9)

      REAL*4    DNXYZ(8,3,ME)
C
      INTEGER*4 LSTVALID(MP)
      INTEGER*4 NUMVALID
C
CCC [LOCAL]    
      INTEGER*4 IP,IE
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
C
C     ARGUMENT LISTINGS
C
C (1) INPUT
C INT *4 MLOOP        ; MAX. NUMBER OF COLORS
C INT *4 NLOOP        ; NUMBER OF COLORS
C INT *4 LLOOP(I,4)   ; ADRESS OF COLOR LOOPS
C INT *4 JSET         ; OVERSET FLAG (0:OFF,LARGER THAN 1:ON)
C INT *4 ME           ; MAX. NUMBER OF TOTAL ELEMENTS
C INT *4 N            ; =8
C INT *4 N1           ; THE DIMENSION SIZE OF THE FIRST ELEMENTS OF THE 
C INT *4 NE           ; NUMBER OF ELEMENTS
C INT *4 NP           ; NUMBER OF NODES
C INT *4 NEX       (I); INCLUDES NUMBER OF ELEMENTS AND NUMBER OF LOCAL NODES
C INT *4 NE           ; NUMBER OF ELEMENTS
C INT *4 NP           ; NUMBER OF NODES
C INT *4 NODE   (I,IE); NODE TABLE
C INT *4 NFRAME       ; NUMBER OF FRAMES
C INT *4 IEATTR   (IE); ELEMENT ATTIRBUTE DATA (-1:ROTATING, 0:STATIC)
C INT *4 IPATTR   (IP); NODE    ATTIRBUTE DATA (-1:ROTATING, 0:STATIC)
C INT *4 IUT0         ; FILE NUMBER TO REPORT ERROR OCCURENCE
C REAL*4 S        (IP); INPUT DATA 
C REAL*4 DNX    (I,IE); INTEGRATED ELEMENT VECTOR OF NX
C REAL*4 DNY    (I,IE); INTEGRATED ELEMENT VECTOR OF NY
C REAL*4 DNZ    (I,IE); INTEGRATED ELEMENT VECTOR OF NZ
C REAL*4 DNXI   (I,IE); ELEMENT CENTER VALUE OF NX
C REAL*4 DNYI   (I,IE); ELEMENT CENTER VALUE OF NY
C REAL*4 DNZI   (I,IE); ELEMENT CENTER VALUE OF NZ
C REAL*4 CM       (IP); INVERSED LUMPED MASS MATRIX
C REAL*4 X        (IP); X-CORDINATE
C REAL*4 Y        (IP); Y-CORDINATE
C REAL*4 OMEGA        ; ANGULAR MOMENTUM
C REAL*4 TIMER        ; CURRENT TIME
C REAL*4 ADIAG    (NP); DIAGONAL TERM IN MOMENTUM EQ. (IN STEADY MODE)
C REAL*4 UFRAME (1,IF); X-DIR. VELOCITY    OF TRANSLATING FRAME 'IF'
C REAL*4 VFRAME (1,IF); Y-DIR. VELOCITY    OF TRANSLATING FRAME 'IF'
C REAL*4 WFRAME (1,IF); Z-DIR. VELOCITY    OF TRANSLATING FRAME 'IF'
C REAL*4 UFRAME (2,IF); X-DIR. ACCELERAION OF TRANSLATING FRAME 'IF'
C REAL*4 VFRAME (2,IF); Y-DIR. ACCELERAION OF TRANSLATING FRAME 'IF'
C REAL*4 WFRAME (2,IF); Z-DIR. ACCELERAION OF TRANSLATING FRAME 'IF'
C
C === B.C.-[1] ===    ; INTERCONNECT B.C.
C INT *4 IPART        ; SUB-DOMAIN NUMBER THAT THIS TASK SHOULD TAKE/IS
C                       TAKING CARE OF. IPART BEING SET ZERO MEANS THAT
C                       THE PROGRAM SHOULD RUN/IS RUNNING IN SERIAL 
C                       MODE.
C INT *4 NDOM         ; NUMBER OF THE NERIBERING SUB-DOMAINS
C INT*4  MBPDOM       ; THE MAXIMUM NUMBER OF THE INTER-CONNECT 
C                       BOUNDARY NODES FOR ONE NEIBERING SUB-DOMAIN
C INT *4 LDOM   (IDOM); NEIBERING SUB-DOMAIN NUMBER
C INT *4 NBPDOM (IDOM); NUMBER OF INTER-CONNECT BOUNDARY NODES
C                       SHARING WITH THE IDOM'TH NEIBERING SUB-DOMAIN,
C                       LDOM(IDOM)
C INT *4 IPSLF(I,IDOM); INTER-CONNECT BOUNDARY NODE NUMBER IN THE
C                       CALLING TASK'S SUB-DOMAIN, FOR THE IDOM'TH
C                       NEIBERING SUB-DOMAIN, LDOM(IDOM)
C INT *4 IPSND(I,IDOM); INTER-CONNECT BOUNDARY NODE NUMBER IN THE
C                       SUB-DOMAIN THAT IS RECEIVING THE CALLING
C                       TASK'S RESIDUALS.
C
C === B.C.-[2] ===    ; GRADIENT-ZETO B.C.
C INT *4 NPFIX        ; NUMBER OF FIX BOUNDARY NODES
C INT *4 LPFIX    (IB); FIX BOUNDARY NODES
C
C === B.C.-[3] ===    ; SYMMETRIC B.C.
C INT *4 NPSYMT       ; NUMBER OF SYMMETRIC BOUNDARY NODES
C INT *4 LPSYMT   (IB); SYMMETRIC BOUNDARY NODES
C REAL*4 XPSYMT   (IB); X-DIR COMPONENT OF SYMMETRIC NODE NORMAL VECTOR
C REAL*4 YPSYMT   (IB); Y-DIR COMPONENT OF SYMMETRIC NODE NORMAL VECTOR
C REAL*4 ZPSYMT   (IB); Z-DIR COMPONENT OF SYMMETRIC NODE NORMAL VECTOR
C
C === B.C.-[4] ===    ; OVERSET B.C.
C INT *4 NPSET        : NUMBER OF OVERSET BOUNDARY NODES
C INT *4 NPSND        ; NUMBER OF DOMAINS TO SEND OVERSET NODE VALUE
C INT *4 NPRCV        ; NUMBER OF DOMAINS TO RECEIVE OVERSET NODE VALUE
C INT *4 LPSET1  (IBP); OVERSET BOUNDARY NODES
C INT *4 LPSET2  (IBP); ELEMENT NUMBER TO CALCULATE OVERSET VALUES
C INT *4 LPSET3  (IBP); DOMAIN NUMBER TO SEND/RECEIVE OVERSET VALUES
C                   0 --- CALCULATE AND SET OVERSET VALUE WITHIN THE
C                         SELF-DOMAIN
C          (POS. INT.)--- SEND    OVERSET VALUE TO   DOMAIN  LPSET3(IB)
C                         AFTER CALCULATING IT WITHIN THE SELF-DOMAIN
C          (NEG. INT.)--- RECEIVE OVERSET VALUE FROM DOMAIN -LPSET3(IB)
C
C INT *4 LPSND  (IDOM); DOMAIN NUMBER     TO SEND OVERSET NODE VALUE
C INT *4 NPTSND (IDOM); NUMBER OF OVERSET NODE POINTS TO SEND TO
C                     ; DOMAIN 'LPSND(IDOM)'  
C INT *4 LPRCV  (IDOM); DOMAIN NUMBER     TO RECEIVE OVERSET NODE VALUE
C INT *4 NPTRCV (IDOM); NUMBER OF OVERSET POINTS TO RECEIVE FROM
C                       DOMAIN 'LPRCV(IDOM)'
C INT *4 IPSET(IPT,IDOM); OVERSET NODE NUMBER IN THE DOMAIN RECEIVING
C                         THE OVERSET VALUES.
C INT *4 IPSRC(IPT,IDOM); INDICATES POSITION IN THE OVERSET-VALUES
C                         PASSING ARRAYS WHEN OVERSET NODE DATA
C                         ARE COMPILED SEQUENTIALLY
C REAL*4  COVER1 (IBP); LOCAL COORDINATE IN INTERPOLATING ELEMENT
C REAL*4  COVER2 (IBP); LOCAL COORDINATE IN INTERPOLATING ELEMENT
C REAL*4  COVER3 (IBP); LOCAL COORDINATE IN INTERPOLATING ELEMENT
C
C (2) OUTPUT
C REAL*4 SA       (IP); LAPLASIAN OF S
C INT *4 IERR         ; RETURN CODE TO REPORT ERROR OCCURRENCE
C                   0 --- NORMAL TERMINATION
C                   1 --- A FATAL ERROR HAS OCCURRED
C
C (3) WORK
C REAL*4 FX       (IP); X-COMPONET OF GRADIENT OF S
C REAL*4 FY       (IP); Y-COMPONET OF GRADIENT OF S
C REAL*4 FZ       (IP); Z-COMPONET OF GRADIENT OF S
C REAL*4 RX     (I,IE); WORK REGION PASSED FOR CALLAP
C REAL*4 RY     (I,IE); WORK REGION PASSED FOR CALLAP
C
C
      
#ifdef cputime
      NUMLAP=NUMLAP+1
C     CALL CPU_TIME( TBUF1 )
      TBUF1 = MPI_WTIME()
#endif
      ICAVI=0
CC    IF(IPRESS.EQ.3) ICAVI=1
      IERR=0
C
      CALL USTSTA(25)
      CALL GRAD3X(MCOLOR,MCPART,NCOLOR,NCPART,LLOOP,
     *            JSET,ICAVI,
     *            ME,MP,N1,N2,NE,NP,NEX,NODE,IEATTR,IPATTR,
     *            S,DNXYZP,CM,X,Y,Z,OMEGA,TIMER,ADIAG,
     *            NFRAME,UFRAME,VFRAME,WFRAME,
     *            IPART,NDOM,MBPDOM,LDOM,NBPDOM,IPSLF,IPSND,
     *            NPFIX,LPFIX,LEFIX,
     *            NPSYMT,LPSYMT,XPSYMT,YPSYMT,ZPSYMT,
     *            NPSET,LPSET1,LPSET2,LPSET3,LPSET4,
     *            COVER1,COVER2,COVER3,
     *            NPSND,LPSND,NPTSND,
     *            NPRCV,LPRCV,NPTRCV,IPSET,IPSRC,
     *            FXYZ,RX,RY,MWRK,WRKN,
     *            NMRF,IFATTR,OMGMRF,AMRF,
     *            IVOF,IMASS,RHO3D,NSP,NS,LOCAL,
     *            NFACE,LFACE,AVEC,FFA,NFINLT,LFINLT,
     *            NFFREE,LFFREE,NPFREE,LPFREE,
     *            XPFREE,YPFREE,ZPFREE,
     *            NESET,NESND,NERCV,NBESET,LBESET,
     *            LESET1,LESET2,LESET3,
     *            EOVER1,EOVER2,EOVER3,
     *            LESND,NETSND,LERCV,NETRCV,IESET,IESRC,
     *            SN,SNESET,OSBCOE,XNESET,YNESET,ZNESET,
     *            SOSP,SOSWRK,WRKOS1,WRKOS2,
     *            MEP,NEP,IENP,JENP,
     *            IUT0,IERR,NUMVALID,LSTVALID)
      CALL USTEND(25)
C
C
C    COMPUTE DIV(U) AT ELEMENTS   
C
C          OPERATION COUNTS:   26 FLOP /ELEMENT
C          DATA LOADINGS   :   40 WORDS/ELEMENT
C                           (  16  WORDS CONTIGUOUSLY,
C                              12 WORDS BY STRIDE, AND
C                              12 WORDS BY LIST )
C
CC      DO 100 IP=1,NP
CC         WRKN(IP,1)=FXYZ(1,IP)
CC         WRKN(IP,2)=FXYZ(2,IP)
CC         WRKN(IP,3)=FXYZ(3,IP)
CC 100  CONTINUE
C
CC    CALL FILD3X(IMODE,ME,NE,NP,NEX,N1,N2,
CC   &            WRKN(1,1),WRKN(1,2),WRKN(1,3),
CC   &            AS,NODE,DNXI,DNYI,DNZI,IUT0,IERR)
      CALL USTSTA(26)
      CALL FLD3X2(IMODE,ME,NE,NP,NEX,N1,N2,FXYZ,
     &            AS,NODE,DNXI,DNYI,DNZI,IUT0,IERR,
     &            DNXYZ)
C      CALL FLD3X2(IMODE,ME,NE,NP,NEX,N1,N2,FXYZ,
C     &            AS,NODE,DNXI,DNYI,DNZI,IUT0,IERR)
      CALL USTEND(26)
C
C
C
C
C    LOW MACH ASSUMPTION
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
              AS(IE)=AS(IE)-COEF*S(IE)
 1000     CONTINUE
      ELSE IF (IPRESS.EQ.3) THEN
          DO 1010 IE=1,NE
              AS(IE)=AS(IE)-(FLE(IE)*COEF+FESRC(IE)/DT)*S(IE)
CC            AS(IE)=AS(IE)-(COEF+FESRC(IE)/DT)*S(IE)
 1010     CONTINUE
      ENDIF
c
!ocl simd
      DO 1100 IE=1, NE
         IF (LEFIX(IE).EQ.1) THEN
            AS(IE)=0.0E0
         ELSE
            AS(IE)=AS(IE)/PG(IE)
          ENDIF
 1100  CONTINUE
C
#ifdef cputime
C     CALL CPU_TIME( TBUF2 )
      TBUF2 = MPI_WTIME()
      DTCPU=TBUF2-TBUF1
      DTLAPA=DTLAPA+DTCPU
      DTLAPR=DTLAPR+DTCPU*DTCPU
#endif
C
      RETURN
      END
