C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.8.1                                   C
C                                                                      C
C  SUB ROUTINE : LESROP                                                C
C                                                                      C
C                                       WRITTEN BY Y.GUO               C
C                                                                      C
C                                                                      C
C CONTACT ADDRESS : IIS, THE UNIVERSITY OF TOKYO, CISS                 C
C                                                                      C
C THERMO-FLUID ANALYSIS SOLVERS FOR LARGE-SCALE-ASSEMBLY               C
C                                                                      C
C======================================================================C
      SUBROUTINE LESROP(IUT0,IUT5,IUT6,IPART,IFILTR,LOSEND,
     *                  LFREE,INRBC,IBCIO,EPSM,IMASS,
     *                  MSDEP,NSDEP,LHEAD,ISOLV,ISOLT,ISOLP,
     *                  NMAXB,NITRNR,CSMAX,IPREF,
     *                  XREF,YREF,ZREF,IPCAV,IDES,LVT0,
     *                  NITRIT,NITRIP,BTDCOE,DIVESC,
     *                  JSORT,JCOLOR,JUNROL,NDIVX,NDIVY,NDIVZ,NEIP,
     *                  OMEGA0,NRFN,IRFNMW,IRFNFF,NGRID,IRCAPC,
     *                  MER,MPR,MEPR,MPPR,MBR,MBPR,MDOMR,
     *                  IMONTR,ALPHAV,ALPHAP,
     *                  MFFO,MDGCOE,NFFO,NDGCOE,COEFFO,
     *                  NFRCNT,ALPHAQ,IDIAGV,THDT3D,PLIMIT,
     *                  JFSPRS,JPRESS,GRAV,IVELIN,VELIN0,IGRAV,
     *                  MAXPRO,LPRO,CPRO0,CPRO1,CPRO2,CTREF,
     *                  D000,U000,T000,TREF,RHO000,
     *                  IFIXFL,
     *                  EPST,EPSP,
     *                  EPSREV,EPSREP,
     *                  ALPHAT,EPSQ,EPSREQ,NS,NL,
     *                  IALE,NMAXA,EPSA,EPSREA,STPWR,NMODE,AOBJ,TOBJ,
     *                  MRFN,XRFMIN,YRFMIN,ZRFMIN,XRFMAX,YRFMAX,ZRFMAX,
     *                  IRFBOX,NLAYER,NLAYRT,
     *                  NLYNG,EYNG,MRSALE,IWRTIM,IALEDB,JSPADV,
     *                  IWRITE,EPSMID,JGRID, 
     *                  MMRF,NMRF,OMGMRF,AMRF,ORGMRF,IDSM,
     *                  NBLKX,NBLKY,NBLKZ,JWRTOS,
     *                  IVOF,NSCYC,RHOF2,VISCM2,NMAXVF,EPSVF,EPSRVF,
     *                  PRT,JSSMAP,NUMSSB,NOUTSS,NITRSS,CODSSB,
     *                  IRFNFT,FILECD,FILECR,JNTFND,EPSOS,COSBIN,COSBFR,
     *                  EPSBLK,BLKMIN,ICAVI,CGAS,CLQD,F0,
     *                  IERR)
C
      IMPLICIT NONE
C
      INTEGER*4 IUT0,IUT5,IUT6,IPART,IFILTR,LOSEND,LFREE,INRBC,IBCIO,
     *          IMASS,MSDEP,NSDEP,LHEAD,ISOLV,ISOLT,ISOLP,NMAXB,
     *          NITRNR,IPREF,IPCAV,IDES,LVT0,NITRIT,NITRIP,
     *          JSORT,JCOLOR,JUNROL,NDIVX,NDIVY,NDIVZ,
     *          NRFN,IRFNMW,IRFNFF,NGRID,IRCAPC,
     *          MER,MPR,MEPR,MPPR,MBR,MBPR,MDOMR,IMONTR,
     *          MFFO,MDGCOE,NFFO,NFRCNT,IDIAGV,
     *          JFSPRS,JPRESS,IVELIN,IGRAV,
     *          IFIXFL,NS,NL,IALE,NMAXA,NMODE,MRFN,
     *          NLYNG,MRSALE,IWRTIM,IALEDB,JSPADV,IWRITE,JGRID,
     *          MMRF,NMRF,IDSM,NBLKX,NBLKY,NBLKZ,JWRTOS,IVOF,NSCYC,
     *          NMAXVF,JSSMAP,NOUTSS,NITRSS,JNTFND,ICAVI,IERR
C
      REAL*4    EPSM,CSMAX,XREF,YREF,ZREF,BTDCOE,DIVESC,OMEGA0,
     *          ALPHAV,ALPHAP,ALPHAQ,THDT3D,PLIMIT,GRAV,
     *          EPST,EPSP,EPSREV,EPSREP,ALPHAT,EPSQ,EPSREQ,
     *          EPSA,EPSREA,STPWR,AOBJ,TOBJ,XRFMIN,YRFMIN,ZRFMIN,
     *          XRFMAX,YRFMAX,ZRFMAX,EYNG,EPSMID,
     *          RHOF2,VISCM2,EPSVF,EPSRVF,PRT,VELIN0,
     *          EPSOS,COSBIN,COSBFR,EPSBLK,BLKMIN,CGAS,CLQD,F0
C
      INTEGER*4 MAXPRO,MKEYWD,LPRO(MAXPRO)
      REAL*4    CPRO0(9,MAXPRO),CPRO1(4,MAXPRO),
     *          CPRO2(4,MAXPRO),CTREF(4,MAXPRO)
      REAL*4    D000,U000,T000,TREF,RHO000,CP000,VS000,CD000
      REAL*4    OMGMRF(MMRF),AMRF(3,MMRF),ORGMRF(3,MMRF) 
C
      DIMENSION VELIN0(3)
      DIMENSION GRAV(3)
C
      REAL*4 KINIT,EPINIT,ALPHAK,ALPHEO
C
      DIMENSION OMEGA0(3)
      DIMENSION BTDCOE(4)
C
      INTEGER*4 NDGCOE(MFFO)
      REAL*4    COEFFO(0:MDGCOE,MFFO)
C
      INTEGER*4 NEIP(4)
C
      INTEGER*4 NUMSSB(3)
      REAL*4    CODSSB(4)
C
      DIMENSION XRFMIN(MRFN),YRFMIN(MRFN),ZRFMIN(MRFN)
      DIMENSION XRFMAX(MRFN),YRFMAX(MRFN),ZRFMAX(MRFN)
      REAL*4 XRMIN,YRMIN,ZRMIN,XRMAX,YRMAX,ZRMAX
      INTEGER*4 IRFBOX,NLAYER(MRFN),NLAYRT(MRFN)
C
      INTEGER*4 I,J,IDUM,IRFN,IAUTO,ITLOCL,NDUM,IPRO,IMRF,IRFNB,
     *          ICS,ICE,NC
      REAL*4    CFLSET,CLFSET,EPSK,EPSEP,EPSREK,EPSREO,OMEGA2,
     *          AX,AY,AZ,ORGX,ORGY,ORGZ,AA
C
      INTEGER*4 IRFNFT
      CHARACTER*60 FILE,FILECD,FILECR
C
      PARAMETER ( MKEYWD = 93 )
      CHARACTER*8 CKEYWD(MKEYWD)
      DATA CKEYWD( 1) / '#OPTIONS' /
      DATA CKEYWD( 2) / '#OPTIONE' /
      DATA CKEYWD( 3) / '#GRDFILT' /
      DATA CKEYWD( 4) / '#OVERROP' /
      DATA CKEYWD( 5) / '#BC_FREE' /
      DATA CKEYWD( 6) / '#BC_NRBC' /
      DATA CKEYWD( 7) / '#BC_NRIO' /
      DATA CKEYWD( 8) / '#BC_EPSM' /
      DATA CKEYWD( 9) / '#BC_MICX' /
      DATA CKEYWD(10) / '#CALHEAD' /
      DATA CKEYWD(11) / '#SOL_TMP' /
      DATA CKEYWD(12) / '#SOL_PRS' /
      DATA CKEYWD(13) / '#ITR_RCM' /
      DATA CKEYWD(14) / '#ITR_NRX' /
      DATA CKEYWD(15) / '#MOD_DES' /
      DATA CKEYWD(16) / '#LIM_DSM' /
      DATA CKEYWD(17) / '#BC_PREF' /
      DATA CKEYWD(18) / '#CAVPREF' /
      DATA CKEYWD(19) / '#VT_INIT' /
      DATA CKEYWD(20) / '#ITRT_IN' /
      DATA CKEYWD(21) / '#ITRP_IN' /
      DATA CKEYWD(22) / '#COE_BTD' /
      DATA CKEYWD(23) / '#RO_SORT' /
      DATA CKEYWD(24) / '#RO_COLR' /
      DATA CKEYWD(25) / '#MLT_OMG' /
      DATA CKEYWD(26) / '#RCAP_RF' /
      DATA CKEYWD(27) / '#RF_MESH' /
      DATA CKEYWD(28) / '#RF_FLOW' /
      DATA CKEYWD(29) / '#RCAP_CP' /
      DATA CKEYWD(30) / '#SZ_GRID' /
      DATA CKEYWD(31) / '#SZ_CNCT' /
      DATA CKEYWD(32) / '#SZ_BOUN' /
      DATA CKEYWD(33) / '#MONITOR' /
      DATA CKEYWD(34) / '#DT_AUTO' /
      DATA CKEYWD(35) / '#DT_LOCL' /
      DATA CKEYWD(36) / '#CFL_VAL' /
      DATA CKEYWD(37) / '#RELAX_P' /
      DATA CKEYWD(38) / '#RELAX_V' /
      DATA CKEYWD(39) / '#FFOBJCT' /
      DATA CKEYWD(40) / '#FR_CONT' /
      DATA CKEYWD(41) / '#RELAX_Q' /
      DATA CKEYWD(42) / '#DIAGVEL' /
      DATA CKEYWD(43) / '#EFFE_DT' /
      DATA CKEYWD(44) / '#LIMIT_P' /
      DATA CKEYWD(45) / '#FS_PRES' /
      DATA CKEYWD(46) / '#PRS_ELM' /
      DATA CKEYWD(47) / '#INLT__K' /
      DATA CKEYWD(48) / '#INLT_EP' /
      DATA CKEYWD(49) / '#RELAX_K' /
      DATA CKEYWD(50) / '#RELAX_E' /      
      DATA CKEYWD(51) / '#SETTURB' /      
      DATA CKEYWD(52) / '#FIXFLOW' /
      DATA CKEYWD(53) / '#GRAVITY' /
      DATA CKEYWD(54) / '#INLTVEL' /
      DATA CKEYWD(55) / '#PRO_TBL' /
      DATA CKEYWD(56) / '#EPSIL_K' /      
      DATA CKEYWD(57) / '#EPSIL_E' /      
      DATA CKEYWD(58) / '#EPSRE_V' /
      DATA CKEYWD(59) / '#EPSRE_P' /
      DATA CKEYWD(60) / '#EPSRE_K' /
      DATA CKEYWD(61) / '#EPSRE_E' /
      DATA CKEYWD(62) / '#RELAX_T' /
      DATA CKEYWD(63) / '#EPSIL_T' /
      DATA CKEYWD(64) / '#EPSRE_T' /
      DATA CKEYWD(65) / '#IDR__NS' /
      DATA CKEYWD(66) / '#IDR__NL' /
      DATA CKEYWD(67) / '#USE_ALE' /
      DATA CKEYWD(68) / '#MRESALE' /
      DATA CKEYWD(69) / '#CL_TIME' /
      DATA CKEYWD(70) / '#ALE_DBG' /
      DATA CKEYWD(71) / '#ALELYNG' /
      DATA CKEYWD(72) / '#SPWRITE' /
      DATA CKEYWD(73) / '#GFWRITE' /
      DATA CKEYWD(74) / '#EPS_MID' /
      DATA CKEYWD(75) / '#AXUNROL' /
      DATA CKEYWD(76) / '#DBLGRID' /
      DATA CKEYWD(77) / '#SOL_VEL' /
      DATA CKEYWD(78) / '#DIV_ESC' /
      DATA CKEYWD(79) / '#MULTI_R' /
      DATA CKEYWD(80) / '#DSM_OLD' /
      DATA CKEYWD(81) / '#BLK_NUM' /
      DATA CKEYWD(82) / '#WRTOVST' /
      DATA CKEYWD(83) / '#MLPHASE' /
      DATA CKEYWD(84) / '#TUR_PRT' /
      DATA CKEYWD(85) / '#RFN_BOX' /
      DATA CKEYWD(86) / '#N_LAYER' /
      DATA CKEYWD(87) / '#N_LAYRT' /
      DATA CKEYWD(88) / '#MAP_SRC' /
      DATA CKEYWD(89) / '#RF__FIT' /
      DATA CKEYWD(90) / '#OVST_NF' /
      DATA CKEYWD(91) / '#BC_OVST' /
      DATA CKEYWD(92) / '#BLK_SIZ' /
      DATA CKEYWD(93) / '#CAV_MDL' /
C
      CHARACTER*60 CBUF,CBUF2
C
      CHARACTER*60 ERMSG1
     & / ' ## THIS PARAMETER DOES NOT WORK IN THIS VERSION (FFB8.1)  ' /
C
      CHARACTER*60 ERMSGB
     & / ' ## SUBROUTINE LESROP: FATAL      ERROR OCCURENCE; RETURNED' /
C
      CHARACTER*60 EREXP1
     & / ' AN ILLEGAL VALUE WAS SPECIFIED FOR CONTROL PARAMETER      ' /
C
      CHARACTER*5 CONOFF(0:1)
      DATA CONOFF(0) /'OFF  '/
      DATA CONOFF(1) /'ON   '/
C
C
C
C      READ CONTROL PARAMETERS FOR OPTIONAL FUNCTIONS
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          IUT0        ; FILE NUMBER TO WRITE  ERROR MESSAGE
C          IUT5        ; FILE NUMBER TO READ PARAMETERS AND FILE NAMES
C          IUT6        ; FILE NUMBER TO WRITE CALCULATION SEQUENCE
C          MSDEP       ; MAXIMUM NUMBER OF OVERSET SURFACES
C
C
C       (2) OUTPUT
C          IFILTR      ; CONTROLS GRID-FILTER WIDTH AS FOLLOWS:
C                   1 --- THE MINIMUM SIDE LENGTH
C                   2 --- INVERSE SQUARE AVERAGE OF SIDE LENGTH
C                   3 --- CUBIC ROUTE OF ELEMENT VOLUME
C
C          LOSEND      ; FLAG OF TREATING THE OVERSET FILE
C                       WHEN IT REACHES THE END
C                   0 --- THE POSITION IS FIXED AT THE END
C                         OF THE OVERSET FILE.
C                   1 --- THE OVERSET FILE WILL REOPEN AT THE HEAD
C
C          LFREE       ; FLAG OF TREATING THE FREE BOUNDARY
C                   0 --- NO SPECIAL TREATMENT
C                   1 --- THE NORMAL VELOCITY WILL BE SET TO ZERO
C                         IF REVERSE FLOW IS DETECTED
C                         AT THE FREE BOUNDARY.
C
C          INRBC       ; FLAG OF TREATING NON-REFLECTING B.C. (NRBC)
C                   0 --- OFF
C                   1 --- NRBC BASED ON ZERO PRESSURE GRADIENT
C                         IN INCOMPRESSIBLE PART
C                   2 --- NRBC BASED ON CONVECTIVE B.C.
C                         IN INCOMPRESSIBLE PART
C
C          IBCIO       ; SELECTION OF BOUNDARIES WHICH USE NRBC
C                   0 --- NRBC WILL NOT BE USED AT INLET AND OUTLET
C                   1 --- NRBC WILL BE USED ONLY AT INLET
C                   2 --- NRBC WILL BE USED ONLY AT OUTLET
C                   3 --- NRBC WILL BE USED AT INLET AND OUTLET
C
C          EPSM        ; CONTROLING THE TYPE OF PRESSURE B.C.
C                         AT INLET AND OUTLET, IF MACH NUMBER
C                         IS LARGER THAN 'EPSM'
C                         AND 'INRBC' IS GREATER THAN ZERO,
C                         NON-REFLECTING BOUNDARY CONDITION
C                         FOR THE COMPRESSIBLE PART OF THE PRESSURE
C                         WILL BE APPLIED
C
C          IPREF       ; FLAG OF PRESSURE REF. POINT
C                   0 --- NO PRESSURE REFERENCE POINT
C                   1 --- USE (XREF,YREF,ZREF) AS PRESSURE REF. POINT
C                   2 --- USE AVERAGE PRESSURE AT OUTLET AS
C                         REF. PRESSURE
C
C          XREF        ; X-COORDINATE OF PRESSURE REFERENCE POINT
C          YREF        ; Y-COORDINATE OF PRESSURE REFERENCE POINT
C          ZREF        ; Z-COORDINATE OF PRESSURE REFERENCE POINT
C
C          IMASS       ; FLAG OF MASS IMBALANCE CORRECTION (MIC)
C                   0 --- NO SPECIAL TREATMENT
C                   1 --- THE FLOW RATE AT EACH OVERSET DOMAIN WILL BE
C                         FORCED TO BALANCE BETWEEN INLET AND OUTLET.
C                   2 --- THE FLOW RATE AT EACH OVERSET SURFACE WILL BE
C                         FORCED TO BE EQUAL TO THE FLOW RATE
C                         AT OVERALL INLET.
C                         IN SOME CASES, IMASS=2 CAN NOT BE USED.
C
C          NSDEP       ; NUMBER OF OVERSET SURFACES
C
C
C          LHEAD       ; FLAG OF CALCULATION OF HEAD (PRESSURE INCREASE)
C                   0 --- HEAD WILL NOT BE CALCULATED.
C                   1 --- HEAD WILL BE CALCULATED.
C
C          ISOLU       ; MATRIX SOLVER FOR MOMENTUM EQUATIONS
C                   1 --- Bi-CGSTAB
C                   2 --- JACOBI ITERATION
C
C          ISOLP       ; MATRIX SOLVER FOR PRESSURE EQUATION
C                   1 --- Bi-CGSTAB
C                   2 --- RESIDUAL CUTTING METHOD (RCM)
C                   3 --- IDR
C
C          ISOLT       ; MATRIX SOLVER FOR HEAT TRANSFER EQUATION
C                   1 --- Bi-CGSTAB
C                   3 --- IDR
C
C          NMAXB       ; THE NUMBER OF INNER ITERATIONS OF RCM
C
C          NITRNR      ; ITERATION NUMBERS OF NEWTON-RAPHSON ITERATIONS
C                       FOR MOMENTUM EQUATIONS
C
C          CSMAX       ; MAXIMUM CS VALUE IN DSM
C
C          IPCAV       ; FLAG OF REFERENCE PRESSURE IN THE DEFINITION
C                       OF CAVITATION NUMBER
C                   0 --- USE PRESSURE AT OUTLET AS REFERENCE PRESSURE
C                   1 --- USE PRESSURE AT INLET AS REFERENCE PRESSURE
C
C          IDES        ; DUMMY PARAMETER
C
C          LVT0        ; DUMMY PARAMETER
C
C          NITRIT      ; ITERATION NUMBER OF INNER INTERATION FOR 
C                       MOMENTUM EQUATIONS IN OVERSET COMPUTATION 
C
C          NITRIP      ; ITERATION NUMBER OF INNER INTERATION FOR 
C                       PRESSURE EQUATION IN OVERSET COMPUTATION 
C
C          CBTD0       ;COEFFICIENT FOR THE BTD TERM
C
C          JSORT       ;RENUMBERING FLAG FOR SORTING      (0:OFF, 1:ON)
C          JCOLOR      ;RENUMBERING FLAG FOR COLORING     (0:OFF, 1:ON)
C          JUNROL      ;FULL UNROLL FLAG FOR AX OPERATION (0:OFF, 1:ON)
C
C          NLE         ;SIZE OF SPLITTED LOOP
C
C          NRFN        ;NUMBER OF REFINE  (DEFAULT=0)
C                       IT MUST BE ZERO OR ONE IN THIS VERSION 
C          IRFNMW      ;REFINED MESH AND BOUN FILE WILL BE OUTPUT
C                       WHEN THIS PARAMETER IS ONE (DEFAULT=0)      
C          IRFNFF      ;READ IN FLOW DATA (U,V,W,P) WILL BE REFINED
C                       WHEN THIS PARAMETER IS ONE (DEFAULT=0)      
C          NGRID       ;
C                       
C          IRCAPC      ;RCAP_COUPLER I.F. FLAG ,0 OFF (DEFAULT), 1 ON
C
C          IALE        ; FLAG OF MOVING B.C. IN ALE METHOD
C                   0 --- NO MOVING B.C.
C                   1 --- DEFINED BY REVOCAP
C                   2 --- DEFINED BY STANDARD FUNCTION
C                   3 --- DEFINED BY USER SUBROUTINE
C          NMAXA       ; NUMBER OF ITERATIONS
C          EPSA        ; ABSOLUTE CONVERGENCE CRITERIA
C          EPSREA      ; RELATIVE CONVERGENCE CRITERIA
C          STPWR       ; STIFFENING POWER FOR YOUNG'S MODULUS
C          NMODE       ; 
C          AOBJ        ; AMPLITUDE OF MOVING OBJECT
C          TOBJ        ; CYCLE     OF MOVING OBJECT
C
      IERR = 0
C
      DO 101 I=1,MAXPRO
          LPRO(I)=0
 101  CONTINUE
C
C     /// DUMMY VALUES
      THDT3D = 1.0E+10
      IDIAGV = 1
C
      ALPHAT=1.0
C
      EPSREV = -1.0
      EPSREP = -1.0
      EPSQ = -1.0
      EPSREQ = -1.0
      EPSA   = -1.0
      EPSREA = -1.0
C
   10 READ(IUT5,'(A60)',END=100) CBUF
      IF(CBUF(1:8).EQ.CKEYWD(1)) THEN
          WRITE(IUT6,*) ' LESROP: START READING OPTIONAL PARAMETERS '
      ELSE
          GO TO 10
      ENDIF
C
 1000 CONTINUE
C
      READ(IUT5,'(A60)') CBUF
C
C
      IF(CBUF(1:8).EQ.CKEYWD(3)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(3), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) IFILTR
          IF(IFILTR.LT.1 .OR. IFILTR.GT.3) THEN
              WRITE(IUT0,'(A60)')
              WRITE(IUT0,'(A60)') EREXP1
              GO TO 999
          ENDIF
C
          WRITE(IUT6,*) ' LESROP: GRID FILTER TYPE FLAG: IFILTR=',
     &                  IFILTR
      ENDIF
C
C
      IF(CBUF(1:8).EQ.CKEYWD(4)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(4), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG1
C
          LOSEND = 1
C
          WRITE(IUT6,*) ' LESROP: REOPEN FUNCTION OF OVERSET FILE '
      ENDIF
C
C
      IF(CBUF(1:8).EQ.CKEYWD(5)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(5), '" IS DETECTED.'
C
          LFREE = 1
C
          WRITE(IUT6,*) ' LESROP: REVERSE FLOW ZERO SET FUNCTION '
      ENDIF
C
C
      IF(CBUF(1:8).EQ.CKEYWD(6)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(6), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG1
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) INRBC
C
          IF(INRBC.LT.0 .OR. INRBC.GT.2) THEN
              WRITE(IUT0,'(A60)')
              WRITE(IUT0,'(A60)') EREXP1
              GO TO 999
          ENDIF
C
          WRITE(IUT6,*) ' LESROP: NRBC FLAG: INRBC =', INRBC
      ENDIF
C
C
      IF(CBUF(1:8).EQ.CKEYWD(7)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(7), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG1
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) IBCIO
          IF(IBCIO.LT.0 .OR. IBCIO.GT.3) THEN
              WRITE(IUT0,'(A60)')
              WRITE(IUT0,'(A60)') EREXP1
              GO TO 999
          ENDIF
C
          WRITE(IUT6,*) ' LESROP: NRBC FLAG: IBCIO =',IBCIO
          IF(IBCIO.EQ.0) THEN
              WRITE(IUT6,*) ' LESROP: NO NRBC AT INLET AND OUTLET'
          ELSE IF(IBCIO.EQ.1) THEN
              WRITE(IUT6,*) ' LESROP: NRBC ONLY AT INLET'
          ELSE IF(IBCIO.EQ.2) THEN
              WRITE(IUT6,*) ' LESROP: NRBC ONLY AT OUTLET'
          ELSE IF(IBCIO.EQ.3) THEN
              WRITE(IUT6,*) ' LESROP: NRBC AT INLET AND OUTLET'
          ENDIF
C
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(8)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(8), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG1
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) EPSM
          IF(EPSM.LE.0.E0) THEN
              WRITE(IUT0,'(A60)')
              WRITE(IUT0,'(A60)') EREXP1
              GO TO 999
          ENDIF
C
          WRITE(IUT6,'(A45,A13,1PE12.5)')
     &    '  LESROP: THE MINIMUM MACH NUMBER WHICH NRBC ',
     &    'CAN BE USED: ',EPSM
      ENDIF
C
C
      IF(CBUF(1:8).EQ.CKEYWD(9)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(9), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG1
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) IMASS
          IF(IMASS.LT.0 .OR. IMASS.GT.2) THEN
              WRITE(IUT0,'(A60)')
              WRITE(IUT0,'(A60)') EREXP1
              GO TO 999
          ENDIF
C
          IF(IMASS.GT.0) THEN
              READ(CBUF2,*) IDUM,NSDEP
              IF(NSDEP.LE.0 .OR. NSDEP.GT.MSDEP
     &           .OR. MOD(NSDEP,2).NE.0) THEN
                  WRITE(IUT0,'(A60)')
                  WRITE(IUT0,'(A60)') EREXP1
                  GO TO 999
              ENDIF
          ENDIF
C
          WRITE(IUT6,*) ' LESROP: MIC FLAG  : IMASS =', IMASS
          IF(IMASS.GT.0)
     &    WRITE(IUT6,*) ' LESROP: NUMBER OF MIC SURFACE : NSDEP =',
     &    NSDEP
C
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(10)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(10), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG1
C
          LHEAD = 1
          WRITE(IUT6,*) ' LESROP: HEAD CALCULATION FUNCTION OPEN'
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(11)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(11), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) ISOLT
          IF(ISOLT.NE.1 .AND. ISOLT.NE.3) THEN
              WRITE(IUT0,'(A60)')
              WRITE(IUT0,'(A60)') EREXP1
              GO TO 999
          ENDIF
C
          IF(ISOLT.EQ.1) THEN
              WRITE(IUT6,*)
     &        ' LESROP: BI-CGSTAB WILL BE APPLIED FOR TRANSPORT EQ.'
          ELSE
              WRITE(IUT6,*)
     &        ' LESROP: IDR       WILL BE APPLIED FOR TRANSPORT EQ.'
          ENDIF
C
      ENDIF
C
C
      IF(CBUF(1:8).EQ.CKEYWD(12)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(12), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) ISOLP
          IF(ISOLP.LT.1 .OR. ISOLP.GT.3)THEN
              WRITE(IUT0,'(A60)') ISOLP
              WRITE(IUT0,'(A60)') EREXP1
              GO TO 999
          ENDIF
C
          IF(ISOLP.EQ.1) THEN
              WRITE(IUT6,*)
     &        ' LESROP: BI-CGSTAB WIL BE APPLIED FOR PRS-E.Q.'
          ELSE IF(ISOLP.EQ.2) THEN
              WRITE(IUT6,*)
     *        ' LESROP: RCM WILL BE APPLIED FOR PRS.-E.Q.'
          ELSE IF(ISOLP.EQ.3) THEN
              WRITE(IUT6,*)
     *        ' LESROP: IDR WILL BE APPLIED FOR PRS.-E.Q.'
          ENDIF
C
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(13)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(13), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) NMAXB
          IF(NMAXB.LT.1)THEN
              WRITE(IUT0,'(A60)')
              WRITE(IUT0,'(A60)') EREXP1
              GO TO 999
          ENDIF
C
          WRITE(IUT6,*)
     &    ' LESROP: NUMBER OF RCM INNER ITERATIONS =',NMAXB
      ENDIF
C
C
      IF(CBUF(1:8).EQ.CKEYWD(14)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(14), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG1
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) NITRNR
          IF(NITRNR.LT.0)THEN
              WRITE(IUT0,'(A60)')
              WRITE(IUT0,'(A60)') EREXP1
              GO TO 999
          ENDIF
C
          WRITE(IUT6,*)
     &    ' LESROP: NUMBER OF NEWTON RAPSON ITERATIONS =',NITRNR
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(15)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(15), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG1
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) IDES
          IF(IDES.LT.0.OR.IDES.GT.2) THEN
              WRITE(IUT0,'(A60)')
              WRITE(IUT0,'(A60)') EREXP1
              GO TO 999
          ENDIF
C
C          WRITE(IUT6,*)
C     &    ' LESROP: DES MODE: IDES =',IDES
C          IF(IDES.EQ.0) THEN
C              WRITE(IUT6,*) ' LESROP: RANS/LES HYBRID MODE'
C          ELSE IF(IDES.EQ.1) THEN
C              WRITE(IUT6,*) ' LESROP: RANS ONLY MODE'
C          ELSE IF(IDES.EQ.2) THEN
C              WRITE(IUT6,*) ' LESROP: LES ONLY MODE'
C          ENDIF
          WRITE(IUT6,*) ' LESROP: DUMMY PARAMETER '
C
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(16)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(16), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG1
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) CSMAX
          IF(CSMAX.LT.0.E0)THEN
              WRITE(IUT0,'(A60)')
              WRITE(IUT0,'(A60)') EREXP1
              GO TO 999
          ENDIF
C
          WRITE(IUT6,'(A41,1PE12.5)')
     &    '  LESROP: UPPER LIMITATION OF CS IN DSM: ',CSMAX
      ENDIF
C
C
      IF(CBUF(1:8).EQ.CKEYWD(17)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(17), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG1
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) IPREF
          IF(IPREF.LT.0.OR.IPREF.GT.2) THEN
              WRITE(IUT0,'(A60)')
              WRITE(IUT0,'(A60)') EREXP1
              GO TO 999
          ENDIF
C
          WRITE(IUT6,*)
     &    ' LESROP: FLAG OF REFERENCE PRESSURE POINT: ',IPREF
C
          IF(IPREF.EQ.0) THEN
              WRITE(IUT6,*)
     &        ' LESROP: NO REFERENCE PRESSURE POINT '
          ELSE IF(IPREF.EQ.1) THEN
              READ(CBUF2,*) IDUM,XREF,YREF,ZREF
              WRITE(IUT6,'(A45,1PE12.5,1PE12.5,1PE12.5)')
     &        '  LESROP: COORDINATE OF REF. PRESSURE POINT: ',
     &        XREF,YREF,ZREF
          ELSE IF(IPREF.EQ.2) THEN
              WRITE(IUT6,*)
     &        ' LESROP: TAKE AVERAGE PRESSURE AT OUTLET AS',
     &        ' REFERENCE PRESSURE '
          ENDIF
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(18)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(18), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG1
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) IPCAV
          IF(IPCAV.LT.0.OR.IPCAV.GT.1) THEN
              WRITE(IUT0,'(A60)')
              WRITE(IUT0,'(A60)') EREXP1
              GO TO 999
          ENDIF
C
          WRITE(IUT6,*)
     &    ' LESROP: FLAG OF REF. PRESSURE IN THE DEF. OF CAVITATION',
     &    ' NUMBER:',IPCAV
          IF(IPCAV.EQ.0) THEN
              WRITE(IUT6,*) ' LESROP: USE PRESSURE AT OUTLET',
     &        ' AS REF. PRESSURE'
          ELSE IF(IPCAV.EQ.1) THEN
              WRITE(IUT6,*) ' LESROP: USE PRESSURE AT INLET',
     &        ' AS REF. PRESSURE'
          ENDIF
C
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(19)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(19), '" IS DETECTED.'
          WRITE(IUT6,*) ERMSG1
C
C          WRITE(IUT6,*)
C     &    ' LESROP: EDDY VISCOSITY WILL BE REINITIATED',
C     &    ' BY LOCAL EQUILIBRIUM ASSUMPTION'
C
          WRITE(IUT6,*) ' LESROP: DUMMY PARAMETER '
          LVT0 = 1
C
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(20)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(20), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) NITRIT
          IF(NITRIT.LT.1) THEN
              WRITE(IUT0,'(A60)')
              WRITE(IUT0,'(A60)') EREXP1
              GO TO 999
          ENDIF
C
          WRITE(IUT6,*) ' LESROP: INNER ITR. NUM. FOR MOM. EQ. =',
     &                  NITRIT
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(21)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(21), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) NITRIP
          IF(NITRIP.LT.1) THEN
              WRITE(IUT0,'(A60)')
              WRITE(IUT0,'(A60)') EREXP1
              GO TO 999
          ENDIF
C
          WRITE(IUT6,*) ' LESROP: INNER ITR. NUM. FOR MOM. EQ. =',
     &                  NITRIP
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(22)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(22), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) BTDCOE(1),BTDCOE(2),BTDCOE(3),BTDCOE(4)
          IF(BTDCOE(1).LT.0.0E0) THEN
              WRITE(IUT0,'(A60)')
              WRITE(IUT0,'(A60)') EREXP1
              GO TO 999
          ENDIF
C
          WRITE(IUT6,*) ' LESROP: COEFFICIENT OF BTD TERM '
          WRITE(IUT6,*)
          WRITE(IUT6,*) ' LESROP: BTD COEF FOR TET = ', BTDCOE(1)
          WRITE(IUT6,*) ' LESROP: BTD COEF FOR PYR = ', BTDCOE(2)
          WRITE(IUT6,*) ' LESROP: BTD COEF FOR WED = ', BTDCOE(3)
          WRITE(IUT6,*) ' LESROP: BTD COEF FOR HEX = ', BTDCOE(4)
      ENDIF
C
C
      IF(CBUF(1:8).EQ.CKEYWD(23)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(23), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) JSORT,NDIVX,NDIVY,NDIVZ
          IF(JSORT.NE.0 .AND. JSORT.NE.1) THEN
              WRITE(IUT0,'(A60)')
              WRITE(IUT0,'(A60)') EREXP1
              GO TO 999
          ENDIF
C
          WRITE(IUT6,*) ' LESROP: REORDERING '
          WRITE(IUT6,*) ' LESROP: SORT       :',CONOFF(JSORT )
          WRITE(IUT6,*) ' LESROP: NDIVX      =',NDIVX
          WRITE(IUT6,*) ' LESROP: NDIVY      =',NDIVY
          WRITE(IUT6,*) ' LESROP: NDIVZ      =',NDIVZ
C
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(24)) THEN
#ifdef NOMETIS
          WRITE(IUT0,*)
     *    ' LESROP: THIS OPTION IS NOT AVALABLE WITH THIS SOLVER.'
          GO TO 999
#endif
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(24), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) JCOLOR,NEIP(1),NEIP(2),NEIP(3),NEIP(4)
          IF(JCOLOR.NE.0 .AND. JCOLOR.NE.1) THEN
              WRITE(IUT0,'(A60)')
              WRITE(IUT0,'(A60)') EREXP1
              GO TO 999
          ENDIF
C
          WRITE(IUT6,*) ' LESROP: REORDERING '
          WRITE(IUT6,*) ' LESROP: COLORING   :',CONOFF(JCOLOR)
          WRITE(IUT6,*) ' LESROP: NEIP(TET)  =',NEIP(1)
          WRITE(IUT6,*) ' LESROP: NEIP(PRD)  =',NEIP(2)
          WRITE(IUT6,*) ' LESROP: NEIP(WED)  =',NEIP(3)
          WRITE(IUT6,*) ' LESROP: NEIP(HEX)  =',NEIP(4)
C
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(75)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(75),'" IS DETECTED.'
          JUNROL=1
C
          WRITE(IUT6,*) ' LESROP: AX OPERATION '
          WRITE(IUT6,*) ' LESROP: FULL UNROOL:',CONOFF(JUNROL)
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(76)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(76), '" IS DETECTED.'
C
          JGRID = 2
C
          WRITE(IUT6,*) 
     *    ' LESROP: DOUBLE PRECISION GRID DATA WILL BE READ'
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(25)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(25), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) OMEGA0(1),OMEGA0(2),OMEGA0(3)
C
          WRITE(IUT6,*) ' LESROP: MULTI-OMEGA FUNCTION '
          WRITE(IUT6,*) 
          WRITE(IUT6,*) ' NOTE THAT 1ST OMEGA WILL BE UPDATED'
          WRITE(IUT6,*) 
          WRITE(IUT6,*) ' LESROP: 1ST OMEGA           =',OMEGA0(1)
          WRITE(IUT6,*) ' LESROP: 2ND OMEGA           =',OMEGA0(2)
          WRITE(IUT6,*) ' LESROP: 3RD OMEGA           =',OMEGA0(3)
C
          OMEGA0(1)=OMEGA0(1)/(U000/D000)
          OMEGA0(2)=OMEGA0(2)/(U000/D000)
          OMEGA0(3)=OMEGA0(3)/(U000/D000)
C
      ENDIF
C
C     * #RCAP_RF *
      IF(CBUF(1:8).EQ.CKEYWD(26)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(26),'" IS DETECTED.'
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) NRFN,NGRID
C
          IF(NRFN.LT.0 .AND. NRFN.GT.5 ) THEN
              WRITE(IUT0,'(A60)')
              WRITE(IUT0,'(A60)') EREXP1
              WRITE(IUT0,'(A60)') 'NUM. OF REFINE MUST BE 0, 1 .. 5'
              GO TO 999
          ENDIF
C
          IF (NGRID.LT.-2) THEN
             WRITE(IUT0,'(A60)')
             WRITE(IUT0,'(A60)') EREXP1
             WRITE(IUT0,'(A60)') 'NUM. OF NGRID MUST BE LATGER THAN -2'
             GO TO 999
          ENDIF
C
          WRITE(IUT6,*) ' LESROP: NUMBER OF REFINE = ',NRFN
          IF(NGRID.EQ.-1) THEN
              WRITE(IUT6,*) ' LESROP: GLOBAL REFINE   '
          ELSE IF(NGRID.EQ.-2) THEN
              WRITE(IUT6,*) ' LESROP: READ REFINE TARGET SURFACE DATA'
          ELSE
              WRITE(IUT6,*) ' LESROP: NUMBER OF NGRID  = ',NGRID
          ENDIF
C
          DO 300 IRFN=1,NRFN
             IF (NGRID.GT.0) THEN
                NLAYER(IRFN)=NGRID
                NLAYRT(IRFN)=NGRID
             ELSE
                NLAYER(IRFN)=0
                NLAYRT(IRFN)=0
             ENDIF
 300      CONTINUE
C
       ENDIF
C
C
      IF(CBUF(1:8).EQ.CKEYWD(27)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(27), '" IS DETECTED.'
C
          IRFNMW = 1
C
          WRITE(IUT6,*) 
     *    ' LESROP: REFINED MESH AND B.C FILE WILL BE WRITTEN'
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(28)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(28), '" IS DETECTED.'
C
          IRFNFF = 1
C
          WRITE(IUT6,*) 
     *    ' LESROP: FLOW DATA READ IN WILL BE REFINED           '
      ENDIF
C
C      * #RCAP_CP *
      IF(CBUF(1:8).EQ.CKEYWD(29)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(29),'" IS DETECTED.'
          IRCAPC=1          
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(30)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(30), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) MER,MPR
C
          WRITE(IUT6,*) ' LESROP: MAX. NUMBER FOR GRID' 
          WRITE(IUT6,*) ' LESROP: MER  =',MER
          WRITE(IUT6,*) ' LESROP: MPR  =',MPR
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(31)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(31), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) MEPR,MPPR
C
          WRITE(IUT6,*) ' LESROP: MAX. NUMBER FOR CNNECTIVITY DATA' 
          WRITE(IUT6,*) ' LESROP: MEPR =',MEPR
          WRITE(IUT6,*) ' LESROP: MPPR =',MPPR
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(32)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(32), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) MBR,MBPR,MDOMR
C
          WRITE(IUT6,*) ' LESROP: MAX. NUMBER FOR B.C. DATA' 
          WRITE(IUT6,*) ' LESROP: MBR  =',MBR
          WRITE(IUT6,*) ' LESROP: MBPR =',MBPR
          WRITE(IUT6,*) ' LESROP: MDOMR=',MDOMR
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(33)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(33), '" IS DETECTED.'
C
          IMONTR = 1
          WRITE(IUT6,*) 
     *    ' LESROP: PARAMETER FILE WILL READ AT EVERY STEP'
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(34)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(34), '" IS DETECTED.'
C
          IAUTO=1    
          WRITE(IUT6,*) 
     *    ' LESROP: TIME INCREMENT WILL BE DETERMINED AUTOMATICALLY'
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(35)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(35), '" IS DETECTED.'
C
          ITLOCL=1
          WRITE(IUT6,*) 
     *    ' LESROP: LOCAL TIME INCREMENT WILL BE USED'
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(36)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(36), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) CFLSET
C
          WRITE(IUT6,*) 
     *    ' LESROP: CFLSET :',CLFSET
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(37)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(37), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) ALPHAP
C
          IF(ALPHAP.LE.0.0E0 .OR. ALPHAP.GT.1.0E0) THEN
              WRITE(IUT0,'(A60)')
              WRITE(IUT0,'(A60)') EREXP1
              GO TO 999
          ENDIF
C
          WRITE(IUT6,*) 
     *    ' LESROP: ALPHAP :',ALPHAP
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(38)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(38), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) ALPHAV
C
          IF(ALPHAV.LE.0.0E0 .OR. ALPHAV.GT.1.0E0) THEN
              WRITE(IUT0,'(A60)')
              WRITE(IUT0,'(A60)') EREXP1
              GO TO 999
          ENDIF
C
          WRITE(IUT6,*) 
     *    ' LESROP: ALPHAV :',ALPHAV
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(39)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(39), '" IS DETECTED.'
C
         IF (IALE.NE.0) THEN
            WRITE(IUT0,*)
     &' LESROP: THIS OPTION IS NOT AVAILABLE WITH [#USE_ALE] OPTION '
         ENDIF
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) NFFO
          DO 200 I=1,NFFO
              READ  (IUT5,*) NDUM
              BACKSPACE(IUT5) 
              READ  (IUT5,*) NDGCOE(I),(COEFFO(J,I),J=0,NDUM)
              WRITE (IUT6,'(I6,A3,8E10.2)')
     *        NDGCOE(I)," : ",(COEFFO(J,I),J=1,NDUM)
  200     CONTINUE    
C
          WRITE(IUT6,*) 
     *    ' LESROP: NFFO :',NFFO
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(40)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(40), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) NFRCNT
C
          WRITE(IUT6,*) 
     *    ' LESROP: NFRCNT :',NFRCNT
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(41)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(41), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) ALPHAQ
C
          IF(ALPHAQ.LT.0.0E0 .OR. ALPHAQ.GT.1.0E0) THEN
              WRITE(IUT0,'(A60)')
              WRITE(IUT0,'(A60)') EREXP1
              GO TO 999
          ENDIF
C
          WRITE(IUT6,*) 
     *    ' LESROP: ALPHAQ :',ALPHAQ
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(42)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(42), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) IDIAGV
C
CDEBG
C         // ALLOW DEBUG MODE: IDIAGV=99 FOR NO RELAXATION
          IF(IDIAGV.NE.0 .AND. IDIAGV.NE.1.AND.IDIAGV.NE.99) THEN
CDEBG
              WRITE(IUT0,'(A60)')
              WRITE(IUT0,'(A60)') EREXP1
              GO TO 999
          ENDIF
C
          WRITE(IUT6,*) 
     *    ' LESROP: IDIAGV :',IDIAGV
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(43)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(43), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) THDT3D
          IF(THDT3D.LT.1.0E0) THEN
              WRITE(IUT0,'(A60)')
              WRITE(IUT0,'(A60)') 'THDT3D MUST BE LARGER THAN 1.0'
              WRITE(IUT0,'(A60)') EREXP1
              GO TO 999
          ENDIF
C
          WRITE(IUT6,*) 
     *    ' LESROP: THDT3D :',THDT3D
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(44)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(44), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) PLIMIT
C
          WRITE(IUT6,*) 
     *    ' LESROP: PLIMIT :',PLIMIT
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(45)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(45), '" IS DETECTED.'
C
          JFSPRS=1
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(46)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(46), '" IS DETECTED.'
C
          JPRESS=2
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(47)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(47), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) KINIT
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(48)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(48), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) EPINIT
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(49)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(49), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) ALPHAK
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(50)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(50), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) ALPHEO
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(51)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(51), '" IS DETECTED.'
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(52)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(52), '" IS DETECTED.'
C
          IFIXFL=1
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(53)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(53), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) GRAV(1),GRAV(2),GRAV(3)
C
          WRITE(IUT6,*) ' LESROP: GRAVITY IS SET AS FOLLOWS; '
          WRITE(IUT6,*) ' LESROP: GX                  =',GRAV(1)
          WRITE(IUT6,*) ' LESROP: GY                  =',GRAV(2)
          WRITE(IUT6,*) ' LESROP: GZ                  =',GRAV(3)
C
          IGRAV=1
          GRAV(1)=GRAV(1)/(U000*U000/D000)
          GRAV(2)=GRAV(2)/(U000*U000/D000)
          GRAV(3)=GRAV(3)/(U000*U000/D000)
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(54)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(54), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) VELIN0(1),VELIN0(2),VELIN0(3)
          IVELIN=1
C
          WRITE(IUT6,*) ' LESROP: INLET VEL. IS SET AS FOLLOWS; '
          WRITE(IUT6,*) ' LESROP: U                  =',VELIN0(1)
          WRITE(IUT6,*) ' LESROP: V                  =',VELIN0(2)
          WRITE(IUT6,*) ' LESROP: Z                  =',VELIN0(3)
C
          VELIN0(1)=VELIN0(1)/U000
          VELIN0(2)=VELIN0(2)/U000
          VELIN0(3)=VELIN0(3)/U000
      ENDIF 
C
      IF(CBUF(1:8).EQ.CKEYWD(55)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(55), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) IPRO
          IF(IPRO.LT.2.OR.IPRO.GT.MAXPRO) THEN
              WRITE(IUT0,*) ' LESROP: INVALID PROPERTY-ID: ERROR '
              IERR=1
              RETURN
          ENDIF
          LPRO(IPRO)=1
          READ(IUT5,*) (CPRO0(I,IPRO),I=1,9)
          READ(IUT5,*) (CPRO1(I,IPRO),I=1,4)
          READ(IUT5,*) (CPRO2(I,IPRO),I=1,4)
          READ(IUT5,*) (CTREF(I,IPRO),I=1,4)
C
          WRITE(IUT6,*)
          WRITE(IUT6,*)  ' PROPERTY-ID   ' ,          IPRO
          WRITE(IUT6,*)  ' RHO_0         ' ,  CPRO0(1,IPRO)
          WRITE(IUT6,*)  ' CP_0          ' ,  CPRO0(2,IPRO)
          WRITE(IUT6,*)  ' NU_0          ' ,  CPRO0(3,IPRO)
          WRITE(IUT6,*)  ' LAMBDA_0      ' , (CPRO0(I,IPRO),I=4,9)
          WRITE(IUT6,*)  ' COEF. (1)     ' , (CPRO1(I,IPRO),I=1,4)
          WRITE(IUT6,*)  ' COEF. (2)     ' , (CPRO2(I,IPRO),I=1,4)
          WRITE(IUT6,*)  ' C-TREF.       ' , (CTREF(I,IPRO),I=1,4)
C
          CP000=(U000*U000/T000)
          VS000=(D000*U000)
          CD000=(RHO000*U000*U000*U000*D000/T000)
C
          CPRO0(1,IPRO)=CPRO0(1,IPRO)/RHO000
          CPRO0(2,IPRO)=CPRO0(2,IPRO)/CP000
          CPRO0(3,IPRO)=CPRO0(3,IPRO)/VS000
          CPRO0(4,IPRO)=CPRO0(4,IPRO)/CD000
          CPRO0(5,IPRO)=CPRO0(5,IPRO)/CD000
          CPRO0(6,IPRO)=CPRO0(6,IPRO)/CD000
          CPRO0(7,IPRO)=CPRO0(7,IPRO)/CD000
          CPRO0(8,IPRO)=CPRO0(8,IPRO)/CD000
          CPRO0(9,IPRO)=CPRO0(9,IPRO)/CD000
C
          CPRO1(1,IPRO)=CPRO1(1,IPRO)/(RHO000/T000)
          CPRO1(2,IPRO)=CPRO1(2,IPRO)/(CP000 /T000)
          CPRO1(3,IPRO)=CPRO1(3,IPRO)/(VS000 /T000)
          CPRO1(4,IPRO)=CPRO1(4,IPRO)/(CD000 /T000)
C
          CPRO2(1,IPRO)=CPRO2(1,IPRO)/(RHO000/T000/T000)
          CPRO2(2,IPRO)=CPRO2(2,IPRO)/(CP000 /T000/T000)
          CPRO2(3,IPRO)=CPRO2(3,IPRO)/(VS000 /T000/T000)
          CPRO2(4,IPRO)=CPRO2(4,IPRO)/(CD000 /T000/T000)
C
          CTREF(1,IPRO)=(CTREF(1,IPRO)-TREF)/T000
          CTREF(2,IPRO)=(CTREF(2,IPRO)-TREF)/T000
          CTREF(3,IPRO)=(CTREF(3,IPRO)-TREF)/T000
          CTREF(4,IPRO)=(CTREF(4,IPRO)-TREF)/T000
C
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(56)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(56), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) EPSK
C
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(57)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(57), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) EPSEP
C
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(58)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(58), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) EPSREV
C
          IF(EPSREV.LE.0.0E0) THEN
              WRITE(IUT0,'(A60)')
              WRITE(IUT0,'(A60)') EREXP1
              GO TO 999
          ENDIF
C
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(59)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(59), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) EPSREP
C
          IF(EPSREP.LE.0.0E0) THEN
              WRITE(IUT0,'(A60)')
              WRITE(IUT0,'(A60)') EREXP1
              GO TO 999
          ENDIF
C
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(60)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(60), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) EPSREK
C
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(61)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(61), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) EPSREO
C
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(62)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(62), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) ALPHAT
C
          IF(ALPHAT.LE.0.0E0 .OR. ALPHAT.GT.1.0E0) THEN
              WRITE(IUT0,'(A60)')
              WRITE(IUT0,'(A60)') EREXP1
              GO TO 999
          ENDIF
C
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(63)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(63), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) EPSQ
C
          IF(EPSQ.LE.0.0E0) THEN
              WRITE(IUT0,'(A60)')
              WRITE(IUT0,'(A60)') EREXP1
              GO TO 999
          ENDIF
C
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(64)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(64), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) EPSREQ
C
          IF(EPSREQ.LE.0.0E0) THEN
              WRITE(IUT0,'(A60)')
              WRITE(IUT0,'(A60)') EREXP1
              GO TO 999
          ENDIF
C
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(65)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(65), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) NS
C
          IF(NS.LT.1.OR.NS.GT.32) THEN
              WRITE(IUT0,'(A60)')
              WRITE(IUT0,'(A60)') EREXP1
              GO TO 999
          ENDIF
C
          WRITE(IUT6,*) ' LESROP: NS PARAMETER IN IDR METHOD',
     &                  NS
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(66)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(66), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) NL
C
          IF(NL.LT.1.OR.NL.GT.32) THEN
              WRITE(IUT0,'(A60)')
              WRITE(IUT0,'(A60)') EREXP1
              GO TO 999
          ENDIF
C
          WRITE(IUT6,*) ' LESROP: NL PARAMETER IN IDR METHOD',
     &                  NL
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(67)) THEN
         WRITE(IUT6,*) ' LESROP:'
         WRITE(IUT6,*) ' LESROP: "',CKEYWD(67), '" IS DETECTED.'
C
         IF (NFFO.NE.0) THEN
            WRITE(IUT0,*)
     &' LESROP: THIS OPTION IS NOT AVAILABLE WITH [#FFOBJCT] OPTION '
         ENDIF
C
         CBUF2 = CBUF(9:60)
         READ(CBUF2,*) IALE,NMAXA,EPSA,STPWR,NMODE,AOBJ,TOBJ
C
         IF (IALE.EQ.1) THEN
            IF (IRCAPC.EQ.0) THEN
               WRITE(IUT0,*)
     &' LESROP: [#RCAP_CP] OPTION MUST BE SET ON AHEAD'
               GO TO 999
            ENDIF
         ELSE IF (IALE.EQ.2) THEN
            IF (NMODE.LE.0.OR.AOBJ .LT.0.0E0.OR.TOBJ.LE.0.0E0) THEN
               WRITE(IUT0,'(A60)')
               WRITE(IUT0,'(A60)') EREXP1
               GO TO 999
            ENDIF
         ELSE IF (IALE.EQ.3) THEN
            WRITE(IUT0,*)
     &' LESROP: THIS ALE MODE IS NOT AVALABLE IN THIS VERSION '
            GO TO 999
         ELSE IF (IALE.LT.1.OR.IALE.GT.3) THEN
            WRITE(IUT0,'(A60)')
            WRITE(IUT0,'(A60)') EREXP1
            GO TO 999
         ENDIF
C
         IF(STPWR.LT.0.0E0.OR.NMAXA.LE.0.OR.EPSA.LE.0.0E0) THEN
            WRITE(IUT0,'(A60)')
            WRITE(IUT0,'(A60)') EREXP1
            GO TO 999
         ENDIF
         EPSREA=EPSA
C
         WRITE(IUT6,*)
     &   ' LESROP: ALE METHOD WILL BE USED FOR MOVING MESH '
         WRITE(IUT6,*) ' LESROP: MODE: IALE         = ', IALE
         WRITE(IUT6,*) ' LESROP: NUM. OF ITERATIONS = ', NMAXA
         WRITE(IUT6,*) ' LESROP: CONV. CRITERIA     = ', EPSA
         WRITE(IUT6,*) ' LESROP: STIFFENING POWER   = ', STPWR
         IF (IALE.EQ.2) THEN
            WRITE(IUT6,*) ' LESROP: PARAM MODE         = ', NMODE
            WRITE(IUT6,*) ' LESROP: AMP. OF OBJECT     = ', AOBJ
            WRITE(IUT6,*) ' LESROP: CYC. OF OBJECT     = ', TOBJ
         ENDIF
C
         AOBJ=AOBJ/D000
         TOBJ=TOBJ*U000/D000
C
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(68)) THEN
         WRITE(IUT6,*) ' LESROP:'
         WRITE(IUT6,*) ' LESROP: "',CKEYWD(68), '" IS DETECTED.'
         MRSALE=1
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(69)) THEN
         WRITE(IUT6,*) ' LESROP:'
         WRITE(IUT6,*) ' LESROP: "',CKEYWD(69), '" IS DETECTED.'
         IWRTIM=1
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(70)) THEN
         WRITE(IUT6,*) ' LESROP:'
         WRITE(IUT6,*) ' LESROP: "',CKEYWD(70), '" IS DETECTED.'
         IALEDB=1
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(71)) THEN
         WRITE(IUT6,*) ' LESROP:'
         WRITE(IUT6,*) ' LESROP: "',CKEYWD(71), '" IS DETECTED.'
C
         CBUF2 = CBUF(9:60)
         READ(CBUF2,*) NLYNG,EYNG
C
         IF(NLYNG.LT.0.OR.EYNG.LE.0.0E0) THEN
            WRITE(IUT0,'(A60)')
            WRITE(IUT0,'(A60)') EREXP1
            GO TO 999
         ENDIF
C
         WRITE(IUT6,*) ' LESROP: NUM. OF LAYER  = ', NLYNG
         WRITE(IUT6,*) ' LESROP: YOUNGS MODULUS = ', EYNG
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(72)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(72), '" IS DETECTED.'
C
          JSPADV = 1
C
          WRITE(IUT6,*) ' LESROP: SURFACE PRESSURE WILL '
          WRITE(IUT6,*) ' BE WRITTEN IN ADVENTURE FORMAT '
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(73)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(73), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) IWRITE
C
          WRITE(IUT6,*) ' LESROP: GF-WRITE-MODE IWRITE IBCIO =',IWRITE
          IF(IWRITE.EQ.1) THEN
              WRITE(IUT6,*) ' LESROP: ASCII-MODE' 
          ELSE IF(IBCIO.EQ.2) THEN
              WRITE(IUT6,*) ' LESROP: UNFORMATED-MODE'
          ELSE IF(IBCIO.EQ.3) THEN
              WRITE(IUT6,*) ' LESROP: BINARY MODE'
          ELSE 
              IWRITE=2
              WRITE(IUT6,*) ' LESROP: UNFORMATED-MODE'
          ENDIF
C
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(74)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(74), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) EPSMID
C
          IF(EPSMID.LE.0.0E0) THEN
              WRITE(IUT0,'(A60)')
              WRITE(IUT0,'(A60)') EREXP1
              GO TO 999
          ENDIF
C
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(77)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(77), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) ISOLV
          IF(ISOLV.NE.1 .AND. ISOLV.NE.2) THEN
              WRITE(IUT0,'(A60)')
              WRITE(IUT0,'(A60)') EREXP1
              GO TO 999
          ENDIF
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(78)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(78), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) DIVESC
C
          IF(EPSMID.LE.0.0E0) THEN
              WRITE(IUT0,'(A60)')
              WRITE(IUT0,'(A60)') EREXP1
              GO TO 999
          ENDIF
C
          WRITE(IUT6,*) ' LESROP: DIVESC = ', DIVESC
C
      ENDIF
C
C
      IF(CBUF(1:8).EQ.CKEYWD(79)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(79), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) IMRF,OMEGA2,AX,AY,AZ,ORGX,ORGY,ORGZ
          IF(IMRF.LT.2 .AND. IMRF.GT.MMRF) THEN
              WRITE(IUT0,'(A60)')
              WRITE(IUT0,'(A60)') EREXP1
              GO TO 999
          ENDIF
C
          IF(AZ.NE.0.0E0) THEN
              WRITE(IUT0,'(A60)') 'CURRENT VERSION DOES NOT SUPPORT '
              WRITE(IUT0,'(A60)') 'Z-COMPONENT FOR JITEN-AXIS       ' 
              WRITE(IUT0,'(A60)') 'SET 0.0E0 FOR AZ                 ' 
              WRITE(IUT0,'(A60)') EREXP1
              GO TO 999
          ENDIF
C
          IF(ORGX.NE.0.0E0 .OR. ORGY.NE.0.0E0 .OR. ORGZ.NE.0.0E0) THEN
              WRITE(IUT0,'(A60)') 'CURRENT VERSION DOES NOT SUPPORT '
              WRITE(IUT0,'(A60)') 'OFF-SET OF JITEN-AXIS            ' 
              WRITE(IUT0,'(A60)') 'SET 0.0 0.0 0.0 FOR ORGX,Y,Z     ' 
              WRITE(IUT0,'(A60)') EREXP1
              GO TO 999
          ENDIF
C
          AA = SQRT(AX*AX+AY*AY+AZ*AZ)
          OMGMRF(  IMRF) = OMEGA2/(U000/D000)
          AMRF  (1,IMRF) = AX/AA
          AMRF  (2,IMRF) = AY/AA
          AMRF  (3,IMRF) = AZ/AA
          ORGMRF(1,IMRF) = ORGX
          ORGMRF(2,IMRF) = ORGY
          ORGMRF(3,IMRF) = ORGZ
C
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(79), '" IS DETECTED.'
          WRITE(IUT6,*) ' LESROP: ID          :',IMRF
          WRITE(IUT6,*) ' LESROP: OMEGA2      :',OMGMRF(IMRF)
          WRITE(IUT6,*) ' LESROP: DIR. OF AXIS:',(  AMRF(I,IMRF),I=1,3)
          WRITE(IUT6,*) ' LESROP: ORG. OF AXIS:',(ORGMRF(I,IMRF),I=1,3)
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(80)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(80), '" IS DETECTED.'
C
          IDSM = 1
C
          WRITE(IUT6,*) ' LESROP: DSM IN LES3C WILL BE USED '
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(81)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(81), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) NBLKX,NBLKY,NBLKZ
C
          WRITE(IUT6,*) ' LESROP: NBLKX      =',NBLKX
          WRITE(IUT6,*) ' LESROP: NBLKY      =',NBLKY
          WRITE(IUT6,*) ' LESROP: NBLKZ      =',NBLKZ
C
      ENDIF
C
C
      IF(CBUF(1:8).EQ.CKEYWD(82)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(82), '" IS DETECTED.'
C
          JWRTOS = 1
C
          WRITE(IUT6,*) ' LESROP: OVERSET DATA WILL WRITTEN'
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(83)) THEN
         WRITE(IUT6,*) ' LESROP:'
         WRITE(IUT6,*) ' LESROP: "',CKEYWD(83), '" IS DETECTED.'
C
         CBUF2 = CBUF(9:60)
         READ(CBUF2,*) IVOF,NSCYC,RHOF2,VISCM2,NMAXVF,EPSVF
C
         IF(NSCYC.LT.1) THEN
            WRITE(IUT0,'(A60)')
            WRITE(IUT0,'(A60)') EREXP1
            GO TO 999
         ENDIF
C
         IF(IVOF.LT.1.OR.IVOF.GT.2) THEN
            WRITE(IUT0,'(A60)')
            WRITE(IUT0,'(A60)') EREXP1
            GO TO 999
         ENDIF
C
         IF(RHOF2.LT.0.0E0.OR.VISCM2.LT.0.0E0) THEN
            WRITE(IUT0,'(A60)')
            WRITE(IUT0,'(A60)') EREXP1
            GO TO 999
         ENDIF
C
         IF(NMAXVF.LE.0.OR.EPSVF.LE.0.0E0) THEN
            WRITE(IUT0,'(A60)')
            WRITE(IUT0,'(A60)') EREXP1
            GO TO 999
         ENDIF
         EPSRVF=EPSVF
C
         WRITE(IUT6,*)
     *   ' LESROP: MODE: IVOF                          = ', IVOF
         WRITE(IUT6,*)
     *   ' LESROP: NUMBER OF SUBCYCLE                  = ', NSCYC
         WRITE(IUT6,*)
     *   ' LESROP: DENSITY OF SECOND FLUID             = ', RHOF2
         WRITE(IUT6,*)
     *   ' LESROP: MOLECULAR VISCOSITY OF SECOND FLUID = ', VISCM2
         WRITE(IUT6,*)
     *   ' LESROP: NUM. OF ITERATIONS                  = ', NMAXVF
         WRITE(IUT6,*)
     *   ' LESROP: CONV. CRITERIA                      = ', EPSVF
C
         RHOF2 =RHOF2 /RHO000
         VISCM2=VISCM2/(U000*D000)
C
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(84)) THEN
         WRITE(IUT6,*) ' LESROP:'
         WRITE(IUT6,*) ' LESROP: "',CKEYWD(84), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) PRT
C
          WRITE(IUT6,*) ' LESRO: TURABULENT PRANTL NUMBER =',PRT
C
          IF(PRT.LE.0) THEN
             WRITE(IUT0,'(A60)')
             WRITE(IUT0,'(A60)') EREXP1
             GO TO 999
          ENDIF
C
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(85)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(83), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) IRFNB, XRMIN, YRMIN, ZRMIN, XRMAX, YRMAX, ZRMAX
          XRFMIN(IRFNB)=XRMIN
          XRFMAX(IRFNB)=XRMAX

          YRFMIN(IRFNB)=YRMIN
          YRFMAX(IRFNB)=YRMAX

          ZRFMIN(IRFNB)=ZRMIN
          ZRFMAX(IRFNB)=ZRMAX
          WRITE(IUT6,*) "    BOUNDING BOX: "
          WRITE(IUT6,*) "        ", 
     *         XRFMIN(IRFNB), YRFMIN(IRFNB), ZRFMIN(IRFNB)
          WRITE(IUT6,*) "        ", 
     *         XRFMAX(IRFNB), YRFMAX(IRFNB), ZRFMAX(IRFNB)
C
          IRFBOX=1
C
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(86)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(84), '" IS DETECTED.'
C
          IF (NGRID.NE.-2) THEN
             WRITE(IUT0,*)' LESROP: THIS OPTION IS NOT AVALABLE'
             WRITE(IUT0,*)' BECAUSE NGRID IS NOT -2'
             GO TO 999
          ENDIF
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) NLAYER(1), NLAYER(2), NLAYER(3), NLAYER(4)
C
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(87)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(85), '" IS DETECTED.'
C
          IF (NGRID.NE.-2) THEN
             WRITE(IUT0,*)' LESROP: THIS OPTION IS NOT AVALABLE'
             WRITE(IUT0,*)' BECAUSE NGRID IS NOT -2'
             GO TO 999
          ENDIF
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) NLAYRT(1), NLAYRT(2), NLAYRT(3), NLAYRT(4)
C
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(88)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(86), '" IS DETECTED.'
C
          JSSMAP=1
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) NUMSSB(1),NUMSSB(2),NUMSSB(3),NOUTSS,NITRSS,
     *                  CODSSB(1),CODSSB(2),CODSSB(3),CODSSB(4)
          IF(BTDCOE(1).LT.0.0E0) THEN
              WRITE(IUT0,'(A60)')
              WRITE(IUT0,'(A60)') EREXP1
              GO TO 999
          ENDIF
C
          WRITE(IUT6,*)
          WRITE(IUT6,*) ' LESROP: PARAMETERS OF SOUND SOURCE BOX'
          WRITE(IUT6,*) ' LESROP: NX                 ', NUMSSB(1)
          WRITE(IUT6,*) ' LESROP: NY                 ', NUMSSB(2)
          WRITE(IUT6,*) ' LESROP: NZ                 ', NUMSSB(3)
          WRITE(IUT6,*) ' LESROP: OUTPUT INTERVAL    ', NOUTSS
          WRITE(IUT6,*) ' LESROP: NUM. OF TEST-FILTER', NITRSS
          WRITE(IUT6,*) ' LESROP: X0                 ', CODSSB(1)
          WRITE(IUT6,*) ' LESROP: Y0                 ', CODSSB(2)
          WRITE(IUT6,*) ' LESROP: Z0                 ', CODSSB(3)
          WRITE(IUT6,*) ' LESROP: GRID SIZE          ', CODSSB(4)
          WRITE(IUT6,*)
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(89)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(89), '" IS DETECTED.'
C
          IRFNFT = 1
C
          ICS=0
          ICE=0
          DO 2000 I=9,60
             IF (ICS.EQ.0.AND.CBUF(I:I).NE.' ') THEN
                ICS=I
                GOTO 2000
             ENDIF
             IF (ICS.GT.0.AND.CBUF(I:I).EQ.' ') THEN
                ICE=I-1
                GOTO 2010
             ENDIF
 2000     CONTINUE
 2010     CONTINUE
C
          NC=ICE-ICS+1
          FILECD(1:NC) = CBUF(ICS:ICE)
C
          CALL RFNAME(FILECD,FILECR,IUT0,IERR)
          IF(IPART.GE.1) THEN
             CALL MFNAME(FILECD,FILE,IPART,IUT0,IERR)
             FILECD = FILE
             CALL MFNAME(FILECR,FILE,IPART,IUT0,IERR)
             FILECR = FILE
          ENDIF
          IF (IERR.NE.0) GOTO 999
C
          WRITE(IUT6,'(A29,A52)') ' LESROP: REVOCAP CAD FILE = ',FILECD
C
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(90)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(90), '" IS DETECTED.'
C
          JNTFND=1
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) EPSOS
C
          WRITE(IUT6,'(A29,E13.5)') ' LESROP: DISTANS CRITERIA = ',EPSOS
C
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(91)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(90), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) COSBIN,COSBFR
          IF(      COSBIN.LT.0.0E0 .OR. COSBIN.GT.1.0E0
     *        .OR. COSBFR.LT.0.0E0 .OR. COSBFR.GT.1.0E0 ) THEN
              WRITE(IUT0,'(A60)')
              WRITE(IUT0,'(A60)') EREXP1
              GO TO 999
          ENDIF
C
          WRITE(IUT6,*) ' LESROP: COEFFICIENT OF BTD TERM '
          WRITE(IUT6,*)
          WRITE(IUT6,*) ' LESROP: COEF FOR O.S. VEL. BC.:',COSBIN
          WRITE(IUT6,*) ' LESROP: COEF FOR O.S. PRS. BC.:',COSBFR
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(92)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(92), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) EPSBLK,BLKMIN
          IF(BTDCOE(1).LT.0.0E0) THEN
              WRITE(IUT0,'(A60)')
              WRITE(IUT0,'(A60)') EREXP1
              GO TO 999
          ENDIF
C
          WRITE(IUT6,*) ' LESROP: COEFFICIENT OF BTD TERM '
          WRITE(IUT6,*)
          WRITE(IUT6,*) ' LESROP: BLOCK MERGIN RATIO  = ', EPSBLK
          WRITE(IUT6,*) ' LESROP: MIN. SIZE OF BLOCAK = ', BLKMIN
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(93)) THEN
          WRITE(IUT6,*) ' LESROP:'
          WRITE(IUT6,*) ' LESROP: "',CKEYWD(93), '" IS DETECTED.'
C
          CBUF2 = CBUF(9:60)
          READ(CBUF2,*) ICAVI,CGAS,CLQD,F0
          IF(ICAVI.EQ.1) THEN
              WRITE(IUT6,*) ' LESROP: OKITA MODEL IS SELECTED:'
              WRITE(IUT6,*) ' LESROP: CGAS                   :',CGAS
              WRITE(IUT6,*) ' LESROP: CLQD                   :',CLQD
          ELSE IF(ICAVI.EQ.2) THEN
              WRITE(IUT6,*) ' LESROP: MARKLE MODEL IS SELECTED:'
              WRITE(IUT6,*) ' LESROP: CGAS                   :',CGAS
              WRITE(IUT6,*) ' LESROP: CLQD                   :',CLQD
          ELSE IF(ICAVI.EQ.3) THEN
              WRITE(IUT6,*) ' LESROP: KUNZE  MODEL IS SELECTED:'
              WRITE(IUT6,*) ' LESROP: CGAS(DASH)             :',CGAS
              WRITE(IUT6,*) ' LESROP: CLQD                   :',CLQD
              WRITE(IUT6,*) ' LESROP: F0                     :',F0
          ELSE
              IERR=1
              RETURN
          ENDIF
      ENDIF
C
      IF(CBUF(1:8).EQ.CKEYWD(2)) THEN
          WRITE(IUT6,*) ' LESROP: END   READING OPTIONAL PARAMETERS'
      ELSE
          GO TO 1000
      ENDIF
C
C     DEFAULT VALUES
C
      IF(EPSREV.LT.0.0) EPSREV = EPST
      IF(EPSREP.LT.0.0) EPSREP = EPSP
      IF(EPSQ.LT.0.0) EPSQ = EPST
      IF(EPSREQ.LT.0.0) EPSREQ = EPSQ
C
      RETURN
C
  999 CONTINUE
CC    IDUM = IDUM
      WRITE(IUT0,'(A60)') ERMSGB
      IERR = 1
      RETURN
C
  100 WRITE(IUT6,*) ' LESROP: NO OPTIONAL PARAMETERS'
      RETURN
C
C
      END
