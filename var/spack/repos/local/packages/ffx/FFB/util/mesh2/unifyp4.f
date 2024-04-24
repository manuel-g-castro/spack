C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.5.0                                   C
C                                                                      C
C  MAIN PRORGRAM  UNIFY                                                C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      PROGRAM MAIN
      IMPLICIT NONE
      INCLUDE 'gf2.h'
      INTEGER*4 N,MP,ME,MB,MPART,MBPT,MEPT,MPPT 
      PARAMETER ( N = 8 )
C
      CHARACTER*60 COMM(2)
      DATA COMM(1) /'** UNIFY: PRESSURE IS DEFINED AT ELEMENTS **'/
      DATA COMM(2) /'** UNIFY: PRESSURE IS DEFINED AT NODES    **'/
C
C
C ARRAYS HOLDING THE GLOBAL-DOMAIN DATA
C
C
      INTEGER*4, ALLOCATABLE:: 
     *           NODE(:,:),NODEPT(:,:),
     *           LISTIP(:),LISTIE(:),
     *           LPINLT(:),LPMWAL(:),LPWALL(:),
     *           LPSYMT(:),LPFREE(:),LPBODY(:),
     *           LPINT1(:),LPINT2(:),LPINT3(:),
     *           JFGRID(:),JFNODE(:),JFVELO(:),JFPRES(:),
     *           JFINLT(:),JFMWAL(:),JFWALL(:),JFSYMT(:),
     *           JFBODY(:),JFFREE(:)
      REAL*4,    ALLOCATABLE:: 
     *           X  (:),Y  (:),Z  (:),
     *           XPT(:),YPT(:),ZPT(:),
     *           U  (:),V  (:),W  (:),P  (:),PN  (:),FE  (:),F(:),
     *           UPT(:),VPT(:),WPT(:),PPT(:),PNPT(:),FEPT(:),FPT(:)
C
C
C FILES TO BE ACCESSED
C
C
      CHARACTER*60 FILEDD, FILEMS, FILEBC, FILEFF, FILEPT
      CHARACTER*60         FILEMW, FILEBW, FILEFW
C
      INTEGER*4 IUT0,IUT5,IUT6,IUTDD,IUTMS,IUTBC,IUTFF
      DATA IUT0   /  0 /
      DATA IUT5   /  5 /
      DATA IUT6   /  6 /
      DATA IUTDD  /  2 /
      DATA IUTMS  /  8 /
      DATA IUTBC  /  9 /
      DATA IUTFF  / 10 /
C
      INTEGER*4  JMESH,JBOUN,JFLOW,JCHECK,IERR,IP,IE,I,
     *           IPART,NPART,IPPT,NPPT,IEPT,NEPT,NPCHK,NECHK,
     *           NPINLT,NPMWAL,NPWALL,NPSYMT,NPFREE,NPBODY,NPINT,
     *           IPINLT,IPMWAL,IPWALL,IPSYMT,IPBODY,IPFREE,
     *           NEDUM,IPRS,NPDUM,NDUM,
     *           NP,NE,NPMESH,NEMESH,ISTEP
      REAL*4     TIME
C
C
C      A GENERAL PURPOSE UTILITY PROGRAM FOR COMBINING SUB-DOMAIN DATA
C     FILES TO MAKE A GLOBAL-DOMAIN DATA FILE.
C
C                        VERSION 1998. 9. 17
C                        VERSION 2007. 5. 08
C
C      THIS PROGRAM READS A DOMAIN DECOMPOSITION DESCRIPTION FILE
C     (DDD FILE), SUB-DOMAIN DATA FILES FOR MESH, BOUNDARY CONDITIONS,
C     AND/OR FLOW FIELD, COMBINES THEM, AND OUTPUTS A CORRESPONDING
C     GLOBAL-DOMAIN DATA FILE(S).
C
C      NOTES: A DDD FILE MUST ALWAYS BE SPECIFIED TO COMBINE ANY OF
C            MESH, BOUNDRAY CONDITIONS, AND FLOW FIELD DATA FILES, WHILE
C            MESH, BOUNDARY CONDITIONS, AND FLOW FIELD DATA FILES 
C            MUST BE SPECIFIED ONLY FOR THOSE FILES TO BE COMBINED.
C
C      NOTES: INTER-CONNECT BOUNDARY CONDITIONS DATA (*BC_INTR) 
C            CONTAINED IN EACH OF SUB-DOMIAN BOUNDARY CONDITIONS DATA
C            FILES WILL BE DISCARDED WHEN COMBINING SUB-DOMAIN DATA
C            FILES.
C        
C
C
      IPRS=0
C
      WRITE(IUT6,*)
      WRITE(IUT6,*) ' ** UNIFY: UNIFYING SUB-DOMAIN DATA FILES **'
C
   10 CONTINUE
C
      WRITE(IUT6,*)
      WRITE(IUT6,*) ' SPECIFY DOMAIN DECOMPOSITION DESCRIPTION FILE'
      READ (IUT5,'(A60)') FILEDD
C
      JMESH = 0
      WRITE(IUT6,*)
      WRITE(IUT6,*) ' SPECIFY FILENAME OF MESH DATA'
      READ (IUT5,'(A60)') FILEMS
      IF(FILEMS.NE.' ') JMESH = 1
C
      JBOUN = 0
      WRITE(IUT6,*) ' SPECIFY FILENAME OF BOUNDARY DATA'
      READ (IUT5,'(A60)') FILEBC
      IF(FILEBC.NE.' ') JBOUN = 1
C
      JFLOW = 0
      WRITE(IUT6,*) ' SPECIFY FILENAME OF FLOW FIELD DATA'
      READ (IUT5,'(A60)') FILEFF
      IF(FILEFF.NE.' ') JFLOW = 1
C
      WRITE(IUT6,*)
      WRITE(IUT6,*) 
     *' SPECIFY FILENAME OF MESH DATA FOR OUTPUT'
      READ (IUT5,'(A60)') FILEMW
C
      WRITE(IUT6,*) 
     *' SPECIFY FILENAME OF BOUNDARY DATA FOR OUTPUT'
      READ (IUT5,'(A60)') FILEBW
C
      WRITE(IUT6,*) 
     *' SPECIFY FILENAME OF FLOW FIELD DATA FOR OUTPUT'
      READ (IUT5,'(A60)') FILEFW
C
C
C
C CONFIRM GIVEN PARAMETERS
C
C
C
      WRITE(IUT6,*)
      WRITE(IUT6,*) 'DESCRIPTION FILE:', FILEDD
      IF(JMESH.EQ.1) WRITE(IUT6,*) 'MESH DATA FILE:', FILEMS
      IF(JBOUN.EQ.1) WRITE(IUT6,*) 'BC   DATA FILE:', FILEBC
      IF(JFLOW.EQ.1) WRITE(IUT6,*) 'FLOW DATA FILE:', FILEFF
C
      WRITE(IUT6,*)
      WRITE(IUT6,*) ' ENTER 1 IF ABOVE PARAMETERS ARE OK'
      READ (IUT5,*) JCHECK
      IF(JCHECK.NE.1) GO TO 10
C
C
C
C CHECK SIZE
C
C
      CALL CHKDDD(FILEDD,IUTDD,IUT0,IUT6,
     *            MPART,MPPT,MEPT,MBPT,IERR)
      IF(IERR.NE.0) GOTO 9500
      MP=MPPT*MPART
      ME=MEPT*MPART
      MB=MBPT*MPART
      MB=MP
C
C
C
C
C ALLOCATE
C
C
C
C
      ALLOCATE( X     (    MP), STAT=LERR(01))
      ALLOCATE( Y     (    MP), STAT=LERR(02))
      ALLOCATE( Z     (    MP), STAT=LERR(03))
      ALLOCATE( NODE  (  N,ME), STAT=LERR(04))
      ALLOCATE( U     (    MP), STAT=LERR(05))
      ALLOCATE( V     (    MP), STAT=LERR(06))
      ALLOCATE( W     (    MP), STAT=LERR(07))
      ALLOCATE( P     (    ME), STAT=LERR(08))
      ALLOCATE( PN    (    MP), STAT=LERR(09))
      ALLOCATE( FE    (    ME), STAT=LERR(09))
      ALLOCATE( F     (    MP), STAT=LERR(09))
      ALLOCATE( LISTIP(  MPPT), STAT=LERR(10))
      ALLOCATE( LISTIE(  MEPT), STAT=LERR(11))
      ALLOCATE( XPT   (  MPPT), STAT=LERR(12))
      ALLOCATE( YPT   (  MPPT), STAT=LERR(13))
      ALLOCATE( ZPT   (  MPPT), STAT=LERR(14))
      ALLOCATE( NODEPT(N,MEPT), STAT=LERR(15))
      ALLOCATE( UPT   (  MPPT), STAT=LERR(16))
      ALLOCATE( VPT   (  MPPT), STAT=LERR(17))
      ALLOCATE( WPT   (  MPPT), STAT=LERR(18))
      ALLOCATE( PPT   (  MEPT), STAT=LERR(19))
      ALLOCATE( PNPT  (  MPPT), STAT=LERR(20))
      ALLOCATE( FEPT  (  MEPT), STAT=LERR(20))
      ALLOCATE( FPT   (  MPPT), STAT=LERR(20))
      ALLOCATE( LPINLT(    MB), STAT=LERR(21))
      ALLOCATE( LPMWAL(    MB), STAT=LERR(22))
      ALLOCATE( LPWALL(    MB), STAT=LERR(23))
      ALLOCATE( LPSYMT(    MB), STAT=LERR(24))
      ALLOCATE( LPFREE(    MB), STAT=LERR(25))
      ALLOCATE( LPBODY(    MB), STAT=LERR(26))
      ALLOCATE( LPINT1(    MB), STAT=LERR(27))
      ALLOCATE( LPINT2(    MB), STAT=LERR(28))
      ALLOCATE( LPINT3(    MB), STAT=LERR(29))
      ALLOCATE( JFGRID(    MP), STAT=LERR(30))
      ALLOCATE( JFNODE(    ME), STAT=LERR(31))
      ALLOCATE( JFVELO(    MP), STAT=LERR(32))
      ALLOCATE( JFPRES(    ME), STAT=LERR(33))
      ALLOCATE( JFINLT(    MP), STAT=LERR(34))
      ALLOCATE( JFMWAL(    MP), STAT=LERR(35))
      ALLOCATE( JFWALL(    MP), STAT=LERR(36))
      ALLOCATE( JFSYMT(    MP), STAT=LERR(37))
      ALLOCATE( JFBODY(    MP), STAT=LERR(38))
      ALLOCATE( JFFREE(    MP), STAT=LERR(39))
      CALL CHKALC(39,LERR,IUT6,IERR) 
      IF(IERR.NE.0) GOTO 9400
      WRITE(IUT6,*) 'UNIFYP: ALLOCATING FINISH       '
C
C
C
C CLEAR FILLED FLAGS
C
C
C
      DO 100 IP = 1 , MP
          JFGRID(IP) = 0
          JFVELO(IP) = 0
          JFINLT(IP) = 0
          JFMWAL(IP) = 0
          JFWALL(IP) = 0
          JFSYMT(IP) = 0
          JFBODY(IP) = 0
          JFFREE(IP) = 0
  100 CONTINUE
C
      DO 110 IE = 1 , ME
          JFNODE(IE) = 0
          JFPRES(IE) = 0
  110 CONTINUE
C
C
C
C START COMBINING SUB-DOMAIN DATA FILES
C
C
C
      IACT = 3
      CALL GFALL(IUT0,IUT6,IUTDD,FILEDD,
     *           MCOM,NCOMFL,COMFLE,
     *           MCOM,NCOMST,COMSET,
     *           IACT,IWRITE,INAME,IRESV,  
     *           ICAST,IDATA0,IALL,ISKIP,IERR,
     *           ' !',ICHECK)
      IF(IERR.NE.0) STOP
C
      NPART = 0
C
      WRITE(IUT6,*)
      WRITE(IUT6,*) ' ** NOW START COMBINING SUB-DOMAIN DATA **'
C
  115 CONTINUE
C
C
C     LOOK FOR THE NEXT DOMAIN DECOMPOSITION DESCRIPTION DATA SET
C
C
          IACT = 5
          CALL GFALL(IUT0,IUT6,IUTDD,FILEDD,
     *               MCOM,NCOMFL,COMFLE,
     *               MCOM,NCOMST,COMSET,
     *               IACT,IWRITE,INAME,IRESV,  
     *               ICAST,IDATA0,IALL,ISKIP,IERR,
     *               '*NUM_DOM *PT_NODE *PT_ELEM *BC_INTR !',
     *               NAME,IPART,
     *               NAME,MPPT,NPPT,LISTIP,
     *               NAME,MEPT,NEPT,LISTIE,
     *               NAME,MB  ,NPINT,LPINT1,LPINT2,LPINT3,
     *               ICHECK)
          IF(IERR.NE.0) STOP
          IF(IACT.EQ.7) GO TO 1000
C
          NPART = NPART+1
C
          WRITE(IUT6,*)
          WRITE(IUT6,*) ' ** PROCESSING SUB-DOMAIN: IPART =', NPART
C
          DO 120 IPPT = 1 , NPPT
              IF(LISTIP(IPPT).GT.MP) THEN
                  WRITE(IUT0,*) ' ## LIMIT OVER :MP; STOP'
                  STOP
              ENDIF
  120     CONTINUE
C
          DO 130 IEPT = 1 , NEPT
              IF(LISTIE(IEPT).GT.ME) THEN
                  WRITE(IUT0,*) ' ## LIMIT OVER :ME; STOP'
                  STOP
              ENDIF
  130     CONTINUE
C
C
C
C     COMBINE SUB-DOMAIN MESH DATA
C
C
C
          IF(JMESH.EQ.1) THEN
              CALL MFNAME(FILEMS,FILEPT,IPART,IUT0,IERR)
              IF(IERR.NE.0) STOP
C
              IACT = 1
              CALL GFALL(IUT0,IUT6,IUTMS,FILEPT,
     *                   MCOM,NCOMFL,COMFLE,
     *                   MCOM,NCOMST,COMSET,
     *                   IACT,IWRITE,INAME,IRESV,  
     *                   ICAST,IDATA0,IALL,ISKIP,IERR,
     *                   '*GRID_3D *NODE_3D !',
     *                   NAME,MPPT,NPCHK,XPT,YPT,ZPT,
     *                   NAME,MEPT,N,NECHK,NDUM,NODEPT,
     *                   ICHECK)
              IF(IERR.NE.0) STOP
              IF(NPCHK.NE.NPPT .OR. NECHK.NE.NEPT) THEN
                  WRITE(IUT0,*) ' ## MESH DO NOT MATCH DDD DATA; STOP'
                  STOP
              ENDIF
C
              DO 200 IPPT = 1 , NPPT
                  IP = LISTIP(IPPT)
                  IF(IP.EQ.0) GOTO 200
                  X(IP)      = XPT(IPPT)
                  Y(IP)      = YPT(IPPT)
                  Z(IP)      = ZPT(IPPT)
                  JFGRID(IP) = NPART
  200         CONTINUE
C
              DO 220 IEPT = 1 , NEPT
                  IE = LISTIE(IEPT)
                  IF(IE.EQ.0) GOTO 220
                  DO 210 I = 1 , N
                      IP=NODEPT(I,IEPT)
                      IF(IP.EQ.0) THEN
                          NODE(I,IE) = 0
                      ELSE
                          NODE(I,IE) = LISTIP(NODEPT(I,IEPT))
                      ENDIF
  210             CONTINUE
                  JFNODE(IE) = NPART
  220         CONTINUE
          ENDIF
C
C
C
C     READ SUB-DOMAIN BOUNDARY CONDITIONS DATA AND SET B.C. FLAGS
C
C
C
          NPWALL = 0
          NPMWAL = 0
          NPINLT = 0
          NPFREE = 0
          NPSYMT = 0
          NPBODY = 0
          NPINT  = 0
          IF(JBOUN.EQ.1) THEN
              CALL MFNAME(FILEBC,FILEPT,IPART,IUT0,IERR)
              IF(IERR.NE.0) STOP
C
              IACT = 1
              CALL GFALL(IUT0,IUT6,IUTBC,FILEPT,
     *                   MCOM,NCOMFL,COMFLE,
     *                   MCOM,NCOMST,COMSET,
     *                   IACT,IWRITE,INAME,IRESV,  
     *                   ICAST,IDATA0,IALL,ISKIP,IERR,
     *                   '*BC_INLT *BC_MWAL *BC_WALL *BC_SYMT 
     *                    *BC_FREE *BC_BODY *BC_INTR  !',
     *                    NAME,MB,NPINLT,LPINLT,
     *                    NAME,MB,NPMWAL,LPMWAL,
     *                    NAME,MB,NPWALL,LPWALL,
     *                    NAME,MB,NPSYMT,LPSYMT,
     *                    NAME,MB,NPFREE,LPFREE,
     *                    NAME,MB,NPBODY,LPBODY,
     *                    NAME,MB,NPINT ,LPINT1,LPINT2,LPINT3,
     *                   ICHECK)
              IF(IERR.NE.0) STOP
C
              DO 300 IPINLT = 1 , NPINLT
                  IPPT       = LPINLT(IPINLT)
                  IF(IPPT.LT.1 .OR. IPPT.GT.NPPT) THEN
                      WRITE(IUT0,*) ' ## OUT-OF-BOUND NODE REFERED;STOP'
                      STOP
                  ENDIF
                  IP         = LISTIP(IPPT)
                  IF(IP.EQ.0) GOTO 300
                  JFINLT(IP) = NPART
  300         CONTINUE
C
              DO 310 IPMWAL = 1 , NPMWAL
                  IPPT       = LPMWAL(IPMWAL)
                  IF(IPPT.LT.1 .OR. IPPT.GT.NPPT) THEN
                      WRITE(IUT0,*) ' ## OUT-OF-BOUND NODE REFERED;STOP'
                      STOP
                  ENDIF
                  IP         = LISTIP(IPPT)
                  IF(IP.EQ.0) GOTO 310
                  JFMWAL(IP) = NPART
  310         CONTINUE
C
              DO 320 IPWALL = 1 , NPWALL
                  IPPT       = LPWALL(IPWALL)
                  IF(IPPT.LT.1 .OR. IPPT.GT.NPPT) THEN
                      WRITE(IUT0,*) ' ## OUT-OF-BOUND NODE REFERED;STOP'
                      STOP
                  ENDIF
                  IP         = LISTIP(IPPT)
                  IF(IP.EQ.0) GOTO 320
                  JFWALL(IP) = NPART
  320         CONTINUE
C
              DO 330 IPSYMT = 1 , NPSYMT
                  IPPT       = LPSYMT(IPSYMT)
                  IF(IPPT.LT.1 .OR. IPPT.GT.NPPT) THEN
                      WRITE(IUT0,*) ' ## OUT-OF-BOUND NODE REFERED;STOP'
                      STOP
                  ENDIF
                  IP         = LISTIP(IPPT)
                  IF(IP.EQ.0) GOTO 330
                  JFSYMT(IP) = NPART
  330         CONTINUE
C
              DO 340 IPBODY = 1 , NPBODY
                  IPPT       = LPBODY(IPBODY)
                  IF(IPPT.LT.1 .OR. IPPT.GT.NPPT) THEN
                      WRITE(IUT0,*) ' ## OUT-OF-BOUND NODE REFERED;STOP'
                      STOP
                  ENDIF
                  IP         = LISTIP(IPPT)
                  IF(IP.EQ.0) GOTO 340
                  JFBODY(IP) = NPART
  340         CONTINUE
C
              DO 350 IPFREE = 1 , NPFREE
                  IPPT       = LPFREE(IPFREE)
                  IF(IPPT.LT.1 .OR. IPPT.GT.NPPT) THEN
                      WRITE(IUT0,*) ' ## OUT-OF-BOUND NODE REFERED;STOP'
                      STOP
                  ENDIF
                  IP         = LISTIP(IPPT)
                  IF(IP.EQ.0) GOTO 350
                  JFFREE(IP) = NPART
  350         CONTINUE
C
          ENDIF
C
C
C
C     COMBINE SUB-DOMAIN FLOW DATA
C
C
C
          IF(JFLOW.EQ.1) THEN
              CALL MFNAME(FILEFF,FILEPT,IPART,IUT0,IERR)
              IF(IERR.NE.0) STOP
C
              IACT = 1
              CALL GFALL(IUT0,IUT6,IUTFF,FILEPT,
     *                   MCOM,NCOMFL,COMFLE,
     *                   MCOM,NCOMST,COMSET,
     *                   IACT,IWRITE,INAME,IRESV,  
     *                   ICAST,IDATA0,IALL,ISKIP,IERR,
     *                   '*TIME_PS *STEP_PS 
     *                    *VELO_3D *PRES_3E *PRES_3D 
     *                    *VFRC_3E *LIQD_3D !',
     *                   NAME,TIME,
     *                   NAME,ISTEP,
     *                   NAME,MPPT,NPCHK,UPT,VPT,WPT,
     *                   NAME,MEPT,NEDUM,PPT,
     *                   NAME,MPPT,NPDUM,PNPT,
     *                   NAME,MEPT,NEDUM,FEPT,
     *                   NAME,MPPT,NPDUM,FPT ,
     *                   ICHECK)
              IF(IERR.NE.0) STOP
C
              IF(NEDUM.EQ.NEPT) THEN
                IPRS = 1
                WRITE(IUT6,*) COMM(IPRS)
              ELSE IF (NPDUM.EQ.NPPT) THEN
                IPRS = 2
                WRITE(IUT6,*) COMM(IPRS)
              ELSE
                GOTO 9000
              ENDIF
C
              DO 400 IPPT = 1 , NPPT
                  IP = LISTIP(IPPT)
                  IF(IP.EQ.0) GOTO 400
                  U  (IP)    = UPT  (IPPT)
                  V  (IP)    = VPT  (IPPT)
                  W  (IP)    = WPT  (IPPT)
                  PN (IP)    = PNPT (IPPT)
                  F  (IP)    = FPT  (IPPT)
                  JFVELO(IP) = NPART
  400         CONTINUE
C
                 DO 410 IEPT = 1 , NEPT
                  IE = LISTIE(IEPT)
                  IF(IE.EQ.0) GOTO 410
                  P(IE) = PPT(IEPT)
                  FE(IE)=FEPT(IEPT)
                  JFPRES(IE) = NPART
  410         CONTINUE
          ENDIF
C
          GO TO 115
 1000 CONTINUE
C
      IACT = 7
      CALL GFALL(IUT0,IUT6,IUTDD,FILEDD,
     *           MCOM,NCOMFL,COMFLE,
     *           MCOM,NCOMST,COMSET,
     *           IACT,IWRITE,INAME,IRESV,  
     *           ICAST,IDATA0,IALL,ISKIP,IERR,
     *           ' !',ICHECK)
      IF(IERR.NE.0) STOP
C
C
C
C END COMBINING SUB-DOMAIN DATA FILES
C
C
C
      WRITE(IUT6,*)
      WRITE(IUT6,*) ' !! SUB-DOMAIN DATA COMBINATION COMPLETE !!'
C
C
C
C CHECK CONSISTENCY OF THE GLOBAL DATA (WITH MAKING GLOBAL BC DATA)
C
C
C (1) MESH DATA
C
C
      IF(JMESH.EQ.1) THEN
          WRITE(IUT6,*)
          WRITE(IUT6,*) ' ** CHEKING CONSISTENCY OF THE MESH DATA **'
C
          NP = 0
          DO 1100 IP = 1 , MP
              IF(JFGRID(IP).NE.0) NP = NP+1
 1100     CONTINUE
C
          NE = 0
          DO 1110 IE = 1 , ME
              IF(JFNODE(IE).NE.0) NE = NE+1
 1110     CONTINUE
C
          NPMESH = NP
          NEMESH = NE
C
          DO 1120 IP = 1 , NP
              IF(JFGRID(IP).EQ.0) THEN
                  WRITE(IUT0,*) ' ## MISSING SOME GRIDS DATA; STOP'
                  STOP
              ENDIF
 1120     CONTINUE
C
          DO 1130 IE = 1 , NE
              IF(JFNODE(IE).EQ.0) THEN
                  WRITE(IUT0,*) ' ## MISSING SOME NODES DATA; STOP'
                  STOP
              ENDIF
 1130     CONTINUE
C
          DO 1150 IE = 1 , NE
              DO 1140 I = 1 , N
                  IF(NODE(I,IE).EQ.0) GOTO 1140
                  IF(NODE(I,IE).LT.1 .OR. NODE(I,IE).GT.NP) THEN
                      WRITE(IUT0,*) ' ## OUT-OF-BOUND NODE REFERED;STOP'
                      STOP
                  ENDIF
 1140         CONTINUE
 1150     CONTINUE
C
          DO 1160 IP = 1 , NP
              JFGRID(IP) = 0
 1160     CONTINUE
C
          DO 1180 IE = 1 , NE
              DO 1170 I = 1 , N
                  IP=NODE(I,IE)
                  IF(IP.EQ.0) GOTO 1170
                  JFGRID(IP) = 1
 1170         CONTINUE
 1180     CONTINUE
C
          DO 1190 IP = 1 , NP
              IF(JFGRID(IP).EQ.0) THEN
                  WRITE(IUT0,*) ' ## WARNING NON-REFERED NODE EXIST'
              ENDIF
 1190     CONTINUE
          WRITE(IUT6,*) ' DONE!'
      ENDIF
C
C
C (2) BOUNDARY CONDITIONS DATA
C
C
      IF(JBOUN.EQ.1) THEN
C
          WRITE(IUT6,*)
          WRITE(IUT6,*) ' ** CHEKING & MAKING GLOBAL B.C. DATA **'
C
          NPINLT = 0
          NPMWAL = 0
          NPWALL = 0
          NPSYMT = 0
          NPBODY = 0
          NPFREE = 0
C
          DO 1200 IP = 1 , MP
              IF(JFINLT(IP).GT.0) THEN
                  NPINLT = NPINLT+1
                  IF(NPINLT.GT.MB) THEN
                      WRITE(IUT0,*) ' ## LIMIT OVER :MB; STOP'
                      STOP
                  ENDIF
                  LPINLT(NPINLT) = IP
              ENDIF
C
              IF(JFMWAL(IP).GT.0) THEN
                  NPMWAL = NPMWAL+1
                  IF(NPMWAL.GT.MB) THEN
                      WRITE(IUT0,*) ' ## LIMIT OVER :MB; STOP'
                      STOP
                  ENDIF
                  LPMWAL(NPMWAL) = IP
              ENDIF
C
              IF(JFWALL(IP).GT.0) THEN
                  NPWALL = NPWALL+1
                  IF(NPWALL.GT.MB) THEN
                      WRITE(IUT0,*) ' ## LIMIT OVER :MB; STOP'
                      STOP
                  ENDIF
                  LPWALL(NPWALL) = IP
              ENDIF
C
              IF(JFSYMT(IP).GT.0) THEN
                  NPSYMT = NPSYMT+1
                  IF(NPSYMT.GT.MB) THEN
                      WRITE(IUT0,*) ' ## LIMIT OVER :MB; STOP'
                      STOP
                  ENDIF
                  LPSYMT(NPSYMT) = IP
              ENDIF
C
              IF(JFBODY(IP).GT.0) THEN
                  NPBODY = NPBODY+1
                  IF(NPBODY.GT.MB) THEN
                      WRITE(IUT0,*) ' ## LIMIT OVER :MB; STOP'
                      STOP
                  ENDIF
                  LPBODY(NPBODY) = IP
              ENDIF
C
              IF(JFFREE(IP).GT.0) THEN
                  NPFREE = NPFREE+1
C
                  IF(NPFREE.GT.MB) THEN
                      WRITE(IUT0,*) ' ## LIMIT OVER :MB; STOP'
                      STOP
                  ENDIF
                  LPFREE(NPFREE) = IP
              ENDIF
C
 1200     CONTINUE
          WRITE(IUT6,*) ' DONE!'
      ENDIF
C
C
C (3) FLOW DATA
C
C
      IF(JFLOW.EQ.1) THEN
          WRITE(IUT6,*)
          WRITE(IUT6,*) ' ** CHEKING CONSISTENCY OF THE FLOW DATA **'
C
          NP = 0
          DO 1300 IP = 1 , MP
              IF(JFVELO(IP).NE.0) NP = NP+1
 1300     CONTINUE
C
        IF(IPRS.EQ.1) THEN
          NE = 0
          DO 1310 IE = 1 , ME
              IF(JFPRES(IE).NE.0) NE = NE+1
 1310     CONTINUE
        ENDIF
C
          DO 1320 IP = 1 , NP
              IF(JFVELO(IP).EQ.0) THEN
                  WRITE(IUT0,*) ' ## MISSING SOME VELOCITY DATA; STOP'
                  STOP
              ENDIF
 1320     CONTINUE
C
        IF(IPRS.EQ.1) THEN
          DO 1330 IE = 1 , NE
              IF(JFPRES(IE).EQ.0) THEN
                  WRITE(IUT0,*) ' ## MISSING SOME PRESSURE DATA; STOP'
                  STOP
              ENDIF
 1330     CONTINUE
        ENDIF
C
          WRITE(IUT6,*) ' DONE!'
      ENDIF
C
      NP = NPMESH
      NE = NEMESH
C
      IF(IPRS.EQ.1) THEN
        NEDUM=NE
        NPDUM=0
      ELSE
        NEDUM=0
        NPDUM=NP
      ENDIF
C
C
C     WRITE GLOBAL DOMAIN MESH DATA
C
C
C
      IF(JMESH.EQ.1) THEN
          IACT = 2
          CALL GFALL(IUT0,IUT6,IUTMS,FILEMW,
     *               MCOM,NCOMFL,COMFLE,
     *               MCOM,NCOMST,COMSET,
     *               IACT,IWRITE,INAME,IRESV,  
     *               ICAST,IDATA0,IALL,ISKIP,IERR,
     *               '*GRID_3D *NODE_3D !',
     *               NAME,MP,NP,X,Y,Z,
     *               NAME,ME,N, NE,NDUM,NODE,
     *               ICHECK)
          IF(IERR.NE.0) STOP
      ENDIF
C
C
C
C WRITE GLOBAL DOMAIN BOUNDARY CONDITIONS DATA
C
C
C
      IF(JBOUN.EQ.1) THEN
          IACT  = 2
          NPINT = 0
          CALL GFALL(IUT0,IUT6,IUTBC,FILEBW,
     *               MCOM,NCOMFL,COMFLE,
     *               MCOM,NCOMST,COMSET,
     *               IACT,IWRITE,INAME,IRESV,  
     *               ICAST,IDATA0,IALL,ISKIP,IERR,
     *               '*BC_INLT *BC_MWAL *BC_WALL *BC_SYMT 
     *                *BC_FREE *BC_BODY !',
     *                NAME,MB,NPINLT,LPINLT,
     *                NAME,MB,NPMWAL,LPMWAL,
     *                NAME,MB,NPWALL,LPWALL,
     *                NAME,MB,NPSYMT,LPSYMT,
     *                NAME,MB,NPFREE,LPFREE,
     *                NAME,MB,NPBODY,LPBODY,
     *               ICHECK)
          IF(IERR.NE.0) STOP
      ENDIF
C
C
C
C WRITE GLOBAL DOMAIN FLOW DATA
C
C
C
      IF(JFLOW.EQ.1) THEN
          IACT = 2
          CALL GFALL(IUT0,IUT6,IUTFF,FILEFW,
     *               MCOM,NCOMFL,COMFLE,
     *               MCOM,NCOMST,COMSET,
     *               IACT,IWRITE,INAME,IRESV,  
     *               ICAST,IDATA0,IALL,ISKIP,IERR,
     *               '*TIME_PS *STEP_PS 
     *                *VELO_3D *PRES_3E *PRES_3D 
     *                *VFRC_3E *LIQD_3D !',
     *               NAME,TIME,
     *               NAME,ISTEP,
     *               NAME,MPPT,NP,U,V,W,
     *               NAME,MEPT,NEDUM,P,
     *               NAME,MPPT,NPDUM,PN,
     *               NAME,MEPT,NEDUM,FE,
     *               NAME,MPPT,NPDUM,F ,
     *               ICHECK)
          IF(IERR.NE.0) STOP
      ENDIF
C
      WRITE(IUT6,*)
      WRITE(IUT6,*) ' UNIFY: SUCCESSFULLY TERMINATED'
      WRITE(IUT6,*) '        NUMBER OF SUB-DOMAINS COMBINDED =', NPART
C     
      STOP
C
 9000 CONTINUE
      WRITE(IUT0,*) ' ## FLOW DO NOT MATCH DDD DATA; STOP'
      GOTO 9999
C
 9400 CONTINUE
      WRITE(IUT6,*) 'ALLOCATION ERRORS     '
      GOTO 9999
C
 9500 CONTINUE
      WRITE(IUT6,*) ' ERROR: WHEN CHECKING DDD FILE'
      GOTO 9999
C
 9999 CONTINUE
      WRITE(IUT6,*) ' UNIFY: TERMINATED'
      STOP
      END

