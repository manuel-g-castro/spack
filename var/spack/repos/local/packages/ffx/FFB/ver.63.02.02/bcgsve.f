C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.8.1                                   C
C                                                                      C
C  SUB ROUTINE : BCGSVE                                                C
C                                                                      C
C                                       WRITTEN BY H.YOSHIMURA         C
C                                                                      C
C                                                                      C
C CONTACT ADDRESS : IIS, THE UNIVERSITY OF TOKYO, CISS                 C
C                                                                      C
C THERMO-FLUID ANALYSIS SOLVERS FOR LARGE-SCALE-ASSEMBLY               C
C                                                                      C
C======================================================================C
      SUBROUTINE BCGSVE(NE,NFACE,NFACE1,NFACE2,NFACE3,LEFACE,LFACE,
     *                  A,AD,B,EPS,EPSRE,NMAX,NITR,S,RES,
     *                  IPART,MBFDOM,NDOMF,LDOMF,NBFDOM,IFSLF,IFSND,
     *                  SWRK,BUFSND,BUFRCV,R0,RK,PK,APK,ATK,TK,
     *                  IUT6,IUT0,IERR)
C
      IMPLICIT NONE
C
C     [INPUT]
      INTEGER*4 NE,NFACE,NFACE1,NFACE2,NFACE3
      INTEGER*4 LFACE(5,NFACE),LEFACE(6,NE),NMAX
      REAL*4    A(6,NE),AD(NE),B(NE)
      REAL*4    EPS,EPSRE
C
C     [IN-OUTPUT]
      INTEGER*4 NITR,IUT6,IUT0,IERR
      REAL*4    S(NE),RES
C
C     [WORK]
      INTEGER*4 IPART,MBFDOM,NDOMF,LDOMF(NDOMF),NBFDOM(NDOMF)
      INTEGER*4 IFSLF(MBFDOM,NDOMF),IFSND(MBFDOM,NDOMF)
      REAL*4    SWRK(NFACE3),BUFSND,BUFRCV,
     *          R0(NE),RK(NE),PK(NE),APK(NE),ATK(NE),TK(NE)
C
C     [LOCL]
      INTEGER*4 IE,IFACE,IFACE3,MAXBUF,IDIM,IERRA
      REAL*4    RKDOT,RKDOTA,BDOT,BDOTA,RESR,APDOT,APDOTA,
     *          ATTDOT,ATTDTA,AT2DOT,AT2DTA,
     *          RKDOTP,RSDOT,RSDOTA,
     *          ALFA,BETA,QK
      REAL*4    EPS0 
      DATA EPS0 / 1.E-30 /
C
      CHARACTER*60 ERMSGC
     & /' ## SUBROUTINE BCGSVE: FATAL      ERROR REPORT   ; RETURNED' /
C
C
      MAXBUF=5*NFACE
      IF(NMAX.EQ.0) RETURN
C
      NITR=0
C
C
CCC   1. SET INITIAL RESIDUAL VECTOR AND SEARCH-DIRECTION VECTOR
C
C   
C          OPERATION COUNTS:   FLOP /ELEMENT
C          DATA LOADINGS   :   WORDS/ELEMENT
C                           (  WORDS CONTIGUOUSLY,
C                              WORDS BY 4-WORD STRIDE, AND
C                              WORDS BY LIST )
      CALL CALAXV(NE,NFACE3,LEFACE,A,AD,S,RK,SWRK,IUT6,IUT0,IERR)
C
      IF(IERR.EQ.1) THEN
          WRITE(IUT0,*) ERMSGC
          RETURN
      ENDIF
C 
      RKDOT = 0.E0
      BDOT = 0.E0
      DO 300 IE = 1 , NE
          RK (IE) = B (IE)-RK (IE)
          R0 (IE) = RK(IE)
          PK (IE) = RK(IE)
          TK (IE) = 0.E0
          RKDOT = RKDOT+R0(IE)*RK(IE)
          BDOT =  BDOT +B (IE)*B (IE)
  300 CONTINUE
C
      IF(IPART.GE.1) THEN
          CALL DDCOM2(RKDOT,RKDOTA)
          CALL DDCOM2(BDOT,BDOTA)
          RKDOT = RKDOTA
          BDOT = BDOTA
      ENDIF
C
      IF(ABS(BDOT).LE.EPS0) BDOT = 1.0E0
C
      RES  = SQRT(RKDOT)
      RESR = RES/SQRT(BDOT)
C
      IF(RES.LE.EPS.OR.RESR.LE.EPSRE) RETURN
C
C
CCC   2. COMPUTE PRODUCT OF COEFFICIENT MATRIX AND SEARCH-DIRECTION VECTOR
CCC      AND INNER PRODUCT OF COMPUTED PRODUCT AND SEARCH-DIRECTION VECTOR
C
C 
 10   CONTINUE
C
      NITR=NITR+1
C
CC
CC    [] COMMUNICATE PK
CC
      IF (IPART.NE.0) THEN
      DO 310 IFACE3=1,NFACE3
         IFACE=NFACE1+NFACE2+IFACE3
         IE=LFACE(1,IFACE)
         SWRK(IFACE3)=PK(IE)
 310  CONTINUE
C
      IDIM=1
      CALL DDFAC1(IDIM,MBFDOM,NDOMF,LDOMF,NBFDOM,IFSLF,IFSND,NFACE3,
     *            SWRK,SWRK,SWRK,BUFSND,BUFRCV,MAXBUF,IUT0,IERR)
      CALL ERCHK2(IUT6,IPART,1,IERR,IERRA)
      IF(IERRA.NE.0) GOTO 9999
      ENDIF
C
CCC   2.1 COMPUTE APK,ALFA
C
C
C          OPERATION COUNTS:   FLOP /ELEMENT
C          DATA LOADINGS   :   WORDS/ELEMENT
C                           (  WORDS CONTIGUOUSLY,
C                              WORDS BY 4-WORD STRIDE, AND
C                              WORDS BY LIST )
      CALL CALAXV(NE,NFACE3,LEFACE,A,AD,PK,APK,SWRK,IUT6,IUT0,IERR)
C
      IF(IERR.EQ.1) THEN
          WRITE(IUT0,*) ERMSGC
          RETURN
      ENDIF
C
      APDOT = 0.E0
      DO 400 IE = 1 , NE
          APDOT = APDOT+R0(IE)*APK(IE)
  400 CONTINUE
C
      IF(IPART.GE.1) THEN
          CALL DDCOM2(APDOT,APDOTA)
          APDOT = APDOTA
      ENDIF
C
      IF(APDOT .EQ. 0.0E0) RETURN
      ALFA = RKDOT/APDOT
C
C
CCC   2.2 COMPUTE TK=RK-ALFA*APK     
C
C
      DO 500 IE = 1 , NE
          TK (IE) = RK(IE)-ALFA*APK(IE) 
  500 CONTINUE 
C
CC
CC    [] COMMUNICATE TK
CC
      IF (IPART.NE.0) THEN
      DO 510 IFACE3=1,NFACE3
         IFACE=NFACE1+NFACE2+IFACE3
         IE=LFACE(1,IFACE)
         SWRK(IFACE3)=TK(IE)
 510  CONTINUE
C
      IDIM=1
      CALL DDFAC1(IDIM,MBFDOM,NDOMF,LDOMF,NBFDOM,IFSLF,IFSND,NFACE3,
     *            SWRK,SWRK,SWRK,BUFSND,BUFRCV,MAXBUF,IUT0,IERR)
      CALL ERCHK2(IUT6,IPART,1,IERR,IERRA)
      IF(IERRA.NE.0) GOTO 9999
      ENDIF
C
CCC   2.3 COMPUTE ATK
C
C
C          OPERATION COUNTS:   36 FLOP /ELEMENT
C          DATA LOADINGS   :   48 WORDS/ELEMENT
C                           (  32 WORDS CONTIGUOUSLY,
C                               4 WORDS BY 4-WORD STRIDE, AND
C                              12 WORDS BY LIST )
      CALL CALAXV(NE,NFACE3,LEFACE,A,AD,TK,ATK,SWRK,IUT6,IUT0,IERR)
C
      IF(IERR.EQ.1) THEN
          WRITE(IUT0,*) ERMSGC
          RETURN
      ENDIF
C
C
CCC   2.4 COMPUTE QK
C
C
      ATTDOT  = 0.E0
      AT2DOT  = 0.E0
      DO 600 IE = 1 , NE
          ATTDOT = ATTDOT+ATK(IE)* TK(IE)
          AT2DOT = AT2DOT+ATK(IE)*ATK(IE)
  600 CONTINUE
C
      IF(IPART.GE.1) THEN
          CALL DDCOM2(ATTDOT,ATTDTA)
          CALL DDCOM2(AT2DOT,AT2DTA)
          ATTDOT = ATTDTA
          AT2DOT = AT2DTA
      ENDIF
C
      IF(AT2DOT .EQ. 0.E0) RETURN
      QK = ATTDOT/AT2DOT
C
C
CCC   2.5 UPDATE SOLUTION VECTOR AND RESIDUAL VECTOR
CCC   2.6 RETURN IF L2-NORM OF UPDATED SOLUTION VECTOR IS LESS THAN CRITERIA
C
C
      RKDOTP = RKDOT
      RKDOT  = 0.E0
      RSDOT  = 0.E0
      DO 700 IE = 1 , NE
          S  (IE) = S (IE)+ ALFA*PK(IE) + QK*TK (IE)   
          RK (IE) = TK(IE)              - QK*ATK(IE) 
          RKDOT = RKDOT+R0(IE)*RK(IE)
          RSDOT = RSDOT+RK(IE)*RK(IE)
  700 CONTINUE
C
C
      IF(IPART.GE.1) THEN
          CALL DDCOM2(RKDOT,RKDOTA)
          CALL DDCOM2(RSDOT,RSDOTA)
          RKDOT = RKDOTA
          RSDOT = RSDOTA
      ENDIF
C
      RES  = SQRT(RSDOT)
      RESR = RES/SQRT(BDOT)
C
      IF(RES.LE.EPS.OR.RESR.LE.EPSRE) RETURN
C
C
CCC   2.7 UPDATE SEARCH-DIRECTION VECTOR
C     
C
      IF(QK     .EQ. 0.E0) RETURN
      IF(RKDOTP .EQ. 0.E0) RETURN
      BETA = (ALFA/QK)*(RKDOT/RKDOTP)
C 
      DO 800 IE = 1 , NE
          PK (IE) = RK(IE)+BETA*(PK(IE)-QK*APK(IE))
  800 CONTINUE
C
C
CCC   2.7 RETURN IF ITERATION NUMBER HAS REACHED THE GIVEN MAXIMUM NUMBER,
CCC       OTHERWISE CONTINUE ITERATIONS UNTIL SOLUTION IS CONVERGED
C
C
      IF(NITR.EQ.NMAX) THEN          
          RETURN
      END IF  
C
      GO TO 10
C
 9999 CONTINUE
      WRITE(IUT0,*) ERMSGC
      IERR=1
C
      RETURN
      END
