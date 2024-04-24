C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.8.1                                   C
C                                                                      C
C  SUB ROUTINE : MKFAC3                                                C
C                                                                      C
C                                       WRITTEN BY H.YOSHIMURA         C
C                                                                      C
C                                                                      C
C CONTACT ADDRESS : IIS, THE UNIVERSITY OF TOKYO, CISS                 C
C                                                                      C
C THERMO-FLUID ANALYSIS SOLVERS FOR LARGE-SCALE-ASSEMBLY               C
C                                                                      C
C======================================================================C
      SUBROUTINE MKFAC3(IPART,NE,NP,N2,NSP,NS,MEP,MFACE,MBFDOM,
     *                  NODE,LOCAL,NEP,IENP,X,Y,Z,
     *                  MDOM,NDOM,MBPDOM,LDOM,NBPDOM,IPSLF,IPSND,
     *                  NFACE,NFACE1,NFACE2,LFACE,AVEC,DVEC,
     *                  LWRK01,LEFACE,IUT6,IUT0,IERR)
C
      IMPLICIT NONE
C
C     [INPUT]
      INTEGER*4 IPART,NE,NP,N2,NSP,NS,MEP,MFACE,MBFDOM
      INTEGER*4 NODE(N2,NE),LOCAL(NSP,NS,4),NEP(NP),IENP(MEP,NP)
      REAL*8    X(NP),Y(NP),Z(NP)
      INTEGER*4 MDOM,NDOM,MBPDOM
      INTEGER*4 LDOM(MDOM),NBPDOM(MDOM),
     *          IPSLF(MBPDOM,MDOM),IPSND(MBPDOM,MDOM)
      INTEGER*4 NFACE1,NFACE2
C
C     [IN-OUTPUT]
      INTEGER*4 NFACE
      INTEGER*4 LFACE(5,MFACE)
      REAL*4    AVEC(4,MFACE),DVEC(3,MFACE)
      INTEGER*4 LEFACE(6,NE)
      INTEGER*4 IUT6,IUT0,IERR
C
C     [WORK]
      INTEGER*4 LWRK01(NP)
C
C     [LOCAL]
      INTEGER*4 MBFDM6
      INTEGER*4,ALLOCATABLE::
     *          NFSND(:),NFRCV(:),NFSND2(:),NFRCV2(:),
     *          LFSND(:,:),LFRCV(:,:)
      INTEGER*4 IP,IP1,IP2,IP3,IP4,IMATCH,JBC,IFACE
      INTEGER*4 IE,NSD,IETYPE,NLS,IE1,IE2,IS,IS1,I,I1,I2
      INTEGER*4 IBP,IBP1,IBP2,IBP3,IBP4,IDOM
      INTEGER*4 IFS1,IFS2,IFS3,IFS4,IFS5,IFS6
      INTEGER*4 IFR1,IFR2,IFR3,IFR4,IFR5,IFR6
      INTEGER*4 NFACE3,IFACES,IFACEN,LISTS(4),LISTR(4),NUM1
      INTEGER*4 IERRA,LERR(6)
      DATA NUM1 /1/
C
      CHARACTER*60 ERMSGC
     * / ' ## SUBROUTINE MKFAC3: ERROR OCCURED            ; RETURNED' /
      CHARACTER*60 EREXP1
     * / ' ALLOCATING FAILED                                        ' /
      CHARACTER*60 EREXP2
     * / ' THE NUMBER OF INTER-CONNECT BOUNDARY FACE IS OVER MBFDOM ' /
      CHARACTER*60 EREXP3
     * / ' THE NUMBER OF FACE IS OVER MFACEM                        ' /
C
CC
CCHY [2] ALLOCATE WORK ARRAY
CC
      MBFDM6=6*MBFDOM
      ALLOCATE(NFSND (       NDOM),STAT=LERR(01))
      ALLOCATE(NFSND2(       NDOM),STAT=LERR(02))
      ALLOCATE(LFSND (MBFDM6,NDOM),STAT=LERR(03))
      ALLOCATE(NFRCV (       NDOM),STAT=LERR(04))
      ALLOCATE(NFRCV2(       NDOM),STAT=LERR(05))
      ALLOCATE(LFRCV (MBFDM6,NDOM),STAT=LERR(06))
      CALL ERRCHK(IUT6,IPART,6,LERR,IERRA)
      IF(IERRA.NE.0) THEN
         WRITE(IUT0,*) EREXP1
         GOTO 9999
      ENDIF
CC
CCHY [3] MAKE REQUEST INTER-CONNECT BOUNDARY FACE LIST
CC
      DO 1000 IDOM=1,NDOM
C
         DO 1100 IP=1,NP
            LWRK01(IP)=0
 1100    CONTINUE
C            
         DO 1200 IBP=1,NBPDOM(IDOM)
            IP=IPSLF(IBP,IDOM)
            LWRK01(IP)=IBP
 1200    CONTINUE
C
         NFSND(IDOM)=0
         DO 1300 IE=1,NE
            IF(     NODE(8,IE).GE.1) THEN ! HEX
               IETYPE=4
               NLS=6
            ELSE IF(NODE(6,IE).GE.1) THEN ! PRS
               IETYPE=3
               NLS=5
            ELSE IF(NODE(5,IE).GE.1) THEN ! PYR
               IETYPE=2
               NLS=5
            ELSE                          ! TET
               IETYPE=1
               NLS=4
            ENDIF
            DO 1400 IS=1,NLS
               IF (LEFACE(IS,IE).NE.0) GOTO 1400
C
               IP1=NODE(LOCAL(1,IS,IETYPE),IE)
               IP2=NODE(LOCAL(2,IS,IETYPE),IE)
               IP3=NODE(LOCAL(3,IS,IETYPE),IE)
C
               IF ((IETYPE.EQ.1            ).OR. ! TRI
     *             (IETYPE.EQ.2.AND.IS.LE.4).OR.
     *             (IETYPE.EQ.3.AND.IS.LE.2)) THEN
                  IP4=0
                  JBC=LWRK01(IP1)*LWRK01(IP2)*LWRK01(IP3)
               ELSE                             ! QUAD
                  IP4=NODE(LOCAL(4,IS,IETYPE),IE)
                  JBC=LWRK01(IP1)*LWRK01(IP2)*LWRK01(IP3)*LWRK01(IP4)
               ENDIF
C
               IF (JBC.EQ.0) GOTO 1400
C
               NFSND(IDOM)=NFSND(IDOM)+1
               IF (NFSND(IDOM).GT.MBFDOM) THEN
                  WRITE(IUT0,*) EREXP2,MBFDOM
                  GOTO 9999
               ENDIF
C
               IFS1=NFSND(IDOM)
               IFS2=NFSND(IDOM)+MBFDOM
               IFS3=NFSND(IDOM)+MBFDOM*2
               IFS4=NFSND(IDOM)+MBFDOM*3
               IFS5=NFSND(IDOM)+MBFDOM*4
               IFS6=NFSND(IDOM)+MBFDOM*5
               LFSND(IFS1,IDOM)=LWRK01(IP1)
               LFSND(IFS2,IDOM)=LWRK01(IP2)
               LFSND(IFS3,IDOM)=LWRK01(IP3)
               IF (IP4.EQ.0) THEN
                  LFSND(IFS4,IDOM)=0
               ELSE
                  LFSND(IFS4,IDOM)=LWRK01(IP4)
               ENDIF
               LFSND(IFS5,IDOM)=IE
               LFSND(IFS6,IDOM)=IS
C
 1400       CONTINUE
 1300    CONTINUE
C
 1000 CONTINUE
C
CC
CCHY [4] COMMUNICATE LFSND
CC
      DO 2000 IDOM=1,NDOM
         LWRK01(IDOM)=1
 2000 CONTINUE
C
      CALL DDSET5(IPART,NUM1,
     *            NDOM,LDOM,LWRK01,NFSND,
     *            NDOM,LDOM,LWRK01,NFRCV,IERR)
      DO 2100 IDOM=1,NDOM
         NFSND2(IDOM)=MBFDM6
         NFRCV2(IDOM)=MBFDM6
 2100 CONTINUE
      CALL DDSET5(IPART,MBFDM6,
     *            NDOM,LDOM,NFSND2,LFSND,
     *            NDOM,LDOM,NFRCV2,LFRCV,IERR)
      CALL ERRCHK(IUT6,IPART,1,IERR,IERRA)
      IF(IERRA.NE.0) GOTO 9999
C
CC
CCHY [5] MAKE INTER-CONNECT BOUNDARY FACE LIST
CC
      NFACE3=0
      DO 3000 IDOM=1,NDOM
C
         DO 3100 I1=1,NFSND(IDOM)
            IFS1=I1
            IFS2=I1+MBFDOM
            IFS3=I1+MBFDOM*2
            IFS4=I1+MBFDOM*3
            IFS5=I1+MBFDOM*4
            IFS6=I1+MBFDOM*5
            IBP1=LFSND(IFS1,IDOM)
            IBP2=LFSND(IFS2,IDOM)
            IBP3=LFSND(IFS3,IDOM)
            IBP4=LFSND(IFS4,IDOM)
            IE1 =LFSND(IFS5,IDOM)
            IS1 =LFSND(IFS6,IDOM)
            IF (LEFACE(IS1,IE1).NE.0) GOTO 3100
C
            LISTS(1)=IPSLF(IBP1,IDOM)
            LISTS(2)=IPSLF(IBP2,IDOM)
            LISTS(3)=IPSLF(IBP3,IDOM)
            IF (IBP4.EQ.0) THEN
               LISTS(4)=0
            ELSE
               LISTS(4)=IPSLF(IBP4,IDOM)
            ENDIF
C
            DO 3200 I2=1,NFRCV(IDOM)
               IFR1=I2
               IFR2=I2+MBFDOM
               IFR3=I2+MBFDOM*2
               IFR4=I2+MBFDOM*3
               IFR5=I2+MBFDOM*4
               IFR6=I2+MBFDOM*5
               IF (LFRCV(IFR1,IDOM).LT.0) GOTO 3200
C
               IBP1 =LFRCV(IFR1,IDOM)
               IBP2 =LFRCV(IFR2,IDOM)
               IBP3 =LFRCV(IFR3,IDOM)
               IBP4 =LFRCV(IFR4,IDOM)
               LISTR(1)=IPSND(IBP1,IDOM)
               LISTR(2)=IPSND(IBP2,IDOM)
               LISTR(3)=IPSND(IBP3,IDOM)
               IF (IBP4.EQ.0) THEN
                  LISTR(4)=0
               ELSE
                  LISTR(4)=IPSND(IBP4,IDOM)
               ENDIF
C
               CALL MATCHX(LISTS,LISTR,IMATCH)
               IF (IMATCH.NE.0) THEN
                  NFACE =NFACE +1
                  NFACE3=NFACE3+1
                  IF (NFACE.GT.MFACE) THEN
                     WRITE(IUT0,*) EREXP3,MFACE
                     GOTO 9999
                  ENDIF
                  LEFACE(IS1,IE1)=IE1
                  LFACE(1,NFACE)=IE1
                  LFACE(2,NFACE)=0
                  LFACE(3,NFACE)=IS1
                  LFACE(4,NFACE)=0
                  LFACE(5,NFACE)=LDOM(IDOM)
                  CALL CALAVC(IE1,IS1,NE,NP,N2,NSP,NS,NODE,LOCAL,
     *                        X,Y,Z,AVEC(1,NFACE))
                  LFRCV(IFR1,IDOM)=-NFACE3
                  LFSND(IFS2,IDOM)= NFACE3
                  LFSND(IFS6,IDOM)=LFRCV(IFR5,IDOM)
                  GOTO 3100
               ENDIF
 3200       CONTINUE
            LFSND(IFS2,IDOM)=0
 3100    CONTINUE
C
 3000 CONTINUE
C
CC
CCHY [5] COMMUNICATE LFRCV
CC
      CALL DDSET5(IPART,MBFDM6,
     *            NDOM,LDOM,NFRCV,LFRCV,
     *            NDOM,LDOM,NFSND,LFSND,IERR)
      CALL ERRCHK(IUT6,IPART,1,IERR,IERRA)
      IF(IERRA.NE.0) GOTO 9999
C
CC
CCHY [7] SET INDEX NUMBER AT NEIBHGOR DOMAIN
CC
      DO 4000 IDOM=1,NDOM
         DO 4100 I1=1,NFSND(IDOM)
            IFS1=I1
            IFS2=I1+MBFDOM
            IFS3=I1+MBFDOM*2
            IFS4=I1+MBFDOM*3
            IFS5=I1+MBFDOM*4
            IFS6=I1+MBFDOM*5
            IF (LFSND(IFS1,IDOM).LT.0) THEN
               IFACEN=LFSND(IFS1,IDOM) ! NEIGHBOR
               IFACES=LFSND(IFS2,IDOM) ! SELF
               IFACE=NFACE1+NFACE2+IFACES
               LFACE(2,IFACE)=IFACEN
            ENDIF
 4100    CONTINUE
 4000 CONTINUE
C
CC
CCHY [8] DEALLOCATE WORK ARRAY
CC
      DEALLOCATE(NFSND )
      DEALLOCATE(NFSND2)
      DEALLOCATE(LFSND )
      DEALLOCATE(NFRCV )
      DEALLOCATE(NFRCV2)
      DEALLOCATE(LFRCV )
C
      RETURN
C
 9999 CONTINUE
      WRITE(IUT0,*)
      WRITE(IUT0,*) ERMSGC
      IERR=1
C
      RETURN
      END
C
