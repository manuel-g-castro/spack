C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.8.1                                   C
C                                                                      C
C  SUB ROUTINE : MKFAC1                                                C
C                                                                      C
C                                       WRITTEN BY H.YOSHIMURA         C
C                                                                      C
C                                                                      C
C CONTACT ADDRESS : IIS, THE UNIVERSITY OF TOKYO, CISS                 C
C                                                                      C
C THERMO-FLUID ANALYSIS SOLVERS FOR LARGE-SCALE-ASSEMBLY               C
C                                                                      C
C======================================================================C
      SUBROUTINE MKFAC1(IPART,NE,NP,N2,NSP,NS,MEP,MFACE,
     *                  NODE,LOCAL,NEP,IENP,X,Y,Z,
     *                  NFACE,LFACE,AVEC,DVEC,
     *                  LEFACE,IUT6,IUT0,IERR)
C
      IMPLICIT NONE
C
C     [INPUT]
      INTEGER*4 IPART,NE,NP,N2,NSP,NS,MEP,MFACE
      INTEGER*4 NODE(N2,NE),LOCAL(NSP,NS,4),NEP(NP),IENP(MEP,NP)
      REAL*8    X(NP),Y(NP),Z(NP)
C
C     [IN-OUTPUT]
      INTEGER*4 NFACE
      INTEGER*4 LFACE(5,MFACE)
      REAL*4    AVEC(4,MFACE),DVEC(3,MFACE)
      INTEGER*4 LEFACE(6,NE)
      INTEGER*4 IUT6,IUT0,IERR

C
C     [LOCAL]
      INTEGER*4 MSD
      INTEGER*4,ALLOCATABLE::LWORK(:,:)
      INTEGER*4 IP,IP1,IP2,IP3,IP4,IMATCH
      INTEGER*4 IE,NSD,IETYPE,NLS,IE1,IE2,IS,IS1,IS2,I,I1,I2,J
      INTEGER*4 IB1,IB2,IB3,IB4,IBTYPE,IERRA
C
      CHARACTER*60 ERMSGC
     * /' ## SUBROUTINE MKFAC1: ERROR OCCURED             ; RETURNED' /
      CHARACTER*60 EREXP1
     * / ' ALLOCATING FAILED                                        ' /
      CHARACTER*60 EREXP2
     * / ' THE NUMBER OF ADJACENT FACE TO NODE IS OVER MBFDOM       ' /
      CHARACTER*60 EREXP3
     * / ' THE NUMBER OF FACE IS OVER MFACE                         ' /

C
CC
CCHY [1] COUNT MAXMUM NUMBER OF ADJACENT FACES TO NODE IP
CC
      MSD=0
      DO 1000 IP=1,NP
         NSD=0
         DO 1100 I=1,NEP(IP)
            IE=IENP(I,IP)
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
            DO 1200 IS=1,NLS
C
               IP1=NODE(LOCAL(1,IS,IETYPE),IE)
               IP2=NODE(LOCAL(2,IS,IETYPE),IE)
               IP3=NODE(LOCAL(3,IS,IETYPE),IE)
C
               IF((IETYPE.EQ.1            ).OR. ! TRI
     *              (IETYPE.EQ.2.AND.IS.LE.4).OR.
     *              (IETYPE.EQ.3.AND.IS.LE.2))THEN
                  IP4=0
               ELSE                             ! QUAD
                  IP4=NODE(LOCAL(4,IS,IETYPE),IE)
               ENDIF
               IF(     IP.NE.IP1 .AND. IP.NE.IP2
     *            .AND.IP.NE.IP3 .AND. IP.NE.IP4) GOTO 1200
C
               NSD=NSD+1
 1200       CONTINUE
 1100    CONTINUE
C
         IF (NSD.GT.MSD) THEN
            MSD=NSD
         ENDIF
C
 1000 CONTINUE
C
      WRITE(IUT6,*) 'MAXMUM NUMBER OF ADJACENT FACES TO NODE :',MSD
C
CC
CCHY [2] ALLOCATE WORK ARRAYS
CC
      ALLOCATE(LWORK(6,MSD),STAT=IERR)
      CALL ERRCHK(IUT6,IPART,1,IERR,IERRA)
      IF(IERRA.NE.0) THEN
         WRITE(IUT0,*) EREXP1
         GOTO 9999
      ENDIF
C
      DO 2000 IE=1,MSD
         DO 2100 J=1,6
            LWORK(J,IE)=0
 2100    CONTINUE
 2000 CONTINUE
C
CC
CCHY [3] MAKE FACE LIST FOR INNER REGION
CC
      DO 3000 IP=1,NP
         NSD=0
         DO 3100 I=1,NEP(IP)
            IE=IENP(I,IP)
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
            DO 3200 IS=1,NLS
               IF (LEFACE(IS,IE).NE.0) GOTO 3200
C
               IP1=NODE(LOCAL(1,IS,IETYPE),IE)
               IP2=NODE(LOCAL(2,IS,IETYPE),IE)
               IP3=NODE(LOCAL(3,IS,IETYPE),IE)
C
               IF ((IETYPE.EQ.1            ).OR. ! TRI
     *             (IETYPE.EQ.2.AND.IS.LE.4).OR.
     *             (IETYPE.EQ.3.AND.IS.LE.2)) THEN
                  IP4=0
               ELSE                             ! QUAD
                  IP4=NODE(LOCAL(4,IS,IETYPE),IE)
               ENDIF
               IF (     IP.NE.IP1 .AND. IP.NE.IP2
     *             .AND.IP.NE.IP3 .AND. IP.NE.IP4) GOTO 3200
C
               NSD=NSD+1
               IF (NSD.GT.MSD) THEN
                  WRITE(IUT0,*) EREXP2,MSD
                  GOTO 9999
               ENDIF
               LWORK(1,NSD)=IP1
               LWORK(2,NSD)=IP2
               LWORK(3,NSD)=IP3
               LWORK(4,NSD)=IP4
               LWORK(5,NSD)=IE
               LWORK(6,NSD)=IS
 3200       CONTINUE
 3100    CONTINUE
C
         DO 3300 I1=1,NSD
            IE1=LWORK(5,I1)
            IS1=LWORK(6,I1)
            DO 3400 I2=I1+1,NSD
               IE2=LWORK(5,I2)
               IS2=LWORK(6,I2)
               IF (IE1.EQ.IE2) GOTO 3400
               IF (LEFACE(IS2,IE2).NE.0) GOTO 3400
C
               CALL MATCHX(LWORK(1,I1),LWORK(1,I2),IMATCH)
               IF (IMATCH.NE.0) THEN
                  NFACE=NFACE+1
                  IF (NFACE.GT.MFACE) THEN
                     WRITE(IUT0,*) EREXP3,MFACE
                     GOTO 9999
                  ENDIF
                  LEFACE(IS1,IE1)=IE2
                  LEFACE(IS2,IE2)=IE1
                  IF (IE1.GT.IE2) THEN
                     LFACE(1,NFACE)=IE1
                     LFACE(2,NFACE)=IE2
                     LFACE(3,NFACE)=IS1
                     LFACE(4,NFACE)=IS2
                     CALL CALAVC(IE1,IS1,NE,NP,N2,NSP,NS,NODE,LOCAL,
     *                           X,Y,Z,AVEC(1,NFACE))
                  ELSE
                     LFACE(1,NFACE)=IE2
                     LFACE(2,NFACE)=IE1
                     LFACE(3,NFACE)=IS2
                     LFACE(4,NFACE)=IS1
                     CALL CALAVC(IE2,IS2,NE,NP,N2,NSP,NS,NODE,LOCAL,
     *                           X,Y,Z,AVEC(1,NFACE))
                  ENDIF
                  LFACE(5,NFACE)=0
                  GOTO 3300
               ENDIF
 3400       CONTINUE
 3300    CONTINUE
C
 3000 CONTINUE
C
CC
CCHY [4] DEALLOCATE WORK ARRAYS
CC
      DEALLOCATE(LWORK)
C
      RETURN
C
 9999 CONTINUE
      WRITE(IUT0,*) ERMSGC
      IERR=1
      RETURN
C
      END
