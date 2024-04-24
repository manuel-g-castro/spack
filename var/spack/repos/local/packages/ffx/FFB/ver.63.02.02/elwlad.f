      SUBROUTINE ELWLAD(ME,NE,NP,N2,NEX,NS,NSP,N2D,
     *                  LOCAL,NODE,MPWLAD,LPWALL,NPWALL,
     *                  LEWLAD,NEWLAD,
     *                  LWORK,
     *                  IUT0,IUT6,IERR)
C
      IMPLICIT NONE
C
      INTEGER*4 ME,NE,NP,N2,NEX,NS,NSP,N2D,
     *          LOCAL,NODE,MPWLAD,LPWALL,NPWALL,
     *          LEWLAD,NEWLAD,LWORK,
     *          IUT0,IUT6,IERR
C
      INTEGER*4 IP,IPWALL,IE,IFLG,I
C
      DIMENSION LOCAL(NSP,NS,4),NODE(N2,NE),NEX(8),
     *          LPWALL(NPWALL),LEWLAD(MPWLAD),
     *          LWORK(NP)
C
      DO 100 IP=1, NP
          LWORK(IP) = 0
 100  CONTINUE
C
      DO 110 IPWALL=1, NPWALL
          LWORK(LPWALL(IPWALL)) = 1
 110  CONTINUE
C
      NEWLAD = 0
C
      DO 200 IE=1, NE
C
          IFLG = 0
C
          DO 210 I=1, 8
              IP=NODE(I,IE)
              IF(IP.EQ.0) GOTO 210
              IF(LWORK(IP).EQ.1) THEN
                  IFLG = 1
                  GOTO 300
              ENDIF
 210      CONTINUE
C
 300      CONTINUE
          IF (IFLG.EQ.0) THEN
              GOTO 200
          ENDIF
C
          NEWLAD = NEWLAD+1
C
          IF(NEWLAD.GT.MPWLAD) THEN
              WRITE(IUT0,*) 'ELWLAD:INSUFFICIENT MEMORY:ERROR'
              IERR=1
              RETURN
          ENDIF
C
          LEWLAD(NEWLAD) = IE
C
 200  CONTINUE
C
      RETURN 
      END
