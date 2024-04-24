      SUBROUTINE FINDNW(IFNDNW,MLST,ME,NE,NP,N2,NEX,NS,NSP,N2D,
     *                  X,Y,Z,MPWLAD,
     *                  LPWALL,NPWALL,
     *                  LEWALL,NEWALL,XNWALL,YNWALL,ZNWALL,
     *                  LEWLAD,NEWLAD,LPWLAE,LPWLAD,NPWLAD,
     *                  DPWLAD,
     *                  LOCAL,NODE,
     *                  LWORK,LWRK01,
     *                  IPART,IDUM,LDOM,NBPDOM,NDOM,IPSLF,IPSND,
     *                  MBPDOM,WRK01,RX,RY,MAXBUF,
     *                  IUT0,IUT6,IERR)
C
      IMPLICIT NONE
C
      INTEGER*4 IFNDNW,MLST,ME,NE,NP,N2,NEX,NS,NSP,N2D,
     *          MPWLAD,LPWALL,NPWALL,LEWALL,NEWALL,
     *          LEWLAD,NEWLAD,LPWLAE,LPWLAD,NPWLAD,
     *          LOCAL,NODE,LWORK,LWRK01,
     *          IPART,IDUM,LDOM,NBPDOM,NDOM,IPSLF,IPSND,
     *          MBPDOM,MAXBUF,IUT0,IUT6,IERR
C
      REAL*4    X,Y,Z,XNWALL,YNWALL,ZNWALL,DPWLAD,WRK01,
     *          RX,RY,XWG,YWG,ZWG,D
C
      INTEGER*4 IP,IEWLAD,I,IPWALL,IS,IE,J,IEW,IETYPE,IEWALL,IPWLAD,
     *          IPW1,IPW2,IPW3,IPW4,IWRK,IWRKL
C
      REAL*4    XNWLAD,YNWLAD,ZNWLAD
C
      DIMENSION LOCAL(NSP,NS,4),NODE(N2,NE),NEX(8),
     *          LPWALL(NPWALL),
     *          LEWALL(MLST,NEWALL),LEWLAD(NEWLAD),
     *          LPWLAE(7,NEWLAD),LPWLAD(MPWLAD),DPWLAD(MPWLAD),
     *          XNWALL(NEWALL),YNWALL(NEWALL),ZNWALL(NEWALL),
     *          X(NP),Y(NP),Z(NP),
     *          XNWLAD(NPWLAD),YNWLAD(NPWLAD),ZNWLAD(NPWLAD),
     *          LWORK(NP),LWRK01(NP),IWRKL(8),
     *          LDOM(NDOM),NBPDOM(NDOM),
     *          IPSLF(MBPDOM,NDOM),IPSND(MBPDOM,NDOM),
     *          WRK01(*),RX(0:N2,ME),RY(0:N2,ME)
C
      IERR = 0
C
      IF (IFNDNW.EQ.1) GOTO 1000
C
C //CLEAR LISTS
C
      DO 100 IP = 1, NP
          LWORK(IP) = 0
          LWRK01(IP) = 0
  100 CONTINUE
C
      DO 101 IEWLAD = 1, NEWLAD
          DO 102 I = 1, 7
              LPWLAE(I,IEWLAD) = 0
  102     CONTINUE
  101 CONTINUE
C
      DO 103 IPWALL = 1, NPWALL
          LWORK(LPWALL(IPWALL)) = 1 
 103  CONTINUE
C
C //SEARCH AND REGISTER WALL-ADJECENT NODES
C
      NPWLAD = 0
C
      DO 110 IEWLAD = 1, NEWLAD
          IE = LEWLAD(IEWLAD)
C
C         // LOCAL WALL-ADJACENT NODE INDEX (1-7(MAX))
          J=0
          DO 120 I = 1, 8
C
              IP = NODE(I,IE)
C
              IF(IP.LT.1) THEN
C             //EXCEEDED LOCAL NODE NUMBER -> GOTO NEXT LEWLAD
                  GOTO 121
              ENDIF
C
              IF(LWORK(IP).EQ.0) THEN
C             //NOT WALL-NODES -> WALL-ADJACENT NODE!                  
                  J=J+1
                  IF(LWRK01(IP).EQ.0) THEN
C                 //NOT REGISTERD IN LPWLAD                      
                      NPWLAD = NPWLAD + 1
                      LPWLAD(NPWLAD) = IP
                      LWRK01(IP) = NPWLAD
                      LPWLAE(J,IEWLAD) = NPWLAD
                  ELSE
C                 //ALREADY REGISTERD IN LPWLAD
                      LPWLAE(J,IEWLAD)=LWRK01(IP)
                  ENDIF
C
              ENDIF
C
  120     CONTINUE
  121     CONTINUE
C
  110 CONTINUE
C
 1000 CONTINUE
C
C // CALCULATE WALL-DISTANCE
C
       DO 200 IEWALL = 1 , NEWALL
          IEW  = LEWALL(1,IEWALL)
          IS   = LEWALL(2,IEWALL)
          IF(     NODE(8,IEW).GE.1) THEN ! HEX
             IETYPE = 4
          ELSE IF(NODE(6,IEW).GE.1) THEN ! PRS
             IETYPE = 3
          ELSE IF(NODE(5,IEW).GE.1) THEN ! PYR
             IETYPE = 2
          ELSE                           ! TET
             IETYPE = 1
          ENDIF
          IF(LOCAL(4,IS,IETYPE).GE.1) THEN      ! QUADRILATERAL
             IPW1 = NODE(LOCAL(1,IS,IETYPE),IEW)
             IPW2 = NODE(LOCAL(2,IS,IETYPE),IEW)
             IPW3 = NODE(LOCAL(3,IS,IETYPE),IEW)
             IPW4 = NODE(LOCAL(4,IS,IETYPE),IEW)
             XWG = ( X(IPW1)+X(IPW2)+X(IPW3)+X(IPW4) )/4.0E0
             YWG = ( Y(IPW1)+Y(IPW2)+Y(IPW3)+Y(IPW4) )/4.0E0
             ZWG = ( Z(IPW1)+Z(IPW2)+Z(IPW3)+Z(IPW4) )/4.0E0
          ELSE                                  ! TRIANGLE   
             IPW1 = NODE(LOCAL(1,IS,IETYPE),IEW)
             IPW2 = NODE(LOCAL(2,IS,IETYPE),IEW)
             IPW3 = NODE(LOCAL(3,IS,IETYPE),IEW)
             XWG = ( X(IPW1)+X(IPW2)+X(IPW3) )/3.0E0
             YWG = ( Y(IPW1)+Y(IPW2)+Y(IPW3) )/3.0E0
             ZWG = ( Z(IPW1)+Z(IPW2)+Z(IPW3) )/3.0E0
          ENDIF   
C
          DO 210 IPWLAD = 1 , NPWLAD
              IP=LPWLAD(IPWLAD)
              D = (X(IP)-XWG)*XNWALL(IEWALL)
     *           +(Y(IP)-YWG)*YNWALL(IEWALL)
     *           +(Z(IP)-ZWG)*ZNWALL(IEWALL)
              IF(IEWALL.EQ.1.OR.D.LE.DPWLAD(IPWLAD).AND.D.GT.0.0E0) 
     *          THEN
                  DPWLAD(IPWLAD) = D
              ENDIF
  210     CONTINUE
  200 CONTINUE
C
      DO 300 IP=1, NP
          WRK01(IP)=0.0
 300  CONTINUE
C
      DO 310 IPWLAD = 1, NPWLAD
          WRK01(LPWLAD(IPWLAD)) = - DPWLAD(IPWLAD)
 310  CONTINUE
C
      IDUM=1
      CALL DDCMAX(IPART,IDUM,LDOM,NBPDOM,NDOM,IPSLF,IPSND,
     *         MBPDOM,WRK01,WRK01,WRK01,NP,IUT0,IERR,RX,RY,MAXBUF)
C
      DO 320 IPWLAD = 1, NPWLAD
          DPWLAD(IPWLAD) = - WRK01(LPWLAD(IPWLAD))
 320  CONTINUE
C
CCTT DEBUG WRITE
C      DO 330 IEWLAD = 1, NEWLAD
C          WRITE(IUT6,'(8I10.8)') LPWLAE(1,IEWLAD), LPWLAE(2,IEWLAD),
C     *         LPWLAE(3,IEWLAD), LPWLAE(4,IEWLAD), LPWLAE(5,IEWLAD),
C     *         LPWLAE(6,IEWLAD), LPWLAE(7,IEWLAD), LEWLAD(IEWLAD)
C 330  CONTINUE
C
CCTT
C
      RETURN
      END
