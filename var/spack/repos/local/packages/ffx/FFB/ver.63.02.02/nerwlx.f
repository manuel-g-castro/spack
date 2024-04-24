C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.8.1                                   C
C                                                                      C
C  SUB ROUTINE : NERWLX                                                C
C                                                                      C
C                                       WRITTEN BY Y.YAMADE            C
C                                                                      C
C                                                                      C
C CONTACT ADDRESS : IIS, THE UNIVERSITY OF TOKYO, CISS                 C
C                                                                      C
C THERMO-FLUID ANALYSIS SOLVERS FOR LARGE-SCALE-ASSEMBLY               C
C                                                                      C
C======================================================================C
      SUBROUTINE NERWLX(X,Y,Z,NODE,NE,NP,N2,NEX,NS,NSP,
     *                  LOCAL,LEWALL,NEWALL,NEAR,DSNEAR,WRK1,WRK2,WRK3)
C
      IMPLICIT NONE
C
      INTEGER*4 NODE,NE,NP,N2,NEX,NS,NSP,LOCAL,LEWALL,NEWALL,NEAR
      REAL*4    X,Y,Z,DSNEAR,WRK1,WRK2,WRK3
C
      INTEGER*4 I,IS,IE,NNPE,IEWALL,IEW,IETYPE,IPW1,IPW2,IPW3,IPW4
      REAL*4    XWG,YWG,ZWG,D
C
      DIMENSION X(NP),Y(NP),Z(NP),NODE(N2,NE),NEX(8),LOCAL(NSP,NS,4),
     1          LEWALL(2,NEWALL),NEAR(NE),DSNEAR(NE),
     2          WRK1(NE),WRK2(NE),WRK3(NE)
C
C
C      SEARCH THE NEAREST WALL SURFACE FOR ALL THE ELEMENTS
C         ( 3-D CALCULATION : SINGLE WORD & MULTI ELEMENT VERSION )
C                                           CODED BASED ON 'NEARWL'
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          X       (IP); X-COORDINATE OF GLOBAL NODES
C          Y       (IP); Y-COORDINATE OF GLOBAL NODES
C          Z       (IP); Z-COORDINATE OF GLOBAL NODES
C          NODE  (I,IE); NODE NO. TABLE BASED ON ELEMENT
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          NP          ; NUMBER OF TOTAL    NODES
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT
C          LOCAL (I,IS); NODE NUMBER TABLE DEFINING ELEMENT SURFACES
C          LEWALL(I,IBE); WALL BOUNDARY ELEMENT AND ITS SURFACE
C          NEWALL      ; NUMBER OF WALL BOUNDARY SURFACES
C
C       (2) OUTPUT
C          NEAR    (IE); NEAREST WALL SURFACE
C          DSNEAR  (IE); DISTANCE TO THE NEAREST WALL SURFACE
C
C       (4) WORK
C          WRK1    (IE); STORES ELEMENT CENTER X-COORDINATES
C          WRK2    (IE); STORES ELEMENT CENTER Y-COORDINATES
C          WRK3    (IE); STORES ELEMENT CENTER Z-COORDINATES
C
C
      DO 100 IE = 1 , NE
         IF(     NODE(8,IE).GE.1) THEN ! HEX
            NNPE = 8
         ELSE IF(NODE(6,IE).GE.1) THEN ! PRS
            NNPE = 6
         ELSE IF(NODE(5,IE).GE.1) THEN ! PYR
            NNPE = 5
         ELSE                          ! TET
            NNPE = 4
         ENDIF   
         WRK1(IE) = 0.0E0
         WRK2(IE) = 0.0E0
         WRK3(IE) = 0.0E0
         DO 101 I=1,NNPE
            WRK1(IE) = WRK1(IE) + X(NODE(I,IE))
            WRK2(IE) = WRK2(IE) + Y(NODE(I,IE))
            WRK3(IE) = WRK3(IE) + Z(NODE(I,IE))
 101     CONTINUE
         WRK1(IE) = WRK1(IE)/FLOAT(NNPE)
         WRK2(IE) = WRK2(IE)/FLOAT(NNPE)
         WRK3(IE) = WRK3(IE)/FLOAT(NNPE)
  100 CONTINUE
C
C
      DO 210 IEWALL = 1 , NEWALL
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
          DO 200 IE = 1 , NE
              D = (WRK1(IE)-XWG)**2+(WRK2(IE)-YWG)**2+(WRK3(IE)-ZWG)**2
              IF(IEWALL.EQ.1 .OR. D.LE.DSNEAR(IE)) THEN
                  DSNEAR(IE) = D
                  NEAR  (IE) = IEWALL
              ENDIF
  200     CONTINUE
  210 CONTINUE
C
C
      DO 300 IE = 1 , NE
          DSNEAR(IE) = SQRT(DSNEAR(IE))
  300 CONTINUE
C
C
      RETURN
      END
