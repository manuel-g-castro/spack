C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    NEARWL                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE NEARWL(X,Y,Z,NODE,NE,NP,N,LOCAL,LEWALL,NEWALL,
     *                  NEAR,DSNEAR,WRK1,WRK2,WRK3)
     *                  
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION X(NP),Y(NP),Z(NP),NODE(N,NE),LOCAL(4,6),
     1          LEWALL(2,NEWALL),NEAR(NE),DSNEAR(NE),
     2          WRK1(NE),WRK2(NE),WRK3(NE)
C
C
C      SEARCH THE NEAREST WALL SURFACE FOR ALL THE ELEMENTS
C         ( 3-D ; SINGLE PRECISION )
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
          WRK1(IE) = 0.125E0*(X(NODE(1,IE))+X(NODE(5,IE))
     &                       +X(NODE(2,IE))+X(NODE(6,IE))
     &                       +X(NODE(3,IE))+X(NODE(7,IE))
     &                       +X(NODE(4,IE))+X(NODE(8,IE)))
          WRK2(IE) = 0.125E0*(Y(NODE(1,IE))+Y(NODE(5,IE))
     &                       +Y(NODE(2,IE))+Y(NODE(6,IE))
     &                       +Y(NODE(3,IE))+Y(NODE(7,IE))
     &                       +Y(NODE(4,IE))+Y(NODE(8,IE)))
          WRK3(IE) = 0.125E0*(Z(NODE(1,IE))+Z(NODE(5,IE))
     &                       +Z(NODE(2,IE))+Z(NODE(6,IE))
     &                       +Z(NODE(3,IE))+Z(NODE(7,IE))
     &                       +Z(NODE(4,IE))+Z(NODE(8,IE)))
  100 CONTINUE
C
C
      DO 210 IEWALL = 1 , NEWALL
          IEW  = LEWALL(1,IEWALL)
          IS   = LEWALL(2,IEWALL)
C
          IPW1  = NODE(LOCAL(1,IS),IEW)
          IPW2  = NODE(LOCAL(2,IS),IEW)
          IPW3  = NODE(LOCAL(3,IS),IEW)
          IPW4  = NODE(LOCAL(4,IS),IEW)
C
          XWG   = 0.25E0*(X(IPW1)+X(IPW2)+X(IPW3)+X(IPW4))
          YWG   = 0.25E0*(Y(IPW1)+Y(IPW2)+Y(IPW3)+Y(IPW4))
          ZWG   = 0.25E0*(Z(IPW1)+Z(IPW2)+Z(IPW3)+Z(IPW4))
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
