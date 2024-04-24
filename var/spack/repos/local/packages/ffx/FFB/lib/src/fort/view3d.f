C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    VIEW3D                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE VIEW3D(JPSOUT,IUTPS,IMODE,X,Y,SR,NODE,NE,NP,N,
     *                  SCL,RAD1,RAD2,RNGX,RNGY,SFCRT,ICL3D1,ICL3D2,
     *                  XX,YY,ZZ,ZZG,LIST)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION X(NP),Y(NP),SR(NP),NODE(N,NE),
     1          XX(NP),YY(NP),ZZ(NP),ZZG(NE),LIST(NE)
C
#ifdef VOS
      INCLUDE (GN)
#else
      INCLUDE 'gn.h'
#endif
C
      DIMENSION T(3,3)
      DIMENSION XP(5), YP(5)
C
C
C      DISPLAY 3-DIMENSIONAL VIEW OF SCALAR FIELD
C         ( 2-D GRAPHICS )
C
C
C     NOTE 1 ; CALLING GENERIC GRAPHIC AND POSTSCRIPT INTERFACES.
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          JPSOUT      ; POSTSCRIPT FILE WILL BE OUTPUT IF SET TO 1 OE 2
C          IUTPS       ; FILE NUMBER TO ACCESS POSTSCRIPT FILE
C          IMODE       ; SPECIFIES DISPLAY MODE AS FOLLOWS
C                   1 --- TRANSPARENT
C                   2 --- SHADING
C          X       (IP); X-COORDINATE OF NODES
C          Y       (IP); Y-COORDINATE OF NODES
C          SR      (IP); SCALAR
C          NODE  (I,IE); NODE TABLE
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          NP          ; NUMBER OF TOTAL NODES
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT
C          SCL         ; SCALAR-TO-FIELD SCALING FACTOR
C          RAD1        ; ANGLE BETWEEN VIEWING DIR. AND X-AXIS
C          RAD2        ; ANGLE BETWEEN VIEWING DIR. AND Z-AXIS
C          RNGX        ; X-DIR. GRAPHIC RANGE SPECIFIED
C          RNGY        ; Y-DIR. GRAPHIC RANGE SPECIFIED
C          SFCRT       ; GRAPHIC SCALING COEFFICIENT
C          ICL3D1      ; COLOR INDEX   USED TO PAINT ELEMENT
C          ICL3D2      ; COLOR INDEX   USED TO DRAW  ELEMENT BORDER
C
C       (4) WORK
C          XX      (IP); TRANSVERSED X-DIR. COORDINATE OF NODE
C          YY      (IP); TRANSVERSED Y-DIR. COORDINATE OF NODE
C          ZZ      (IP); TRANSVERSED Z-DIR. COORDINATE OF NODE
C          ZZG     (IE); TRANSVERSED Z-DIR. COORDINATE OF ELEMENT
C          LIST (IEORD); ELEMENT PAINTING ORDER SPECIFYING LIST
C
C
      NEORD = NE
C
C
C      CALCULATE PROJECTED X-Y COORDINATE OF NODE
C
C
      CALL TRANSG(RAD1,RAD2,T)
      DO 10 IP = 1 , NP
          XX(IP) = T(1,1)*X(IP)+T(2,1)*Y(IP)+T(3,1)*SCL*SR(IP)
          YY(IP) = T(1,2)*X(IP)+T(2,2)*Y(IP)+T(3,2)*SCL*SR(IP)
          ZZ(IP) = T(1,3)*X(IP)+T(2,3)*Y(IP)+T(3,3)*SCL*SR(IP)
   10 CONTINUE
C
C
C      GRAPHIC SCALING
C
C
      CALL SCALEG(XX,YY,NP,RNGX,RNGY,SFCRT,XMIN,XMAX,YMIN,YMAX,SFC)
      DO 20 IP = 1 , NP
          XX(IP) = SFC*(XX(IP)-XMIN)
          YY(IP) = SFC*(YY(IP)-YMIN)
   20 CONTINUE
C
C
C      PAINTING ORDER DECISION FOR SHADING DISPLAY
C
C
      IF(IMODE.EQ.2) THEN
          DO 30 IE = 1 , NE
              ZZG(IE) = 0.25*(ZZ(NODE(1,IE))+ZZ(NODE(2,IE))
     &                       +ZZ(NODE(3,IE))+ZZ(NODE(4,IE)))
   30     CONTINUE
          CALL ORDERG(ZZG,NE,LIST)
      ELSE
          DO 35 IE = 1 , NE
              LIST(IE) = IE
   35     CONTINUE
      ENDIF
C
C
C      3-D VIEW
C
C
      DO 60 IEORD = 1 , NEORD
        IE = LIST(IEORD)
C
C SHADING
C
        IF (IMODE.EQ.2) THEN
           CALL GNCSET(ICL3D1)
           IF(JPSOUT.EQ.1) CALL PSGRAY(IUTPS,ICL3D1)
           IF(JPSOUT.EQ.2) CALL PSCSET(IUTPS,ICL3D1)
C
           DO 40 I = 1 , N
               IP = NODE(I,IE)
               XP(I) = XX(IP)
               YP(I) = YY(IP)
   40      CONTINUE
           CALL  GNFILL(XP,YP,N)
           IF(JPSOUT.GE.1) CALL  PSFILL(IUTPS,XP,YP,N)
        ENDIF
C
C LINE
C
        LNTYPE = GLSOLD
        PLTYPE = GLSOLD
C
        LNWDTH = GLBOLD
        PLWDTH = GLBOLD
C
        CALL GNCSET (ICL3D2)
        IF(JPSOUT.EQ.1) CALL PSGRAY (IUTPS,ICL3D2)
        IF(JPSOUT.EQ.2) CALL PSCSET (IUTPS,ICL3D2)
        DO 50 I = 1 , N
            IP = NODE(I,IE)
            XP(I) = XX(IP)
            YP(I) = YY(IP)
   50    CONTINUE
         XP(N+1) = XX(NODE(1,IE))
         YP(N+1) = YY(NODE(1,IE))
         CALL  GNLINE(XP,YP,N+1)
         IF(JPSOUT.GE.1) CALL  PSLINE(IUTPS,XP,YP,N+1)
   60 CONTINUE
C
      RETURN
      END
