C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    HELPMS                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE HELPMS(X,Y,NODE,NE,NP,N,LSIDE,NS,
     *                  RNGX,RNGY,WINDX1,WINDX2,WINDY1,WINDY2,
     *                  ICLMSH,ICLBLN,ICLMRK,ICLNUM,
     *                  CSZMRK,CSZNUM,ICODE,DELT,IUT5,IUT6)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION X(NP),Y(NP),NODE(N,NE),LSIDE(2,NS)
C
      INTEGER*4 KEEPAS
      PARAMETER ( KEEPAS = 1 )
C
      REAL*4 SBOUND(4)
      DATA        SBOUND(1) / 0.2344 /
      DATA        SBOUND(2) / 0.9922 /
      DATA        SBOUND(3) / 0.23   /
      DATA        SBOUND(4) / 1.0    /
C
      REAL*4 GBOUND(4)
C
      REAL*4 XPARY(5),YPARY(5)
C
      CHARACTER*8 BUF
C
#ifdef VOS
      INCLUDE (GN)
#else
      INCLUDE 'gn.h'
#endif
C
C
C
C      DISPLAY NODE'S & ELEMENT'S  NO. ON T560/20 TERMINAL
C         ( 2-D GRAPHICS )
C
C
C     NOTE 1 ; CGDM GRAPHIC SYSTEM IS USED.
C     NOTE 2 ; GRAPHIC INITIALIZE AND CLOSING IS DONE IN THIS ROUTINE.
C     NOTE 3 ; SOFTWARE ZOOMING FUNCTION IS IMPLEMENTED.
C     NOTE 4 ; THIS SUBROUTINE IS AVAILABLE FOR ANY KIND OF ELEMENTS
C             ( INCLUDING A MIXTURE OF DIFFERENT KINDS OF ELEMENTS ).
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          X(IP)       ; X-COORDINATE OF NODES
C          Y(IP)       ; Y-COORDINATE OF NODES
C          NODE(I,IE)  ; NODE TABLE
C                       ( IF AN ELEMENT IE HAS LESS NODES THAN N,
C                        NODE(I+NNODE,IE),,NODE(N,IE) MUST HAVE BEEN
C                        SET TO ZERO, WHERE NNODE DENOTES THE NUMBER
C                        OF NODES WHICH THE ELEMENT HAS. )
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          NP          ; NUMBER OF TOTAL    NODES
C          N           ; ALIGNMENT  DIMENSION OF THE ARRAY NODE (I,IE)
C          LSIDE(I,IS) ; BOUNDARY SIDES
C          NS          ; NUMBER OF BOUNDARY SIDES
C
C          RNGX        ; X-DIR. GRAPHIC RANGE
C          RNGY        ; Y-DIR. GRAPHIC RANGE
C          WINDX1      ; GRAPHIC WINDOW ( X-COOR. AT LEFT  BOTTOM )
C          WINDX2      ; GRAPHIC WINDOW ( X-COOR. AT RIGHT UP     )
C          WINDY1      ; GRAPHIC WINDOW ( Y-COOR. AT LEFT  BOTTOM )
C          WINDY2      ; GRAPHIC WINDOW ( Y-COOR. AT RIGHT UP     )
C          ICLMSH      ; COLOR INDEX USED TO DRAW MESH
C          ICLBLN      ; COLOR INDEX USED TO DRAW BORDER LINE
C          ICLMRK      ; COLOR INDEX USED TO MARK BOUNDARY NODE
C          ICLNUM      ; COLOR INDEX USED TO WRITE NUMBER
C          CSZMRK      ; CHARACTER SIZE OF MARK
C          CSZNUM      ; CHARACTER SIZE OF NUMBER
C          ICODE       ; CHARACTER CODE OF MARK
C          DELT        ; DIFFERENTIAL DISTANCE
C          IUT5        ; DEVICE NUMBER FOR COMMUNICATION (INPUT)
C          IUT6        ; DEVICE NUMBER FOR COMMUNICATION (OUTPUT)
C
C       (2) OUTPUT
C          NONE
C
C
      GBOUND(1) = WINDX1
      GBOUND(2) = WINDX2
      GBOUND(3) = WINDY1
      GBOUND(4) = WINDY2
C
C      DISPLAY MODE SPECIFICATION
C
C
      WRITE(IUT6,*) ' SPECIFY DISPLAY MODE ; IMODE'
      WRITE(IUT6,*) '    0 - ONLY DRAW MESH SYSTEM'
      WRITE(IUT6,*) '    1 - WRITE BOUNARY NODE  NO. WITH BORDER  LINE'
      WRITE(IUT6,*) '    2 - WRITE BOUNARY NODE  NO. WITH ALL THE MESH'
      WRITE(IUT6,*) '    3 - WRITE ALL THE NODE  NO. WITH ALL THE MESH'
      WRITE(IUT6,*) '    4 - WRITE ALL THE ELEM. NO. WITH ALL THE MESH'
      READ (IUT5,*) IMODE
C
C
C      CLIPPING RANGE SPECIFICATION
C
C
      CALL FINDMM(X,NP,OXMIN,OXMAX)
      CALL FINDMM(Y,NP,OYMIN,OYMAX)
      XMIN = OXMIN 
      XMAX = OXMAX 
      YMIN = OYMIN 
      YMAX = OYMAX
C
      WRITE(IUT6,*) ' ENTER ; 1 TO HAVE LOCAL ZOOMING'
      READ (IUT5,*) JZOOM
C
      IF(JZOOM.EQ.1) THEN
      WRITE(IUT6,*) ' SPECIFY ZOOMING RANGE  ; XMIN, XMAX, YMIN, YMAX'
      READ (IUT5,*)  XMIN , XMAX , YMIN , YMAX
      ENDIF
C
      SFC = AMIN1(RNGX/(XMAX-XMIN) , RNGY/(YMAX-YMIN))
C
C
C      TERMINAL INITIALIZATION
C
C
      CALL GNINIT(GBOUND,SBOUND,KEEPAS)
C
C MARK
      MKTYPE = ICODE
C
      MKSIZE = CSZMRK
C LINE
      LNTYPE = GLSOLD
      LNWDTH = GLNORM
C TEXT
      TXSIZE = CSZNUM
C
C
C      MESH DRAWING
C
C
      IF(IMODE.EQ.0 .OR. IMODE.GE.2) THEN
          CALL GNCSET(ICLMSH)
          DO 500 IE = 1 , NE
              NNODE = 0
              DO 100 I = 1 , N
                  IF(NODE(I,IE).NE.0) NNODE = NNODE+1
  100         CONTINUE
              DO 200 I = 1 , NNODE
                  IF(X(NODE(I,IE)).GE.XMIN .AND.
     &               X(NODE(I,IE)).LE.XMAX .AND.
     &               Y(NODE(I,IE)).GE.YMIN .AND.
     &               Y(NODE(I,IE)).LE.YMAX ) GO TO 300
  200         CONTINUE
              GO TO 500
  300         CONTINUE
              DO 400 I = 1 , NNODE
                  XPARY(I) = (X(NODE(I,IE))-XMIN)*SFC
                  YPARY(I) = (Y(NODE(I,IE))-YMIN)*SFC
  400         CONTINUE
              XPARY(NNODE+1) = (X(NODE(1,IE))-XMIN)*SFC
              YPARY(NNODE+1) = (Y(NODE(1,IE))-YMIN)*SFC
              CALL GNLINE(XPARY,YPARY,NNODE+1)
  500     CONTINUE
      ENDIF
C
C
C      BORDER LINE DRAWING
C
C
      IF(IMODE.GE.1) THEN
      CALL GNCSET(ICLBLN)
      DO 800 IS = 1 , NS
          DO 600 I = 1 , 2
              IF(X(LSIDE(I,IS)).GE.XMIN .AND.
     &           X(LSIDE(I,IS)).LE.XMAX .AND.
     &           Y(LSIDE(I,IS)).GE.YMIN .AND.
     &           Y(LSIDE(I,IS)).LE.YMAX ) GO TO 700
  600     CONTINUE
          GO TO 800
  700     CONTINUE
          XPARY(1) = (X(LSIDE(1,IS))-XMIN)*SFC
          YPARY(1) = (Y(LSIDE(1,IS))-YMIN)*SFC
          XPARY(2) = (X(LSIDE(2,IS))-XMIN)*SFC
          YPARY(2) = (Y(LSIDE(2,IS))-YMIN)*SFC
          CALL GNLINE(XPARY,YPARY,2)
  800 CONTINUE
C
C
C      BOUNDARY NODE MARKING
C
C
      CALL GNCSET(ICLMRK)
      DO 1100 IS = 1 , NS
          DO 900 I = 1 , 2
              IF(X(LSIDE(I,IS)).GE.XMIN .AND.
     &           X(LSIDE(I,IS)).LE.XMAX .AND.
     &           Y(LSIDE(I,IS)).GE.YMIN .AND.
     &           Y(LSIDE(I,IS)).LE.YMAX ) GO TO 1000
  900     CONTINUE
          GO TO 1100
 1000     CONTINUE
          XPARY(1) = (X(LSIDE(1,IS))-XMIN)*SFC
          YPARY(1) = (Y(LSIDE(1,IS))-YMIN)*SFC
          CALL GNMARK(XPARY,YPARY,1)
          XPARY(1) = (X(LSIDE(2,IS))-XMIN)*SFC
          YPARY(1) = (Y(LSIDE(2,IS))-YMIN)*SFC
          CALL GNMARK(XPARY,YPARY,1)
 1100 CONTINUE
      ENDIF
C
C
C      BOUNDARY NODE NO. WRITING
C
C
      IF(IMODE.EQ.1 .OR. IMODE.EQ.2) THEN
          CALL GNCSET(ICLNUM)
          DO 1400 IS = 1 , NS
              DO 1200 I = 1 , 2
                  IF(X(LSIDE(I,IS)).GE.XMIN .AND.
     &               X(LSIDE(I,IS)).LE.XMAX .AND.
     &               Y(LSIDE(I,IS)).GE.YMIN .AND.
     &               Y(LSIDE(I,IS)).LE.YMAX ) GO TO 1300
 1200         CONTINUE
              GO TO 1400
 1300         CONTINUE
              XP = (X(LSIDE(1,IS))-XMIN)*SFC+DELT
              YP = (Y(LSIDE(1,IS))-YMIN)*SFC+DELT
              WRITE(BUF,'(I8)') LSIDE(1,IS)
              CALL GNTEXT(XP,YP,BUF,8)
              XP = (X(LSIDE(2,IS))-XMIN)*SFC+DELT
              YP = (Y(LSIDE(2,IS))-YMIN)*SFC+DELT
              WRITE(BUF,'(I8)') LSIDE(2,IS)
 1400     CONTINUE
      ENDIF
C
C
C      ALL NODE NO. WRITING
C
C
      IF(IMODE.EQ.3) THEN
          CALL GNCSET(ICLNUM)
          DO 1500 IP = 1 , NP
              IF(X(IP).LT.XMIN .OR.
     &           X(IP).GT.XMAX .OR.
     &           Y(IP).LT.YMIN .OR.
     &           Y(IP).GT.YMAX ) GO TO 1500
              XP = (X(IP)-XMIN)*SFC+DELT
              YP = (Y(IP)-YMIN)*SFC+DELT
              WRITE(BUF,'(I8)') IP
              CALL GNTEXT(XP,YP,BUF,8)
 1500     CONTINUE
      ENDIF
C
C
C      ALL ELEMENT NO. WRITING
C
C
      IF(IMODE.EQ.4) THEN
          CALL GNCSET(ICLNUM)
          DO 2000 IE = 1 , NE
              NNODE = 0
              DO 1600 I = 1 , N
                  IF(NODE(I,IE).NE.0) NNODE = NNODE+1
 1600         CONTINUE
              DO 1700 I = 1 , NNODE
                  IF(X(NODE(I,IE)).GE.XMIN .AND.
     &               X(NODE(I,IE)).LE.XMAX .AND.
     &               Y(NODE(I,IE)).GE.YMIN .AND.
     &               Y(NODE(I,IE)).LE.YMAX ) GO TO 1800
 1700         CONTINUE
              GO TO 2000
 1800         CONTINUE
              XG = 0.0
              YG = 0.0
              DO 1900 I = 1 , NNODE
                  XG = XG+X(NODE(I,IE))/NNODE
                  YG = YG+Y(NODE(I,IE))/NNODE
 1900         CONTINUE
              XP = (XG-XMIN)*SFC-DELT
              YP = (YG-YMIN)*SFC-DELT
              WRITE(BUF,'(I8)') IE
              CALL GNTEXT(XP,YP,BUF,8)
 2000     CONTINUE
      ENDIF
C
C
C      TERMINAL CLOSURE
C
C
      CALL GNENDG
C
C
      RETURN
      END
