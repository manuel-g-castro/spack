C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    CLIP2D                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE CLIP2D(JPSOUT,IUTPS,WINDX1,WINDY1,WINDX2,WINDY2,
     *                  ORGX,ORGY,XMIN,YMIN,XMAX,YMAX,RNGX,RNGY,ICLCLP)
      IMPLICIT REAL*4(A-H,O-Z)
C
#ifdef VOS
      INCLUDE (GN)
#else
      INCLUDE 'gn.h'
#endif
C
      DIMENSION XIO(8), YIO(8)
C
C
C      CLIP UNNECESSARY AREA
C         ( 2-D GRAPHICS )
C
C
C     NOTE 1 ; CALLING GENERIC GRAPHIC AND POSTSCRIPT INTERFACES.
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          JPSOUT      ; POSTSCRIPT FILE WILL BE OUTPUT IF SET TO 1 OR 2
C          IUTPS       ; FILE NUMBER TO ACCESS POSTSCRIPT FILE
C          WINDX1      ; GRAPHIC WINDOW ( X-COOR. AT LEFT  BOTTOM )
C          WINDX2      ; GRAPHIC WINDOW ( X-COOR. AT RIGHT UP     )
C          WINDY1      ; GRAPHIC WINDOW ( Y-COOR. AT LEFT  BOTTOM )
C          WINDY2      ; GRAPHIC WINDOW ( Y-COOR. AT RIGHT UP     )
C          ORGX        ; X-DIR. ORIGIN OF THE ACTIVE GRAPHIC AREA
C          ORGY        ; Y-DIR. ORIGIN OF THE ACTIVE GRAPHIC AREA
C          XMIN        ; MINIMUM X-VALUE SPECIFIED
C          YMIN        ; MINIMUM Y-VALUE SPECIFIED
C          XMAX        ; MAXIMUM X-VALUE SPECIFIED
C          YMAX        ; MAXIMUM Y-VALUE SPECIFIED
C          RNGX        ; X-DIR. GRAPHIC RANGE
C          RNGY        ; Y-DIR. GRAPHIC RANGE
C          ICLCLP      ; COLOR INDEX USED TO CLIP
C
C       (2) OUTPUT
C          NONE
C
C
      CALL GNCSET(ICLCLP)
      IF(JPSOUT.EQ.1) CALL PSGRAY(IUTPS,ICLCLP)
      IF(JPSOUT.EQ.2) CALL PSCSET(IUTPS,ICLCLP)
C
      SFC = AMIN1(RNGX/(XMAX-XMIN),RNGY/(YMAX-YMIN))
      XI1 = ORGX
      YI1 = ORGY
      XI2 = ORGX+(XMAX-XMIN)*SFC
      YI2 = ORGY
      XI3 = ORGX+(XMAX-XMIN)*SFC
      YI3 = ORGY+(YMAX-YMIN)*SFC
      XI4 = ORGX
      YI4 = ORGY+(YMAX-YMIN)*SFC
C
      XO1 = WINDX1
      YO1 = WINDY1
      XO2 = WINDX2
      YO2 = WINDY1
      XO3 = WINDX2
      YO3 = WINDY2
      XO4 = WINDX1
      YO4 = WINDY2
C
      XIO(1) = XO2
      YIO(1) = YO2
      XIO(2) = XO3
      YIO(2) = YO3
      XIO(3) = XO4
      YIO(3) = YO4
      XIO(4) = XI4
      YIO(4) = YI4
      XIO(5) = XI3
      YIO(5) = YI3
      XIO(6) = XI2
      YIO(6) = YI2
      XIO(7) = XI1
      YIO(7) = YI1
      XIO(8) = XO1
      YIO(8) = YO1
      CALL GNFILL(XIO,YIO,8)
      IF(JPSOUT.GE.1) CALL PSFILL(IUTPS,XIO,YIO,8)
C
      XIO(1) = XO4
      YIO(1) = YO4
      XIO(2) = XI4
      YIO(2) = YI4
      XIO(3) = XI1
      YIO(3) = YI1
      XIO(4) = XO1
      YIO(4) = YO1
      CALL GNFILL(XIO,YIO,4)
      IF(JPSOUT.GE.1) CALL PSFILL(IUTPS,XIO,YIO,4)
C
C
      RETURN
      END
