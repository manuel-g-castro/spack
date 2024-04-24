C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    AXISGE                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE AXISGE(JPSOUT,IUTPS,SXMIN,SXMAX,SYMIN,SYMAX,
     *                  NDIVX,NDIVY,TITLEX,TITLEY,ORGX,ORGY,RNGX,RNGY,
     *                  ICLAXS,CSZAXS,SCLX,SCLY)
      IMPLICIT REAL*4(A-H,O-Z)
      CHARACTER*20 TITLEX,TITLEY
C
#ifdef VOS
      INCLUDE (GN)
#else
      INCLUDE 'gn.h'
#endif
C
      DATA  NTITLE / 20 /
C
      CHARACTER* 9 SCALE         
      DATA NSCALE / 9 /
      CHARACTER* 8 FORMAT        
      DATA FORMAT / '(1PE9.2)' /
C
      DIMENSION XBUF(2), YBUF(2)
C
      D = 2.0
      DSCLXX = 0.5*NSCALE*CSZAXS
      DSCLXY = CSZAXS+D
      DSCLYX = NSCALE*CSZAXS+D
      DSCLYY = 0.5*CSZAXS
C
      DTTLXX = NTITLE*CSZAXS
      DTTLXY = 2.0*(CSZAXS+D)
      DTTLYX = 0.0
      DTTLYY = D
C
      ZERO = 0.E0
      CUTOFF = 1.E-3
C
C
C      DRAW AXES AND WRITE SCALES IN THE E-FORMAT
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
C          SXMIN       ; X-DIR. MIN. SCALE
C          SXMAX       ; X-DIR. MAX. SCALE
C          SYMIN       ; Y-DIR. MIN. SCALE
C          SYMAX       ; Y-DIR. MAX. SCALE
C          NDIVX       ; X-DIR. DIVIDING NUMBER
C          NDIVY       ; Y-DIR. DIVIDING NUMBER
C
C          TITLEX      ; THE TITLE FOR THE X-AXIS
C          TITLEY      ; THE TITLE FOR THE Y-AXIS
C
C          ORGX        ; X-DIR. GRAPHIC ORIGIN
C          ORGY        ; Y-DIR. GRAPHIC ORIGIN
C          RNGX        ; X-DIR. GRAPHIC RANGE
C          RNGY        ; Y-DIR. GRAPHIC RANGE
C
C          ICLAXS      ; COLOR INDEX    FOR WRITING SCALES AND TITLES
C          CSZAXS      ; CHARACTER SIZE FOR WRITING SCALES AND TITLES
C          THEAXS      ; TILTED ANGLE   FOR WRITING SCALES AND TITLES
C
C       (2) OUTPUT
C          SCLX        ; X-DIR. SCALING FACTOR
C          SCLY        ; Y-DIR. SCALING FACTOR
C
C
      CALL GNCSET(ICLAXS)
      IF(JPSOUT.EQ.1) CALL PSGRAY(IUTPS,ICLAXS)
      IF(JPSOUT.EQ.2) CALL PSCSET(IUTPS,ICLAXS)
C
      LNTYPE = GLSOLD
      PLTYPE = GLSOLD
C
      LNWDTH = GLNORM
      PLWDTH = GLNORM
C
C
C      (1) DRAW AXES
C
C
C VERTICAL   AXES
C
      DO 10 IDIV = 0 , NDIVX
          RATIO  = FLOAT(IDIV)/FLOAT(NDIVX)
          POINTX = ORGX+RATIO*RNGX
          XBUF(1) = POINTX
          YBUF(1) = ORGY     
          XBUF(2) = POINTX
          YBUF(2) = ORGY+RNGY
          CALL GNLINE(XBUF,YBUF,2)
          IF(JPSOUT.GE.1) CALL PSLINE(IUTPS,XBUF,YBUF,2)
   10 CONTINUE
C
C HORIZONTAL AXES
C
      DO 20 IDIV = 0 , NDIVY
          RATIO  = FLOAT(IDIV)/FLOAT(NDIVY)
          POINTY = ORGY+RATIO*RNGY
          XBUF(1) = ORGX
          YBUF(1) = POINTY     
          XBUF(2) = ORGX+RNGX
          YBUF(2) = POINTY
          CALL GNLINE(XBUF,YBUF,2)
          IF(JPSOUT.GE.1) CALL PSLINE(IUTPS,XBUF,YBUF,2)
   20 CONTINUE
C
C
C      (2) WRITE SCALES ON THE AXES
C
C
C ON THE HORIZONTAL AXIS
C
C
      TXSIZE = CSZAXS
      PTSIZE = CSZAXS
      DO 30 IDIV = 0 , NDIVX
          RATIO  = FLOAT(IDIV)/FLOAT(NDIVX)
          POINTX = ORGX+RATIO*RNGX-DSCLXX
          POINTY = ORGY           -DSCLXY
          VALUE = SXMIN+RATIO*(SXMAX-SXMIN)
          IF(ABS(VALUE/(SXMAX-SXMIN)).LT.CUTOFF) VALUE = ZERO
          WRITE(SCALE,FORMAT) VALUE
          CALL GNTEXT(POINTX,POINTY,SCALE,NSCALE)
          IF(JPSOUT.GE.1) CALL PSTEXT(IUTPS,POINTX,POINTY,SCALE,NSCALE)
   30 CONTINUE
C
C ON THE VERTICAL   AXIS
C
      DO 40 IDIV = 0 , NDIVY
          RATIO  = FLOAT(IDIV)/FLOAT(NDIVY)
          POINTX = ORGX           -DSCLYX
          POINTY = ORGY+RATIO*RNGY-DSCLYY
          VALUE = SYMIN+RATIO*(SYMAX-SYMIN)
          IF(ABS(VALUE/(SYMAX-SYMIN)).LT.CUTOFF) VALUE = ZERO
          WRITE(SCALE,FORMAT) VALUE
          CALL GNTEXT(POINTX,POINTY,SCALE,NSCALE)
          IF(JPSOUT.GE.1) CALL PSTEXT(IUTPS,POINTX,POINTY,SCALE,NSCALE)
   40 CONTINUE
C
C
C      (3) WRITE TITLES FOR THE AXES
C
C
C FOR THE HORIZONTAL AXIS
C
      POINTX = ORGX+RNGX-DTTLXX
      POINTY = ORGY     -DTTLXY
      CALL GNTEXT(POINTX,POINTY,TITLEX,NTITLE)
      IF(JPSOUT.GE.1) CALL PSTEXT(IUTPS,POINTX,POINTY,TITLEX,NTITLE)
C
C FOR THE VERTICAL   AXIS
C
      POINTX = ORGX     +DTTLYX
      POINTY = ORGY+RNGY+DTTLYY
      CALL GNTEXT(POINTX,POINTY,TITLEY,NTITLE)
      IF(JPSOUT.GE.1) CALL PSTEXT(IUTPS,POINTX,POINTY,TITLEY,NTITLE)
C
C
C      (4) CALCULATE SCALING FACTORS FOR X AND Y DIRECTIONS
C
C
      SCLX = RNGX/(SXMAX-SXMIN)
      SCLY = RNGY/(SYMAX-SYMIN)
C
C
      RETURN
      END
