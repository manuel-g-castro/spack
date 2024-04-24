C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    AXIS2D                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE AXIS2D(JPSOUT,IUTPS,IAXIS,SMINX,SMAXX,SMINY,SMAXY,
     *                  GDELX,GDELY,TITLEX,TITLEY,NTITLE,
     *                  ORGX,ORGY,RNGX,RNGY,ICLAXS,SCLX,SCLY,IUT0,IWRN)
      IMPLICIT REAL*4(A-H,O-Z)
      CHARACTER*10 TITLEX,TITLEY
C
      CHARACTER*80 BUF
      DIMENSION XBUF2(2),YBUF2(2)
C
#ifdef VOS
      INCLUDE (GN)
#else
      INCLUDE 'gn.h'
#endif
C
      CHARACTER*72 WRMSG1
     & /' *** SUBROUTINE AXIS2D ISSUES A WARNING                ***   '/
      CHARACTER*72 WREXP1
     & /' LOGARITHMIC SCALING IS IMPOSSIBLE DUE TO THE NEGATIVE VALUES'/
C
      BASE  = 10.E0
      IBASE = 10
      UNIT  =  1.E0
      ZERO  =  0.E0
      EPS   =  1.E-1
C
      CSZAXS = RNGX/40
      CSZSUF = 0.6*CSZAXS
      D      = RNGX/100
C
      DSCLXX = 2*CSZAXS
      DSCLXY = 2*CSZAXS
      DSCLYX = 6*CSZAXS+D
      DSCLYY = 0.5*CSZAXS
C
      DTTLXX = (NTITLE+7)*CSZAXS
      DTTLXY =   4*CSZAXS
      DTTLYX = ZERO
      DTTLYY = D
C
C
C      WRITE X-Y AXES
C         ( 2-D GRAPHICS )
C
C
C     NOTE 1 ; CALLING GENERIC GRAPHIC AND POSTSCRIPT INTERFACES.
C     NOTE 2 ; IF AN IMPROPER SCALING RANGE SPECIFIED FOR THE LOGARITH-
C             MIC SCALING, THIS SUBROUTINE WILL IMMEDEATELY CLOSE THE
C             GRAPHIC SYSTEM AND RETURN THE SEQUENCE AFTER REPORTING
C             A WARNING MESSAGE WRITTEN IN FORTRAN WRITE.
C     NOTE 3 ; WHILE THE SCALING FACTORS WILL ALWAYS BE CALCULATED
C             BY USING THE MINIMUM SCALE, MAXIMUM SCALE, AND GRAPHIC
C             RANGE SPECIFIED, THE GRIDDING WILL BE DONE ACCORDING
C             EITHER TO THE MINIMUM SCALE AND GRIDDING INTERVAL
C             SPECIFIED OR TO THE MININUM 10TH INCLUDED BETWEEN THE
C             MINIMUM AND MAXIMUM SCALES AND A CONSTANT INRERVAL OF 10,
C             FOR LINEAR SCALE AND LOGARITHMIC SCALE, RESPECTIVELY,
C     NOTE 4 ; THE SIZE AND LOCATIONS OF THE INDICES AND THE TITLES
C             WILL BE AUTOMATICALLY ADJUSTTED, ACCORDING TO THE GRAPHIC
C             RANGE SPECIFIED.
C
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          JPSOUT      ; POSTSCRIPT FILE WILL BE OUTPUT IF SET TO 1 OR 2
C          IUTPS       ; FILE NUMBER TO ACCESS POSTSCRIPT FILE
C          IAXIS       ; SPECIFIES THE AXIS AS FOLLOWS
C                   1 ---     X -    Y
C                   2 --- LOG(X)-    Y
C                   3 ---     X -LOG(Y)
C                   4 --- LOG(X)-LOG(Y)
C          SMINX       ; X-DIR. MIN. SCALE
C          SMAXX       ; X-DIR. MAX. SCALE
C          SMINY       ; Y-DIR. MIN. SCALE
C          SMAXY       ; Y-DIR. MAX. SCALE
C          GDELX       ; X-DIR. GRIDDING INTERVAL(IGNORED FOR LOG SCALE)
C          GDELY       ; Y-DIR. GRIDDING INTERVAL(IGNORED FOR LOG SCALE)
C
C          TITLEX      ; THE TITLE FOR THE X-AXIS
C          TITLEY      ; THE TITLE FOR THE Y-AXIS
C          NTITLE      ; THE LENGTH OF TITLE CHARACTERS
C
C          ORGX        ; X-DIR. GRAPHIC ORIGIN
C          ORGY        ; Y-DIR. GRAPHIC ORIGIN
C          RNGX        ; X-DIR. GRAPHIC RANGE
C          RNGY        ; Y-DIR. GRAPHIC RANGE
C          ICLAXS      ; COLOR INDEX    FOR WRITING THE AXES
C
C          IUT0       ; DEVICE NUMBER TO ISSUE A WARNING
C
C       (2) OUTPUT
C          SCLX        ; X-DIR. SCALING FACTOR
C          SCLY        ; Y-DIR. SCALING FACTOR
C          IWRN        ; RETURN CODE TO REPORT A WARNING
C                   0 --- NORMAL TERMINATION
C                   1 --- A WARNING ISSUED
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
C      (1) CHECK THE GIVEN PARAMETERS
C
C
      IWRN = 0
C
C  X-DIR. SCALING
C
      IF((IAXIS.EQ.2    .OR. IAXIS.EQ.4   )  .AND.
     &   (SMINX.LE.ZERO .OR. SMAXX.LE.ZERO)) THEN
          CALL GNENDG
          IF(JPSOUT.GE.1) CALL PSENDG(IUTPS)
          WRITE(IUT0,*) WRMSG1
          WRITE(IUT0,*) WREXP1
          IWRN = 1
          RETURN
      ENDIF
C
C  Y-DIR. SCALING
C
      IF((IAXIS.EQ.3    .OR. IAXIS.EQ.4   )  .AND.
     &   (SMINY.LE.ZERO .OR. SMAXY.LE.ZERO)) THEN
          CALL GNENDG
          IF(JPSOUT.GE.1) CALL PSENDG(IUTPS)
          WRITE(IUT0,*) WRMSG1
          WRITE(IUT0,*) WREXP1
          IWRN = 1
          RETURN
      ENDIF
C
C
C      (2) CALCULATE THE SCALING FACTORS
C
C
C  X-DIR. SCALE
C
      IF(IAXIS.EQ.1 .OR. IAXIS.EQ.3) THEN
          SCLX = RNGX/(       SMAXX -       SMINX )
      ELSE
          SCLX = RNGX/(ALOG10(SMAXX)-ALOG10(SMINX))
      ENDIF
C
C  Y-DIR. SCALE
C
      IF(IAXIS.EQ.1 .OR. IAXIS.EQ.2) THEN
          SCLY = RNGY/(       SMAXY -       SMINY)
      ELSE
          SCLY = RNGY/(ALOG10(SMAXY)-ALOG10(SMINY))
      ENDIF
C
C
C      (3) DECIDE THE GRIDDING RANGES
C
C
C  VERTICAL GRIDS
C
      IF(IAXIS.EQ.1 .OR. IAXIS.EQ.3) THEN
          NGRIDX = (SMAXX-SMINX)/GDELX+EPS
          GMINX  = SMINX
          GMAXX  = SMINX+FLOAT(NGRIDX)*GDELX
      ELSE
          FMIN   = UNIT 
          IF(SMINX.LT.UNIT) FMIN = -UNIT
          FMAX   = UNIT 
          IF(SMAXX.LT.UNIT) FMAX = -UNIT
          LMINX  = ALOG10(SMINX)+FMIN*EPS
          LMAXX  = ALOG10(SMAXX)+FMAX*EPS
          GMINX  = BASE**LMINX
          GMAXX  = BASE**LMAXX
          NGRIDX = LMAXX-LMINX
      ENDIF
C
C  HORIZONTAL GRIDS
C
      IF(IAXIS.EQ.1 .OR. IAXIS.EQ.2) THEN
          NGRIDY = (SMAXY-SMINY)/GDELY+EPS
          GMINY  = SMINY
          GMAXY  = SMINY+FLOAT(NGRIDY)*GDELY
      ELSE
          FMIN   = UNIT 
          IF(SMINY.LT.UNIT) FMIN = -UNIT
          FMAX   = UNIT 
          IF(SMAXY.LT.UNIT) FMAX = -UNIT
          LMINY  = ALOG10(SMINY)+FMIN*EPS
          LMAXY  = ALOG10(SMAXY)+FMAX*EPS
          GMINY  = BASE**LMINY
          GMAXY  = BASE**LMAXY
          NGRIDY = LMAXY-LMINY
      ENDIF
C
C
C      (4) DRAW THE GRID LINES
C
C
C  VERTICAL GRID LINES
C
      IF(IAXIS.EQ.1 .OR. IAXIS.EQ.2) THEN
          GRFY1 = ORGY+SCLY*(GMINY-SMINY)
          GRFY2 = ORGY+SCLY*(GMAXY-SMINY)
      ELSE
          GRFY1 = ORGY+SCLY*(ALOG10(GMINY)-ALOG10(SMINY))
          GRFY2 = ORGY+SCLY*(ALOG10(GMAXY)-ALOG10(SMINY))
      ENDIF
C
      DO 20 IGRID = 0 , NGRIDX
          IF(IAXIS.EQ.1 .OR. IAXIS.EQ.3) THEN
              GRFX = ORGX+SCLX*FLOAT(IGRID)*GDELX
              XBUF2(1) = GRFX
              YBUF2(1) = GRFY1
              XBUF2(2) = GRFX
              YBUF2(2) = GRFY2
              CALL GNLINE(XBUF2,YBUF2,2)
              IF(JPSOUT.GE.1) CALL PSLINE(IUTPS,XBUF2,YBUF2,2)
          ELSE
              DO 10 IFACT = 1 , 9
                  IF(IGRID.EQ.NGRIDX .AND. IFACT.GE.2) GO TO 10
                  VALUE = FLOAT(IFACT)*BASE**(IGRID+LMINX)
                  GRFX  = ORGX+SCLX*(ALOG10(VALUE)-ALOG10(SMINX))
                  XBUF2(1) = GRFX
                  YBUF2(1) = GRFY1
                  XBUF2(2) = GRFX
                  YBUF2(2) = GRFY2
                  CALL GNLINE(XBUF2,YBUF2,2)
                  IF(JPSOUT.GE.1) CALL PSLINE(IUTPS,XBUF2,YBUF2,2)
   10         CONTINUE
          ENDIF
   20 CONTINUE
C
C  HORIZONTAL GRID LINES
C
      IF(IAXIS.EQ.1 .OR. IAXIS.EQ.3) THEN
          GRFX1 = ORGX+SCLX*(GMINX-SMINX)
          GRFX2 = ORGX+SCLX*(GMAXX-SMINX)
      ELSE
          GRFX1 = ORGX+SCLX*(ALOG10(GMINX)-ALOG10(SMINX))
          GRFX2 = ORGX+SCLX*(ALOG10(GMAXX)-ALOG10(SMINX))
      ENDIF
C
      DO 40 IGRID = 0 , NGRIDY
          IF(IAXIS.EQ.1 .OR. IAXIS.EQ.2) THEN
              GRFY = ORGY+SCLY*FLOAT(IGRID)*GDELY
              XBUF2(1) = GRFX1
              YBUF2(1) = GRFY
              XBUF2(2) = GRFX2
              YBUF2(2) = GRFY
              CALL GNLINE(XBUF2,YBUF2,2)
              IF(JPSOUT.GE.1) CALL PSLINE(IUTPS,XBUF2,YBUF2,2)
          ELSE
              DO 30 IFACT = 1 , 9
                  IF(IGRID.EQ.NGRIDY .AND. IFACT.GE.2) GO TO 30
                  VALUE = FLOAT(IFACT)*BASE**(IGRID+LMINY)
                  GRFY  = ORGY+SCLY*(ALOG10(VALUE)-ALOG10(SMINY))
                  XBUF2(1) = GRFX1
                  YBUF2(1) = GRFY
                  XBUF2(2) = GRFX2
                  YBUF2(2) = GRFY
                  CALL GNLINE(XBUF2,YBUF2,2)
                  IF(JPSOUT.GE.1) CALL PSLINE(IUTPS,XBUF2,YBUF2,2)
   30         CONTINUE
          ENDIF
   40 CONTINUE
C
C
C      (5) WRITE INDICES ON THE AXES
C
C
C  X-DIR. AXIS
C
      IF(IAXIS.EQ.1 .OR. IAXIS.EQ.2) THEN
          GRFY = ORGY+SCLY*(       GMINY -       SMINY )-DSCLXY
      ELSE
          GRFY = ORGY+SCLY*(ALOG10(GMINY)-ALOG10(SMINY))-DSCLXY
      ENDIF
      GRFYS = GRFY+CSZAXS
      IORDX = 0
C
      IF(IAXIS.EQ.1 .OR. IAXIS.EQ.3) THEN
          ORDER  = AMAX1(ABS(SMINX),ABS(SMAXX))
          IORDX  = ALOG10(ORDER)
          IF(ORDER.LT.UNIT) IORDX = IORDX-1
          DO 50 IGRID = 0 , NGRIDX
              VALUE = SMINX+FLOAT(IGRID)*GDELX
              SCAL  = VALUE*BASE**(-IORDX)
              GRFX  = ORGX+SCLX*(VALUE-SMINX)-DSCLXX
              WRITE(BUF,'(F6.3)') SCAL
              TXSIZE = CSZAXS
              PTSIZE = CSZAXS
              CALL GNTEXT(GRFX,GRFY,BUF,6)
              IF(JPSOUT.GE.1) CALL PSTEXT(IUTPS,GRFX,GRFY,BUF,6)
   50     CONTINUE
      ELSE
          DO 60 IGRID = 0 , NGRIDX
              SCAL  = FLOAT(IGRID+LMINX)
              VALUE = BASE**SCAL
              GRFX  = ORGX+SCLX*(ALOG10(VALUE)-ALOG10(SMINX))-DSCLXX
              GRFXS = GRFX+2*CSZAXS
              WRITE(BUF,'(I2)') IBASE
              TXSIZE = CSZAXS
              PTSIZE = CSZAXS
              CALL GNTEXT(GRFX,GRFY,BUF,5)
              IF(JPSOUT.GE.1) CALL PSTEXT(IUTPS,GRFX,GRFY,BUF,5)
C
              WRITE(BUF,'(I2)') IGRID+LMINX
              TXSIZE = CSZSUF
              PTSIZE = CSZSUF
              CALL GNTEXT(GRFXS,GRFYS,BUF,5)
              IF(JPSOUT.GE.1) CALL PSTEXT(IUTPS,GRFXS,GRFYS,BUF,5)
   60     CONTINUE
      ENDIF
C
C  Y-DIR. AXIS
C
      IF(IAXIS.EQ.1 .OR. IAXIS.EQ.3) THEN
          GRFX = ORGX+SCLX*(       GMINX -       SMINX )-DSCLYX
      ELSE
          GRFX = ORGX+SCLX*(ALOG10(GMINX)-ALOG10(SMINX))-DSCLYX
      ENDIF
      GRFXS = GRFX+2*CSZAXS
      IORDY = 0
C
      IF(IAXIS.EQ.1 .OR. IAXIS.EQ.2) THEN
          ORDER  = AMAX1(ABS(SMINY),ABS(SMAXY))
          IORDY  = ALOG10(ORDER)
          IF(ORDER.LT.UNIT) IORDY = IORDY-1
          DO 70 IGRID = 0 , NGRIDY
              VALUE = SMINY+FLOAT(IGRID)*GDELY
              SCAL  = VALUE*BASE**(-IORDY)
              GRFY  = ORGY+SCLY*(VALUE-SMINY)-DSCLYY
              WRITE(BUF,'(F6.3)') SCAL
              TXSIZE = CSZAXS
              PTSIZE = CSZAXS
              CALL GNTEXT(GRFX,GRFY,BUF,6)
              IF(JPSOUT.GE.1) CALL PSTEXT(IUTPS,GRFX,GRFY,BUF,6)
   70     CONTINUE
      ELSE
          DO 80 IGRID = 0 , NGRIDY
              SCAL  = FLOAT(IGRID+LMINY)
              VALUE = BASE**SCAL
              GRFY  = ORGY+SCLY*(ALOG10(VALUE)-ALOG10(SMINY))-DSCLYY
              GRFYS = GRFY+CSZAXS
              WRITE(BUF,'(I2)') IBASE 
              TXSIZE = CSZAXS
              PTSIZE = CSZAXS
              CALL GNTEXT(GRFX,GRFY,BUF,5)
              IF(JPSOUT.GE.1) CALL PSTEXT(IUTPS,GRFX,GRFY,BUF,5)
C
              WRITE(BUF,'(I2)') IGRID+LMINY
              TXSIZE = CSZSUF
              PTSIZE = CSZSUF
              CALL GNTEXT(GRFXS,GRFYS,BUF,5)
              IF(JPSOUT.GE.1) CALL PSTEXT(IUTPS,GRFXS,GRFYS,BUF,5)
   80     CONTINUE
      ENDIF
C
C
C      (6) WRITE TITLES  ON THE AXES
C
C
C  X-DIR. AXIS
C
      IF(IAXIS.EQ.1 .OR. IAXIS.EQ.2) THEN
          GRFY = ORGY+SCLY*(       GMINY -       SMINY )-DTTLXY
      ELSE
          GRFY = ORGY+SCLY*(ALOG10(GMINY)-ALOG10(SMINY))-DTTLXY
      ENDIF
C
      IF(IAXIS.EQ.1 .OR. IAXIS.EQ.3) THEN
          GRFX = ORGX+SCLX*(GMAXX-SMINX)-DTTLXX
      ELSE
          GRFX = ORGX+SCLX*(ALOG10(GMAXX)-ALOG10(SMINX))-DTTLXX
      ENDIF
C
      WRITE(BUF,'(A10)') TITLEX
      TXSIZE = CSZAXS
      PTSIZE = CSZAXS
      CALL GNTEXT(GRFX,GRFY,BUF,NTITLE)
      IF(JPSOUT.GE.1) CALL PSTEXT(IUTPS,GRFX,GRFY,BUF,NTITLE)
C
      IF(IORDX.NE.0) THEN
          GRFX = GRFX+CSZAXS*NTITLE
C
          WRITE(BUF,'(7H(*10  ))')
          TXSIZE = CSZAXS
          PTSIZE = CSZAXS
          CALL GNTEXT(GRFX,GRFY,BUF,7)
          IF(JPSOUT.GE.1) CALL PSTEXT(IUTPS,GRFX,GRFY,BUF,7)
C
          GRFX = GRFX+CSZAXS*4
          GRFY = GRFY+CSZAXS
C
          WRITE(BUF,'(I2)') IORDX
          TXSIZE = CSZSUF
          PTSIZE = CSZSUF
          CALL GNTEXT(GRFX,GRFY,BUF,2)
          IF(JPSOUT.GE.1) CALL PSTEXT(IUTPS,GRFX,GRFY,BUF,2)
      ENDIF
C
C  Y-DIR. AXIS
C
      IF(IAXIS.EQ.1 .OR. IAXIS.EQ.3) THEN
          GRFX = ORGX+SCLX*(       GMINX -       SMINX )-DTTLYX
      ELSE
          GRFX = ORGX+SCLX*(ALOG10(GMINX)-ALOG10(SMINX))-DTTLYX
      ENDIF
C
      IF(IAXIS.EQ.1 .OR. IAXIS.EQ.2) THEN
          GRFY = ORGY+SCLY*(GMAXY-SMINY)+DTTLYY
      ELSE
          GRFY = ORGY+SCLY*(ALOG10(GMAXY)-ALOG10(SMINY))+DTTLYY
      ENDIF
C
      WRITE(BUF,'(A10)') TITLEY
      TXSIZE = CSZAXS
      PTSIZE = CSZAXS
      CALL GNTEXT(GRFX,GRFY,BUF,NTITLE)
      IF(JPSOUT.GE.1) CALL PSTEXT(IUTPS,GRFX,GRFY,BUF,NTITLE)
C
      IF(IORDY.NE.0) THEN
          GRFX = GRFX+CSZAXS*NTITLE
C
          WRITE(BUF,'(7H(*10  ))')
          TXSIZE = CSZAXS
          PTSIZE = CSZAXS
          CALL GNTEXT(GRFX,GRFY,BUF,7)
          IF(JPSOUT.GE.1) CALL PSTEXT(IUTPS,GRFX,GRFY,BUF,7)
          GRFX = GRFX+CSZAXS*4
          GRFY = GRFY+CSZAXS
C
          WRITE(BUF,'(I2)') IORDY
          TXSIZE = CSZSUF
          PTSIZE = CSZSUF
          CALL GNTEXT(GRFX,GRFY,BUF,2)
          IF(JPSOUT.GE.1) CALL PSTEXT(IUTPS,GRFX,GRFY,BUF,2)
      ENDIF
C
C
      RETURN
      END
