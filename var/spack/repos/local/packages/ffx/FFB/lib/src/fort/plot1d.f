C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    PLOT1D                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE PLOT1D(JPSOUT,IUTPS,JPLOUT,FILEPL,IUTPL,
     *                  IMODE,XPLT,YPLT,JPLT,NPLT,
     *                  ORGX,ORGY,SCLX,SCLY,SXMIN,SYMIN,ICLPLT,
     *                  ICDPLT,CSZPLT)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION XPLT(NPLT),YPLT(NPLT),JPLT(NPLT)
C
      CHARACTER*(*) FILEPL
C
#ifdef VOS
      INCLUDE (GN)
#else
      INCLUDE 'gn.h'
#endif
C
      DIMENSION XBUF (10),YBUF (10)
      DIMENSION XBUF2(10),YBUF2(10)
C
C
C      PLOT GIVEN X-Y PAIRS ON A X-Y GRAPH
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
C          JPLOUT      ; PLOT DATA  FILE WILL BE OUTPUT IF SET TO 1
C          FILEPL      ; FILE NAME   FOR PLOT DATA FILE
C          IUTPL       ; FILE NUMBER TO ACCESS PLOT DATA  FILE
C          IMODE       ; SPECIFIES THE PLOTTING MODE AS FOLLOWS
C                   1 --- PLOT THE PAIRS BY MARKS
C                   2 --- PLOT THE PAIRS BY MARKS CONNECTED BY LINES
C          XPLT  (IPLT); X-COORDINATES OF THE PAIRS
C          YPLT  (IPLT); Y-COORDINATES OF THE PAIRS
C          JPLT  (IPLT); THE VALIDITY FLAGS
C                    (IF JPLT(IPLT) IS 0, THE PAIR WILL NOT BE PLOTTED.)
C          NPLT        ; THE NUMBER OF GIVEN PAIRS
C
C          ORGX        ; X-DIR. GRAPHIC ORIGIN
C          ORGY        ; Y-DIR. GRAPHIC ORIGIN
C          SCLX        ; X-DIR. SCALING FACTOR
C          SCLY        ; Y-DIR. SCALING FACTOR
C          SXMIN       ; X-DIR. MIN. SCALE
C          SYMIN       ; Y-DIR. MIN. SCALE
C
C          ICLPLT      ; COLOR INDEX    FOR PLOTTING THE PAIRS
C          ICDPLT      ; SYMBOL CODE    FOR PLOTTING THE PAIRS
C          CSZPLT      ; CHARACTER SIZE FOR PLOTTING THE PAIRS
C          THEPLT      ; TILTED ANGLE   FOR PLOTTING THE PAIRS
C
C       (2) OUTPUT
C          NONE
C
C
      CALL GNCSET(ICLPLT)
      IF(JPSOUT.EQ.1) CALL PSGRAY(IUTPS,ICLPLT)
      IF(JPSOUT.EQ.2) CALL PSCSET(IUTPS,ICLPLT)
C
      LNTYPE = GLSOLD
      PLTYPE = GLSOLD
C
      LNWDTH = GLNORM
      PLWDTH = GLNORM
C
      MKTYPE = ICDPLT
      PMTYPE = ICDPLT
C
      MKSIZE = CSZPLT
      PKSIZE = CSZPLT
C
#ifdef VOS
      IF(JPLOUT.EQ.1) OPEN(IUTPL,FILE=FILEPL,FORM='FORMATTED',
     &                     ACTION='WRITE')
#else
      IF(JPLOUT.EQ.1) OPEN(IUTPL,FILE=FILEPL,FORM='FORMATTED')
#endif
C
      DO 10 IPLT = 1 , NPLT
          IF(JPLT(IPLT).NE.0) THEN
              POINTX = ORGX+SCLX*(XPLT(IPLT)-SXMIN)
              POINTY = ORGY+SCLY*(YPLT(IPLT)-SYMIN)
              IF(IMODE.EQ.2 .AND. IPLT.GE.2 .AND. JPLT(IPLT-1).NE.0)THEN
                  XBUF(2) = POINTX
                  YBUF(2) = POINTY
                  CALL GNLINE(XBUF,YBUF,2)
                  IF(JPSOUT.GE.1) CALL PSLINE(IUTPS,XBUF,YBUF,2)
              ENDIF
              XBUF (1) = POINTX
              YBUF (1) = POINTY
              XBUF2(1) = POINTX
              YBUF2(1) = POINTY
              CALL GNMARK(XBUF2,YBUF2,1)
              IF(JPSOUT.GE.1) CALL PSMARK(IUTPS,XBUF2,YBUF2,1)
              IF(JPLOUT.EQ.1) WRITE(IUTPL,*) XPLT(IPLT),YPLT(IPLT)
          ENDIF
   10 CONTINUE
      IF(JPLOUT.EQ.1) CLOSE(IUTPL)
C
C
      RETURN
      END
