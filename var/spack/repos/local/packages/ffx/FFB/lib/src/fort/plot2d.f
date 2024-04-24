C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    PLOT2D                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE PLOT2D(JPSOUT,IUTPS,IAXIS,IMODE,INTPLT,
     *                  SMINX,SMAXX,SMINY,SMAXY,XL,YL,NL,
     *                  SCLX,SCLY,ORGX,ORGY,ICLPLT,ICDPLT,CSZPLT)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION XL(NL),YL(NL)
C
#ifdef VOS
      INCLUDE (GN)
#else
      INCLUDE 'gn.h'
#endif
C
      DIMENSION XBUF (1),YBUF (1)
      DIMENSION XBUF2(2),YBUF2(2)
C
C
C      PLOT X-Y PAIRS
C         ( 2-D GRAPHICS )
C
C
C     NOTE 1 ; CALLING GENERIC GRAPHIC AND POSTSCRIPT INTERFACES.
C     NOTE 2 ; IF A GIVEN POINT IS OUT OF THE SCALE RANGE SPECIFIED,
C             THE POINT WILL NOT BE PLOTTED.
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
C          IMODE       ; SPECIFIES THE PLOTTING MODE AS FOLLOWS
C                   1 --- PLOT THE PAIRS BY MARKS
C                   2 --- PLOT THE PAIRS BY MARKS AND LINK THEM BY LINES
C                        IN THE ORDER THAT THEY ARE STORED IN THE ARRAYS
C                   3 --- ONLY LINK THEM BY LINES
C                        IN THE ORDER THAT THEY ARE STORED IN THE ARRAYS
C          INTPLT      ; PLOTTING INTERVAL
C
C          SMINX       ; X-DIR. MIN. SCALE
C          SMAXX       ; X-DIR. MAX. SCALE
C          SMINY       ; Y-DIR. MIN. SCALE
C          SMAXY       ; Y-DIR. MAX. SCALE
C
C          XL      (IL); X-DIR. COORDINATES OF THE PAIRS
C          YL      (IL); Y-DIR. COORDINATES OF THE PAIRS
C          NL          ; THE NUMBER         OF THE PAIRS
C
C          SCLX        ; X-DIR. SCALING FACTOR
C          SCLY        ; Y-DIR. SCALING FACTOR
C
C          ORGX        ; X-DIR. GRAPHIC ORIGIN
C          ORGY        ; Y-DIR. GRAPHIC ORIGIN
C          ICLPLT      ; COLOR INDEX TO BE USED IN THIS SUBROUTINE
C          ICDPLT      ; CODE NUMBER    FOR THE MARK
C          CSZPLT      ; CHARACTER SIZE FOR THE MARK
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
      LNWDTH = GLBOLD
      PLWDTH = GLBOLD
C
      MKTYPE = ICDPLT
      PMTYPE = ICDPLT
C
      MKSIZE = CSZPLT
      PKSIZE = CSZPLT
C
      JONCE = 0
      DO 10 IL = 1 , NL
          IF(MOD(IL,INTPLT).NE.0) GO TO 10
          IF(XL(IL).GE.SMINX .AND. XL(IL).LE.SMAXX .AND.
     &       YL(IL).GE.SMINY .AND. YL(IL).LE.SMAXY) THEN
C
              IF(IAXIS.EQ.1 .OR. IAXIS.EQ.3) THEN
                  GX = ORGX+SCLX*(       XL(IL) -       SMINX )
              ELSE
                  GX = ORGX+SCLX*(ALOG10(XL(IL))-ALOG10(SMINX))
              ENDIF
C
              IF(IAXIS.EQ.1 .OR. IAXIS.EQ.2) THEN
                  GY = ORGY+SCLY*(       YL(IL) -       SMINY )
              ELSE
                  GY = ORGY+SCLY*(ALOG10(YL(IL))-ALOG10(SMINY))
              ENDIF
C
              IF(IMODE.EQ.1 .OR. IMODE.EQ.2) THEN
                  XBUF (1) = GX
                  YBUF (1) = GY
                  CALL GNMARK(XBUF,YBUF,1)
                  IF(JPSOUT.GE.1) CALL PSMARK(IUTPS,XBUF,YBUF,1)
              ENDIF
              IF(IMODE.EQ.2 .OR. IMODE.EQ.3) THEN
                  XBUF2(2) = GX
                  YBUF2(2) = GY
                  IF(JONCE.EQ.1) CALL GNLINE(XBUF2,YBUF2,2)
                  IF(JPSOUT.GE.1 .AND. JONCE.EQ.1)
     &                CALL PSLINE(IUTPS,XBUF2,YBUF2,2)
                  XBUF2(1) = GX
                  YBUF2(1) = GY
                  JONCE = 1
              ENDIF
          ENDIF
   10 CONTINUE
      LNWDTH = GLNORM
      PLWDTH = GLNORM
C
C
      RETURN
      END
