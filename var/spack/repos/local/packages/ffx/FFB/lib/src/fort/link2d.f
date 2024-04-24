C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    LINK2D                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE LINK2D(LSIDE,NS,ISTART,INEXT,IEND,MLINK,
     *                  LINK,NLINK,IUT0,IERR,IWRN)
      DIMENSION LSIDE(2,NS),LINK(MLINK)
C
      CHARACTER*60 ERMSG
      CHARACTER*60 EREXP1
      CHARACTER*60 WRMSG
      CHARACTER*60 WREXP1
      CHARACTER*60 WREXP2
      CHARACTER*60 WREXP3
C
      DATA ERMSG
     & /' *** SUBROUTINE LINK2D REPORTS A FATAL ERROR OCCURENCE ***' /
      DATA EREXP1
     & /' NON-ENCLOSED SET OF BORDER LINE IS INCLUDED IN LSIDE(I,IS)' /
C
      DATA WRMSG
     & /' *** SUBROUTINE LINK2D ISSUES WARNING ***' /
      DATA WREXP1
     & /' GIVEN PAIR OF ISTART AND INEXT CONSTITUTES NO BOUNDARY SIDE'/
      DATA WREXP2
     & /' LINK2D MEETS ISTART SOONER THAN MEETING IEND' /
      DATA WREXP3
     & /' NUMBER OF LINKED NODES HAS EXCEEDED THE LIMIT' /
C
C
C      LINK BOUNDARY NODES BETWEEN ISTART AND IEND
C         ( 2-D CALCULATION & GRAPHICS )
C
C
C     NOTE 1 ; THIS SUBROUTINE IS AVAILABLE FOR ANY KIND OF ELEMENTS
C             ( INCLUDING A MIXTURE OF DIFFERENT KINDS OF ELEMENTS ).
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          LSIDE(I,IS) ; BOUNDARY SIDES
C          NS          ; NUMBER OF BOUNDARY SIDES
C          ISTART      ; NODE NUMBER OF THE LINK STARTING NODE
C          INEXT       ; NODE NUMBER OF THE ADJACENT NODE TO ISTART
C          IEND        ; NODE NUMBER OF THE LINK ENDING   NODE
C          MLINK       ; THE DIMENSION OF THE ARRAY LINK(ILINK)
C          IUT0        ; DEVICE NUMBER TO REPORT ERROR OCCURENCE
C
C       (2) OUTPUT
C          LINK(ILINK) ; LINKED BOUNDARY NODES BETWEEN ISTART AND IEND
C          NLINK       ; NUMBER OF LINKED BOUNDARY NODES
C          IERR        ; RETURN CODE TO REPORT ERROR OCCURENCE
C                   0 --- NORMAL TERMINATION
C                   1 --- A FATAL ERROR HAS OCCURED
C          IWRN        ; RETURN CODE TO ISSUE  WARNING
C                   0 --- NORMAL TERMINATION
C                   1 --- LINK FAILURE
C
      IERR  = 0
      IWRN  = 0
      NLINK = 0
C
C
C      EXCEPTION
C
C
      IF(ISTART.EQ.INEXT .AND. INEXT.EQ.IEND) THEN
          NLINK = NLINK+1
          IF(NLINK.GT.MLINK) THEN
              WRITE(IUT0,*) WRMSG
              WRITE(IUT0,*) WREXP3
              IWRN = 1
              RETURN
          ENDIF
          LINK(NLINK) = IEND
          RETURN
      ENDIF
C
C
C      INITIAL SETTING
C
C
      DO 100 IS = 1 , NS
          IF(LSIDE(1,IS).EQ.ISTART .AND. LSIDE(2,IS).EQ.INEXT  .OR.
     &       LSIDE(2,IS).EQ.ISTART .AND. LSIDE(1,IS).EQ.INEXT) THEN
              ISFNT = IS
              NLINK = NLINK+1
              IF(NLINK.GT.MLINK) THEN
                  WRITE(IUT0,*) WRMSG
                  WRITE(IUT0,*) WREXP3
                  IWRN = 1
                  RETURN
              ENDIF
              LINK(NLINK) = ISTART
C
              NLINK = NLINK+1
              IF(NLINK.GT.MLINK) THEN
                  WRITE(IUT0,*) WRMSG
                  WRITE(IUT0,*) WREXP3
                  IWRN = 1
                  RETURN
              ENDIF
              LINK(NLINK) = INEXT
              IF(LINK(NLINK) .EQ. IEND) RETURN
              GO TO  200
          ENDIF
  100 CONTINUE
      WRITE(IUT0,*) WRMSG
      WRITE(IUT0,*) WREXP1
      IWRN = 1
      RETURN
  200 CONTINUE
C
C
C      LINKAGE CONTINUEING UNTIL MEETING IEND
C
C
  300 CONTINUE
      DO 400 IS = 1 , NS
          IF(IS.EQ.ISFNT) GO TO 400
          JLINK = 0
          IF(LSIDE(1,IS).EQ.LINK(NLINK)) JLINK = 2
          IF(LSIDE(2,IS).EQ.LINK(NLINK)) JLINK = 1
          IF(JLINK.GE.1) THEN
              ISFNT = IS
              NLINK = NLINK+1
              IF(NLINK.GT.MLINK) THEN
                  WRITE(IUT0,*) WRMSG
                  WRITE(IUT0,*) WREXP3
                  IWRN = 1
                  RETURN
              ENDIF
              LINK(NLINK) = LSIDE(JLINK,ISFNT)
C
              IF(LINK(NLINK) .EQ. IEND  ) RETURN
              IF(LINK(NLINK) .EQ. ISTART) THEN
                  WRITE(IUT0,*) WRMSG
                  WRITE(IUT0,*) WREXP2
                  IWRN = 1
                  RETURN
              ENDIF
              GO TO 300
          ENDIF
  400 CONTINUE
      WRITE(IUT0,*) ERMSG
      WRITE(IUT0,*) EREXP1
      IERR = 1
      RETURN
C
C
      END
