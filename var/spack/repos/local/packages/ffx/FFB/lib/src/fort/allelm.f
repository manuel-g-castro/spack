C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    ALLELM                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE ALLELM(ISTART,IENE,NEE,NE,MEE,IEFLAG,IUT0,IERR)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION IENE(MEE,NE),NEE(NE),IEFLAG(NE)
C
      CHARACTER*60 ERMSGB
     & / ' ## SUBROUTINE ALLELM: FATAL      ERROR OCCURENCE; RETURNED' /
      CHARACTER*60 EREXP1
     & / ' AN ILLEGAL STARTGING ELEMENT NUMBER WAS SPECIFIED         ' /
C
C
C      RECURSIVELY SEARCH NEIBERING ELEMENTS FROM A SPECIFIED ELEMENT
C     UNTIL NO MORE NEIBERING ELEMENT CAN BE FOUND
C         ( 2-D , 3-D CALCULATION & GRAPHICS )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          ISTART      ; ELEMENT NUMBER TO START NEIBERING SEARCH WITH
C          IENE(IEE,IE); ADJACENT ELEMENT NUMBER TO ELEMENT IE
C          NEE     (IE); NUMBER OF ADJACENT ELEMENTS TO ELEMENT IE
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          MEE         ; THE FIRST DIMENSION OF ARRAY IENE
C          IUT0        ; FILE NUMBER TO REPORT ERROR OCCURENCE
C
C       (2) OUTPUT
C          IEFLAG  (IE); ELEMENT FLAG WHICH INDICATES WHETHER OR NOT
C                       TO BELONG TO THE SAME ELEMENT GROUP AS THE
C                       STARTING ELEMENT DOES
C                   0 --- DOES NOT BELONG TO THE SAME ELEMENT GROUP
C                   1 ---          BELONG TO THE SAME ELEMENT GROUP
C          IERR        ; RETURN CODE TO REPORT ERROR OCCURENCE
C                   0 --- NORMAL TERMINATION
C                   1 --- A FATAL ERROR HAS OCCURED
C
C
      IERR = 0
C
C
      IF(ISTART.LT.1 .OR. ISTART.GT.NE) THEN
          WRITE(IUT0,*) ERMSGB
          WRITE(IUT0,*) EREXP1
          IERR = 1
          RETURN
      ENDIF
C
C
      DO 100 IE = 1 , NE
          IEFLAG(IE) = 0
  100 CONTINUE
      IEFLAG(ISTART) = -1
C
C
  110 CONTINUE
          DO 120 IE = 1 , NE
              IF(IEFLAG(IE).EQ.-1) THEN
                  IESRCH = IE
                  GO TO 130
              ENDIF
  120     CONTINUE
C
          RETURN
C
  130     CONTINUE
          DO 140 IEE = 1 , NEE(IESRCH)
              IENEIB = IENE(IEE,IESRCH)
              IF(IEFLAG(IENEIB).EQ.0) IEFLAG(IENEIB) = -1
  140     CONTINUE
          IEFLAG(IESRCH) = 1
      GO TO 110
C
C
      END
