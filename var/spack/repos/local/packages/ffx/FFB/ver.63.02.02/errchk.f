      SUBROUTINE ERRCHK(IUT6,IPART,NUM,LERR,IERR)
      IMPLICIT NONE
      INTEGER*4 IUT6,IPART,NUM,LERR(NUM),IERR
      INTEGER*4 I
      REAL*4    FERR,FERRA
C
      IERR=0
      IF(NUM.EQ.0) RETURN
C
      WRITE(IUT6,*) 'CHECKING ERR FLAG: 0:OK, NOT-0:FAIL'
      WRITE(IUT6,'(10I4)') (LERR(I),I=1,NUM)
C
      DO 1000 I=1,NUM
         IF(LERR(I).NE.0) IERR=1
 1000 CONTINUE
C
      IF(IPART.GE.1) THEN
          FERR = FLOAT(IERR)
          CALL DDCOM2(FERR, FERRA)
          IF(FERRA.GT.0.5E0) THEN
              IERR = 1
          ENDIF
      ENDIF
C
      RETURN
      END

