C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    RANDM1                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE RANDM1(IMODE,LTABLE,IARY,FARY,MAX,MAX2,
     *                  IWRK,FWRK,IUT0,IERR)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION LTABLE(MAX),IARY(MAX2,MAX),FARY(MAX2,MAX),
     1          IWRK(MAX),FWRK(MAX)
C
      CHARACTER*60 ERMSGB
     & / ' ## SUBROUTINE RANDM1: FATAL      ERROR OCCURENCE; RETURNED' /
      CHARACTER*60 EREXP1
     & / ' AN ILLEGAL VALUE WAS SPECIFIED FOR CONTROL PARAMETER IMODE' /
C
C
C      PERMUTE AN ARRAY
C
C
C     ARGUMENT LISTINGS
C          IMODE       ; SPECIFIES TARGET ARRAY & PERMUTATION DIRECTION
C                   1 --- FORWARD -PERMUTE INTEGER ARRAY
C                   2 --- FORWARD -PERMUTE FLOAT   ARRAY
C                  -1 --- BACKWARD-PERMUTE INTEGER ARRAY
C                  -2 --- BACKWARD-PERMUTE FLOAT   ARRAY
C          LTABLE   (I); PERMUTATION TABLE
C          MAX         ; NUMBER OF ELEMENTS        IN TARGET ARRAY
C          MAX2        ; ADJUSTABLE DIMENSION SIZE OF TARGET ARRAY
C          IUT0        ; FILE NUMBER TO REPORT ERROR OCCURENCE
C
C       (2) OUTPUT
C          IERR        ; RETURN CODE TO REPORT ERROR OCCURENCE
C                   0 --- NORMAL TERMINATION
C                   1 --- A FATAL ERROR HAS OCCURED
C
C       (3) INPUT-OUTPUT
C          IARY   (J,I); INTEGER ARRAY TO BE PERMUTED
C          FARY   (J,I); FLOAT   ARRAY TO BE PERMUTED
C
C       (4) WORK
C          IWRK     (I); WORK AREA STORING INTEGER ARRAY CONTENTS
C          FWRK     (I); WORK AREA STORING FLOAT   ARRAY CONTENTS
C
C
      IERR = 0
C
C
C CHECK SPECIFIED CONTROL PARAMETER
C
C
      IF(IMODE.NE. 1 .AND. IMODE.NE. 2 .AND.
     &   IMODE.NE.-1 .AND. IMODE.NE.-2 ) THEN
          WRITE(IUT0,*) ERMSGB
          WRITE(IUT0,*) EREXP1
          IERR = 1
          RETURN
      ENDIF
C
      DO 610 J = 1 , MAX2
C
C
C STORE CONTENTS OF ARRAY
C
C     
          IF(IMODE.EQ.1 .OR. IMODE.EQ.-1) THEN
              DO 100 I = 1 , MAX
                  IWRK(I) = IARY(J,I)
  100         CONTINUE
          ELSE
              DO 200 I = 1 , MAX
                  FWRK(I) = FARY(J,I)
  200         CONTINUE
          ENDIF
C
C
C PERFORM FORWARD-PERMUTATION
C
C     
          IF(IMODE.EQ.1) THEN
C*$*ASSERT PERMUTATION ( LTABLE )
              DO 300 I = 1 , MAX
                  IARY(J,LTABLE(I)) = IWRK(I)
  300         CONTINUE
          ENDIF
C
          IF(IMODE.EQ.2) THEN
C*$*ASSERT PERMUTATION ( LTABLE )
              DO 400 I = 1 , MAX
                  FARY(J,LTABLE(I)) = FWRK(I)
  400         CONTINUE
          ENDIF
C 
C
C PERFORM BACKWARD-PERMUTATION
C
C     
          IF(IMODE.EQ.-1) THEN
              DO 500 I = 1 , MAX
                  IARY(J,I) = IWRK(LTABLE(I))
  500         CONTINUE
          ENDIF
C
          IF(IMODE.EQ.-2) THEN
              DO 600 I = 1 , MAX
                  FARY(J,I) = FWRK(LTABLE(I))
  600         CONTINUE
          ENDIF
C
  610 CONTINUE
C
C
      RETURN
      END
