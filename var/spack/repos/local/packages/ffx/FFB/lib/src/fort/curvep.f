C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    CURVEP                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE CURVEP(IMODE,NPOINT,XIN,XOUT,NTIME,IUT0,IERR)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION XIN(NTIME),XOUT(NTIME)
C
      PARAMETER ( M = 11 , MPOINT = 12 )
      DIMENSION LPOINT(M),ISTAND(M,2),IWEIT(0:MPOINT,M,2)
C
      DATA LPOINT /  5, 7,  9, 11, 13,  15, 17,  19,  21,  23,  25 /
      DATA ISTAND / 35,21,231,429,143,1105,323,2261,3059, 805,5175,
     &              10,28, 60,110,182, 280,408, 570, 770,1012,1300 /
C
      DATA ((IWEIT(IPOINT,I,1), IPOINT = 0, MPOINT), I = 1, M)
     &/  17,  12,  -3,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &    7,   6,   3,  -2,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &   59,  54,  39,  14, -21,   0,   0,   0,   0,   0,   0,   0,   0,
     &   89,  84,  69,  44,   9, -36,   0,   0,   0,   0,   0,   0,   0,
     &   25,  24,  21,  16,   9,   0, -11,   0,   0,   0,   0,   0,   0,
     &  167, 162, 147, 122,  87,  42, -13, -78,   0,   0,   0,   0,   0,
     &   43,  42,  39,  34,  27,  18,   7,  -6, -21,   0,   0,   0,   0,
     &  269, 264, 249, 224, 189, 144,  89,  24, -51,-136,   0,   0,   0,
     &  329, 324, 309, 284, 249, 204, 149,  84,   9, -76,-171,   0,   0,
     &   79,  78,  75,  70,  63,  54,  43,  30,  15,  -2, -21, -42,   0,
     &  467, 462, 447, 422, 387, 342, 287, 222, 147,  62, -33,-138,-253/
C
      DATA ((IWEIT(IPOINT,I,2), IPOINT = 0, MPOINT), I = 1, M)
     &/   0,   1,   2,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &    0,   1,   2,   3,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     &    0,   1,   2,   3,   4,   0,   0,   0,   0,   0,   0,   0,   0,
     &    0,   1,   2,   3,   4,   5,   0,   0,   0,   0,   0,   0,   0,
     &    0,   1,   2,   3,   4,   5,   6,   0,   0,   0,   0,   0,   0,
     &    0,   1,   2,   3,   4,   5,   6,   7,   0,   0,   0,   0,   0,
     &    0,   1,   2,   3,   4,   5,   6,   7,   8,   0,   0,   0,   0,
     &    0,   1,   2,   3,   4,   5,   6,   7,   8,   9,   0,   0,   0,
     &    0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,   0,   0,
     &    0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,   0,
     &    0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12/
C
      CHARACTER*60 ERMSGB
     & /' ## SUBROUTINE CURVEP: FATAL      ERROR OCCURENCE; RETURNED' /
      CHARACTER*60 EREXP1
     & /' SPECIFIED OPERATION MODE IS NOT SUPPORTED                 ' /
C
C
C      SMOOTH/DIFFERENTIATE GIVEN DATA BY POLYNOMIAL CURVE FITTING
C
C
C      NOTES; OUTPUT DATA WILL BE UNDEFINED FOR NPOINT/2 POINTS
C            FROM BOTH THE EDGES.
C
C      NOTES; PASSED ARGUMENT 'NPOINT' WILL BE MODIFIED IF IMPROPERLY
C            SET.
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          IMODE       ; SPECIFIES TYPE OF OPERATIONS
C                   1 --- SMOOTHING
C                   2 --- DIFFERENTIATION
C          NPOINT      ; NUMBER OF POINTS USED FOR CURVE FITTING
C                       ( MUST BE ODD NUMBER IN THE RANGE 5 TO 25 )
C          XIN  (ITIME); INPUT DATA
C          NTIME       ; NUMBER OF TOTAL TIME STEPS
C          IUT0        ; FILE NUMBER TO WRITE  ERROR MESSAGE
C
C       (2) OUTPUT
C          XOUT (ITIME); OUTPUT DATA
C          IERR        ; RETURN CODE WHOSE VALUE WILL BE EITHER
C            0       --- INDICATING SUCCESSFUL TERMINATION
C        OR  1       --- INDICATING OCCURENCE OF SOME ERROR CONDITIONS
C
C
      IERR = 0
C
C CHECK PASSED PARAMETERS
C
      IF(IMODE.NE.1 .AND. IMODE.NE.2) THEN
          WRITE(IUT0,*) ERMSGB
          WRITE(IUT0,*) EREXP1
          IERR = 1
          RETURN
      ENDIF
C
C DETERMINE EXACT NUMBER OF POINTS USED FOR CURVE FITTING
C
      DO 10 I = 1 , M
          IF(LPOINT(I).GE.NPOINT) THEN
              NPOINT = LPOINT(I)
              N      = I
              GO TO 20
          ENDIF
   10 CONTINUE
      NPOINT = LPOINT(M)
      N      = M
   20 CONTINUE
C
C DO POLYNOMIAL CURVE FITTING
C
      DO 100 ITIME = NPOINT/2+1 , NTIME-NPOINT/2
          XOUT(ITIME) = IWEIT(0,N,IMODE)*XIN(ITIME)
  100 CONTINUE
C
      DO 220 IPOINT = 1 , NPOINT/2
          IF(IMODE.EQ.1) THEN
          DO 200 ITIME = NPOINT/2+1 , NTIME-NPOINT/2
              XOUT(ITIME) = XOUT(ITIME)
     &      +IWEIT(IPOINT,N,IMODE)*(XIN(ITIME+IPOINT)+XIN(ITIME-IPOINT))
  200     CONTINUE
          ELSE
          DO 210 ITIME = NPOINT/2+1 , NTIME-NPOINT/2
              XOUT(ITIME) = XOUT(ITIME)
     &      +IWEIT(IPOINT,N,IMODE)*(XIN(ITIME+IPOINT)-XIN(ITIME-IPOINT))
  210     CONTINUE
          ENDIF
  220 CONTINUE
C
      DO 300 ITIME = NPOINT/2+1 , NTIME-NPOINT/2
          XOUT(ITIME) = XOUT(ITIME)/FLOAT(ISTAND(N,IMODE))
  300 CONTINUE
C
      RETURN
      END
