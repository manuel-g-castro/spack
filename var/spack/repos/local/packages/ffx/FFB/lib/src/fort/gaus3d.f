C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    GAUS3D                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE GAUS3D(IGAUSS,MPOINT,GP,EP,TP,WP,NPOINT,IUT0,IERR)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION GP(MPOINT),EP(MPOINT),TP(MPOINT),WP(MPOINT)
      PARAMETER ( MGAUSS = 4 )
      DIMENSION P(MGAUSS),H(MGAUSS)
C
      CHARACTER*60 ERMSGB
     & /' ## SUBROUTINE GAUS3D: FATAL      ERROR OCCURENCE; RETURNED' /
      CHARACTER*60 EREXP1
     & /' SPECIFIED GAUSS POINT  IS NOT CURRENTLY SUPPORTED         ' /
      CHARACTER*60 EREXP2
     & /' THE DIMENSION OF ARRAY GP , EP , TP , WP IS NOT SUFFICIENT' /
C
C
C      SET GAUSS POINTS, AND CORRESPONDING WEIGHTING FUNCTIONS
C         ( 3-D CALCULATION : SINGLE WORD VERSION )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          IGAUSS      ; SPECIFIES NUMBER OF GAUSS POINTS IN ONE DIR.
C          MPOINT      ; THE DIMENSION OF THE ARRAY GP , EP , TP , WP
C          IUT0        ; FILE NUMBER TO REPORT ERROR OCCURENCE
C
C       (2) OUTPUT
C          GP(IPOINT)  ; GZAI-COORDINATE    OF GAUSS POINTS
C          EP(IPOINT)  ; EATA-COORDINATE    OF GAUSS POINTS
C          TP(IPOINT)  ; TETA-COORDINATE    OF GAUSS POINTS
C          WP(IPOINT)  ; WEIGHTING FUNCTION OF GAUSS POINTS
C          NPOINT      ; NUMBER             OF GAUSS POINTS
C          IERR        ; RETURN CODE TO REPORT ERROR OCCURENCE
C                   0 --- NORMAL TERMINATION
C                   1 --- A FATAL ERROR HAS OCCURED
C
C
      IERR = 0
C
      IF(IGAUSS.LT.1 .OR. IGAUSS.GT.4) THEN
          WRITE(IUT0,*) ERMSGB
          WRITE(IUT0,*) EREXP1
          IERR = 1
          RETURN
      ENDIF
C
      GO TO ( 100 , 200 , 300 , 400 ) , IGAUSS
C
C      1 POINT
C
  100 CONTINUE
          P(1) =  0.E0
          H(1) =  2.E0
      GO TO 500
C
C      2 POINTS
C
  200 CONTINUE
          P(1) = -0.577350E0
          H(1) = 1.E0
          P(2) =  0.577350E0
          H(2) = 1.E0
      GO TO 500
C
C      3 POINTS
C
  300 CONTINUE
          P(1) = -0.774590E0
          H(1) = 0.555555E0
          P(2) =  0.000000E0
          H(2) = 0.888888E0
          P(3) =  0.774590E0
          H(3) = 0.555555E0
      GO TO 500
C
C      4 POINTS
C
  400 CONTINUE
          P(1) = -0.861136E0
          H(1) = 0.3478548E0
          P(2) = -0.339980E0
          H(2) = 0.6521451E0
          P(3) =  0.339980E0
          H(3) = 0.6521451E0
          P(4) =  0.861136E0
          H(4) = 0.3478548E0
      GO TO 500
C
C
  500 CONTINUE
C
      NPOINT = 0
      DO 620 I = 1 , IGAUSS
          DO 610 J = 1 , IGAUSS
              DO 600 K = 1 , IGAUSS
                  NPOINT = NPOINT+1
C
                  IF(NPOINT.GT.MPOINT) THEN
                      WRITE(IUT0,*) ERMSGB
                      WRITE(IUT0,*) EREXP2
                      IERR = 1
                      RETURN
                  ENDIF
C
                  GP(NPOINT) = P(I)
                  EP(NPOINT) = P(J)
                  TP(NPOINT) = P(K)
                  WP(NPOINT) = H(I)*H(J)*H(K)
  600         CONTINUE
  610     CONTINUE
  620 CONTINUE
C
C
      RETURN
 6300 FORMAT(A72)
      END
