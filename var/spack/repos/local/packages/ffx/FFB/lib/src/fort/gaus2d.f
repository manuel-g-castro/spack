C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    GAUS2D                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE GAUS2D(IGAUSS,MPOINT,GP,EP,WP,NPOINT,IUT0,IERR)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION GP(MPOINT),EP(MPOINT),WP(MPOINT)
C
      PARAMETER ( MGAUSS = 4 )
      DIMENSION P(MGAUSS),H(MGAUSS)
C
      CHARACTER*72 ERMSG
     & /' *** SUBROUTINE GAUS2D REPORTS A FATAL ERROR OCCURENCE ***' /
      CHARACTER*72 EREXP1
     & /' SPECIFIED GAUSS POINT  IS NOT CURRENTLY SUPPORTED        ' /
      CHARACTER*72 EREXP2
     & /' THE DIMENSION OF ARRAY GP , EP , WP IS NOT SUFFICIENT    ' /
C
C
C      SET GAUSS POINTS, AND CORRESPONDING WEIGHTING FUNCTIONS
C         ( 2-D CALCULARION )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          IGAUSS      ; SPECIFIES NUMBER OF GAUSS POINTS TO BE SET
C                   1 --- ONE POINT
C                   2 --- 2*2 POINTS
C                   3 --- 3*3 POINTS
C                   4 --- 4*4 POINTS
C          MPOINT      ; THE DIMENSION OF THE ARRAY GP , EP , WP
C          IUT0        ; DEVICE NO. TO REPORT ERROR OCCURENCE
C
C       (2) OUTPUT
C          GP(IPOINT)  ; GZAI-COORDINATE    OF GAUSS POINTS
C          EP(IPOINT)  ; EATA-COORDINATE    OF GAUSS POINTS
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
          WRITE(IUT0,6300) ERMSG
          WRITE(IUT0,6300) EREXP1 
          IERR = 1 
          RETURN
      ENDIF
C
      GO TO ( 100 , 200 , 300 , 400 ) , IGAUSS
C
C      1*1 POINTS
C
  100 CONTINUE
          P(1) = 0.D0        
          H(1) = 2.D0
      GO TO 500
C
C      2*2 POINTS
C
  200 CONTINUE
          P(1) = -0.577350D0  
          H(1) = 1.D0
          P(2) =  0.577350D0  
          H(2) = 1.D0
      GO TO 500
C
C      3*3 POINTS
C
  300 CONTINUE
          P(1) = -0.774590D0  
          H(1) = 0.555555D0
          P(2) =  0.000000D0  
          H(2) = 0.888888D0
          P(3) =  0.774590D0  
          H(3) = 0.555555D0
      GO TO 500
C
C      4*4 POINTS
C
  400 CONTINUE
          P(1) = -0.861136D0  
          H(1) = 0.3478548D0
          P(2) = -0.339980D0  
          H(2) = 0.6521451D0
          P(3) =  0.339980D0  
          H(3) = 0.6521451D0
          P(4) =  0.861136D0  
          H(4) = 0.3478548D0
      GO TO 500
C
C
  500 CONTINUE
C
      NPOINT = 0
      DO 610 I = 1 , IGAUSS
          DO 600 J = 1 , IGAUSS
              NPOINT = NPOINT+1
C
              IF(NPOINT.GT.MPOINT) THEN
                  WRITE(IUT0,6300) ERMSG
                  WRITE(IUT0,6300) EREXP2 
                  IERR = 1 
                  RETURN
              ENDIF
C
              GP(NPOINT) = P(I)
              EP(NPOINT) = P(J)
              WP(NPOINT) = H(I)*H(J)
  600     CONTINUE
  610 CONTINUE
C
C
      RETURN
 6300 FORMAT(A72)
      END
