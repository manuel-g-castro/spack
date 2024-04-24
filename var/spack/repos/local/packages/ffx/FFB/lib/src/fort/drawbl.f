C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    DRAWBL                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE DRAWBL(JPSOUT,IUTPS,X,Y,LSIDE,NP,NS,XMIN,YMIN,SFC,
     *                  ICLBLN)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION X(NP),Y(NP),LSIDE(2,NS)
C
#ifdef VOS
      INCLUDE (GN)
#else
      INCLUDE 'gn.h'
#endif
C
      DIMENSION XP(2),YP(2)
C
C
C      DRAW BORDER LINE
C         ( 2-D GRAPHICS )
C
C
C     NOTE 1 ; CALLING GENERIC GRAPHIC AND POSTSCRIPT INTERFACES.
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          JPSOUT      ; POSTSCRIPT FILE WILL BE OUTPUT IF SET TO 1 OR 2
C          IUTPS       ; FILE NUMBER TO ACCESS POSTSCRIPT FILE
C          X       (IP); X-COORDINATE OF NODES
C          Y       (IP); Y-COORDINATE OF NODES
C          LSIDE (I,IS); BOUNDARY SIDES
C              ( LSIDE(1,IS),LSIDE(2,IS) CONSTITUTES ONE BOUNDARY SIDE )
C          NP          ; NUMBER OF TOTAL NODES
C          NS          ; NUMBER OF BOUNDARY SIDES FOUND
C          XMIN        ; MIN. X OF GRAPHIC RANGE SPECIFIED
C          YMIN        ; MIN. Y OF GRAPHIC RANGE SPECIFIED
C          SFC         ; GRAPHIC SCALING FACTOR
C          ICLBLN      ; COLOR INDEX USED TO DRAW BORDER LINES
C
C       (2) OUTPUT
C          NONE
C
C
      LNTYPE = GLSOLD
      PLTYPE = GLSOLD
C
      LNWDTH = GLBOLD
      PLWDTH = GLBOLD
C
      CALL  GNCSET(ICLBLN)
      IF(JPSOUT.EQ.1) CALL  PSGRAY(IUTPS,ICLBLN)
      IF(JPSOUT.EQ.2) CALL  PSCSET(IUTPS,ICLBLN)
      DO 10 IS = 1 , NS
          XP(1) = SFC*(X(LSIDE(1,IS))-XMIN)
          YP(1) = SFC*(Y(LSIDE(1,IS))-YMIN)
          XP(2) = SFC*(X(LSIDE(2,IS))-XMIN)
          YP(2) = SFC*(Y(LSIDE(2,IS))-YMIN)
          CALL GNLINE(XP,YP,2)
          IF(JPSOUT.GE.1) CALL PSLINE(IUTPS,XP,YP,2)
   10 CONTINUE
C
C
      RETURN
      END
