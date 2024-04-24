C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    DRAWMS                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE DRAWMS(JPSOUT,IUTPS,X,Y,NODE,NE,NP,N,XMIN,YMIN,SFC,
     *                  ICLMSH)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION X(NP),Y(NP),NODE(N,NE)
C
#ifdef VOS
      INCLUDE (GN)
#else
      INCLUDE 'gn.h'
#endif
C
      REAL*4 XP(5),YP(5)
C
C
C      DRAW MESH CONFIGURATION
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
C          X(IP)       ; X-COORDINATE OF NODES
C          Y(IP)       ; Y-COORDINATE OF NODES
C          NODE(I,IE)  ; NODE TABLE
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          NP          ; NUMBER OF TOTAL NODES
C          N           ; ALIGNMENT  DIMENSION OF THE ARRAY NODE (I,IE)
C          XMIN        ; MIN. X OF GRAPHIC RANGE SPECIFIED
C          YMIN        ; MIN. Y OF GRAPHIC RANGE SPECIFIED
C          SFC         ; GRAPHIC SCALING FACTOR
C          ICLMSH      ; COLOR INDEX USED TO DRAW MESH
C
C       (2) OUTPUT
C          NONE
C
C
      LNTYPE = GLSOLD
      PLTYPE = GLSOLD
      LNWDTH = GLNORM
      PLWDTH = GLNORM
C
      CALL  GNCSET (ICLMSH)
      IF(JPSOUT.EQ.1) CALL PSGRAY (IUTPS,ICLMSH)
      IF(JPSOUT.EQ.2) CALL PSCSET (IUTPS,ICLMSH)
C
      DO 20 IE = 1 , NE
          DO 10 I = 1 , N
              XP(I) = SFC*(X(NODE(I,IE))-XMIN)
              YP(I) = SFC*(Y(NODE(I,IE))-YMIN)
   10     CONTINUE
          XP(N+1) = SFC*(X(NODE(1,IE))-XMIN)
          YP(N+1) = SFC*(Y(NODE(1,IE))-YMIN)
          CALL GNLINE(XP,YP,N+1)
          IF(JPSOUT.GE.1) CALL PSLINE(IUTPS,XP,YP,N+1)
   20 CONTINUE
C
C
      RETURN
      END
