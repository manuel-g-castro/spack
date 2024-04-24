C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    PATCHE                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE PATCHE(JPSOUT,IUTPS,X,Y,SR,NODE,NE,NP,N,SRMIN,SRMAX,
     *                  XMIN,YMIN,SFC,LCL,NCL)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION X(NP),Y(NP),SR(NP),NODE(N,NE),LCL(NCL)
C
#ifdef VOS
      INCLUDE (GN)
#else
      INCLUDE 'gn.h'
#endif
C
      DIMENSION XP(4),YP(4)
C
C
C      PATCH ELEMENTS WITH CORRESPONDING COLOR TO SR(IP)
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
C          SR      (IP); SCALAR
C          NODE  (I,IE); NODE TABLE
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          NP          ; NUMBER OF TOTAL NODES
C          N           ; NUMBER OF ASSIGNED TO ONE ELEMENT
C          SRMIN       ; MIN. SCALAR VALUE SPECIFIED
C          SRMAX       ; MAX. SCALAR VALUE SPECIFIED
C          XMIN        ; MIN. X OF GRAPHIC RANGE SPECIFIED
C          YMIN        ; MIN. Y OF GRAPHIC RANGE SPECIFIED
C          SFC         ; GRAPHIC SCALING FACTOR
C          LCL    (ICL); COLOR INDECES
C          NCL         ; NUMBER OF COLOR INDECES DEFINED
C
C       (2) OUTPUT
C          NONE
C
C
      DO 20 IE = 1 , NE
          SRG = 0.25*(SR(NODE(1,IE))+SR(NODE(2,IE))
     &               +SR(NODE(3,IE))+SR(NODE(4,IE)))
          RATIO = (SRG-SRMIN)/(SRMAX-SRMIN)
          ICL   = RATIO*NCL+1
          IF(ICL.LT.1  ) THEN
              ICL = 1
          ENDIF
          IF(ICL.GT.NCL) THEN
              ICL = NCL
          ENDIF
C
          CALL GNCSET (LCL(ICL))
          IF(JPSOUT.EQ.1) CALL PSGRAY (IUTPS,LCL(ICL))
          IF(JPSOUT.EQ.2) CALL PSCSET (IUTPS,LCL(ICL))
C
          DO 10 I = 1 , N
              XP(I) = SFC*(X(NODE(I,IE))-XMIN)
              YP(I) = SFC*(Y(NODE(I,IE))-YMIN)
   10     CONTINUE
          CALL GNFILL(XP,YP,4)
          IF(JPSOUT.GE.1) CALL PSFILL(IUTPS,XP,YP,4)
   20 CONTINUE
C
C
      RETURN
      END
