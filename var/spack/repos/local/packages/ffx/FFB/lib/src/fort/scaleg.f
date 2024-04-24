C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    SCALEG                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE SCALEG(X,Y,NP,RNGX,RNGY,RATIO,XMIN,XMAX,YMIN,YMAX,SFC)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION X(NP),Y(NP)
C
C
C      SCALE 2-DIMENSIONAL FIELD
C         ( 2-D GRAPHICS )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          X(IP)       ; X-DIR. COORDINATE
C          Y(IP)       ; Y-DIR. COORDINATE
C          NP          ; NUMBER OF TOTAL NODES
C          RNGX        ; X-DIR. GRAPHIC RANGE SPECIFIED
C          RNGY        ; Y-DIR. GRAPHIC RANGE SPECIFIED
C          RATIO       ; SCALING COEFFICIENT
C                RATIO < 1.0 --- UNDER SCALING
C                RATIO > 1.0 --- OVER  SCALING
C
C       (2) OUTPUT
C          XMIN        ; MINIMUM X-VALUE
C          XMAX        ; MAXIMUM X-VALUE
C          YMIN        ; MINIMUM Y-VALUE
C          YMAX        ; MAXIMUM Y-VALUE
C          SFC         ; SCALING FACTOR
C
C
      CALL FINDMM(X,NP,OXMIN,OXMAX)
      CALL FINDMM(Y,NP,OYMIN,OYMAX)
C
      XMIN = OXMIN-0.5*(1.0-RATIO)/RATIO*(OXMAX-OXMIN)
      XMAX = OXMAX+0.5*(1.0-RATIO)/RATIO*(OXMAX-OXMIN)
      YMIN = OYMIN-0.5*(1.0-RATIO)/RATIO*(OYMAX-OYMIN)
      YMAX = OYMAX+0.5*(1.0-RATIO)/RATIO*(OYMAX-OYMIN)
C
      SFC = AMIN1(RNGX/(XMAX-XMIN) , RNGY/(YMAX-YMIN))
C
C
      RETURN
      END
