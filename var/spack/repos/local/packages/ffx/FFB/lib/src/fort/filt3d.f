C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    FILT3D                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE FILT3D(IFILTR,X,Y,Z,NODE,NE,NP,N,FILTER)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION X(NP),Y(NP),Z(NP),NODE(N,NE),FILTER(NE)
C
      INTEGER*4 LSIDE(2,12)
     & / 1 , 5 , 2 , 6 , 3 , 7 , 4 , 8 ,
     &   1 , 4 , 2 , 3 , 6 , 7 , 5 , 8 , 
     &   1 , 2 , 4 , 3 , 8 , 7 , 5 , 6 /
C
C
C
C      COMPUTE ELEMENT (GRID) FILTER WIDTH
C         ( 3-D CALCULATION : SINGLE WORD & SAME ELEMENT VERSION )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          IFILTR      ; CONTROLS GRID-FILTER WIDTH COMPUTATIONS AS:
C                   1 --- THE MINIMUM SIDE LENGTH
C                   2 --- INVERSE SQUARE AVERAGE OF SIDE LENGTH
C          X      (IP) ; X-DIR. COORDINATE         OF NODE
C          Y      (IP) ; Y-DIR. COORDINATE         OF NODE
C          Z      (IP) ; Y-DIR. COORDINATE         OF NODE
C          NODE (I,IE) ; NODE TABLE
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          NP          ; NUMBER OF TOTAL    NODES
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT
C
C       (2) OUTPUT
C          FILTER (IE) ; ELEMENT FILTER WIDTH
C
C
      IF(IFILTR.EQ.2) THEN
          DO 100 IE = 1 , NE
              FILTER(IE) = 0.E0
  100     CONTINUE
C
          DO 210 I = 1 , 12
              DO 200 IE = 1 , NE
                  I1 = NODE(LSIDE(1, I),IE)
                  I2 = NODE(LSIDE(2, I),IE)
                  FILTER(IE) = FILTER(IE)
     &        +1.E0/((X(I2)-X(I1))**2+(Y(I2)-Y(I1))**2+(Z(I2)-Z(I1))**2)
  200         CONTINUE
  210     CONTINUE
C
          DO 300 IE = 1 , NE
              FILTER(IE) = SQRT(12.E0/FILTER(IE))
  300     CONTINUE
      ELSE
          DO 410 I = 1 , 12
              DO 400 IE = 1 , NE
                  I1 = NODE(LSIDE(1, I),IE)
                  I2 = NODE(LSIDE(2, I),IE)
                  DIST = SQRT((X(I2)-X(I1))**2+(Y(I2)-Y(I1))**2
     &                       +(Z(I2)-Z(I1))**2)
                  IF(I.EQ.1 .OR. DIST.LE.FILTER(IE)) FILTER(IE) = DIST 
  400         CONTINUE
  410     CONTINUE
      ENDIF
C
C
      RETURN
      END
