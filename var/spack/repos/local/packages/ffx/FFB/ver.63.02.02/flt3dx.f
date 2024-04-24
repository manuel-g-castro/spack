C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.8.1                                   C
C                                                                      C
C  SUB ROUTINE : FLT3DX                                                C
C                                                                      C
C                                       WRITTEN BY Y.YAMADE            C
C                                                                      C
C                                                                      C
C CONTACT ADDRESS : IIS, THE UNIVERSITY OF TOKYO, CISS                 C
C                                                                      C
C THERMO-FLUID ANALYSIS SOLVERS FOR LARGE-SCALE-ASSEMBLY               C
C                                                                      C
C======================================================================C
      SUBROUTINE FLT3DX(IFILTR,X,Y,Z,NODE,NE,NP,N2,NEX,FILTER)
C
      IMPLICIT NONE
C
      INTEGER*4 IFILTR,NODE,NE,NP,N2,NEX
      REAL*4    X,Y,Z,FILTER
C
      INTEGER*4 NETET,NEPRD,NEWED,NEHEX,NTET,NWED,NHEX,
     *          IES1,IEE1,IES2,IEE2,IES3,IEE3,IES4,IEE4,IE,I,
     *          NPRD,I1,I2
      REAL*4    DIST
C
      DIMENSION X(NP),Y(NP),Z(NP),NODE(N2,NE),FILTER(NE),NEX(8)
C
CC    INTEGER*4 LSIDE(2,6)
CC   & / 2,3, 3,4, 4,2, 1,2, 1,3, 1,4 / 
      INTEGER*4 EDGTET(2,6)
     & / 2,3, 3,4, 4,2, 1,2, 1,3, 1,4 /
      INTEGER*4 EDGHEX(2,12)
     & / 1,5, 2,6, 3,7, 4,8,
     &   1,4, 2,3, 6,7, 5,8,
     &   1,2, 4,3, 8,7, 5,6 /
      INTEGER*4 EDGPRS(2,9)
     & / 1,2, 2,3, 3,1, 4,5, 5,6, 6,4,
     &   1,4, 2,5, 3,6 /
      INTEGER*4 EDGPYR(2,8)
     & / 1,2, 2,3, 3,4, 4,1,
     &   5,1, 5,2, 5,3, 5,4 / 
C
C
C
C      COMPUTE ELEMENT (GRID) FILTER WIDTH
C         ( 3-D CALCULATION : SINGLE WORD & MULTU ELEMENT VERSION )
C                                           CODED BASED ON 'FILT3D'
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
      NETET=NEX(1)
      NEPRD=NEX(2)
      NEWED=NEX(3)
      NEHEX=NEX(4)
      NE   =NETET+NEPRD+NEWED+NEHEX
C
      NTET =NEX(5)
      NPRD =NEX(6)
      NWED =NEX(7)
      NHEX =NEX(8)
C
C     * TET *
      IES1=1
      IEE1=NETET
C
C     * PYRAMID *
      IES2=NETET+1
      IEE2=NETET+NEPRD
C
C     * WEDGE *
      IES3=NETET+NEPRD+1
      IEE3=NETET+NEPRD+NEWED
C
C     * HEX *
      IES4=NETET+NEPRD+NEWED+1
      IEE4=NETET+NEPRD+NEWED+NEHEX
C
      IF(IFILTR.EQ.2) THEN
          DO 100 IE = 1 , NE
              FILTER(IE) = 0.E0
  100     CONTINUE
C
C         * TET *
          DO 210 I = 1 , 6
              DO 200 IE = IES1, IEE1
                  I1 = NODE(EDGTET(1, I),IE)
                  I2 = NODE(EDGTET(2, I),IE)
                  FILTER(IE) = FILTER(IE)
     &        +1.E0/((X(I2)-X(I1))**2+(Y(I2)-Y(I1))**2+(Z(I2)-Z(I1))**2)
  200         CONTINUE
  210     CONTINUE
          DO 300 IE = IES1, IEE1
              FILTER(IE) = SQRT(6.E0/FILTER(IE))
  300     CONTINUE
C
C         * PYRAMID *
          DO 211 I = 1 , 8
              DO 201 IE = IES2, IEE2
                  I1 = NODE(EDGPYR(1, I),IE)
                  I2 = NODE(EDGPYR(2, I),IE)
                  FILTER(IE) = FILTER(IE)
     &        +1.E0/((X(I2)-X(I1))**2+(Y(I2)-Y(I1))**2+(Z(I2)-Z(I1))**2)
 201          CONTINUE
 211      CONTINUE
          DO 301 IE = IES2, IEE2
              FILTER(IE) = SQRT(8.E0/FILTER(IE))
 301       CONTINUE
C
C         * WEDGE *
          DO 212 I = 1 , 9
              DO 202 IE = IES3, IEE3
                  I1 = NODE(EDGPRS(1, I),IE)
                  I2 = NODE(EDGPRS(2, I),IE)
                  FILTER(IE) = FILTER(IE)
     &        +1.E0/((X(I2)-X(I1))**2+(Y(I2)-Y(I1))**2+(Z(I2)-Z(I1))**2)
 202          CONTINUE
 212      CONTINUE
          DO 302 IE = IES3, IEE3
              FILTER(IE) = SQRT(9.E0/FILTER(IE))
 302       CONTINUE
C
C         * HEX *
          DO 213 I = 1 , 12
              DO 203 IE = IES4, IEE4
                  I1 = NODE(EDGHEX(1, I),IE)
                  I2 = NODE(EDGHEX(2, I),IE)
                  FILTER(IE) = FILTER(IE)
     &        +1.E0/((X(I2)-X(I1))**2+(Y(I2)-Y(I1))**2+(Z(I2)-Z(I1))**2)
 203          CONTINUE
 213      CONTINUE
          DO 303 IE = IES4, IEE4
              FILTER(IE) = SQRT(12.E0/FILTER(IE))
 303       CONTINUE
      ELSE
C
C         * TET *
          DO 410 I = 1 , 6
              DO 400 IE = IES1, IEE1
                  I1 = NODE(EDGTET(1, I),IE)
                  I2 = NODE(EDGTET(2, I),IE)
                  DIST = SQRT((X(I2)-X(I1))**2+(Y(I2)-Y(I1))**2
     &                       +(Z(I2)-Z(I1))**2)
                  IF(I.EQ.1 .OR. DIST.LE.FILTER(IE)) FILTER(IE) = DIST 
  400         CONTINUE
  410     CONTINUE
C
C         * PYRAMID *
          DO 411 I = 1 , 8
              DO 401 IE = IES2, IEE2
                  I1 = NODE(EDGPYR(1, I),IE)
                  I2 = NODE(EDGPYR(2, I),IE)
                  DIST = SQRT((X(I2)-X(I1))**2+(Y(I2)-Y(I1))**2
     &                       +(Z(I2)-Z(I1))**2)
                  IF(I.EQ.1 .OR. DIST.LE.FILTER(IE)) FILTER(IE) = DIST 
 401          CONTINUE
 411      CONTINUE
C
C         * WEDGE *
          DO 412 I = 1 , 9
              DO 402 IE = IES3, IEE3
                  I1 = NODE(EDGPRS(1, I),IE)
                  I2 = NODE(EDGPRS(2, I),IE)
                  DIST = SQRT((X(I2)-X(I1))**2+(Y(I2)-Y(I1))**2
     &                       +(Z(I2)-Z(I1))**2)
                  IF(I.EQ.1 .OR. DIST.LE.FILTER(IE)) FILTER(IE) = DIST 
 402          CONTINUE
 412      CONTINUE
C
C         * HEX *
          DO 413 I = 1 , 12
              DO 403 IE = IES4, IEE4
                  I1 = NODE(EDGHEX(1, I),IE)
                  I2 = NODE(EDGHEX(2, I),IE)
                  DIST = SQRT((X(I2)-X(I1))**2+(Y(I2)-Y(I1))**2
     &                       +(Z(I2)-Z(I1))**2)
                  IF(I.EQ.1 .OR. DIST.LE.FILTER(IE)) FILTER(IE) = DIST 
 403          CONTINUE
 413      CONTINUE
      ENDIF
C
C
      RETURN
      END
