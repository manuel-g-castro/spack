C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    SHIFT3                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE SHIFT3(IFORM,DELTA,VISC,DT,U,V,W,NODE,NE,NP,N,
     *                  LEWALL,NEWALL,SX,SY,SZ,VABS,PEC,GZAI,TAU)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION DELTA(NE),VISC(NE),U(NP),V(NP),W(NP),
     1          NODE(N,NE),LEWALL(2,NEWALL),SX(NE),SY(NE),SZ(NE),
     2          VABS(NE),PEC(NE),GZAI(NE),TAU(NE)
      DATA D / 1.E-30 /
C
C
C      CALCULATE UPWIND VECTORS
C         ( 3-D CALCULATION : SINGLE WORD VERSION )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          IFORM       ; SPECIFIES UPWIND VECTORS TO BE CALCULATED
C                   3 --- ELEMENT PECLET NO. DEPENDENT UPWIND VECTOR
C                   4 --- TIME INCREMENT     DEPENDENT UPWIND VECTOR
C           NOTES ; FOR WALL FACING ELEMENTS SPECIFIED BY ARGUMENT
C                  'LEWALL(1,IEWALL)', ELEMENT PECLET NUMBER DEPENDING
C                  UPWIND VECTOR WILL BE SET, REGARDLESS OF THE VALUE
C                  OF 'IFORM'.
C          DELTA   (IE); ELEMENT CHARACTERISTIC DIMENSION
C          VISC    (IE); ELEMENT DIFFUSION COEFFICIENT
C          DT          ; TIME INCREMENT
C          U       (IP); X-DIR. VELOCITY COMPONENT
C          V       (IP); Y-DIR. VELOCITY COMPONENT
C          W       (IP); Z-DIR. VELOCITY COMPONENT
C          NODE  (I,IE); NODE NO. TABLE BASED ON ELEMENT
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          NP          ; NUMBER OF TOTAL     NODES
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT ( N=4 )
C          LEWALL(I,IBE); WALL BOUNDARY ELEMENT AND ITS SURFACE
C          NEWALL      ; NUMBER OF WALL BOUNDARY SURFACES
C
C       (2) OUTPUT
C          SX      (IE); UPWIND VECTOR IN X-DIR.
C          SY      (IE); UPWIND VECTOR IN Y-DIR.
C          SZ      (IE); UPWIND VECTOR IN Z-DIR.
C
C       (3) WORK
C          VABS    (IE); VELOCITY MAGNITUDE CALCULATION
C          PEC     (IE); ELEMENT PECLET NUMBER
C          GZAI    (IE); UPWINDING PARAMETER
C          TAU     (IE); UPWINDING PARAMETER
C
C
C
C      (1) UPWIND VECTOR BASED ON ELEMENT PECLET NUMBER
C
C
      IF(IFORM.EQ.3) THEN
          COEFF = 0.125E0
          DO 300 IE = 1 , NE
              SX(IE) = COEFF*(U(NODE(1,IE))+U(NODE(5,IE))
     &                       +U(NODE(2,IE))+U(NODE(6,IE))
     &                       +U(NODE(3,IE))+U(NODE(7,IE))
     &                       +U(NODE(4,IE))+U(NODE(8,IE)))
C
              SY(IE) = COEFF*(V(NODE(1,IE))+V(NODE(5,IE))
     &                       +V(NODE(2,IE))+V(NODE(6,IE))
     &                       +V(NODE(3,IE))+V(NODE(7,IE))
     &                       +V(NODE(4,IE))+V(NODE(8,IE)))
C
              SZ(IE) = COEFF*(W(NODE(1,IE))+W(NODE(5,IE))
     &                       +W(NODE(2,IE))+W(NODE(6,IE))
     &                       +W(NODE(3,IE))+W(NODE(7,IE))
     &                       +W(NODE(4,IE))+W(NODE(8,IE)))
  300     CONTINUE
C
          DO 400 IE = 1 , NE
              VABS(IE) = SX(IE)*SX(IE)+SY(IE)*SY(IE)+SZ(IE)*SZ(IE)+D
              VABS(IE) = SQRT(VABS(IE))
              PEC (IE) = VABS(IE)*DELTA(IE)/(VISC(IE)+D)+D
  400     CONTINUE
C
          DO 500 IE = 1 , NE
              GZAI(IE) = TANH(PEC(IE))
              GZAI(IE) = 1.E0/GZAI(IE)-1.E0/PEC(IE)
              TAU (IE) = 0.5E0*GZAI(IE)*DELTA(IE)/(VABS(IE))
C
              SX(IE) = TAU(IE)*SX(IE)
              SY(IE) = TAU(IE)*SY(IE)
              SZ(IE) = TAU(IE)*SZ(IE)
  500     CONTINUE
      ENDIF
C
C      (2) UPWIND VECTOR BASED ON TIME INCREMENT
C
      IF(IFORM.EQ.4) THEN
          COEFF = 0.125E0*0.5E0*DT
          DO 600 IE = 1 , NE
              SX(IE) = COEFF*(U(NODE(1,IE))+U(NODE(5,IE))
     &                       +U(NODE(2,IE))+U(NODE(6,IE))
     &                       +U(NODE(3,IE))+U(NODE(7,IE))
     &                       +U(NODE(4,IE))+U(NODE(8,IE)))
C
              SY(IE) = COEFF*(V(NODE(1,IE))+V(NODE(5,IE))
     &                       +V(NODE(2,IE))+V(NODE(6,IE))
     &                       +V(NODE(3,IE))+V(NODE(7,IE))
     &                       +V(NODE(4,IE))+V(NODE(8,IE)))
C
              SZ(IE) = COEFF*(W(NODE(1,IE))+W(NODE(5,IE))
     &                       +W(NODE(2,IE))+W(NODE(6,IE))
     &                       +W(NODE(3,IE))+W(NODE(7,IE))
     &                       +W(NODE(4,IE))+W(NODE(8,IE)))
  600     CONTINUE
C
          DO 700 IEWALL = 1 , NEWALL
              IE = LEWALL(1,IEWALL)
C
              SX(IE)   = SX(IE)/(0.5E0*DT)
              SY(IE)   = SY(IE)/(0.5E0*DT)
              SZ(IE)   = SZ(IE)/(0.5E0*DT)
C
              VABS(IE) = SX(IE)*SX(IE)+SY(IE)*SY(IE)+SZ(IE)*SZ(IE)+D
              VABS(IE) = SQRT(VABS(IE))
              PEC (IE) = VABS(IE)*DELTA(IE)/(VISC(IE)+D)+D
              GZAI(IE) = TANH(PEC(IE))
              GZAI(IE) = 1.E0/GZAI(IE)-1.E0/PEC(IE)
              TAU (IE) = 0.5E0*GZAI(IE)*DELTA(IE)/(VABS(IE))
C
              SX(IE) = TAU(IE)*SX(IE)
              SY(IE) = TAU(IE)*SY(IE)
              SZ(IE) = TAU(IE)*SZ(IE)
  700     CONTINUE
      ENDIF
C
C
      RETURN
      END
