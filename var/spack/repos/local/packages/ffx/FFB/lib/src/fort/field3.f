C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    FIELD3                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE FIELD3(IMODE,U,V,W,DNXI,DNYI,DNZI,NODE,NE,NP,N,FE)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION U(NP),V(NP),W(NP),
     1          DNXI(N,NE),DNYI(N,NE),DNZI(N,NE),NODE(N,NE),FE(NE)
C
C
C      CALCULATE VELOCITY FIELD CHARACTERISTICS
C         ( 3-D CALCULATION : SINGLE WORD VERSION )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          IMODE       ; SPECIFIES THE TYPE OF FIELD DATA AS FOLLOWS
C                   1 --- ELEMENT DIVERGENT
C                   2 --- ELEMENT VORTICITY ( ROTATION ) ABOUT X-AXIS
C                   3 --- ELEMENT VORTICITY ( ROTATION ) ABOUT Y-AXIS
C                   4 --- ELEMENT VORTICITY ( ROTATION ) ABOUT Z-AXIS
C          U     (IP)  ; X-DIR. VELOCITY COMPONENT
C          V     (IP)  ; Y-DIR. VELOCITY COMPONENT
C          W     (IP)  ; Z-DIR. VELOCITY COMPONENT
C          DNXI(I,IE)  ; ELEMENT CENTER VALUE OF     NX
C          DNYI(I,IE)  ; ELEMENT CENTER VALUE OF     NY
C          DNZI(I,IE)  ; ELEMENT CENTER VALUE OF     NZ
C          NODE(I,IE)  ; NODE NO. TABLE BASED ON ELEMENT
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          NP          ; NUMBER OF TOTAL    NODES
C          N           ; NUMBER OF NODE ASSIGNED TO ONE ELEMENT
C
C      (2) OUTPUT
C          FE    (IE)  ; FIELD DATA
C
C
C
C      (1) CALCULATE DIVERGENT
C
      IF(IMODE.EQ.1) THEN
          DO 100 IE = 1 , NE
              FE(IE) = DNXI(1,IE)*U(NODE(1,IE))
     &                +DNXI(2,IE)*U(NODE(2,IE))
     &                +DNXI(3,IE)*U(NODE(3,IE))
     &                +DNXI(4,IE)*U(NODE(4,IE))
     &                +DNXI(5,IE)*U(NODE(5,IE))
     &                +DNXI(6,IE)*U(NODE(6,IE))
     &                +DNXI(7,IE)*U(NODE(7,IE))
     &                +DNXI(8,IE)*U(NODE(8,IE))
  100     CONTINUE
          DO 200 IE = 1 , NE
              FE(IE) = FE(IE)
     &                +DNYI(1,IE)*V(NODE(1,IE))
     &                +DNYI(2,IE)*V(NODE(2,IE))
     &                +DNYI(3,IE)*V(NODE(3,IE))
     &                +DNYI(4,IE)*V(NODE(4,IE))
     &                +DNYI(5,IE)*V(NODE(5,IE))
     &                +DNYI(6,IE)*V(NODE(6,IE))
     &                +DNYI(7,IE)*V(NODE(7,IE))
     &                +DNYI(8,IE)*V(NODE(8,IE))
  200     CONTINUE
          DO 300 IE = 1 , NE
              FE(IE) = FE(IE)
     &                +DNZI(1,IE)*W(NODE(1,IE))
     &                +DNZI(2,IE)*W(NODE(2,IE))
     &                +DNZI(3,IE)*W(NODE(3,IE))
     &                +DNZI(4,IE)*W(NODE(4,IE))
     &                +DNZI(5,IE)*W(NODE(5,IE))
     &                +DNZI(6,IE)*W(NODE(6,IE))
     &                +DNZI(7,IE)*W(NODE(7,IE))
     &                +DNZI(8,IE)*W(NODE(8,IE))
  300     CONTINUE
      ENDIF
C
C      (2) CALCULATE ROTATION ABOUT X-AXIS
C
      IF(IMODE.EQ.2) THEN
          DO 400 IE = 1 , NE
              FE(IE) = DNYI(1,IE)*W(NODE(1,IE))
     &                +DNYI(2,IE)*W(NODE(2,IE))
     &                +DNYI(3,IE)*W(NODE(3,IE))
     &                +DNYI(4,IE)*W(NODE(4,IE))
     &                +DNYI(5,IE)*W(NODE(5,IE))
     &                +DNYI(6,IE)*W(NODE(6,IE))
     &                +DNYI(7,IE)*W(NODE(7,IE))
     &                +DNYI(8,IE)*W(NODE(8,IE))
     &                -DNZI(1,IE)*V(NODE(1,IE))
     &                -DNZI(2,IE)*V(NODE(2,IE))
     &                -DNZI(3,IE)*V(NODE(3,IE))
     &                -DNZI(4,IE)*V(NODE(4,IE))
     &                -DNZI(5,IE)*V(NODE(5,IE))
     &                -DNZI(6,IE)*V(NODE(6,IE))
     &                -DNZI(7,IE)*V(NODE(7,IE))
     &                -DNZI(8,IE)*V(NODE(8,IE))
  400     CONTINUE
      ENDIF
C
C      (3) CALCULATE ROTATION ABOUT Y-AXIS
C
      IF(IMODE.EQ.3) THEN
          DO 500 IE = 1 , NE
              FE(IE) = DNZI(1,IE)*U(NODE(1,IE))
     &                +DNZI(2,IE)*U(NODE(2,IE))
     &                +DNZI(3,IE)*U(NODE(3,IE))
     &                +DNZI(4,IE)*U(NODE(4,IE))
     &                +DNZI(5,IE)*U(NODE(5,IE))
     &                +DNZI(6,IE)*U(NODE(6,IE))
     &                +DNZI(7,IE)*U(NODE(7,IE))
     &                +DNZI(8,IE)*U(NODE(8,IE))
     &                -DNXI(1,IE)*W(NODE(1,IE))
     &                -DNXI(2,IE)*W(NODE(2,IE))
     &                -DNXI(3,IE)*W(NODE(3,IE))
     &                -DNXI(4,IE)*W(NODE(4,IE))
     &                -DNXI(5,IE)*W(NODE(5,IE))
     &                -DNXI(6,IE)*W(NODE(6,IE))
     &                -DNXI(7,IE)*W(NODE(7,IE))
     &                -DNXI(8,IE)*W(NODE(8,IE))
  500     CONTINUE
      ENDIF
C
C      (4) CALCULATE ROTATION ABOUT Z-AXIS
C
      IF(IMODE.EQ.4) THEN
          DO 600 IE = 1 , NE
              FE(IE) = DNXI(1,IE)*V(NODE(1,IE))
     &                +DNXI(2,IE)*V(NODE(2,IE))
     &                +DNXI(3,IE)*V(NODE(3,IE))
     &                +DNXI(4,IE)*V(NODE(4,IE))
     &                +DNXI(5,IE)*V(NODE(5,IE))
     &                +DNXI(6,IE)*V(NODE(6,IE))
     &                +DNXI(7,IE)*V(NODE(7,IE))
     &                +DNXI(8,IE)*V(NODE(8,IE))
     &                -DNYI(1,IE)*U(NODE(1,IE))
     &                -DNYI(2,IE)*U(NODE(2,IE))
     &                -DNYI(3,IE)*U(NODE(3,IE))
     &                -DNYI(4,IE)*U(NODE(4,IE))
     &                -DNYI(5,IE)*U(NODE(5,IE))
     &                -DNYI(6,IE)*U(NODE(6,IE))
     &                -DNYI(7,IE)*U(NODE(7,IE))
     &                -DNYI(8,IE)*U(NODE(8,IE))
  600     CONTINUE
      ENDIF
C
C
      RETURN
      END
