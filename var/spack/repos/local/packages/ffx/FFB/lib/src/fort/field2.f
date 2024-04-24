C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    FIELD2                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE FIELD2(IMODE,U,V,DNXI,DNYI,NODE,NE,N,FE)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION U(*),V(*),DNXI(N,NE),DNYI(N,NE),NODE(N,NE),FE(NE)
C
C
C      CALCULATE VELOCITY FIELD CHARACTERISTICS
C         ( 2-D CALCULATION )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          IMODE       ; SPECIFIES THE TYPE OF FIELD DATA AS FOLLOWS
C                   1 --- ELEMENT DIVERGENT
C                   2 --- ELEMENT VORTICITY ( ROTATION )
C          U     (IP)  ; X-DIR. VELOCITY COMPONENT
C          V     (IP)  ; Y-DIR. VELOCITY COMPONENT
C          DNXI(I,IE)  ; ELEMENT CENTER VALUE OF     NX
C          DNYI(I,IE)  ; ELEMENT CENTER VALUE OF     NY
C          NODE(I,IE)  ; NODE NO. TABLE BASED ON ELEMENT
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          N           ; NUMBER OF NODE ASSIGNED TO ONE ELEMENT
C
C      (2) OUTPUT
C          FE    (IE)  ; FIELD DATA
C
C
C      (1) CALCULATE DIVERGENT
C
      IF(IMODE.EQ.1) THEN
          DO 100 IE = 1 , NE
              FE(IE) = DNXI(1,IE)*U(NODE(1,IE))+DNYI(1,IE)*V(NODE(1,IE))
     &                +DNXI(2,IE)*U(NODE(2,IE))+DNYI(2,IE)*V(NODE(2,IE))
     &                +DNXI(3,IE)*U(NODE(3,IE))+DNYI(3,IE)*V(NODE(3,IE))
     &                +DNXI(4,IE)*U(NODE(4,IE))+DNYI(4,IE)*V(NODE(4,IE))
  100     CONTINUE
      ENDIF
C
C      (2) CALCULATE ROTATION
C
      IF(IMODE.EQ.2) THEN
          DO 200 IE = 1 , NE
              FE(IE) = DNXI(1,IE)*V(NODE(1,IE))-DNYI(1,IE)*U(NODE(1,IE))
     &                +DNXI(2,IE)*V(NODE(2,IE))-DNYI(2,IE)*U(NODE(2,IE))
     &                +DNXI(3,IE)*V(NODE(3,IE))-DNYI(3,IE)*U(NODE(3,IE))
     &                +DNXI(4,IE)*V(NODE(4,IE))-DNYI(4,IE)*U(NODE(4,IE))
  200     CONTINUE
      ENDIF
C
C
      RETURN
      END
