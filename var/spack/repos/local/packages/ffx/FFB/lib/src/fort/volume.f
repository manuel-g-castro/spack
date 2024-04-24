C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    VOLUME                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE VOLUME(IDIM,JELM,S,DELTA,NODE,NE,N,AVR,F)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION S(*),DELTA(NE),NODE(N,NE),F(NE)
C
C
C      CALCULATE VOLUMETIRC AVERAGE OF ELEMENT OR NODE DEFINED FIELD
C         ( 2-D , 3-D CALCULATION )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          IDIM        ; SPECIFIES THE SPACE DIMENSION AS FOLLOWS
C                   2 --- TWO   DIMENSIONAL PROBLEM
C                   3 --- THREE DIMENSIONAL PROBLEM
C          JELM        ; SPECIFIES THE VARIABLE DEFINITION AS FOLLOWS
C                   0 --- NORDALLY DEFINED
C                   1 --- ELEMENT  DEFINED
C          S (IE OR IP); ELEMENT OR NODE ASSIGNED VARIABLE
C          NODE  (I,IE); NODE TABLE
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          NP          ; NUMBER OF TOTAL    NODES
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT
C          DELTA   (IE); CHARACTERISTIC ELEMENT DIMENION
C
C       (2) OUTPUT
C          AVR         ; VOLUMETRIC AVERAGE OF THE FIELD
C
C       (3) WORK
C          F       (IE); ELEMENT VALUE OF S(IP)
C
C
      IF(JELM.EQ.1) THEN
          DO 100 IE = 1 , NE
              F(IE) = S(IE)
  100     CONTINUE
      ELSE
          IF(IDIM.EQ.2) THEN
              DO 200 IE = 1 , NE
                  F(IE) = 0.250D0*(S(NODE(1,IE))+S(NODE(3,IE))
     &                            +S(NODE(2,IE))+S(NODE(4,IE)))
  200         CONTINUE
          ELSE
              DO 300 IE = 1 , NE
                  F(IE) = 0.125D0*(S(NODE(1,IE))+S(NODE(5,IE))
     &                            +S(NODE(2,IE))+S(NODE(6,IE))
     &                            +S(NODE(3,IE))+S(NODE(7,IE))
     &                            +S(NODE(4,IE))+S(NODE(8,IE)))
  300         CONTINUE
          ENDIF
      ENDIF
C
      SIG1 = 0.D0 
      SIG2 = 0.E0
      IF(IDIM.EQ.2) THEN
          DO 400 IE = 1 , NE
               SIG1 = SIG1+DELTA(IE)**2*F(IE)
               SIG2 = SIG2+DELTA(IE)**2
  400     CONTINUE
      ELSE
          DO 500 IE = 1 , NE
               SIG1 = SIG1+DELTA(IE)**3*F(IE)
               SIG2 = SIG2+DELTA(IE)**3
  500     CONTINUE
      ENDIF
C
      AVR = SIG1/SIG2
C
C
      RETURN
      END
