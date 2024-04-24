C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    CENTRG                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE CENTRG(X,Y,NODE,NE,NP,N,IEM,NM,XM,YM)
      IMPLICIT REAL*4 (A-H,O-Z)
      DIMENSION X(NP),Y(NP),NODE(N,NE),IEM(NM),XM(NM),YM(NM)
C
C
C      CALCULATE ELEMENT CENTER COORDINATE
C         ( 2-D GRAPHICS )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          X       (IP); X-DIR. COORDINATE OF NODE
C          Y       (IP); Y-DIR. COORDINATE OF NODE
C          NODE  (I,IE); NODE TABLE BASED ON ELEMENT
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          NP          ; NUMBER OF TOTAL NODES
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT
C          IEM     (IM); ELEMENT NO. SPECIFIED
C          NM          ; NUMBER OF ELEMENTS SPECIFIED
C
C       (2) OUTPUT
C          XM      (IM); ELEMENT CENTER COORDINATE ( X-DIR. )
C          YM      (IM); ELEMENT CENTER COORDINATE ( Y-DIR. )
C
C
      DO 100 IM = 1 , NM
           XM(IM) = X(NODE(1,IEM(IM)))+X(NODE(2,IEM(IM)))
     &             +X(NODE(3,IEM(IM)))+X(NODE(4,IEM(IM)))
           YM(IM) = Y(NODE(1,IEM(IM)))+Y(NODE(2,IEM(IM)))
     &             +Y(NODE(3,IEM(IM)))+Y(NODE(4,IEM(IM)))
  100 CONTINUE
C
      DO 200 IM = 1 , NM
           XM(IM) = 0.25D0*XM(IM)
           YM(IM) = 0.25D0*YM(IM)
  200 CONTINUE
C
C
      RETURN
      END
