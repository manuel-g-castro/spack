C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    FIND2D                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE FIND2D(X,Y,EX1,EX2,EX3,EY1,EY2,EY3,DET1,DET2,
     *                  NODE,NE,N,XM,YM,NM,IEM,IUT0,IWRN)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X(*),Y(*),
     1          EX1 (NE),EX2 (NE),EX3   (NE),EY1(NE),EY2(NE),EY3(NE),
     1          DET1(NE),DET2(NE),NODE(N,NE),XM (NM),YM (NM),IEM(NM)
C
      CHARACTER*72 WRMSG1
     & /' *** SUBROUTINE FIND2D ISSUES A WARNING                ***  ' /
      CHARACTER*72 WREXP1
     & /' SPECIFIED POINT IS OUT OF THE COMPUTAION DOMAIN            ' /
C
      D = 1.D-1
C
C
C      FIND ELEMENTS WHICH INCLUDE SPECIFIED POINTS
C         ( 2-D CALCULATION )
C
C
C     NOTE ; 1. IF A SPECIFIED POINT DOES NOT BELONG TO ANY ELEMENT,
C              SUBROUTINE FIND2D WILL ISSUE A WARNING AND CONTINUE
C              THE NORMAL PROCESS.
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          X (IP)      ; X-DIR. COORDINATES OF NODES
C          Y (IP)      ; Y-DIR. COORDINATES OF NODES
C          EX1 (IE)    ; ELEMENT VECTORS X(NODE(2,IE))-X(NODE(1,IE))
C          EX2 (IE)    ; ELEMENT VECTORS X(NODE(3,IE))-X(NODE(1,IE))
C          EX3 (IE)    ; ELEMENT VECTORS X(NODE(4,IE))-X(NODE(1,IE))
C          EY1 (IE)    ; ELEMENT VECTORS Y(NODE(2,IE))-Y(NODE(1,IE))
C          EY2 (IE)    ; ELEMENT VECTORS Y(NODE(3,IE))-Y(NODE(1,IE))
C          EY3 (IE)    ; ELEMENT VECTORS Y(NODE(4,IE))-Y(NODE(1,IE))
C          DET1(IE)    ; DETERMINANTS ( EX1 , EY1 , EX2 , EY2 )
C          DET2(IE)    ; DETERMINANTS ( EX2 , EY2 , EX3 , EY3 )
C
C          NODE(I,IE)  ; NODE TABLE
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT
C
C          XM (IM)     ; X-COORDINATES OF THE SPECIFIED POINTS
C          YM (IM)     ; Y-COORDINATES OF THE SPECIFIED POINTS
C          NM          ; NUMBER OF THE SPECIFIED POINTS
C          IUT0        ; DEVICE NUMBER TO ISSUE A WARNING
C
C       (2) OUTPUT
C          IEM(IM)     ; ELEMENT NO.'S FOUND
C                       IF NO ELEMENT FOUND FOR A POINT, THEN IEM WILL
C                      BE SET TO ZERO FOR THAT POINT.
C          IWRN        ; RETURN CODE TO REPORT A WARNING
C                   0 --- NORMAL TERMINATION
C                   1 --- A WARNING ISSUED
C
C
      IWRN = 0
C
      DO 110 IM = 1 , NM
          IFOUND = 0
          DO 100 IE = 1 , NE
              XF = XM(IM)-X(NODE(1,IE))
              YF = YM(IM)-Y(NODE(1,IE))
              A1 = ( EY2(IE)*XF-EX2(IE)*YF)/DET1(IE)
              B1 = (-EY1(IE)*XF+EX1(IE)*YF)/DET1(IE)
              A2 = ( EY3(IE)*XF-EX3(IE)*YF)/DET2(IE)
              B2 = (-EY2(IE)*XF+EX2(IE)*YF)/DET2(IE)
C
              IF(A1.GE.-D .AND. B1.GE.-D .AND. A1+B1.LE.1.D0+D  .OR.
     &           A2.GE.-D .AND. B2.GE.-D .AND. A2+B2.LE.1.D0+D) THEN
                  IFOUND = IE
              ENDIF
  100     CONTINUE
          IF(IFOUND.EQ.0) THEN
              WRITE(IUT0,6300) WRMSG1
              WRITE(IUT0,6300) WREXP1 
              IWRN = 1
              WRITE(IUT0,6000) IM
          ENDIF
          IEM(IM) = IFOUND
  110 CONTINUE
C
C
      RETURN
 6000 FORMAT(20H   *** POINT NO. ***, I5/)
 6300 FORMAT(A72)
      END
