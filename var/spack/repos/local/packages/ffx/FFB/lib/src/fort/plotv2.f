C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    PLOTV2                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE PLOTV2(JPSOUT,IUTPS,X,Y,VR1,VR2,SR,NP,SCL,AL,AW,
     *                  SRMIN,SRMAX,XMIN,YMIN,SFC,LCL,NCL)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION X(NP),Y(NP),VR1(NP),VR2(NP),SR(NP),LCL(NCL)
C
#ifdef VOS
      INCLUDE (GN)
#else
      INCLUDE 'gn.h'
#endif
C
      DIMENSION XP(3),YP(3)
      DATA EPS / 1.0E-9 /
C
C
C      PLOT VECTOR WITH CORRESPONDING COLOR TO SR(IP)
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
C          VR1     (IP); X-DIR. VECTOR COMPONENT
C          VR2     (IP); Y-DIR. VECTOR COMPONENT
C          SR      (IP); SCALAR
C          NP          ; NUMBER OF TOTAL NODES
C          SCL         ; VECTOR SCALE
C          AL          ; ARROW LENGTH
C          AW          ; ARROW WIDTH
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
      LNTYPE = GLSOLD
      PLTYPE = GLSOLD
C
      LNWDTH = GLNORM
      PLWDTH = GLNORM
C
      DO 10 IP = 1 , NP
          IF(ABS(SRMAX-SRMIN).GE.EPS) THEN
              RATIO = (SR(IP)-SRMIN)/(SRMAX-SRMIN)
          ENDIF
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
          XP(1) = SFC*(X(IP)-XMIN)
          YP(1) = SFC*(Y(IP)-YMIN)
          XP(2) = XP(1)+SCL*VR1(IP)
          YP(2) = YP(1)+SCL*VR2(IP)
C
          ALL=(XP(2)-XP(1))**2+(YP(2)-YP(1))**2
          IF( ALL .GE. EPS) THEN
              CALL GNLINE(XP,YP,2)
              IF(JPSOUT.GE.1) CALL PSLINE(IUTPS,XP,YP,2)
C
              ALL = SQRT(ALL)
              CS = ( XP(2) - XP(1) ) / ALL
              SN = ( YP(2) - YP(1) ) / ALL
              XC = CS * AL
              YC = SN * AL
              XCR = -SN * AW/2.0
              YCR =  CS * AW/2.0
C
              XP(1) = XP(2) + (-XC) + (-XCR)
              YP(1) = YP(2) + (-YC) + (-YCR)
              XP(3) = XP(2) + (-XC) - (-XCR)
              YP(3) = YP(2) + (-YC) - (-YCR)
C
              CALL GNLINE(XP,YP,3)
              IF(JPSOUT.GE.1) CALL PSLINE(IUTPS,XP,YP,3)
          ENDIF
   10 CONTINUE
C
C
      RETURN
      END
