C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    DERIVL                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE DERIVL(G,E,XE,YE,N,DNX,DNY)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION XE(N),YE(N),DNX(N),DNY(N)
C
      DIMENSION DNG(4),DNE(4)
C
C
C      CALCULATE X,Y DERIVERTIVES OF SHAPE FUNCTION
C     AT A SPECIFIED POINT IN AN ELEMENT
C         ( 2-D GRAPHICS )
C
C
C     NOTE ; 1. CALCULATION IS CARRIED OUT AT A GIVEN LOCATION(G,E)
C              IN ONE ELEMENT.
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          G,E         ; GZAI, EATA COORDINATE WHERE CALCULATION DONE
C          XE (I)      ; X-COORDINATE OF NODES CONSTITUTING THE ELEMENT
C          YE (I)      ; Y-COORDINATE OF NODES CONSTITUTING THE ELEMENT
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT
C
C       (2) OUTPUT
C          DNX(I)      ; X-DERIVERTIVES OF SHAPE FUNCTION N(I)
C          DNI(I)      ; Y-DERIVERTIVES OF SHAPE FUNCTION N(I)
C
C
C
C
      DO 10 I = 1 , N
            DNG(I) = SHAPEG(I,G,E,2)
            DNE(I) = SHAPEG(I,G,E,3)
   10 CONTINUE
C
      DXG = DNG(1)*XE(1)+DNG(2)*XE(2)+DNG(3)*XE(3)+DNG(4)*XE(4)
      DYG = DNG(1)*YE(1)+DNG(2)*YE(2)+DNG(3)*YE(3)+DNG(4)*YE(4)
      DXE = DNE(1)*XE(1)+DNE(2)*XE(2)+DNE(3)*XE(3)+DNE(4)*XE(4)
      DYE = DNE(1)*YE(1)+DNE(2)*YE(2)+DNE(3)*YE(3)+DNE(4)*YE(4)
C
      DET = DXG*DYE-DYG*DXE
C
      DO 20 I = 1 , N
          DNX(I) = ( DYE*DNG(I)-DYG*DNE(I))/DET
          DNY(I) = (-DXE*DNG(I)+DXG*DNE(I))/DET
   20 CONTINUE
C
C
      RETURN
      END
