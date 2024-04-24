C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    INTERP                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE INTERP(S,X,Y,NODE,NE,NP,N,IE,XP,YP,
     *                  SP,G,E,NMAX,EPS,IUT0,IWRN,IRN)
      IMPLICIT REAL*4 (A-H,O-Z)
      DIMENSION S(NP),X(NP),Y(NP),NODE(N,NE)
C
      CHARACTER*60 WRMSG
      CHARACTER*60 WREXP1
      CHARACTER*60 WREXP2
C
      DIMENSION GI(4),EI(4),XE(4),YE(4),SE(4)
      DATA GI / -1.E0 ,  1.E0 ,  1.E0 ,  -1.E0 /
      DATA EI / -1.E0 , -1.E0 ,  1.E0 ,   1.E0 /
C
      WRMSG  = ' *** SUBROUTINE INTERP ISSUES WARNING '
      WREXP1 = ' INTERPOLATION WAS NOT CONVERGED '
      WREXP2 = ' SPECIFIED POINT IS NOT INCLUDED IN THE ELEMENT '
C
      GINIT = 0.E0
      EINIT = 0.E0
C
C
C      INTERPOLATE SCALAR VALUE AT A SPECIFIED POINT
C         ( 2-D GRAPHICS )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          S     (IP)  ; SCALAR VALUES DEFINED     AT NODES
C          X     (IP)  ; X-DIR. COORDINATE         AT NODES
C          Y     (IP)  ; Y-DIR. COORDINATE         AT NODES
C          NODE(I,IE)  ; NODE NUMBER TABLE
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          NP          ; NUMBER OF TOTAL NODES
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT
C          IE          ; ELEMENT NO. WHERE GIVEN TEH POINT INCLUDED
C          XP          ; X-DIR. COORDINATE OF GIVEN POINT
C          YP          ; Y-DIR. COORDINATE OF GIVEN POINT
C          NMAX        ; MAX. ITERATION NUMBER FOR NEWTON LAPSON METHOD
C          EPS         ; CONVERGENCE CRITERIA
C          IUT0       ; DEVICE NUMBER TO ISSUE WARNING (IF THIS IS SET
C                       ZERO, WARNING WILL BE, IF ANY, SURPRESSED.
C
C       (2) OUTPUT
C          SP          ; INTERPOLATED SCALAR VALUE
C          G           ; GZI COORDINATES OF THE POINT
C          E           ; ETA COORDINATES OF THE POINT
C          IWRN        ; RETURN CODE TO ISSUE WARNING
C                   0 --- NORMAL TERMINATION
C                   1 --- WARNING ISSUED
C          IRN         ; CALCULATION ITERATED NUMBER (IF CALCULATION
C                       IS NOT CONVERGED WITHIN THE MAXIMUM ITERATION
C                       NUMBER SPECIFIED, THIS CODE WILL BE SET TO ZERO)
C
C
      IWRN = 0
      IRN  = 0
      DO 10 I = 1 , N
          XE(I) = X(NODE(I,IE))
          YE(I) = Y(NODE(I,IE))
          SE(I) = S(NODE(I,IE))
   10 CONTINUE
C
      ESIZE = 0.5E0*(SQRT((XE(3)-XE(1))**2+(YE(3)-YE(1))**2)
     &              +SQRT((XE(4)-XE(2))**2+(YE(4)-YE(2))**2))
C
      GEX = 0.E0
      GEY = 0.E0
      GX  = 0.E0
      GY  = 0.E0
      EX  = 0.E0
      EY  = 0.E0
      CX  = 0.E0
      CY  = 0.E0
C
      DO 20 I = 1 , N
          GEX = GEX+0.25E0*GI(I)*EI(I)*XE(I)
          GEY = GEY+0.25E0*GI(I)*EI(I)*YE(I)
          GX  = GX +0.25E0*GI(I)*XE(I)
          GY  = GY +0.25E0*GI(I)*YE(I)
          EX  = EX +0.25E0*EI(I)*XE(I)
          EY  = EY +0.25E0*EI(I)*YE(I)
          CX  = CX +0.25E0*XE(I)
          CY  = CY +0.25E0*YE(I)
   20 CONTINUE
C
      G = GINIT
      E = EINIT
      DO 100 ITER = 1 , NMAX
          DFG = GEX*E+GX
          DFE = GEX*G+EX
          DHG = GEY*E+GY
          DHE = GEY*G+EY
C
          DET = DFG*DHE-DFE*DHG
          F = GEX*G*E+GX*G+EX*E+CX-XP
          H = GEY*G*E+GY*G+EY*E+CY-YP
C
          DG = 1.E0/DET*(-DHE*F+DFE*H)
          DE = 1.E0/DET*( DHG*F-DFG*H)
          G = G+DG
          E = E+DE
C
          XPC = 0.E0
          YPC = 0.E0
          SP  = 0.E0
          DO 30 I = 1 , N
              SHAPE = 0.25E0*(1.E0+GI(I)*G)*(1.E0+EI(I)*E)
              XPC   = XPC+SHAPE*XE(I)
              YPC   = YPC+SHAPE*YE(I)
              SP    = SP +SHAPE*SE(I)
   30     CONTINUE
          DIFF = SQRT((XPC-XP)**2+(YPC-YP)**2)/ESIZE
          IF(DIFF.LE.EPS) THEN
              IRN = ITER
              IF((G+1.E0)*(G-1.E0).GT.0 .OR.
     &           (E+1.E0)*(E-1.E0).GT.0) THEN
                  IF(IUT0.NE.0) THEN
                      WRITE(IUT0,*) WRMSG
                      WRITE(IUT0,*) WREXP2
                  ENDIF
                  IWRN = 1
              ENDIF
              RETURN
          ENDIF
  100 CONTINUE
C
      IF(IUT0.NE.0) THEN
          WRITE(IUT0,*) WRMSG
          WRITE(IUT0,*) WREXP1
          WRITE(IUT0,*) '  RELATIVE ERROR = ' , DIFF
      ENDIF
C
C
      RETURN
      END
