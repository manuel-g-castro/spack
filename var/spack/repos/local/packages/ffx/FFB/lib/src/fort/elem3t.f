C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    ELEM3T                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE ELEM3T(X,Y,Z,NODE,NES,NE,N,LAPEX,NAPEX,NPOLY,
     *                  E,NDIM)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION X(*),Y(*),Z(*),NODE(N,*),LAPEX(NAPEX,*),
     1          E(NDIM,NDIM,NPOLY,*)
C
      DATA EPS / 1.0E-30 /
C
C
C      CALCULATE INVERSE MATRICES
C     OF ELEMENT BASE VECTOR      ; MARKER TRACING IN 3-D FIELD 0
C         ( 3-D CALCULATION )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          X       (IP); X-DIR. COORDINATE OF NODE
C          Y       (IP); Y-DIR. COORDINATE OF NODE
C          Z       (IP); Z-DIR. COORDINATE OF NODE
C          NODE  (I,IE); NODE TABLE
C          NES         ; FIRST ELEMENT NUMBER
C          NE          ; NUMBER OF TOTAL NODES
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT
C          LAPEX(IA,IG); APEX NO. OF POLYGONS CONSTITUTING AN ELEMENT
C          NAPEX       ; NUMBER OF APEXES CONSTITUTING A POLYGON
C          NPOLY       ; NUMBER OF POLYGONS CONSTITUTING AN ELEMENT
C          NDIM        ; THE FIRST AND SECOND DIMENSION OF ARRAY E
C
C       (2) OUTPUT
C          E(J,I,IG,IE); INVERSE MATRICES OF ELEMENT BASE VECTOR
C
C
C*$* ASSERT DO PREFER(SERIAL)
      DO 110 IPOLY = 1 , NPOLY
          DO 100 IE = NES , NE
              AX = X(NODE(LAPEX(2,IPOLY),IE))-X(NODE(LAPEX(1,IPOLY),IE))
              BX = X(NODE(LAPEX(3,IPOLY),IE))-X(NODE(LAPEX(1,IPOLY),IE))
              CX = X(NODE(LAPEX(4,IPOLY),IE))-X(NODE(LAPEX(1,IPOLY),IE))
              AY = Y(NODE(LAPEX(2,IPOLY),IE))-Y(NODE(LAPEX(1,IPOLY),IE))
              BY = Y(NODE(LAPEX(3,IPOLY),IE))-Y(NODE(LAPEX(1,IPOLY),IE))
              CY = Y(NODE(LAPEX(4,IPOLY),IE))-Y(NODE(LAPEX(1,IPOLY),IE))
              AZ = Z(NODE(LAPEX(2,IPOLY),IE))-Z(NODE(LAPEX(1,IPOLY),IE))
              BZ = Z(NODE(LAPEX(3,IPOLY),IE))-Z(NODE(LAPEX(1,IPOLY),IE))
              CZ = Z(NODE(LAPEX(4,IPOLY),IE))-Z(NODE(LAPEX(1,IPOLY),IE))
C
              DETI = 1.E0/(AX*(BY*CZ-BZ*CY)+AY*(BZ*CX-BX*CZ)
     &                    +AZ*(BX*CY-BY*CX)+EPS)
C
              E(1,1,IPOLY,IE) = DETI*(BY*CZ-BZ*CY)
              E(2,1,IPOLY,IE) = DETI*(BZ*CX-BX*CZ)
              E(3,1,IPOLY,IE) = DETI*(BX*CY-BY*CX)
              E(1,2,IPOLY,IE) = DETI*(CY*AZ-CZ*AY)
              E(2,2,IPOLY,IE) = DETI*(CZ*AX-CX*AZ)
              E(3,2,IPOLY,IE) = DETI*(CX*AY-CY*AX)
              E(1,3,IPOLY,IE) = DETI*(AY*BZ-AZ*BY)
              E(2,3,IPOLY,IE) = DETI*(AZ*BX-AX*BZ)
              E(3,3,IPOLY,IE) = DETI*(AX*BY-AY*BX)
  100     CONTINUE
  110 CONTINUE
C
C
      RETURN
      END
