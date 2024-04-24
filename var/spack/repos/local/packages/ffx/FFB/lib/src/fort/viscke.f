C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    VISCKE                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE VISCKE(IDIM,CNUE,CLMTK,CLMTE,TK,TE,NODE,NE,NP,N,
     *                  NEAR,DSNEAR,UTAU,NEWALL,AP,VISCM,VISC,VISCAV)
     *                  
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION TK(NP),TE(NP),NODE(N,NE),
     1          NEAR(NE),DSNEAR(NE),UTAU(NEWALL),VISC(NE)
C
      DATA EPS / 1.0E-20 /
C
C
C      CALCULATE ELEMENT VISCOSITY BY K-EPSILON MODEL WITH VAN-DRIEST
C     DAMPING FUNCTION
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          IDIM        ; SPATIAL DIMENSION ( 2 OR 3 )
C          CNUE        ; CONSTANT FOR TURBULENT KINEMATIC VISCOSITY 
C          CLMTK       ; LOWER BOUND OF TURBULENT ENERGY
C          CLMTE       ; LOWER BOUND OF DISSIPATION RATE OF TURBULENT E.
C          TK      (IP); TURBULENT KINETIC ENERGY
C          TE      (IP); DISSIPATION RATE OF TURBULENT KINETIC ENERGY
C          NODE  (I,IE); NODE NO. TABLE BASED ON ELEMENT
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          NP          ; NUMBER OF TOTAL     NODES
C          N           ; NUMBER OF NODE ASSIGNED TO ONE ELEMENT
C                 
C          NEAR    (IE); NEAREST WALL SURFACE
C          DSNEAR  (IE); DISTANCE TO THE NEAREST WALL SURFACE
C          UTAU   (IBE); FRICTION VELOCITY AT WALL SURFACES
C          NEWALL      ; NUMBER OF WALL BOUNDARY SURFACES
C          AP          ; VAN-DRIEST DAMPING CONSTANT
C          VISCM       ; MOLECULAR VISCOSITY
C
C       (2) OUTPUT
C          VISC    (IE); ELEMENT VISCOSITY ( MOLECULAR+TURBULENT )
C          VISCAV      ; SPATIALLY AVERAGED TURBULENT EDDY VISCOSITY
C
C
C
      DO 100 IE = 1 , NE
          TKG = TK(NODE(1,IE))+TK(NODE(2,IE))
     &         +TK(NODE(3,IE))+TK(NODE(4,IE))
          TEG = TE(NODE(1,IE))+TE(NODE(2,IE))
     &         +TE(NODE(3,IE))+TE(NODE(4,IE))
          IF(IDIM.EQ.3) THEN
              TKG = TKG+TK(NODE(5,IE))+TK(NODE(6,IE))
     &                 +TK(NODE(7,IE))+TK(NODE(8,IE))
              TEG = TEG+TE(NODE(5,IE))+TE(NODE(6,IE))
     &                 +TE(NODE(7,IE))+TE(NODE(8,IE))
          ENDIF
          TKG = TKG/FLOAT(N)
          TEG = TEG/FLOAT(N)
C
          IF(TKG.LE.CLMTK+EPS .OR. TEG.LE.CLMTE+EPS) THEN
              VISC(IE) = 0.E0
          ELSE
              VISC(IE) = CNUE*TKG**2/(TEG+EPS)
          ENDIF
  100 CONTINUE
C
C
      IF(NEWALL.GE.1) THEN
          DO 200 IE = 1 , NE
              YP = UTAU(NEAR(IE))*DSNEAR(IE)/VISCM
              VISC(IE) = VISC(IE)*(1.E0-EXP(-YP/AP))**2
  200     CONTINUE
      ENDIF
C
C
      VISCAV = 0.E0
      DO 300 IE = 1 , NE
          VISCAV   = VISCAV+VISC(IE)/NE
          VISC(IE) = VISCM +VISC(IE)
  300 CONTINUE
C
C
      RETURN
      END
