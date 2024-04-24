C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    FSETUP                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE FSETUP(DT,U,V,W,NODE,IENP,NEP,MAXEP,NE,NP,N,GI,EI,TI,
     *                  MEPFR,NPFREE,LPFREE,IEUP,XPFREE,YPFREE,ZPFREE,
     *                  INDXFR,NEFREE,ELMFR,NITRFR,
     *                  GPFREE,EPFREE,TPFREE,UPFREE,VPFREE,WPFREE,IWRK,
     *                  IUT0,IWRN)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION U(NP),V(NP),W(NP),
     1          NODE(N,NE),IENP(MAXEP,NP),NEP(NP),GI(N),EI(N),TI(N),
     2          LPFREE(NPFREE),IEUP  (NPFREE),
     3          XPFREE(NPFREE),YPFREE(NPFREE),ZPFREE(NPFREE),
     4          INDXFR(MEPFR,NPFREE),ELMFR(24,NEFREE),
     5          GPFREE(NPFREE),EPFREE(NPFREE),TPFREE(NPFREE),
     6          UPFREE(NPFREE),VPFREE(NPFREE),WPFREE(NPFREE),
     7          IWRK  (NPFREE)
C
      DATA EPS     / 1.0E-3 /
C
      CHARACTER*60 WRNMSG
     & / ' ## SUBROUTINE FSETUP: NO UPWIND ELEMENT FOUND FOR NODE IP=' /
C
C
C      SET FREE BOUNDARY VELOCITY COMPONENT
C         ( 3-D CALCULATION )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          DT          ; TIME INCTREMENT
C          U       (IP); X-DIR. VELOCITY COMPONENT
C          V       (IP); Y-DIR. VELOCITY COMPONENT
C          W       (IP); Z-DIR. VELOCITY COMPONENT
C          NODE  (I,IE); NODE NO. TABLE BASED ON ELEMENT
C          IENP(IEP,IP); ADJACENT ELEMENT NUMBER TO NODE IP
C          NEP     (IP); NUMBER OF ADJACENT ELEMENTS TO NODE IP
C          MAXEP       ; THE FIRST DIMENSION OF ARRAY IENP
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          NP          ; NUMBER OF TOTAL    NODES
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT
C          GI       (I); LOCAL GZAI  COORDINATES OF ELEMENT'S NODES
C          EI       (I); LOCAL EATA  COORDINATES OF ELEMENT'S NODES
C          TI       (I); LOCAL THETA COORDINATES OF ELEMENT'S NODES
C
C          MEPFR       ; MAX. NUMBER OF ADJACENT FREE BOUNDARY ELEMENTS
C          NPFREE      ; NUMBER OF FREE BOUNDARY NODES
C          LPFREE (IBP); FREE BOUNDARY NODES
C          IEUP   (IBP); UPWIND REFERENCE ELEMENT OF FREE BOUNDARY NODES
C           NOTES; IEUP(IBP) IS AN INPUT-OUTPUT ARGUMENT WHICH KEEPS
C                 CURRENT UPWINDING REFERENCE ELEMENTS FOR FREE BOUNDARY
C                 NODES. WHEN THIS ARGUMENT HAS ZERO VALUE, NO UPWIND
C                 REFERENCE ELEMENT IS SET FOR THAT PARTICULAR FREE
C                 BOUNDARY NODE.
C          XPFREE (IBP); GLOBAL X-COORDINATES     OF FREE BOUNDARY NODES
C          YPFREE (IBP); GLOBAL Y-COORDINATES     OF FREE BOUNDARY NODES
C          ZPFREE (IBP); GLOBAL Z-COORDINATES     OF FREE BOUNDARY NODES
C          INDXFR      ; INDICATES REGIONAL ELEMENT NUMBER 'IBE' OF THE
C           (IEPFR,IBP) 'IEPFR' TH ADJACENT ELEMENT OF FREE BOUNDARY
C                        NODE 'IBP'
C          NEFREE      ; NUMBER OF FREE BOUNDARY ELEMENTS
C          ELMFR(K,IBE); ELEMENT POSION AND SHAPE DEPENDENT CONSTANTS
C                       FOR ELEMENTS ADJACENT TO FREE BOUNDARY NODES
C          NITRFR      ; ITERATIONS MADE FOR UPWIND ELEMENT SEARCH
C                       FOR FREE BOUNDARY NODES
C
C          IUT0        ; FILE NUMBER TO REPORT ERROR OCCURENCE
C
C       (2) OUTPUT
C          UPFREE (IBP); X-DIR. FREE BOUNDARY VELOCITY COMPONENT
C          VPFREE (IBP); Y-DIR. FREE BOUNDARY VELOCITY COMPONENT
C          WPFREE (IBP); Z-DIR. FREE BOUNDARY VELOCITY COMPONENT
C
C          IWRN        ; A WARNING FLAG WHICH WILL BE SET (TO ONE),
C                       IF NO UPWIND REFERENCE ELEMENT IS FOUND FOR A 
C                       FREE BOUNDARY NODE
C
C       (4) WORK
C          GPFREE (IBP); STORES LOCAL GZAI   COORDINATES OF FREE NODES
C          EPFREE (IBP); STORES LOCAL EATA   COORDINATES OF FREE NODES
C          TPFREE (IBP); STORES LOCAL THEATA COORDINATES OF FREE NODES
C          IWRK   (IBP); STORES REGIONAL ELEMENT NUMBER  OF FREE NODES
C
C
      IWRN = 0
C
C
C SEARCH FOR UPWIND REFERENCE ELEMENTS
C
C
C*$*ASSERT PERMUTATION ( LPFREE )
      DO 100 IPFREE = 1 , NPFREE
          UPFREE(IPFREE) = XPFREE(IPFREE)-DT*U(LPFREE(IPFREE))
          VPFREE(IPFREE) = YPFREE(IPFREE)-DT*V(LPFREE(IPFREE))
          WPFREE(IPFREE) = ZPFREE(IPFREE)-DT*W(LPFREE(IPFREE))
  100 CONTINUE
C
      DO 210 IEP = 1 , MEPFR
C*$*ASSERT PERMUTATION ( LPFREE )
          DO 200 IPFREE = 1 , NPFREE
              IP = LPFREE(IPFREE)
              IE = IEUP  (IPFREE)
              IF(IEP.GT.NEP     (IP)) GO TO 200
              IF(IE .EQ.IENP(IEP,IP)) IWRK(IPFREE) = INDXFR(IEP,IPFREE)
  200     CONTINUE
  210 CONTINUE
C
      DO 510 IEP = 0 , MEPFR
          DO 300 IPFREE = 1 , NPFREE
              GPFREE(IPFREE) = 0.E0
              EPFREE(IPFREE) = 0.E0
              TPFREE(IPFREE) = 0.E0
  300     CONTINUE
C
          DO 410 ITER = 1 , NITRFR
C*$*ASSERT PERMUTATION ( LPFREE )
              DO 400 IPFREE = 1 , NPFREE
                  IP = LPFREE(IPFREE)
C
                  IF(IEP.EQ.0       .AND. IEUP(IPFREE).EQ.0) GO TO 400
                  IF(IEP.GE.1       .AND. IEUP(IPFREE).GE.1) GO TO 400
                  IF(IEP.GT.NEP(IP)                        ) GO TO 400
C
                  IF(IEP.EQ.0) IEFREE = IWRK      (IPFREE)
                  IF(IEP.GE.1) IEFREE = INDXFR(IEP,IPFREE)
C
                  DFG=ELMFR( 4,IEFREE)+ELMFR(13,IEFREE)*EPFREE(IPFREE)
     &                                +ELMFR(19,IEFREE)*TPFREE(IPFREE)
     &                 +ELMFR(22,IEFREE)*EPFREE(IPFREE)*TPFREE(IPFREE)
C
                  DGG=ELMFR( 5,IEFREE)+ELMFR(14,IEFREE)*EPFREE(IPFREE)
     &                                +ELMFR(20,IEFREE)*TPFREE(IPFREE)
     &                 +ELMFR(23,IEFREE)*EPFREE(IPFREE)*TPFREE(IPFREE)
C
                  DHG=ELMFR( 6,IEFREE)+ELMFR(15,IEFREE)*EPFREE(IPFREE)
     &                                +ELMFR(21,IEFREE)*TPFREE(IPFREE)
     &                 +ELMFR(24,IEFREE)*EPFREE(IPFREE)*TPFREE(IPFREE)
C
                  DFE=ELMFR( 7,IEFREE)+ELMFR(16,IEFREE)*TPFREE(IPFREE)
     &                                +ELMFR(13,IEFREE)*GPFREE(IPFREE)
     &                 +ELMFR(22,IEFREE)*TPFREE(IPFREE)*GPFREE(IPFREE)
C
                  DGE=ELMFR( 8,IEFREE)+ELMFR(17,IEFREE)*TPFREE(IPFREE)
     &                                +ELMFR(14,IEFREE)*GPFREE(IPFREE)
     &                 +ELMFR(23,IEFREE)*TPFREE(IPFREE)*GPFREE(IPFREE)
C
                  DHE=ELMFR( 9,IEFREE)+ELMFR(18,IEFREE)*TPFREE(IPFREE)
     &                                +ELMFR(15,IEFREE)*GPFREE(IPFREE)
     &                 +ELMFR(24,IEFREE)*TPFREE(IPFREE)*GPFREE(IPFREE)
C
                  DFT=ELMFR(10,IEFREE)+ELMFR(19,IEFREE)*GPFREE(IPFREE)
     &                                +ELMFR(16,IEFREE)*EPFREE(IPFREE)
     &                 +ELMFR(22,IEFREE)*GPFREE(IPFREE)*EPFREE(IPFREE)
C
                  DGT=ELMFR(11,IEFREE)+ELMFR(20,IEFREE)*GPFREE(IPFREE)
     &                                +ELMFR(17,IEFREE)*EPFREE(IPFREE)
     &                 +ELMFR(23,IEFREE)*GPFREE(IPFREE)*EPFREE(IPFREE)
C
                  DHT=ELMFR(12,IEFREE)+ELMFR(21,IEFREE)*GPFREE(IPFREE)
     &                                +ELMFR(18,IEFREE)*EPFREE(IPFREE)
     &                 +ELMFR(24,IEFREE)*GPFREE(IPFREE)*EPFREE(IPFREE)
C
                  FV =ELMFR( 1,IEFREE)-UPFREE(IPFREE)
     &               +ELMFR( 4,IEFREE)*GPFREE(IPFREE)
     &               +ELMFR( 7,IEFREE)*EPFREE(IPFREE)
     &               +ELMFR(10,IEFREE)*TPFREE(IPFREE)
     &               +ELMFR(13,IEFREE)*GPFREE(IPFREE)*EPFREE(IPFREE)
     &               +ELMFR(16,IEFREE)*EPFREE(IPFREE)*TPFREE(IPFREE)
     &               +ELMFR(19,IEFREE)*TPFREE(IPFREE)*GPFREE(IPFREE)
     &               +ELMFR(22,IEFREE)*GPFREE(IPFREE)*EPFREE(IPFREE)
     &               *TPFREE(IPFREE)
C
                  GV =ELMFR( 2,IEFREE)-VPFREE(IPFREE)
     &               +ELMFR( 5,IEFREE)*GPFREE(IPFREE)
     &               +ELMFR( 8,IEFREE)*EPFREE(IPFREE)
     &               +ELMFR(11,IEFREE)*TPFREE(IPFREE)
     &               +ELMFR(14,IEFREE)*GPFREE(IPFREE)*EPFREE(IPFREE)
     &               +ELMFR(17,IEFREE)*EPFREE(IPFREE)*TPFREE(IPFREE)
     &               +ELMFR(20,IEFREE)*TPFREE(IPFREE)*GPFREE(IPFREE)
     &               +ELMFR(23,IEFREE)*GPFREE(IPFREE)*EPFREE(IPFREE)
     &               *TPFREE(IPFREE)
C
                  HV =ELMFR( 3,IEFREE)-WPFREE(IPFREE)
     &               +ELMFR( 6,IEFREE)*GPFREE(IPFREE)
     &               +ELMFR( 9,IEFREE)*EPFREE(IPFREE)
     &               +ELMFR(12,IEFREE)*TPFREE(IPFREE)
     &               +ELMFR(15,IEFREE)*GPFREE(IPFREE)*EPFREE(IPFREE)
     &               +ELMFR(18,IEFREE)*EPFREE(IPFREE)*TPFREE(IPFREE)
     &               +ELMFR(21,IEFREE)*TPFREE(IPFREE)*GPFREE(IPFREE)
     &               +ELMFR(24,IEFREE)*GPFREE(IPFREE)*EPFREE(IPFREE)
     &               *TPFREE(IPFREE)
C
                  DET = DFG*(DGE*DHT-DGT*DHE)
     &                 +DFE*(DGT*DHG-DGG*DHT)
     &                 +DFT*(DGG*DHE-DGE*DHG)
C
                  A11 = (DGE*DHT-DGT*DHE)/DET
                  A21 = (DGT*DHG-DGG*DHT)/DET
                  A31 = (DGG*DHE-DGE*DHG)/DET
                  A12 = (DHE*DFT-DHT*DFE)/DET
                  A22 = (DHT*DFG-DHG*DFT)/DET
                  A32 = (DHG*DFE-DHE*DFG)/DET
                  A13 = (DFE*DGT-DFT*DGE)/DET
                  A23 = (DFT*DGG-DFG*DGT)/DET
                  A33 = (DFG*DGE-DFE*DGG)/DET
C
                  GPFREE(IPFREE) = GPFREE(IPFREE)-A11*FV-A12*GV-A13*HV
                  EPFREE(IPFREE) = EPFREE(IPFREE)-A21*FV-A22*GV-A23*HV
                  TPFREE(IPFREE) = TPFREE(IPFREE)-A31*FV-A32*GV-A33*HV
  400         CONTINUE
  410     CONTINUE
C
C*$*ASSERT PERMUTATION ( LPFREE )
          DO 500 IPFREE = 1 , NPFREE
              IF(IEP.EQ.0 .AND. IEUP(IPFREE).EQ.0) GO TO 500
              IF(IEP.GE.1 .AND. IEUP(IPFREE).GE.1) GO TO 500
C
              IF(IEP.EQ.0 .AND. (      ABS(GPFREE(IPFREE)).GT.1.0+EPS
     &                           .OR.  ABS(EPFREE(IPFREE)).GT.1.0+EPS
     &                           .OR.  ABS(TPFREE(IPFREE)).GT.1.0+EPS))
     &        IEUP(IPFREE) = 0
C
              IF(IEP.GE.1 .AND. (      ABS(GPFREE(IPFREE)).LE.1.0+EPS
     &                           .AND. ABS(EPFREE(IPFREE)).LE.1.0+EPS
     &                           .AND. ABS(TPFREE(IPFREE)).LE.1.0+EPS))
     &        IEUP(IPFREE) = IENP(IEP,LPFREE(IPFREE))
  500     CONTINUE
  510 CONTINUE
C
      DO 520 IPFREE = 1 , NPFREE
          IF(IEUP(IPFREE).EQ.0) THEN
              WRITE(IUT0,*) WRNMSG, LPFREE(IPFREE)
              IWRN = 1
          ENDIF
  520 CONTINUE
C
C
C INTERPOLATE VELOCITY COMPONENTS
C
C
      DO 600 IPFREE = 1 , NPFREE
          IF(IEUP(IPFREE).EQ.0) GO TO 600
          G = GPFREE(IPFREE)
          E = EPFREE(IPFREE)
          T = TPFREE(IPFREE)
C
          IP1 = NODE(1,IEUP(IPFREE))
          IP2 = NODE(2,IEUP(IPFREE))
          IP3 = NODE(3,IEUP(IPFREE))
          IP4 = NODE(4,IEUP(IPFREE))
          IP5 = NODE(5,IEUP(IPFREE))
          IP6 = NODE(6,IEUP(IPFREE))
          IP7 = NODE(7,IEUP(IPFREE))
          IP8 = NODE(8,IEUP(IPFREE))
C
          S1  =  0.125E0*(1.E0+GI(1)*G)*(1.E0+EI(1)*E)*(1.E0+TI(1)*T)
          S2  =  0.125E0*(1.E0+GI(2)*G)*(1.E0+EI(2)*E)*(1.E0+TI(2)*T)
          S3  =  0.125E0*(1.E0+GI(3)*G)*(1.E0+EI(3)*E)*(1.E0+TI(3)*T)
          S4  =  0.125E0*(1.E0+GI(4)*G)*(1.E0+EI(4)*E)*(1.E0+TI(4)*T)
          S5  =  0.125E0*(1.E0+GI(5)*G)*(1.E0+EI(5)*E)*(1.E0+TI(5)*T)
          S6  =  0.125E0*(1.E0+GI(6)*G)*(1.E0+EI(6)*E)*(1.E0+TI(6)*T)
          S7  =  0.125E0*(1.E0+GI(7)*G)*(1.E0+EI(7)*E)*(1.E0+TI(7)*T)
          S8  =  0.125E0*(1.E0+GI(8)*G)*(1.E0+EI(8)*E)*(1.E0+TI(8)*T)
C
          UPFREE(IPFREE)= S1*U(IP1)+S2*U(IP2)+S3*U(IP3)+S4*U(IP4)
     &                   +S5*U(IP5)+S6*U(IP6)+S7*U(IP7)+S8*U(IP8)
C
          VPFREE(IPFREE)= S1*V(IP1)+S2*V(IP2)+S3*V(IP3)+S4*V(IP4)
     &                   +S5*V(IP5)+S6*V(IP6)+S7*V(IP7)+S8*V(IP8)
C
          WPFREE(IPFREE)= S1*W(IP1)+S2*W(IP2)+S3*W(IP3)+S4*W(IP4)
     &                   +S5*W(IP5)+S6*W(IP6)+S7*W(IP7)+S8*W(IP8)
  600 CONTINUE
C
C
      RETURN
      END
