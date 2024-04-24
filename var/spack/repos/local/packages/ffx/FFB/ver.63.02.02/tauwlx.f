C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.8.1                                   C
C                                                                      C
C  SUB ROUTINE : TAUWLX                                                C
C                                                                      C
C                                       WRITTEN BY Y.YAMADE            C
C                                                                      C
C                                                                      C
C CONTACT ADDRESS : IIS, THE UNIVERSITY OF TOKYO, CISS                 C
C                                                                      C
C THERMO-FLUID ANALYSIS SOLVERS FOR LARGE-SCALE-ASSEMBLY               C
C                                                                      C
C======================================================================C
      SUBROUTINE TAUWLX
     *         ( VKAP,B,U,V,W,NODE,NE,NP,N2,NEX,NS,NSP,N2D,
     *           LOCAL,DEVLP2,      
     *           LPWALL,NPWALL,UWALL,VWALL,WWALL,
     *           LEWALL,NEWALL,YP,XNWALL,YNWALL,ZNWALL,
     *           VISC,NITER,UTAU,TAUX,TAUY,TAUZ,
     *           REP,UPP,UPWL,VPWL,WPWL)
C
      IMPLICIT NONE
C
      INTEGER*4 NODE,NE,NP,N2,NEX,NS,NSP,N2D,LOCAL,
     *          LPWALL,NPWALL,UWALL,VWALL,WWALL,
     *          LEWALL,NEWALL,NITER
C
      REAL*4    VKAP,B,U,V,W,DEVLP2,YP,XNWALL,YNWALL,ZNWALL,
     *          VISC,UTAU,TAUX,TAUY,TAUZ,REP,UPP,UPWL,VPWL,WPWL
C
      INTEGER*4 MAXENT,IPWALL,IP,IEWALL,IS,IE,IETYPE,NNPE,NNPS,I,
     *          IOPPSN,II,NOPPSN,ITER,IENT
      REAL*4    UPW,VPW,WPW,UPF,VPF,WPF,UPR,TABLE,EPS,DEVLP1,VPR,WPR,
     *          COF,F,DF,UP
C
      DIMENSION U(NP),V(NP),W(NP),NODE(N2,NE),NEX(8),
     1          LOCAL(NSP,NS,4),LPWALL(NPWALL),
     2          UWALL(NPWALL),VWALL(NPWALL),WWALL(NPWALL),
     3          LEWALL(2,NEWALL),YP(NEWALL),
     4          XNWALL(NEWALL),YNWALL(NEWALL),ZNWALL(NEWALL),VISC(NE),
     5          UTAU(NEWALL),TAUX(NEWALL),TAUY(NEWALL),TAUZ(NEWALL),
     6          REP (NEWALL),UPP (NEWALL),UPWL(NP),VPWL(NP),WPWL(NP)
C
      PARAMETER ( MAXENT = 8 )
      DIMENSION TABLE(2,MAXENT)
      DATA TABLE / 3.0E+2, 12.43, 1.0E+3, 15.57,
     &             3.0E+3, 18.16, 1.0E+4, 20.90,
     &             3.0E+4, 23.39, 1.0E+5, 26.13,
     &             3.0E+5, 28.64, 1.0E+6, 31.42 /
C
      DATA EPS  / 1.E-20 /
C
      DIMENSION IOPPSN(8)
C
C
C      CALCULATE WALL SHEAR STRESSES BASED ON THE SPALDING'S
C     GENERALIZED WALL LAW.
C         ( 3-D CALCULATION : SINGLE WORD & MULTI ELEMENT VERSION )
C                                           CODED BASED ON 'TAUWAL'
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          VKAP        ; VON-KARMAN CONSTANT
C          B           ; CONSTANT APPEARING IN THE UNIVERSAL FUNCTION
C          U       (IP); X-DIR. VELOCITY COMPONENT
C          V       (IP); Y-DIR. VELOCITY COMPONENT
C          W       (IP); Z-DIR. VELOCITY COMPONENT
C          NODE  (I,IE); NODE NO. TABLE BASED ON ELEMENT
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          NP          ; NUMBER OF TOTAL    NODES
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT
C          LOCAL (I,IS); NODE NUMBER TABLE DEFINING ELEMENT SURFACES
C          DEVLP2      ; DEVELOPMENT FUNCTION FOR WALL VELOCITIES
C          LPWALL (IBP); WALL BOUNDARY NODES
C          NPWALL      ; NUMBER OF WALL BOUNDARY NODES
C          UWALL  (IBP); WALL BOUNDARY U-VELOCITIES
C          VWALL  (IBP); WALL BOUNDARY V-VELOCITIES
C          WWALL  (IBP); WALL BOUNDARY W-VELOCITIES
C          LEWALL(I,IBE); WALL BOUNDARY ELEMENT AND ITS SURFACE
C          NEWALL      ; NUMBER OF WALL BOUNDARY SURFACES
C          YP     (IBE); DISTANCE BETWEEN WALL AND ITS OPPOSITE SURFACE
C          XNWALL (IBE); X NORMAL OF WALL BOUNDARY SURFACE
C          YNWALL (IBE); Y NORMAL OF WALL BOUNDARY SURFACE
C          ZNWALL (IBE); Z NORMAL OF WALL BOUNDARY SURFACE
C          VISC    (IE); MOLECULAR VISCOSITY
C          NITER       ; ITERATIONS MADE FOR NEWTON LAPSON METHOD
C
C       (2) OUTPUT
C          UTAU   (IBE); FRICTION VELOCITY             AT WALL SURFACES
C          TAUX   (IBE); X-DIR. SHEAR STRESS COMPONENT AT WALL SURFACES
C          TAUY   (IBE); Y-DIR. SHEAR STRESS COMPONENT AT WALL SURFACES
C          TAUZ   (IBE); Z-DIR. SHEAR STRESS COMPONENT AT WALL SURFACES
C
C       (4) WORK
C          REP    (IBE); STORE WALL SURFACE REYNOLDS NUMBER(UP*YP/VISC)
C          UPP    (IBE); STORE WALL SURFACE WALL VELOCITY  (UP/UTAU)
C          UPWL    (IP); STORE WALL U-VELOCITY AT GLOBAL NODE
C          VPWL    (IP); STORE WALL V-VELOCITY AT GLOBAL NODE
C          WPWL    (IP); STORE WALL W-VELOCITY AT GLOBAL NODE
C
C
C  STORE WALL VELOCITIES TO GLOBAL NODE ARRAYS
C
C
      DEVLP1=0.0E0
      DO 50 IPWALL = 1 , NPWALL
          IP = LPWALL(IPWALL)
          UPWL(IP) = DEVLP1*UWALL(IPWALL)
          VPWL(IP) = DEVLP1*VWALL(IPWALL)
          WPWL(IP) = DEVLP1*WWALL(IPWALL)
   50 CONTINUE
C
C
C  CALCULATE VELOCITY COMPONENTS AT THE WALL OPPOSITE SURFACE ALIGNED
C TO THE WALL SURFACE
C
C
      DO 100 IEWALL = 1 , NEWALL
         IE  = LEWALL(1,IEWALL)
         IS  = LEWALL(2,IEWALL)
         IF(     NODE(8,IE).GE.1) THEN ! HEX
            IETYPE = 4
            NNPE   = 8
         ELSE IF(NODE(6,IE).GE.1) THEN ! PRS
            IETYPE = 3
            NNPE   = 6
         ELSE IF(NODE(5,IE).GE.1) THEN ! PYR
            IETYPE = 2
            NNPE   = 5 
         ELSE                          ! TET
            IETYPE = 1
            NNPE   = 4
         ENDIF
         IF(LOCAL(4,IS,IETYPE).GE.1) THEN ! QUADRILATERAL
            NNPS = 4
         ELSE                             ! TRIANGLE
            NNPS = 3
         ENDIF   
         UPW=0.0E0
         VPW=0.0E0
         WPW=0.0E0
         DO 101 I=1,NNPS
            UPW = UPW + UPWL(NODE(LOCAL(I,IS,IETYPE),IE))
            VPW = VPW + VPWL(NODE(LOCAL(I,IS,IETYPE),IE))
            WPW = WPW + WPWL(NODE(LOCAL(I,IS,IETYPE),IE))
 101     CONTINUE 
         UPW = UPW/FLOAT(NNPS)
         VPW = VPW/FLOAT(NNPS)
         WPW = WPW/FLOAT(NNPS)
C
         DO 102 I=1,NNPE
            IOPPSN(I)=1
 102     CONTINUE
         DO 103 I=1,N2D
            II=LOCAL(I,IS,IETYPE)
            IF(II.GE.1) IOPPSN(II)=0
 103     CONTINUE
         NOPPSN=0
         UPF=0.0E0
         VPF=0.0E0
         WPF=0.0E0
         DO 104 I=1,NNPE
            IF(IOPPSN(I).EQ.1) THEN
               NOPPSN=NOPPSN+1
               UPF = UPF + U(NODE(I,IE))
               VPF = VPF + V(NODE(I,IE))
               WPF = WPF + W(NODE(I,IE))
            ENDIF
 104     CONTINUE
         UPF = UPF/FLOAT(NOPPSN)
         VPF = VPF/FLOAT(NOPPSN)
         WPF = WPF/FLOAT(NOPPSN)
C
          UPR = UPF-UPW
          VPR = VPF-VPW
          WPR = WPF-WPW
C
          COF = XNWALL(IEWALL)*UPR+YNWALL(IEWALL)*VPR+ZNWALL(IEWALL)*WPR
C
          TAUX(IEWALL) = UPR-COF*XNWALL(IEWALL)
          TAUY(IEWALL) = VPR-COF*YNWALL(IEWALL)
          TAUZ(IEWALL) = WPR-COF*ZNWALL(IEWALL)
C
          UTAU(IEWALL) = SQRT(TAUX(IEWALL)**2
     &                       +TAUY(IEWALL)**2
     &                       +TAUZ(IEWALL)**2)+EPS
C
          REP (IEWALL) = UTAU(IEWALL)*YP(IEWALL)/VISC(IE)
  100 CONTINUE
C
C
C  CALCULATE FRICTION VELOCITY BASED ON THE SPALDING'S GENERALIZED
C WALL LAW, USING THE NEWTON LAPSON METHOD. NOTE THAT THE GENERALIZED
C WALL LAW CAN BE SOLVED FOR UP/UTAU WITH A SINGLE PARAMETER REP.
C
C
      DO 310 ITER = 1 , NITER
          IF(ITER.EQ.1) THEN
              DO 220 IEWALL = 1 , NEWALL
                  IF(REP(IEWALL).LE.TABLE(1,1)) THEN
                      UPP(IEWALL) = SQRT(REP(IEWALL))
                      GO TO 220
                  ENDIF
C
                  DO 210 IENT = 2 , MAXENT
                      IF(REP(IEWALL).LE.TABLE(1,IENT)) THEN
                          UPP(IEWALL) = TABLE(2,IENT)
                          GO TO 220
                      ENDIF
  210             CONTINUE
C
                  UPP(IEWALL) = TABLE(2,MAXENT)
  220         CONTINUE
          ELSE
              DO 300 IEWALL = 1 , NEWALL
                  F  = UPP(IEWALL)-REP(IEWALL)/UPP(IEWALL)
     &                +EXP(-VKAP*B)*(EXP(VKAP*UPP(IEWALL))-1.E0
     &                              -VKAP*UPP(IEWALL)
     &                              -0.5E0*(VKAP*UPP(IEWALL))**2
     &                              -1.E0/6.E0*(VKAP*UPP(IEWALL))**3)
C
                  DF = 1.E0+REP(IEWALL)/UPP(IEWALL)**2
     &                +EXP(-VKAP*B)*(VKAP*EXP(VKAP*UPP(IEWALL))
     &                              -VKAP-VKAP**2*UPP(IEWALL)
     &                              -0.5E0*VKAP**3*UPP(IEWALL)**2)
C
                  UPP(IEWALL) = UPP(IEWALL)-F/DF
  300         CONTINUE
          ENDIF
  310 CONTINUE
C
      DO 400 IEWALL = 1 , NEWALL
          UP           =  UTAU(IEWALL)
          UTAU(IEWALL) =  UP/UPP(IEWALL)
          TAUX(IEWALL) = -UTAU(IEWALL)**2*TAUX(IEWALL)/UP
          TAUY(IEWALL) = -UTAU(IEWALL)**2*TAUY(IEWALL)/UP
          TAUZ(IEWALL) = -UTAU(IEWALL)**2*TAUZ(IEWALL)/UP
  400 CONTINUE
C
C
      RETURN
      END
