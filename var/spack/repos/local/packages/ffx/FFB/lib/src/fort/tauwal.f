C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    TAUWAL                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE TAUWAL(VKAP,B,U,V,W,NODE,NE,NP,N,LOCAL,DEVLP2,      
     *                  LPWALL,NPWALL,UWALL,VWALL,WWALL,
     *                  LEWALL,NEWALL,YP,XNWALL,YNWALL,ZNWALL,
     *                  VISCM,NITER,UTAU,TAUX,TAUY,TAUZ,
     *                  REP,UPP,UPWL,VPWL,WPWL)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION U(NP),V(NP),W(NP),NODE(N,NE),LOCAL(4,6),LPWALL(NPWALL),
     1          UWALL(NPWALL),VWALL(NPWALL),WWALL(NPWALL),
     2          LEWALL(2,NEWALL),YP(NEWALL),
     3          XNWALL(NEWALL),YNWALL(NEWALL),ZNWALL(NEWALL),
     4          UTAU(NEWALL),TAUX(NEWALL),TAUY(NEWALL),TAUZ(NEWALL),
     5          REP (NEWALL),UPP (NEWALL),UPWL(NP),VPWL(NP),WPWL(NP)
C
      DIMENSION LOCALF(4,6)
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
C
C      CALCULATE WALL SHEAR STRESSES BASED ON THE SPALDING'S
C     GENERALIZED WALL LAW.
C         ( 3-D ; SINGLE PRECISION )
C
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
C          VISCM       ; MOLECULAR VISCOSITY
C          NITER       ; ITERATIONS MADE FOR NEWTON LAPSON METHOD
C
C       (2) OUTPUT
C          UTAU   (IBE); FRICTION VELOCITY             AT WALL SURFACES
C          TAUX   (IBE); X-DIR. SHEAR STRESS COMPONENT AT WALL SURFACES
C          TAUY   (IBE); Y-DIR. SHEAR STRESS COMPONENT AT WALL SURFACES
C          TAUZ   (IBE); Z-DIR. SHEAR STRESS COMPONENT AT WALL SURFACES
C
C       (4) WORK
C          REP    (IBE); STORE WALL SURFACE REYNOLDS NUMBER(UP*YP/VISCM)
C          UPP    (IBE); STORE WALL SURFACE WALL VELOCITY  (UP/UTAU)
C          UPWL    (IP); STORE WALL U-VELOCITY AT GLOBAL NODE
C          VPWL    (IP); STORE WALL V-VELOCITY AT GLOBAL NODE
C          WPWL    (IP); STORE WALL W-VELOCITY AT GLOBAL NODE
C
C
C  MAKE OPPOSITE SURFACE NODE TABLE
C
C
      DO 20 IS = 1 , 6
          IF(MOD(IS,2).EQ.1) THEN
              ISF = IS+1
          ELSE
              ISF = IS-1
          ENDIF
          DO 10 I = 1 , 4
              LOCALF(I,ISF) = LOCAL(I,IS)
   10     CONTINUE
   20 CONTINUE
C
C
C  STORE WALL VELOCITIES TO GLOBAL NODE ARRAYS
C
C
      DO 50 IPWALL = 1 , NPWALL
          IP = LPWALL(IPWALL)
          UPWL(IP) = DEVLP2*UWALL(IPWALL)
          VPWL(IP) = DEVLP2*VWALL(IPWALL)
          WPWL(IP) = DEVLP2*WWALL(IPWALL)
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
C
          IPW1 = NODE(LOCAL (1,IS),IE)
          IPW2 = NODE(LOCAL (2,IS),IE)
          IPW3 = NODE(LOCAL (3,IS),IE)
          IPW4 = NODE(LOCAL (4,IS),IE)
C
          IPF1 = NODE(LOCALF(1,IS),IE)
          IPF2 = NODE(LOCALF(2,IS),IE)
          IPF3 = NODE(LOCALF(3,IS),IE)
          IPF4 = NODE(LOCALF(4,IS),IE)
C
          UPW = 0.25E0*(UPWL(IPW1)+UPWL(IPW2)+UPWL(IPW3)+UPWL(IPW4))
          VPW = 0.25E0*(VPWL(IPW1)+VPWL(IPW2)+VPWL(IPW3)+VPWL(IPW4))
          WPW = 0.25E0*(WPWL(IPW1)+WPWL(IPW2)+WPWL(IPW3)+WPWL(IPW4))
C
          UPF = 0.25E0*(U(IPF1)+U(IPF2)+U(IPF3)+U(IPF4))
          VPF = 0.25E0*(V(IPF1)+V(IPF2)+V(IPF3)+V(IPF4))
          WPF = 0.25E0*(W(IPF1)+W(IPF2)+W(IPF3)+W(IPF4))
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
          REP (IEWALL) = UTAU(IEWALL)*YP(IEWALL)/VISCM
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
