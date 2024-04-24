      SUBROUTINE OVRST3(IMODE,IPART,NPSET,N1,N2,ME,NE,NP,NEX,NODE,
     *                  X,Y,Z,OMEGA,TIMER,
     *                  NFRAME,IEATTR,IPATTR,UFRAME,VFRAME,WFRAME,
     *                  U,V,W,
     *                  LPSET1,LPSET2,LPSET3,LPSET4,
     *                  COVER1,COVER2,COVER3,
     *                  NDOM,MBPDOM,NPSND,NPRCV,
     *                  LPSND,NPTSND,LPRCV,NPTRCV,IPSET,IPSRC,
     *                  WRK01,WRK02,WRK03,UG,VG,WG,RX,RY,
     *                  NMRF,IFATTR,OMGMRF,AMRF,IUT0,IERR)
      IMPLICIT NONE
C
C[INPUT]
      INTEGER*4 IMODE,IPART,NPSET,N1,N2,ME,NE,NP,NEX(12)
      INTEGER*4 NODE(N2,NE),NFRAME,IEATTR(NE),IPATTR(NP),
     *          NDOM,MBPDOM,NPSND,NPRCV,
     *          LPSND(NDOM),NPTSND(NDOM),
     *          LPRCV(NDOM),NPTRCV(NDOM),
     *          IPSET(MBPDOM,NDOM),IPSRC(MBPDOM,NDOM),
     *          LPSET1(NPSET),LPSET2(NPSET),LPSET3(NPSET),
     *          LPSET4(NPSET)
C
      REAL*4    OMEGA,TIMER,X(NP),Y(NP),Z(NP)
      REAL*4    UFRAME(2,NFRAME),VFRAME(2,NFRAME),WFRAME(2,NFRAME)
      REAL*4    COVER1(NPSET),COVER2(NPSET),COVER3(NPSET)
      INTEGER*4 IUT0
C     [INPUT:MRF]
      INTEGER*4 NMRF
      INTEGER*4 IFATTR(*)
      REAL*4    OMGMRF(NMRF),AMRF(3,NMRF)
C
C[INPUT-OUTPUT]
      REAL*4    U(NP),V(NP),W(NP)
C
C[OUTPUT]
      INTEGER*4 IERR
C
C[WORK]
      REAL*4    WRK01(NP),WRK02(NP),WRK03(NP),
     *          UG(NP),VG(NP),WG(NP),RX(N1,ME),RY(N1,ME)
C[LOCAL]
      INTEGER*4 IBP,ISEND,IE,IFRME,IFRMP,IDIM,MAXBUF,NB,
     *          NETET,NEPRD,NEWED,NEHEX,NTET,NPRD,NWED,NHEX,
     *          IP,IP1,IP2,IP3,IP4,IP5,IP6,IP7,IP8 
      REAL*4    GP,EP,TP,T1,T2,T3,T4,T5,T6,T7,T8,TH,UR,VR,
     *          COSTH,SINTH,COEF,
     *          O1,O2,XBUF,YBUF,C0,S0,C1,S1,C2,S2,
     *          UR1,VR1,WR1,UR2,VR2,WR2,UR3,VR3,WR3,
     *          XR1,YR1,ZR1
C
      CHARACTER*60 ERMSGC
     & /' ## SUBROUTINE OVRST3: FATAL      ERROR REPORT   ; RETURNED' /
C
C
      IF(IMODE.EQ.1) THEN
          COEF=1.0E0
      ELSE
          COEF=0.0E0
      ENDIF
C
      MAXBUF = NE*N1
C
      NETET=NEX(1)
      NEPRD=NEX(2)
      NEWED=NEX(3)
      NEHEX=NEX(4)
C
      NTET=NEX(5)
      NPRD=NEX(6)
      NWED=NEX(7)
      NHEX=NEX(8)
C
      DO 1000 IBP = 1 , NPSET
          ISEND = LPSET3(IBP)
          IF(ISEND.LT.0) GO TO 1000
C
          IE = LPSET2(IBP)
          GP = COVER1(IBP)
          EP = COVER2(IBP)
          TP = COVER3(IBP)
CCYY---
          IF(IE.EQ.0) THEN
              WRK01(IBP)=0.0E0
              WRK02(IBP)=0.0E0
              WRK03(IBP)=0.0E0
              UG   (IBP)=0.0E0
              VG   (IBP)=0.0E0
              WG   (IBP)=0.0E0
              GOTO 1000 
          ENDIF 
CCYY---
C
          IF(NODE(NHEX,IE).NE.0) THEN
              IP1=NODE(1,IE) 
              IP2=NODE(2,IE) 
              IP3=NODE(3,IE) 
              IP4=NODE(4,IE) 
              IP5=NODE(5,IE) 
              IP6=NODE(6,IE) 
              IP7=NODE(7,IE) 
              IP8=NODE(8,IE) 
              T1=0.125E0*(1.-GP)*(1.-EP)*(1.-TP)
              T2=0.125E0*(1.+GP)*(1.-EP)*(1.-TP)
              T3=0.125E0*(1.+GP)*(1.+EP)*(1.-TP)
              T4=0.125E0*(1.-GP)*(1.+EP)*(1.-TP)
              T5=0.125E0*(1.-GP)*(1.-EP)*(1.+TP)
              T6=0.125E0*(1.+GP)*(1.-EP)*(1.+TP)
              T7=0.125E0*(1.+GP)*(1.+EP)*(1.+TP)
              T8=0.125E0*(1.-GP)*(1.+EP)*(1.+TP)
           WRK01(IBP)=T1*X(IP1)+T2*X(IP2)+T3*X(IP3)+T4*X(IP4)
     *               +T5*X(IP5)+T6*X(IP6)+T7*X(IP7)+T8*X(IP8)
           WRK02(IBP)=T1*Y(IP1)+T2*Y(IP2)+T3*Y(IP3)+T4*Y(IP4)
     *               +T5*Y(IP5)+T6*Y(IP6)+T7*Y(IP7)+T8*Y(IP8)
           WRK03(IBP)=T1*Z(IP1)+T2*Z(IP2)+T3*Z(IP3)+T4*Z(IP4)
     *               +T5*Z(IP5)+T6*Z(IP6)+T7*Z(IP7)+T8*Z(IP8)
              UG(IBP)=T1*U(IP1)+T2*U(IP2)+T3*U(IP3)+T4*U(IP4)
     *               +T5*U(IP5)+T6*U(IP6)+T7*U(IP7)+T8*U(IP8)
              VG(IBP)=T1*V(IP1)+T2*V(IP2)+T3*V(IP3)+T4*V(IP4)
     *               +T5*V(IP5)+T6*V(IP6)+T7*V(IP7)+T8*V(IP8)
              WG(IBP)=T1*W(IP1)+T2*W(IP2)+T3*W(IP3)+T4*W(IP4)
     *               +T5*W(IP5)+T6*W(IP6)+T7*W(IP7)+T8*W(IP8)
          ELSE IF(NODE(NWED,IE).NE.0) THEN
              IP1=NODE(1,IE) 
              IP2=NODE(2,IE) 
              IP3=NODE(3,IE) 
              IP4=NODE(4,IE) 
              IP5=NODE(5,IE) 
              IP6=NODE(6,IE) 
              T1=0.5E0*GP        *(1.-TP)
              T2=0.5E0*EP        *(1.-TP)
              T3=0.5E0*(1.-GP-EP)*(1.-TP)
              T4=0.5E0*GP        *(1.+TP)
              T5=0.5E0*EP        *(1.+TP)
              T6=0.5E0*(1.-GP-EP)*(1.+TP)
           WRK01(IBP)=T1*X(IP1)+T2*X(IP2)+T3*X(IP3)+T4*X(IP4)
     *               +T5*X(IP5)+T6*X(IP6)
           WRK02(IBP)=T1*Y(IP1)+T2*Y(IP2)+T3*Y(IP3)+T4*Y(IP4)
     *               +T5*Y(IP5)+T6*Y(IP6)
           WRK03(IBP)=T1*Z(IP1)+T2*Z(IP2)+T3*Z(IP3)+T4*Z(IP4)
     *               +T5*Z(IP5)+T6*Z(IP6)
             UG(IBP)=T1*U(IP1)+T2*U(IP2)+T3*U(IP3)+T4*U(IP4)
     *              +T5*U(IP5)+T6*U(IP6)
             VG(IBP)=T1*V(IP1)+T2*V(IP2)+T3*V(IP3)+T4*V(IP4)
     *              +T5*V(IP5)+T6*V(IP6)
             WG(IBP)=T1*W(IP1)+T2*W(IP2)+T3*W(IP3)+T4*W(IP4)
     *              +T5*W(IP5)+T6*W(IP6)
          ELSE IF(NODE(NPRD,IE).NE.0) THEN
              IP1=NODE(1,IE) 
              IP2=NODE(2,IE) 
              IP3=NODE(3,IE) 
              IP4=NODE(4,IE) 
              IP5=NODE(5,IE) 
              T1=GP
              T2=EP
              T3=TP
              T4=1.0E0-(T1+T2+T3)
              T1=0.25E0*((1.-GP)*(1.-EP)-TP+GP*EP*TP/(1.-TP))
              T2=0.25E0*((1.+GP)*(1.-EP)-TP-GP*EP*TP/(1.-TP))
              T3=0.25E0*((1.+GP)*(1.+EP)-TP+GP*EP*TP/(1.-TP))
              T4=0.25E0*((1.-GP)*(1.+EP)-TP-GP*EP*TP/(1.-TP))
              T5= TP
           WRK01(IBP)=T1*X(IP1)+T2*X(IP2)+T3*X(IP3)+T4*X(IP4)
     *               +T5*X(IP5)
           WRK02(IBP)=T1*Y(IP1)+T2*Y(IP2)+T3*Y(IP3)+T4*Y(IP4)
     *               +T5*Y(IP5)
           WRK03(IBP)=T1*Z(IP1)+T2*Z(IP2)+T3*Z(IP3)+T4*Z(IP4)
     *               +T5*Z(IP5)
              UG(IBP)=T1*U(IP1)+T2*U(IP2)+T3*U(IP3)+T4*U(IP4)
     *               +T5*U(IP5)
              VG(IBP)=T1*V(IP1)+T2*V(IP2)+T3*V(IP3)+T4*V(IP4)
     *               +T5*V(IP5)
              WG(IBP)=T1*W(IP1)+T2*W(IP2)+T3*W(IP3)+T4*W(IP4)
     *               +T5*W(IP5)
          ELSE IF(NODE(NTET,IE).NE.0) THEN
              IP1=NODE(1,IE) 
              IP2=NODE(2,IE) 
              IP3=NODE(3,IE) 
              IP4=NODE(4,IE) 
              T1=GP
              T2=EP
              T3=TP
              T4=1.0E0-(GP+EP+TP)
           WRK01(IBP)=T1*X(IP1)+T2*X(IP2)+T3*X(IP3)+T4*X(IP4)
           WRK02(IBP)=T1*Y(IP1)+T2*Y(IP2)+T3*Y(IP3)+T4*Y(IP4)
           WRK03(IBP)=T1*Z(IP1)+T2*Z(IP2)+T3*Z(IP3)+T4*Z(IP4)
              UG(IBP)=T1*U(IP1)+T2*U(IP2)+T3*U(IP3)+T4*U(IP4)
              VG(IBP)=T1*V(IP1)+T2*V(IP2)+T3*V(IP3)+T4*V(IP4)
              WG(IBP)=T1*W(IP1)+T2*W(IP2)+T3*W(IP3)+T4*W(IP4)
CC        ELSE
CC            WRITE(IUT0,*)'FRCT1X:INVALID NODE TABLE:ERROR'
CC            IERR=1      
CC            RETURN
          ENDIF
C
 1000 CONTINUE
C
C         CONVERT INTERPOLATED VELOCITIES FOR STATIONARY REFERENCE FRAME
C
      DO 2000 IBP = 1 , NPSET
          ISEND = LPSET3(IBP)
          IF(ISEND.LT.0) GO TO 2000
C
          IE    = LPSET2(IBP)
          IFRME = IEATTR(IE)
CCYYTMP--
CC        IFRMP = IFATTR(LPSET4(IBP))
          IFRMP = 0
CCYYTMP--
CCCC
CCCC NOTE THAT 
CCC  IFRME IS FRAME-OF-REFERENCE-ID AT PARANTS ELEMENTS
CCC  IFRMP IS FRAME-OF-REFERENCE-ID AT CHILD   NODES.
C
          O1=OMEGA
          IF(IFRME.GE.-1) THEN  
              O2=0.0E0
              C0=0.0E0 
              S0=0.0E0 
          ELSE
              O2=OMGMRF(  -IFRME)
              XBUF=AMRF(1,-IFRME)
              YBUF=AMRF(2,-IFRME)
              C0=XBUF/SQRT(XBUF*XBUF+YBUF*YBUF)
              S0=YBUF/SQRT(XBUF*XBUF+YBUF*YBUF)
          ENDIF
          C1  = COS(O1*TIMER)
          S1  = SIN(O1*TIMER)
          C2  = COS(O2*TIMER)
          S2  = SIN(O2*TIMER)
C
          IF(IFRME.EQ.-1.AND.IFRMP.GE.-1) THEN
C
CC            [ADD ROTATIONAL COMPONENTS]
              UR = UG(IBP)-O1*WRK02(IBP)*COEF
              VR = VG(IBP)+O1*WRK01(IBP)*COEF
C
CC            [ROTATE VELOCITIES AROUND X-AXIS WITH THE ANGLE A1]
              UG(IBP) = C1*UR-S1*VR
              VG(IBP) = S1*UR+C1*VR
          ENDIF
C
          IF(IFRME.LT.-1) THEN
C
CC            [ROTATE COORDINATES AROUND Z-AXIS]
              XR1= C0*WRK01(IBP)+S0*WRK02(IBP)
              YR1=-S0*WRK01(IBP)+C0*WRK02(IBP)
              ZR1=    WRK03(IBP)
C
CC            [ROTATE VELOCITIES AROUND Z-AXIS WITH THE ANGLE -A0]
              UR2= C0*UG(IBP)+S0*VG(IBP)
              VR2=-S0*UG(IBP)+C0*VG(IBP)
              WR2=    WG(IBP)
C
CC            [ADD ROTATIONAL COMPONENTS]
              UR2=UR2
              VR2=VR2-O2*ZR1*COEF
              WR2=WR2+O2*YR1*COEF
C
CC            [ROTATE VELOCITIES AROUND X-AXIS WITH THE ANGLE A2]
              UR3=   UR2
              VR3=C2*VR2-S2*WR2
              WR3=S2*VR2+C2*WR2
C
CC            [ROTATE VELOCITIES AROUND Z-AXIS WITH THE ANGLE A0]
              UG(IBP) = C0*UR3-S0*VR3
              VG(IBP) = S0*UR3+C0*VR3
              WG(IBP) =    WR3
          ENDIF
C
          IF(IFRME.GE. 1) THEN
              UG(IBP) = UG(IBP)+UFRAME(1,IFRME)
              VG(IBP) = VG(IBP)+VFRAME(1,IFRME)
              WG(IBP) = WG(IBP)+WFRAME(1,IFRME)
          ENDIF
 2000 CONTINUE
C
C PERFORM SELF-DOMAIN VELOCITY OVERSETS
C
      NB = 0
!ocl norecurrence(U,V,W)
      DO 3000 IBP = 1 , NPSET
          ISEND = LPSET3(IBP)
          IF(ISEND.LT.0) GOTO 3000
C
          IP = LPSET1(IBP)
C
          IF(ISEND.EQ.0) THEN
              U(IP) = UG(IBP)
              V(IP) = VG(IBP)
              W(IP) = WG(IBP)
          ELSE
              NB = NB+1
              WRK01(NB) = UG(IBP)
              WRK02(NB) = VG(IBP)
              WRK03(NB) = WG(IBP)
          ENDIF
 3000 CONTINUE
C
C PERFORM INTER-DOMAIN VELOCITY OVERSETS
C
      IF(IPART.GE.1) THEN
          IDIM=3
          CALL DDSET3(NPSND,LPSND,NPTSND,IPSET,IPSRC,
     *                WRK01,WRK02,WRK03,NB,
     *                NPRCV,LPRCV,NPTRCV,U,V,W,NP,
     *                IDIM,MBPDOM,IUT0,IERR,RX,RY,MAXBUF)
          IF(IERR.NE.0) THEN
              WRITE(IUT0,*)
              WRITE(IUT0,*) ERMSGC
              RETURN
          ENDIF
      ENDIF
C
C CONVERT OVERSET VELOCITIES FOR THE OWN REFERENCE FRAME
C
!ocl norecurrence(U,V,W)
      DO 4000 IBP = 1 , NPSET
          ISEND = LPSET3(IBP)
          IF(ISEND.GE.1) GO TO 4000
C
          IP    = LPSET1(IBP)
CCYYTMP--
CC        IFRME = IFATTR(LPSET4(IBP))
          IFRME = 0
CCYYTMP--
          IFRMP = IPATTR(IP)
CCCC
CCCC NOTE THAT 
CCC  IFRME IS FRAME-OF-REFERENCE-ID AT PARANTS ELEMENTS
CCC  IFRMP IS FRAME-OF-REFERENCE-ID AT CHILD   NODES.
C
          TH     = OMEGA*TIMER
          COSTH  = COS(TH)
          SINTH  = SIN(TH)
          O1=OMEGA
          IF(IFRMP.GE.-1) THEN  
              O2=0.0E0
              C0=0.0E0 
              S0=0.0E0 
          ELSE
              O2=OMGMRF(  -IFRMP)
              XBUF=AMRF(1,-IFRMP)
              YBUF=AMRF(2,-IFRMP)
              C0=XBUF/SQRT(XBUF*XBUF+YBUF*YBUF)
              S0=YBUF/SQRT(XBUF*XBUF+YBUF*YBUF)
          ENDIF
          C1  = COS(O1*TIMER)
          S1  = SIN(O1*TIMER)
          C2  = COS(O2*TIMER)
          S2  = SIN(O2*TIMER)
C
          IF(IFRMP.EQ.-1.AND.IFRME.GE.-1) THEN
CC            [ROTATE VELOCITIES AROUND X-AXIS WITH THE ANGLE A1]
              UR    = C1*U(IP)+S1*V(IP)
              VR    =-S1*U(IP)+C1*V(IP)
C
CC            [ADD ROTATIONAL COMPONENTS]
              U(IP) = UR+O1*Y(IP)*COEF
              V(IP) = VR-O1*X(IP)*COEF
          ENDIF
C
          IF(IFRMP.LT.-1) THEN
C
CC            [ROTATE COORDINATES AROUND Z-AXIS]
              XR1= C0*X(IP)+S0*Y(IP)
              YR1=-S0*X(IP)+C0*Y(IP)
              ZR1=    Z(IP)
C
CC            [ROTATE VELOCITIES AROUND Z-AXIS WITH THE ANGLE -A0]
              UR2= C0*U(IP)+S0*V(IP)
              VR2=-S0*U(IP)+C0*V(IP)
              WR2=    W(IP)
C
CC            [ROTATE VELOCITIES AROUND X-AXIS WITH THE ANGLE A2]
              UR3=UR2
              VR3= C2*VR2+S2*WR2
              WR3=-S2*VR2+C2*WR2
C
CC            [ADD ROTATIONAL COMPONENTS]
              UR3=UR3
              VR3=VR3-O2*ZR1*COEF
              WR3=WR3+O2*YR1*COEF
C
CC            [ROTATE VELOCITIES AROUND Z-AXIS WITH THE ANGLE A0]
              U(IP) = C0*UR3-S0*VR3
              V(IP) = S0*UR3+C0*VR3
              W(IP) =    WR3
          ENDIF
C
          IF(IFRMP.GE. 1) THEN
              U(IP) = U(IP)-UFRAME(1,IFRMP)
              V(IP) = V(IP)-VFRAME(1,IFRMP)
              W(IP) = W(IP)-WFRAME(1,IFRMP)
          ENDIF
 4000 CONTINUE
C
      RETURN
      END
