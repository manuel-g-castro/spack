      SUBROUTINE LIQUID(MCOLOR,MCPART,NCOLOR,NCPART,LLOOP,
     *                  N1,N2,ME,MELM,NE,NP,MEP,NEX,NODE,NUMIP,
     *                  ICAVI,ISOLT,NS,NL,ITIME,JSET,NMAX,
     *                  NCRS,NCRS2,NPPMAX,ITPCRS,IUT6,IUT0,JUNROL,
     *                  NPP,IPCRS,LTAB,NITR,RES,
     *                  EPS,EPSRE,SIGMA,CGAS,CLQD,F0,
     *                  FLINLT,FLIMIT,FLMIN,
     *                  EAP1,EAP2,EAP3,EBP,AP,MP,IENP,NODP,NEP,
     *                  SN,DELTA,CM,
     *                  UG,VG,WG,P,DT,VISC,SCT,F,FESRC,FE,
     *                  NPINLT,LPINLT,
     *                  NPSET, LPSET1,LPSET2,LPSET3,
     *                  COVER1,COVER2,COVER3,
     *                  NPSND ,LPSND ,NPTSND,IPSET,IPSRC,
     *                  NPRCV ,LPRCV ,NPTRCV,
     *                  IPART,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *                  LFIX3D,LPFIX,RX,RY,ACRS,A,
     *                  WRK01,WRK02,WRK03,WRK04,WRK05,WRK06,WRK07,
     *                  RHS,A0,TS,TACRS,IERR,ICRS_T)
      IMPLICIT NONE
C
C[INPUT]
      INTEGER*4 MCOLOR,MCPART,
     *          NCOLOR(4),NCPART(MCOLOR,4),LLOOP(MCPART,MCOLOR,4),
     *          N1,N2,ME,MELM,NE,NP,MEP,NEX(12),NODE(N2,NE),NUMIP(NP),
     *          ICAVI,ISOLT,NS,NL,ITIME,JSET,NMAX,
     *          NCRS,NPP(NP),IPCRS(NCRS),LTAB(N1,N2,NE),
     *          NPPMAX,NCRS2,ITPCRS(NCRS2),IUT6,IUT0,JUNROL
      REAL*4    EPS,EPSRE,SIGMA,CGAS,CLQD,F0,FLINLT,FLIMIT,FLMIN
      REAL*4    UG(NE),VG(NE),WG(NE),P(NE),DT,VISC(NE),SCT
      REAL*4    SN(N1,NE),DELTA(NE),CM(NP)
      REAL*4    EAP1(N2,MEP,NP),EAP2(3,N2,MEP,NP),EAP3(6,N2,MEP,NP)
      REAL*4    EBP(3,N2,MEP,NP),AP(N2,MEP,NP)
      INTEGER*4 MP,IENP(MEP,MP),NODP(N2,MEP,NP),NEP(MP)
      INTEGER*4 NPINLT,LPINLT(NPINLT)
      INTEGER*4 IPART,NDOM,MBPDOM,
     *          LDOM(NDOM),NBPDOM(NDOM),
     *          IPSLF(MBPDOM,NDOM),IPSND(MBPDOM,NDOM)
      INTEGER*4 NPSND,NPRCV
      INTEGER*4 NPSET,LPSET1(NPSET),LPSET2(NPSET),LPSET3(NPSET),
     *          LPSND (NDOM),NPTSND(NDOM) ,LPRCV (NDOM) ,NPTRCV(NDOM),
     *          IPSET (MBPDOM,NDOM),IPSRC (MBPDOM,NDOM)
      REAL*4    COVER1(NPSET),COVER2(NPSET),COVER3(NPSET)
C
C[INPUT/OUTPUT]
      REAL*4    F(NP)
C
C[OUTPUT]
      INTEGER*4 IERR,NITR
      REAL*4    FESRC(NE),FE(NE)
      REAL*4    RES
C
C[WORK]
      INTEGER*4 LFIX3D(NP),LPFIX(NP)
      REAL*4    RX(0:N2,ME),RY(0:N2,ME),ACRS(NCRS),A(N1,N2,NE),
     *          WRK01(NP),WRK02(NP),WRK03(NP),WRK04(NP),
     *          WRK05(NP),WRK06(NP),WRK07(NP),
     *          RHS(NP),A0(NP),TS(0:NP),TACRS(NCRS2)
C
C[LOCAL]
      INTEGER*4 MAXBUF,IDUM,I,IE,IP,IB,ISEND,IDIM,NB,NPFIX
      INTEGER*4 IP1,IP2,IP3,IP4,IP5,IP6,IP7,IP8
      REAL*4    COEF,CEB,RESR,SOURCE,DELTAP
C
      INTEGER*4 IBCGS
      DATA IBCGS  / 0 /
C
      CHARACTER*60 ERMSGC
     & / ' ## SUBROUTINE LIQUID: FATAL      ERROR REPORT   ; RETURNED' /
C 
      INTEGER*4 ICRS,K
      INTEGER*4 ICRS_T(NP)
C
      IERR=0
C
      IF(ITIME.EQ.0) GOTO 4000
C
CCYY [1] PREPARE FIX B.C. LIST
C
      DO 1100 IP = 1 , NP
          LFIX3D(IP) = 0
          LPFIX(IP) = 0
 1100 CONTINUE
C
      DO 1110 IB = 1 , NPINLT
          LFIX3D(LPINLT(IB))=1
 1110 CONTINUE
C
      DO 1120 IB = 1 , NPSET
          ISEND = LPSET3(IB)
          IF(ISEND.GT.0) GO TO 1120
          LFIX3D(LPSET1(IB))=1
 1120 CONTINUE
C
      NPFIX=0
      DO 1200 IP = 1 , NP
          IF(LFIX3D(IP).EQ.0) GO TO 1200
          NPFIX=NPFIX+1
          LPFIX(NPFIX) = IP
 1200 CONTINUE
C
      IERR=0
      MAXBUF = ME*N2
C
CCYY [2] CAL. TIME, ADV., AND DIFF. TERM 
C
      CALL ADVDFL(N1,N2,NE,NP,MEP,MELM,NEX,NODE,
     *            UG,VG,WG,DT,SCT,VISC,CEB,
     *            EAP1,EAP2,EAP3,EBP,IENP,NODP,NEP,MP,
     *            AP,A,F,RHS,A0,
     *            DELTA,IERR)
      IDUM = 2
      CALL DDCOMX(IPART,IDUM,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *            RHS,A0,RHS,NP,IUT0,IERR,RX,RY,MAXBUF)
      IF(IERR.NE.0) THEN
          WRITE(IUT0,*)
          WRITE(IUT0,*) ERMSGC
          RETURN
      ENDIF
C
CCYY [3] ADD SOURCE TERM
C
      DO 2000 IE = 1 , NE
          IP1=NODE(1,IE)
          IP2=NODE(2,IE)
          IP3=NODE(3,IE)
          IP4=NODE(4,IE)
          IP5=NODE(5,IE)
          IP6=NODE(6,IE)
          IP7=NODE(7,IE)
          IP8=NODE(8,IE)
          IF(IP8.NE.0) THEN 
              FE(IE) = ( F(IP1)+F(IP2)+F(IP3)+F(IP4)
     *                  +F(IP5)+F(IP6)+F(IP7)+F(IP8))/8.0E0
          ELSE IF(IP6.NE.0) THEN
              FE(IE) = ( F(IP1)+F(IP2)+F(IP3)+F(IP4)
     *                  +F(IP5)+F(IP6)              )/6.0E0
          ELSE IF(IP5.NE.0) THEN
              FE(IE) = ( F(IP1)+F(IP2)+F(IP3)+F(IP4)
     *                  +F(IP5)                     )/5.0E0
          ELSE IF(IP4.NE.0) THEN
              FE(IE) = ( F(IP1)+F(IP2)+F(IP3)+F(IP4))/4.0E0
          ENDIF 
C
          DELTAP=P(IE)+0.5E0*SIGMA
C
          IF(ICAVI.EQ.1) THEN 
              IF(DELTAP.LE.0) THEN
                  SOURCE = (CGAS*(1.E0-FE(IE))+CLQD*FE(IE))*DELTAP
              ELSE
                  SOURCE = (0.1E0*CGAS*(1.E0-FE(IE))+CLQD*FE(IE))*DELTAP
              ENDIF
          ELSE IF(ICAVI.EQ.2) THEN 
              IF(DELTAP.LE.0.0) THEN
                  SOURCE = CLQD*FE(IE)*DELTAP
              ELSE
                  SOURCE = CGAS*(1.E0-FE(IE))*DELTAP
              ENDIF                        
          ELSE IF(ICAVI.EQ.3) THEN 
              IF(DELTAP.LE.0) THEN
                  SOURCE = CLQD*FE(IE)*DELTAP
              ELSE
                  SOURCE = CGAS*((FE(IE)-F0)**0.5)*(1.0E0-FE(IE)-F0)
              ENDIF                        
          ENDIF
C
        IF(DELTAP.GE.0.0E0 .AND. FE(IE).GE.FLIMIT) SOURCE=0.0E0
C
        FESRC(IE) = SOURCE
 2000 CONTINUE
C
      CALL NODLEX(NODE,ME,NP,NE,NP,N1,N2,NEX,SN,
     *            IPART,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *            FESRC,WRK01,CM,IUT0,IERR,RX,RY,MAXBUF)
C
      DO 2100 IP = 1,NP
          RHS(IP)=RHS(IP) + DT*WRK01(IP)/CM(IP)
 2100 CONTINUE

C
CCYY [4] MATIRIX CONVET FROM ELEMENT-WIZE TO CRS
C
      CALL E2PMAT(MCOLOR,MCPART,NCOLOR,NCPART,LLOOP,
     *            N2,N1,NE,NEX,NCRS,A,ACRS,LTAB,IUT0,IERR)
      IF(IERR.NE.0) THEN
          WRITE(IUT0,*)'ERROR CODE REPORTED FROM E2PMAT'
          WRITE(IUT0,*)ERMSGC
          RETURN
      ENDIF
C
CCYY [5] DIAGONAL SCALING
C
      CALL DGNSCL(ACRS,A0,NP,NE,NCRS,IPCRS,NPP,ME)
C
      DO 1300 IP=1, NP
          RHS(IP) = RHS(IP)/A0(IP)
 1300 CONTINUE
C
CCYY [6] CLEAR CRS MATRIX FOR DIRICHLET B.C.
C
      CALL CLRCRS(IPART,ACRS,NP,NCRS,IPCRS,NPP,
     *            A0,LFIX3D,NUMIP,WRK01)
C
CCYY [7] SET B.C. (1)INLET, (2)OVERSET
C
      IF (JSET.GE.1) THEN
          CALL OVRST1(IPART,NPSET,N1,N2,ME,NE,NP,NEX,NODE,F,
     *                LPSET1,LPSET2,LPSET3,COVER1,COVER2,COVER3,
     *                NDOM,MBPDOM,NPSND,NPRCV,
     *                LPSND,NPTSND,LPRCV,NPTRCV,IPSET,IPSRC,
     *                WRK01,WRK02,RX,RY,IUT0,IERR)
          IF(IERR.NE.0) THEN
              WRITE(IUT0,*)'ERROR CODE REPORTED FROM OVRST1'
              WRITE(IUT0,*)ERMSGC
              RETURN
          ENDIF
      ENDIF
C
      DO 1400 IB=1,NPINLT
          F(LPINLT(IB)) = FLINLT
 1400 CONTINUE   

      DO 1500 IB = 1 , NPFIX
            IP=LPFIX(IB)
            RHS(IP) = F(IP)
 1500 CONTINUE

C
CCYY [8] SOLVE EQUATION
C
      IF(JUNROL.EQ.1)
     *CALL CRSCVA(NP,NPPMAX,NCRS,NCRS2,NPP,ACRS,TACRS,ICRS_T)

      IF(ISOLT.EQ.1) THEN
          CALL BCGSTX(
     *         NPP,NCRS,IPCRS,ACRS,
     *         RHS,F,EPS,EPSRE,
     *         NMAX,RES,NITR,NODE,NE,NEX,NP,ME,N2,
     *         IPART,LDOM,NBPDOM,NDOM,
     *         IPSLF,IPSND,MBPDOM,NUMIP,
     *         RX,RY,WRK01,WRK02,WRK03,WRK04,WRK05,WRK06,WRK07,
     *         JUNROL,NPPMAX,NCRS2,TS,TACRS,ITPCRS,
     *         IUT0,IERR)
      ELSE IF(ISOLT.EQ.3) THEN
          CALL IDRBCG(ME,N2,NE,NP,NS,NL,NMAX,EPS,RES,RESR,NITR,
     *                NCRS,NPP,ACRS,IPCRS,RHS,F,NUMIP,
     *                IPART,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *                JUNROL,NPPMAX,NCRS2,TS,TACRS,ITPCRS,
     *                IUT0,IUT6,IERR)
      ELSE
              WRITE(IUT0,*) 'INVALID VALUE FOR ISOLT'  ,ISOLT
              IERR=1
              RETURN 
      ENDIF
C
      IF(IERR.NE.0) THEN
          WRITE(IUT0,*)'ERROR CODE REPORTED FROM BCGSTX'
          WRITE(IUT0,*)ERMSGC
          RETURN
      ENDIF

C
C
CCYY [8]  SET B.C. (1)INLET, (2)OVERSET
C
 4000 CONTINUE
C
      DO 4100 IP = 1 , NP
          F(IP)  = AMAX1(FLMIN,F(IP))
          F(IP)  = AMIN1(1.E0 ,F(IP))
 4100 CONTINUE
C
      IF (JSET.GE.1) THEN
          CALL OVRST1(IPART,NPSET,N1,N2,ME,NE,NP,NEX,NODE,F,
     *                LPSET1,LPSET2,LPSET3,COVER1,COVER2,COVER3,
     *                NDOM,MBPDOM,NPSND,NPRCV,
     *                LPSND,NPTSND,LPRCV,NPTRCV,IPSET,IPSRC,
     *                WRK01,WRK02,RX,RY,IUT0,IERR)
          IF(IERR.NE.0) THEN
              WRITE(IUT0,*)'ERROR CODE REPORTED FROM OVRST1'
              WRITE(IUT0,*)ERMSGC
              RETURN
          ENDIF
      ENDIF
C
      DO 4200 IB=1,NPINLT
          F(LPINLT(IB)) = FLINLT
 4200 CONTINUE   
C
      DO 4300 IE = 1 , NE
          IP1=NODE(1,IE)
          IP2=NODE(2,IE)
          IP3=NODE(3,IE)
          IP4=NODE(4,IE)
          IP5=NODE(5,IE)
          IP6=NODE(6,IE)
          IP7=NODE(7,IE)
          IP8=NODE(8,IE)
          IF(IP8.NE.0) THEN 
              FE(IE) = ( F(IP1)+F(IP2)+F(IP3)+F(IP4)
     *                  +F(IP5)+F(IP6)+F(IP7)+F(IP8))/8.0E0
          ELSE IF(IP6.NE.0) THEN
              FE(IE) = ( F(IP1)+F(IP2)+F(IP3)+F(IP4)
     *                  +F(IP5)+F(IP6)              )/6.0E0
          ELSE IF(IP5.NE.0) THEN
              FE(IE) = ( F(IP1)+F(IP2)+F(IP3)+F(IP4)
     *                  +F(IP5)                     )/5.0E0
          ELSE IF(IP4.NE.0) THEN
              FE(IE) = ( F(IP1)+F(IP2)+F(IP3)+F(IP4))/4.0E0
          ENDIF 
C
          DELTAP=P(IE)+0.5E0*SIGMA
C
          IF(ICAVI.EQ.1) THEN 
              IF(DELTAP.LE.0) THEN
                  SOURCE = (CGAS*(1.E0-FE(IE))+CLQD*FE(IE))*DELTAP
              ELSE
                  SOURCE = (0.1E0*CGAS*(1.E0-FE(IE))+CLQD*FE(IE))*DELTAP
              ENDIF
          ELSE IF(ICAVI.EQ.2) THEN 
              IF(DELTAP.LE.0.0) THEN
                  SOURCE = CLQD*FE(IE)*DELTAP
              ELSE
                  SOURCE = CGAS*(1.E0-FE(IE))*DELTAP
              ENDIF                        
          ELSE IF(ICAVI.EQ.3) THEN 
              IF(DELTAP.LE.0) THEN
                  SOURCE = CLQD*FE(IE)*DELTAP
              ELSE
                  SOURCE = CGAS*((FE(IE)-F0)**0.5)*(1.0E0-FE(IE)-F0)
              ENDIF                        
          ENDIF
C
        IF(DELTAP.GE.0.0E0 .AND. FE(IE).GE.FLIMIT) SOURCE=0.0E0
C
        FESRC(IE) = SOURCE
C
 4300 CONTINUE
C
      RETURN
      END
