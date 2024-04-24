      SUBROUTINE OVRST1(IPART,NPSET,N1,N2,ME,NE,NP,NEX,NODE,S,
     *                  LPSET1,LPSET2,LPSET3,COVER1,COVER2,COVER3,
     *                  NDOM,MBPDOM,NPSND,NPRCV,
     *                  LPSND,NPTSND,LPRCV,NPTRCV,IPSET,IPSRC,
     *                  WRK01,WRK02,RX,RY,IUT0,IERR)
      IMPLICIT NONE
      INTEGER*4 IPART,NPSET,N1,N2,ME,NE,NP,NEX(12)
      INTEGER*4 NODE(N2,NE)
      REAL*4    S(NP)
      INTEGER*4 LPSET1(NPSET),LPSET2(NPSET),LPSET3(NPSET)
      REAL*4    COVER1(NPSET),COVER2(NPSET),COVER3(NPSET)
      INTEGER*4 NDOM,MBPDOM,NPSND,NPRCV,
     *          LPSND(NDOM),NPTSND(NDOM),
     *          LPRCV(NDOM),NPTRCV(NDOM),
     *          IPSET (MBPDOM,NDOM),IPSRC (MBPDOM,NDOM)
      REAL*4    WRK01(NP),WRK02(NP),RX(N1,ME),RY(N1,ME)
      INTEGER*4 IUT0,IERR  
C
      INTEGER*4 NETET,NEPRD,NEWED,NEHEX,NTET,NPRD,NWED,NHEX,
     *          IBP,ISEND,IE,IP,NB,MAXBUF,IDIM,
     *          IP1,IP2,IP3,IP4,IP5,IP6,IP7,IP8
      REAL*4    GP,EP,TP,T1,T2,T3,T4,T5,T6,T7,T8
C
      CHARACTER*60 ERMSGC
     & / ' ## SUBROUTINE OVRST1: FATAL      ERROR REPORT   ; RETURNED' /
C
      MAXBUF = ME*N1
C
      NETET =NEX( 1)
      NEPRD =NEX( 2)
      NEWED =NEX( 3)
      NEHEX =NEX( 4)
      NTET  =NEX( 5)
      NPRD  =NEX( 6)
      NWED  =NEX( 7)
      NHEX  =NEX( 8)
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
!NOTE THAT: THIS IS SPECICAL VALUE FOR LOQUID FRACTION 
              WRK01(IBP)=1.0
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
              WRK01(IBP)=T1*S(IP1)+T2*S(IP2)+T3*S(IP3)+T4*S(IP4)
     *                  +T5*S(IP5)+T6*S(IP6)+T7*S(IP7)+T8*S(IP8)
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
              WRK01(IBP)=T1*S(IP1)+T2*S(IP2)+T3*S(IP3)+T4*S(IP4)
     *                  +T5*S(IP5)+T6*S(IP6)
          ELSE IF(NODE(NPRD,IE).NE.0) THEN
              IP1=NODE(1,IE) 
              IP2=NODE(2,IE) 
              IP3=NODE(3,IE) 
              IP4=NODE(4,IE) 
              IP5=NODE(5,IE) 
              T1=0.25E0*((1.-GP)*(1.-EP)-TP+GP*EP*TP/(1.-TP))
              T2=0.25E0*((1.+GP)*(1.-EP)-TP-GP*EP*TP/(1.-TP))
              T3=0.25E0*((1.+GP)*(1.+EP)-TP+GP*EP*TP/(1.-TP))
              T4=0.25E0*((1.-GP)*(1.+EP)-TP-GP*EP*TP/(1.-TP))
              T5= TP
              WRK01(IBP)=T1*S(IP1)+T2*S(IP2)+T3*S(IP3)+T4*S(IP4)
     *                  +T5*S(IP5)
          ELSE IF(NODE(NTET,IE).NE.0) THEN
              IP1=NODE(1,IE) 
              IP2=NODE(2,IE) 
              IP3=NODE(3,IE) 
              IP4=NODE(4,IE) 
              T1=GP
              T2=EP
              T3=TP
              T4=1.0E0-(GP+EP+TP)
              WRK01(IBP)=T1*S(IP1)+T2*S(IP2)+T3*S(IP3)+T4*S(IP4)
          ELSE
              WRITE(IUT0,*)'HEAT3X:INVALID NODE TABLE:ERROR'
              IERR=1      
              RETURN
          ENDIF
 1000 CONTINUE
C
C         PERFORM SELF-DOMAIN VELOCITY OVERSETS
C
      NB = 0
*POPTION INDEP(T)
      DO 2000 IBP = 1 , NPSET
          ISEND = LPSET3(IBP)
          IF(ISEND.LT.0) GO TO 2000
C
          IP = LPSET1(IBP)
          IF(ISEND.EQ.0) THEN
              S(IP) = WRK01(IBP)
          ELSE
              NB = NB+1
              WRK02(NB) = WRK01(IBP)
          ENDIF
 2000 CONTINUE
C
C         PERFORM INTER-DOMAIN VELOCITY OVERSETS
C
      IF(IPART.GE.1) THEN
          IDIM=1
          CALL DDSET3(NPSND,LPSND,NPTSND,IPSET,IPSRC,
     *                WRK02,WRK02,WRK02,NB,
     *                NPRCV,LPRCV,NPTRCV,S,S,S,NP,
     *                IDIM,MBPDOM,IUT0,IERR,RX,RY,MAXBUF)
          IF(IERR.NE.0) THEN
              WRITE(IUT0,*)
              WRITE(IUT0,*) ERMSGC
              RETURN
          ENDIF
      ENDIF
C
      RETURN
      END
