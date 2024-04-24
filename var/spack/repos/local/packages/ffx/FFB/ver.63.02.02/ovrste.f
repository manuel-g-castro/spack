      SUBROUTINE OVRSTE(IPART,NESET,N1,N2,ME,NE,NP,NEX,NODE,S,SP,
     *                  LESET1,LESET2,LESET3,EOVER1,EOVER2,EOVER3,
     *                  NDOM,MBPDOM,NESND,NERCV,
     *                  LESND,NETSND,LERCV,NETRCV,IESET,IESRC,
     *                  WRK01,WRK02,RX,RY,IUT0,IERR)
      IMPLICIT NONE
      INTEGER*4 IPART,NESET,N1,N2,ME,NE,NP,NEX(12)
      INTEGER*4 NODE(N2,NE)
      REAL*4    S(NE),SP(NP)
      INTEGER*4 LESET1(NESET),LESET2(NESET),LESET3(NESET)
      REAL*4    EOVER1(NESET),EOVER2(NESET),EOVER3(NESET)
      INTEGER*4 NDOM,MBPDOM,NESND,NERCV,
     *          LESND(NDOM),NETSND(NDOM),
     *          LERCV(NDOM),NETRCV(NDOM),
     *          IESET (MBPDOM,NDOM),IESRC (MBPDOM,NDOM)
      REAL*4    WRK01(NE),WRK02(NE),RX(N1,ME),RY(N1,ME)
      INTEGER*4 IUT0,IERR  
C
      INTEGER*4 NETET,NEPRD,NEWED,NEHEX,NTET,NPRD,NWED,NHEX,
     *          IBE,ISEND,IE,IP,NB,MAXBUF,IDIM,
     *          IP1,IP2,IP3,IP4,IP5,IP6,IP7,IP8
      REAL*4    GP,EP,TP,T1,T2,T3,T4,T5,T6,T7,T8
C
      CHARACTER*60 ERMSGC
     & / ' ## SUBROUTINE OVRSTE: FATAL      ERROR REPORT   ; RETURNED' /
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
      DO 1000 IBE = 1 , NESET
          ISEND = LESET3(IBE)
          IF(ISEND.LT.0) GO TO 1000
C
          IE = LESET2(IBE)
          GP = EOVER1(IBE)
          EP = EOVER2(IBE)
          TP = EOVER3(IBE)
CCYY---
          IF(IE.EQ.0) THEN
              WRK01(IBE)=S(LESET1(IBE))  
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
              WRK01(IBE)=T1*SP(IP1)+T2*SP(IP2)+T3*SP(IP3)+T4*SP(IP4)
     *                  +T5*SP(IP5)+T6*SP(IP6)+T7*SP(IP7)+T8*SP(IP8)
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
              WRK01(IBE)=T1*SP(IP1)+T2*SP(IP2)+T3*SP(IP3)+T4*SP(IP4)
     *                  +T5*SP(IP5)+T6*SP(IP6)
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
              WRK01(IBE)=T1*SP(IP1)+T2*SP(IP2)+T3*SP(IP3)+T4*SP(IP4)
     *                  +T5*SP(IP5)
          ELSE IF(NODE(NTET,IE).NE.0) THEN
              IP1=NODE(1,IE) 
              IP2=NODE(2,IE) 
              IP3=NODE(3,IE) 
              IP4=NODE(4,IE) 
              T1=GP
              T2=EP
              T3=TP
              T4=1.0E0-(GP+EP+TP)
              WRK01(IBE)=T1*SP(IP1)+T2*SP(IP2)+T3*SP(IP3)+T4*SP(IP4)
          ELSE
CC            WRITE(IUT0,*)'HEAT3X:INVALID NODE TABLE:ERROR'
CC            IERR=1      
CC            RETURN
          ENDIF
 1000 CONTINUE
C
C         PERFORM SELF-DOMAIN VELOCITY OVERSETS
C
      NB = 0
*POPTION INDEP(T)
      DO 2000 IBE = 1 , NESET
          ISEND = LESET3(IBE)
          IF(ISEND.LT.0) GO TO 2000
C
          IE = LESET1(IBE)
          IF(ISEND.EQ.0) THEN
              S(IE) = WRK01(IBE)
          ELSE
              NB = NB+1
              WRK02(NB) = WRK01(IBE)
          ENDIF
 2000 CONTINUE
C
C         PERFORM INTER-DOMAIN VELOCITY OVERSETS
C
      IF(IPART.GE.1) THEN
          IDIM=1
          CALL DDSET3(NESND,LESND,NETSND,IESET,IESRC,
     *                WRK02,WRK02,WRK02,NB,
     *                NERCV,LERCV,NETRCV,S,S,S,NE,
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
