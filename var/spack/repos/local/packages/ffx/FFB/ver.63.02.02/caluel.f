      SUBROUTINE CALUEL(N2,NE,NP,NEX,NODE,
     *                  U,V,W,UE,VE,WE,IUT6,IERR)
      IMPLICIT NONE
C
      INTEGER*4 N2,NE,NP,NEX(12)
      INTEGER*4 NODE(N2,NE)
      INTEGER*4 IUT6,IERR
      REAL*4    U(NP),V(NP),W(NP),UE(NE),VE(NE),WE(NE)
C               
      INTEGER*4 NETET,NEPRD,NEWED,NEHEX,
     *          IE,IES1,IEE1,IES2,IEE2,IES3,IEE3,IES4,IEE4,
     *          IP1,IP2,IP3,IP4,IP5,IP6,IP7,IP8
      REAL*4    COEF
C
#ifdef USE_TIMER
      real*8 ts0, te0

      include 'timer.h'
      include 'mpif.h'

      ncaluel = ncaluel + 1
      tstart = MPI_WTIME()
#endif
      IERR=0
C
      NETET=NEX(1)
      NEPRD=NEX(2)
      NEWED=NEX(3)
      NEHEX=NEX(4)
C
C   == TET. ==  
      IES1=1
      IEE1=NETET 
C
C   == PYRAMID ==  
      IES2=NETET+1
      IEE2=NETET+NEPRD
C
C   == WEDGE ==  
      IES3=NETET+NEPRD+1
      IEE3=NETET+NEPRD+NEWED
C
C   == HEX. ==  
      IES4=NETET+NEPRD+NEWED+1
      IEE4=NETET+NEPRD+NEWED+NEHEX 
C
      COEF=1.0E0/4.0E0
      DO 210 IE = IES1 , IEE1
          IP1=NODE(1,IE)
          IP2=NODE(2,IE)
          IP3=NODE(3,IE)
          IP4=NODE(4,IE)
          UE(IE) = COEF*(U(IP1)+U(IP2)+U(IP3)+U(IP4))
          VE(IE) = COEF*(V(IP1)+V(IP2)+V(IP3)+V(IP4))
          WE(IE) = COEF*(W(IP1)+W(IP2)+W(IP3)+W(IP4))
  210 CONTINUE
C
      COEF=1.0E0/5.0E0
      DO 220 IE = IES2 , IEE2
          IP1=NODE(1,IE)
          IP2=NODE(2,IE)
          IP3=NODE(3,IE)
          IP4=NODE(4,IE)
          IP5=NODE(5,IE)
          UE(IE) = COEF*(U(IP1)+U(IP2)+U(IP3)+U(IP4)
     *                  +U(IP5))
          VE(IE) = COEF*(V(IP1)+V(IP2)+V(IP3)+V(IP4)
     *                  +V(IP5))
          WE(IE) = COEF*(W(IP1)+W(IP2)+W(IP3)+W(IP4)
     *                  +W(IP5))
  220 CONTINUE
C
      COEF=1.0E0/6.0E0
      DO 230 IE = IES3 , IEE3
          IP1=NODE(1,IE)
          IP2=NODE(2,IE)
          IP3=NODE(3,IE)
          IP4=NODE(4,IE)
          IP5=NODE(5,IE)
          IP6=NODE(6,IE)
          UE(IE) = COEF*(U(IP1)+U(IP2)+U(IP3)+U(IP4)
     *                  +U(IP5)+U(IP6))
          VE(IE) = COEF*(V(IP1)+V(IP2)+V(IP3)+V(IP4)
     *                  +V(IP5)+V(IP6))
          WE(IE) = COEF*(W(IP1)+W(IP2)+W(IP3)+W(IP4)
     *                  +W(IP5)+W(IP6))
 230   CONTINUE
C
      COEF=1.0E0/8.0E0
      DO 240 IE = IES4 , IEE4
          IP1=NODE(1,IE)
          IP2=NODE(2,IE)
          IP3=NODE(3,IE)
          IP4=NODE(4,IE)
          IP5=NODE(5,IE)
          IP6=NODE(6,IE)
          IP7=NODE(7,IE)
          IP8=NODE(8,IE)
          UE(IE) = COEF*(U(IP1)+U(IP2)+U(IP3)+U(IP4)
     *                  +U(IP5)+U(IP6)+U(IP7)+U(IP8))
          VE(IE) = COEF*(V(IP1)+V(IP2)+V(IP3)+V(IP4)
     *                  +V(IP5)+V(IP6)+V(IP7)+V(IP8))
          WE(IE) = COEF*(W(IP1)+W(IP2)+W(IP3)+W(IP4)
     *                  +W(IP5)+W(IP6)+W(IP7)+W(IP8))
 240  CONTINUE
C
#ifdef USE_TIMER
      tend = MPI_WTIME()
      tcaluel = tcaluel + (tend - tstart)
#endif      
      RETURN
      END
