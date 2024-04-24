      SUBROUTINE HSRC3X(N1,N2,NE,NP,NEX,NODE,DT,DTDT,RHS,SN,
     *                  NEHEAT,LEHEAT,HEATE,SHEAT)
      IMPLICIT NONE
      INTEGER*4 N1,N2,NE,NP,NEX(12)
      INTEGER*4 NODE(N2,NE),NEHEAT,LEHEAT(2,NEHEAT)
      REAL*4    DT,DTDT(NE),RHS(NP),SN(N1,NE),
     *          HEATE(NEHEAT),SHEAT(N1,NEHEAT) 
      INTEGER*4 NETET,NEPRD,NEWED,NEHEX,
     *          IE,IS,
     *          IES1,IEE1,IES2,IEE2,IES3,IEE3,IES4,IEE4,
     *          IP1,IP2,IP3,IP4,IP5,IP6,IP7,IP8,IBE
      REAL*4    HBUF
C   
      NETET =NEX( 1)
      NEPRD =NEX( 2)
      NEWED =NEX( 3)
      NEHEX =NEX( 4)
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
C          OPERATION COUNTS:   12 FLOP /ELEMENT
C          DATA LOADINGS   :   14 WORDS/ELEMENT
C                           (   2 WORDS CONTIGUOUSLY,
C                               8 WORDS BY STRIDE, AND
C                               4 WORDS BY LIST )
      DO 1000 IE=IES1,IEE1
          IP1=NODE(1,IE)
          IP2=NODE(2,IE)
          IP3=NODE(3,IE)
          IP4=NODE(4,IE)
          RHS(IP1)=RHS(IP1)+DTDT(IE)*DT*SN(1,IE)
          RHS(IP2)=RHS(IP2)+DTDT(IE)*DT*SN(2,IE)
          RHS(IP3)=RHS(IP3)+DTDT(IE)*DT*SN(3,IE)
          RHS(IP4)=RHS(IP4)+DTDT(IE)*DT*SN(4,IE)
 1000 CONTINUE
C
C          OPERATION COUNTS:   15 FLOP /ELEMENT
C          DATA LOADINGS   :   17 WORDS/ELEMENT
C                           (   2 WORDS CONTIGUOUSLY,
C                              10 WORDS BY STRIDE, AND
C                               5 WORDS BY LIST )
      DO 1100 IE=IES2,IEE2
          IP1=NODE(1,IE)
          IP2=NODE(2,IE)
          IP3=NODE(3,IE)
          IP4=NODE(4,IE)
          IP5=NODE(5,IE)
          RHS(IP1)=RHS(IP1)+DTDT(IE)*DT*SN(1,IE)
          RHS(IP2)=RHS(IP2)+DTDT(IE)*DT*SN(2,IE)
          RHS(IP3)=RHS(IP3)+DTDT(IE)*DT*SN(3,IE)
          RHS(IP4)=RHS(IP4)+DTDT(IE)*DT*SN(4,IE)
          RHS(IP5)=RHS(IP5)+DTDT(IE)*DT*SN(5,IE)
 1100 CONTINUE
C
C          OPERATION COUNTS:   18 FLOP /ELEMENT
C          DATA LOADINGS   :   20 WORDS/ELEMENT
C                           (   2 WORDS CONTIGUOUSLY,
C                              12 WORDS BY STRIDE, AND
C                               6 WORDS BY LIST )
      DO 1200 IE=IES3,IEE3
          IP1=NODE(1,IE)
          IP2=NODE(2,IE)
          IP3=NODE(3,IE)
          IP4=NODE(4,IE)
          IP5=NODE(5,IE)
          IP6=NODE(6,IE)
          RHS(IP1)=RHS(IP1)+DTDT(IE)*DT*SN(1,IE)
          RHS(IP2)=RHS(IP2)+DTDT(IE)*DT*SN(2,IE)
          RHS(IP3)=RHS(IP3)+DTDT(IE)*DT*SN(3,IE)
          RHS(IP4)=RHS(IP4)+DTDT(IE)*DT*SN(4,IE)
          RHS(IP5)=RHS(IP5)+DTDT(IE)*DT*SN(5,IE)
          RHS(IP6)=RHS(IP6)+DTDT(IE)*DT*SN(6,IE)
 1200 CONTINUE
C
C          OPERATION COUNTS:   24 FLOP /ELEMENT
C          DATA LOADINGS   :   26 WORDS/ELEMENT
C                           (   2 WORDS CONTIGUOUSLY,
C                              16 WORDS BY STRIDE, AND
C                               8 WORDS BY LIST )
      DO 1300 IE=IES4,IEE4
          IP1=NODE(1,IE)
          IP2=NODE(2,IE)
          IP3=NODE(3,IE)
          IP4=NODE(4,IE)
          IP5=NODE(5,IE)
          IP6=NODE(6,IE)
          IP7=NODE(7,IE)
          IP8=NODE(8,IE)
          RHS(IP1)=RHS(IP1)+DTDT(IE)*DT*SN(1,IE)
          RHS(IP2)=RHS(IP2)+DTDT(IE)*DT*SN(2,IE)
          RHS(IP3)=RHS(IP3)+DTDT(IE)*DT*SN(3,IE)
          RHS(IP4)=RHS(IP4)+DTDT(IE)*DT*SN(4,IE)
          RHS(IP5)=RHS(IP5)+DTDT(IE)*DT*SN(5,IE)
          RHS(IP6)=RHS(IP6)+DTDT(IE)*DT*SN(6,IE)
          RHS(IP7)=RHS(IP7)+DTDT(IE)*DT*SN(7,IE)
          RHS(IP8)=RHS(IP8)+DTDT(IE)*DT*SN(8,IE)
 1300 CONTINUE
C
      DO 2000 IBE=1,NEHEAT
          IE=LEHEAT(1,IBE)
          HBUF=HEATE(IBE)*DT 
          IF(     NODE(8,IE).GE.1) THEN ! HEX
              IP1=NODE(1,IE) 
              IP2=NODE(2,IE) 
              IP3=NODE(3,IE) 
              IP4=NODE(4,IE) 
              IP5=NODE(5,IE) 
              IP6=NODE(6,IE) 
              IP7=NODE(7,IE) 
              IP8=NODE(8,IE) 
              RHS(IP1)=RHS(IP1)+HBUF*SHEAT(1,IBE)
              RHS(IP2)=RHS(IP2)+HBUF*SHEAT(2,IBE)
              RHS(IP3)=RHS(IP3)+HBUF*SHEAT(3,IBE)
              RHS(IP4)=RHS(IP4)+HBUF*SHEAT(4,IBE)
              RHS(IP5)=RHS(IP5)+HBUF*SHEAT(5,IBE)
              RHS(IP6)=RHS(IP6)+HBUF*SHEAT(6,IBE)
              RHS(IP7)=RHS(IP7)+HBUF*SHEAT(7,IBE)
              RHS(IP8)=RHS(IP8)+HBUF*SHEAT(8,IBE)
C
          ELSE IF(NODE(6,IE).GE.1) THEN ! PRS
              IP1=NODE(1,IE) 
              IP2=NODE(2,IE) 
              IP3=NODE(3,IE) 
              IP4=NODE(4,IE) 
              IP5=NODE(5,IE) 
              IP6=NODE(6,IE) 
              RHS(IP1)=RHS(IP1)+HBUF*SHEAT(1,IBE)
              RHS(IP2)=RHS(IP2)+HBUF*SHEAT(2,IBE)
              RHS(IP3)=RHS(IP3)+HBUF*SHEAT(3,IBE)
              RHS(IP4)=RHS(IP4)+HBUF*SHEAT(4,IBE)
              RHS(IP5)=RHS(IP5)+HBUF*SHEAT(5,IBE)
              RHS(IP6)=RHS(IP6)+HBUF*SHEAT(6,IBE)
C
          ELSE IF(NODE(5,IE).GE.1) THEN ! PYR
              IP1=NODE(1,IE) 
              IP2=NODE(2,IE) 
              IP3=NODE(3,IE) 
              IP4=NODE(4,IE) 
              IP5=NODE(5,IE) 
              RHS(IP1)=RHS(IP1)+HBUF*SHEAT(1,IBE)
              RHS(IP2)=RHS(IP2)+HBUF*SHEAT(2,IBE)
              RHS(IP3)=RHS(IP3)+HBUF*SHEAT(3,IBE)
              RHS(IP4)=RHS(IP4)+HBUF*SHEAT(4,IBE)
              RHS(IP5)=RHS(IP5)+HBUF*SHEAT(5,IBE)
C
          ELSE                          ! TET  
              IP1=NODE(1,IE) 
              IP2=NODE(2,IE) 
              IP3=NODE(3,IE) 
              IP4=NODE(4,IE) 
              RHS(IP1)=RHS(IP1)+HBUF*SHEAT(1,IBE)
              RHS(IP2)=RHS(IP2)+HBUF*SHEAT(2,IBE)
              RHS(IP3)=RHS(IP3)+HBUF*SHEAT(3,IBE)
              RHS(IP4)=RHS(IP4)+HBUF*SHEAT(4,IBE)
          ENDIF   
 2000 ENDDO   
C
      RETURN
      END
