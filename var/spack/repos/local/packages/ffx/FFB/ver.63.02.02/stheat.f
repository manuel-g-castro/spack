      SUBROUTINE STHEAT(N2,NE,NP,NSP,NS,LOCAL,NODE,
     *                  NEHSRC,NPHFIX,NPHTRS,NPHEAT,NEHEAT,
     *                  LEHSRC,LPHEAT,LEHEAT,RHOS,CPS,
     *                  T,TREF,HSRC,HFIX,HTRS,DTDT,HEAT,HEATE,
     *                  RHOCP,WRK)
      IMPLICIT NONE
      INTEGER*4 N2,NE,NP,NSP,NS,LOCAL(NSP,NS,4),
     *          NEHSRC,NPHFIX,NPHTRS,NPHEAT,NEHEAT
      INTEGER*4 NODE(N2,NE),
     *          LEHSRC(NEHSRC),LPHFIX(NPHFIX),
     *          LPHEAT(NPHEAT),LEHEAT(2,NEHEAT)
      REAL*4    T(NP),TREF,
     *          HSRC(NEHSRC),HFIX(NPHFIX),
     *          HTRS(NPHTRS),DTDT(NE),
     *          HEAT(NPHEAT),HEATE(NEHEAT),RHOCP(NE)
      REAL*4    RHOS,CPS
      INTEGER*4 IE,IP,IS,IBE,IBP,IETYPE,NNPS,I
      REAL*4    WRK(NP),BUF
CC
CC [2] SET HEAT SOURCE
CC
      DO 2000 IE=1,NE
          DTDT(IE)=0.0E0
 2000 CONTINUE
C
      DO 2100 IBE=1,NEHSRC
          IE=LEHSRC(IBE)
          DTDT(IE)=HSRC(IBE)
 2100 CONTINUE
CC
CC [3] SET HEAT FLUX AT BOUNDARY
CC
      DO 3000 IBP=1,NPHFIX
          HEAT(IBP)=HFIX(IBP)
 3000 CONTINUE     
C
      DO 3100 IBP=1,NPHTRS
          IP=LPHEAT(NPHFIX+IBP)
          HEAT(NPHFIX+IBP)=HTRS(IBP)*(TREF-T(IP))*RHOS*CPS
 3100 CONTINUE     
C
      DO 3200 IP=1,NP
          WRK(IP)=0.0E0
 3200 CONTINUE     
C
      DO 3300 IBP=1,NPHEAT
          IP=LPHEAT(IBP)  
          WRK(IP)=HEAT(IBP)
 3300 CONTINUE     
C
      DO 3400 IBE=1,NEHEAT
          IE=LEHEAT(1,IBE)
          IS=LEHEAT(2,IBE)
          IF(     NODE(8,IE).GE.1) THEN ! HEX
              IETYPE = 4
          ELSE IF(NODE(6,IE).GE.1) THEN ! PRS
              IETYPE = 3
          ELSE IF(NODE(5,IE).GE.1) THEN ! PYR
              IETYPE = 2
          ELSE                          ! TET  
              IETYPE = 1
          ENDIF   
          IF(LOCAL(4,IS,IETYPE).GE.1) THEN ! QUADRILATERAL
              NNPS = 4
          ELSE                             ! TRIANGLE
              NNPS = 3
          ENDIF   
          HEATE(IBE)=0.0E0
          DO 3500 I=1,NNPS
              BUF=WRK(NODE(LOCAL(I,IS,IETYPE),IE))
              BUF=BUF/RHOCP(IE)
              HEATE(IBE)=HEATE(IBE)+BUF
 3500     CONTINUE
          HEATE(IBE) = HEATE(IBE)/FLOAT(NNPS)
 3400 CONTINUE     
C
      RETURN
      END        
