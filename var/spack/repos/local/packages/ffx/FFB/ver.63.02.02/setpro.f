      SUBROUTINE SETPRO(IVOF,IPRESS,N2,NE,NP,NEX,NODE,IEPROP,
     *                  MAXPRO,LPRO,CPRO0,CPRO1,CPRO2,CTREF,
     *                  FE,RHOF,RHOF2,RHOS,VISCM,VISCM2,
     *                  CPF,CPS,CONDF,CONDS,PRT,T000,TREF,T,FLE,
     *                  RHO3D,VISC,RHOCP,COND3D,TELM,IUT0,IERR)
      IMPLICIT NONE
C
C [INPUT] 
      INTEGER*4 IVOF,IPRESS,MAXPRO,LPRO (MAXPRO)
      REAL*4    CPRO0(9,MAXPRO),CPRO1(4,MAXPRO),
     *          CPRO2(4,MAXPRO),CTREF(4,MAXPRO)
      INTEGER*4 N2,NE,NP,NEX(8)
      INTEGER*4 NODE(N2,NE),IEPROP(NE)
      REAL*4    FE(NE),RHOF,RHOF2,RHOS,VISCM,VISCM2,
     *          CPF,CPS,CONDF,CONDS,PRT,T000,TREF,T(NP),FLE(NE)
      INTEGER*4 IUT0
C
C [OUTPUT]
      REAL*4    RHO3D(NE),VISC(NE),RHOCP(NE),COND3D(NE)
      INTEGER*4 IERR
C
C [WORK]
      REAL*4    TELM(NE)
C
C [LOCAL]
      INTEGER*4 IES1,IEE1,IES2,IEE2,IES3,IEE3,IES4,IEE4,
     *          IE,IP1,IP2,IP3,IP4,IP5,IP6,IP7,IP8,
     *          NETET,NEPRD,NEWED,NEHEX,IPRO
      REAL*4    COEF,TBUF1,TBUF2,TBUF3,TBUF4,CPBUF,FTMP
C
      IERR=0
C
      IF (IVOF.GE.1) GOTO 2200
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
C [1.] CAL. PESSURE AT ELEMENTS
C
C     OPERATION COUNTS:      4 FLOP /ELEMENT
C     DATA LOADINGS   :      8 WORDS/ELEMENT
C                      (     0 WORDS CONTIGUOUSLY,
C                            4 WORDS BY STRIDE, AND
C                            4 WORDS BY LIST )
C
      COEF=1.0E0/4.0E0
      DO 1010 IE = IES1 , IEE1
          IP1=NODE(1,IE)
          IP2=NODE(2,IE)
          IP3=NODE(3,IE)
          IP4=NODE(4,IE)
          TELM(IE) = COEF*(T(IP1)+T(IP2)+T(IP3)+T(IP4))
 1010 CONTINUE
C
C     OPERATION COUNTS:      5 FLOP /ELEMENT
C     DATA LOADINGS   :     10 WORDS/ELEMENT
C                      (     0 WORDS CONTIGUOUSLY,
C                            5 WORDS BY STRIDE, AND
C                            5 WORDS BY LIST )
      COEF=1.0E0/5.0E0
      DO 1020 IE = IES2 , IEE2
          IP1=NODE(1,IE)
          IP2=NODE(2,IE)
          IP3=NODE(3,IE)
          IP4=NODE(4,IE)
          IP5=NODE(5,IE)
          TELM(IE) = COEF*(T(IP1)+T(IP2)+T(IP3)+T(IP4)
     *                    +T(IP5))
 1020 CONTINUE
C
C     OPERATION COUNTS:      6 FLOP /ELEMENT
C     DATA LOADINGS   :     12 WORDS/ELEMENT
C                      (     0 WORDS CONTIGUOUSLY,
C                            6 WORDS BY STRIDE, AND
C                            6 WORDS BY LIST )
C
      COEF=1.0E0/6.0E0
      DO 1030 IE = IES3 , IEE3
          IP1=NODE(1,IE)
          IP2=NODE(2,IE)
          IP3=NODE(3,IE)
          IP4=NODE(4,IE)
          IP5=NODE(5,IE)
          IP6=NODE(6,IE)
          TELM(IE) = COEF*(T(IP1)+T(IP2)+T(IP3)+T(IP4)
     *                    +T(IP5)+T(IP6))
 1030 CONTINUE
C
C     OPERATION COUNTS:      8 FLOP /ELEMENT
C     DATA LOADINGS   :     16 WORDS/ELEMENT
C                      (     0 WORDS CONTIGUOUSLY,
C                            8 WORDS BY STRIDE, AND
C                            8 WORDS BY LIST )
C
      COEF=1.0E0/8.0E0
      DO 1040 IE = IES4 , IEE4
          IP1=NODE(1,IE)
          IP2=NODE(2,IE)
          IP3=NODE(3,IE)
          IP4=NODE(4,IE)
          IP5=NODE(5,IE)
          IP6=NODE(6,IE)
          IP7=NODE(7,IE)
          IP8=NODE(8,IE)
          TELM(IE) = COEF*(T(IP1)+T(IP2)+T(IP3)+T(IP4)
     *                    +T(IP5)+T(IP6)+T(IP7)+T(IP8))
 1040 CONTINUE
C
C
C [2.] CAL. PROPERTIES AT ELEMENTS
C
      DO 2000 IE=1,NE
C
          IPRO=IEPROP(IE)
          IF(IPRO.EQ.0.OR.IPRO.EQ.1) GOTO 2100
C
          IF(LPRO(IPRO).EQ.0) THEN
              WRITE(IUT0,*) 'SETPRO:PROPERTY ID IS NOT DEFINED:ERROR'
              IERR=1
              RETURN
          ENDIF  
 2100 CONTINUE   
C
          IF(IPRO.EQ.0) THEN
CCYYMOD---
CC            COEF=1.0E0+TELM(IE)*T000/TREF
              COEF=1.0E0
CCYYMOD---
              IF(COEF.LE.0) THEN
                  WRITE(IUT0,*) 'SETPRO: INVALID VALUE FOR RHO3D'
                  WRITE(IUT0,*) 'SETPRO: ERROR                '
                  IERR=1
                  RETURN
              ENDIF              
              RHO3D (IE)=1.0E0/COEF
C             RHO3D (IE)=1.0E0
              RHOCP (IE)=RHOF*CPF
              COND3D(IE)=CONDF+RHOF*CPF*VISC(IE)/PRT
          ELSE IF (IPRO.EQ.1) THEN
              RHO3D (IE)=RHOS
              RHOCP (IE)=RHOS*CPS
              COND3D(IE)=CONDS
          ELSE
              TBUF1=TELM(IE)-CTREF(1,IPRO)
              TBUF2=TELM(IE)-CTREF(2,IPRO)
              TBUF3=TELM(IE)-CTREF(3,IPRO)
              TBUF4=TELM(IE)-CTREF(4,IPRO)
              RHO3D(IE)=  CPRO0(1,IPRO)
     *                   +CPRO1(1,IPRO)*TBUF1
     *                   +CPRO2(1,IPRO)*TBUF1*TBUF1
              CPBUF     = CPRO0(2,IPRO)
     *                   +CPRO1(2,IPRO)*TBUF2
     *                   +CPRO2(2,IPRO)*TBUF2*TBUF2
              RHOCP (IE)= RHO3D(IE)*CPBUF      
              COND3D(IE)= CPRO0(4,IPRO)
     *                   +CPRO1(4,IPRO)*TBUF4
     *                   +CPRO2(4,IPRO)*TBUF4*TBUF4
          ENDIF
C
          VISC(IE)=VISCM
 2000 CONTINUE
C
      IF(IPRESS.EQ.3) THEN
         DO 2110 IE=1,NE
            RHO3D(IE)=RHO3D(IE)*FLE(IE)
 2110    CONTINUE
      ENDIF
C
      RETURN
C
 2200 CONTINUE
C
      DO 3000 IE=1,NE
         FTMP=AMIN1( AMAX1(FE(IE),0.0E0), 1.0E0 )
         RHO3D(IE)=FTMP*RHOF +(1.0E0-FTMP)*RHOF2
         VISC (IE)=FTMP*VISCM+(1.0E0-FTMP)*VISCM2
 3000 CONTINUE
C
C
      RETURN
      END

