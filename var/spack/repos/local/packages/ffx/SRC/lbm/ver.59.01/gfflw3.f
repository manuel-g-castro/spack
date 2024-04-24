      SUBROUTINE GFFLW3(IMODE,FILEFF,
     *                  NG,NC,NP,NTIME,CVEL,VSCALE,TIME,V3D,
     *                  MPBOUN,NPBOUN,LPBOUN,
     *                  MCOM,NCOMFL,NCOMST,COMFLE,COMSET,
     *                  IUT6,IUT0,IUTFF,IERR,
     *                  NG3,WRK1,WRK2,WRK3,WRK4)
      IMPLICIT NONE
      INTEGER*4    IMODE,NG,NC,NP,NTIME
      REAL*8       TIME,CVEL(9,NP),VSCALE,
     *             V3D(4,0:NG+2,0:NG+2,0:NG+2)
      INTEGER*4    MPBOUN,NPBOUN(NC),LPBOUN(5,MPBOUN,NC)
      INTEGER*4    IUT6,IUT0,IUTFF,IERR
C
      INTEGER*4    MCOM,NCOMFL,NCOMST
      CHARACTER*60 FILEFF,COMFLE(MCOM),COMSET(MCOM)
C
      INTEGER*4    NG3
      REAL*4       TBUF,WRK1(NG3),WRK2(NG3),WRK3(NG3),WRK4(NG3)
C
      INTEGER*4 IACT,ITARGT,IRESV,IWRITE,INAME,
     *          ICAST,ICAST1,IDATA0,IALL,ISKIP,ISKIP1,ICHECK
      INTEGER*4 MAXPRN
      CHARACTER*30 NAME
      DATA IWRITE / 2 /
      DATA INAME  / 1 /
      DATA MAXPRN / 200 /
      DATA ICAST  /0/
      DATA ICAST1 /1/
      DATA IDATA0 /0/
      DATA IALL   /0/
      DATA ISKIP  /0/
      DATA ISKIP1 /1/
      DATA ICHECK /999999/
C
      INTEGER*4 N4,NDUM
      DATA N4   /4/
C
      INTEGER*4 I,J,K,IG,IC,MG,IPB,NDUM1,NDUM2
C
C[INPUT]
C     IMODE : SPECIFY ACTION MODE (0:CHECK SIZE, 1:READ, 2:WRITE)
C     MC    : MAX. NUMBER OF B.C GROUPS IN CUBES (=152)
C     IUT6  : UNIT NUMBER OF STANDARD OUTPUT
C     IUT0  : UNIT NUMBER OF ERROR OUTPUT
C     IUTFF : UNIT NUMBER OF GF-FLOW FILE 
C
C[INPUT&OUTPUT] 
C
C[OUTPUT]
C     IERR   : ERROR FLAG
C
      IERR=0
      MG=(NG+3)*(NG+3)*(NG+3) 
C
      IF(IMODE.EQ.1) THEN
          WRITE(IUT6,*) 
          WRITE(IUT6,*) '  GFFLW3 :: READ GF-FLOW FILE   '
          GOTO 1000
      ELSE IF(IMODE.EQ.2) THEN
          WRITE(IUT6,*) 
          WRITE(IUT6,*) '  GFFLW3 :: WRITE GF-FLOW FILE   '
          GOTO 2000
      ELSE IF(IMODE.EQ.3) THEN
          WRITE(IUT6,*)
          WRITE(IUT6,*) '  GFFLW3 :: OPEN GF-FLOW FILE   '
          GOTO 1000
      ELSE IF(IMODE.EQ.4) THEN
          WRITE(IUT6,*)
          WRITE(IUT6,*) '  GFFLW3 :: OPEN GF-FLOW FILE   '
          GOTO 2000
      ELSE IF(IMODE.EQ.5) THEN
          WRITE(IUT6,*)
          WRITE(IUT6,*) '  GFFLW3 :: READ GF-FLOW FILE   '
          GOTO 5000
      ELSE IF(IMODE.EQ.6) THEN
          WRITE(IUT6,*)
          WRITE(IUT6,*) '  GFFLW3 :: APPEND GF-FLOW FILE   '
          GOTO 6000
      ELSE IF(IMODE.EQ.7) THEN
          WRITE(IUT6,*)
          WRITE(IUT6,*) '  GFFLW3 :: CLOSE GF-FLOW FILE   '
          GOTO 7000
      ELSE IF(IMODE.EQ.8) THEN
          WRITE(IUT6,*)
          WRITE(IUT6,*) '  GFFLW3 :: CLOSE GF-FLOW FILE   '
          GOTO 8000
      ELSE
          IERR=1
          RETURN
      ENDIF
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                      C
C    IMODE=1: READ MODE                                C 
C                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
 1000 CONTINUE
C
C
CCC OPEN GF-FLOW FILE TO BE READ
C
      IACT = 3
      CALL GFALL(IUT0,IUT6,IUTFF,FILEFF,
     *           MCOM,NCOMFL,COMFLE,
     *           MCOM,NCOMST,COMSET,
     *           IACT,IWRITE,INAME,IRESV,
     *           ICAST,IDATA0,IALL,ISKIP,IERR,
     *           ' !',ICHECK)
C
      IF(IMODE.EQ.3) RETURN 
C
 5000 CONTINUE   
C
CCC OPEN GF-FLOW FILE TO BE READ
C
      DO 1100 IC=1,NC
          IACT = 5
          TBUF=REAL(TIME)
          CALL GFALL(IUT0,IUT6,IUTFF,FILEFF,
     *               MCOM,NCOMFL,COMFLE,
     *               MCOM,NCOMST,COMSET,
     *               IACT,IWRITE,INAME,IRESV,  
     *               ICAST,IDATA0,IALL,ISKIP,IERR,
     *               ' *TIME_PS *STEP_PS *LBM_RHO *LBM_VEL !',
     *               NAME,TBUF,
     *               NAME,NTIME,
     *               NAME,NG3,NDUM1,WRK1,
     *               NAME,NG3,NDUM2,WRK2,WRK3,WRK4,
     *               ICHECK)
C
          IG=0
          DO 1110 K=0,NG+2
          DO 1120 J=0,NG+2
          DO 1130 I=0,NG+2
              IG=IG+1
              V3D(1,I,J,K)=DBLE(WRK1(IG))
              V3D(2,I,J,K)=DBLE(WRK2(IG)*VSCALE)
              V3D(3,I,J,K)=DBLE(WRK3(IG)*VSCALE)
              V3D(4,I,J,K)=DBLE(WRK4(IG)*VSCALE)
 1130     CONTINUE
 1120     CONTINUE
 1110     CONTINUE
C
 1100 CONTINUE
C
      IF(IMODE.EQ.5) RETURN
C
CCC CLOSE GF-FLOW FILE
C
 7000 CONTINUE
C
      IACT = 7
      CALL GFALL(IUT0,IUT6,IUTFF,FILEFF,
     *           MCOM,NCOMFL,COMFLE,
     *           MCOM,NCOMST,COMSET,
     *           IACT,IWRITE,INAME,IRESV,
     *           ICAST,IDATA0,IALL,ISKIP,IERR,
     *           ' !',ICHECK)
C
      RETURN
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                      C
C    IMODE=2: WRITE MODE                               C 
C                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
 2000 CONTINUE
C
CCC OPEN GF-FLOW FILE TO BE WRITTEN
C
      IACT = 4
      CALL GFALL(IUT0,IUT6,IUTFF,FILEFF,
     *           MCOM,NCOMFL,COMFLE,
     *           MCOM,NCOMST,COMSET,
     *           IACT,IWRITE,INAME,IRESV,
     *           ICAST,IDATA0,IALL,ISKIP,IERR,
     *           ' !',ICHECK)
C
      IF(IMODE.EQ.4) RETURN
C
 6000 CONTINUE
C
CCC WRITE CUBE DATA
C
      DO 2100 IC=1,NC
C
          DO 2200 K=0,NG+2
          DO 2300 J=0,NG+2
          DO 2400 I=0,NG+2
              V3D(1,I,J,K)=0.0D0
              V3D(2,I,J,K)=0.0D0
              V3D(3,I,J,K)=0.0D0
              V3D(4,I,J,K)=0.0D0
 2400     CONTINUE
 2300     CONTINUE   
 2200     CONTINUE   
C
          DO 2500 IPB=1,NPBOUN(IC)
              I=LPBOUN(1,IPB,IC)
              J=LPBOUN(2,IPB,IC)
              K=LPBOUN(3,IPB,IC)
              V3D(1,I,J,K)=1.0D0
 2500     CONTINUE
C
          IG=0
          DO 3100 K=0,NG+2
          DO 3200 J=0,NG+2
          DO 3300 I=0,NG+2
              IG=IG+1
              WRK1(IG)=REAL(V3D(1,I,J,K))
              WRK2(IG)=REAL(V3D(2,I,J,K)/VSCALE)
              WRK3(IG)=REAL(V3D(3,I,J,K)/VSCALE)
              WRK4(IG)=REAL(V3D(4,I,J,K)/VSCALE)
 3300     CONTINUE
 3200     CONTINUE
 3100     CONTINUE
C
          IACT = 6
          TBUF=REAL(TIME)
          CALL GFALL(IUT0,IUT6,IUTFF,FILEFF,
     *               MCOM,NCOMFL,COMFLE,
     *               MCOM,NCOMST,COMSET,
     *               IACT,IWRITE,INAME,IRESV,  
     *               ICAST,IDATA0,IALL,ISKIP,IERR,
     *               ' *TIME_PS *STEP_PS *LBM_RHO *LBM_VEL !',
     *               NAME,TBUF,
     *               NAME,NTIME,
     *               NAME,NG3,NG3,WRK1,
     *               NAME,NG3,NG3,WRK2,WRK3,WRK4,
     *               ICHECK)
 2100 CONTINUE
C
      IF(IMODE.EQ.6) RETURN
C
 8000 CONTINUE
C
CCC CLOSE GF-FLOW FILE
C
      IACT=8
      CALL GFALL(IUT0,IUT6,IUTFF,FILEFF,
     *           MCOM,NCOMFL,COMFLE,
     *           MCOM,NCOMST,COMSET,
     *           IACT,IWRITE,INAME,IRESV,
     *           ICAST,IDATA0,IALL,ISKIP,IERR,
     *           ' !',ICHECK)
C
      RETURN
      END
