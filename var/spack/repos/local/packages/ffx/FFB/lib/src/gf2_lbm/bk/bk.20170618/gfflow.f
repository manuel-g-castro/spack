      SUBROUTINE GFFLOW(IMODE,MC,FILEFF,
     *                  NG,NC,NTIME,TIME,V3D,
     *                  MCOM,NCOMFL,NCOMST,COMFLE,COMSET,
     *                  IUT6,IUT0,IUTFF,IERR,
     *                  NG3,WRK1,WRK2,WRK3,WRK4)
      IMPLICIT NONE
      INTEGER*4    IMODE,MC,NG,NC,NTIME
      REAL*4       TIME,V3D(10,0:NG+2,0:NG+2,0:NG+2,MC)
      INTEGER*4    IUT6,IUT0,IUTFF,IERR
C
      INTEGER*4    MCOM,NCOMFL,NCOMST
      CHARACTER*60 FILEFF,COMFLE(MCOM),COMSET(MCOM)
C
      INTEGER*4    NG3
      REAL*4       WRK1(NG3),WRK2(NG3),WRK3(NG3),WRK4(NG3)
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
      INTEGER*4 N4
      DATA N4   /4/
C
      INTEGER*4 I,J,K,IG,IC,IB,MG,NTMP
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
          WRITE(IUT6,*) '  GFFLOW :: READING GF-FLOW FILE   '
      ELSE IF(IMODE.EQ.2) THEN
          WRITE(IUT6,*) 
          WRITE(IUT6,*) '  GFFLOW :: WRITING GF-FLOW FILE   '
          GOTO 2000
      ELSE
          IERR=1
          RETURN
      ENDIF 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                      C
C    IMODE=1: READ MODE                                C 
C                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
CCC OPEN GF-CUBE FILE TO BE READ     
C
      IACT = 3
      CALL GFALL(IUT0,IUT6,IUTFF,FILEFF,
     *           MCOM,NCOMFL,COMFLE,
     *           MCOM,NCOMST,COMSET,
     *           IACT,IWRITE,INAME,IRESV,  
     *           ICAST,IDATA0,IALL,ISKIP,IERR,
     *           ' !',ICHECK)
      IF(IERR.NE.0) RETURN
C
CCC READ CUBE DATA
C
      DO 1100 IC=1,NC
          IACT = 5
          CALL GFALL(IUT0,IUT6,IUTFF,FILEFF,
     *               MCOM,NCOMFL,COMFLE,
     *               MCOM,NCOMST,COMSET,
     *               IACT,IWRITE,INAME,IRESV,  
     *               ICAST,IDATA0,IALL,ISKIP,IERR,
     *               ' *TIME_PS *STEP_PS *LBM_RHO *LBM_VEL !',
     *               NAME,TIME,
     *               NAME,NTIME,
     *               NAME,NG3,NTMP,WRK1,
     *               NAME,NG3,NTMP,WRK2,WRK3,WRK4,
     *               ICHECK)
C
          IG=0
          DO 1200 K=0,NG+2
          DO 1300 J=0,NG+2
          DO 1400 I=0,NG+2
              IG=IG+1
              V3D(1,I,J,K,IC)=WRK1(IG)
              V3D(2,I,J,K,IC)=WRK2(IG)
              V3D(3,I,J,K,IC)=WRK3(IG)
              V3D(4,I,J,K,IC)=WRK4(IG)
 1400     CONTINUE   
 1300     CONTINUE   
 1200     CONTINUE   
C
 1100 CONTINUE
C
CCC CLOSE GF-CUBE FILE
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
 2000 CONTINUE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                      C
C    IMODE=2: WRITE MODE                               C 
C                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
CCC OPEN GF-CUBE FILE TO BE READ     
C
      IACT = 4
      CALL GFALL(IUT0,IUT6,IUTFF,FILEFF,
     *           MCOM,NCOMFL,COMFLE,
     *           MCOM,NCOMST,COMSET,
     *           IACT,IWRITE,INAME,IRESV,  
     *           ICAST,IDATA0,IALL,ISKIP,IERR,
     *           ' !',ICHECK)
C
CCC READ CUBE DATA
C
      DO 2100 IC=1,NC
          IACT = 6
C
          IG=0
          DO 2200 K=0,NG+2
          DO 2300 J=0,NG+2
          DO 2400 I=0,NG+2
              IG=IG+1
              WRK1(IG)=V3D(1,I,J,K,IC)
              WRK2(IG)=V3D(2,I,J,K,IC)
              WRK3(IG)=V3D(3,I,J,K,IC)
              WRK4(IG)=V3D(4,I,J,K,IC)
 2400     CONTINUE   
 2300     CONTINUE   
 2200     CONTINUE   
C
          CALL GFALL(IUT0,IUT6,IUTFF,FILEFF,
     *               MCOM,NCOMFL,COMFLE,
     *               MCOM,NCOMST,COMSET,
     *               IACT,IWRITE,INAME,IRESV,  
     *               ICAST,IDATA0,IALL,ISKIP,IERR,
     *               ' *TIME_PS *STEP_PS *LBM_RHO *LBM_VEL !',
     *               NAME,TIME,
     *               NAME,NTIME,
     *               NAME,NG3,NG3,WRK1,
     *               NAME,NG3,NG3,WRK2,WRK3,WRK4,
     *               ICHECK)
 2100 CONTINUE
C
CCC CLOSE GF-CUBE FILE
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
 
