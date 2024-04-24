      SUBROUTINE GFFLW2(IMODE,FILEFF,
     *                  NG,NC,NP,NTIME,CVEL,TIME,V3D,F,
     *                  MCOM,NCOMFL,NCOMST,COMFLE,COMSET,
     *                  IUT6,IUT0,IUTFF,IERR,
     *                  NG3,WRK1,WRK2,WRK3,WRK4)
      IMPLICIT NONE
      INTEGER*4    IMODE,NG,NC,NP,NTIME
      REAL*8       TIME,CVEL(9,NP),
     *             V3D(4,0:NG+2,0:NG+2,0:NG+2),
     *             F(NP,0:NG+2,0:NG+2,0:NG+2,NC)
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
      DATA IWRITE / 1 /
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
      INTEGER*4 I,J,K,IG,IC,MG
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
          WRITE(IUT6,*) '  GFFLW2 DOES NOT SUPPORT READING'
          IERR=1
          RETURN
      ELSE IF(IMODE.EQ.2) THEN
          WRITE(IUT6,*) 
          WRITE(IUT6,*) '  GFFLW2 :: WRITE GF-FLOW FILE   '
          GOTO 2000
      ELSE IF(IMODE.EQ.4) THEN
          WRITE(IUT6,*) 
          WRITE(IUT6,*) '  GFFLW2 :: OPEN GF-FLOW FILE   '
          GOTO 2000
      ELSE IF(IMODE.EQ.6) THEN
          WRITE(IUT6,*) 
          WRITE(IUT6,*) '  GFFLW2 :: APPEND GF-FLOW FILE   '
          GOTO 6000
      ELSE IF(IMODE.EQ.8) THEN
          WRITE(IUT6,*) 
          WRITE(IUT6,*) '  GFFLW2 :: CLOSE GF-FLOW FILE   '
          GOTO 8000
      ELSE
          IERR=1
          RETURN
      ENDIF 
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                      C
C    IMODE=2: WRITE MODE                               C 
C                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
 2000 CONTINUE
C
CCC OPEN GF-CUBE FILE TO BE WRITTEN     
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
          CALL CALMCR(NG,NP,CVEL,F(1,0,0,0,IC),V3D)
C
          TBUF=REAL(TIME)
          IG=0
          DO 2200 K=0,NG+2
          DO 2300 J=0,NG+2
          DO 2400 I=0,NG+2
              IG=IG+1
              WRK1(IG)=REAL(V3D(1,I,J,K))
              WRK2(IG)=REAL(V3D(2,I,J,K))
              WRK3(IG)=REAL(V3D(3,I,J,K))
              WRK4(IG)=REAL(V3D(4,I,J,K))
 2400     CONTINUE   
 2300     CONTINUE   
 2200     CONTINUE   
C
          IACT = 6
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
 
