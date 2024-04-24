      SUBROUTINE GFGEOM(IMODE,MC,MPSOLD,MPSURF,FILEGM,
     *                  NG,NC,NPSOLD,NPSURF,
     *                  LPSOLD,LPSURF,QSURF,
     *                  MCOM,NCOMFL,NCOMST,COMFLE,COMSET,
     *                  IUT6,IUT0,IUTGM,IERR)
      IMPLICIT NONE
      INTEGER*4    IMODE,MC,MPSOLD,MPSURF,NG,NC
      INTEGER*4    NPSOLD,LPSOLD(MPSOLD,MC),NPSURF,LPSURF(4,MPSURF,MC)
      REAL*4       QSURF(MPSURF,MC) 
      INTEGER*4    IUT6,IUT0,IUTGM,IERR
C
      INTEGER*4    MCOM,NCOMFL,NCOMST
      CHARACTER*60 FILEGM,COMFLE(MCOM),COMSET(MCOM)
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
      INTEGER*4 IBUF1,IBUF2,IBUF3,IBUF4,IBUF5,IBUF6,IBUF7,IC,IB
C
C[INPUT]
C     IMODE : SPECIFY ACTION MODE (0:CHECK SIZE, 1:READ, 2:WRITE)
C     MC    : MAX. NUMBER OF B.C GROUPS IN CUBES (=152)
C     IUT6  : UNIT NUMBER OF STANDARD OUTPUT
C     IUT0  : UNIT NUMBER OF ERROR OUTPUT
C     IUTGM : UNIT NUMBER OF GF-GEOM. FILE 
C
C[INPUT&OUTPUT] 
C
C[OUTPUT]
C     IERR   : ERROR FLAG
C
      IERR=0
C
      IF(IMODE.EQ.0) THEN
          WRITE(IUT6,*) 
          WRITE(IUT6,*) '  GFGEOM :: CHECKING SIZE OF GF-GEOM FILE   '
      ELSE IF(IMODE.EQ.1) THEN
          WRITE(IUT6,*) 
          WRITE(IUT6,*) '  GFGEOM :: READING GF-GEOM FILE   '
          GOTO 1000
      ELSE IF(IMODE.EQ.2) THEN
          WRITE(IUT6,*) 
          WRITE(IUT6,*) '  GFGEOM :: WRITING GF-GEOM FILE   '
          GOTO 2000
      ELSE
          IERR=1
          RETURN
      ENDIF 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                      C
C    IMODE=0: CHECK MODE                               C 
C                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
CCC OPEN GF-GEOM FILE TO BE READ     
C
      IACT = 3
      CALL GFALL(IUT0,IUT6,IUTGM,FILEGM,
     *           MCOM,NCOMFL,COMFLE,
     *           MCOM,NCOMST,COMSET,
     *           IACT,IWRITE,INAME,IRESV,  
     *           ICAST,IDATA0,IALL,ISKIP,IERR,
     *           ' !',ICHECK)
      IF(IERR.NE.0) RETURN
C
  100 CONTINUE
C
CCC CHECK SIZE CUBE DATA
C
      MPSOLD=0
      MPSURF=0
      DO 200 IC=1,NC
          IACT = 5
          CALL GFALL(IUT0,IUT6,IUTGM,FILEGM,
     *               MCOM,NCOMFL,COMFLE,
     *               MCOM,NCOMST,COMSET,
     *               IACT,IWRITE,INAME,IRESV,  
     *               ICAST,IDATA0,IALL,ISKIP1,IERR,
     *               '*LBM_CSZ *LBM_SLD *LBM_LPS *LBM_LPQ !',
     *               NAME,IBUF1,
     *               NAME,MPSOLD,IBUF2,LPSOLD,
     *               NAME,MPSURF,N4,IBUF3,IBUF4,LPSURF(1,1,IC),
     *               NAME,MPSURF,IBUF5,QSURF, 
     *               ICHECK)
          IF(IBUF2.GT.MPSOLD) MPSOLD=IBUF2
          IF(IBUF3.GT.MPSURF) MPSURF=IBUF5
  200 CONTINUE
C
CCC CLOSE GF-GEOM FILE
C
      CALL GFALL(IUT0,IUT6,IUTGM,FILEGM,
     *           MCOM,NCOMFL,COMFLE,
     *           MCOM,NCOMST,COMSET,
     *           IACT,IWRITE,INAME,IRESV,  
     *           ICAST,IDATA0,IALL,ISKIP,IERR,
     *           ' !',ICHECK)
C 
      RETURN
C
 1000 CONTINUE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                      C
C    IMODE=1: READ MODE                                C 
C                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
CCC OPEN GF-GEOM FILE TO BE READ     
C
      IACT = 3
      CALL GFALL(IUT0,IUT6,IUTGM,FILEGM,
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
          CALL GFALL(IUT0,IUT6,IUTGM,FILEGM,
     *               MCOM,NCOMFL,COMFLE,
     *               MCOM,NCOMST,COMSET,
     *               IACT,IWRITE,INAME,IRESV,  
     *               ICAST,IDATA0,IALL,ISKIP,IERR,
     *               '*LBM_CSZ *LBM_SLD *LBM_LPS *LBM_LPQ !',
     *               NAME,IBUF1,
     *               NAME,MPSOLD,NPSOLD(IC),LPSOLD(1,IC),
     *               NAME,MPSURF,N4,NPSURF(IC),IBUF4,LPSURF(1,1,IC),
     *               NAME,MPSURF,NPSURF(IC),QSURF(1,IC), 
     *               ICHECK)
     
 1100 CONTINUE
C
CCC CLOSE GF-GEOM FILE
C
      IACT = 7
      CALL GFALL(IUT0,IUT6,IUTGM,FILEGM,
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
CCC OPEN GF-GEOM FILE TO BE READ     
C
      IACT = 4
      CALL GFALL(IUT0,IUT6,IUTGM,FILEGM,
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
          CALL GFALL(IUT0,IUT6,IUTGM,FILEGM,
     *               MCOM,NCOMFL,COMFLE,
     *               MCOM,NCOMST,COMSET,
     *               IACT,IWRITE,INAME,IRESV,  
     *               ICAST,IDATA0,IALL,ISKIP,IERR,
     *               '*LBM_CSZ *LBM_SLD *LBM_LPS *LBM_LPQ !',
     *               NAME,NG,
     *               NAME,MPSOLD,NPSOLD(IC),LPSOLD(1,IC),
     *               NAME,MPSURF,N4,NPSURF(IC),N4,LPSURF(1,1,IC),
     *               NAME,MPSURF,NPSURF(IC),QSURF(1,IC), 
     *               ICHECK)
 2100 CONTINUE
C
CCC CLOSE GF-GEOM FILE
C
      IACT=8
      CALL GFALL(IUT0,IUT6,IUTGM,FILEGM,
     *           MCOM,NCOMFL,COMFLE,
     *           MCOM,NCOMST,COMSET,
     *           IACT,IWRITE,INAME,IRESV,  
     *           ICAST,IDATA0,IALL,ISKIP,IERR,
     *           ' !',ICHECK)
C
      RETURN
      END
 
