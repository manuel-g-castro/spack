      SUBROUTINE GFBOUN(IMODE,MC,MPBOUN,FILEBC,
     *                  NG,NC,NPBOUN,LPBOUN,QBOUN,
     *                  MCOM,NCOMFL,NCOMST,COMFLE,COMSET,
     *                  IUT6,IUT0,IUTBC,IERR)
      IMPLICIT NONE
      INTEGER*4    IMODE,MC,MPBOUN,NG,NC,
     *             NPBOUN(MC),LPBOUN(5,MPBOUN,MC)
      REAL*4       QBOUN(MPBOUN,MC) 
      INTEGER*4    IUT6,IUT0,IUTBC,IERR
C
      INTEGER*4    MCOM,NCOMFL,NCOMST
      CHARACTER*60 FILEBC,COMFLE(MCOM),COMSET(MCOM)
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
      DATA ISKIP  /0/
      DATA ISKIP1 /1/
      DATA ICHECK /999999/
C
      INTEGER*4 N5
      DATA N5   /5/
C
      INTEGER*4 IBUF1,IBUF2,IBUF3,IBUF4,IBUF5,IBUF6,IBUF7,IC,IB
C
C[INPUT]
C     IMODE : SPECIFY ACTION MODE (0:CHECK SIZE, 1:READ, 2:WRITE)
C     MC    : MAX. NUMBER OF B.C GROUPS IN CUBES (=152)
C     IUT6  : UNIT NUMBER OF STANDARD OUTPUT
C     IUT0  : UNIT NUMBER OF ERROR OUTPUT
C     IUTBC : UNIT NUMBER OF GF-B.C. FILE 
C
C[INPUT&OUTPUT] 
C
C[OUTPUT]
C     IERR   : ERROR FLAG
C
      IERR=0
      IALL=-1
C
      IF(IMODE.EQ.0) THEN
          WRITE(IUT6,*) 
          WRITE(IUT6,*) '  GFBOUN :: CHECKING SIZE OF GF-BOUN FILE   '
      ELSE IF(IMODE.EQ.1) THEN
          WRITE(IUT6,*) 
          WRITE(IUT6,*) '  GFBOUN :: READING GF-BOUN FILE   '
          GOTO 1000
      ELSE IF(IMODE.EQ.2) THEN
          WRITE(IUT6,*) 
          WRITE(IUT6,*) '  GFBOUN :: WRITING GF-BOUN FILE   '
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
CCC OPEN GF-BOUN FILE TO BE READ     
C
      IACT = 3
      CALL GFALL(IUT0,IUT6,IUTBC,FILEBC,
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
      MPBOUN=0
      DO 200 IC=1,NC
          IACT = 5
          CALL GFALL(IUT0,IUT6,IUTBC,FILEBC,
     *               MCOM,NCOMFL,COMFLE,
     *               MCOM,NCOMST,COMSET,
     *               IACT,IWRITE,INAME,IRESV,  
     *               ICAST,IDATA0,IALL,ISKIP1,IERR,
     *               '*LBM_BND  !',
     *               NAME,MPBOUN,N5,IBUF3,IBUF4,LPBOUN(1,1,IC),
     *               ICHECK)
          IF(IBUF3.GT.MPBOUN) MPBOUN=IBUF3
  200 CONTINUE
C
CCC CLOSE GF-BOUN FILE
C
      CALL GFALL(IUT0,IUT6,IUTBC,FILEBC,
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
CCC OPEN GF-BOUN FILE TO BE READ     
C
      IACT = 3
      CALL GFALL(IUT0,IUT6,IUTBC,FILEBC,
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
          CALL GFALL(IUT0,IUT6,IUTBC,FILEBC,
     *               MCOM,NCOMFL,COMFLE,
     *               MCOM,NCOMST,COMSET,
     *               IACT,IWRITE,INAME,IRESV,  
     *               ICAST,IDATA0,IALL,ISKIP,IERR,
     *               '*LBM_CSZ *LBM_BND *LBM_LPQ !',
     *               NAME,NG,
     *               NAME,MPBOUN,N5,NPBOUN(IC),IBUF5,LPBOUN(1,1,IC),
     *               NAME,MPBOUN,NPBOUN(IC),QBOUN(1,IC), 
     *               ICHECK)
 1100 CONTINUE
C
CCC CLOSE GF-BOUN FILE
C
      IACT = 7
      CALL GFALL(IUT0,IUT6,IUTBC,FILEBC,
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
CCC OPEN GF-BOUN FILE TO BE READ     
C
      IACT = 4
      CALL GFALL(IUT0,IUT6,IUTBC,FILEBC,
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
          CALL GFALL(IUT0,IUT6,IUTBC,FILEBC,
     *               MCOM,NCOMFL,COMFLE,
     *               MCOM,NCOMST,COMSET,
     *               IACT,IWRITE,INAME,IRESV,  
     *               ICAST,IDATA0,IALL,ISKIP,IERR,
     *               '*LBM_CSZ *LBM_BND *LBM_LPQ !',
     *               NAME,NG,
     *               NAME,MPBOUN,N5,NPBOUN(IC),N5,LPBOUN(1,1,IC),
     *               NAME,MPBOUN,NPBOUN(IC),QBOUN(1,IC), 
     *               ICHECK)
 2100 CONTINUE
C
CCC CLOSE GF-BOUN FILE
C
      IACT=8
      CALL GFALL(IUT0,IUT6,IUTBC,FILEBC,
     *           MCOM,NCOMFL,COMFLE,
     *           MCOM,NCOMST,COMSET,
     *           IACT,IWRITE,INAME,IRESV,  
     *           ICAST,IDATA0,IALL,ISKIP,IERR,
     *           ' !',ICHECK)
C
      RETURN
      END
 
