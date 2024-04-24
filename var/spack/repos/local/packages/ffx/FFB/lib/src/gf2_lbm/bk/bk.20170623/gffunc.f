      SUBROUTINE GFFUNC(IMODE,MC,FILEVF,
     *                  NG,NC,NP,NTIME,TIME,F,
     *                  MCOM,NCOMFL,NCOMST,COMFLE,COMSET,
     *                  IUT6,IUT0,IUTVF,IERR)
      IMPLICIT NONE
      INTEGER*4    IMODE,MC,NG,NC,NP,NTIME
      REAL*8       TIME,F(NP,0:NG+2,0:NG+2,0:NG+2,MC)
      INTEGER*4    IUT6,IUT0,IUTVF,IERR
C
      INTEGER*4    MCOM,NCOMFL,NCOMST
      CHARACTER*60 FILEVF,COMFLE(MCOM),COMSET(MCOM)
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
      INTEGER*4 I,J,K,IG,IC,IB,NG3,NTMP,NDUM1,NDUM2
      REAL*4    TBUF
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
      NG3=(NG+3)*(NG+3)*(NG+3)
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
      CALL GFALL(IUT0,IUT6,IUTVF,FILEVF,
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
          CALL GFALL(IUT0,IUT6,IUTVF,FILEVF,
     *               MCOM,NCOMFL,COMFLE,
     *               MCOM,NCOMST,COMSET,
     *               IACT,IWRITE,INAME,IRESV,  
     *               ICAST,IDATA0,IALL,ISKIP,IERR,
     *               ' *TIME_PS *STEP_PS *F_3D15Q%D !',
     *               NAME,TIME,
     *               NAME,NTIME,
     *               NAME,NG3,NP,NDUM1,NDUM2,F(1,1,1,1,IC),
     *               ICHECK)
 1100 CONTINUE
C
CCC CLOSE GF-CUBE FILE
C
      IACT = 7
      CALL GFALL(IUT0,IUT6,IUTVF,FILEVF,
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
CCC OPEN GF-CUBE FILE TO BE WRITTEN     
C
      IACT = 4
      CALL GFALL(IUT0,IUT6,IUTVF,FILEVF,
     *           MCOM,NCOMFL,COMFLE,
     *           MCOM,NCOMST,COMSET,
     *           IACT,IWRITE,INAME,IRESV,  
     *           ICAST,IDATA0,IALL,ISKIP,IERR,
     *           ' !',ICHECK)
C
CCC WRITE CUBE DATA
C
      DO 2100 IC=1,NC
          IACT = 6
C
          CALL GFALL(IUT0,IUT6,IUTVF,FILEVF,
     *               MCOM,NCOMFL,COMFLE,
     *               MCOM,NCOMST,COMSET,
     *               IACT,IWRITE,INAME,IRESV,  
     *               ICAST,IDATA0,IALL,ISKIP,IERR,
     *               ' *TIME_PS *STEP_PS *F_3D15Q%D !',
     *               NAME,TIME,
     *               NAME,NTIME,
     *               NAME,NG3,NP,NG3,NP,F(1,1,1,1,IC),
     *               ICHECK)
 2100 CONTINUE
C
CCC CLOSE GF-CUBE FILE
C
      IACT=8
      CALL GFALL(IUT0,IUT6,IUTVF,FILEVF,
     *           MCOM,NCOMFL,COMFLE,
     *           MCOM,NCOMST,COMSET,
     *           IACT,IWRITE,INAME,IRESV,  
     *           ICAST,IDATA0,IALL,ISKIP,IERR,
     *           ' !',ICHECK)
C
      RETURN
      END
 
