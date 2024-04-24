      SUBROUTINE GFCUBE(IMODE,MC,MBC,FILECB,
     *                  NG,NC,LLEVEL,LPOSI,NBC,LBC,
     *                  MCOM,NCOMFL,NCOMST,COMFLE,COMSET,
     *                  IUT6,IUT0,IUTCB,IERR)
      IMPLICIT NONE
      INTEGER*4    IMODE,MC,MBC,NG,NC
      INTEGER*4    LLEVEL(MC),LPOSI(3,MC),
     *             NBC(MC),LBC(5,MBC,MC)
      INTEGER*4    IUT6,IUT0,IUTCB,IERR
C
      INTEGER*4    MCOM,NCOMFL,NCOMST
      CHARACTER*60 FILECB,COMFLE(MCOM),COMSET(MCOM)
C
      INTEGER*4 IACT,ITARGT,IRESV,IWRITE,INAME,
     *          ICAST,ICAST1,IDATA0,IALL,ISKIP,ISKIP1,ICHECK
      INTEGER*4 LCOWRK(26),LBCWRK(5,MBC)
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
      INTEGER*4 N03,N05,N26
      DATA N03   / 3/
      DATA N05   / 5/
      DATA N26   /26/
C
      INTEGER*4 IC,IB,NDUM
C
C[INPUT]
C     IMODE : SPECIFY ACTION MODE (0:CHECK SIZE, 1:READ, 2:WRITE)
C     MC    : MAX NUMBER OF CUBES IN SUB-DOMAIN
C     MBC   : MAX. NUMBER OF B.C GROUPS IN CUBES (=152)
C     IUT6  : UNIT NUMBER OF STANDARD OUTPUT
C     IUT0  : UNIT NUMBER OF ERROR OUTPUT
C     IUTCB : UNIT NUMBER OF GF-CUBE FILE 
C
C[INPUT&OUTPUT] 
C     NC             : NUMBER OF CUBES IN SUB-DOMAIN
C     NG             : CUBE SIZE (=2^N)
C     LLEVEL(IC)     : LEVEL OF CUBES, WHICH INDICATE THE GRID RESOLUTION. 
C     LPOSI(3,IC)    : INDICATES THE POSITIONS OF CUBES, WHICH ARE NORMALIZED 
C                      BY THE MINIMUM CUBE SIZE.
C     NBC(IC)        : NUMBER OF B.C. GROUPS IN CUBES
C     LBC(II,IBC,IC) : ATTRIBUTE DATA OF B.C. GROUPS
C                      II=1 B.C. GROUP ID (1-26) IN AN ADJACENT CUBE
C                      II=2 SUB-DOMAIN NUMBER OF AN ADJACENT CUBE
C                      II=3 CUBE NUMBER OF AN ADJACENT CUBE IN A DOMAIN
C                      II=4 RELATIVE LEVEL OF AN ADJACENT CUBE
C                       (-1: FINE, 0:SAME, 1:COARSE)
C                      II=5 POSITION IN COARSER CUBE
C
C[OUTPUT]
C     IERR   : ERROR FLAG
C
      IERR=0
C
      IF(IMODE.EQ.0) THEN
          WRITE(IUT6,*) 
          WRITE(IUT6,*) '  GFCUBE :: CHECKING SIZE OF GF-CUBE FILE   '
      ELSE IF(IMODE.EQ.1) THEN
          WRITE(IUT6,*) 
          WRITE(IUT6,*) '  GFCUBE :: READING GF-CUBE FILE   '
          GOTO 1000
      ELSE IF(IMODE.EQ.2) THEN
          WRITE(IUT6,*) 
          WRITE(IUT6,*) '  GFCUBE :: WRITING GF-CUBE FILE   '
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
CCC OPEN GF-CUBE FILE TO BE READ     
C
      IACT = 3
      CALL GFALL(IUT0,IUT6,IUTCB,FILECB,
     *           MCOM,NCOMFL,COMFLE,
     *           MCOM,NCOMST,COMSET,
     *           IACT,IWRITE,INAME,IRESV,  
     *           ICAST,IDATA0,IALL,ISKIP,IERR,
     *           ' !',ICHECK)
      IF(IERR.NE.0) RETURN
C
      NC=0
C
  100 CONTINUE
C
CCC CHECK SIZE CUBE DATA
C
      IACT = 5
      CALL GFALL(IUT0,IUT6,IUTCB,FILECB,
     *           MCOM,NCOMFL,COMFLE,
     *           MCOM,NCOMST,COMSET,
     *           IACT,IWRITE,INAME,IRESV,  
     *           ICAST,IDATA0,IALL,ISKIP,IERR,
     *           '*LBM_CSZ !',
     *           NAME,NG,
     *           ICHECK)
      IF(IACT.NE.7) THEN
          NC=NC+1
          GOTO 100
      ENDIF
C
CCC CLOSE GF-CUBE FILE
C
      CALL GFALL(IUT0,IUT6,IUTCB,FILECB,
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
CCC OPEN GF-CUBE FILE TO BE READ     
C
      IACT = 3
      CALL GFALL(IUT0,IUT6,IUTCB,FILECB,
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
          CALL GFALL(IUT0,IUT6,IUTCB,FILECB,
     *               MCOM,NCOMFL,COMFLE,
     *               MCOM,NCOMST,COMSET,
     *               IACT,IWRITE,INAME,IRESV,  
     *               ICAST,IDATA0,IALL,ISKIP,IERR,
     *               '*LBM_CSZ *LBM_LEV *LBM_POS *LBM_LCB !',
     *               NAME,NG,
     *               NAME,LLEVEL(IC),
     *               NAME,N03,NDUM,LPOSI(1,IC),
     *               NAME,MBC,N05,NBC(IC),N05,LBC(1,1,IC),
     *               ICHECK)
          IF(IERR.NE.0) RETURN  
C
 1100 CONTINUE
C
CCC CLOSE GF-CUBE FILE
C
      IACT = 7
      CALL GFALL(IUT0,IUT6,IUTCB,FILECB,
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
      CALL GFALL(IUT0,IUT6,IUTCB,FILECB,
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
          CALL GFALL(IUT0,IUT6,IUTCB,FILECB,
     *               MCOM,NCOMFL,COMFLE,
     *               MCOM,NCOMST,COMSET,
     *               IACT,IWRITE,INAME,IRESV,  
     *               ICAST,IDATA0,IALL,ISKIP,IERR,
     *               '*LBM_CSZ *LBM_LEV *LBM_POS *LBM_LCB !',
     *               NAME,NG,
     *               NAME,LLEVEL(IC),
     *               NAME,N03,N03,LPOSI(1,IC),
     *               NAME,MBC,N05,NBC(IC),N05,LBC(1,1,IC),
     *               ICHECK)
 2100 CONTINUE
C
CCC CLOSE GF-CUBE FILE
C
      IACT=8
      CALL GFALL(IUT0,IUT6,IUTCB,FILECB,
     *           MCOM,NCOMFL,COMFLE,
     *           MCOM,NCOMST,COMSET,
     *           IACT,IWRITE,INAME,IRESV,  
     *           ICAST,IDATA0,IALL,ISKIP,IERR,
     *           ' !',ICHECK)
C
      RETURN
      END
 
