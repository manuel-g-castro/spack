      SUBROUTINE GFFUNC(IMODE,IRFNF,MC,FILEVF,
     *                  NG,NC,NP,NTIME,TIME,F,FWRK,
     *                  MCOM,NCOMFL,NCOMST,COMFLE,COMSET,
     *                  IUT6,IUT0,IUTVF,IERR)
      IMPLICIT NONE
      INTEGER*4    IMODE,IRFNF,MC,NG,NC,NP,NTIME
      REAL*8       TIME,F(0:NG+2,0:NG+2,0:NG+2,NP,MC)
      REAL*8       FWRK(NP,0:NG+2,0:NG+2,0:NG+2)
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
      INTEGER*4 IP,I,J,K,IG,IC,IB,NG3,NTMP,NDUM1,NDUM2
      INTEGER*4 NGH,NG3H
      REAL*4    TBUF
C
C[INPUT]
C     IMODE : SPECIFY ACTION MODE (0:CHECK SIZE, 1:READ, 2:WRITE)
C     MC    : MAX. NUMBER OF B.C GROUPS IN CUBES (=152)
C     IUT6  : UNIT NUMBER OF STANDARD OUTPUT
C     IUT0  : UNIT NUMBER OF ERROR OUTPUT
C     IUTFF : UNIT NUMBER OF GF-FUNC FILE 
C
C[INPUT&OUTPUT] 
C
C[OUTPUT]
C     IERR   : ERROR FLAG
C
      IERR=0
      NG3=(NG+3)*(NG+3)*(NG+3)
      NGH=NG/2 
      NG3H=(NGH+3)*(NGH+3)*(NGH+3)
C
      IF(IMODE.EQ.1) THEN
          WRITE(IUT6,*) 
          WRITE(IUT6,*) '  GFFUNC :: READING GF-FUNC FILE   '
      ELSE IF(IMODE.EQ.2) THEN
          WRITE(IUT6,*) 
          WRITE(IUT6,*) '  GFFUNC :: WRITING GF-FUNC FILE   '
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
     *               NAME,TBUF,
     *               NAME,NTIME,
     *               NAME,NG3,NP,NDUM1,NDUM2,FWRK,
     *               ICHECK)
          TIME=DBLE(TBUF)
          IF(NDUM1.EQ.NG3) THEN 
              DO 1110 IP=1,NP
              DO 1111 I =0,NG+2
              DO 1112 J =0,NG+2
              DO 1113 K =0,NG+2
                  F(I,J,K,IP,IC)=FWRK(IP,I,J,K)
 1113         CONTINUE
 1112         CONTINUE
 1111         CONTINUE
 1110         CONTINUE
C
          ELSE IF(IRFNF.EQ.1 .AND. NDUM1.EQ.NG3H) THEN
          WRITE(IUT6,*) 'LBMINI:DISTRIBUTION FUNCTION WILL BE REFINED'
          CALL RFFUNC(NG,NGH,NP,F(0,0,0,1,IC),FWRK)
      ELSE 
          WRITE(IUT6,*) 'LBMINI:INCONSISTENT DITRIBUTION FUNCTION'
          IERR=1
          RETURN
      ENDIF
C
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
          TBUF=REAL(TIME)
C
          DO 2110 IP=1,NP
          DO 2111 I =0,NG+2
          DO 2112 J =0,NG+2
          DO 2113 K =0,NG+2
              FWRK(IP,I,J,K)=F(I,J,K,IP,IC)
 2113     CONTINUE
 2112     CONTINUE
 2111     CONTINUE
 2110     CONTINUE
C
          CALL GFALL(IUT0,IUT6,IUTVF,FILEVF,
     *               MCOM,NCOMFL,COMFLE,
     *               MCOM,NCOMST,COMSET,
     *               IACT,IWRITE,INAME,IRESV,  
     *               ICAST,IDATA0,IALL,ISKIP,IERR,
     *               ' *TIME_PS *STEP_PS *F_3D15Q%D !',
     *               NAME,TBUF,
     *               NAME,NTIME,
     *               NAME,NG3,NP,NG3,NP,FWRK,
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
 
