      SUBROUTINE CHKFLW(FILE,IUTFF,IUT0,IUT6,
     *                  MP,ME,IERR)
      IMPLICIT NONE
      INCLUDE 'gf2.h'
C
C[INPUT]
      INTEGER*4 IUTFF
      INTEGER*4 IUT0,IUT6
      CHARACTER*60 FILE
C
C[OUTPUT]
      INTEGER*4 MP,ME,IERR
C
C[LOCAL]
      INTEGER*4 N,NP,NEPRS,NEFRC
      REAL*4    DUM
C
      IACT=1
      N   =8
      CALL GFALL(IUT0,IUT6,IUTFF,FILE,
     *           MCOM,NCOMFL,COMFLE,
     *           MCOM,NCOMST,COMSET,
     *           IACT,IWRITE,INAME,IRESV,  
     *           ICAST,IDATA0,IALL,ISKIP1,IERR,
     *           '*VELO_3D *PRES_3E  *VFRC_3E !',
     *            NAME,MP,NP,DUM,DUM,DUM,
     *            NAME,ME,NEPRS,DUM,
     *            NAME,ME,NEFRC,DUM,
     *           ICHECK)
      IF (IERR.NE.0) RETURN
C
      MP=NP
      IF (NEPRS.NE.0) THEN
         ME=NEPRS
      ELSE IF (NEFRC.NE.0) THEN
         ME=NEFRC
      ELSE
         ME=0
      ENDIF
C
      WRITE(IUT6,*)
      WRITE(IUT6,*) 'CHKFLW: MP',MP
      WRITE(IUT6,*) 'CHKFLW: ME',ME
C
      RETURN 
      END
  
