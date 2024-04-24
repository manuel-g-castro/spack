      SUBROUTINE CHKMSH(FILE,IUTMS,IUT0,IUT6,
     *                  MP,ME,IERR)
      IMPLICIT NONE
      INCLUDE 'gf2.h'
C
C[INPUT]
      INTEGER*4 IUTMS
      INTEGER*4 IUT0,IUT6
      CHARACTER*60 FILE
C
C[OUTPUT]
      INTEGER*4 MP,ME,IERR
C
C[LOCAL]
      INTEGER*4 N,LDUM,NP,NE
      REAL*4    DUM
C
      IACT=1
      N   =8
      CALL GFALL(IUT0,IUT6,IUTMS,FILE,
     *           MCOM,NCOMFL,COMFLE,
     *           MCOM,NCOMST,COMSET,
     *           IACT,IWRITE,INAME,IRESV,  
     *           ICAST,IDATA0,IALL,ISKIP1,IERR,
     *           '*GRID_3D *NODE_3D  !',
     *            NAME,MP,NP,DUM,DUM,DUM,
     *            NAME,ME,N, NE, LDUM,LDUM,
     *           ICHECK)
      IF (IERR.NE.0) RETURN
C
      MP=NP
      ME=NE
C
      WRITE(IUT6,*)
      WRITE(IUT6,*) 'CHKMSH: MP',MP
      WRITE(IUT6,*) 'CHKMSH: ME',ME
C
      RETURN 
      END
  
