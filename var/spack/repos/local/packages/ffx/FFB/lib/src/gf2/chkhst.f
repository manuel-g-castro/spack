      SUBROUTINE CHKHST(FILE,IUTHT,IUT0,IUT6,
     *                  MTIME,MHST,IERR)
      IMPLICIT NONE
      INCLUDE 'gf2.h'
C
C[INPUT]
      INTEGER*4 IUTHT
      INTEGER*4 IUT0,IUT6
      CHARACTER*60 FILE
C
C[OUTPUT]
      INTEGER*4 MTIME,MHST,IERR
C
C[LOCAL]
      INTEGER*4 NTIME,NHST
      REAL*4    DUM
C
      IACT=1
      CALL GFALL(IUT0,IUT6,IUTHT,FILE,
     *           MCOM,NCOMFL,COMFLE,
     *           MCOM,NCOMST,COMSET,
     *           IACT,IWRITE,INAME,IRESV,  
     *           ICAST,IDATA0,IALL,ISKIP1,IERR,
     *           '*HISTORY !',
     *            NAME,MTIME,MHST,NTIME,NHST,DUM,
     *           ICHECK)
      IF (IERR.NE.0) RETURN
C
      MTIME=NTIME
      MHST =NHST
C
      WRITE(IUT6,*)
      WRITE(IUT6,*) 'CHKHST: MTIME',MTIME
      WRITE(IUT6,*) 'CHKHST: MHST ',MHST
C
      RETURN 
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE CHKHS2(FILE,IUTHT,IUT0,IUT6,NPART,
     *                  MTIME,MHST,IERR)
      IMPLICIT NONE
      INCLUDE 'gf2.h'
C
C[INPUT]
      INTEGER*4 IUTHT,IUT0,IUT6,NPART
      CHARACTER*60 FILE
C
C[OUTPUT]
      INTEGER*4 MTIME,MHST,IERR,I
C
C[LOCAL]
      INTEGER*4 IPART,NTIME,NHST,NRESV,NDATA
      REAL*4    DUM
      CHARACTER*60 FILED
      INTEGER*4, ALLOCATABLE:: LIST(:)
C
      MHST=0
      DO 1000 IPART = 1 , NPART
          CALL MFNAME(FILE,FILED,IPART,IUT0,IERR)
          IACT=1
          CALL GFALL(IUT0,IUT6,IUTHT,FILED,
     *               MCOM,NCOMFL,COMFLE,
     *               MCOM,NCOMST,COMSET,
     *               IACT,IWRITE,INAME,IRESV,  
     *               ICAST,IDATA0,IALL,ISKIP1,IERR,
     *               '*HISTORY !',
     *                NAME,MTIME,MHST,NTIME,NHST,DUM,
     *               ICHECK)
          IF (IERR.NE.0) RETURN
          IF(NHST.GT.MHST) MHST=NHST
 1000 CONTINUE    
C
      ALLOCATE (LIST(MHST))
C
      NDATA=0
      DO 2000 IPART = 1 , NPART
          CALL MFNAME(FILE,FILED,IPART,IUT0,IERR)
          IACT=1
          CALL GFALL(IUT0,IUT6,IUTHT,FILED,
     *               MCOM,NCOMFL,COMFLE,
     *               MCOM,NCOMST,COMSET,
     *               IACT,IWRITE,INAME,IRESV,  
     *               ICAST,IDATA0,IALL,ISKIP,IERR,
     *               '*HSTLIST  !',
     *                NAME,MHST,NHST,LIST,
     *               ICHECK)
C
          IF(IPART.EQ.1) THEN
              DO 2010 I=1,NHST
                  NRESV=I
                  IF(LIST(I).GT.0) THEN
                      NRESV=I-1
                      GOTO 2020
                  ENDIF
 2010         CONTINUE
 2020         CONTINUE
          ENDIF  
C
          DO 2030 I=1,NHST
              IF(LIST(I).GT.NDATA) NDATA=LIST(I)
 2030     CONTINUE
C
 2000 CONTINUE    
C
      MTIME=NTIME
      MHST =NRESV+NDATA
C
      WRITE(IUT6,*)
      WRITE(IUT6,*) 'CHKHS2: MTIME',MTIME
      WRITE(IUT6,*) 'CHKHS2: MHST ',MHST
C
      DEALLOCATE (LIST)
C
C
      RETURN 
      END
  
  
