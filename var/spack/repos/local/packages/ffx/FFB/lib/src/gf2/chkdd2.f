      SUBROUTINE CHKDD2(MPART,MPPT,MEPT,MBPT,
     *                  FILE,IUTDDD,IUT0,IUT6,
     *                  MDOM,MBPDOM,
     *                  LPWRK, LEWRK,
     *                  LPINT1,LPINT2,LPINT3,
     *                  LDOM,NBPDOM,IERR)
      IMPLICIT NONE
      INCLUDE 'gf2.h'
C
C[INPUT]
      INTEGER*4 MPART,MPPT,MEPT,MBPT
      INTEGER*4 IUTDDD
      INTEGER*4 IUT0,IUT6
      CHARACTER*60 FILE
C
C[OUTPUT]
      INTEGER*4 MDOM,MBPDOM,IERR
C
C[WORK]
      INTEGER*4 LPWRK(MPPT),LEWRK(MEPT),
     *          LPINT1(MBPT),LPINT2(MBPT),LPINT3(MBPT),
     *          LDOM(MPART),NBPDOM(MPART)
C
C[LOCAL]
      INTEGER*4 NPPT,NEPT,NBPT,NDOM,NBPMAX
C
      MDOM  =0
      MBPDOM=0
C
CC[OPEN]
      IACT = 3
      CALL GFALL(IUT0,IUT6,IUTDDD,FILE,
     *           MCOM,NCOMFL,COMFLE,
     *           MCOM,NCOMST,COMSET,
     *           IACT,IWRITE,INAME,IRESV,  
     *           ICAST,IDATA0,IALL,ISKIP,IERR,
     *           '*PT_NODE *PT_ELEM *BC_INTR !',
     *           NAME,MPPT,NPPT,LPWRK,
     *           NAME,MEPT,NEPT,LEWRK,
     *           NAME,MBPT,NBPT,LPINT1,LPINT2,LPINT3,
     *           ICHECK)
      IF (IERR.NE.0) RETURN
C
 1000 CONTINUE
CC[CHECK DATA SIZE]
      IACT = 5
      CALL GFALL(IUT0,IUT6,IUTDDD,FILE,
     *           MCOM,NCOMFL,COMFLE,
     *           MCOM,NCOMST,COMSET,
     *           IACT,IWRITE,INAME,IRESV,  
     *           ICAST,IDATA0,IALL,ISKIP,IERR,
     *           '*PT_NODE *PT_ELEM *BC_INTR !',
     *           NAME,MPPT,NPPT,LPWRK,
     *           NAME,MEPT,NEPT,LEWRK,
     *           NAME,MBPT,NBPT,LPINT1,LPINT2,LPINT3,
     *           ICHECK)
      IF (IERR.NE.0) RETURN
C
      IF(IACT.EQ.7) GOTO 2000 
C
      CALL CHKCOM(MPART,NBPT,LPINT2,NDOM,NBPMAX,
     *            LDOM,NBPDOM)
      IF(NDOM  .GT.MDOM  ) MDOM  =NDOM
      IF(NBPMAX.GT.MBPDOM) MBPDOM=NBPMAX
C
      GOTO 1000
C
 2000 CONTINUE 
CC[CLOSE]
      CALL GFALL(IUT0,IUT6,IUTDDD,FILE,
     *           MCOM,NCOMFL,COMFLE,
     *           MCOM,NCOMST,COMSET,
     *           IACT,IWRITE,INAME,IRESV,  
     *           ICAST,IDATA0,IALL,ISKIP,IERR,
     *           '*PT_NODE *PT_ELEM *BC_INTR !',
     *           NAME,MPPT,NPPT,LPWRK,
     *           NAME,MEPT,NEPT,LEWRK,
     *           NAME,MBPT,NBPT,LPINT1,LPINT2,LPINT3,
     *           ICHECK)
      IF (IERR.NE.0) RETURN
C
      WRITE(IUT6,*)
      WRITE(IUT6,*) 'CHKDD2: MDOM  ',MDOM
      WRITE(IUT6,*) 'CHKDD2: MBPDOM',MBPDOM

C
      RETURN 
      END
C
C   
      SUBROUTINE CHKCOM(MPART,NPINT,LPINT2,NDOM,NBPMAX,
     *                  LDOM,NBPDOM)
      IMPLICIT NONE
C
C[INPUT]
      INTEGER*4 MPART,NPINT,LPINT2(NPINT)
C
C[OUTPUT]
      INTEGER*4 NDOM,NBPMAX
C
C[WORK]
      INTEGER*4 LDOM(MPART),NBPDOM(MPART)
C
C[LOACAL]
      INTEGER*4 IDOM,ICHK,IPINT,IFNEW,I
C
      DO 1000 IDOM=1,MPART
          NBPDOM(IDOM)=0
 1000 CONTINUE
C
      NDOM  =0
      NBPMAX=0
C
      DO 2000 IPINT=1,NPINT
          IFNEW=LPINT2(IPINT)
          DO 2100 ICHK=1,NDOM
              IF(LDOM(ICHK).EQ.IFNEW) THEN
                  IDOM=IFNEW
                  GOTO 2200
              ENDIF
 2100     CONTINUE
C
          NDOM=NDOM+1
          IDOM=IFNEW
          LDOM(NDOM)=IFNEW
C         
 2200     CONTINUE
          NBPDOM(IDOM)=NBPDOM(IDOM)+1
          IF(NBPDOM(IDOM).GT.NBPMAX) NBPMAX=NBPDOM(IDOM)
 2000 CONTINUE
C
      RETURN
      END
  
