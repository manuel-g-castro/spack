C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.5.0                                   C
C                                                                      C
C  SUB ROUTINE    DDDMY                                                C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
C
C      DUMMY FORTRAN INTERFACE FOR DOMAIN-DECOMPOSITION PROGRAMMING
C     MODEL PREPARED TO ENABLE FULL-LINKING FOR SYSTEMS WHERE PARALLEL
C     ENVIRONMENT IS NOT SUPPORTED
C
C                        AUTHOR: C. KATO, MERL, HITACHI, LTD.
C                        DATE WRITTEN : MARCH    2ND, 1996
C                        DATE MODIFIED: JANUARY 12TH, 1998
C                             (ENTRY 'DDEXIT' ADDED)
C                        DATE MODIFIED: MARCH    7TH, 2003
C                             (ENTRIES 'DDCOM1' AND 'DDCOM2' ADDED)
C
C
      SUBROUTINE DDINIT(NPART,IPART)
      IMPLICIT REAL*4(A-H,O-Z)
C
      NPART = 0
      IPART = 0
C
      RETURN
      END
C
C
      SUBROUTINE DDEXIT
C
      RETURN
      END
C
C
      SUBROUTINE DDSYNC
C
      RETURN
      END
C
C
      SUBROUTINE DDSTOP(IPART,IUT0)
      IMPLICIT REAL*4(A-H,O-Z)
C
      IPART = IPART
      IUT0  = IUT0 
C
      RETURN
      END
C
      SUBROUTINE DDCOMA(SEND,RECV)
      RETURN
      END
C
      SUBROUTINE DDCOM0(NDOM,LDOM,NBPSND,NBPRCV,IERR)
      RETURN
      END
C
C
      SUBROUTINE DDCOM1(NDOM,LDOM,MPB,NBPSND,NBPRCV,
     *                  BUFSND,BUFRCV,IERR)
      RETURN
      END
C
      SUBROUTINE DDCOM2(NDOM,LDOM,MPB,NBPSND,NBPRCV,
     *                  BUFSND,BUFRCV,IERR)
      RETURN
      END
