      SUBROUTINE DDCOM4(NDOM,LDOM,MAX,
     *                  NPB1,LPB1,XPB1,YPB1,ZPB1,
     *                  NPB2,LPB2,XPB2,YPB2,ZPB2,
     *                  MAXBUF,BUFSND,BUFRCV,
     *                  IUT0,IERR)
      IMPLICIT REAL*4 (A-H,O-Z)
      DIMENSION LDOM(NDOM),
     *          LPB1(MAX,NDOM),LPB2(MAX,NDOM),
     *          XPB1(MAX,NDOM),XPB2(MAX,NDOM),
     *          YPB1(MAX,NDOM),YPB2(MAX,NDOM),
     *          ZPB1(MAX,NDOM),ZPB2(MAX,NDOM),
     *          BUFSND(MAXBUF),BUFRCV(MAXBUF)
C
      RETURN
      END
