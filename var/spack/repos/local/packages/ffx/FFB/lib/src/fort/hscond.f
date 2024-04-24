C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    HSCOND                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE HSCOND(IESMPL,LSMPL,XSMPL,YSMPL,ZSMPL,
     *                  GSMPL,ESMPL,TSMPL,NSMPL,COMHST,NHST,MRESV,IUTW)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION IESMPL(NSMPL),LSMPL(NSMPL),
     1           XSMPL(NSMPL),YSMPL(NSMPL),ZSMPL(NSMPL),
     2           GSMPL(NSMPL),ESMPL(NSMPL),TSMPL(NSMPL)
      CHARACTER*60 COMHST(NHST)
C
      CHARACTER*60 WRMSGA
     & /' ## SUBROUTINE HSCOND: WARNING          ISSUING  ; CONTINUE' /
      CHARACTER*60 WREXP1
     & /' SPECIFIED POINT IS OUT OF THE COMPUTAION DOMAIN           ' /
C
C
C
C      CONDENSE HISTORY SAVING ARRAYS FOR SAMPLING POINTS OUT OF THE
C     COMPUTAIONAL DOMAIN
C                        ( 3-D CALCULATION )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          MRESV       ; DEFAULT NUMBER OF HISTORY SAVING    DATA
C          IUTW        ; DEVICE NUMBER TO ISSUE A WARNING
C           NOTES; SET 'IUTW' NEGATIVE TO DISABLE WARNING MESSAGES
C
C       (3) INPUT-OUTPUT
C          IESMPL(ISMPL); ELEMENT NUMBER  OF  SAMPLING POINT
C          LSMPL (ISMPL); TYPE OF DATA TO BE SAMPLED
C          XSMPL(ISMPL); X     COORDINATE OF  SAMPLING POINT
C          YSMPL(ISMPL); Y     COORDINATE OF  SAMPLING POINT
C          ZSMPL(ISMPL); Z     COORDINATE OF  SAMPLING POINT
C          GSMPL(ISMPL); GZAI  COORDINATE OF  SAMPLING POINT
C          ESMPL(ISMPL); EATA  COORDINATE OF  SAMPLING POINT
C          TSMPL(ISMPL); THETA COORDINATE OF  SAMPLING POINT
C          NSMPL       ; NUMBER OF DATA SAMPLING POINTS
C          COMHST(IHST); SPECIFIC SET COMMENT STRINGS TO BE WRITTEN
C                        TO HISTORY FILE, WHICH IDENTIFIES EACH HISTORY
C                        DATA
C          NHST        ; TOTAL NUMBER OF TIME HISTORY DATA TO BE SAVED
C           NOTES; SAMPLING FOR THOSE POINTS OUT OF THE COMPUTATIONAL
C                 DOMAIN (IE. POINTS WITH IESMPL SET TO ZERO) WILL BE
C                 DISABLED AND THE SAMPLING NUMBER WILL BE CONDENSED. 
C
C

      IF(NSMPL.LE.0) RETURN
C
      IFRONT = 1
   10 CONTINUE
          IF(IESMPL(IFRONT).EQ.0) THEN
              IF(IUTW.GE.0) THEN
                  WRITE(IUTW,*) WRMSGA
                  WRITE(IUTW,*) WREXP1
                  WRITE(IUTW,*) 
     &        '    (X,Y,Z)=',XSMPL(IFRONT),YSMPL(IFRONT),ZSMPL(IFRONT)
              ENDIF
              DO 20 I = IFRONT , NSMPL-1
                  IESMPL (I)       = IESMPL (I+1)
                   LSMPL (I)       =  LSMPL (I+1)
                   XSMPL (I)       =  XSMPL (I+1)
                   YSMPL (I)       =  YSMPL (I+1)
                   ZSMPL (I)       =  ZSMPL (I+1)
                   GSMPL (I)       =  GSMPL (I+1)
                   ESMPL (I)       =  ESMPL (I+1)
                   TSMPL (I)       =  TSMPL (I+1)
                   COMHST(I+MRESV) =  COMHST(I+1+MRESV)
   20         CONTINUE
              NSMPL = NSMPL-1
              NHST  = NHST -1
          ELSE
              IFRONT = IFRONT+1
          ENDIF
      IF(IFRONT.LE.NSMPL) GO TO 10
C
C
      RETURN
      END
