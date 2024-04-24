C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    PRINTI                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE PRINTI(MTI,MTO)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION  DATA(20)
C
C
C      PRINT OUT INPUT FILE DATA DEFINED BY MTI
C     TO OUTPUT FILE DEFINED BY MTO
C         ( 2-D CALCULATION & GRAPHICS )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          MTI         ; INPUT  FILE NUMBER
C          MTO         ; OUTPUT FILE NUMBER
C
C       (2) OUTPUT
C          NONE
C
C
      REWIND MTI
      LCNT    = 1
      IPCNT   = 1
      JCNT    = 1
    1 READ    (MTI,  10 , END = 999)  DATA
      WRITE   (MTO, 100)   IPCNT
      WRITE   (MTO, 200)
      GO  TO  3
    2 READ    (MTI,  10 , END = 999)  DATA
    3 CONTINUE
      WRITE   (MTO, 300)   LCNT  ,  DATA
      LCNT    = LCNT + 1
      JCNT    = JCNT + 1
      IF      (JCNT . NE . 51)  GO TO 2
      IPCNT   = IPCNT + 1
      JCNT    = 1
      GO  TO  1
  999 CONTINUE
      WRITE   (MTO, 400)
      REWIND MTI
C
C
      RETURN
   10 FORMAT  (20A4)
  100 FORMAT  (1H1,10X,110('*') / 1H ,10X,21('*'),5X,'I N P U T   D A T
     1A   L I S T I N G',5X,26('*'),2X,'PAGE : ',I3,3X,'***' / 1H ,10X,1
     210('*'))
  200 FORMAT  (1H0,10X,'LCNT',6X,'COLUMN   *---10---**---20---**---30---
     1**---40---**---50---**---60---**---70---**---80---*'//)
  300 FORMAT  (1H , 9X,I5,15X,20A4)
  400 FORMAT  (1H1)
      END
