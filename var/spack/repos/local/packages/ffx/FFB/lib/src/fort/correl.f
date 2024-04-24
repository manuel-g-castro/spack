C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    CORREL                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE CORREL(IMODE,XIN,YIN,COR,NTIME)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION XIN(NTIME),YIN(NTIME),COR(0:NTIME/2)
C
C
C      TAKE CORRELATION OF GIVEN DATA IN TIME SPACE
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          IMODE       ; SPECIFIES TYPE OF CORRELATION AS FOLLOWS
C                   1 --- AUTO   CORRELATION
C                   2 --- CROSS  CORRELATION
C          XIN  (ITIME); INPUT DATA 1
C          YIN  (ITIME); INPUT DATA 2 ( DUMMY ARGUMENT FOR IMODE = 1 )
C          NTIME       ; NUMBER OF TOTAL TIME STEPS
C
C       (2) OUTPUT
C          COR(0:ITIME); CORRELATION
C           NOTES; FOR IMODE = 1, CALCULATED CORRELATIONS WILL BE PASSED
C                 IN THE ARGUMENT COR(ITIME), ITIME=0,NTIME/2, WITH
C                 ITIME=0 DENOTING NO TIME LAG AND ITIME=NTIME/2 MAXIMUM
C                 POSSITIVE TIME LAG OF HAHF THE TIME SPAN IN THE INPUT
C                 HISTORY DATA.
C                  FOR IMODE = 2, CALCULATED CORRELATIONS WILL BE PASSED
C                 IN THE ARGUMENT COR(ITIME), ITIME=0,NTIME/2, WITH
C                 ITIME=0 DENOTING MAXIMUM NEGATIVE TIME LAG OF 
C                 ONE-FORTH THE TIME SPAN IN THE INPUT HISTORY DATA.
C                 AND ITIME=NTIME/2 MAXIMUM POSSITIVE TIME LAG OF
C                 OF ONE-FORTH THE TIME SPAN IN THE INPUT HISTORY DATA.
C
C
C TAKE AUTO   CORRELATIONS
C
      IF(IMODE.EQ.1) THEN
          DO 110 ITAU = 0 , NTIME/2
              COR(ITAU) = 0.E0
              DO 100 ITIME = 1 , NTIME/2
                  COR(ITAU) = COR(ITAU)+XIN(ITIME)*XIN(ITIME+ITAU)
  100         CONTINUE
              COR(ITAU) = COR(ITAU)/FLOAT(NTIME/2) 
  110     CONTINUE
      ELSE
C
C TAKE CROSS CORRELATIONS
C
          DO 210 ITAU = 0 , NTIME/2
              COR(ITAU) = 0.E0
              DO 200 ITIME = NTIME/4+1 , (3*NTIME)/4
                  COR(ITAU)=COR(ITAU)+XIN(ITIME)*YIN(ITIME+ITAU-NTIME/4)
  200         CONTINUE
              COR(ITAU) = COR(ITAU)/FLOAT(NTIME/2)
  210     CONTINUE
      ENDIF
C
      RETURN
      END
