C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    RELEAS                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE RELEAS(XMR,YMR,IEMR,NMR,XM,YM,IEM,NM,MRK,IUT0,IERR)
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION XMR(NMR),YMR(NMR),IEMR(NMR),XM(NM),YM(NM),IEM(NM)
C
      CHARACTER*72 ERMSG
     & /' *** SUBROUTINE RELEAS REPORTS A FATAL ERROR OCCURENCE ***' /
      CHARACTER*72 EREXP1
     & /' NUMBER OF TOTAL MARKER PARTICLES HAS EXCEEDED LIMIT' /
C
C
C      RELEASE MARKER PARTICLES TO THE FLOW FIELD
C         ( 2-D CALCULATION )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          XMR (IMR)   ; LOCATION    WHERE PARTICLES RELEASED
C          YMR (IMR)   ; LOCATION    WHERE PARTICLES RELEASED
C          IEMR(IMR)   ; ELEMENT NO. WHERE PARTICLES RELEASED
C          NMR         ; NUMBER OF PARTICLES RELEASED SIMULTANEOUSLY
C          MRK         ; LIMIT OF EXISTING MARKER PARTICLES' NUMBER
C          IUT0        ; DEVICE NUMBER TO REPORT ERROR OCCURENCE
C
C       (3) INPUT-OUTPUT
C          XM (IM)     ; LOCATION    OF PARTICLES EXISTING IN THE FIELD
C          YM (IM)     ; LOCATION    OF PARTICLES EXISTING IN THE FIELD
C          IEM(IM)     ; ELEMENT NO. OF PARTICLES EXISTING IN THE FIELD
C          NM          ; NUMBER      OF PARTICLES EXISTING IN THE FIELD
C          IERR        ; RETURN CODE TO REPORT ERROR OCCURENCE
C                   0 --- NORMAL TERMINATION
C                   1 --- A FATAL ERROR HAS OCCURED
C
C
      IERR = 0
      IF(NM+NMR.GT.MRK) THEN
          WRITE(IUT0,6300) ERMSG
          WRITE(IUT0,6300) EREXP1 
          IERR = 1 
          RETURN
      ENDIF
C
      DO 100 IMR = 1 , NMR
          NM = NM+1
           XM(NM) =  XMR(IMR)
           YM(NM) =  YMR(IMR)
          IEM(NM) = IEMR(IMR)
  100 CONTINUE
C
C
      RETURN
 6300 FORMAT(A72)
      END
