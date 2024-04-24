C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    RELES3                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE RELES3(XMR,YMR,ZMR,IEMR,NMR,XM,YM,ZM,IEM,IMSTAT,NM,MRK,
     *                  IUT0,IERR)
      IMPLICIT REAL*4 (A-H,O-Z)
      DIMENSION XMR(NMR),YMR(NMR),ZMR(NMR),IEMR(NMR),
     1          XM(MRK),YM(MRK),ZM(MRK),IEM(MRK),IMSTAT(MRK)
C
      CHARACTER*60 ERMSGB
     & / ' ## SUBROUTINE RELES3: FATAL      ERROR OCCURENCE; RETURNED'/ 
      CHARACTER*60 EREXP1
     & / ' NUMBER OF TOTAL MARKER PARTICLES HAS EXCEEDED LIMIT OF' /
C
C
C      RELEASE MARKER PARTICLES TO THE FLOW FIELD
C         ( 3-D CALCULATION )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          XMR (IMR)   ; LOCATION    WHERE PARTICLES RELEASED
C          YMR (IMR)   ; LOCATION    WHERE PARTICLES RELEASED
C          ZMR (IMR)   ; LOCATION    WHERE PARTICLES RELEASED
C          IEMR(IMR)   ; ELEMENT NO. WHERE PARTICLES RELEASED
C          NMR         ; NUMBER OF PARTICLES RELEASED SIMULTANEOUSLY
C          MRK         ; LIMIT OF EXISTING MARKER PARTICLES' NUMBER
C          IUT0        ; FILE NUMBER TO REPORT ERROR OCCURENCE
C
C       (2) OUTPUT
C          IERR        ; RETURN CODE TO REPORT ERROR OCCURENCE
C                   0 --- NORMAL TERMINATION
C                   1 --- A FATAL ERROR HAS OCCURED
C
C       (3) INPUT-OUTPUT
C          XM    (IM)  ; LOCATION    OF PARTICLES EXISTING IN THE FIELD
C          YM    (IM)  ; LOCATION    OF PARTICLES EXISTING IN THE FIELD
C          ZM    (IM)  ; LOCATION    OF PARTICLES EXISTING IN THE FIELD
C          IEM   (IM)  ; ELEMENT NO. OF PARTICLES EXISTING IN THE FIELD
C          IMSTAT(IM)  ; CURRENT MARKER STATUS ( 0-DEAD , 1-ALIVE )
C          NM          ; NUMBER      OF PARTICLES EXISTING IN THE FIELD
C
C
      IERR = 0
      IF(NM+NMR.GT.MRK) THEN
          WRITE(IUT0,*) ERMSGB
          WRITE(IUT0,*) EREXP1 , MRK
          IERR = 1 
          RETURN
      ENDIF
C
      DO 100 IMR = 1 , NMR
          NM = NM+1
           XM(NM) =  XMR(IMR)
           YM(NM) =  YMR(IMR)
           ZM(NM) =  ZMR(IMR)
          IEM(NM) = IEMR(IMR)
          IF(IEM(NM).GE.1) THEN
              IMSTAT(NM) = 1
          ELSE
              IMSTAT(NM) = 0
          ENDIF
  100 CONTINUE
C
C
      RETURN
      END
