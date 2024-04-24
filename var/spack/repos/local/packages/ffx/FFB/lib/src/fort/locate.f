C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    LOCATE                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE LOCATE(X,Y,NODE,NE,NP,N,XM,YM,NM,IEM,IUT0,IWRN)
      IMPLICIT REAL*4 (A-H,O-Z)
      DIMENSION X(NP),Y(NP),NODE(N,NE),XM(NM),YM(NM),IEM(NM)
C
      CHARACTER*60 WRMSG
      CHARACTER*60 WREXP1
      DATA WRMSG
     & /' *** SUBROUTINE LOCATE ISSUES WARNING ***'/
      DATA WREXP1
     & /' GIVEN LOCATION BELONGS TO NO ELEMENT    '/
C
C
C      FIND ELEMENT WHERE MARKERS BELONG TO
C         ( 2-D GRAPHICS )
C
C
C     NOTE 1 ; IF ANY OF GIVEN LOCATION BELONGS TO NO ELEMENTS , LOCATE
C             WILL ISSUES WARNING MESSAGE AND THEN RETURN
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          X       (IP); X-DIR. COORDINATE OF NODE
C          Y       (IP); Y-DIR. COORDINATE OF NODE
C          NODE  (I,IE); NODE TABLE BASED ON ELEMENT
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          NP          ; NUMBER OF TOTAL NODES
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT
C          XM      (IM); MARKER LOCATION ( X-DIR. )
C          YM      (IM); MARKER LOCATION ( Y-DIR. )
C          NM          ; NUMBER OF MARKERS
C          IUT0       ; DEVICE NUMBER TO ISSUE  WARNING
C
C       (2) OUTPUT
C          IEM     (IM); ELEMENT NO. WHERE MARKERS BELONG TO
C          IWRN        ; RETURN CODE TO ISSUE  WARNING
C                   0 --- NORMAL TERMINATION
C                   1 --- ELEMENT NOT FOUND
C
C
      IWRN = 0
      DO 20 IM = 1 , NM
         XP = XM(IM)
         YP = YM(IM)
         DO 10 IE = 1 , NE
             CALL INCLUD(IE,X,Y,NODE,NE,NP,N,XP,YP,IRN)
             IF(IRN.EQ.1) THEN
               IEM(IM) = IE
               GO TO 20
             ENDIF
   10    CONTINUE
         WRITE(IUT0,*) WRMSG
         WRITE(IUT0,*) WREXP1
         WRITE(IUT0,   *) ' IM = ' , IM
         IWRN = 1
         RETURN
   20 CONTINUE
C
C
      RETURN
      END
