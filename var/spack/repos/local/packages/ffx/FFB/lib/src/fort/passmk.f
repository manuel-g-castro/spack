C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    PASSMK                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE PASSMK(JPSOUT,IUTPS,IMODE,IOPT,X,Y,VR1,VR2,
     *                  NODE,NE,NP,N,IENE,NEE,MAXEE,
     *                  XMR,YMR,IEMR,NMR,DT,NTIME,CU,CV,
     *                  XMINTS,XMAXTS,YMINTS,YMAXTS,JVALID,
     *                  XMIN,YMIN,SFC,ICLMRK,ICDMRK,CSZMRK,
     *                  SR,SRMIN,SRMAX,LCL,NCL,NMAX,EPS)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION X(NP),Y(NP),VR1(NP),VR2(NP),
     1          NODE(N,NE),IENE(MAXEE,NE),NEE(NE),
     2          XMR(NMR),YMR(NMR),IEMR(NMR),SR(NP),LCL(NCL)
C
      DIMENSION XP1(2),YP1(2)
C
#ifdef VOS
      INCLUDE (GN)
#else
      INCLUDE 'gn.h'
#endif
C
      DIMENSION XBUF(1),YBUF(1)
C
      MTWRN  = 0
C
C
C      DRAW PASS LINES OF RELEASED MARKERS
C         ( 2-D GRAPHICS )
C
C
C     NOTE 1 ; CALLING GENERIC GRAPHIC AND POSTSCRIPT INTERFACES.
C     NOTE 2 ; WARNING MESSAGE FROM SUBROUTINE INTERP WILL BE (IF ANY)
C             SURPRESSED, BECAUSE GRAPHIC SYSTEM IS CURRENTLY OPEN.
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          JPSOUT      ; POSTSCRIPT FILE WILL BE OUTPUT IF SET TO 1 OR 2
C          IUTPS       ; FILE NUMBER TO ACCESS POSTSCRIPT FILE
C          IMODE       ; SPECIFIES PASS LINE DISPLAY MODE
C                   1 --- BY MARK
C                   2 --- BY LINE
C                   3 --- BY MARK WITH CORRESPONDING COLOR TO SR
C          IOPT        ; MARKER TRACING OPTION
C                   1 --- NORMAL TRACING
C                   2 --- IF A MARKER CANNOT BE FOUND AFTER THE NORMAL
C                        ADJACENT ELEMENT SEARCH, ALL ELEMENT SEARCH
C                        WILL BE DONE BEFORE TRUNCATING THE TRACE
C          VR1     (IP); X-DIR. VELOCITY COMPONENT
C          VR2     (IP); Y-DIR. VELOCITY COMPONENT
C          NODE  (I,IE); NODE TABLE
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          NP          ; NUMBER OF TOTAL NODES
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT
C          IENE(IEE,IE); ADJACENT ELEMENT NUMBER TO ELEMENT IE
C          NEE     (IE); NUMBER OF ADJACENT ELEMENTS TO ELEMENT IE
C          MAXEE       ; THE FIRST DIMENSION OF ARRAY IENE
C          XMR (IMR)   ; LOCATION    WHERE MARKERS RELEASED
C          YMR (IMR)   ; LOCATION    WHERE MARKERS RELEASED
C          IEMR(IMR)   ; ELEMENT NO. WHERE MARKERS RELEASED
C          NMR         ; NUMBER OF MARKERS RELEASED SIMALTANEOUSLY
C          DT          ; TIME INCREMENT
C          NTIME       ; TIME STEP WHEN MARKER TRACING TRUNCATED
C          CU          ; X-DIR. VELOCITY COMPONENT OF OBSERVER
C          CV          ; Y-DIR. VELOCITY COMPONENT OF OBSERVER
C          XMINTS      ; MIN. X-COOR. OF THE TRACING   ZONE   (JVALID=1)
C          XMAXTS      ; MAX. X-COOR. OF THE TRACING   ZONE   (JVALID=1)
C          YMINTS      ; MIN. Y-COOR. OF THE TRACING   ZONE   (JVALID=1)
C          YMAXTS      ; MAX. Y-COOR. OF THE TRACING   ZONE   (JVALID=1)
C          JVALID      ; SPECIFIES THE EFFECTIVITY OF THE TRACING ZONE
C                   0 --- INEFFECTIVE
C                   1 ---   EFFECTIVE
C          XMIN        ; MIN. X OF GRAPHIC RANGE SPECIFIED
C          YMIN        ; MIN. Y OF GRAPHIC RANGE SPECIFIED
C          SFC         ; GRAPHIC SCALING FACTOR
C          ICLMRK      ; COLOR INDEX   USED TO DRAW PASS LINES
C          ICDMRK      ; CHARACTER CODE OF MARK
C          CSZMRK      ; CHARACTER SIZE OF MARK
C          SR      (IP); SCALAR
C          SRMIN       ; MIN. SCALAR VALUE SPECIFIED
C          SRMAX       ; MAX. SCALAR VALUE SPECIFIED
C          LCL    (ICL); COLOR INDECES
C          NCL         ; NUMBER OF COLOR INDECES DEFINED
C          NMAX        ; MAXIMUM ITERATION NUMBER FOR SUBROUTINE INTERP
C          EPS         ; CONVERGENCE CRITERIA     FOR SUBROUTINE INTERP
C
C       (2) OUTPUT
C          NONE
C
C
C MARK
      MKTYPE = ICDMRK
      PMTYPE = ICDMRK
C
      MKSIZE = CSZMRK
      PKSIZE = CSZMRK
C LINE
      LNTYPE = GLSOLD
      PLTYPE = GLSOLD
C
      LNWDTH = GLNORM
      PLWDTH = GLNORM
C
      CALL GNCSET(ICLMRK)
      IF(JPSOUT.EQ.1) CALL PSGRAY(IUTPS,ICLMRK)
      IF(JPSOUT.EQ.2) CALL PSCSET(IUTPS,ICLMRK)
C
      DO 200 IMR = 1 , NMR
          IEM = IEMR(IMR)
          XM  = XMR(IMR)
          YM  = YMR(IMR)
          IF(JVALID.EQ.1 .AND.
     &       (XM.LT.XMINTS .OR. XM.GT.XMAXTS .OR.
     &        YM.LT.YMINTS .OR. YM.GT.YMAXTS))   GO TO 200
C
          CALL INTERP(VR1,X,Y,NODE,NE,NP,N,IEM,XM,YM,UM,
     *                GDUM,EDUM,NMAX,EPS,MTWRN,IWRN,IRN)
          CALL INTERP(VR2,X,Y,NODE,NE,NP,N,IEM,XM,YM,VM,
     *                GDUM,EDUM,NMAX,EPS,MTWRN,IWRN,IRN)
          IF(IMODE.EQ.3) THEN
              CALL INTERP(SR ,X,Y,NODE,NE,NP,N,IEM,XM,YM,SM,
     *                    GDUM,EDUM,NMAX,EPS,MTWRN,IWRN,IRN)
          ENDIF
C
          XP = SFC*(XM-XMIN)
          YP = SFC*(YM-YMIN)
          IF(IMODE.EQ.1) THEN
              XBUF(1) = XP
              YBUF(1) = YP
              CALL GNMARK(XBUF,YBUF,1)
              IF(JPSOUT.GE.1) CALL PSMARK(IUTPS,XBUF,YBUF,1)
          ENDIF
          IF(IMODE.EQ.2) THEN
              XP1(1) = XP
              YP1(1) = YP
          ENDIF
          IF(IMODE.EQ.3) THEN
              RATIO = (SM-SRMIN)/(SRMAX-SRMIN)
              ICL   = RATIO*NCL+1
              IF(ICL.LT.1  ) THEN
                  ICL = 1
              ENDIF
              IF(ICL.GT.NCL) THEN
                  ICL = NCL
              ENDIF
              CALL  GNCSET(LCL(ICL))
              IF(JPSOUT.EQ.1) CALL  PSGRAY(IUTPS,LCL(ICL))
              IF(JPSOUT.EQ.2) CALL  PSCSET(IUTPS,LCL(ICL))
              XBUF(1) = XP
              YBUF(1) = YP
              CALL GNMARK(XBUF,YBUF,1)
              IF(JPSOUT.GE.1) CALL PSMARK(IUTPS,XBUF,YBUF,1)
          ENDIF
C
          DO 100 ITIME = 1 , NTIME
              XM = XM+DT*(UM-CU)
              YM = YM+DT*(VM-CV)
              IF(JVALID.EQ.1 .AND.
     &           (XM.LT.XMINTS .OR. XM.GT.XMAXTS .OR.
     &            YM.LT.YMINTS .OR. YM.GT.YMAXTS))   GO TO 200
C
              CALL INCLUD(IEM,X,Y,NODE,NE,NP,N,XM,YM,IRN)
              IF(IRN.EQ.1) THEN
                  GO TO 50
              ENDIF
              DO 10 IEE = 1 , NEE(IEM)
                  CALL INCLUD(IENE(IEE,IEM),X,Y,NODE,NE,NP,N,XM,YM,IRN)
                  IF(IRN.EQ.1) THEN
                      IEM = IENE(IEE,IEM)
                      GO TO 50
                  ENDIF
   10         CONTINUE
              IF(IOPT.EQ.2) THEN
                  DO 20 IE = 1 , NE
                      CALL INCLUD(IE,X,Y,NODE,NE,NP,N,XM,YM,IRN)
                      IF(IRN.EQ.1) THEN
                          IEM = IE
                          GO TO 50
                      ENDIF
   20             CONTINUE
              ENDIF
              GO TO 200
C
   50         CONTINUE
              CALL INTERP(VR1,X,Y,NODE,NE,NP,N,IEM,XM,YM,UM,
     *                    GDUM,EDUM,NMAX,EPS,MTWRN,IWRN,IRN)
              CALL INTERP(VR2,X,Y,NODE,NE,NP,N,IEM,XM,YM,VM,
     *                    GDUM,EDUM,NMAX,EPS,MTWRN,IWRN,IRN)
              IF(IMODE.EQ.3) THEN
                  CALL INTERP(SR ,X,Y,NODE,NE,NP,N,IEM,XM,YM,SM,
     *                        GDUM,EDUM,NMAX,EPS,MTWRN,IWRN,IRN)
              ENDIF
C
              XP = SFC*(XM-XMIN)
              YP = SFC*(YM-YMIN)
              IF(IMODE.EQ.1) THEN
              XBUF(1) = XP
              YBUF(1) = YP
              CALL GNMARK(XBUF,YBUF,1)
              IF(JPSOUT.GE.1) CALL PSMARK(IUTPS,XBUF,YBUF,1)
              ENDIF
              IF(IMODE.EQ.2) THEN
                 XP1(2) = XP
                 YP1(2) = YP
                 CALL GNLINE(XP1,YP1,2)
                 IF(JPSOUT.GE.1) CALL PSLINE(IUTPS,XP1,YP1,2)
                 XP1(1) = XP
                 YP1(1) = YP
              ENDIF
              IF(IMODE.EQ.3) THEN
                  RATIO = (SM-SRMIN)/(SRMAX-SRMIN)
                  ICL   = RATIO*NCL+1
                  IF(ICL.LT.1  ) THEN
                      ICL = 1
                  ENDIF
                  IF(ICL.GT.NCL) THEN
                      ICL = NCL
                  ENDIF
                  CALL  GNCSET(LCL(ICL))
                  IF(JPSOUT.EQ.1) CALL  PSGRAY(IUTPS,LCL(ICL))
                  IF(JPSOUT.EQ.2) CALL  PSCSET(IUTPS,LCL(ICL))
                  XBUF(1) = XP
                  YBUF(1) = YP
                  CALL GNMARK(XBUF,YBUF,1)
                  IF(JPSOUT.GE.1) CALL PSMARK(IUTPS,XBUF,YBUF,1)
              ENDIF
  100     CONTINUE
  200 CONTINUE
C
C
      RETURN
      END
