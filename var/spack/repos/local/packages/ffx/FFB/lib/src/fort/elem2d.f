C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    ELEM2D                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE ELEM2D(IMODE,IGAUSS,X,Y,NODE,NE,N,
     *                  SNI,DNXI,DNYI,
     *                  SN ,DNX ,DNY ,
     *                  E  ,EX  ,EY  ,
     *                  EXX,EYY ,EXY ,DELTA,
     *                  DET,DXG,DYG,DXE,DYE,
     *                  IUT0,IERR)
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*4   SN,DNX,DNY,E,EX,EY,EXX,EYY,EXY
      DIMENSION X(*),Y(*),NODE(N,NE),
     1          SNI  (N,NE),DNXI (N,NE),DNYI (N,NE),
     2          SN   (N,NE),DNX  (N,NE),DNY  (N,NE),
     3          E  (N,N,NE),EX (N,N,NE),EY (N,N,NE),
     4          EXX(N,N,NE),EYY(N,N,NE),EXY(N,N,NE),DELTA(NE),
     5          DET(NE),DXG(NE),DYG(NE),DXE(NE),DYE(NE)
C
      PARAMETER ( MPOINT = 4*4 )
      DIMENSION GP(MPOINT),EP(MPOINT),WP(MPOINT)
C
      CHARACTER*72 ERMSG
     & /' FATAL ERROR REPORTED !! ELEM2D TERMINATES ALL THE PROCESS '/
C
C
C      CALCULATE ELEMENT CHARACTERISTIC DIMENSION AND
C     INTEGRAL ELEMENT VECTOR AND MATRICES AND CALCULATE ELEMENT CENTER
C     X,Y DERIVERTIVES OF SHAPE FUNCTION
C         ( 2-D CALCULATION )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          IMODE       ; CONTROLS MATRICES TO BE CALCULATED AS FOLLOWS
C                   1 --- E,EX,EY,EXX,EYY,EXY ARE CALCULATED
C                   2 --- E,      EXX         ARE CALCULATED
C                    ( IN MODE 2 , EXX IS LAPLACIAN MATRIX )
C          IGAUSS      ; SPECIFIES NUMBER OF GAUSS POINTS IN ONE DIR.
C          X      (IP) ; X-COORDINATE OF NODE ' IP '
C          Y      (IP) ; Y-COORDINATE OF NODE ' IP '
C          NODE (I,IE) ; NODE TABLE
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT
C          IUT0        ; DEVICE NO. TO REPORT ERROR OCCURENCE
C
C       (2) OUTPUT
C          SNI  (I,IE) ; ELEMENT CENTER VALUE OF N
C          DNXI (I,IE) ; ELEMENT CENTER VALUE OF NX
C          DNYI (I,IE) ; ELEMENT CENTER VALUE OF NY
C          SN   (I,IE) ; ELEMENT INTEGRATED VECTOR OF N
C          DNX  (I,IE) ; ELEMENT INTEGRATED VECTOR OF NX
C          DNY  (I,IE) ; ELEMENT INTEGRATED VECTOR OF NY
C
C          E  (J,I,IE) ; ELEMENT INTEGRATED MATRIX OF N *N T
C          EX (J,I,IE) ; ELEMENT INTEGRATED MATRIX OF N *NXT
C          EY (J,I,IE) ; ELEMENT INTEGRATED MATRIX OF N *NYT
C          EXX(J,I,IE) ; ELEMENT INTEGRATED MATRIX OF NX*NXT
C                       OR                            NX*NXT+NY*NYT
C          EYY(J,I,IE) ; ELEMENT INTEGRATED MATRIX OF NY*NYT
C          EXY(J,I,IE) ; ELEMENT INTEGRATED MATRIX OF NX*NYT
C          DELTA  (IE) ; ELEMENT CHARACTERISTIC DIMENSION
C          IERR        ; RETURN CODE TO REPORT ERROR OCCURENCE
C                   0 --- NORMAL TERMINATION
C                   1 --- A FATAL ERROR HAS OCCURED
C
C       (3) WORK
C          DET    (IE) ; DETERMINANT OF JACOBIAN MATRIX
C          DXG    (IE) ; G-DIR. DERIVERTIVE OF X
C          DYG    (IE) ; G-DIR. DERIVERTIVE OF Y
C          DXE    (IE) ; E-DIR. DERIVERTIVE OF X
C          DYE    (IE) ; E-DIR. DERIVERTIVE OF Y
C
C
C      (1) GAUSS POINTS SETTING
C
      CALL GAUS2D(IGAUSS,MPOINT,GP,EP,WP,NPOINT,IUT0,IERR)
C
      IF(IERR.EQ.1) THEN
          WRITE(IUT0,6300) ERMSG 
          RETURN
      ENDIF
C
C      (2) ARRAYS CLEARING
C
      DO 100 IE = 1 , NE
          DELTA(IE) = 0.D0
  100 CONTINUE
C
      DO 210 I = 1 , N
          DO 200 IE = 1 , NE
               SN (I,IE) = 0.D0
               DNX(I,IE) = 0.D0
               DNY(I,IE) = 0.D0
  200     CONTINUE
  210 CONTINUE
C
      IF(IMODE.EQ.1) THEN
      DO 320 I = 1 , N
          DO 310 J = 1 , N
              DO 300 IE = 1 , NE
                  E  (J,I,IE) = 0.D0
                  EX (J,I,IE) = 0.D0
                  EY (J,I,IE) = 0.D0
                  EXX(J,I,IE) = 0.D0
                  EYY(J,I,IE) = 0.D0
                  EXY(J,I,IE) = 0.D0
  300         CONTINUE
  310     CONTINUE
  320 CONTINUE
      ELSE
      DO 420 I = 1 , N
          DO 410 J = 1 , N
              DO 400 IE = 1 , NE
                  E  (J,I,IE) = 0.D0
                  EXX(J,I,IE) = 0.D0
  400         CONTINUE
  410     CONTINUE
  420 CONTINUE
      ENDIF
C
C      (3) GAUSS INTEGRATION
C
      DO 830 IPOINT = 1 , NPOINT
          GG = GP(IPOINT) 
          EG = EP(IPOINT) 
          W = WP(IPOINT)
C
          CALL DERIV2(GG,EG,X,Y,NODE,NE,N,SNI,DNXI,DNYI,DET,
     *                DXG,DYG,DXE,DYE)
C
          DO 500 IE = 1 , NE
              DELTA(IE) = DELTA(IE)+W*DET(IE)
  500     CONTINUE
C
          DO 610 I = 1 , N
              DO 600 IE = 1 , NE
                  SN (I,IE) = SN (I,IE)+W*DET(IE)* SNI(I,IE)
                  DNX(I,IE) = DNX(I,IE)+W*DET(IE)*DNXI(I,IE)
                  DNY(I,IE) = DNY(I,IE)+W*DET(IE)*DNYI(I,IE)
  600         CONTINUE
  610     CONTINUE
C
          IF(IMODE.EQ.1) THEN
          DO 720 I = 1 , N
             DO 710 J = 1 , N
              DO 700 IE = 1 , NE
               E  (J,I,IE) = E  (J,I,IE)+W*DET(IE)*SNI (I,IE)*SNI (J,IE)
               EX (J,I,IE) = EX (J,I,IE)+W*DET(IE)*SNI (I,IE)*DNXI(J,IE)
               EY (J,I,IE) = EY (J,I,IE)+W*DET(IE)*SNI (I,IE)*DNYI(J,IE)
               EXX(J,I,IE) = EXX(J,I,IE)+W*DET(IE)*DNXI(I,IE)*DNXI(J,IE)
               EYY(J,I,IE) = EYY(J,I,IE)+W*DET(IE)*DNYI(I,IE)*DNYI(J,IE)
               EXY(J,I,IE) = EXY(J,I,IE)+W*DET(IE)*DNXI(I,IE)*DNYI(J,IE)
  700         CONTINUE
  710        CONTINUE
  720     CONTINUE
          ELSE
          DO 820 I = 1 , N
             DO 810 J = 1 , N
              DO 800 IE = 1 , NE
               E  (J,I,IE) = E  (J,I,IE)+W*DET(IE)*SNI (I,IE)*SNI (J,IE)
               EXX(J,I,IE) = EXX(J,I,IE)+W*DET(IE)*DNXI(I,IE)*DNXI(J,IE)
     &                                  +W*DET(IE)*DNYI(I,IE)*DNYI(J,IE)
  800         CONTINUE
  810        CONTINUE
  820     CONTINUE
          ENDIF
  830 CONTINUE
C
      DO 900 IE = 1 , NE
          DELTA(IE) = DELTA(IE)**0.5D0
  900 CONTINUE
C
C      (4) ELEMENT CENTER X,Y DERIVERTIVES OF SHAPE FUNCTION
C
        GG = 0.D0       
        EG = 0.D0
        CALL DERIV2(GG,EG,X,Y,NODE,NE,N,SNI,DNXI,DNYI,DET,
     *              DXG,DYG,DXE,DYE)
C
C
      RETURN
 6300 FORMAT(A72)
      END
