C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    ELEM3D                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE ELEM3D(IGAUSS,X,Y,Z,NODE,ME,NE,NP,N,
     *                  SNI ,DNXI,DNYI,DNZI,
     *                  SN  ,DNX ,DNY ,DNZ ,
     *                  E   ,EX  ,EY  ,EZ  ,
     *                  EXX ,EYY ,EZZ ,
     *                  EXY ,EXZ ,EYZ ,DELTA,
     *                  DET,DXG,DYG,DZG,
     *                      DXE,DYE,DZE,
     *                      DXT,DYT,DZT,DGET,
     *                  IUT0,IERR)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION NODE(N,NE),X(NP),Y(NP),Z(NP),
     1          SNI  (N,NE),DNXI (N,NE),DNYI (N,NE),DNZI  (N,NE),
     2          SN   (N,NE),DNX  (N,NE),DNY  (N,NE),DNZ   (N,NE),
     3          E  (ME,N,N),EX (ME,N,N),EY (ME,N,N),EZ (ME,N,N),
     4          EXX(ME,N,N),EYY(ME,N,N),EZZ(ME,N,N),
     5          EXY(ME,N,N),EXZ(ME,N,N),EYZ(ME,N,N),DELTA(NE),
     6          DET(NE),DXG(NE),DYG(NE),DZG(NE),DXE(NE),DYE(NE),DZE(NE),
     7                  DXT(NE),DYT(NE),DZT(NE),DGET(N,NE)
C
      PARAMETER ( MPOINT = 4*4*4 )
      DIMENSION GP(MPOINT),EP(MPOINT),TP(MPOINT),WP(MPOINT)
C
      CHARACTER*60 ERMSGB
     & /' ## SUBROUTINE ELEM3D: FATAL      ERROR OCCURENCE; RETURNED' /
C
C
C      CALCULATE ELEMENT CHARACTERISTIC DIMENSION AND
C     INTEGRAL ELEMENT VECTOR AND MATRICES AND CALCULATE ELEMENT CENTER
C     X,Y,Z DERIVERTIVES OF SHAPE FUNCTION
C         ( 3-D CALCULATION : SINGLE WORD & SAME ELEMENT VERSION )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          IGAUSS      ; SPECIFIES NUMBER OF GAUSS POINTS IN ONE DIR.
C          X     (IP)  ; X-DIR. COORDINATE         OF NODE
C          Y     (IP)  ; Y-DIR. COORDINATE         OF NODE
C          Z     (IP)  ; Y-DIR. COORDINATE         OF NODE
C          NODE(I,IE)  ; NODE TABLE
C          ME          ; MAX. NUMBER OF TOTAL ELEMENTS
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          NP          ; NUMBER OF TOTAL    NODES
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT
C          IUT0        ; FILE NUMBER TO REPORT ERROR OCCURENCE
C
C       (2) OUTPUT
C           NOTES ; IN THE FOLLOWING EXPRESSIONS, 'N' DENOTES THE
C          SHAPE FUNCTION AND 'NX', 'NY', AND, 'NZ', RESPECTIVELY DENOTE
C          X, Y, AND, Z DERIVERTIVES OF THE SHAPE FUNCTION. THE SURFIX
C          'T' DENOTES THE TRANSVERS OF THE MATRICES OR VECTORS.
C          SNI  (I,IE) ; ELEMENT CENTER VALUE OF N
C          DNXI (I,IE) ; ELEMENT CENTER VALUE OF NX
C          DNYI (I,IE) ; ELEMENT CENTER VALUE OF NY
C          DNZI (I,IE) ; ELEMENT CENTER VALUE OF NZ
C          SN   (I,IE) ; ELEMENT INTEGRATED VECTOR OF N
C          DNX  (I,IE) ; ELEMENT INTEGRATED VECTOR OF NX
C          DNY  (I,IE) ; ELEMENT INTEGRATED VECTOR OF NY
C          DNZ  (I,IE) ; ELEMENT INTEGRATED VECTOR OF NZ
C
C          E   (I,IE,J); INTEGRATED ELEMENT MATRIX OF N*NT
C          EX  (I,IE,J); INTEGRATED ELEMENT MATRIX OF N*NXT
C          EY  (I,IE,J); INTEGRATED ELEMENT MATRIX OF N*NYT
C          EZ  (I,IE,J); INTEGRATED ELEMENT MATRIX OF N*NZT
C          EXX (I,IE,J); INTEGRATED ELEMENT MATRIX OF NX*NXT
C          EYY (I,IE,J); INTEGRATED ELEMENT MATRIX OF NY*NYT
C          EZZ (I,IE,J); INTEGRATED ELEMENT MATRIX OF NZ*NZT
C          EXY (I,IE,J); INTEGRATED ELEMENT MATRIX OF NX*NYT
C          EXZ (I,IE,J); INTEGRATED ELEMENT MATRIX OF NX*NZT
C          EYZ (I,IE,J); INTEGRATED ELEMENT MATRIX OF NY*NZT
C
C          DELTA  (IE) ; ELEMENT CHARACTERISTIC DIMENSION
C          IERR        ; RETURN CODE TO REPORT ERROR OCCURENCE
C                   0 --- NORMAL TERMINATION
C                   1 --- A FATAL ERROR HAS OCCURED
C
C       (3) WORK
C           NOTES ; PREPARE THE FOLLOWING ARRAYS FOR MAX(IE) = NE,
C                  ANE MAX(K) = 3.
C          DET    (IE) ; DETERMINANT OF JACOBIAN MATRIX
C          DXG    (IE) ; G-DIR. DERIVERTIVE OF X
C          DYG    (IE) ; G-DIR. DERIVERTIVE OF Y
C          DZG    (IE) ; G-DIR. DERIVERTIVE OF Z
C          DXE    (IE) ; E-DIR. DERIVERTIVE OF X
C          DYE    (IE) ; E-DIR. DERIVERTIVE OF Y
C          DZE    (IE) ; E-DIR. DERIVERTIVE OF Z
C          DXT    (IE) ; T-DIR. DERIVERTIVE OF X
C          DYT    (IE) ; T-DIR. DERIVERTIVE OF Y
C          DZT    (IE) ; T-DIR. DERIVERTIVE OF Z
C          DGET (K,IE) ; D(G,E,T)/D(X,Y,Z) OF ELEMENT
C
C
C      (1) GAUSS POINTS SETTING
C
      CALL GAUS3D(IGAUSS,MPOINT,GP,EP,TP,WP,NPOINT,IUT0,IERR)
C
      IF(IERR.EQ.1) THEN
          WRITE(IUT0,*) ERMSGB
          RETURN
      ENDIF
C
C      (2) ARRAYS CLEARING
C
      DO 100 IE = 1 , NE
           DELTA(IE) = 0.E0
  100 CONTINUE
C
      DO 210 I = 1 , N
          DO 200 IE = 1 , NE
               SN (I,IE) = 0.E0
               DNX(I,IE) = 0.E0
               DNY(I,IE) = 0.E0
               DNZ(I,IE) = 0.E0
  200     CONTINUE
  210 CONTINUE
C
      DO 320 J = 1 , N
          DO 310 I = 1 , N
              DO 300 IE = 1 , NE
                  E  (IE,I,J) = 0.E0
                  EX (IE,I,J) = 0.E0
                  EY (IE,I,J) = 0.E0
                  EZ (IE,I,J) = 0.E0
                  EXX(IE,I,J) = 0.E0
                  EYY(IE,I,J) = 0.E0
                  EZZ(IE,I,J) = 0.E0
                  EXY(IE,I,J) = 0.E0
                  EXZ(IE,I,J) = 0.E0
                  EYZ(IE,I,J) = 0.E0
  300         CONTINUE
  310     CONTINUE
  320 CONTINUE
C
C      (3) GAUSS INTEGRATION
C
      DO 630 IPOINT = 1 , NPOINT
          GG = GP(IPOINT)
          EG = EP(IPOINT)
          TG = TP(IPOINT)
          W  = WP(IPOINT)
C
          CALL DERIV3(GG,EG,TG,X,Y,Z,NODE,NE,NP,N,
     *                SNI,DNXI,DNYI,DNZI,DET,
     *                DXG,DYG,DZG,DXE,DYE,DZE,DXT,DYT,DZT,DGET)
C
          DO 400 IE = 1 , NE
              DELTA(IE) = DELTA(IE)+W*DET(IE)
  400     CONTINUE
C
          DO 510 I = 1 , N
              DO 500 IE = 1 , NE
                  SN (I,IE) = SN (I,IE)+W*DET(IE)* SNI(I,IE)
                  DNX(I,IE) = DNX(I,IE)+W*DET(IE)*DNXI(I,IE)
                  DNY(I,IE) = DNY(I,IE)+W*DET(IE)*DNYI(I,IE)
                  DNZ(I,IE) = DNZ(I,IE)+W*DET(IE)*DNZI(I,IE)
  500         CONTINUE
  510     CONTINUE
C
          DO 620 I = 1 , N
             DO 610 J = 1 , N
              DO 600 IE = 1 , NE
               E  (IE,I,J) = E  (IE,I,J)+W*DET(IE)*SNI (I,IE)*SNI (J,IE)
               EX (IE,I,J) = EX (IE,I,J)+W*DET(IE)*SNI (I,IE)*DNXI(J,IE)
               EY (IE,I,J) = EY (IE,I,J)+W*DET(IE)*SNI (I,IE)*DNYI(J,IE)
               EZ (IE,I,J) = EZ (IE,I,J)+W*DET(IE)*SNI (I,IE)*DNZI(J,IE)
               EXX(IE,I,J) = EXX(IE,I,J)+W*DET(IE)*DNXI(I,IE)*DNXI(J,IE)
               EYY(IE,I,J) = EYY(IE,I,J)+W*DET(IE)*DNYI(I,IE)*DNYI(J,IE)
               EZZ(IE,I,J) = EZZ(IE,I,J)+W*DET(IE)*DNZI(I,IE)*DNZI(J,IE)
               EXY(IE,I,J) = EXY(IE,I,J)+W*DET(IE)*DNXI(I,IE)*DNYI(J,IE)
               EXZ(IE,I,J) = EXZ(IE,I,J)+W*DET(IE)*DNXI(I,IE)*DNZI(J,IE)
               EYZ(IE,I,J) = EYZ(IE,I,J)+W*DET(IE)*DNYI(I,IE)*DNZI(J,IE)
  600         CONTINUE
  610        CONTINUE
  620     CONTINUE
  630 CONTINUE
C
      DO 700 IE = 1 , NE
          DELTA(IE) = DELTA(IE)**0.333E0
  700 CONTINUE
C
C      (4) ELEMENT CENTER X,Y DERIVERTIVES OF SHAPE FUNCTION
C
      GG = 0.E0
      EG = 0.E0
      TG = 0.E0
C
      CALL DERIV3(GG,EG,TG,X,Y,Z,NODE,NE,NP,N,
     *            SNI,DNXI,DNYI,DNZI,DET,
     *            DXG,DYG,DZG,DXE,DYE,DZE,DXT,DYT,DZT,DGET)
C
      RETURN
      END
