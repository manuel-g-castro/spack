C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    FIND3E                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE FIND3E(IOPT,IMODE,ELM,XM,YM,ZM,GM,EM,TM,IEM,IEMWRK,NM,
     *                  IENE,NEE,NE,MEE,NITER,
     *                  XMINE,YMINE,ZMINE,XMAXE,YMAXE,ZMAXE,
     *                  G,E,T,IWRK,LOVER,MOVER,IUT0,IERR)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION ELM(24,NE),XM(NM),YM(NM),ZM(NM),GM(NM),EM(NM),TM(NM),
     1          IEM(NM),IEMWRK(NM),IENE(MEE,NE),NEE(NE),
     2          G(NE),E(NE),T(NE),IWRK(NE),LOVER(MOVER),
     3          XMINE(NE),YMINE(NE),ZMINE(NE),
     4          XMAXE(NE),YMAXE(NE),ZMAXE(NE)
      DIMENSION DELTA(1)
C
      DATA JCHECK / 0 /
C
      CHARACTER*60 ERMSGB
     & / ' ## SUBROUTINE FIND3E: FATAL      ERROR OCCURENCE; RETURNED' /
      CHARACTER*60 EREXP1
     & / ' INSUFFICIENT ARRAY SIZE FOR ADJACENET ELEMENT SEARCH      ' /
      CHARACTER*60 EREXP2
     & / ' INSUFFICIENT ARRAY SIZE FOR ALL       ELEMENT SEARCH      ' /
C
C
C      FIND ELEMENTS INCLUDING SPECIFIED POINTS. RETURN THE ELEMENT
C     NUMBERS AND LOCAL COORDINATES OF THE POINTS
C      5 DIFFEERENT ELEMENT FINDING OPTIONS ARE AVAILABLE FOR EFFICIENT
C     ELEMENT SEARCH
C         ( 3-D CALCULATION )
C
C
C     NOTE ; 1. INCLUSION IN AN ELEMENT OF A POINT WILL BE JUDGED
C              BASED ON ITS LOCAL GZAI, EATA, AND THETA COORDINATES,
C              CALCULATED BY THE NEWTON LAPSON METHODS.
C
C     NOTE ; 2. TOTAL OF 'NITER' ITERATIONS WILL BE DONE WITH THE
C              NEWTON LAPSON METHOD, REGARDLESS TO ITS CONVERGENCE.
C              BUT, TWO OR THREE ITERATIONS ARE, IN GENERAL, ENOUGH
C              TO OBTAIN THE LOCAL COORDINATES WITH REASONABLE ACCURACY
C              UNLESS THE ELEMENT IS STRONGLY SKEWED.
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          IOPT        ; FLAG WHICH CONTROLS ELEMENT SEARCH
C                   0 --- FOR THOSE POINTS WITH POSITIVE ELEMENT
C                        NUMBERS, JUDGE IF THEY ARE INCLUDED IN THE
C                        SPECIFIED ELEMENTS.
C                         RETURN THE LOCAL COORDINATES FOR THOSE POINTS
C                        INCLUDED IN THE SPECIFIED ELEMENTS, (-1)*
C                        GIVEN ELEMENT NUMBERS FOR THOSE NOT INCLUDED
C                        IN THE SPECIFIED ELEMENTS.
C                   1 --- SAME AS IOPT=0 EXCEPT THAT
C                        ADJACENT ELEMENT SEARCH WILL BE DONE FOR THOSE
C                        POINTS NOT INCLUDED IN THE SPECIFIED ELEMENTS,
C                         RETURN THE NEW ELEMENT NUMBERS AND LOCAL
C                        COORDINATES FOR THOSE INCLUDED IN SOME OF THEIR
C                        ADJACENT ELEMENTS AND (-1)*GIVEN ELEMENT NUMBER
C                        FOR THOSE NOT INCLUDED IN ANY OF THEIR ADJACENT
C                        ELEMENTS.
C                   2 --- SAME AS IOPT=1 EXCEPT THAT
C                        ALL ELEMENT SEARCH WILL BE DONE FOR THOSE
C                        POINTS NOT INCLUDED IN ANY OF THEIR ADJACENT
C                        ELEMENTS,
C                         RETURN THE NEW ELEMENT NUMBERS AND LOCAL
C                        COORDINATES FOR THOSE INCLUDED IN ANY ELEMENTS
C                        AND (-1)*GIVEN ELEMENT NUMBER FOR THOSE NOT
C                        INCLUDED IN ANY ELEMENTS.
C                   3 --- SAME AS IOPT=2 EXCEPT THAT
C                        FOR THOSE POINTS INITIALLY POSSESSING ZERO OR
C                        NEGATIVE ELEMENT NUMBERS ALSO, ALL ELEMENT
C                        SEARCH WILL BE DONE. FOR THIS PARTICULAR
C                        ELEMENT SEARCH, RETURN THE GIVEN ELEMENT NUMBER
C                        FOR THOSE NOT INCLUDED IN ANY ELEMENTS.
C                   4 --- FOR ALL THE GIVEN POINTS, ALL ELEMENT SEARCH
C                        WILL BE DONE.
C                         RETURN THE ELEMENT NUMBER AND LOCAL
C                        COORDINATES FOR THOSE POINTS INCLUDED IN ANY
C                        ELEMENT, ELEMENT NUMBER OF ZERO FOR THOSE NOT
C                        INCLUDED IN ANY ELEMENT.
C
C          IMODE       ; PASS ARGUMENTS 'XMINE'-'ZMAXE' WITH THIS FLAG
C                       BEING SET TO ONE, TO ACTIVATE FAST ELEMENT
C                       SEARCH MODE
C
C           NOTES; 'FIND3E' RESTRICTS THOSE ELEMENTS TO BE SEARCHED
C                 BASED ON THE PASSED ELEMENT'S MINIMUM & MAXIMUM
C                 COORDINATES IF 'IMODE' FLAG IS BEING SET TO ONE.
C                 ELEMENT RESTRICTIONS WILL BE DONE WHEN THE ADJACENT
C                 ELEMENTS ARE SEARCHED AS WELL AS ALL THE ELEMENT ARE
C                 SEARCHED. NOTE THAT VECTOR OPERATIONS WILL BE 
C                 SUBSTANTIALLY SURPRESSED FOR ALL THE ELEMENT SEARCH
C                 IF 'IMODE' FLAG IS BEING SET TO ONE, BECAUSE THE 
C                 NUMBER OF ELEMENTS TO BE SEARCHED WILL BE SMALL
C                 DUE TO THE RESTRICTION.
C
C          ELM( 1,IE)  ; 0.125*SUM OF X(NODE(I,IE))
C          ELM( 2,IE)  ; 0.125*SUM OF Y(NODE(I,IE))
C          ELM( 3,IE)  ; 0.125*SUM OF Z(NODE(I,IE))
C
C          ELM( 4,IE)  ; 0.125*SUM OF X(NODE(I,IE))*GI(I)
C          ELM( 5,IE)  ; 0.125*SUM OF Y(NODE(I,IE))*GI(I)
C          ELM( 6,IE)  ; 0.125*SUM OF Z(NODE(I,IE))*GI(I)
C          ELM( 7,IE)  ; 0.125*SUM OF X(NODE(I,IE))*EI(I)
C          ELM( 8,IE)  ; 0.125*SUM OF Y(NODE(I,IE))*EI(I)
C          ELM( 9,IE)  ; 0.125*SUM OF Z(NODE(I,IE))*EI(I)
C          ELM(10,IE)  ; 0.125*SUM OF X(NODE(I,IE))*TI(I)
C          ELM(11,IE)  ; 0.125*SUM OF Y(NODE(I,IE))*TI(I)
C          ELM(12,IE)  ; 0.125*SUM OF Z(NODE(I,IE))*TI(I)
C
C          ELM(13,IE)  ; 0.125*SUM OF X(NODE(I,IE))*GI(I)*EI(I)
C          ELM(14,IE)  ; 0.125*SUM OF Y(NODE(I,IE))*GI(I)*EI(I)
C          ELM(15,IE)  ; 0.125*SUM OF Z(NODE(I,IE))*GI(I)*EI(*)
C          ELM(16,IE)  ; 0.125*SUM OF X(NODE(I,IE))*EI(I)*TI(I)
C          ELM(17,IE)  ; 0.125*SUM OF Y(NODE(I,IE))*EI(I)*TI(I)
C          ELM(18,IE)  ; 0.125*SUM OF Z(NODE(I,IE))*EI(I)*TI(*)
C          ELM(19,IE)  ; 0.125*SUM OF X(NODE(I,IE))*TI(I)*GI(I)
C          ELM(20,IE)  ; 0.125*SUM OF Y(NODE(I,IE))*TI(I)*GI(I)
C          ELM(21,IE)  ; 0.125*SUM OF Z(NODE(I,IE))*TI(I)*GI(*)
C
C          ELM(22,IE)  ; 0.125*SUM OF X(NODE(I,IE))*GI(I)*EI(I)*TI(I)
C          ELM(23,IE)  ; 0.125*SUM OF Y(NODE(I,IE))*GI(I)*EI(I)*TI(I)
C          ELM(24,IE)  ; 0.125*SUM OF Z(NODE(I,IE))*GI(I)*EI(I)*TI(I)
C
C          XM    (IM)  ; X-DIR. COORDINATE OF THE POINTS
C          YM    (IM)  ; Y-DIR. COORDINATE OF THE POINTS
C          ZM    (IM)  ; Z-DIR. COORDINATE OF THE POINTS
C          IEM   (IM)  ; ELEMENT NUMBER WHICH MOST PROBABLY INCLUDES
C                       THE POINTS. SET IOPT=4 IF NO INITIAL GUESS IS
C                       POSSIBLE FOR THE ELEMENT NUMBERS.
C          NM          ; NUMBER OF POINTS
C          IENE(IEE,IE); ADJACENT ELEMENT NUMBER TO ELEMENT IE
C                      ( DUMMY ARGUMENT FOR IOPT = 0 OR 4 )
C          NEE     (IE); NUMBER OF ADJACENT ELEMENTS TO ELEMENT IE
C                      ( DUMMY ARGUMENT FOR IOPT = 0 OR 4 )
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          MEE         ; THE FIRST DIMENSION OF ARRAY IENE
C          NITER       ; NUMBER OF ITERATIVE CALCULATIONS TO BE DONE
C
C           NOTES; THE FOLLOWING MIN & MAX VALUES WILL BE USED TO
C                 ACTIVATE FAST ELEMENT SEARCH MODE AND ARE NEEDED ONLY
C                 WHEN IMODE = 1 IS SPECIFIED.
C          XMINE   (IE); MINIMUM X-DIR. COORDINATE OF ELEMENT
C          YMINE   (IE); MINIMUM Y-DIR. COORDINATE OF ELEMENT
C          ZMINE   (IE); MINIMUM Z-DIR. COORDINATE OF ELEMENT
C          XMAXE   (IE); MAXIMUM X-DIR. COORDINATE OF ELEMENT
C          YMAXE   (IE); MAXIMUM Y-DIR. COORDINATE OF ELEMENT
C          ZMAXE   (IE); MAXIMUM Z-DIR. COORDINATE OF ELEMENT
C
C          MOVER       ; DIMENSION OF ARRAY 'LOVER(IOVER)'
C          IUT0        ; FILE NUMBER TO REPORT AN ERROR OCCURENCE
C
C       (2) OUTPUT
C          IEM   (IM)  ; ELEMENT NUMBER WHICH INCLUDES THE SPECIIED
C                       POINTS WHEN THIS IS POSITIVE. FOR NEGATIVE
C                       NUMBER RETURNED, SEE EXPLANATION ON IOPT
C          GM    (IM)  ; GZAI  COORDINATE OF THE POINT FOR ELEMENT FOUND
C          EM    (IM)  ; EATA  COORDINATE OF THE POINT FOR ELEMENT FOUND
C          TM    (IM)  ; THETA COORDINATE OF THE POINT FOR ELEMENT FOUND
C          IERR        ; RETURN CODE TO REPORT AN ERROR OCCURENCE
C                   0 --- NORMAL TERMINATION
C                   1 --- FATAL ERROR OCCURENCE
C
C       (4) WORK
C          IEMWRK(IM)  ; PREPARE FOR ALL POINTS     WHEN IOPT = 0,1,2,3
C          G     (IE)  ; PREPARE FOR ALL ELEMENTS   WHEN IOPT =   2,3,4
C          E     (IE)  ; PREPARE FOR ALL ELEMENTS   WHEN IOPT =   2,3,4
C          T     (IE)  ; PREPARE FOR ALL ELEMENTS   WHEN IOPT =   2,3,4
C          IWRK  (IE)  ; PREPARE FOR ALL ELEMENTS   WHEN IOPT =   2,3,4
C          LOVER(IOVER); PREPARE FOR THOSE POINTS FOR WHICH ADJACENT
C                       ELEMENT SEARCH WILL BE DONE WHEN IOPT = 1,2,3
C
C
      IERR  = 0
C
      IF(IOPT.EQ.4) GO TO 600
C
C
C STORE INITIAL ELEMENT NUMBERS IN IEMWRK(IEM)
C
C
      DO 100 IM = 1 , NM
          IEMWRK(IM) = IEM(IM)
  100 CONTINUE
C
C
C SEARCH THE SPECIFIED ELEMENTS
C
C
      CALL FIND32(ELM,NE,XM,YM,ZM,IEM,NM,NITER,GM,EM,TM,
     *            JCHECK,DELTA,ERRMAX)
      IF(IOPT.EQ.0) RETURN
C
C
C SEARCH THE ADJACENT ELEMENTS TO THE SPECIFIED ELEMENTS
C
C
      NOVER = 0
      DO 110 IM = 1 , NM
          IF(IEM(IM).EQ.IEMWRK(IM)) GO TO 110
          NOVER = NOVER+1
          IF(NOVER.GT.MOVER) THEN
              WRITE(IUT0,*) ERMSGB
              WRITE(IUT0,*) EREXP1
              IERR = 1
              RETURN
          ENDIF
          LOVER(NOVER) = IM
  110 CONTINUE
C
      DO 310 IEE = 1 , MEE
          DO 200 IOVER = 1 , NOVER
              IM      = LOVER(IOVER)
              IEM(IM) = 0
              IF(NEE(IEMWRK(IM)).LT.IEE) GO TO 200
C
C FOR FAST ELEMENT SEARCH
              IECHK = IENE(IEE,IEMWRK(IM))
              IF(IMODE.EQ.1 .AND. XMINE(IECHK).GT.XM(IM)) GO TO 200
              IF(IMODE.EQ.1 .AND. YMINE(IECHK).GT.YM(IM)) GO TO 200
              IF(IMODE.EQ.1 .AND. ZMINE(IECHK).GT.ZM(IM)) GO TO 200
              IF(IMODE.EQ.1 .AND. XMAXE(IECHK).LT.XM(IM)) GO TO 200
              IF(IMODE.EQ.1 .AND. YMAXE(IECHK).LT.YM(IM)) GO TO 200
              IF(IMODE.EQ.1 .AND. ZMAXE(IECHK).LT.ZM(IM)) GO TO 200
C
              IEM(IM) = IENE(IEE,IEMWRK(IM))
  200     CONTINUE
          CALL FIND33(ELM,NE,NM,XM,YM,ZM,IEM,LOVER,NOVER,NITER,GM,EM,TM,
     *                JCHECK,DELTA,ERRMAX)
          NOVER  = 0
          DO 300 IM = 1 , NM
              IF(IEMWRK(IM).LE.0 .OR. IEM(IM).GT.0) GO TO 300
              NOVER        = NOVER+1
              LOVER(NOVER) = IM
              IEM(IM) = -IEMWRK(IM)
  300     CONTINUE
  310 CONTINUE
C
      IF(IOPT.EQ.1) RETURN
C
C
C SEARCH ALL ELEMENTS
C
C
      IF(IOPT.EQ.3) THEN
      DO 410 IM = 1 , NM
          IF(IEMWRK(IM).GT.0) GO TO 410
          NOVER = NOVER+1
          IF(NOVER.GT.MOVER) THEN
              WRITE(IUT0,*) ERMSGB
              WRITE(IUT0,*) EREXP2
              IERR = 1
              RETURN
          ENDIF
          LOVER(NOVER) = IM
  410 CONTINUE
      ENDIF
C
      DO 510 IOVER = 1 , NOVER
          IM = LOVER(IOVER)
          XP = XM(IM)
          YP = YM(IM)
          ZP = ZM(IM)
          CALL FIND31(IMODE,ELM,NE,XP,YP,ZP,NITER,
     *                XMINE,YMINE,ZMINE,XMAXE,YMAXE,ZMAXE,
     *                IFOUND,GP,EP,TP,JCHECK,DELTA,ERR,G,E,T,IWRK)
          IF(IFOUND.GT.0) THEN
              IEM(IM) = IFOUND
               GM(IM) = GP
               EM(IM) = EP
               TM(IM) = TP
          ELSE
             IEM(IM) = MIN(-IEMWRK(IM), IEMWRK(IM))
          ENDIF
  510 CONTINUE
C
      IF(IOPT.LE.3) RETURN
C
  600 CONTINUE
      DO 610 IM = 1 , NM
          XP = XM(IM)
          YP = YM(IM)
          ZP = ZM(IM)
          CALL FIND31(IMODE,ELM,NE,XP,YP,ZP,NITER,
     *                XMINE,YMINE,ZMINE,XMAXE,YMAXE,ZMAXE,
     *                IEM(IM),GP,EP,TP,JCHECK,DELTA,ERR,G,E,T,IWRK)
          GM (IM) = GP
          EM (IM) = EP
          TM (IM) = TP
  610 CONTINUE
C
C
      RETURN
      END
