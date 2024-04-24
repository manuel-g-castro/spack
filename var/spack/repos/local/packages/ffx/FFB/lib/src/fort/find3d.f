C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    FIND3D                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE FIND3D(MCNTYP,X,Y,Z,NODE,NE,NP,N,LAPEX,NPOLY,
     *                  E,XM,YM,ZM,IEM,NM,IUT0,IWRN)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION X(NP),Y(NP),Z(NP),NODE(N,NE),
     1          LAPEX(4,NPOLY),E(3,3,NPOLY,NE),
     2          XM(NM),YM(NM),ZM(NM),IEM(NM)
C
      CHARACTER*60 WRMSGA
     & /' ## SUBROUTINE FIND3D: WARNING          ISSUING  ; CONTINUE' /
      CHARACTER*60 WREXP1
     & /' SPECIFIED POINT IS OUT OF THE COMPUTAION DOMAIN           ' /
C
      D = 1.E-1
C
C
C      FIND ELEMENTS WHICH INCLUDE SPECIFIED POINTS
C         ( 3-D CALCULATION )
C
C
C     NOTE 1 ;  IF A SPECIFIED POINT DOES NOT BELONG TO ANY ELEMENT,
C              SUBROUTINE FIND3D WILL ISSUE A WARNING AND CONTINUE
C              THE NORMAL PROCESS.
C        
C     NOTE 2 ;  X, Y, AND Z COORDINATES OF EACH POINT ARE TRANSFORMED,
C              BY LINEAR TRANSFORMATION, TO LOCAL COORDINATES IN A 
C              TETRAHEDRON CONSTITUTING AN ELEMENT. BASED ON THESE LOCAL
C              COORDINATES OF THE POINT FOR FIVE TETRAHEDRA (OR ONE
C              TETRAHEDRON FOR A TETRAHEDRAL ELEMENT) CONSTITUTING
C              AN ELEMENT, THE POINT WILL BE JUDGED WHETER OR NOT TO 
C              RESIDE IN THE ELEMENT. THE SEARCHING ALGORITHM USED IN 
C              THIS SUBROUTINE IS ROUGHLY TWICE AS FAST AS USED IN 
C              ANOTHER SUBROUTINE 'FIND3E', FOR WHICH RESIDING ELEMENT
C              SEARCH WILL BE DONE BASED ON THE SHAPE FUNCTIONS.
C
C     BUGS   ; THERE IS A SLIGHT CHANCE OF FAILURE IN FINDING A
C             RESIDING ELEMENT FOR A POINT EVEN IF THE POINT IS 
C             IN THE COMPUTATIONAL DOMAIN. THE BASIC SEARCHING REGION
C             OF THIS SUBROUTINE DOES NOT EXACTLY COVER THE ENTIRE
C             COMPUTATIONAL DOMAIN. A SMALL REGION CAN EXIST, DEPENDING
C             ON THE SHAPES OF ELEMENTS AND THE ORDER OF ELEMENT-WISE
C             NODE NUMBERING, BETWEEN ELEMENT BOUNDARIES, WHERE THE
C             CURRENT SEARCHING ALGORITHM DOES NOT CHECK. IN ORDER TO
C             KIND OF FIX THIS BUG, THIS SUBROUTINE EXPANDS EACH
C             ELEMENT BOUNDARIES BY A SMALL FRACTION OF 1.0. BUT, THIS
C             FIXING IS NOT PERFECT. IF SUCH REGION STILL EXISTS AND A
C             POINT HAPPENS TO RESIDE IN THAT REGION, THE ELEMENT
C             SEARCH FOR THAT POINT WILL FAIL. THIS BUG IS FIXED IN 
C             THE SEARCHING ALGORITHM IMPLEMENTED IN THE NEXT VERSION
C             OF ELEMENT SEARCHING SUBROUTINE 'FIND3E'.
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          MCNTYP      ; SPECIFY MACHINE TYPE TO EXECUTE THE OBJECT CODE
C                   1 --- VECTOR   ARCHITECTURE MACHINE
C                   2 --- PARALLEL ARCHITECTURE MACHINE
C                 NOTE 1 ; ACCORDING TO THE VALUE OF THIS ARGUMENT, THE
C                         DO LOOP TO BE EXECUTED WILL BE ALTERED BASED
C                         ON AN ASSUMPTION THAT NUMBER OF TOTAL ELEMENTS
C                         IS, IN GENERAL, VERY LARGE, WHILE NUMBER OF 
C                         TOTAL POINTS IS NOT ALWAYS LARGE. 
C                          IF THE MACHINE DOES NOT ADOPT VECTOR, NOR
C                         PARALLEL ARCHITECTUE, SPECIFY THIS ARGUMENT
C                         TO 2 ( PARALLEL ). 
C     
C          X       (IP); X-DIR. COORDINATE OF NODE
C          Y       (IP); Y-DIR. COORDINATE OF NODE
C          Z       (IP); Z-DIR. COORDINATE OF NODE
C          NODE  (I,IE); NODE NO. TABLE BASED ON ELEMENT
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          NP          ; NUMBER OF TOTAL    NODES
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT(4 OR 8)
C          LAPEX(IA,IG); APEX NO. OF TETRAHEDRA CONSTITUTING AN ELEMENT
C          NPOLY       ; NUMBER OF TETRAHEDRA CONSTITUTING AN ELEMENT
C          E(J,I,IG,IE); INVERSE MATRICES OF ELEMENT BASE VECTOR
C          XM      (IM); X-COORDINATES OF THE SPECIFIED POINTS
C          YM      (IM); Y-COORDINATES OF THE SPECIFIED POINTS
C          ZM      (IM); Z-COORDINATES OF THE SPECIFIED POINTS
C          NM          ; NUMBER OF THE SPECIFIED POINTS
C          IUT0        ; DEVICE NUMBER TO ISSUE A WARNING
C
C       (2) OUTPUT
C          IEM     (IM); ELEMENT NO.'S FOUND
C                       IF NO ELEMENT IS FOUND FOR POINT 'IM', 'IEM(IM)'
C                      WILL BE SET TO ZERO FOR THAT POINT.
C          IWRN        ; RETURN CODE TO REPORT A WARNING
C                   0 --- NORMAL TERMINATION
C                   1 --- A WARNING ISSUED
C
C

      IWRN = 0
C
      DO 100 IM = 1 , NM
          IEM   (IM) = 0
  100 CONTINUE
C
C FOR A VECTOR MACHINE
C
      IF(MCNTYP.EQ.1) THEN
      DO 220 IM = 1 , NM
          DO 210 IG = 1 , NPOLY
              IF(IEM(IM).GE.1) GO TO 210
C
C NOTE ; THE ABOVE IF STATEMENT COULD BE PUT IN THE NEXT DO LOOP (IE)
C       TO REDUCE THE AMOUNT OF OPERATIONS TO BE EXECUTED. BUT, THAT
C       WOULD PRODUCE RECURRENCE RELATION DUE TO THE VARIABLE 'IEM(IM)',
C       WHICH WOULD PREVENT THE NEXT DO LOOP FROM RUNNING IN VECTOR
C       MODE.
C
              DO 200 IE = 1 , NE
                  XF=XM(IM)-X(NODE(LAPEX(1,IG),IE))
                  YF=YM(IM)-Y(NODE(LAPEX(1,IG),IE))
                  ZF=ZM(IM)-Z(NODE(LAPEX(1,IG),IE))
                  A = E(1,1,IG,IE)*XF+E(2,1,IG,IE)*YF+E(3,1,IG,IE)*ZF
                  B = E(1,2,IG,IE)*XF+E(2,2,IG,IE)*YF+E(3,2,IG,IE)*ZF
                  C = E(1,3,IG,IE)*XF+E(2,3,IG,IE)*YF+E(3,3,IG,IE)*ZF
C
                  IF(A.GE.-D .AND. B.GE.-D .AND. C.GE.-D .AND.
     &               A+B+C.LE.1.E0+D) THEN
                      IEM   (IM) = IE
                  ENDIF
  200         CONTINUE
  210     CONTINUE
          IF(IEM(IM).EQ.0) THEN
              WRITE(IUT0,6300) WRMSGA
              WRITE(IUT0,6300) WREXP1
              IWRN = 1
              WRITE(IUT0,6000) IM
          ENDIF
  220 CONTINUE
C
C FOR A PARALLEL MACHINE AND NORMAL SCALAR MACHINE
C
      ELSE
      DO 350 IE = 1 , NE
          XMIN = X(NODE(1,IE))
          YMIN = Y(NODE(1,IE))
          ZMIN = Z(NODE(1,IE))
C
          XMAX = X(NODE(1,IE))
          YMAX = Y(NODE(1,IE))
          ZMAX = Z(NODE(1,IE))
C
C*$*ASSERT DO PREFER(CONCURRENT)
          DO 310 I = 1 , N
              IF(X(NODE(I,IE)).LT.XMIN) XMIN = X(NODE(I,IE))
              IF(Y(NODE(I,IE)).LT.YMIN) YMIN = Y(NODE(I,IE))
              IF(Z(NODE(I,IE)).LT.ZMIN) ZMIN = Z(NODE(I,IE))
C 
              IF(X(NODE(I,IE)).GT.XMAX) XMAX = X(NODE(I,IE))
              IF(Y(NODE(I,IE)).GT.YMAX) YMAX = Y(NODE(I,IE))
              IF(Z(NODE(I,IE)).GT.ZMAX) ZMAX = Z(NODE(I,IE))
  310     CONTINUE
C
C*$*ASSERT DO PREFER(CONCURRENT)
          DO 330 IM = 1 , NM
              IF(IEM(IM).GE.1)   GO TO 330
C
              IF(XM(IM).LT.XMIN) GO TO 330
              IF(YM(IM).LT.YMIN) GO TO 330
              IF(ZM(IM).LT.ZMIN) GO TO 330
C
              IF(XM(IM).GT.XMAX) GO TO 330
              IF(YM(IM).GT.YMAX) GO TO 330
              IF(ZM(IM).GT.ZMAX) GO TO 330
C
              DO 320 IG = 1 , NPOLY
                  XF=XM(IM)-X(NODE(LAPEX(1,IG),IE))
                  YF=YM(IM)-Y(NODE(LAPEX(1,IG),IE))
                  ZF=ZM(IM)-Z(NODE(LAPEX(1,IG),IE))
                  A = E(1,1,IG,IE)*XF+E(2,1,IG,IE)*YF+E(3,1,IG,IE)*ZF
                  B = E(1,2,IG,IE)*XF+E(2,2,IG,IE)*YF+E(3,2,IG,IE)*ZF
                  C = E(1,3,IG,IE)*XF+E(2,3,IG,IE)*YF+E(3,3,IG,IE)*ZF
C
                  IF(A.GE.-D .AND. B.GE.-D .AND. C.GE.-D .AND.
     &               A+B+C.LE.1.E0+D) THEN
                      IEM   (IM) = IE
                  ENDIF
  320         CONTINUE
  330     CONTINUE
C
          DO 340 IM = 1 , NM
              IF(IEM(IM).EQ.0) GO TO 350
  340     CONTINUE
          GO TO 360
C
  350 CONTINUE
C
  360 CONTINUE
C
      DO 370 IM = 1 , NM
          IF(IEM(IM).EQ.0) THEN
              WRITE(IUT0,6300) WRMSGA
              WRITE(IUT0,6300) WREXP1
              IWRN = 1
              WRITE(IUT0,6000) IM
          ENDIF
  370 CONTINUE
      ENDIF
C
C
      RETURN
 6000 FORMAT(20H   *** POINT NO. ***, I7/)
 6300 FORMAT(A60)
      END
