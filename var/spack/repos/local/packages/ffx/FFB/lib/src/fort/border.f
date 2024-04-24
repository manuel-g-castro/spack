C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    BORDER                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE BORDER(IMODE,NODE,NE,N,MS,MB,
     *                  LSIDE,NS,LBOUN,NB,IUT0,IERR)
      DIMENSION NODE(N,NE),LSIDE(2,MS),LBOUN(MB)
C
      CHARACTER*60 ERMSG
      CHARACTER*60 EREXP1
      CHARACTER*60 EREXP2
      DATA ERMSG
     & /' *** SUBROUTINE BORDER REPORTS A FATAL ERROR OCCURENCE ***' /
      DATA EREXP1
     & /' NUMBER OF BOUNDARY SIDES HAS EXCEEDED THE LIMIT' /
      DATA EREXP2
     & /' NUMBER OF BOUNDARY NODES HAS EXCEEDED THE LIMIT' /
C
C
C      FIND BOUNDARY SIDES AND BOUNDARY NODES
C         ( 2-D CALCULATION & GRAPHICS )
C
C
C     NOTE 1 ; THIS SUBROUTINE IS AVAILABLE FOR ANY KIND OF ELEMENTS
C             ( INCLUDING A MIXTURE OF DIFFERENT KINDS OF ELEMENTS ).
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          IMODE       ; SPECIFY THE FUNCTION MODE AS FOLLOWS
C                   1 --- FIND BOUNDARY SIDES
C                   2 --- FIND BOUNDARY SIDES AND BOUNDARY NODES
C          NODE(I,IE)  ; NODE TABLE
C                       ( IF AN ELEMENT IE HAS LESS NODES THAN N,
C                        NODE(I+NNODE,IE),,NODE(N,IE) MUST HAVE BEEN
C                        SET TO ZERO, WHERE NNODE DENOTES THE NUMBER
C                        OF NODES WHICH THE ELEMENT HAS. )
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          N           ; ALIGNMENT  DIMENSION OF THE ARRAY NODE (I,IE)
C          MS          ; THE SECOND DIMENSION OF THE ARRAY LSIDE(I,IS)
C          MB          ; THE        DIMENSION OF THE ARRAY LBOUN  (IB)
C          IUT0       ; DEVICE NUMBER TO REPORT ERROR OCCURENCE
C
C       (2) OUTPUT
C          LSIDE(I,IS) ; BOUNDARY SIDES FOUND
C              ( LSIDE(1,IS),LSIDE(2,IS) CONSTITUTES ONE BOUNDARY SIDE )
C          NS          ; NUMBER OF BOUNDARY SIDES FOUND
C          LBOUN(IB)   ; BOUNDARY NODES FOUND
C          NB          ; NUMBER OF BOUNDARY NODES FOUND
C          IERR        ; RETURN CODE TO REPORT ERROR OCCURENCE
C                   0 --- NORMAL TERMINATION
C                   1 --- A FATAL ERROR HAS OCCURED
C
      IERR = 0
C
C      (1) FIND BOUNDARY SIDES
C
      NS = 0
C
      DO 600 IE = 1 , NE
          NNODE = 0
          DO 100 I = 1 , N
              IF(NODE(I,IE).NE.0) NNODE = NNODE+1
  100     CONTINUE
          DO 500 I = 1 , NNODE
              IP1 = NODE(I,IE)
              IP2 = NODE(MOD(I,NNODE)+1,IE)
              IEXIST = 0
              DO 200 IS = 1 , NS
                  IF(LSIDE(1,IS).EQ.IP1 .AND. LSIDE(2,IS).EQ.IP2   .OR.
     &               LSIDE(1,IS).EQ.IP2 .AND. LSIDE(2,IS).EQ.IP1) THEN
                     IEXIST = IS
                     GO TO 300
                  ENDIF
  200         CONTINUE
  300         CONTINUE
              IF(IEXIST.EQ.0) THEN
                  NS = NS + 1
                  IF(NS.GT.MS) THEN
                      WRITE(IUT0,*) ERMSG
                      WRITE(IUT0,*) EREXP1
                      IERR = 1
                      RETURN
                  ENDIF
                  LSIDE(1,NS) = IP1
                  LSIDE(2,NS) = IP2
              ELSE
                  DO 400 IS = IEXIST+1, NS
                      LSIDE(1,IS-1) = LSIDE(1,IS)
                      LSIDE(2,IS-1) = LSIDE(2,IS)
  400             CONTINUE
                  NS = NS-1
              ENDIF
  500     CONTINUE
  600 CONTINUE
C
C      (2) FIND BOUNDARY NODES USING LSIDE(I,IS)
C
      IF(IMODE.EQ.2) THEN
      NB = 0
      DO 1200 IS = 1 , NS
          DO 1100 I = 1 , 2
              IP = LSIDE(I,IS)
              DO 1000 IB = 1 , NB
                  IF(LBOUN(IB).EQ.IP) GO TO 1100
 1000         CONTINUE
              NB = NB + 1
              IF(NB.GT.MB) THEN
                  WRITE(IUT0,*) ERMSG
                  WRITE(IUT0,*) EREXP2
                  IERR = 1
                  RETURN
              ENDIF
              LBOUN(NB) = IP
 1100     CONTINUE
 1200 CONTINUE
      ENDIF
C
C
      RETURN
      END
