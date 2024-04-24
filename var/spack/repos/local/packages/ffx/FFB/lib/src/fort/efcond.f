C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    EFCOND                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE EFCOND(JCON,EFD,MP,MDAT,NSET,ISET,ISR,IVR1,IVR2,
     *                  SR,VR1,VR2,X,Y,NODE,NE,NP,N,XMIN,XMAX,YMIN,YMAX,
     *                  LCOND,XX,YY)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION EFD(MP,MDAT,NSET),SR(NP),VR1(NP),VR2(NP),
     1          X(NP),Y(NP),NODE(N,NE),LCOND(NP),XX(NP),YY(NP)
C
C
C      ALLOCATES FIELD DATA WITH OR WITHOUT FIELD CONDENSATION
C         ( 2-D GRAPHICS )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT(-OUTPUT FOR JCON=1)
C          JCON        ; CONDENSATION WILL BE DONE IF THIS IS SET 1
C          EFD(IP,I,J) ; FIELD DATA DEFINED IN THE GLOBAL REGION
C          MP          ; MAX. NO. OF NODES ALLOWED
C          MDAT        ; MAX. NO. OF DATA  ALLOWED
C           NOTES : MP AND MDAT ARE THE FIRST AND SECOND ELEMENT
C                  SIZE OF ARRAY EFCOND, RESPECTIVELY.
C          NSET        ; NUMBER OF TOTAL SETS
C          ISET        ; SPECIFIES SET NUMBER              TO ALLOCATE
C          ISR         ; SPECIFIES SCALAR                  TO ALLOCATE
C          IVR1        ; SPECIFIES FIRST  VECTOR COMPONENT TO ALLOCATE
C          IVR2        ; SPECIFIES SECOND VECTOR COMPONENT TO ALLOCATE
C           NOTES : IF ANY OF THESE FOUR IS SET ZERO, THE ALLOCATION
C                  WILL NOT BE DONE.
C          X      (IP) ; X-COOR. OF NODE (OF CONDENSED FIELD FOR JCON=1)
C          Y      (IP) ; Y-COOR. OF NODE (OF CONDENSED FIELD FOR JCON=1)
C          NODE (I,IE) ; NODE TABLE      (OF CONDENSED FIELD FOR JCON=1)
C          NE          ; NUMBER OF ELEM. (OF CONDENSED FIELD FOR JCON=1)
C          NP          ; NUMBER OF NODES (OF CONDENSED FIELD FOR JCON=1)
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT
C          XMIN        ; MIN. X OF GRAPHIC RANGE SPECIFIED
C          XMAX        ; MAX. X OF GRAPHIC RANGE SPECIFIED
C          YMIN        ; MIN. Y OF GRAPHIC RANGE SPECIFIED
C          YMAX        ; MAX. Y OF GRAPHIC RANGE SPECIFIED
C
C       (2) OUTPUT
C          SR (IP)     ; ALLOCATED SCALAR
C          VR1(IP)     ; ALLOCATED FIRST  VECTOR COMPONENT
C          VR2(IP)     ; ALLOCATED SECOND VECTOR COMPONENT
C
C       (4) WORK
C          XX     (IP) ; ORIGINAL           X-DIR. COORDINATE OF NODE
C          YY     (IP) ; ORIGINAL           Y-DIR. COORDINATE OF NODE
C          LCOND  (IP) ; CORRESPONDING NODE NO. IN THE CONDENSED FIELD
C                      ( IF THERE IS NO CORRESPONDING NODE , THEN
C                       LCOND(IP) WILL BE SET TO ZERO.)
C
C
      IF(JCON.EQ.1) THEN
          DO 10 IP = 1 , NP
              LCOND(IP) = 0
              XX   (IP) = X(IP)
              YY   (IP) = Y(IP)
   10     CONTINUE
C
          NENEW = 0
          NPNEW = 0
          DO 50 IE = 1 , NE
              DO 20 I = 1 , N
                  IP = NODE(I,IE)
                  SIGX = (XX(IP)-XMIN)*(XX(IP)-XMAX)
                  SIGY = (YY(IP)-YMIN)*(YY(IP)-YMAX)
                  IF(SIGX.LE.0.0 .AND. SIGY.LE.0.0) GO TO 30
   20         CONTINUE
              GO TO 50
   30         CONTINUE
              NENEW = NENEW+1
              DO 40 I = 1 , N
                  IP = NODE(I,IE)
                  IF(LCOND(IP).EQ.0) THEN
                      NPNEW = NPNEW+1
                      LCOND(IP) = NPNEW
                      X  (NPNEW) = XX(IP)
                      Y  (NPNEW) = YY(IP)
                      IF(ISET*ISR  .NE.0) SR (NPNEW) = EFD(IP,ISR ,ISET)
                      IF(ISET*IVR1 .NE.0) VR1(NPNEW) = EFD(IP,IVR1,ISET)
                      IF(ISET*IVR2 .NE.0) VR2(NPNEW) = EFD(IP,IVR2,ISET)
                  ENDIF
                  NODE(I,NENEW) = LCOND(IP)
   40         CONTINUE
   50     CONTINUE
          NE = NENEW
          NP = NPNEW
      ELSE
          DO 60 IP = 1 , NP
              IF(ISET*ISR  .NE.0) SR (IP) = EFD(IP,ISR ,ISET)
              IF(ISET*IVR1 .NE.0) VR1(IP) = EFD(IP,IVR1,ISET)
              IF(ISET*IVR2 .NE.0) VR2(IP) = EFD(IP,IVR2,ISET)
   60     CONTINUE
      ENDIF
C
C
      RETURN
      END
