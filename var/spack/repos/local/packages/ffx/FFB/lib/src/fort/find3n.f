C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    FIND3N                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE FIND3N(X,Y,Z,NODE,NES,NE,NP,IENE,NEE,MEE,N,
     *                  LAPEX,NAPEX,NPOLY,E,ELM,NDIM,GI,EI,TI,
     *                  XP,YP,ZP,IES,EPS,MAXITR,
     *                  GP,EP,TP,IEF,NITR,IUTWRN,IWRN)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION X(NP),Y(NP),Z(NP),NODE(N,NE),IENE(MEE,NE),NEE(NE),
     1          LAPEX(NAPEX,NPOLY),E(NDIM,NDIM,NPOLY,NE),ELM(24,NE),
     2          GI(N),EI(N),TI(N)
C
      CHARACTER*60 WRMSGA
     & / ' ## SUBROUTINE FIND3N: WARNING          ISSUING  ; CONTINUE' /
      CHARACTER*60 WREXP1
     & / ' THE INITIAL ELEMENT NUMBER IS INVALID. IT WILL BE INGORED.' /
C
C
C
C      LOCATE AN ELEMENT WHICH INCLUDES SPECIFIED POINT
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          X       (IP); X-DIR. COORDINATE OF NODE
C          Y       (IP); Y-DIR. COORDINATE OF NODE
C          Z       (IP); Z-DIR. COORDINATE OF NODE
C          NODE  (I,IE); NODE TABLE
C          NES         ; FIRST ELEMENT NUMBER
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          NP          ; NUMBER OF TOTAL    NODES
C          IENE(IEE,IE); ADJACENT ELEMENT NUMBER TO ELEMENT IE
C          NEE     (IE); NUMBER OF ADJACENT ELEMENTS TO ELEMENT IE
C          MEE         ; THE FIRST DIMENSION OF ARRAY IENE
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT
C          LAPEX(IA,IG); APEX NO. OF POLYGONS CONSTITUTING AN ELEMENT
C          NAPEX       ; NUMBER OF APEXES CONSTITUTING A POLYGON
C          NPOLY       ; NUMBER OF POLYGONS CONSTITUTING AN ELEMENT
C          E(J,I,IG,IE); INVERSE MATRICES OF ELEMENT BASE VECTOR
C          ELM   (K,IE); ELEMENT POSITION AND SHAPE DEPENDENT CONSTANTS,
C                       DEFINED IN ELEM3E AND USED IN FIND3E FOR
C                       CALCULATING LOCAL COORDINATES OF A GIVEN POINT.
C                        THIS ARRAY IS NEEDED FOR TEMPORARILY ONLY, AND
C                       DECLARED AS EQUIVALENCE TO WORK ARRAYS RX(N,ME),
C                       RY(N,ME), AND RZ(N,ME)
C           (SEE ELEM3E FOR THE MEANING OF EACH COMPONENT IN ELM(K,IE))
C          NDIM        ; THE FIRST AND SECOND DIMENSION OF ARRAY E
C          GI       (I); LOCAL GZAI  COORDINATES OF ELEMENT'S NODES
C          EI       (I); LOCAL EATA  COORDINATES OF ELEMENT'S NODES
C          TI       (I); LOCAL THETA COORDINATES OF ELEMENT'S NODES
C          XP          ; X-COORDINATE OF THE SPECIFIED POINT
C          YP          ; Y-COORDINATE OF THE SPECIFIED POINT
C          ZP          ; Z-COORDINATE OF THE SPECIFIED POINT
C          IES         ; INITIAL ELEMENT NUMBER FOR ELEMENT SEARCH
C           NOTES ; IF NO GUESS CAN BE MADE FOR THE INITIAL ELEMENT
C                  NUMBER, SET 'IES' TO ZERO.
C          EPS         ; CONGERGENCE CRITERIA  FOR LOCAL COORDINATES CAL
C          MAXITR      ; MAX. ITERATION NUMBER FOR LOCAL COORDINATES CAL
C          IUTWRN      ; FILE NUMBER TO ISSUE A WARNING MESSAGE
C           NOTES ; SET 'IUTWRN' TO A NEGATIVE INTEGER TO SUPPRESS
C                  THE WARNING MESSAGE.
C
C       (2) OUTPUT
C          GP          ; LOCAL GZAI -COORDINATE OF THE SPECIFIED POINT
C          EP          ; LOCAL EATA -COORDINATE OF THE SPECIFIED POINT
C          TP          ; LOCAL THETA-COORDINATE OF THE SPECIFIED POINT
C          IEF         ; ELEMENT WHICH INCLUDES THE SPECIFIED POINT
C           NOTES ; 'IEF' OF ZERO(0) MEANS THAT NO ELEMENT WAS FOUND
C                  THAT INCLUDES THE SPECIFIED POINT. NEGATIVE VALUE
C                  OF 'IEF' MEANS THAT ELEMENT -'IEF' WAS FOUND BUT
C                  THE COORDINATE CALCULATIONS DID NOT CONVERGE TO THE
C                  SPECIFIED CRITERIA WITHIN THE SPECIFIED ITERATIONS.
C          NITR        ; NUMBER OF ITERATIONS MADE
C          IWRN        ; RETURN CODE TO REPORT A WARNING
C                   0 --- NORMAL TERMINATION
C                   1 --- A WARNING ISSUED
C
C
C
      IWRN = 0
      D    = 0.5E0*EPS
C
C
C
C LOCATE AN ELEMENT
C
C
C
      IEF = 0
C
      IF(IES.NE.0 .AND. (IES.LT.NES .OR. IES.GT.NE)) THEN
          IF(IUTWRN.GE.0) THEN
              WRITE(IUTWRN,*) WRMSGA
              WRITE(IUTWRN,*) WREXP1
          ENDIF
          IES  = 0
          IWRN = 1
      ENDIF
C
      IF(IES.EQ.0) GO TO 35
C
C
C     SEARCH INITIAL ELEMENT
C
C
      IE  = IES
      DO 10 IG = 1 , NPOLY
          IP = NODE(LAPEX(1,IG),IE)
C
          XF = XP-X(IP)
          YF = YP-Y(IP)
          ZF = ZP-Z(IP)
          A = E(1,1,IG,IE)*XF+E(2,1,IG,IE)*YF+E(3,1,IG,IE)*ZF
          B = E(1,2,IG,IE)*XF+E(2,2,IG,IE)*YF+E(3,2,IG,IE)*ZF
          C = E(1,3,IG,IE)*XF+E(2,3,IG,IE)*YF+E(3,3,IG,IE)*ZF
C
          IF(A.GE.-D .AND. B.GE.-D .AND. C.GE.-D .AND. 
     &       A+B+C.LE.1.D0+D) IEF = IE
   10 CONTINUE
C
C
C     SEARCH ADJACENT ELEMENTS TO THE INITIAL ELEMENT
C
C
      IF(IEF.EQ.0) THEN
          DO 30 IEE = 1 , NEE(IES)
              IE = IENE(IEE,IES)
              DO 20 IG = 1 , NPOLY
                  IP = NODE(LAPEX(1,IG),IE)
C
                  XF = XP-X(IP)
                  YF = YP-Y(IP)
                  ZF = ZP-Z(IP)
                  A = E(1,1,IG,IE)*XF+E(2,1,IG,IE)*YF+E(3,1,IG,IE)*ZF
                  B = E(1,2,IG,IE)*XF+E(2,2,IG,IE)*YF+E(3,2,IG,IE)*ZF
                  C = E(1,3,IG,IE)*XF+E(2,3,IG,IE)*YF+E(3,3,IG,IE)*ZF
C
                  IF(A.GE.-D .AND. B.GE.-D .AND. C.GE.-D .AND. 
     &               A+B+C.LE.1.D0+D) IEF = IE
   20         CONTINUE
   30     CONTINUE
      ENDIF
C
C
C     SEARCH ALL ELEMENTS
C
C
   35 CONTINUE
      IF(IEF.EQ.0) THEN
          DO 50 IE = NES , NE
              DO 40 IG = 1 , NPOLY
                  IP = NODE(LAPEX(1,IG),IE)
C
                  XF = XP-X(IP)
                  YF = YP-Y(IP)
                  ZF = ZP-Z(IP)
                  A = E(1,1,IG,IE)*XF+E(2,1,IG,IE)*YF+E(3,1,IG,IE)*ZF
                  B = E(1,2,IG,IE)*XF+E(2,2,IG,IE)*YF+E(3,2,IG,IE)*ZF
                  C = E(1,3,IG,IE)*XF+E(2,3,IG,IE)*YF+E(3,3,IG,IE)*ZF
C
                  IF(A.GE.-D .AND. B.GE.-D .AND. C.GE.-D .AND. 
     &               A+B+C.LE.1.D0+D) IEF = IE
   40         CONTINUE
   50     CONTINUE
      ENDIF
      IF(IEF.EQ.0) RETURN
C
      IE = IEF
C
C
C
C CALCULATE LOCAL COORDINATES
C
C
C
      IP1 = NODE(1,IE)
      IP2 = NODE(2,IE)
      IP3 = NODE(3,IE)
      IP4 = NODE(4,IE)
      IP5 = NODE(5,IE)
      IP6 = NODE(6,IE)
      IP7 = NODE(7,IE)
      IP8 = NODE(8,IE)
C
      EA= SQRT((X(IP1)-X(IP7))**2+(Y(IP1)-Y(IP7))**2+(Z(IP1)-Z(IP7))**2)
      EB= SQRT((X(IP2)-X(IP8))**2+(Y(IP2)-Y(IP8))**2+(Z(IP2)-Z(IP8))**2)
      EC= SQRT((X(IP3)-X(IP5))**2+(Y(IP3)-Y(IP5))**2+(Z(IP3)-Z(IP5))**2)
      ED= SQRT((X(IP4)-X(IP6))**2+(Y(IP4)-Y(IP6))**2+(Z(IP4)-Z(IP6))**2)
C
      DSCRIT = EPS*AMIN1(EA,EB,EC,ED)
C
      NITR = 0
  110 CONTINUE
          IF(NITR.EQ.0) THEN
              DFG=ELM( 4,IE)
              DGG=ELM( 5,IE)
              DHG=ELM( 6,IE)
              DFE=ELM( 7,IE)
              DGE=ELM( 8,IE)
              DHE=ELM( 9,IE)
              DFT=ELM(10,IE)
              DGT=ELM(11,IE)
              DHT=ELM(12,IE)
C
              FV =ELM( 1,IE)-XP
              GV =ELM( 2,IE)-YP
              HV =ELM( 3,IE)-ZP
C
              DET = DFG*(DGE*DHT-DGT*DHE)
     &             +DFE*(DGT*DHG-DGG*DHT)
     &             +DFT*(DGG*DHE-DGE*DHG)
C
              A11 = (DGE*DHT-DGT*DHE)/DET
              A21 = (DGT*DHG-DGG*DHT)/DET
              A31 = (DGG*DHE-DGE*DHG)/DET
              A12 = (DHE*DFT-DHT*DFE)/DET
              A22 = (DHT*DFG-DHG*DFT)/DET
              A32 = (DHG*DFE-DHE*DFG)/DET
              A13 = (DFE*DGT-DFT*DGE)/DET
              A23 = (DFT*DGG-DFG*DGT)/DET
              A33 = (DFG*DGE-DFE*DGG)/DET
C
              GP = -A11*FV-A12*GV-A13*HV
              EP = -A21*FV-A22*GV-A23*HV
              TP = -A31*FV-A32*GV-A33*HV
          ELSE
             DFG=ELM( 4,IE)+ELM(13,IE)*EP+ELM(19,IE)*TP+ELM(22,IE)*EP*TP
             DGG=ELM( 5,IE)+ELM(14,IE)*EP+ELM(20,IE)*TP+ELM(23,IE)*EP*TP
             DHG=ELM( 6,IE)+ELM(15,IE)*EP+ELM(21,IE)*TP+ELM(24,IE)*EP*TP
             DFE=ELM( 7,IE)+ELM(16,IE)*TP+ELM(13,IE)*GP+ELM(22,IE)*TP*GP
             DGE=ELM( 8,IE)+ELM(17,IE)*TP+ELM(14,IE)*GP+ELM(23,IE)*TP*GP
             DHE=ELM( 9,IE)+ELM(18,IE)*TP+ELM(15,IE)*GP+ELM(24,IE)*TP*GP
             DFT=ELM(10,IE)+ELM(19,IE)*GP+ELM(16,IE)*EP+ELM(22,IE)*GP*EP
             DGT=ELM(11,IE)+ELM(20,IE)*GP+ELM(17,IE)*EP+ELM(23,IE)*GP*EP
             DHT=ELM(12,IE)+ELM(21,IE)*GP+ELM(18,IE)*EP+ELM(24,IE)*GP*EP
C
             FV =ELM( 1,IE)-XP+ELM( 4,IE)*GP+ELM( 7,IE)*EP+ELM(10,IE)*TP
     &          +ELM(13,IE)*GP*EP+ELM(16,IE)*EP*TP+ELM(19,IE)*TP*GP
     &          +ELM(22,IE)*GP*EP*TP
             GV =ELM( 2,IE)-YP+ELM( 5,IE)*GP+ELM( 8,IE)*EP+ELM(11,IE)*TP
     &          +ELM(14,IE)*GP*EP+ELM(17,IE)*EP*TP+ELM(20,IE)*TP*GP
     &          +ELM(23,IE)*GP*EP*TP
             HV =ELM( 3,IE)-ZP+ELM( 6,IE)*GP+ELM( 9,IE)*EP+ELM(12,IE)*TP
     &          +ELM(15,IE)*GP*EP+ELM(18,IE)*EP*TP+ELM(21,IE)*TP*GP
     &          +ELM(24,IE)*GP*EP*TP
C
             DET = DFG*(DGE*DHT-DGT*DHE)
     &            +DFE*(DGT*DHG-DGG*DHT)
     &            +DFT*(DGG*DHE-DGE*DHG)
C
             A11 = (DGE*DHT-DGT*DHE)/DET
             A21 = (DGT*DHG-DGG*DHT)/DET
             A31 = (DGG*DHE-DGE*DHG)/DET
             A12 = (DHE*DFT-DHT*DFE)/DET
             A22 = (DHT*DFG-DHG*DFT)/DET
             A32 = (DHG*DFE-DHE*DFG)/DET
             A13 = (DFE*DGT-DFT*DGE)/DET
             A23 = (DFT*DGG-DFG*DGT)/DET
             A33 = (DFG*DGE-DFE*DGG)/DET
C
             GP = GP-A11*FV-A12*GV-A13*HV
             EP = EP-A21*FV-A22*GV-A23*HV
             TP = TP-A31*FV-A32*GV-A33*HV
          ENDIF
C
          XS = 0.E0
          YS = 0.E0
          ZS = 0.E0
          DO 120 I = 1 , N
              S  = 0.125*(1.E0+GI(I)*GP)*(1.E0+EI(I)*EP)*(1.E0+TI(I)*TP)
              XS = XS+S*X(NODE(I,IE))
              YS = YS+S*Y(NODE(I,IE))
              ZS = ZS+S*Z(NODE(I,IE))
  120     CONTINUE
          DS = SQRT((XS-XP)**2+(YS-YP)**2+(YS-YP)**2)
      IF(DS.GT.DSCRIT) THEN
          NITR = NITR+1
          IF(NITR.LE.MAXITR) THEN
              GO TO 110
          ELSE
              IEF = -IEF
          ENDIF
      ENDIF
C
C
      RETURN
      END
