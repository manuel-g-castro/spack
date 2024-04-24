C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    CONTOR                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE CONTOR(JPSOUT,IUTPS,IMODE,IRES,IOPT,X,Y,SR,NODE,NE,NP,
     *                  N,SRMIN,SRMAX,NCNT,XMIN,YMIN,SFC,LCL,NCL,ICLCNT)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION X(NP),Y(NP),SR(NP),NODE(N,NE),LCL(NCL)
C
#ifdef VOS
      INCLUDE (GN)
#else
      INCLUDE 'gn.h'
#endif
C
      REAL   *4 XVRTX(3),YVRTX(3),SVRTX(3),XP(2),YP(2),GPNT(4)
      INTEGER*4 MAPV (3)
      INTEGER*4 LOCAL(3,2),LOCALS(3,4),LPAIR(2,2)
      DATA LOCAL  / 1 , 2 , 3 ,
     &              3 , 4 , 1 /
      DATA LOCALS / 1 , 2 , 0 ,
     &              2 , 3 , 0 ,
     &              3 , 4 , 0 ,
     &              4 , 1 , 0 /
C
      DATA EPS / 1.0E-30 /
C
C
C      DRAW CONTOUR LINES BY MARCHING CELL ALGORITHM
C         ( 2-D GRAPHICS )
C
C
C     NOTE 1 ; CALLING GENERIC GRAPHIC AND POSTSCRIPT INTERFACES
C
C     NOTE 2 ; THIS SUBROUTINE CAN BE APPLIED TO CONTOUR DRAWING FOR
C             TRIANGLE MESH, QUADRATIC MESH, AS WELL AS COMBINATION
C             OF THOSE TWO. FOR DRAWING CONTOUR LINES ON A TRIANGLE
C             ELEMENT, THE 4TH ENTRY OF THE NODE TABLE FOR THAT
C             PARTICULAR ELEMENT SHOULD BE SET TO 0, EXCEPT FOR THE
C             CASE WHERE ALL THE ELEMENTS ARE TRIANGLE AND ARGUMENT
C             'N' IS PASSED WITH A VALUE OF 3.
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          JPSOUT      ; POSTSCRIPT FILE WILL BE OUTPUT IF SET TO 1 OR 2
C          IUTPS       ; FILE NUMBER TO ACCESS POSTSCRIPT FILE
C          IMODE       ; SPECIFIES COLOR OF CONTOUR LINES AS FOLLOWS
C                   1 --- MONOCROME COLOR SPECIFIED BY 'ICLCNT' ARGUMENT
C                   2 --- COLOR CORRESPONDING TO THE LOCAL VALUE OF THE
C                        SCALAR FIELD
C          IRES        ; SPECIFIES LOCAL CONTOUR DRAWING ALGORITHM FOR
C                       QUADRATIC MESH AS FOLLOWS
C                   1 --- DIVIDE A QUADRATIC MESH INTO TWO TRIANGLE
C                        SEGMENTS. DRAW CONTOUR LINE SEGMENTS USING
C                        LINEAR INTERPOLATION IN EACH TRIANGLE SEGMENT 
C                   2 --- DIVIDE A QUADRATIC MESH INTO FOUR TRIANGLE
C                        SEGMENTS. DRAW CONTOUR LINE SEGMENTS USING
C                        LINEAR INTERPOLATION IN EACH TRIANGLE SEGMENT 
C                   3 --- DRAW 'IRES' CONTINUEOUS LINE SEGMENTS IN THE
C                OR OVER QUADRATIC MESH USING BI-LINEAR INTERPOLATION
C
C           NOTES ; ARGUMENT 'IRES' HAS NO EFFECTS ON CONTOUR LINE
C                  DRAWING FOR TRIANGLE MESHES, FOR WHICH SIMPLE LINEAR
C                  INTERPOLATION WILL BE USED.
C
C          IOPT        ; SPECIFIES LINE OPTIONS AS FOLLOWS
C                   0 --- NORMAL DRAWING
C                  -1 --- CONTOUR LINES CORRESPONDING TO A NAGATIVE
C                        VALUE WILL BE DRAWN AS DASHED LINE
C            POSITIVE --- CONTOUR LINES WILL BE DRAWN AS BOLD LINE EVERY
C                        IOPT LINES
C          X       (IP); X-COORDINATE OF NODES
C          Y       (IP); Y-COORDINATE OF NODES
C          SR      (IP); SCALAR VALUE DEFINED AT NODES
C          NODE  (I,IE); NODE TABLE
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          NP          ; NUMBER OF TOTAL NODES
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT
C          SRMIN       ; MIN. SCALAR VALUE SPECIFIED
C          SRMAX       ; MAX. SCALAR VALUE SPECIFIED
C          NCNT        ; NUMBER OF CONTOUR LINES TO BE DRAWN
C          XMIN        ; MIN. X OF GRAPHIC RANGE SPECIFIED
C          YMIN        ; MIN. Y OF GRAPHIC RANGE SPECIFIED
C          SFC         ; GRAPHIC SCALING FACTOR
C          LCL    (ICL); COLOR INDICES USED TO DRAW CONTOUR LINES
C          NCL         ; NUMBER OF COLOR INDICES DEFINED
C          ICLCNT      ; COLOR INDEX   USED TO DRAW CONTOUR LINES
C              
C       (2) OUTPUT
C          NONE
C
C
C
C SET DEFAULT LINE COLOR
C
C
      CALL GNCSET(ICLCNT) 
      IF(JPSOUT.EQ.1) CALL PSGRAY(IUTPS,ICLCNT) 
      IF(JPSOUT.EQ.2) CALL PSCSET(IUTPS,ICLCNT) 
C
C
C SCALE FIELD DATA
C
C
      DO 10 IP = 1 , NP
          X(IP) = SFC*(X(IP)-XMIN)
          Y(IP) = SFC*(Y(IP)-YMIN)
   10 CONTINUE
C
C
C DRAW CONTOUR LINES; (1) TRIANGLE SEGMENTATION METHOD
C
C
      IF(IRES.LE.2) THEN
          DO 140 IE = 1 , NE
              IF(IRES.EQ.2) THEN
                  XG = 0.25*(X (NODE(1,IE))+X(NODE (2,IE))
     &                      +X (NODE(3,IE))+X(NODE (4,IE)))
                  YG = 0.25*(Y (NODE(1,IE))+Y(NODE (2,IE))
     &                      +Y (NODE(3,IE))+Y(NODE (4,IE)))
                  SG = 0.25*(SR(NODE(1,IE))+SR(NODE(2,IE))
     &                      +SR(NODE(3,IE))+SR(NODE(4,IE)))
              ENDIF
C
              IF(IRES.EQ.1) THEN
                  NTRI = 2
              ELSE
                  NTRI = 4
              ENDIF
              IF(N.EQ.3 .OR. NODE(4,IE).EQ.0) NTRI = 1
C
              DO 130 ITRI = 1 , NTRI
                  DO 100 I = 1 , 3
                      IF(IRES.EQ.1 .OR. NTRI.EQ.1) THEN
                          IP = NODE(LOCAL(I,ITRI),IE)
                          XVRTX(I) = X (IP)
                          YVRTX(I) = Y (IP)
                          SVRTX(I) = SR(IP)
                          MAPV (I) = NCNT*(SR(IP)-SRMIN)/(SRMAX-SRMIN)+1
                      ELSE
                          IF(I.LE.2) THEN
                              IP = NODE(LOCALS(I,ITRI),IE)
                              XVRTX(I) = X (IP)
                              YVRTX(I) = Y (IP)
                              SVRTX(I) = SR(IP)
                              MAPV (I) = NCNT*(SR(IP)-SRMIN)
     &                                   /(SRMAX-SRMIN)+1
                          ELSE
                              XVRTX(I) = XG
                              YVRTX(I) = YG
                              SVRTX(I) = SG
                              MAPV (I) = NCNT*(SG-SRMIN)/(SRMAX-SRMIN)+1
                          ENDIF
                      ENDIF
C
                      MAPV(I) = MAX(MAPV(I),     0)
                      MAPV(I) = MIN(MAPV(I),NCNT+1)
  100             CONTINUE
C
                  DO 120 ICNT = MIN(MAPV(1),MAPV(2),MAPV(3)),
     &                          MAX(MAPV(1),MAPV(2),MAPV(3))-1
C
                      SRCNT= SRMIN+FLOAT(ICNT)/FLOAT(NCNT)*(SRMAX-SRMIN)
C
C SET LINE COLOR
C
                      IF(IMODE.EQ.2) THEN
                          ICL = NCL*FLOAT(ICNT)/FLOAT(NCNT)+1
                          ICL = MIN(ICL,NCL)
                          CALL GNCSET(LCL(ICL))
                          IF(JPSOUT.EQ.1) CALL PSGRAY(IUTPS,LCL(ICL))
                          IF(JPSOUT.EQ.2) CALL PSCSET(IUTPS,LCL(ICL))
                      ENDIF
C
C SET LINE ATTRIBUTES
C
                      LNTYPE = GLSOLD
                      PLTYPE = GLSOLD
C
                      LNWDTH = GLNORM
                      PLWDTH = GLNORM
C
                      IF(IOPT.EQ.-1.AND.SRCNT.LT.0.E0) LNTYPE = GLDASH
                      IF(IOPT.EQ.-1.AND.SRCNT.LT.0.E0) PLTYPE = GLDASH
C
                      IF(IOPT.GE.1.AND.MOD(ICNT,IOPT).EQ.0)LNWDTH=GLBOLD
                      IF(IOPT.GE.1.AND.MOD(ICNT,IOPT).EQ.0)PLWDTH=GLBOLD
C
C FIND TWO INTERSECT POINTS
C
                      NPOINT = 0
                      DO 110 ILINE = 1 , 3
                          IF(NPOINT.EQ.2) GO TO 110
C
                          IVRTX1 = ILINE
                          IVRTX2 = MOD(ILINE,3)+1
C
                          IF(MAPV(IVRTX1).LE.ICNT .AND.
     &                       MAPV(IVRTX2).GT.ICNT
     &           .OR.        MAPV(IVRTX2).LE.ICNT .AND.
     &                       MAPV(IVRTX1).GT.ICNT)
     &                    THEN
                              NPOINT = NPOINT+1
                              ALF    = (SRCNT-SVRTX(IVRTX1))
     &                                /(SVRTX(IVRTX2)-SVRTX(IVRTX1))
                              XP(NPOINT) = XVRTX(IVRTX1)
     &                               +ALF*(XVRTX(IVRTX2)-XVRTX(IVRTX1))
                              YP(NPOINT) = YVRTX(IVRTX1)
     &                               +ALF*(YVRTX(IVRTX2)-YVRTX(IVRTX1))
                          ENDIF
  110                 CONTINUE
C
C DRAW LINE SEGMENT
C
                      CALL GNLINE(XP,YP,2) 
                      IF(JPSOUT.GE.1) CALL PSLINE(IUTPS,XP,YP,2) 
  120             CONTINUE
  130         CONTINUE
  140     CONTINUE
C
C
C DRAW CONTOUR LINES; (2) BI-LINEAR INTERPOLATION METHOD
C
C
      ELSE
          DO 240 ICNT = 0 , NCNT
              SRCNT = SRMIN+FLOAT(ICNT)/FLOAT(NCNT)*(SRMAX-SRMIN)
C
C SET LINE COLOR
C
              IF(IMODE.EQ.2) THEN
                  ICL = NCL*FLOAT(ICNT)/FLOAT(NCNT)+1
                  ICL = MIN(ICL,NCL)
                  CALL GNCSET(LCL(ICL))
                  IF(JPSOUT.EQ.1) CALL PSGRAY(IUTPS,LCL(ICL))
                  IF(JPSOUT.EQ.2) CALL PSCSET(IUTPS,LCL(ICL))
              ENDIF
C
C SET LINE ATTRIBUTES
C
              LNTYPE = GLSOLD
              PLTYPE = GLSOLD
C
              LNWDTH = GLNORM
              PLWDTH = GLNORM
C
              IF(IOPT.EQ.-1.AND.SRCNT.LT.0.E0) LNTYPE = GLDASH
              IF(IOPT.EQ.-1.AND.SRCNT.LT.0.E0) PLTYPE = GLDASH
C
              IF(IOPT.GE.1.AND.MOD(ICNT,IOPT).EQ.0)LNWDTH=GLBOLD
              IF(IOPT.GE.1.AND.MOD(ICNT,IOPT).EQ.0)PLWDTH=GLBOLD
C
C ENTER CELL MARCH 
C
              DO 230 IE = 1 , NE
                  SR1 = SR(NODE(1,IE))
                  SR2 = SR(NODE(2,IE))
                  SR3 = SR(NODE(3,IE))
                  SR4 = SR(NODE(4,IE))
C
                  IF(SR1.LT.SRCNT .AND. SR2.LT.SRCNT .AND.
     &               SR3.LT.SRCNT .AND. SR4.LT.SRCNT
     &          .OR. SR1.GT.SRCNT .AND. SR2.GT.SRCNT .AND.
     &               SR3.GT.SRCNT .AND. SR4.GT.SRCNT) GO TO 230
C
                  X1 = X(NODE(1,IE))
                  X2 = X(NODE(2,IE))
                  X3 = X(NODE(3,IE))
                  X4 = X(NODE(4,IE))
C
                  Y1 = Y(NODE(1,IE))
                  Y2 = Y(NODE(2,IE))
                  Y3 = Y(NODE(3,IE))
                  Y4 = Y(NODE(4,IE))
C
C FIND INTERSECT POINTS BETWEEN ISO-VALUE LINES AND ELEMENT BORDER LINES
C
                  C0 = SR1+SR3-SR2-SR4
                  C0 = C0+SIGN(EPS,C0)
                  C1 = SR2-SR1
                  C2 = SR4-SR1
                  C3 = SR1
C
                  GA = -C2/C0
                  GA = GA+SIGN(EPS,GA)
                  EA = -C1/C0
                  EA = EA+SIGN(EPS,EA)
                  C  = (C1*C2-C0*C3+C0*SRCNT)/C0**2
C
                  NPNT = 0
C
                  E = EA+C/(-GA)
                  IF(E .GE. 0.E0 .AND. E .LE. 1.E0) THEN
                      NPNT = NPNT+1
                      GPNT(NPNT) = 0.E0
                  ENDIF
C
                  E = EA+C/(1.E0-GA)
                  IF(E .GE. 0.E0 .AND. E .LE. 1.E0) THEN
                      NPNT = NPNT+1
                      GPNT(NPNT) = 1.E0
                  ENDIF
C
                  G = GA+C/(-EA)
                  IF(G .GE. 0.E0 .AND. G .LE. 1.E0) THEN
                      NPNT = NPNT+1
                      GPNT(NPNT) = G
                  ENDIF
C
                  G = GA+C/(1.E0-EA)
                  IF(G .GE. 0.E0 .AND. G .LE. 1.E0) THEN
                      NPNT = NPNT+1
                      GPNT(NPNT) = G
                  ENDIF
C
C DETERMINE PAIR(S) OF INTERSECT POINTS
C
                  IF(NPNT.LE.2) THEN
                      NPAIR = 1
                      LPAIR(1,1) = 1
                      LPAIR(2,1) = 2
                  ELSE
                      NPAIR = 2
                      IF((GPNT(1)-GA)*(GPNT(2)-GA) .GE. 0.E0) THEN
                          LPAIR(1,1) = 1
                          LPAIR(2,1) = 2
                          LPAIR(1,2) = 3
                          LPAIR(2,2) = 4
                      ELSE IF((GPNT(1)-GA)*(GPNT(3)-GA) .GE. 0.E0) THEN
                          LPAIR(1,1) = 1
                          LPAIR(2,1) = 3
                          LPAIR(1,2) = 2
                          LPAIR(2,2) = 4
                      ELSE
                          LPAIR(1,1) = 1
                          LPAIR(2,1) = 4
                          LPAIR(1,2) = 2
                          LPAIR(2,2) = 3
                      ENDIF
                  ENDIF
C
C DRAW LINE SEGMENT
C
                  DO 220 IPAIR = 1 , NPAIR
                      GSTART = GPNT(LPAIR(1,IPAIR))
                      GEND   = GPNT(LPAIR(2,IPAIR))
                      DELG   = (GEND-GSTART)/FLOAT(IRES)
C
                      GS = GSTART-DELG
                      GE = GSTART
                      DO 210 I = 1 , IRES
                          GS = GS+DELG
                          GE = GE+DELG
                          ES = EA+C/(GS-GA) 
                          EE = EA+C/(GE-GA) 
                          XP(1) = (1.E0-GS)*(1.E0-ES)*X1
     &                           +(1.E0-GS)*      ES *X4
     &                           +      GS *(1.E0-ES)*X2
     &                           +      GS *      ES *X3
                          YP(1) = (1.E0-GS)*(1.E0-ES)*Y1
     &                           +(1.E0-GS)*      ES *Y4
     &                           +      GS *(1.E0-ES)*Y2
     &                           +      GS *      ES *Y3
                          XP(2) = (1.E0-GE)*(1.E0-EE)*X1
     &                           +(1.E0-GE)*      EE *X4
     &                           +      GE *(1.E0-EE)*X2
     &                           +      GE *      EE *X3
                          YP(2) = (1.E0-GE)*(1.E0-EE)*Y1
     &                           +(1.E0-GE)*      EE *Y4
     &                           +      GE *(1.E0-EE)*Y2
     &                           +      GE *      EE *Y3
C
                          CALL GNLINE(XP,YP,2) 
                          IF(JPSOUT.GE.1) CALL PSLINE(IUTPS,XP,YP,2) 
  210                 CONTINUE
  220             CONTINUE
  230         CONTINUE
  240     CONTINUE
      ENDIF
C
C
C RESCALE FIELD DATA
C
C
      DO 300 IP = 1 , NP
          X(IP) = X(IP)/SFC+XMIN
          Y(IP) = Y(IP)/SFC+YMIN
  300 CONTINUE
C
C
      RETURN
      END
