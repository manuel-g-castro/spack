C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    SURFEX                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE SURFEX(IMODE,JBNODE,ILIST,LPNT,NPNT,
     *                  LOCAL,NODE,IENP,NEP,NE,NP,X,Y,Z,
     *                  MEP,MESURF,MPSURF,MLST,N,IUT0,IERR,LISTIP,
     *                  LESURF,NESURF,NPSURF,XSURF,YSURF,ZSURF,LPSURF)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION LPNT(NPNT),LOCAL(4,6),
     1          NODE(N,NE),IENP(MEP,NP),NEP(NP),LISTIP(NP),
     2          X(NP),Y(NP),Z(NP),
     3          LESURF(MLST,MESURF),
     4          XSURF(MPSURF),YSURF(MPSURF),ZSURF(MPSURF),LPSURF(MPSURF)
C
      CHARACTER*60 ERMSGB
     & /' ## SUBROUTINE SURFEX: FATAL      ERROR OCCURENCE; RETURNED' /
      CHARACTER*60 EREXP1
     & /' AN ILLEGAL VALUE WAS SPECIFIED FOR A CONTROL PARAMETER     '/
      CHARACTER*60 EREXP2
     & /' THE FIRST DIMENSION OF OUTPUT LIST PASSED IS NOT SUFFICIENT'/
      CHARACTER*60 EREXP3
     & /' THE PASSED NODAL POINT LIST CONTAINS ILLEGAL NODE NUMBER   '/
      CHARACTER*60 EREXP4
     & /' NUMBER OF SURFACE ELEMENTS EXCEEDED LIMIT OF               '/
      CHARACTER*60 EREXP5
     & /' NUMBER OF SURFACE NODES    EXCEEDED LIMIT OF               '/
C
C
C      EXTRACT ALL BOUNDARY SURFACES / ALL SURFACES COMPOSED OF A GIVEN
C     GROUP OF NODAL POINTS
C         ( 2-D , 3-D CALCULATION & GRAPHICS )
C
C
C     NOTE 1 ;  ESTIMATED NUMBER OF OPERATIONS NEEDED FOR EXTRACTING
C              SURFACES ARE, 252*(NUMBER OF TOTAL ELEMENTS), AND
C              11*(NUMBER OF NODES SPECIFIED)**2, FOR BOUNDARY SURFACE
C              EXTRACTION AND EXTRACTION OF SURFACES COMPOSED OF A SET
C              OF NODES, RESPECTIVELY.
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          IMODE       ; SPECIFIES THE TYPE OF SURFACES TO BE EXTRACTED
C                   1 --- ALL BOUNDARY SURFACES
C                   2 --- ALL SURFACES COMPOSED OF A GIVEN GROUP OF 
C                        NODAL POINTS
C          JBNODE      ; SPECIFIES, FOR IMODE = 2, WHETHER OR NOT ALL
C                       THE NODES SPECIFIED BY ARGUMENT 'LPNT' ARE 
C                       BOUNDARY NODES
C                   0 --- ALL THE 'LPNT' ARE NOT BOUNDARY NODES
C                   1 --- ALL THE 'LPNT' ARE     BOUNDARY NODES
C                NOTE  ; FOR IMODE = 2, IF ALL THE NODES SPECIFIED IN
C                       ARGUMENT 'LPNT' ARE BOUNDARY NODES, SET THIS 
C                       ARGUMENT TO 1. IN THIS CASE, SURFACE EXTRACTION
C                       WILL BE DONE TWICE AS FAST AS WHEN THIS ARGUMENT
C                       IS SET TO 0.
C                        FOR IMODE = 1, THIS ARGUMENT WILL BE IGNORED.
C          ILIST       ; SPECIFIES THE TYPE OF SURFACE LIST TO BE OUTPUT
C                       OUTPUT LIST 'LESURF' WILL CONTAIN THE FOLLOWING
C                       INFORMATION, DEPENDING ON THE VALUE OF 'ILIST'
C                       SHOWN ON THE LEFT
C                   1 --- ELEMENT NUMBER AND ITS LOCAL SURFACE NUMBER
C                   2 --- ELEMENT NUMBER AND ITS LOCAL NODE    NUMBERS
C                   3 --- GLOBAL  NODE NUMBERS
C                   4 --- SURFACE NODE TABLE
C                NOTE 1; SEE ARGUMENT LIST OF 'LESURF' FOR DETAIL
C                NOTE 2; FOR ILIST = 4, X, Y, AND Z COORDINATES OF ALL
C                       THE EXTRACTED SURFACE NODES WILL BE OUTPUT TO
C                       THE PASSED ARGUMENT ARRAYS 'XSURF', 'YSURF', 
C                       AND 'ZSURF'. AND GLOBAL NODE NUMBER
C                       CORRESPONDING TO SURFACE NODE NUMBER IN THE 
C                       CREATED SURFACE NODE TABLE 'LESURF' WILL BE  
C                       OUTPUT TO THE PASSED ARGUMENT 'LPSURF'.
C          LPNT  (IPNT); NODE NUMBERS    BY WHICH SURFACE EXTRACTION
C                       WILL BE DONE FOR IMODE = 2
C          NPNT        ; NUMBER OF NODES BY WHICH SURFACE EXTRACTION
C                       WILL BE DONE FOR IMODE = 2
C                NOTE  ; ARGUMENT 'LPNT(IPNT)' AND 'NPNT' MUST BE 
C                       PASSED ONLY FOR IMODE = 2.
C          LOCAL (I,IS); NODE NUMBER TABLE DEFINING ELEMENT SURFACES
C          NODE  (I,IE); NODE NUMBER TABLE BASED ON ELEMENT
C          IENP(IEP,IP); ADJACENT ELEMENT NUMBER TO NODE IP
C          NEP     (IP); NUMBER OF ADJACENT ELEMENTS TO NODE IP
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          NP          ; NUMBER OF TOTAL    NODES
C          X       (IP); X-COORDINATE OF NODE
C          Y       (IP); Y-COORDINATE OF NODE
C          Z       (IP); Z-COORDINATE OF NODE
C                NOTE  ; ARGUMENT X, Y, AND Z MUST BE PASSED ONLY FOR
C                       ILIST = 4.
C          MEP         ; THE FIRST DIMENSION OF ARRAY IENP,JENP
C          MESURF      ; THE MAXIMUM NUMBER  OF SURFACE ELEMENTS
C          MPSURF      ; THE MAXIMUM NUMBER  OF SURFACE NODES
C          MLST        ; THE FIRST DIMENSION OF ARRAY LESURF
C                NOTE  ; ARGUMENT MLST MUST BE EQUAL OR GREATER THAN
C                       2, 5, 4, OR, 4 FOR ILIST = 1, 2, 3, AND 4
C                       RESPECTIVELY.
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT
C          IUT0        ; FILE NUMBER TO WRITE  ERROR MESSAGE
C          IERR        ; RETURN CODE WHOSE VALUE WILL BE EITHER
C                   0 --- INDICATING SUCCESSFUL TERMINATION
C                OR 1 --- INDICATING OCCURENCE OF SOME ERROR CONDITIONS
C
C       (2) OUTPUT
C          LESURF(I,IESURF);CONTAINS THE FOLLOWING INFORMATION OF
C                       EXTRACTED SURFACES, DEPENDING ON THE VALUE OF
C                       'ILIST' SPECIFIED
C
C                FOR ILIST = 1, THE EXTRACTED SURFACES ARE REPRESENTED
C               BY ELEMENT NUMBER FACING ON THE SURFACE, CONTAINED IN
C               ARGUMENT LESURF(1,IESURF), AND THE LOCAL SURFACE NUMBER
C               IN THE ELEMENT, CONTAINED IN ARGUMENT LESURF(2,IESURF).
C               THE LOCAL SURFACE NUMBER IS THE SAME AS THE PASSED
C               LOCAL SURFACE NODE LIST ARGUMENT 'LOCAL'.
C
C                FOR ILIST = 2, THE EXTRACTED SURFACES ARE REPRESENTED
C               BY ELEMENT NUMBER FACING ON THE SURFACE, CONTAINED IN
C               ARGUMENT LESURF(1,IESURF), AND THE LOCAL NODE    NUMBERS
C               IN THE ELEMENT, CONTAINED IN ARGUMENT LESURF(2,IESURF)
C               THROUGH LESURF(5,IESURF).
C               THE ORDER OF LOCAL NODE NUMBERS IS BASED ON THE PASSED
C               LOCAL SURFACE NODE LIST ARGUMENT 'LOCAL'.
C
C                FOR ILIST = 3, THE EXTRACTED SURFACES ARE REPRESENTED
C               BY GLOBAL NODE NUMBERS DEFINING THE SURFACE, CONTAINED
C               IN ARGUMENT LESURF(1,IESURF) THROUGH LESURF(4,IESURF).
C               THE ORDER OF NONE NUMBERS IS BASED ON THE PASSED
C               LOCAL SURFACE NODE LIST ARGUMENT 'LOCAL'.
C
C                FOR ILIST = 4, THE EXTRACTED SURFACES ARE REPRESENTED
C               BY SURFACE NODE TABLE, CONTAINED IN ARGUMENT
C               LESURF(1,IESURF) THROUGH LESURF(4,IESURF), AND THE
C               X, Y, AND Z COORDINATES OF SURFACE NODES, CONTAINED IN
C               ARGUMENTS XSURF(IPSURF), YSURF(IPSURF), ZSURF(IPSURF).
C               THE GLOBAL NODE NUMBER CORRESPONDING TO SURFACE NODE
C               NUMBER IN THE CREATED SURFACE NODE TABLE IS INDICATED
C               BY ARGUMENT 'LPSURF(IPSURF)'.
C
C          NESURF      ; NUMBER OF SURFACE ELEMENTS EXTRACTED
C          NPSURF      ; NUMBER OF SURFACE NODES    EXTRACTED
C          XSURF (IPSURF);X-COORDINATE OF SURFACE NODE
C          YSURF (IPSURF);Y-COORDINATE OF SURFACE NODE
C          ZSURF (IPSURF);Z-COORDINATE OF SURFACE NODE
C          LPSURF(IPSURF);GLOBAL NODE NUMBER CORRESPONDING TO SURFACE
C                        NODE NUMBER 'IPSURF' IN THE CREATED SURFACE
C                        NODE TABLE 'LESURF(I,IESURF)'
C                NOTE  ; ARGUMENT NPSURF, XSURF, YSURF, ZSURF, AND
C                       LPSURF WILL BE OUTPUT ONLY FOR ILIST = 4.
C
C       (4) WORK
C          LISTIP  (IP); NEEDED FOR ALL NODES ONLY FOR ILIST = 4
C
C
      IERR = 0
C
C CHECK PASSED PARAMETERS
      IF(IMODE.NE.1 .AND. IMODE.NE.2) THEN
          WRITE(IUT0,*) ERMSGB
          WRITE(IUT0,*) EREXP1, ' IMODE'
          IERR = 1
          RETURN
      ENDIF
C
      IF(ILIST.LT.1 .OR.  ILIST.GT.4) THEN
          WRITE(IUT0,*) ERMSGB
          WRITE(IUT0,*) EREXP1, ' ILIST'
          IERR = 1
          RETURN
      ENDIF
C
      IF(ILIST.EQ.1 .AND.  MLST.LT.2 .OR.
     &   ILIST.EQ.2 .AND.  MLST.LT.5 .OR.
     &   ILIST.EQ.3 .AND.  MLST.LT.4 .OR.
     &   ILIST.EQ.4 .AND.  MLST.LT.4) THEN
          WRITE(IUT0,*) ERMSGB
          WRITE(IUT0,*) EREXP2
          IERR = 1
          RETURN
      ENDIF
C
      IF(IMODE.EQ.2) THEN
          DO 10 IPNT = 1 , NPNT
              IF(LPNT(IPNT).LT.1 .OR. LPNT(IPNT).GT.NP) THEN
                  WRITE(IUT0,*) ERMSGB
                  WRITE(IUT0,*) EREXP3
                  WRITE(IUT0,*) 
     &            ' *** POINT NO. = ',IPNT,' *** NODE NO. = ',LPNT(IPNT)
                  IERR = 1
              ENDIF
   10     CONTINUE
          IF(IERR.EQ.1) RETURN
      ENDIF
C
      NESURF = 0
      NPSURF = 0
C
C EXTRACT ALL THE BOUNDARY SURFACES
C
      IF(IMODE.EQ.1) THEN
          DO 150 IE = 1 , NE
              DO 140 IS = 1 , 6
                  IP = NODE(LOCAL(1,IS),IE)
                  DO 130 IEP = 1 , NEP(IP)
                      IEF = IENP(IEP,IP)
                      IF(IEF.EQ.IE) GO TO 130
                      DO 120 I = 2 , 4
                          IPF = NODE(LOCAL(I,IS),IE)
                          DO 110 IEPF = 1 , NEP(IPF)
                              IEFF = IENP(IEPF,IPF)
                              IF(IEFF.EQ.IEF) GO TO 120
  110                     CONTINUE
                          GO TO 130
  120                 CONTINUE
                      GO TO 140
  130             CONTINUE
C
                  NESURF = NESURF+1
                  IF(NESURF.GT.MESURF) THEN
                      WRITE(IUT0,*) ERMSGB
                      WRITE(IUT0,*) EREXP4, MESURF
                      IERR = 1
                      RETURN
                  ENDIF
C
                  LESURF(1,NESURF) = IE
                  LESURF(2,NESURF) = IS
C
  140         CONTINUE
  150     CONTINUE
      ENDIF
C
C EXTRACT ALL THE SURFACES COMPOSED OF GIVEN SET OF NODES
C
      IF(IMODE.EQ.2) THEN
          DO 280 IPNT = 1 , NPNT
              IP = LPNT(IPNT)
              DO 270 IEP = 1 , NEP(IP)
                  IE = IENP(IEP,IP)
                  IF(NODE(1,IE).LT.0) GO TO 270
                  DO 260 IS = 1 , 6
                      DO 220 I = 1 , 4
                          IPF = NODE(LOCAL(I,IS),IE)
                          DO 210 IPNTF = 1 , NPNT
                              IPFF = LPNT(IPNTF)
                              IF(IPFF.EQ.IPF) GO TO 220
  210                     CONTINUE
                          GO TO 260
  220                 CONTINUE
C
                      IF(JBNODE.NE.1) THEN
                          DO 250 IESURF = 1 , NESURF
                              IEF = LESURF(1,IESURF)
                              ISF = LESURF(2,IESURF)
                              DO 240 I = 1 , 4
                                  IPF = NODE(LOCAL(I,IS),IE)
                                  DO 230 IF = 1 , 4
                                      IPFF = NODE(LOCAL(IF,ISF),IEF)
                                      IF(IPFF.LT.0) IPFF = -IPFF
                                      IF(IPFF.EQ.IPF) GO TO 240
  230                             CONTINUE
                                  GO TO 250
  240                         CONTINUE
                              GO TO 260
  250                     CONTINUE
                      ENDIF
C
                      NESURF = NESURF+1
                      IF(NESURF.GT.MESURF) THEN
                          WRITE(IUT0,*) ERMSGB
                          WRITE(IUT0,*) EREXP4, MESURF
                          NESURF = NESURF-1
                          IERR   = 1
                          GO TO 285
                      ENDIF
C
                      LESURF(1,NESURF) = IE
                      LESURF(2,NESURF) = IS
  260             CONTINUE
C
                  NODE(1,IE) = - NODE(1,IE)
  270         CONTINUE
  280     CONTINUE
C
  285     CONTINUE
C
          DO 290 IE = 1 , NE
              IF(NODE(1,IE).LT.0) NODE(1,IE) = -NODE(1,IE)
  290     CONTINUE
          IF(IERR.EQ.1) RETURN
      ENDIF
C
C CONVERT OUTPUT LISTS
C
      IF(ILIST.EQ.4) THEN
          DO 300 IP = 1 , NP
              LISTIP(IP) = 0
  300     CONTINUE
      ENDIF
C
      DO 320 IESURF = 1 , NESURF
          IE = LESURF(1,IESURF)
          IS = LESURF(2,IESURF)
C
          DO 310 I = 1 , 4
              IN = LOCAL(I,IS)
              IP = NODE(IN,IE)
C
              IF(ILIST.EQ.2) LESURF(1+I,IESURF) = IN
              IF(ILIST.EQ.3) LESURF(  I,IESURF) = IP
C
              IF(ILIST.EQ.4) THEN
                  IF(LISTIP(IP).EQ.0) THEN
                      NPSURF = NPSURF+1
                      IF(NPSURF.GT.MPSURF) THEN
                          WRITE(IUT0,*) ERMSGB
                          WRITE(IUT0,*) EREXP5, MPSURF
                          IERR = 1
                          RETURN
                      ENDIF
                      LISTIP(IP) = NPSURF
                      LPSURF(NPSURF) = IP
                      LESURF(I,IESURF) = NPSURF
                      XSURF(NPSURF) = X(IP)
                      YSURF(NPSURF) = Y(IP)
                      ZSURF(NPSURF) = Z(IP)
                  ELSE
                      LESURF(I,IESURF) = LISTIP(IP)
                  ENDIF
              ENDIF
  310     CONTINUE
  320 CONTINUE 
C
C
      RETURN
      END
