C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    SURFAL                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE SURFAL(NODE,IENP,NEP,MEP,NE,NP,N,LOCAL,NSP,NS,
     *                  LESURF,NESURF,MESURF,MLST,IUT0,IERR)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION NODE(N,NE),IENP(MEP,NP),NEP(NP),LOCAL(NSP,NS),
     1          LESURF(MLST,MESURF)
C
      CHARACTER*60 ERMSGB
     & /' ## SUBROUTINE SURFAL: FATAL      ERROR OCCURENCE; RETURNED' /
      CHARACTER*60 EREXP1
     & /' THE FIRST DIMENSION OF OUTPUT LIST PASSED IS NOT SUFFICIENT'/
      CHARACTER*60 EREXP2
     & /' NUMBER OF SURFACE ELEMENTS EXCEEDED LIMIT OF               '/
C
C
C      EXTRACT ALL BOUNDARY SURFACES 
C         ( 2-D , 3-D CALCULATION & GRAPHICS )
C
C
C     NOTE 1 ;  THIS SUBROUTINE IS APPLICABLE TO ANY TYPES OF ELEMENT
C              MESH.
C
C     NOTE 2 ;  ESTIMATED NUMBER OF OPERATIONS NEEDED FOR EXTRACTING
C              SURFACES ARE, 252*(NUMBER OF TOTAL ELEMENTS).
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          NODE  (I,IE); NODE NUMBER TABLE BASED ON ELEMENT
C          IENP(IEP,IP); ADJACENT ELEMENT NUMBER TO NODE IP
C          NEP     (IP); NUMBER OF ADJACENT ELEMENTS TO NODE IP
C          MEP         ; THE FIRST DIMENSION OF ARRAY IENP
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          NP          ; NUMBER OF TOTAL    NODES
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT
C
C          LOCAL (I,IS); NODE NUMBER TABLE  DEFINING ELEMENT'S SURFACES
C          NSP         ; NUMBER OF NODES    DEFINING A  SURFACE
C          NS          ; NUMBER OF SURFACES DEFINING AN ELEMENT
C
C          MESURF      ; MAXIMUM NUMBER OF SURFACES TO BE EXTRACTED
C          MLST        ; FIRST DIMENSION OF ARRAY LESURF (2 OR GREATER)
C
C          IUT0        ; FILE NUMBER TO WRITE  ERROR MESSAGE
C
C       (2) OUTPUT
C          LESURF(1,IESURF); EXTRACTED SURFACES REPRESENTED BY ELEMENT
C                       NUMBER FACING ON THE SURFACE, CONTAINED IN
C                       ARGUMENT LESURF(1,IESURF), AND THE LOCAL
C                       SURFACE NUMBER IN THE ELEMENT, CONTAINED IN
C                       ARGUMENT LESURF(2,IESURF). THE LOCAL SURFACE
C                       NUMBER IS THE SAME AS THE PASSED LOCAL SURFACE
C                       NODE LIST ARGUMENT 'LOCAL'.
C          NESURF      ; NUMBER OF SURFACE ELEMENTS EXTRACTED
C
C          IERR        ; RETURN CODE WHOSE VALUE WILL BE EITHER
C                   0 --- INDICATING SUCCESSFUL TERMINATION
C                OR 1 --- INDICATING OCCURENCE OF SOME ERROR CONDITIONS
C
C
      IERR = 0
C
C
C CHECK PASSED PARAMETERS
C
C
      IF(MLST.LT.2) THEN
          WRITE(IUT0,*) ERMSGB
          WRITE(IUT0,*) EREXP1
          IERR = 1
          RETURN
      ENDIF
C
      NESURF = 0
C
C
C EXTRACT ALL THE BOUNDARY SURFACES
C
C
      DO 150 IE = 1 , NE
          DO 140 IS = 1 , NS
              IP = NODE(LOCAL(1,IS),IE)
              DO 130 IEP = 1 , NEP(IP)
                  IEF = IENP(IEP,IP)
                  IF(IEF.EQ.IE) GO TO 130
                  DO 120 I = 2 , NSP
                      IPF = NODE(LOCAL(I,IS),IE)
                      DO 110 IEPF = 1 , NEP(IPF)
                          IEFF = IENP(IEPF,IPF)
                          IF(IEFF.EQ.IEF) GO TO 120
  110                 CONTINUE
                      GO TO 130
  120             CONTINUE
                  GO TO 140
  130         CONTINUE
C
              NESURF = NESURF+1
              IF(NESURF.GT.MESURF) THEN
                  WRITE(IUT0,*) ERMSGB
                  WRITE(IUT0,*) EREXP2, MESURF
                  IERR = 1
                  RETURN
              ENDIF
C
              LESURF(1,NESURF) = IE
              LESURF(2,NESURF) = IS
  140     CONTINUE
  150 CONTINUE
C
C
      RETURN
      END
