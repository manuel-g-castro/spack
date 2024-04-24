C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    SURFSP                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE SURFSP(LPNT,NPNT,NODE,NE,NP,N,LOCAL,NSP,NS,IUT0,IERR,
     *                  LESURF,NESURF,MESURF,MLST,IPFLAG)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION LPNT(NPNT),NODE(N,NE),LOCAL(NSP,NS),
     1          LESURF(MLST,MESURF),IPFLAG(NP)
C
      CHARACTER*60 ERMSGB
     & /' ## SUBROUTINE SURFSP: FATAL      ERROR OCCURENCE; RETURNED '/
      CHARACTER*60 EREXP1
     & /' THE FIRST DIMENSION OF OUTPUT LIST PASSED IS NOT SUFFICIENT'/
      CHARACTER*60 EREXP2
     & /' THE PASSED NODAL POINT LIST CONTAINS ILLEGAL NODE NUMBER   '/
      CHARACTER*60 EREXP3
     & /' NUMBER OF EXTRACTED SURFACES EXCEEDED LIMIT OF             '/
C
C
C      EXTRACT ALL SURFACES COMPOSED OF A GIVEN GROUP OF NODAL POINTS
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          LPNT  (IPNT); SURFACE EXTRACTING NODE NUMBERS
C          NPNT        ; NUMBER OF SURFACE EXTRACTING NODES
C
C          NODE  (I,IE); NODE NUMBER TABLE BASED ON ELEMENT
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
C
C          NESURF      ; NUMBER OF SURFACE ELEMENTS EXTRACTED
C
C          IERR        ; RETURN CODE WHOSE VALUE WILL BE EITHER
C                   0 --- INDICATING SUCCESSFUL TERMINATION
C                OR 1 --- INDICATING OCCURENCE OF SOME ERROR CONDITIONS
C
C       (4) WORK
C          IPFLAG  (IP); NEEDED FOR ALL NODES
C
C
      IERR   = 0
      NESURF = 0
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
      DO 10 IPNT = 1 , NPNT
          IF(LPNT(IPNT).LT.1 .OR. LPNT(IPNT).GT.NP) THEN
              WRITE(IUT0,*) ERMSGB
              WRITE(IUT0,*) EREXP2
              WRITE(IUT0,*) 
     &        ' *** POINT NO. = ',IPNT,' *** NODE NO. = ',LPNT(IPNT)
              IERR = 1
          ENDIF
   10 CONTINUE
      IF(IERR.EQ.1) RETURN
C
C
C
C EXTRACT ALL THE SURFACES COMPOSED OF GIVEN SET OF NODES
C
C
C
      DO 20 IP = 1 , NP
          IPFLAG(IP) = 0
   20 CONTINUE
C
      DO 30 IPNT = 1 , NPNT
          IP         = LPNT(IPNT)
          IPFLAG(IP) = 1
   30 CONTINUE
C
      DO 60 IE = 1 , NE
          DO 50 IS = 1 , NS
              DO 40 I = 1 , NSP
                  IP = NODE(LOCAL(I,IS),IE)
                  IF(IPFLAG(IP).EQ.0) GO TO 50
   40         CONTINUE
C
              NESURF = NESURF+1
C
              IF(NESURF.GT.MESURF) THEN
                  WRITE(IUT0,*) ERMSGB
                  WRITE(IUT0,*) EREXP3, MESURF
                  IERR   = 1
                  RETURN
              ENDIF
C
              LESURF(1,NESURF) = IE
              LESURF(2,NESURF) = IS
   50     CONTINUE
   60 CONTINUE
C
C
      RETURN
      END
