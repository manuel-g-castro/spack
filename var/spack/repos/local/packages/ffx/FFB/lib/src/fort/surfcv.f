C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    SURFCV                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE SURFCV(X,Y,Z,LESURF,NESURF,NODE,NE,NP,N,
     *                  LOCAL,NSP,NS,XSURF,YSURF,ZSURF,LPSURF,NPSURF,
     *                  MPSURF,MLST,IUT0,IERR,LISTIP)
     *                  
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION X(NP),Y(NP),Z(NP),LESURF(MLST,NESURF),NODE(N,NE),
     1          LOCAL(NSP,NS),XSURF(MPSURF),YSURF(MPSURF),ZSURF(MPSURF),
     2          LPSURF(MPSURF),LISTIP(NP)

C
      CHARACTER*60 ERMSGB
     & /' ## SUBROUTINE SURFCV: FATAL      ERROR OCCURENCE; RETURNED' /
      CHARACTER*60 EREXP1
     & /' THE FIRST DIMENSION OF PASSED ARRAY LESURF IS INSUFFICIENT' /
      CHARACTER*60 EREXP2
     & /' NUMBER OF SURFACE NODES    EXCEEDED LIMIT OF              ' /
C
C
C      COVERT SURFACE ELEMENT LIST TO SURFACE TABLE
C         ( 2-D , 3-D CALCULATION & GRAPHICS )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          X       (IP); X-COORDINATE OF NODE
C          Y       (IP); Y-COORDINATE OF NODE
C          Z       (IP); Z-COORDINATE OF NODE
C          NESURF      ; NUMBER OF SURFACE ELEMENTS
C          NODE  (I,IE); NODE NUMBER TABLE BASED ON ELEMENT
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          NP          ; NUMBER OF TOTAL    NODES
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT
C          LOCAL (I,IS); NODE NUMBER TABLE DEFINING ELEMENT SURFACES
C          NSP         ; NUMBER OF NODES    DEFINING A  SURFACE
C          NS          ; NUMBER OF SURFACES DEFINING AN ELEMENT
C          MPSURF      ; THE MAXIMUM NUMBER  OF SURFACE NODES
C          MLST        ; THE FIRST DIMENSION OF LESURF (NSP OR GREATER)
C          IUT0        ; FILE NUMBER TO WRITE  ERROR MESSAGE
C
C       (2) OUTPUT
C          XSURF (IPSURF);X-COORDINATE OF SURFACE NODE
C          YSURF (IPSURF);Y-COORDINATE OF SURFACE NODE
C          ZSURF (IPSURF);Z-COORDINATE OF SURFACE NODE
C          LPSURF(IPSURF);GLOBAL NODE NUMBER CORRESPONDING TO SURFACE
C                        NODE NUMBER 'IPSURF' IN THE CREATED SURFACE
C                        NODE TABLE 'LESURF(I,IESURF)'
C          NPSURF      ; NUMBER OF SURFACE NODES
C
C          IERR        ; RETURN CODE WHOSE VALUE WILL BE EITHER
C                   0 --- INDICATING SUCCESSFUL TERMINATION
C                OR 1 --- INDICATING OCCURENCE OF SOME ERROR CONDITIONS
C
C       (3) INPUT-OUTPUT
C          LESURF(I,IESURF); FOR INPUT, SPECIFY ELEMENT NUMBERS FACING
C                       ON SURFACE, IN 'LESURF(1,IESURF)', AND LOCAL
C                       SURFACE NUMBER IN THE ELEMENT, ACCORDING TO THE
C                       PASSED LOCAL SURFACE NODE LIST 'LOCAL(ISP,IS)',
C                       IN 'LESURF(2,IESURF)'. 
C                            WHEN RETURNING, SURFACE NODE TABLE
C                       REPRESENTED BY 'LESURF(1,IESURF)' THROUGH
C                       'LESURF(NSP,IESURF)' WILL BE OUTPUT. X, Y, AND Z
C                       COORDINATES OF SURFACE NODES WILL BE OUTPUT IN
C                       'XSURF(IPSURF)', 'YSURF(IPSURF)', 
C                       'ZSURF(IPSURF)', AND THE GLOBAL NODE NUMBER
C                       CORRESPONDING TO SURFACE NODE 'IPSURF' IS
C                       INDICATED BY 'LPSURF(IPSURF)'.
C
C       (4) WORK
C          LISTIP  (IP); NEEDED FOR ALL NODES 'NP'
C
C
      IERR = 0
C
C
C CHECK PASSED PARAMETERS
C
C
      IF(MLST.LT.NSP) THEN
          WRITE(IUT0,*) ERMSGB
          WRITE(IUT0,*) EREXP1
          IERR = 1
          RETURN
      ENDIF
C
C
C CONVERT OUTPUT LISTS
C
C
      DO 10 IP = 1 , NP
          LISTIP(IP) = 0
   10 CONTINUE
C
      NPSURF = 0
      DO 30 IESURF = 1 , NESURF
          IE = LESURF(1,IESURF)
          IS = LESURF(2,IESURF)
          DO 20 I = 1 , NSP
              IP = NODE(LOCAL(I,IS),IE)
              IF(LISTIP(IP).EQ.0) THEN
                  NPSURF = NPSURF+1
C
                  IF(NPSURF.GT.MPSURF) THEN
                      WRITE(IUT0,*) ERMSGB
                      WRITE(IUT0,*) EREXP2, MPSURF
                      IERR = 1
                      RETURN
                  ENDIF
C
                  LISTIP(IP) = NPSURF
                  LPSURF(NPSURF) = IP
                  LESURF(I,IESURF) = NPSURF
                  XSURF(NPSURF) = X(IP)
                  YSURF(NPSURF) = Y(IP)
                  ZSURF(NPSURF) = Z(IP)
              ELSE
                  LESURF(I,IESURF) = LISTIP(IP)
              ENDIF
   20     CONTINUE
          DO 25 I = NSP+1 , MLST
              LESURF(I,IESURF) = 0
   25     CONTINUE
   30 CONTINUE 
C
C
      RETURN
      END
