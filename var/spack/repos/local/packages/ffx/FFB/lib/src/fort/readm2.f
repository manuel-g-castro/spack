C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    READM2                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE READM2(IUTMS,FILEMS,MAXE,MAXP,N,
     *                  NE,NP,NODE,X,Y,IUT0,IERR)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION NODE(N,MAXE),X(MAXP),Y(MAXP)
C
      CHARACTER*60 FILEMS
C
      CHARACTER*72 ERMSG
     & /' *** SUBROUTINE READM2 REPORTS A FATAL ERROR OCCURENCE ***' /
      CHARACTER*72 EREXP1
     & /' NUMBER OF TOTAL ELEMENTS OR NODES HAS  EXCEEDED THE LIMIT' /
C
C
C      READ MESH DATA
C         ( 2-D CALCULATION )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          IUTMS       ; EXTERNAL DEVICE NO. SPECIFIED
C          FILEMS      ; FILE NAME TO READ MESH DATA
C          MAXE        ; THE SECOND DIMENSION OF ARRAY NODE
C          MAXP        ; THE        DIMENSION OF ARRAY X , Y
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT
C          IUT0        ; DEVICE NUMBER TO REPORT ERROR OCCURENCE
C
C       (2) OUTPUT
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          NP          ; NUMBER OF TOTAL    NODES
C          NODE(I,IE)  ; NODE NO. TABLE BASED ON ELEMENT
C          X     (IP)  ; X-COORDINATE OF EACH NODE ( ORTHOGONAL )
C          Y     (IP)  ; Y-COORDINATE OF EACH NODE ( ORTHOGONAL )
C          IERR        ; RETURN CODE TO REPORT ERROR OCCURENCE
C                   0 --- NORMAL TERMINATION
C                   1 --- A FATAL ERROR HAS OCCURED
C
C
      IERR = 0
C
#ifdef VOS
      OPEN(IUTMS,FILE=FILEMS,FORM='FORMATTED',ACTION='READ')
#else
      OPEN(IUTMS,FILE=FILEMS,FORM='FORMATTED')
#endif
C
      READ(IUTMS,500) NE , NP
C
      IF(NE.GT.MAXE .OR. NP.GT.MAXP) THEN
          WRITE(IUT0,6300) ERMSG
          WRITE(IUT0,6300) EREXP1 
          IERR = 1 
          RETURN
      ENDIF
C
      READ(IUTMS,500) ((NODE(I,IE) , I = 1 , N) , IE = 1 , NE )
      READ(IUTMS,520) ( X  (IP) , IP = 1 , NP )
      READ(IUTMS,520) ( Y  (IP) , IP = 1 , NP )
C
      CLOSE(IUTMS)
C
C
      RETURN
  500 FORMAT(14I5)
  520 FORMAT(4D20.13)
 6300 FORMAT(A72)
      END
