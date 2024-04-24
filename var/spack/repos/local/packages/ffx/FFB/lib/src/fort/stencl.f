C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    STENCL                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE STENCL(LTYPE,NE,LSTEN,MD,ND,IUT0,IERR)       
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION LTYPE(NE),LSTEN(MD)
C
      CHARACTER*60 ERMSGB
     & / ' ## SUBROUTINE STENCL: FATAL      ERROR OCCURENCE; RETURNED' /
      CHARACTER*60 EREXP1
     & / ' AN ILLEGAL ELEMENT TYPE NUMBER WAS SPECIFIED FOR IE ='      /
      CHARACTER*60 EREXP2
     & / ' NUMBER OF DIFFERENT (STENCIL) ELEMENTS EXCEEDED LIMIT OF'   /
      CHARACTER*60 EREXP3
     & / ' NO STENCIL ELEMENT IS DEFINED FOR ELEMENT TYPE NUMNER ID =' /
C
C
C      DETERMINE STENCIL ELEMENT
C         ( 3-D ; SINGLE PRECISION, REFERING ELEMENT BY TYPE LIST )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          LTYPE  (IE) ; ELEMENT TYPE SPECIFYING LIST ( ID = LTYPE(IE) )
C          NE          ; NUMBER OF TOTAL     ELEMENTS
C          MD          ; MAX. NUMBER OF DIFFERENT ELEMENTS
C          IUT0        ; FILE NUMBER TO REPORT ERROR OCCURENCE
C
C       (2) OUTPUT
C          LSTEN  (ID) ; STENCIL ELEMENT SPECIFYING LIST( IE=LSTEN(ID) )
C          ND          ; NUMBER OF DIFFERENT ELEMENTS
C          IERR        ; RETURN CODE TO REPORT ERROR OCCURENCE
C                   0 --- NORMAL TERMINATION
C                   1 --- A FATAL ERROR HAS OCCURED
C
C
      IERR = 0
C
C CLEAR STENCIL ELEMENT NUMBER
C
      DO 100 ID = 1 , MD
          LSTEN(ID) = 0
  100 CONTINUE
C
C DETERMINE STENCIL ELEMENTS
C
      ND = 0
      DO 110 IE = 1 , NE
          ID = LTYPE(IE)
          IF(ID.LE.0 .OR. ID.GT.NE) THEN
              WRITE(IUT0,*) ERMSGB
              WRITE(IUT0,*) EREXP1, IE
              IERR = 1
              RETURN
          ENDIF 
C
          ND = MAX0(ID,ND)
          IF(ND.GT.MD) THEN
              WRITE(IUT0,*) ERMSGB
              WRITE(IUT0,*) EREXP2, MD
              IERR = 1
              RETURN
          ENDIF 
          IF(LSTEN(ID).EQ.0) LSTEN(ID) = IE
  110 CONTINUE
C
C SCAN STENCIL ELEMENT SPECIFYING LIST
C
      DO 120 ID = 1 , ND
          IF(LSTEN(ID).EQ.0) THEN
              WRITE(IUT0,*) ERMSGB
              WRITE(IUT0,*) EREXP3, ID
              IERR = 1
              RETURN
          ENDIF 
  120 CONTINUE
C
C
      RETURN
      END
