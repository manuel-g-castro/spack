C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    DIRECT                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE DIRECT(MT,COMGEN,COMDAT,COMSET,NDAT,NSET)
      CHARACTER*40 COMGEN,COMDAT(NDAT),COMSET(NSET)
C
C
C      DISPLAY EULER FIELD DATA DIRECTORY
C         ( 2-D CALCULATION & GRAPHICS )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          MT          ; DEVICE NO. TO DISPLAY DIRECTRY WITH
C          COMGEN      ; COMMENT  ON THE FILE
C          COMDAT(IDAT); COMMENTS ON THE DATA STORED
C          COMSET(ISET); COMMENTS ON THE SETS STORED
C          NDAT        ; NUMBER   OF THE DATA STORED
C          NSET        ; NUMBER   OF THE SETS STORED
C
C       (2) OUTPUT
C          NONE
C
C
      WRITE(MT,6400)
      WRITE(MT,6400) COMGEN
C
      WRITE(MT,6400)
      WRITE(MT,7000) NSET
      DO 10 ISET = 1 , NSET
          WRITE(MT,7100) ISET , COMSET(ISET)
   10 CONTINUE
C
      WRITE(MT,6400)
      WRITE(MT,7200) NDAT
      DO 20 IDAT = 1 , NDAT
          WRITE(MT,7300) IDAT , COMDAT(IDAT)
   20 CONTINUE
C
C
      RETURN
 6400 FORMAT(A40)
 7000 FORMAT(' NUMBER OF SETS =' , I2)
 7100 FORMAT('    SET  NO.' , I1 , ' ; ' , A40)
 7200 FORMAT(' NUMBER OF DATA =' , I2)
 7300 FORMAT('    DATA NO.' , I1 , ' ; ' , A40)
      END
