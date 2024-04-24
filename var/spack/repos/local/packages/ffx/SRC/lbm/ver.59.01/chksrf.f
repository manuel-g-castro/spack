      SUBROUTINE CHKSRF(MPG,NPSURF,LSURF,LPSURF,CSURF,JSURF)
      IMPLICIT NONE
      INTEGER*4 MPG,NPSURF(2),LSURF(4,MPG,2),LPSURF(MPG,2)
      REAL*8    CSURF(3,MPG,2)
      INTEGER*4 JSURF 
C
      INTEGER*4 NP1,NP2
C
      NP1=NPSURF(1)
      NP2=NPSURF(2)
C
      IF(NP1.EQ.0 .AND. NP2.EQ.0) THEN
          JSURF=0
          RETURN
      ENDIF
C
      JSURF=1
      IF(NP1.GT.0 .AND. NP2.GT.0) RETURN 
C
      IF(NP1.EQ.0) THEN
          NPSURF(1)=1
          LSURF (1,1,1)=1
          LSURF (2,1,1)=1
          LSURF (3,1,1)=1
          LSURF (4,1,1)=1
          CSURF (1,1,1)=0.0D0
          CSURF (2,1,1)=0.0D0
          CSURF (3,1,1)=0.0D0
          LPSURF(1,1)=0
      ENDIF
C
      IF(NP2.EQ.0) THEN
          NPSURF(2)=1
          LSURF (1,1,2)=1
          LSURF (2,1,2)=1
          LSURF (3,1,2)=1
          LSURF (4,1,2)=1
          CSURF (1,1,2)=0.0D0
          CSURF (2,1,2)=0.0D0
          CSURF (3,1,2)=0.0D0
          LPSURF(  1,2)=0
      ENDIF
C
      RETURN
      END
