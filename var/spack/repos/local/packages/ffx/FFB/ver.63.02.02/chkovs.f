      SUBROUTINE CHKOVS(N2,NE,NODE,NPSET,ERRMAX,
     *                  LPSET2,LPSET3,COVER1,COVER2,COVER3,IUT6)
      IMPLICIT NONE
      INTEGER*4 N2,NE,NODE(N2,NE)
      INTEGER*4 NPSET,LPSET2(NPSET),LPSET3(NPSET),IUT6
      REAL*4    ERRMAX
      REAL*4    COVER1(NPSET),COVER2(NPSET),COVER3(NPSET)
C
      INTEGER*4 IE,IBP,ISEND,IBPMAX
      REAL*4    ERR1,ERR2,ERR3,ERR,GP,EP,TP
C
      ERRMAX=0.0E0   
      IBPMAX=0
C
      DO 1000 IBP = 1 , NPSET
          ISEND = LPSET3(IBP)
          IF(ISEND.LT.0) GO TO 1000
C
          IE = LPSET2(IBP)
          GP = COVER1(IBP)
          EP = COVER2(IBP)
          TP = COVER3(IBP)
C
          IF(NODE(8,IE).NE.0) THEN
              ERR1= ABS(GP)-1.E0
              ERR2= ABS(EP)-1.E0
              ERR3= ABS(TP)-1.E0
          ELSE IF(NODE(6,IE).NE.0) THEN
              ERR1=(ABS(GP-0.5E0)-0.5E0)/2.0E0
              ERR2=(ABS(EP-0.5E0)-0.5E0)/2.0E0
              ERR3= ABS(TP)-1.E0
          ELSE IF(NODE(5,IE).NE.0) THEN
              ERR1= ABS(GP)-1.E0
              ERR2= ABS(EP)-1.E0
              ERR3=(ABS(TP-0.5E0)-0.5E0)/2.0E0
          ELSE
              ERR1=(ABS(GP-0.5E0)-0.5E0)/2.0E0
              ERR2=(ABS(EP-0.5E0)-0.5E0)/2.0E0
              ERR3=(ABS(TP-0.5E0)-0.5E0)/2.0E0
          ENDIF  
C
              ERR    = AMAX1(ERR1,ERR2,ERR3,0.E0)
              IF(ERR.GE.ERRMAX) IBPMAX=IBP
              ERRMAX = AMAX1(ERR,ERRMAX)
C
 1000 CONTINUE
C    
      WRITE(IUT6,*)
      WRITE(IUT6,*) ' CHECKING MAX. ERR IN OVERSET COEF.'
      WRITE(IUT6,*) ' MAX. ERR :' ,  ERRMAX,IBPMAX
      WRITE(IUT6,*)
C
      RETURN
      END
