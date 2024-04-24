      SUBROUTINE DEB001(NG,ITIME,IMODE,IVAR,IPOSI,V3D,ISTOP,IUT6)
      IMPLICIT NONE
      INTEGER*4 NG,ITIME,IVAR,IMODE,IPOSI,ISTOP,IUT6
      REAL*8    V3D(10,0:NG+2,0:NG+2,0:NG+2)
      INTEGER*4 I,J,K
C
      WRITE(IUT6,*)
      WRITE(IUT6,*) '===ITIME===',ITIME
      IF(IVAR.EQ.1) WRITE(IUT6,*)  '===RHO==='
      IF(IVAR.EQ.2) WRITE(IUT6,*)  '=== U ==='
      IF(IVAR.EQ.3) WRITE(IUT6,*)  '=== V ==='
      IF(IVAR.EQ.4) WRITE(IUT6,*)  '=== W ==='
C
      IF(IMODE.EQ.1) THEN
         WRITE(IUT6,*) 'I=   ',IPOSI
         DO 1000 K=NG+2,0,-1
             WRITE(IUT6,'(70F6.3)') (V3D(IVAR,IPOSI,J,K),J=0,NG+2)
 1000    CONTINUE   
      ELSE IF(IMODE.EQ.2) THEN 
         WRITE(IUT6,*) 'J=   ',IPOSI
         DO 2000 K=NG+2,0,-1
             WRITE(IUT6,'(70F6.3)') (V3D(IVAR,I,IPOSI,K),I=0,NG+2)
 2000    CONTINUE   
      ELSE IF(IMODE.EQ.3) THEN
         WRITE(IUT6,*) 'K=   ',IPOSI
         DO 3000 J=NG+2,0,-1
             WRITE(IUT6,'(70F6.3)') (V3D(IVAR,I,J,IPOSI),I=0,NG+2)
 3000    CONTINUE   
      ENDIF 
C
      IF(ISTOP.EQ.1) STOP
C
      RETURN
      END
