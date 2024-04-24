      SUBROUTINE SETTET(N1,NGAUSS,NC,PSIC,NN,PSI,W,IUT0,IERR)
      IMPLICIT NONE
      
***** PARAMETERS *****
      INTEGER N1
      
***** DEFINE ARGUMENTS *****
      INTEGER NGAUSS
      REAL*8 NC(N1)
      REAL*8 PSIC(3,N1)
      REAL*8 NN(N1,NGAUSS)
      REAL*8 PSI(3,N1,NGAUSS)
      REAL*8 W(NGAUSS)
C      
***** OBJECTS *****
      REAL*8 XI(3,NGAUSS)         ! XI, ETA, ZETA
      INTEGER IUT0,IERR      
      INTEGER IP
C
****************************
***** N, PSI AT CENTER *****
****************************
      XI(1,1)=1./4.; XI(2,1)=1./4.; XI(3,1)=1./4.
      NC(1)=1.-XI(1,1)-XI(2,1)-XI(3,1)
      NC(2)=XI(1,1)
      NC(3)=XI(2,1)
      NC(4)=XI(3,1)
C
      PSIC(1,1)=-1.
      PSIC(1,2)=1.
      PSIC(1,3)=0.
      PSIC(1,4)=0.
      
      PSIC(2,1)=-1
      PSIC(2,2)=0.
      PSIC(2,3)=1.
      PSIC(2,4)=0.
      
      PSIC(3,1)=-1.
      PSIC(3,2)=0.
      PSIC(3,3)=0.
      PSIC(3,4)=1.
C
      
      IF(NGAUSS.EQ.1) THEN
         XI(1,1)=1./4.; XI(2,1)=1./4.; XI(3,1)=1./4.
         W(1)=1./6.
         
      ELSEIF(NGAUSS.EQ.4) THEN
         XI(1,1)=0.13819660
         XI(2,1)=0.13819660
         XI(3,1)=0.13819660
         XI(1,2)=0.58541020
         XI(2,2)=0.13819660
         XI(3,2)=0.13819660
         XI(1,3)=0.13819660
         XI(2,3)=0.58541020
         XI(3,3)=0.13819660
         XI(1,4)=0.13819660
         XI(2,4)=0.13819660
         XI(3,4)=0.58541020
C
         W(1)=1./4. /6.
         W(2)=1./4. /6.
         W(3)=1./4. /6.
         W(4)=1./4. /6.
         
      ELSEIF(NGAUSS.EQ.5) THEN
         XI(1,1)=1./4.
         XI(2,1)=1./4.
         XI(3,1)=1./4.
         XI(1,2)=1./6.
         XI(2,2)=1./6.
         XI(3,2)=1./6.
         XI(1,3)=1./2.
         XI(2,3)=1./6.
         XI(3,3)=1./6.
         XI(1,4)=1./6.
         XI(2,4)=1./2.
         XI(3,4)=1./6.
         XI(1,5)=1./6.
         XI(2,5)=1./6.
         XI(3,5)=1./2.
         W(1)=-16./20. /6.
         W(2)=  9./20. /6.
         W(3)=  9./20. /6.
         W(4)=  9./20. /6.
         W(5)=  9./20. /6.
         
      ELSE
         WRITE(IUT0,*) "[ERROR]@SETUP_TET:"
         WRITE(IUT0,*) "NGAUSS=", NGAUSS, "IS NOT SUPPORTED."
         IERR=1
         RETURN
      ENDIF
      
******************
***** N, PSI *****
******************
      DO IP=1,NGAUSS
         NN(1,IP)=1.-XI(1,IP)-XI(2,IP)-XI(3,IP)
         NN(2,IP)=XI(1,IP)
         NN(3,IP)=XI(2,IP)
         NN(4,IP)=XI(3,IP)
C
         PSI(1,1,IP)=-1.0
         PSI(1,2,IP)= 1.0
         PSI(1,3,IP)= 0.0
         PSI(1,4,IP)= 0.0
      
         PSI(2,1,IP)=-1.0
         PSI(2,2,IP)= 0.0
         PSI(2,3,IP)= 1.0
         PSI(2,4,IP)= 0.0
      
         PSI(3,1,IP)=-1.0
         PSI(3,2,IP)= 0.0
         PSI(3,3,IP)= 0.0
         PSI(3,4,IP)= 1.0
      ENDDO
      
      RETURN
      END
