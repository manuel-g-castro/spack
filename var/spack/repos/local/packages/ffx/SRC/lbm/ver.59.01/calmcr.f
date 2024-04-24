      SUBROUTINE CALMCR(NG,NP,CVEL,F,V3D)
      IMPLICIT NONE
      INTEGER*4 NG,NP
      REAL*8 CVEL(3,NP),
     *       F   (   0:NG+2,0:NG+2,0:NG+2,NP),
     *       V3D ( 4,0:NG+2,0:NG+2,0:NG+2)
C  
      INTEGER*4 I,J,K,IP
      REAL*8    RHO,U0,V0,W0,FBUF
C
CCCC
CCCC[1] MACRO VARIABLES
CCCC
C
      DO 1000 K =0,NG+2
      DO 1100 J =0,NG+2
      DO 1200 I =0,NG+2
          RHO=0.0D0
          U0 =0.0D0
          V0 =0.0D0
          W0 =0.0D0
          DO 1300 IP=1,NP
              FBUF=F(I,J,K,IP)
              RHO=RHO+FBUF
              U0 =U0 +FBUF*CVEL(1,IP)
              V0 =V0 +FBUF*CVEL(2,IP)
              W0 =W0 +FBUF*CVEL(3,IP)
 1300     CONTINUE
          U0=U0/RHO 
          V0=V0/RHO 
          W0=W0/RHO 
          V3D(1,I,J,K)=RHO
          V3D(2,I,J,K)=U0
          V3D(3,I,J,K)=V0
          V3D(4,I,J,K)=W0
 1200 CONTINUE
 1100 CONTINUE
 1000 CONTINUE
C
      RETURN
      END
