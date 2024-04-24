      SUBROUTINE ELM3BX(N1,N2,NP,X,Y,Z,NODE,NE,NEX,NSTET,NSPRD,NSWED,
     *                  NSHEX,LEB,NB,SNB,DNXB,DNYB,DNZB,IERR)
      IMPLICIT NONE
C      
***** DEFINE ARGUMENTS *****
      INTEGER N1,N2,NP
      REAL*8  X, Y, Z
      INTEGER NODE
      INTEGER NE,NEX(8)
      INTEGER LEB
      INTEGER NB,IERR
      REAL SNB, DNXB, DNYB, DNZB
      DIMENSION X(NP),Y(NP),Z(NP),NODE(N2,NE),
     *          LEB(2,NB),SNB(N1,NB),
     *          DNXB(N1,NB),DNYB(N1,NB),DNZB(N1,NB)
C      
***** OBJECTS *****
      INTEGER JHEX,JWED,JPYR,JTET
      INTEGER IB, IP, IS, EN
      INTEGER NETET,NEPRD,NEWED,NEHEX
      INTEGER  NTET, NPRD, NWED, NHEX
      INTEGER NSTET,NSPRD,NSWED,NSHEX
      REAL*8  XI1,XI2,XI3
***** *****
      REAL*8 R1D3, R2D3
      REAL*8 MR1D3, MR2D3
      REAL*8 SQRT2
      REAL*8 SQRT1D2, MSQRT1D2, SQRT1D3, MSQRT1D3
      REAL*8 SQRT3D2
      REAL*8 AW,BW
      PARAMETER (R1D3=1./3., R2D3=2./3.)           !1/3, 2/3
      PARAMETER (MR1D3=-1./3., MR2D3=-2./3.)       !1/3, 2/3
      PARAMETER (SQRT2  =1.41421356237309504880 )  !SQRT(2)
      PARAMETER (SQRT1D2=0.70710678118654752440)   !SQRT(1/2)
      PARAMETER (MSQRT1D2=-0.70710678118654752440) !SQRT(1/2)
      PARAMETER (SQRT1D3=0.57735026918962576450)   !SQRT(1/3)
      PARAMETER (MSQRT1D3=-0.57735026918962576450)
      PARAMETER (SQRT3D2=1.22474487139158904909)   !SQRT(3/2)
      PARAMETER (AW     =0.21132486540518711775)   !(-SQRT(1/3)+1)/2
      PARAMETER (BW     =0.78867513459481288225)   !(+SQRT(1/3)+1)/2
C      
***************
***** HEX *****
***************
      REAL*8 HEX_N(8,4,6)
      REAL*8 HEX_PSI(3,8,4,6)
      REAL*8 HEX_XI(3,4,6)
      REAL*8 HEX_NV(3,6)
      REAL*8 HEX_W(4,6)
      DATA HEX_XI /
     $     -1., SQRT1D3, SQRT1D3, -1., SQRT1D3,MSQRT1D3,
     $     -1.,MSQRT1D3, SQRT1D3, -1.,MSQRT1D3,MSQRT1D3, !IS=1
     $     +1., SQRT1D3, SQRT1D3, +1., SQRT1D3,MSQRT1D3,
     $     +1.,MSQRT1D3, SQRT1D3, +1.,MSQRT1D3,MSQRT1D3, !IS=2
     $      SQRT1D3,-1., SQRT1D3,  SQRT1D3,-1.,MSQRT1D3,
     $     MSQRT1D3,-1., SQRT1D3, MSQRT1D3,-1.,MSQRT1D3, !IS=3
     $      SQRT1D3,+1., SQRT1D3,  SQRT1D3,+1.,MSQRT1D3,
     $     MSQRT1D3,+1., SQRT1D3, MSQRT1D3,+1.,MSQRT1D3, !IS=4
     $      SQRT1D3, SQRT1D3,-1.,  SQRT1D3,MSQRT1D3,-1.,
     $     MSQRT1D3, SQRT1D3,-1., MSQRT1D3,MSQRT1D3,-1., !IS=5
     $      SQRT1D3, SQRT1D3,+1.,  SQRT1D3,MSQRT1D3,+1.,
     $     MSQRT1D3, SQRT1D3,+1., MSQRT1D3,MSQRT1D3,+1./ !IS=6
      DATA HEX_W /
     $     1., 1., 1., 1.,
     $     1., 1., 1., 1.,
     $     1., 1., 1., 1.,
     $     1., 1., 1., 1.,
     $     1., 1., 1., 1.,
     $     1., 1., 1., 1./
      DATA HEX_NV /
     $     -1., 0., 0.,
     $     +1., 0., 0.,
     $     0., -1., 0.,
     $     0., +1., 0.,
     $     0.,  0.,-1.,
     $     0.,  0.,+1./
      
***************
***** WED *****
***************
      REAL*8 WED_N(8,4,5)
      REAL*8 WED_PSI(3,8,4,5)
      REAL*8 WED_XI(3,4,5)
      REAL*8 WED_W(4,5)
      REAL*8 WED_NV(3,5)
      DATA WED_XI /
     $     R1D3,R1D3,-1., 0.,0.,0., 0.,0.,0., 0.,0.,0., !IS=1
     $     R1D3,R1D3,+1., 0.,0.,0., 0.,0.,0., 0.,0.,0., !IS=2
     $     AW,BW,SQRT1D3, AW,BW,MSQRT1D3,               !IS=3
     $     BW,AW,SQRT1D3, BW,AW,MSQRT1D3,               !IS=3
     $     0.,AW,SQRT1D3, 0.,AW,MSQRT1D3,               !IS=4
     $     0.,BW,SQRT1D3, 0.,BW,MSQRT1D3,               !IS=4
     $     AW,0.,SQRT1D3, AW,0.,MSQRT1D3,               !IS=5 
     $     BW,0.,SQRT1D3, BW,0.,MSQRT1D3/               !IS=5
      DATA WED_W /
     $     0.5, 0.,0.,0.,
     $     0.5, 0.,0.,0.,
     $     SQRT1D2, SQRT1D2, SQRT1D2, SQRT1D2,
     $     0.5, 0.5, 0.5, 0.5,
     $     0.5, 0.5, 0.5, 0.5/
      DATA WED_NV /
     $     0.,  0.,-1.,
     $     0.,  0.,+1.,
     $     SQRT1D2, SQRT1D2, 0.,
     $     -1., 0., 0.,
     $     0., -1., 0./

***************
***** PYR *****
***************
      REAL*8 PYR_N(8,4,5)
      REAL*8 PYR_PSI(3,8,4,5)
      REAL*8 PYR_XI(3,4,5)
      REAL*8 PYR_W(4,5)
      REAL*8 PYR_NV(3,5)
      DATA PYR_XI /
     $     MR2D3,0.,R1D3, 0.,0.,0., 0.,0.,0., 0.,0.,0., !IS=1
     $      R2D3,0.,R1D3, 0.,0.,0., 0.,0.,0., 0.,0.,0., !IS=2
     $     0.,MR2D3,R1D3, 0.,0.,0., 0.,0.,0., 0.,0.,0., !IS=3
     $     0., R2D3,R1D3, 0.,0.,0., 0.,0.,0., 0.,0.,0., !IS=4
     $      SQRT1D3, SQRT1D3,0.,  SQRT1D3,MSQRT1D3,0.,
     $     MSQRT1D3, SQRT1D3,0., MSQRT1D3,MSQRT1D3,0./ !IS=5
      DATA PYR_W /
     $     SQRT2, 0.,0.,0.,
     $     SQRT2, 0.,0.,0.,
     $     SQRT2, 0.,0.,0.,
     $     SQRT2, 0.,0.,0.,
     $     1., 1., 1., 1./      
      DATA PYR_NV /
     $     MSQRT1D2, 0.,  SQRT1D2,
     $      SQRT1D2, 0.,  SQRT1D2,
     $     0., MSQRT1D2,  SQRT1D2,
     $     0.,  SQRT1D2,  SQRT1D2,
     $     0., 0., -1./
      
***************
***** TET *****
***************
      REAL*8 TET_N(8,4,4)
      REAL*8 TET_PSI(3,8,4,4)
      REAL*8 TET_XI(3,4,4)
      REAL*8 TET_W(4,4)
      REAL*8 TET_NV(3,4)
      DATA TET_XI /
     $     R1D3,R1D3,R1D3, 0,0,0, 0,0,0, 0,0,0, !IS=1
     $     0.,  R1D3,R1D3, 0,0,0, 0,0,0, 0,0,0, !IS=2
     $     R1D3,  0.,R1D3, 0,0,0, 0,0,0, 0,0,0, !IS=3
     $     R1D3,R1D3,  0., 0,0,0, 0,0,0, 0,0,0/ !IS=4
      DATA TET_W /
     $     SQRT3D2,  0,0,0,
     $     0.5,      0,0,0,
     $     0.5,      0,0,0,
     $     0.5,      0,0,0/
      DATA TET_NV /
     $     SQRT1D3, SQRT1D3, SQRT1D3,
     $     -1., 0., 0.,
     $     0., -1., 0.,
     $     0.,  0.,-1./
C
      NETET=NEX(1)
      NEPRD=NEX(2)
      NEWED=NEX(3)
      NEHEX=NEX(4)
       NTET=NEX(5)
       NPRD=NEX(6)
       NWED=NEX(7)
       NHEX=NEX(8)
C      
      DO IS=1,NSHEX
          DO IP=1,4
              XI1=HEX_XI(1,IP,IS)
              XI2=HEX_XI(2,IP,IS)
              XI3=HEX_XI(3,IP,IS)
C
              HEX_N(1,IP,IS)=0.125*(1.-XI1)*(1.-XI2)*(1.-XI3)
              HEX_N(2,IP,IS)=0.125*(1.+XI1)*(1.-XI2)*(1.-XI3)
              HEX_N(3,IP,IS)=0.125*(1.+XI1)*(1.+XI2)*(1.-XI3)
              HEX_N(4,IP,IS)=0.125*(1.-XI1)*(1.+XI2)*(1.-XI3)
              HEX_N(5,IP,IS)=0.125*(1.-XI1)*(1.-XI2)*(1.+XI3)
              HEX_N(6,IP,IS)=0.125*(1.+XI1)*(1.-XI2)*(1.+XI3)
              HEX_N(7,IP,IS)=0.125*(1.+XI1)*(1.+XI2)*(1.+XI3)
              HEX_N(8,IP,IS)=0.125*(1.-XI1)*(1.+XI2)*(1.+XI3)
C
              HEX_PSI(1,1,IP,IS)=-0.125*(1.-XI2)*(1.-XI3)
              HEX_PSI(1,2,IP,IS)=+0.125*(1.-XI2)*(1.-XI3)
              HEX_PSI(1,3,IP,IS)=+0.125*(1.+XI2)*(1.-XI3)
              HEX_PSI(1,4,IP,IS)=-0.125*(1.+XI2)*(1.-XI3)
              HEX_PSI(1,5,IP,IS)=-0.125*(1.-XI2)*(1.+XI3)
              HEX_PSI(1,6,IP,IS)=+0.125*(1.-XI2)*(1.+XI3)
              HEX_PSI(1,7,IP,IS)=+0.125*(1.+XI2)*(1.+XI3)
              HEX_PSI(1,8,IP,IS)=-0.125*(1.+XI2)*(1.+XI3)
C      
              HEX_PSI(2,1,IP,IS)=-0.125*(1.-XI3)*(1.-XI1)
              HEX_PSI(2,2,IP,IS)=-0.125*(1.-XI3)*(1.+XI1)
              HEX_PSI(2,3,IP,IS)=+0.125*(1.-XI3)*(1.+XI1)
              HEX_PSI(2,4,IP,IS)=+0.125*(1.-XI3)*(1.-XI1)
              HEX_PSI(2,5,IP,IS)=-0.125*(1.+XI3)*(1.-XI1)
              HEX_PSI(2,6,IP,IS)=-0.125*(1.+XI3)*(1.+XI1)
              HEX_PSI(2,7,IP,IS)=+0.125*(1.+XI3)*(1.+XI1)
              HEX_PSI(2,8,IP,IS)=+0.125*(1.+XI3)*(1.-XI1)
C
              HEX_PSI(3,1,IP,IS)=-0.125*(1.-XI1)*(1.-XI2)
              HEX_PSI(3,2,IP,IS)=-0.125*(1.+XI1)*(1.-XI2)
              HEX_PSI(3,3,IP,IS)=-0.125*(1.+XI1)*(1.+XI2)
              HEX_PSI(3,4,IP,IS)=-0.125*(1.-XI1)*(1.+XI2)
              HEX_PSI(3,5,IP,IS)=+0.125*(1.-XI1)*(1.-XI2)
              HEX_PSI(3,6,IP,IS)=+0.125*(1.+XI1)*(1.-XI2)
              HEX_PSI(3,7,IP,IS)=+0.125*(1.+XI1)*(1.+XI2)
              HEX_PSI(3,8,IP,IS)=+0.125*(1.-XI1)*(1.+XI2)
         ENDDO
      ENDDO
      
      DO IS=1,NSWED
         DO IP=1,4
             XI1=WED_XI(1,IP,IS)
             XI2=WED_XI(2,IP,IS)
             XI3=WED_XI(3,IP,IS)
             WED_N(1,IP,IS)=0.5*XI1         *(1.-XI3)
             WED_N(2,IP,IS)=0.5*XI2         *(1.-XI3)
             WED_N(3,IP,IS)=0.5*(1.-XI1-XI2)*(1.-XI3)
             WED_N(4,IP,IS)=0.5*XI1         *(1.+XI3)
             WED_N(5,IP,IS)=0.5*XI2         *(1.+XI3)
             WED_N(6,IP,IS)=0.5*(1.-XI1-XI2)*(1.+XI3)
C
             WED_PSI(1,1,IP,IS)=+0.5*(1.-XI3)
             WED_PSI(1,2,IP,IS)=0.
             WED_PSI(1,3,IP,IS)=-0.5*(1.-XI3)
             WED_PSI(1,4,IP,IS)=+0.5*(1.+XI3)
             WED_PSI(1,5,IP,IS)=0.
             WED_PSI(1,6,IP,IS)=-0.5*(1.+XI3)
C      
             WED_PSI(2,1,IP,IS)=0.
             WED_PSI(2,2,IP,IS)=+0.5*(1.-XI3)
             WED_PSI(2,3,IP,IS)=-0.5*(1.-XI3)
             WED_PSI(2,4,IP,IS)=0.
             WED_PSI(2,5,IP,IS)=+0.5*(1.+XI3)
             WED_PSI(2,6,IP,IS)=-0.5*(1.+XI3)
C      
             WED_PSI(3,1,IP,IS)=-0.5*XI1
             WED_PSI(3,2,IP,IS)=-0.5*XI2
             WED_PSI(3,3,IP,IS)=-0.5*(1.-XI1-XI2)
             WED_PSI(3,4,IP,IS)=+0.5*XI1
             WED_PSI(3,5,IP,IS)=+0.5*XI2
             WED_PSI(3,6,IP,IS)=+0.5*(1.-XI1-XI2)
         ENDDO
      ENDDO
      
      DO IS=1,NSPRD
          DO IP=1,4
              XI1=PYR_XI(1,IP,IS)
              XI2=PYR_XI(2,IP,IS)
              XI3=PYR_XI(3,IP,IS)
C
              PYR_N(1,IP,IS)=
     *        0.25*((1.-XI1)*(1.-XI2)-XI3+XI1*XI2*XI3/(1.-XI3))
              PYR_N(2,IP,IS)=
     *        0.25*((1.+XI1)*(1.-XI2)-XI3-XI1*XI2*XI3/(1.-XI3))
              PYR_N(3,IP,IS)=
     *        0.25*((1.+XI1)*(1.+XI2)-XI3+XI1*XI2*XI3/(1.-XI3))
              PYR_N(4,IP,IS)=
     *        0.25*((1.-XI1)*(1.+XI2)-XI3-XI1*XI2*XI3/(1.-XI3))
              PYR_N(5,IP,IS)=XI3
C
              PYR_PSI(1,1,IP,IS)=0.25*( -1. +XI2/(1.-XI3) )
              PYR_PSI(1,2,IP,IS)=0.25*( +1. -XI2/(1.-XI3) )
              PYR_PSI(1,3,IP,IS)=0.25*( +1. +XI2/(1.-XI3) )
              PYR_PSI(1,4,IP,IS)=0.25*( -1. -XI2/(1.-XI3) )
              PYR_PSI(1,5,IP,IS)=0.
      
              PYR_PSI(2,1,IP,IS)=0.25*( -1. +XI1/(1.-XI3) )
              PYR_PSI(2,2,IP,IS)=0.25*( -1. -XI1/(1.-XI3) )
              PYR_PSI(2,3,IP,IS)=0.25*( +1. +XI1/(1.-XI3) )
              PYR_PSI(2,4,IP,IS)=0.25*( +1. -XI1/(1.-XI3) )
              PYR_PSI(2,5,IP,IS)=0.
      
              PYR_PSI(3,1,IP,IS)=0.25*( -1. +XI1*XI2/(1.-XI3)**2 )
              PYR_PSI(3,2,IP,IS)=0.25*( -1. -XI1*XI2/(1.-XI3)**2 )
              PYR_PSI(3,3,IP,IS)=0.25*( -1. +XI1*XI2/(1.-XI3)**2 )
              PYR_PSI(3,4,IP,IS)=0.25*( -1. -XI1*XI2/(1.-XI3)**2 )
              PYR_PSI(3,5,IP,IS)=1.
          ENDDO
      ENDDO

      DO IS=1,NSTET
          DO IP=1,4
              XI1=TET_XI(1,IP,IS)
              XI2=TET_XI(2,IP,IS)
              XI3=TET_XI(3,IP,IS)
              TET_N(1,IP,IS)=1.-XI1-XI2-XI3
              TET_N(2,IP,IS)=XI1
              TET_N(3,IP,IS)=XI2
              TET_N(4,IP,IS)=XI3
              TET_PSI(1,1,IP,IS)=-1.0E0
              TET_PSI(1,2,IP,IS)= 1.0E0
              TET_PSI(1,3,IP,IS)= 0.0E0
              TET_PSI(1,4,IP,IS)= 0.0E0
              TET_PSI(2,1,IP,IS)=-1.0E0
              TET_PSI(2,2,IP,IS)= 0.0E0
              TET_PSI(2,3,IP,IS)= 1.0E0
              TET_PSI(2,4,IP,IS)= 0.0E0
              TET_PSI(3,1,IP,IS)=-1.0E0
              TET_PSI(3,2,IP,IS)= 0.0E0
              TET_PSI(3,3,IP,IS)= 0.0E0
              TET_PSI(3,4,IP,IS)= 1.0E0
          ENDDO
      ENDDO

*********************
***** FACE LOOP *****
*********************
      DO IB=1,NB
***** ELEMENT NUMBER AND SURFACE NUMBER *****
         EN=LEB(1,IB)
         IS=LEB(2,IB)
         
***** DETERMIN ELEMENT TYPE *****
         JHEX=0
         JWED=0
         JPYR=0
         JTET=0
         IF( EN.LE.NETET ) THEN
            JTET=1
         ELSEIF( EN.LE.NETET+NEPRD) THEN
            JPYR=1
         ELSEIF( EN.LE.NETET+NEPRD+NEWED ) THEN
            JWED=1
         ELSEIF( EN.LE.NE ) THEN
            JHEX=1
         ELSE
            WRITE(*,*) "[ERROR]@ELM3BX: INVALID EN=", EN
            IERR=1
            RETURN
         ENDIF
CC
CC************HEX************
         IF( JHEX.EQ.1 ) THEN
            CALL INT3BX(N2,X,Y,Z,NODE,HEX_N(1,1,IS),HEX_PSI(1,1,1,IS),
     *           HEX_XI(1,1,IS),HEX_NV(1,IS),HEX_W(1,IS),NHEX, 
     *           EN,SNB(1,IB),DNXB(1,IB),DNYB(1,IB),DNZB(1,IB),IERR)
CC
CC************WED************
         ELSEIF( JWED.EQ.1 ) THEN
            CALL INT3BX(N2,X,Y,Z,NODE,WED_N(1,1,IS),WED_PSI(1,1,1,IS),
     *           WED_XI(1,1,IS),WED_NV(1,IS),WED_W(1,IS),NWED, 
     *           EN,SNB(1,IB),DNXB(1,IB),DNYB(1,IB),DNZB(1,IB),IERR)
CC
CC************PYR************
         ELSEIF( JPYR.EQ.1 ) THEN
            CALL INT3BX(N2,X,Y,Z,NODE,PYR_N(1,1,IS),PYR_PSI(1,1,1,IS),
     *           PYR_XI(1,1,IS),PYR_NV(1,IS),PYR_W(1,IS),NPRD, 
     *           EN,SNB(1,IB),DNXB(1,IB),DNYB(1,IB),DNZB(1,IB),IERR)
CC
CC************TET************
         ELSE
            CALL INT3BX(N2,X,Y,Z,NODE,TET_N(1,1,IS),TET_PSI(1,1,1,IS),
     *           TET_XI(1,1,IS),TET_NV(1,IS),TET_W(1,IS),NTET, 
     *           EN,SNB(1,IB),DNXB(1,IB),DNYB(1,IB),DNZB(1,IB),IERR)
         ENDIF
      ENDDO
C
      RETURN
      END
