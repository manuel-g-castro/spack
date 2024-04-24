      SUBROUTINE LBMINI(ISTART,ICOLLI,IRFNF,
     *                  NC,NG,NG3,RHO0,VSCALE,FILEVF,
     *                  NP,CVEL,WF,NTIME0,TIME,F,FWRK,V3D,
     *                  MPBOUN,NPBOUN,LPBOUN,
     *                  MCOM,NCOMFL,NCOMST,COMFLE,COMSET,
     *                  WRK1,WRK2,WRK3,WRK4,
     *                  IUT6,IUT0,IUTFF,IERR)
      IMPLICIT NONE
C
      INTEGER*4    ISTART,ICOLLI,IRFNF,NC,NG,NG3,NTIME0,NP,
     *             MPBOUN,NPBOUN(NC),LPBOUN(5,MPBOUN,NC),
     *             IUT6,IUT0,IUTFF,IERR
      REAL*8       RHO0,VSCALE,TIME,CVEL(3,NP),WF(NP),
     *             F(0:NG+2,0:NG+2,0:NG+2,NP,NC),
     *             FWRK(NP,0:NG+2,0:NG+2,0:NG+2),
     *             V3D(4,0:NG+2,0:NG+2,0:NG+2)
      REAL*4       WRK1(NG3),WRK2(NG3),WRK3(NG3),WRK4(NG3)
      INTEGER*4    MCOM,NCOMFL,NCOMST
      CHARACTER*60 FILEVF,COMFLE(MCOM),COMSET(MCOM)
C
      INTEGER*4    IP,NC1
      REAL*8       U0,V0,W0,UU,CU,JX,JY,JZ,EE
      DATA NC1 /1/
C
      INTEGER*4 LMAT(15,15)
      DATA LMAT /  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
     *            -2,-1,-1,-1,-1,-1,-1, 1, 1, 1, 1, 1, 1, 1, 1, 
     *            16,-4,-4,-4,-4,-4,-4, 1, 1, 1, 1, 1, 1, 1, 1, 
     *             0, 1, 0,-1, 0, 0, 0, 1,-1,-1, 1, 1,-1,-1, 1, 
     *             0,-4, 0, 4, 0, 0, 0, 1,-1,-1, 1, 1,-1,-1, 1, 
     *             0, 0, 1, 0,-1, 0, 0, 1, 1,-1,-1, 1, 1,-1,-1, 
     *             0, 0,-4, 0, 4, 0, 0, 1, 1,-1,-1, 1, 1,-1,-1, 
     *             0, 0, 0, 0, 0, 1,-1, 1, 1, 1, 1,-1,-1,-1,-1, 
     *             0, 0, 0, 0, 0,-4, 4, 1, 1, 1, 1,-1,-1,-1,-1, 
     *             0, 2,-1, 2,-1,-1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 
     *             0, 0, 1, 0, 1,-1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 
     *             0, 0, 0, 0, 0, 0, 0, 1,-1, 1,-1, 1,-1, 1,-1, 
     *             0, 0, 0, 0, 0, 0, 0, 1, 1,-1,-1,-1,-1, 1, 1, 
     *             0, 0, 0, 0, 0, 0, 0, 1,-1,-1, 1,-1, 1, 1,-1, 
     *             0, 0, 0, 0, 0, 0, 0, 1,-1, 1,-1,-1, 1,-1, 1 / 
      REAL*8 SD(15)
C 
C[IN]
C     ISTART : RESTART FLAG (0:OFF, 1:ON)
C     NC     : NUMBER OF CUBES IN SUB-DOMAIN
C     NG     : CUBE SIZE (=2^N)
C     RHO0   : INITIAL DENSITY
C     FILEBF : FILE NAME OF GF-FUNC FILE
C     IUT6   : UNIT NUMBER OF STANDARD OUTPUT
C     IUT0   : UNIT NUMBER OF ERROR OUTPUT
C     IUTFF  : UNIT NUMBER OF GF-FLOW FILE 
C
C[OUT]
C     NTIME0         : CURRENT TIME STEP
C     TIME           : TIME
C     F(I,J,K,IP,IC) :PARTICLE DISTRIBUTION FUNCTION
C     IERR           : ERROR FLAG
C
C
C
      INTEGER*4 I,J,K,IC,IMODE,II
      REAL*8    FBUF,FWRK2(15)
C
      SD( 1)=1.0D0/1.5D1
      SD( 2)=1.0D0/1.8D1
      SD( 3)=1.0D0/3.6D2
      SD( 4)=1.0D0/1.0D1
      SD( 5)=1.0D0/4.0D1
      SD( 6)=1.0D0/1.0D1
      SD( 7)=1.0D0/4.0D1
      SD( 8)=1.0D0/1.0D1
      SD( 9)=1.0D0/4.0D1
      SD(10)=1.0D0/1.2D1
      SD(11)=1.0D0/4.0D1
      SD(12)=1.0D0/8.0D0
      SD(13)=1.0D0/8.0D0
      SD(14)=1.0D0/8.0D0
      SD(15)=1.0D0/8.0D0
C
      IF(ISTART.EQ.0) THEN
          NTIME0=0
          TIME=0.0D0
          U0=0.0D0
          V0=0.0D0
          W0=0.0D0
          UU=U0*U0+V0*V0+W0*W0
          JX =RHO0*U0
          JY =RHO0*V0
          JZ =RHO0*W0
          EE =JX*JX+JY*JY+JZ*JZ
          DO 1000 IC=1,NC
              DO 1100 IP=1,NP
              DO 1200 K=0,NG+2
              DO 1300 J=0,NG+2
              DO 1400 I=0,NG+2
                  IF(ICOLLI.EQ.1) THEN 
                      CU=U0*CVEL(1,IP)+V0*CVEL(2,IP)+W0*CVEL(3,IP)
                      F (I,J,K,IP,IC)=WF(IP)*RHO0*
     *                (1.0D0+3.0D0*CU+4.5D0*CU*CU-1.5D0*UU)
                  ELSE IF(ICOLLI.EQ.2) THEN
                      FWRK2( 1)= RHO0
                      FWRK2( 2)=-RHO0+EE
                      FWRK2( 3)=-RHO0
                      FWRK2( 4)= JX
                      FWRK2( 5)=-JX*7.0D0/3.0D0
                      FWRK2( 6)= JY
                      FWRK2( 7)=-JY*7.0D0/3.0D0
                      FWRK2( 8)= JZ
                      FWRK2( 9)=-JZ*7.0D0/3.0D0
                      FWRK2(10)= 2.0D0*JX*JX-JY*JY-JZ*JZ
                      FWRK2(11)=             JY*JY-JZ*JZ
                      FWRK2(12)= JX*JY
                      FWRK2(13)= JY*JZ
                      FWRK2(14)= JZ*JX
                      FWRK2(15)= 0.0D0
                      FBUF=0.0D0 
                      DO 1500 II = 1,15
                          FBUF=FBUF+SD(II)*DBLE(LMAT(IP,II))*FWRK2(II)
 1500                 CONTINUE
                      F(I,J,K,IP,IC)=FBUF
                  ENDIF
 1400         CONTINUE
 1300         CONTINUE   
 1200         CONTINUE   
 1100         CONTINUE   
 1000     CONTINUE   
      ELSE IF(ISTART.EQ.1) THEN 
          IMODE=1
          CALL GFFUNC(IMODE,IRFNF,NC,FILEVF,
     *                NG,NC,NP,NTIME0,TIME,F,FWRK,
     *                MCOM,NCOMFL,NCOMST,COMFLE,COMSET,
     *                IUT6,IUT0,IUTFF,IERR)
      ELSE IF(ISTART.EQ.2) THEN 
          IMODE=3
          CALL GFFLW3(IMODE,FILEVF,
     *                NG,NC,NP,NTIME0,CVEL,VSCALE,TIME,V3D,
     *                MPBOUN,NPBOUN,LPBOUN,
     *                MCOM,NCOMFL,NCOMST,COMFLE,COMSET,
     *                IUT6,IUT0,IUTFF,IERR,
     *                NG3,WRK1,WRK2,WRK3,WRK4)
          NTIME0=0
          TIME=0.0D0

          DO 4000 IC=1,NC
              IMODE=5
              CALL GFFLW3(IMODE,FILEVF,
     *                    NG,NC1,NP,NTIME0,CVEL,VSCALE,TIME,V3D,
     *                    MPBOUN,NPBOUN,LPBOUN,
     *                    MCOM,NCOMFL,NCOMST,COMFLE,COMSET,
     *                    IUT6,IUT0,IUTFF,IERR,
     *                    NG3,WRK1,WRK2,WRK3,WRK4)
              DO 3100 IP=1,NP
              DO 3200 K=0,NG+2
              DO 3300 J=0,NG+2
              DO 3400 I=0,NG+2
              U0=V3D(2,I,J,K)
              V0=V3D(3,I,J,K)
              W0=V3D(4,I,J,K)
              UU=U0*U0+V0*V0+W0*W0
              JX =RHO0*U0
              JY =RHO0*V0
              JZ =RHO0*W0
              EE =JX*JX+JY*JY+JZ*JZ
              IF(ICOLLI.EQ.1) THEN 
                  CU= U0*CVEL(1,IP)+V0*CVEL(2,IP)+W0*CVEL(3,IP)
                  F (I,J,K,IP,IC)=WF(IP)*RHO0*
     *            (1.0D0+3.0D0*CU+4.5D0*CU*CU-1.5D0*UU)
              ELSE IF(ICOLLI.EQ.2) THEN
                  FWRK2( 1)= RHO0
                  FWRK2( 2)=-RHO0+EE
                  FWRK2( 3)=-RHO0
                  FWRK2( 4)= JX
                  FWRK2( 5)=-JX*7.0D0/3.0D0
                  FWRK2( 6)= JY
                  FWRK2( 7)=-JY*7.0D0/3.0D0
                  FWRK2( 8)= JZ
                  FWRK2( 9)=-JZ*7.0D0/3.0D0
                  FWRK2(10)= 2.0D0*JX*JX-JY*JY-JZ*JZ
                  FWRK2(11)=             JY*JY-JZ*JZ
                  FWRK2(12)= JX*JY
                  FWRK2(13)= JY*JZ
                  FWRK2(14)= JZ*JX
                  FWRK2(15)= 0.0D0
                  FBUF=0.0D0 
                  DO 3500 II = 1,15
                      FBUF=FBUF+SD(II)*DBLE(LMAT(IP,II))*FWRK2(II)
 3500             CONTINUE
                  F(I,J,K,IP,IC)=FBUF
              ENDIF       
C             
 3400         CONTINUE
 3300         CONTINUE   
 3200         CONTINUE   
 3100         CONTINUE   
C
 4000     CONTINUE   
C
          IMODE=7
          CALL GFFLW3(IMODE,FILEVF,
     *                NG,NC1,NP,NTIME0,CVEL,VSCALE,TIME,V3D,
     *                MPBOUN,NPBOUN,LPBOUN,
     *                MCOM,NCOMFL,NCOMST,COMFLE,COMSET,
     *                IUT6,IUT0,IUTFF,IERR,
     *                NG3,WRK1,WRK2,WRK3,WRK4)
C
       ENDIF
C
      RETURN
      END
