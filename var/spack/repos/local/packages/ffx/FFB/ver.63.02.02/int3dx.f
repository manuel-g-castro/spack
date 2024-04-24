      SUBROUTINE INT3DX(IE0,NE,N1,N2,N,NSKIP,NGAUSS,
     *                  SNWRK0,PSI0,SNWRK,PSI,
     *                  W,X,Y,Z,NODE,
     *                  SNI,DNXI,DNYI,DNZI,SN,DNX,DNY,DNZ,DELTA,
     *                  EAP1,EAP2,EAP3,EBP,MEP,MP,NP,IENP,JENP,NEP,
     *                  IUT0,IERR)
      IMPLICIT NONE
C      
***** DEFINE ARGUMENTS *****
      INTEGER*4 IE0,NE,N1,N2,N,NSKIP,NGAUSS
      INTEGER*4 NODE
      REAL*8    X,Y,Z,SNWRK0,PSI0,SNWRK,PSI,W
      REAL*4    SNI,DNXI,DNYI,DNZI,SN,DNX,DNY,DNZ,DELTA
      REAL*4    EAP1,EAP2,EAP3,EBP
      INTEGER*4 IENP,JENP,NEP,MEP,MP,NP
      INTEGER IUT0,IERR
C
      DIMENSION X(*),Y(*),Z(*)
      DIMENSION SNWRK0(N1),PSI0(3,N1),SNWRK(N1,NGAUSS),PSI(3,N1,NGAUSS)
      DIMENSION W(NGAUSS)
      DIMENSION NODE(N2,NE),
     *          SNI(N1,NE),DNXI(N1,NE),DNYI(N1,NE),DNZI(N1,NE),
     *          SN (N1,NE),DNX (N1,NE),DNY (N1,NE),DNZ (N1,NE),
     *          DELTA(NE)
      DIMENSION EAP1(N2,MEP,NP),EAP2(3,N2,MEP,NP),EAP3(6,N2,MEP,NP)
      DIMENSION EBP(3,N2,MEP,NP)
      DIMENSION IENP(MEP,MP),JENP(MEP,MP),NEP(MP)
C
C   
C       (1) INPUT
C
C    
C       (2) OUTPUT
C          SNI   (I,IE); ELEMENT CENTER VALUE OF N
C          DNXI  (I,IE); ELEMENT CENTER VALUE OF NX
C          DNYI  (I,IE); ELEMENT CENTER VALUE OF NY
C          DNZI  (I,IE); ELEMENT CENTER VALUE OF NZ
C
C          SN    (I,IE); INTEGRATED ELEMENT VECTOR OF N
C          DNX   (I,IE); INTEGRATED ELEMENT VECTOR OF NX
C          DNY   (I,IE); INTEGRATED ELEMENT VECTOR OF NY
C          DNZ   (I,IE); INTEGRATED ELEMENT VECTOR OF NZ
C
C          IERR        ; RETURN CODE TO REPORT ERROR OCCURENCE
C                   0 --- NORMAL TERMINATION
C                   1 --- A FATAL ERROR HAS OCCURED
C
***** OBJECTS *****
      REAL*8 J11,J12,J13,J21,J22,J23,J31,J32,J33  ! DX/DXI
      REAL*8 DETJ                                 ! DET(J)
      REAL*8 VOL                                  ! VOLUME
      REAL*8 INVJ11,INVJ12,INVJ13,                ! J^-1
     *       INVJ21,INVJ22,INVJ23,                ! J^-1
     *       INVJ31,INVJ32,INVJ33                 ! J^-1
      REAL*8  PHIA1,PHIA2,PHIA3                   ! DN/DX
      REAL*8  PHIB1,PHIB2,PHIB3                   ! DX/DX 
      INTEGER*8 IN, JN, IE, IP
      REAL*4, ALLOCATABLE:: E(:,:),EX(:,:),EY(:,:),EZ(:,:),
     *                      EXX(:,:),EYY(:,:),EZZ(:,:),
     *                      EXY(:,:),EYZ(:,:),EXZ(:,:)
      INTEGER*4 I,IPE,IPE0
***** DATA *****
      
C     $     N,             ! NUMBER OF NODE PER ELEMENT
C     $     NGAUSS,        ! NUMBER OF POINT PER ELEMENT
C     $     SNWRK0,        ! SHAPE FUNCTION AT CENTER
C     $     PSI0,          ! DN/DXI, DN/DETA, DN/DZETA AT CENTER
C     $     SWRK,          ! SHAPE FUNCTION
C     $     PSI,           ! DN/DXI, DN/DETA, DN/DZETA
C     $     W,             ! WEIGHT AT POINT
C
********************************
**********ELEMENT LOOP**********
********************************
      DO 1000 IE=1,NE
**************************
***** ELEMENT CENTER *****
**************************
          J11=0.0D0
          J12=0.0D0
          J13=0.0D0
          J21=0.0D0
          J22=0.0D0
          J23=0.0D0
          J31=0.0D0
          J32=0.0D0
          J33=0.0D0
          DO 1010 IN=1,N
              J11=J11+PSI0(1,IN)*X(NODE(IN,IE))
              J21=J21+PSI0(2,IN)*X(NODE(IN,IE))
              J31=J31+PSI0(3,IN)*X(NODE(IN,IE))
              J12=J12+PSI0(1,IN)*Y(NODE(IN,IE))
              J22=J22+PSI0(2,IN)*Y(NODE(IN,IE))
              J32=J32+PSI0(3,IN)*Y(NODE(IN,IE))
              J13=J13+PSI0(1,IN)*Z(NODE(IN,IE))
              J23=J23+PSI0(2,IN)*Z(NODE(IN,IE))
              J33=J33+PSI0(3,IN)*Z(NODE(IN,IE))
 1010     CONTINUE
C
          DETJ =J11*J22*J33-J11*J23*J32
     *         +J12*J23*J31-J12*J21*J33
     *         +J13*J21*J32-J13*J22*J31
          INVJ11=(J22*J33-J23*J32)/DETJ
          INVJ12=(J13*J32-J12*J33)/DETJ
          INVJ13=(J12*J23-J13*J22)/DETJ
          INVJ21=(J23*J31-J21*J33)/DETJ
          INVJ22=(J11*J33-J13*J31)/DETJ
          INVJ23=(J13*J21-J11*J23)/DETJ
          INVJ31=(J21*J32-J22*J31)/DETJ
          INVJ32=(J12*J31-J11*J32)/DETJ
          INVJ33=(J11*J22-J12*J21)/DETJ
C
***** ADDTO *****
          DO 1020 IN=1,N
              SNI( IN,IE) = REAL(SNWRK0(IN))
              DNXI(IN,IE) = REAL( INVJ11*PSI0(1,IN)
     *                           +INVJ12*PSI0(2,IN)
     *                           +INVJ13*PSI0(3,IN))
              DNYI(IN,IE) = REAL( INVJ21*PSI0(1,IN)
     *                           +INVJ22*PSI0(2,IN)
     *                           +INVJ23*PSI0(3,IN))
              DNZI(IN,IE) = REAL( INVJ31*PSI0(1,IN)
     *                           +INVJ32*PSI0(2,IN)
     *                           +INVJ33*PSI0(3,IN))
 1020     CONTINUE
C
 1000 CONTINUE    
C
      ALLOCATE(E(N,N))
      ALLOCATE(EX(N,N))
      ALLOCATE(EY(N,N))
      ALLOCATE(EZ(N,N))
C
!$omp parallel
!$omp+default(none)
!$omp+private(J11,J12,J13)           ! subroutine local variables
!$omp+private(J21,J22,J23)
!$omp+private(J31,J32,J33)
!$omp+private(DETJ)
!$omp+private(VOL)      
!$omp+private(INVJ11,INVJ12,INVJ13)
!$omp+private(INVJ21,INVJ22,INVJ23)
!$omp+private(INVJ31,INVJ32,INVJ33)
!$omp+private(PHIA1,PHIA2,PHIA3)      
!$omp+private(PHIB1,PHIB2,PHIB3)
!$omp+private(IPE0)      
!$omp+private(E,EX,EY,EZ)            ! element local arrays
!$omp+shared(N,NGAUSS,NE)            ! argument values just only referred
!$omp+shared(JENP,IENP,NEP,NODE)     ! argument arrays just only referred
!$omp+shared(SNWRK,PSI, W)
!$omp+shared(EAP1,EAP2,SN)           ! result arrays
!$omp+shared(DNX,DNY,DNZ)
!$omp+shared(DELTA)      
!$omp+shared(IE0)      
!$omp do
      DO 2000 IE=1,NE
          E=0.0
          EX=0.0
          EY=0.0
          EZ=0.0
C
************************
***** POINT LOOP *******
************************
!ocl nosimd
!ocl noswp
          DO 2100 IP=1,NGAUSS
C
              J11=0.0D0
              J12=0.0D0
              J13=0.0D0
              J21=0.0D0
              J22=0.0D0
              J23=0.0D0
              J31=0.0D0
              J32=0.0D0
              J33=0.0D0
              DO 2110 IN=1,N
                  J11=J11+PSI(1,IN,IP)*X(NODE(IN,IE))
                  J21=J21+PSI(2,IN,IP)*X(NODE(IN,IE))
                  J31=J31+PSI(3,IN,IP)*X(NODE(IN,IE))
                  J12=J12+PSI(1,IN,IP)*Y(NODE(IN,IE))
                  J22=J22+PSI(2,IN,IP)*Y(NODE(IN,IE))
                  J32=J32+PSI(3,IN,IP)*Y(NODE(IN,IE))
                  J13=J13+PSI(1,IN,IP)*Z(NODE(IN,IE))
                  J23=J23+PSI(2,IN,IP)*Z(NODE(IN,IE))
                  J33=J33+PSI(3,IN,IP)*Z(NODE(IN,IE))
 2110         CONTINUE
C
              DETJ= J11*J22*J33-J11*J23*J32
     *             +J12*J23*J31-J12*J21*J33
     *             +J13*J21*J32-J13*J22*J31
              INVJ11=(J22*J33-J23*J32)/DETJ
              INVJ12=(J13*J32-J12*J33)/DETJ
              INVJ13=(J12*J23-J13*J22)/DETJ
              INVJ21=(J23*J31-J21*J33)/DETJ
              INVJ22=(J11*J33-J13*J31)/DETJ
              INVJ23=(J13*J21-J11*J23)/DETJ
              INVJ31=(J21*J32-J22*J31)/DETJ
              INVJ32=(J12*J31-J11*J32)/DETJ
              INVJ33=(J11*J22-J12*J21)/DETJ
              VOL =W(IP)*DETJ
C
              DELTA(IE) = DELTA(IE) +REAL(VOL)
              DO 2120 IN=1,N
                  PHIA1= INVJ11*PSI(1,IN,IP)
     *                  +INVJ12*PSI(2,IN,IP)
     *                  +INVJ13*PSI(3,IN,IP)
                  PHIA2= INVJ21*PSI(1,IN,IP)
     *                  +INVJ22*PSI(2,IN,IP)
     *                  +INVJ23*PSI(3,IN,IP)
                  PHIA3= INVJ31*PSI(1,IN,IP)
     *                  +INVJ32*PSI(2,IN,IP)
     *                  +INVJ33*PSI(3,IN,IP)
                  SN( IN,IE) = SN( IN,IE)+REAL(VOL*SNWRK(IN,IP))
                  DNX(IN,IE) = DNX(IN,IE)+REAL(VOL*PHIA1)
                  DNY(IN,IE) = DNY(IN,IE)+REAL(VOL*PHIA2)
                  DNZ(IN,IE) = DNZ(IN,IE)+REAL(VOL*PHIA3)
C
                  DO 2121 JN=1,N
                      PHIB1= INVJ11*PSI(1,JN,IP)
     *                      +INVJ12*PSI(2,JN,IP)
     *                      +INVJ13*PSI(3,JN,IP)
                      PHIB2= INVJ21*PSI(1,JN,IP)
     *                      +INVJ22*PSI(2,JN,IP)
     *                      +INVJ23*PSI(3,JN,IP)
                      PHIB3= INVJ31*PSI(1,JN,IP)
     *                      +INVJ32*PSI(2,JN,IP)
     *                      +INVJ33*PSI(3,JN,IP)
                      E  (IN,JN)=E  (IN,JN)
     *                          +REAL(VOL*SNWRK(IN,IP)*SNWRK(JN,IP))
                      EX (IN,JN)=EX (IN,JN)
     *                          +REAL(VOL*SNWRK(IN,IP)*PHIB1)
                      EY (IN,JN)=EY (IN,JN)
     *                          +REAL(VOL*SNWRK(IN,IP)*PHIB2)
                      EZ (IN,JN)=EZ (IN,JN)
     *                          +REAL(VOL*SNWRK(IN,IP)*PHIB3)
 2121             CONTINUE
 2120         CONTINUE
 2100     CONTINUE
C
          DO I=1,8
            IP = NODE(I,IE)
            IF(IP.LE.0) CYCLE
            IPE0 = 0
            DO IPE=1,NEP(IP)
              IF(IENP(IPE,IP).EQ.IE+IE0-1) THEN
                IPE0=IPE
                EXIT
              END IF
            END DO
            IN=JENP(IPE0,IP)
C
            DO JN=1,N
              EAP1(JN,IPE0,IP)   = E (IN,JN) 
              EAP2(1,JN,IPE0,IP) = EX(IN,JN)
              EAP2(2,JN,IPE0,IP) = EY(IN,JN)
              EAP2(3,JN,IPE0,IP) = EZ(IN,JN)
            END DO
          END DO
C
 2000 CONTINUE
!$OMP END DO
!$OMP END PARALLEL
C
      DEALLOCATE(E)
      DEALLOCATE(EX)
      DEALLOCATE(EY)
      DEALLOCATE(EZ)
C
      ALLOCATE(EXX(N,N))
      ALLOCATE(EYY(N,N))
      ALLOCATE(EZZ(N,N))
      ALLOCATE(EXY(N,N))
      ALLOCATE(EYZ(N,N))
      ALLOCATE(EXZ(N,N))
C
!$omp parallel
!$omp+default(none)
!$omp+private(J11,J12,J13)              ! subroutine local variables
!$omp+private(J21,J22,J23)
!$omp+private(J31,J32,J33)
!$omp+private(DETJ)
!$omp+private(VOL)      
!$omp+private(INVJ11,INVJ12,INVJ13)
!$omp+private(INVJ21,INVJ22,INVJ23)
!$omp+private(INVJ31,INVJ32,INVJ33)
!$omp+private(PHIA1,PHIA2,PHIA3)      
!$omp+private(PHIB1,PHIB2,PHIB3)
!$omp+private(IPE0)      
!$omp+private(EXX,EYY,EZZ,EXY,EYZ,EXZ)  ! element local arrays
!$omp+shared(N,NGAUSS,NE)               ! argument values just only referred
!$omp+shared(JENP,IENP,NEP,NODE)        ! argument arrays just only referred
!$omp+shared(PSI, W)
!$omp+shared(EAP3,EBP)                  ! result arrays
!$omp+shared(IE0)      
!$omp do
      DO 3000 IE=1,NE
          EXX=0.0
          EYY=0.0
          EZZ=0.0
          EXY=0.0
          EYZ=0.0
          EXZ=0.0
C
************************
***** POINT LOOP *******
************************
!ocl nosimd
!ocl noswp
          DO 3100 IP=1,NGAUSS
C
              J11=0.0D0
              J12=0.0D0
              J13=0.0D0
              J21=0.0D0
              J22=0.0D0
              J23=0.0D0
              J31=0.0D0
              J32=0.0D0
              J33=0.0D0
              DO 3110 IN=1,N
                  J11=J11+PSI(1,IN,IP)*X(NODE(IN,IE))
                  J21=J21+PSI(2,IN,IP)*X(NODE(IN,IE))
                  J31=J31+PSI(3,IN,IP)*X(NODE(IN,IE))
                  J12=J12+PSI(1,IN,IP)*Y(NODE(IN,IE))
                  J22=J22+PSI(2,IN,IP)*Y(NODE(IN,IE))
                  J32=J32+PSI(3,IN,IP)*Y(NODE(IN,IE))
                  J13=J13+PSI(1,IN,IP)*Z(NODE(IN,IE))
                  J23=J23+PSI(2,IN,IP)*Z(NODE(IN,IE))
                  J33=J33+PSI(3,IN,IP)*Z(NODE(IN,IE))
 3110         CONTINUE
C
              DETJ= J11*J22*J33-J11*J23*J32
     *             +J12*J23*J31-J12*J21*J33
     *             +J13*J21*J32-J13*J22*J31
              INVJ11=(J22*J33-J23*J32)/DETJ
              INVJ12=(J13*J32-J12*J33)/DETJ
              INVJ13=(J12*J23-J13*J22)/DETJ
              INVJ21=(J23*J31-J21*J33)/DETJ
              INVJ22=(J11*J33-J13*J31)/DETJ
              INVJ23=(J13*J21-J11*J23)/DETJ
              INVJ31=(J21*J32-J22*J31)/DETJ
              INVJ32=(J12*J31-J11*J32)/DETJ
              INVJ33=(J11*J22-J12*J21)/DETJ
              VOL =W(IP)*DETJ
C
              DO 3120 IN=1,N
                  PHIA1= REAL( INVJ11*PSI(1,IN,IP)
     *                        +INVJ12*PSI(2,IN,IP)
     *                        +INVJ13*PSI(3,IN,IP))
                  PHIA2= REAL( INVJ21*PSI(1,IN,IP)
     *                        +INVJ22*PSI(2,IN,IP)
     *                        +INVJ23*PSI(3,IN,IP))
                  PHIA3= REAL( INVJ31*PSI(1,IN,IP)
     *                        +INVJ32*PSI(2,IN,IP)
     *                        +INVJ33*PSI(3,IN,IP))
C
                  DO 3121 JN=1,N
                      PHIB1=REAL( INVJ11*PSI(1,JN,IP)
     *                           +INVJ12*PSI(2,JN,IP)
     *                           +INVJ13*PSI(3,JN,IP))
                      PHIB2=REAL( INVJ21*PSI(1,JN,IP)
     *                           +INVJ22*PSI(2,JN,IP)
     *                           +INVJ23*PSI(3,JN,IP))
                      PHIB3=REAL( INVJ31*PSI(1,JN,IP)
     *                           +INVJ32*PSI(2,JN,IP)
     *                           +INVJ33*PSI(3,JN,IP))
                      EXX(IN,JN)=EXX(IN,JN)+REAL(VOL*PHIA1*PHIB1)
                      EYY(IN,JN)=EYY(IN,JN)+REAL(VOL*PHIA2*PHIB2)
                      EZZ(IN,JN)=EZZ(IN,JN)+REAL(VOL*PHIA3*PHIB3)
                      EXY(IN,JN)=EXY(IN,JN)+REAL(VOL*PHIA1*PHIB2)
                      EYZ(IN,JN)=EYZ(IN,JN)+REAL(VOL*PHIA2*PHIB3)
                      EXZ(IN,JN)=EXZ(IN,JN)+REAL(VOL*PHIA1*PHIB3)
 3121             CONTINUE
 3120         CONTINUE
 3100     CONTINUE
C
          DO I=1,8
            IP = NODE(I,IE)
            IF(IP.LE.0) CYCLE
            IPE0 = 0
            DO IPE=1,NEP(IP)
              IF(IENP(IPE,IP).EQ.IE+IE0-1) THEN
                IPE0=IPE
                EXIT
              END IF
            END DO
            IN=JENP(IPE0,IP)
C
            DO JN=1,N
              EAP3(1,JN,IPE0,IP) = EXX(IN,JN)
              EAP3(2,JN,IPE0,IP) = EYY(IN,JN)
              EAP3(3,JN,IPE0,IP) = EZZ(IN,JN)
              EAP3(4,JN,IPE0,IP) = EXY(IN,JN)
              EAP3(5,JN,IPE0,IP) = EYZ(IN,JN)
              EAP3(6,JN,IPE0,IP) = EXZ(IN,JN)
              EBP (1,JN,IPE0,IP) = EXY(JN,IN)
              EBP (2,JN,IPE0,IP) = EYZ(JN,IN)
              EBP (3,JN,IPE0,IP) = EXZ(JN,IN)
            END DO
          END DO
C
 3000 CONTINUE
!$OMP END DO
!$OMP END PARALLEL
C
      DEALLOCATE(EXX)
      DEALLOCATE(EYY)
      DEALLOCATE(EZZ)
      DEALLOCATE(EXY)
      DEALLOCATE(EYZ)
      DEALLOCATE(EXZ)
C
      RETURN
      END
