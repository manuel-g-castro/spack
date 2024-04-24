      SUBROUTINE ALE_CALMOV
     *                 (MCOLOR,MCPART,NCOLOR,NCPART,LLOOP,
     *                  NEX,EPS,EPSRE,NMAX,X,Y,Z,
     *                  MELM,EXX,EYY,EZZ,EXY,EXZ,EYZ,
     *                  NODE,ME,NE,NP,EJ,DT,
     *                  NCRS,N1,N2,LTAB,NPP,IPCRS,AP,
     *                  NPMVB,LPMVB,UMVB,VMVB,WMVB,
     *                  UMESH,VMESH,WMESH,
     *                  UMESH_P,VMESH_P,WMESH_P,ITIME,ISTART,
     *                  IPART,LDOM,NBPDOM,NDOM,
     *                  IPSLF,IPSND,MBPDOM,NUMIP,
     *                  NITR,RES,RX,RY,WRK01,AWRK01,AWRK02,
     *                  AWRK03,AWRK04,AWRK05,AWRK06,RHSF,
     *                  DISP,AR,AWRK07,LFIXX,LFIXY,LFIXZ,
     *                  MRSALE,IALEDB,IUTAL,IUT0,IUT6,IERR)
C
      IMPLICIT NONE
C
C     INPUT
      INTEGER*4 MCOLOR,MCPART
      INTEGER*4 NCOLOR(4),NCPART(MCOLOR,4),LLOOP(MCPART,MCOLOR,4)
C
      INTEGER*4 MELM,NEX,
     *          NMAX,NODE,ME,NE,NP,NPMVB,LPMVB,
     *          NCRS,N1,N2,LTAB,NPP,IPCRS,ITIME,ISTART,
     *          IPART,LDOM,NBPDOM,NDOM,
     *          IPSLF,IPSND,MBPDOM,NUMIP,NITR,
     *          LFIXX,LFIXY,LFIXZ,
     *          IUT0,IUT6,IERR
      REAL*4    EPS,EPSRE,X,Y,Z,EJ,EXX,EYY,EZZ,EXY,EXZ,EYZ,
     *          DT,UMVB,VMVB,WMVB,UMESH,VMESH,WMESH,
     *          UMESH_P,VMESH_P,WMESH_P,RES,RX,RY,WRK01,
     *          AWRK01,AWRK02,AWRK03,AWRK04,AWRK05,AWRK06,AWRK07

      DIMENSION LTAB(N1,N2,NE),NEX(12),
     *          NPP(NP),IPCRS(NCRS)
      DIMENSION X(NP),Y(NP),Z(NP), NODE(N2,NE),EJ(NE),
     *          EXX(MELM),EYY(MELM),EZZ(MELM),
     *          EXY(MELM),EXZ(MELM),EYZ(MELM)
      DIMENSION LPMVB(3,NPMVB),
     *          UMVB(NPMVB),VMVB(NPMVB),WMVB(NPMVB)
      DIMENSION UMESH(NP),VMESH(NP),WMESH(NP)
      DIMENSION UMESH_P(NP),VMESH_P(NP),WMESH_P(NP)

      DIMENSION LDOM(NDOM), NBPDOM(NDOM),
     *          IPSLF(MBPDOM,NDOM), IPSND(MBPDOM,NDOM),
     *          NUMIP(NP)
      DIMENSION RX(0:N2,ME),RY(0:N2,ME)
      DIMENSION LFIXX(NP),LFIXY(NP),LFIXZ(NP),WRK01(NP)
      DIMENSION AWRK01(NP*3),AWRK02(NP*3),AWRK03(NP*3),
     *          AWRK04(NP*3),AWRK05(NP*3),AWRK06(NP*3),AWRK07(NP*3)
C
C     WORK
      REAL*4    AP(NCRS*9)
      REAL*4    AR(NP*3),DISP(NP*3),RHSF(NP*3)
      INTEGER*4 ICOLOR,ICPART
CCHY_TMP
      INTEGER*4 MRSALE,IALEDB,IUTAL
CCHY_TMP
C
C     ----STRESS-STRAIN INTERACTION----
      REAL*4    YM,PR,D1,D2,G
      PARAMETER (YM = 1.0E0, PR = 0.3E0)
      PARAMETER (D1 = YM*(1.E0-PR)/(1.E0+PR)/(1.E0-2.E0*PR))
      PARAMETER (D2 = YM*PR       /(1.E0+PR)/(1.E0-2.E0*PR))
      PARAMETER (G  = YM/2.E0/(1.E0+PR))
C
      INTEGER*4 IDOM,MAXBUF,NSIZEBUF,IE,I,J,IP,IBP,IDIM,
     *          NTET,NPRD,NWED,NHEX,IES,IEE,
     *          IELM0,I1,I2,I3,J1,J2,J3,IELMA,IELMB,
     *          NSKIP1,NSKIP2,NSKIP3,NSKIP4,ICRS,
     *          ICRS11,ICRS12,ICRS13,
     *          ICRS21,ICRS22,ICRS23,
     *          ICRS31,ICRS32,ICRS33
      REAL*4    K11,K12,K13,K21,K22,K23,K31,K32,K33
     *
C
      CHARACTER*60 ERMSGC /' ## SUBROUTINE ALE_CALMOV: FATAL
     *      ERROR REPORT   ; RETURNED' /
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          NEX(I)      ; INCLUDES NUMBER OF ELEMENTS AND NUMBER OF LOCAL NODES
C                        AS FOLOOWS
C          NEX(1)      ; NUMBER OF TET.    ELEMENTS
C          NEX(2)      ; NUMBER OF PYRAMID ELEMENTS
C          NEX(3)      ; NUMBER OF WEGDE   ELEMENTS
C          NEX(4)      ; NUMBER OF HEX.    ELEMENTS
C          NEX(5)      ; NUMBER OF LOCAL NODES IN A TET.    ELEMENT (=4)
C          NEX(6)      ; NUMBER OF LOCAL NODES IN A PYRAMID ELEMENT (=5)
C          NEX(7)      ; NUMBER OF LOCAL NODES IN A WEGDE   ELEMENT (=6)
C          NEX(8)      ; NUMBER OF LOCAL NODES IN A HEX.    ELEMENT (=8)
C
C          EPS       ; MAXIMUM ALLOWABLE ERROR
C          EPSRE     ; MAXIMUM RERATIVE ALLOWABLE ERROR
C          NMAX      ; MAX. NUMBER OF MATRIX SOLVER ITERATIONS
C          X(IP)     ; X COORDINATE OF NODE
C          Y(IP)     ; Y COORDINATE OF NODE
C          Z(IP)     ; Z COORDINATE OF NODE
C          NELM      ;
C
C          EXX (I,J,IE); INTEGRATED ELEMENT MATRIX OF NX*NXT
C          EYY (I,J,IE); INTEGRATED ELEMENT MATRIX OF NY*NYT
C          EZZ (I,J,IE); INTEGRATED ELEMENT MATRIX OF NZ*NZT
C          EXY (I,J,IE); INTEGRATED ELEMENT MATRIX OF NX*NYT
C          EXZ (I,J,IE); INTEGRATED ELEMENT MATRIX OF NX*NZT
C          EYZ (I,J,IE); INTEGRATED ELEMENT MATRIX OF NY*NZT
C
C          NODE(I,IE); NODE NO. TABLE BASED ON ELEMENT
C          ME        ; MAX. NUMBER OF TOTAL ELEMENTS
C          NE        ; NUMBER OF TOTAL ELEMENTS
C          NP        ; NUMBER OF TOTAL NODES
C          N         ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT
C          EJ(IE)    ; YOUNG'S MODULUS OF EACH ELEMENT FOR JACOBIAN STIFFENING
C          DT        ; TIME INCREMENT
C          NCRS      ; NUMBER OF NONZERO ELEMENTS IN MATRIX OF CRS FORMAT
C          N1        ;
C          N2        ;
C          LTAB(J1,J2,IE); CRS INDEX TABLE FOR NODE-BASE MATRIX
C                          COEFFICIENT
C          NPP      (IP); NUMBER OF ADJACENT NODES    TO NODE    IP
C          IPCRS  (ICRS); NODE NO. TABLE BASED ON CRS FORMAT
C          UMESH(IP) ; X-DIR MESH VELOCITY OF NODE
C          VMESH(IP) ; Y-DIR MESH VELOCITY OF NODE
C          WMESH(IP) ; Z-DIR MESH VELOCITY OF NODE
C          UMESH_P(IP) ; OLD X-DIR MESH VELOCITY OF NODE
C          VMESH_P(IP) ; OLD Y-DIR MESH VELOCITY OF NODE
C          WMESH_P(IP) ; OLD Z-DIR MESH VELOCITY OF NODE
C        
C        A.  FIXED BOUNDARY
C          NPMVB     ; NUMBER OF MOVING BOUNDARY NODES
C          LPMVB(3,IB) ; MOVING BOUNDARY NODE
C          UMVB(IB)  ; X-DIR VELOCITY OF MOVING BOUNDARY
C          VMVB(IB)  ; Y-DIR VELOCITY OF MOVING BOUNDARY
C          WMVB(IB)  ; Z-DIR VELOCITY OF MOVING BOUNDARY
C
C        B. INTER-CONNECT BOUNDARY
C          IPART     ; SUB-DOMAIN NUMBER THAT THIS TASK SHOULD TAKE/IS
C                      TAKING CARE OF. IPART BEING SET ZERO MEANS THAT
C                      THE PROGRAM SHOULD RUN/IS RUNNING IN SERIAL 
C                      MODE.
C
C          LDOM  (IDOM); NEIBERING SUB-DOMAIN NUMBER
C          NBPDOM(IDOM); NUMBER OF INTER-CONNECT BOUNDARY NODES
C                        SHARING WITH THE IDOM'TH NEIBERING SUB-DOMAIN,
C                        LDOM(IDOM)
C          NDOM        ; NUMBER OF THE NERIBERING SUB-DOMAINS
C          IPSLF(IBP,IDOM); INTER-CONNECT BOUNDARY NODE NUMBER IN THE
C                           CALLING TASK'S SUB-DOMAIN, FOR THE IDOM'TH
C                           NEIBERING SUB-DOMAIN, LDOM(IDOM)
C          IPSND(IBP,IDOM); INTER-CONNECT BOUNDARY NODE NUMBER IN THE
C                           SUB-DOMAIN THAT IS RECEIVING THE CALLING
C                           TASK'S RESIDUALS.
C          MBPDOM      ; THE MAXIMUM NUMBER OF THE INTER-CONNECT 
C                        BOUNDARY NODES FOR ONE NEIBERING SUB-DOMAIN
C          NUMIP(IP)   ; NUMBER OF NEIGHBORING DOMAINS THAT NODE
C                        'IP' BELONG TO
C
C          NITR      ; NUMBER OF MATRIX SOLVER ITERATIONS
C          RES       ; RESIDUAL OF U-EQUATION
C          RX        ;
C          RY        ;
C          IUT0      ; FILE NUMBER TO WRITE ERROR MESSAGE
C          IUT6      ; FILE NUMBER TO WRITE CALCULATION SEQUENCE
C          IERR      ; RETURN CODE TO REPORT ERROR OCCURENCE
C                      0 --- NORMAL TERMINATION
C                      1 --- A FATAL ERROR HAS OCCURED
C
C       (2) OUTPUT
C          UMESH(IP) ; X-DIR MESH VELOCITY OF NODE
C          VMESH(IP) ; Y-DIR MESH VELOCITY OF NODE
C          WMESH(IP) ; Z-DIR MESH VELOCITY OF NODE
C          X(IP)     ; X COORDINATE OF NODE
C          Y(IP)     ; Y COORDINATE OF NODE
C          Z(IP)     ; Z COORDINATE OF NODE
C
C       (3) WORK
C          AP  (ICRS); NODE-BASE MATRIX COEFFICIENT WHICH INCLUDES
C                  ALL THE ERMS AND WILL BE PASSED TO THE MATRIX SOLVER
C          AR(IP*3)      ; DIAGONAL TERM
C          RHSF(IP*3)    ; WORK REGION FOR RHS IN
C          DISP(IP*3)    ; MESH DISPLACEMENT OF NODE
C          LFIXX         ;
C          LFIXY         ;
C          LFIXZ         ;
C
C        STRESS-STRAIN INTERACTION
C          YM            ; YOUNG'S MODULUS (E)
C          PR            ; POISSON'S RATIO (NU)
C          D1            ;
C          D2            ;
C          G             ;
C
      MAXBUF = (N2+1)*NE
      NSIZEBUF = 0
      DO 100 IDOM=1,NDOM
         NSIZEBUF = NSIZEBUF + NBPDOM(IDOM)*4
 100  CONTINUE
      IF(MAXBUF .LT. NSIZEBUF) THEN
         WRITE(IUT0,*) ""
         WRITE(IUT0,*) "MORE BUFFER IS NECESSARY FOR MESH_CONTROL"
         STOP
      ENDIF

C     INITIALIZE 
      DO 200 ICRS=1,NCRS*9
         AP(ICRS)=0.0E0
  200 CONTINUE
C
      DO 300 IP=1,NP*3
         AR(IP) = 0.E0
         RHSF(IP) = 0.E0
 300  CONTINUE
C
C     MAKE FIX BOUNDARY NODES LIST
      DO 400 IP=1,NP
         LFIXX(IP)=0
         LFIXY(IP)=0
         LFIXZ(IP)=0
 400  CONTINUE
      DO 410 IBP=1,NPMVB
         IF ( LPMVB(1,IBP).GT.0 ) LFIXX(LPMVB(1,IBP))=1
         IF ( LPMVB(2,IBP).GT.0 ) LFIXY(LPMVB(2,IBP))=1
         IF ( LPMVB(3,IBP).GT.0 ) LFIXZ(LPMVB(3,IBP))=1
 410  CONTINUE
C
      NTET=NEX(5)
      NPRD=NEX(6)
      NWED=NEX(7)
      NHEX=NEX(8)
      NSKIP1=NEX( 9)
      NSKIP2=NEX(10)
      NSKIP3=NEX(11)
      NSKIP4=NEX(12)
C
C     == TET. ==
      IELM0=1
      DO 1000 ICOLOR=1,NCOLOR(1)
!ocl norecurrence(AP,AR)
      DO 1010 ICPART=1,NCPART(ICOLOR,1)
         IES=LLOOP(ICPART  ,ICOLOR,1)
         IEE=LLOOP(ICPART+1,ICOLOR,1)-1
!ocl nosimd
!ocl noswp
         DO 1020 IE=IES,IEE
            DO 1030 I=1,NTET
               DO 1040 J=1,NTET
                  I1=I+NTET*0
                  I2=I+NTET*1
                  I3=I+NTET*2
                  J1=J+NTET*0
                  J2=J+NTET*1
                  J3=J+NTET*2
C
                  IELMA=IELM0+(I-1)+NTET*(J-1)
                  IELMB=IELM0+(J-1)+NTET*(I-1)
C
                  K11 = D1*EXX(IELMA) + G *EYY(IELMA) + G *EZZ(IELMA)
                  K12 = D2*EXY(IELMA) + G *EXY(IELMB)
                  K13 = D2*EXZ(IELMA) + G *EXZ(IELMB)
                  K21 = D2*EXY(IELMB) + G *EXY(IELMA)
                  K22 =  G*EXX(IELMA) + D1*EYY(IELMA) + G *EZZ(IELMA)
                  K23 = D2*EYZ(IELMA) + G *EYZ(IELMB)
                  K31 = D2*EXZ(IELMB) + G *EXZ(IELMA)
                  K32 = D2*EYZ(IELMB) + G *EYZ(IELMA)
                  K33 = G *EXX(IELMA) + G *EYY(IELMA) + D1*EZZ(IELMA)
                  K11 = K11*EJ(IE)
                  K12 = K12*EJ(IE)
                  K13 = K13*EJ(IE)
                  K21 = K21*EJ(IE)
                  K22 = K22*EJ(IE)
                  K23 = K23*EJ(IE)
                  K31 = K31*EJ(IE)
                  K32 = K32*EJ(IE)
                  K33 = K33*EJ(IE)
C
                  ICRS  =9*(LTAB(I,J,IE)-1)
                  ICRS11=ICRS+1
                  ICRS12=ICRS+2
                  ICRS13=ICRS+3
                  ICRS21=ICRS+4
                  ICRS22=ICRS+5
                  ICRS23=ICRS+6
                  ICRS31=ICRS+7
                  ICRS32=ICRS+8
                  ICRS33=ICRS+9
C
                  AP(ICRS11)=AP(ICRS11)+K11
                  AP(ICRS12)=AP(ICRS12)+K12
                  AP(ICRS13)=AP(ICRS13)+K13
                  AP(ICRS21)=AP(ICRS21)+K21
                  AP(ICRS22)=AP(ICRS22)+K22
                  AP(ICRS23)=AP(ICRS23)+K23
                  AP(ICRS31)=AP(ICRS31)+K31
                  AP(ICRS32)=AP(ICRS32)+K32
                  AP(ICRS33)=AP(ICRS33)+K33
C
                  IF (I.EQ.J ) THEN
                     IP=NODE(I,IE)
                     AR(IP+NP*0)=AR(IP+NP*0)+K11
                     AR(IP+NP*1)=AR(IP+NP*1)+K22
                     AR(IP+NP*2)=AR(IP+NP*2)+K33
                  ENDIF
 1040          CONTINUE
 1030       CONTINUE
            IELM0=IELM0+NSKIP1
 1020    CONTINUE
 1010 CONTINUE
 1000 CONTINUE

C     == PYRAMID. ==
      DO 1100 ICOLOR=1,NCOLOR(2)
!ocl norecurrence(AP,AR)
      DO 1110 ICPART=1,NCPART(ICOLOR,2)
         IES=LLOOP(ICPART  ,ICOLOR,2)
         IEE=LLOOP(ICPART+1,ICOLOR,2)-1
!ocl nosimd
!ocl noswp
         DO 1120 IE=IES,IEE
            DO 1130 I=1,NPRD
               DO 1140 J=1,NPRD
                  I1=I+NPRD*0
                  I2=I+NPRD*1
                  I3=I+NPRD*2
                  J1=J+NPRD*0
                  J2=J+NPRD*1
                  J3=J+NPRD*2
C
                  IELMA=IELM0+(I-1)+NPRD*(J-1)
                  IELMB=IELM0+(J-1)+NPRD*(I-1)
C
                  K11 = D1*EXX(IELMA) + G *EYY(IELMA) + G *EZZ(IELMA)
                  K12 = D2*EXY(IELMA) + G *EXY(IELMB)
                  K13 = D2*EXZ(IELMA) + G *EXZ(IELMB)
                  K21 = D2*EXY(IELMB) + G *EXY(IELMA)
                  K22 =  G*EXX(IELMA) + D1*EYY(IELMA) + G *EZZ(IELMA)
                  K23 = D2*EYZ(IELMA) + G *EYZ(IELMB)
                  K31 = D2*EXZ(IELMB) + G *EXZ(IELMA)
                  K32 = D2*EYZ(IELMB) + G *EYZ(IELMA)
                  K33 = G *EXX(IELMA) + G *EYY(IELMA) + D1*EZZ(IELMA)
                  K11 = K11*EJ(IE)
                  K12 = K12*EJ(IE)
                  K13 = K13*EJ(IE)
                  K21 = K21*EJ(IE)
                  K22 = K22*EJ(IE)
                  K23 = K23*EJ(IE)
                  K31 = K31*EJ(IE)
                  K32 = K32*EJ(IE)
                  K33 = K33*EJ(IE)
C
                  ICRS  =9*(LTAB(I,J,IE)-1)
                  ICRS11=ICRS+1
                  ICRS12=ICRS+2
                  ICRS13=ICRS+3
                  ICRS21=ICRS+4
                  ICRS22=ICRS+5
                  ICRS23=ICRS+6
                  ICRS31=ICRS+7
                  ICRS32=ICRS+8
                  ICRS33=ICRS+9
C
                  AP(ICRS11)=AP(ICRS11)+K11
                  AP(ICRS12)=AP(ICRS12)+K12
                  AP(ICRS13)=AP(ICRS13)+K13
                  AP(ICRS21)=AP(ICRS21)+K21
                  AP(ICRS22)=AP(ICRS22)+K22
                  AP(ICRS23)=AP(ICRS23)+K23
                  AP(ICRS31)=AP(ICRS31)+K31
                  AP(ICRS32)=AP(ICRS32)+K32
                  AP(ICRS33)=AP(ICRS33)+K33
C
                  IF (I.EQ.J ) THEN
                     IP=NODE(I,IE)
                     AR(IP+NP*0)=AR(IP+NP*0)+K11
                     AR(IP+NP*1)=AR(IP+NP*1)+K22
                     AR(IP+NP*2)=AR(IP+NP*2)+K33
                  ENDIF
 1140          CONTINUE
 1130       CONTINUE
            IELM0=IELM0+NSKIP2
 1120    CONTINUE
 1110 CONTINUE
 1100 CONTINUE
C
C     == WEDGE. ==
      DO 1200 ICOLOR=1,NCOLOR(3)
!ocl norecurrence(AP,AR)
      DO 1210 ICPART=1,NCPART(ICOLOR,3)
         IES=LLOOP(ICPART  ,ICOLOR,3)
         IEE=LLOOP(ICPART+1,ICOLOR,3)-1
!ocl nosimd
!ocl noswp
         DO 1220 IE=IES,IEE
            DO 1230 I=1,NWED
               DO 1240 J=1,NWED
                  I1=I+NWED*0
                  I2=I+NWED*1
                  I3=I+NWED*2
                  J1=J+NWED*0
                  J2=J+NWED*1
                  J3=J+NWED*2
C
                  IELMA=IELM0+(I-1)+NWED*(J-1)
                  IELMB=IELM0+(J-1)+NWED*(I-1)
C
                  K11 = D1*EXX(IELMA) + G *EYY(IELMA) + G *EZZ(IELMA)
                  K12 = D2*EXY(IELMA) + G *EXY(IELMB)
                  K13 = D2*EXZ(IELMA) + G *EXZ(IELMB)
                  K21 = D2*EXY(IELMB) + G *EXY(IELMA)
                  K22 =  G*EXX(IELMA) + D1*EYY(IELMA) + G *EZZ(IELMA)
                  K23 = D2*EYZ(IELMA) + G *EYZ(IELMB)
                  K31 = D2*EXZ(IELMB) + G *EXZ(IELMA)
                  K32 = D2*EYZ(IELMB) + G *EYZ(IELMA)
                  K33 = G *EXX(IELMA) + G *EYY(IELMA) + D1*EZZ(IELMA)
                  K11 = K11*EJ(IE)
                  K12 = K12*EJ(IE)
                  K13 = K13*EJ(IE)
                  K21 = K21*EJ(IE)
                  K22 = K22*EJ(IE)
                  K23 = K23*EJ(IE)
                  K31 = K31*EJ(IE)
                  K32 = K32*EJ(IE)
                  K33 = K33*EJ(IE)
C
                  ICRS  =9*(LTAB(I,J,IE)-1)
                  ICRS11=ICRS+1
                  ICRS12=ICRS+2
                  ICRS13=ICRS+3
                  ICRS21=ICRS+4
                  ICRS22=ICRS+5
                  ICRS23=ICRS+6
                  ICRS31=ICRS+7
                  ICRS32=ICRS+8
                  ICRS33=ICRS+9
C
                  AP(ICRS11)=AP(ICRS11)+K11
                  AP(ICRS12)=AP(ICRS12)+K12
                  AP(ICRS13)=AP(ICRS13)+K13
                  AP(ICRS21)=AP(ICRS21)+K21
                  AP(ICRS22)=AP(ICRS22)+K22
                  AP(ICRS23)=AP(ICRS23)+K23
                  AP(ICRS31)=AP(ICRS31)+K31
                  AP(ICRS32)=AP(ICRS32)+K32
                  AP(ICRS33)=AP(ICRS33)+K33
C
                  IF (I.EQ.J ) THEN
                     IP=NODE(I,IE)
                     AR(IP+NP*0)=AR(IP+NP*0)+K11
                     AR(IP+NP*1)=AR(IP+NP*1)+K22
                     AR(IP+NP*2)=AR(IP+NP*2)+K33
                  ENDIF
 1240          CONTINUE
 1230       CONTINUE
            IELM0=IELM0+NSKIP3
 1220    CONTINUE
 1210 CONTINUE
 1200 CONTINUE
C
C     == HEX. ==
      DO 1300 ICOLOR=1,NCOLOR(4)
!ocl norecurrence(AP,AR)
      DO 1310 ICPART=1,NCPART(ICOLOR,4)
         IES=LLOOP(ICPART  ,ICOLOR,4)
         IEE=LLOOP(ICPART+1,ICOLOR,4)-1
!ocl nosimd
!ocl noswp
         DO 1320 IE=IES,IEE
            DO 1330 I=1,NHEX
               DO 1340 J=1,NHEX
                  I1=I+NHEX*0
                  I2=I+NHEX*1
                  I3=I+NHEX*2
                  J1=J+NHEX*0
                  J2=J+NHEX*1
                  J3=J+NHEX*2
C
                  IELMA=IELM0+(I-1)+NHEX*(J-1)
                  IELMB=IELM0+(J-1)+NHEX*(I-1)
C
                  K11 = D1*EXX(IELMA) + G *EYY(IELMA) + G *EZZ(IELMA)
                  K12 = D2*EXY(IELMA) + G *EXY(IELMB)
                  K13 = D2*EXZ(IELMA) + G *EXZ(IELMB)
                  K21 = D2*EXY(IELMB) + G *EXY(IELMA)
                  K22 =  G*EXX(IELMA) + D1*EYY(IELMA) + G *EZZ(IELMA)
                  K23 = D2*EYZ(IELMA) + G *EYZ(IELMB)
                  K31 = D2*EXZ(IELMB) + G *EXZ(IELMA)
                  K32 = D2*EYZ(IELMB) + G *EYZ(IELMA)
                  K33 = G *EXX(IELMA) + G *EYY(IELMA) + D1*EZZ(IELMA)
                  K11 = K11*EJ(IE)
                  K12 = K12*EJ(IE)
                  K13 = K13*EJ(IE)
                  K21 = K21*EJ(IE)
                  K22 = K22*EJ(IE)
                  K23 = K23*EJ(IE)
                  K31 = K31*EJ(IE)
                  K32 = K32*EJ(IE)
                  K33 = K33*EJ(IE)
C
                  ICRS  =9*(LTAB(I,J,IE)-1)
                  ICRS11=ICRS+1
                  ICRS12=ICRS+2
                  ICRS13=ICRS+3
                  ICRS21=ICRS+4
                  ICRS22=ICRS+5
                  ICRS23=ICRS+6
                  ICRS31=ICRS+7
                  ICRS32=ICRS+8
                  ICRS33=ICRS+9
C
                  AP(ICRS11)=AP(ICRS11)+K11
                  AP(ICRS12)=AP(ICRS12)+K12
                  AP(ICRS13)=AP(ICRS13)+K13
                  AP(ICRS21)=AP(ICRS21)+K21
                  AP(ICRS22)=AP(ICRS22)+K22
                  AP(ICRS23)=AP(ICRS23)+K23
                  AP(ICRS31)=AP(ICRS31)+K31
                  AP(ICRS32)=AP(ICRS32)+K32
                  AP(ICRS33)=AP(ICRS33)+K33
C
                  IF (I.EQ.J ) THEN
                     IP=NODE(I,IE)
                     AR(IP+NP*0)=AR(IP+NP*0)+K11
                     AR(IP+NP*1)=AR(IP+NP*1)+K22
                     AR(IP+NP*2)=AR(IP+NP*2)+K33
                  ENDIF
 1340          CONTINUE
 1330       CONTINUE
            IELM0=IELM0+NSKIP4
 1320    CONTINUE
 1310 CONTINUE
 1300 CONTINUE
C
      IDIM = 3
      CALL DDCOMX(IPART,IDIM,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *     AR  ,AR  (NP+1),AR  (NP*2+1),NP,IUT0,IERR,RX,RY,MAXBUF)
      IF(IERR.EQ.1) THEN
         WRITE(IUT0,*)'ERROR CODE REPORTED FROM DDCOMX'
         WRITE(IUT0,*) ERMSGC
         RETURN
      ENDIF
C
C     DIAGONAL SCALING OF CRS MATRIX AND RHS
      ICRS=0
      DO 1400 IP=1,NP
         DO 1410 I=1,NPP(IP)
            ICRS=ICRS+1
            ICRS11=(ICRS-1)*9+1
            ICRS12=(ICRS-1)*9+2
            ICRS13=(ICRS-1)*9+3
            ICRS21=(ICRS-1)*9+4
            ICRS22=(ICRS-1)*9+5
            ICRS23=(ICRS-1)*9+6
            ICRS31=(ICRS-1)*9+7
            ICRS32=(ICRS-1)*9+8
            ICRS33=(ICRS-1)*9+9
            AP(ICRS11)=AP(ICRS11)/AR(IP+NP*0)
            AP(ICRS12)=AP(ICRS12)/AR(IP+NP*0)
            AP(ICRS13)=AP(ICRS13)/AR(IP+NP*0)
            AP(ICRS21)=AP(ICRS21)/AR(IP+NP*1)
            AP(ICRS22)=AP(ICRS22)/AR(IP+NP*1)
            AP(ICRS23)=AP(ICRS23)/AR(IP+NP*1)
            AP(ICRS31)=AP(ICRS31)/AR(IP+NP*2)
            AP(ICRS32)=AP(ICRS32)/AR(IP+NP*2)
            AP(ICRS33)=AP(ICRS33)/AR(IP+NP*2)
 1410    CONTINUE
 1400 CONTINUE
C
      DO 1500 IP=1,NP
         RHSF(IP+NP*0)=RHSF(IP+NP*0)/AR(IP+NP*0)
         RHSF(IP+NP*1)=RHSF(IP+NP*1)/AR(IP+NP*1)
         RHSF(IP+NP*2)=RHSF(IP+NP*2)/AR(IP+NP*2)
 1500 CONTINUE
C
      IDIM = 3
      CALL DDCOMX(IPART,IDIM,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *     RHSF,RHSF(NP+1),RHSF(NP*2+1),NP,IUT0,IERR,RX,RY,MAXBUF)
      IF(IERR.EQ.1) THEN
         WRITE(IUT0,*)'ERROR CODE REPORTED FROM DDCOMX'
         WRITE(IUT0,*) ERMSGC
         RETURN
      ENDIF
C
      DO 1600 IBP=1,NPMVB
         IF ( LPMVB(1,IBP).GT.0 ) THEN
            IP=LPMVB(1,IBP)
            RHSF(IP+NP*0) = UMVB(IBP)*DT
         ENDIF
         IF ( LPMVB(2,IBP).GT.0 ) THEN
            IP=LPMVB(2,IBP)
            RHSF(IP+NP*1) = VMVB(IBP)*DT
         ENDIF
         IF ( LPMVB(3,IBP).GT.0 ) THEN
            IP=LPMVB(3,IBP)
            RHSF(IP+NP*2) = WMVB(IBP)*DT
         ENDIF
 1600 CONTINUE
C
C     CLEAR CRS MATRIX FOR DIRICHLET B.C.
      CALL ALE_CLRCRS(AP,NP,NCRS,IPCRS,NPP,
     *                LFIXX,LFIXY,LFIXZ,NUMIP,WRK01)
C
C     SET INITIIAL VECTOR TO SOLVE
      IF (ITIME.EQ.1.AND.ISTART.EQ.0) THEN
         DO 1700 IBP=1,NPMVB
            IF ( LPMVB(1,IBP).GT.0 ) THEN
               IP=LPMVB(1,IBP)
               UMESH  (IP) = UMVB(IBP)
               UMESH_P(IP) = UMVB(IBP)
            ENDIF
            IF ( LPMVB(2,IBP).GT.0 ) THEN
               IP=LPMVB(2,IBP)
               VMESH  (IP) = VMVB(IBP)
               VMESH_P(IP) = VMVB(IBP)
            ENDIF
            IF ( LPMVB(3,IBP).GT.0 ) THEN
               IP=LPMVB(3,IBP)
               WMESH  (IP) = WMVB(IBP)
               WMESH_P(IP) = WMVB(IBP)
            ENDIF
 1700     CONTINUE
      ENDIF
C
      DO 1800 IP=1,NP
         DISP(IP+NP*0) = ( 1.5E0*UMESH(IP) - 0.5E0*UMESH_P(IP) ) * DT
         DISP(IP+NP*1) = ( 1.5E0*VMESH(IP) - 0.5E0*VMESH_P(IP) ) * DT
         DISP(IP+NP*2) = ( 1.5E0*WMESH(IP) - 0.5E0*WMESH_P(IP) ) * DT
C
         UMESH_P(IP) = UMESH(IP)
         VMESH_P(IP) = VMESH(IP)
         WMESH_P(IP) = WMESH(IP)
 1800  CONTINUE
CC
CCC   SOLVER
C
C     CALL MATRIX SOLVER
      CALL ALE_BCGSTX(NPP,NCRS,IPCRS,AP,RHSF,DISP,EPS,EPSRE,
     *                NMAX,RES,NITR,NODE,NE,NEX,NP,ME,N2,
     *                IPART,LDOM,NBPDOM,NDOM,
     *                IPSLF,IPSND,MBPDOM,NUMIP,
     *                RX,RY,WRK01,AWRK01,AWRK02,
     *                AWRK03,AWRK04,AWRK05,AWRK06,AWRK07,
     *                MRSALE,IALEDB,IUTAL,IUT0,IERR)
      IF (IERR.NE.0) THEN
         WRITE(IUT0,*)'ERROR CODE REPORTED FROM BCGSTX'
         WRITE(IUT0,*) ERMSGC
         RETURN
      ENDIF
C
C     UPDATE MESH
      DO 2000 IP=1,NP
         UMESH(IP) = DISP(IP+NP*0) / DT
         VMESH(IP) = DISP(IP+NP*1) / DT
         WMESH(IP) = DISP(IP+NP*2) / DT
 2000 CONTINUE
C
      DO 2100 IBP=1,NPMVB
         IF ( LPMVB(1,IBP).GT.0 ) THEN
            IP=LPMVB(1,IBP)
            UMESH(IP) = UMVB(IBP)
            DISP(IP+NP*0) = UMVB(IBP)*DT
         ENDIF
         IF ( LPMVB(2,IBP).GT.0 ) THEN
            IP=LPMVB(2,IBP)
            VMESH(IP) = VMVB(IBP)
            DISP(IP+NP*1) = VMVB(IBP)*DT
         ENDIF
         IF ( LPMVB(3,IBP).GT.0 ) THEN
            IP=LPMVB(3,IBP)
            WMESH(IP) = WMVB(IBP)
            DISP(IP+NP*2) = WMVB(IBP)*DT
         ENDIF
 2100 CONTINUE
C
      DO 2200 IP=1,NP
         X(IP) = X(IP) + DISP(IP+NP*0)
         Y(IP) = Y(IP) + DISP(IP+NP*1)
         Z(IP) = Z(IP) + DISP(IP+NP*2)
 2200 CONTINUE
C
      RETURN
      END
