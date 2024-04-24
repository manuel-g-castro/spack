      SUBROUTINE ELM3DX(MGAUSS,IGAUSH,IGAUSW,IGAUSP,IGAUST, 
     *                  MELM,ME,N1,N2,NE,NP,NEX,X,Y,Z,NODE,
     *                  SNI,DNXI,DNYI,DNZI,SN,DNX,DNY,DNZ,DELTA,
     *                  EAP1,EAP2,EAP3,EBP,MEP,MP,IENP,JENP,NEP,
     *                  NN,NC,PSI,PSIC,WW,IUT0,IERR)
      IMPLICIT NONE
C
      INTEGER MGAUSS,IGAUSH, IGAUSW, IGAUSP, IGAUST
      INTEGER ME,N1,N2,NE,NP,NSKIP1,NSKIP2,NSKIP3,NSKIP4,NEX
      INTEGER IELM
      REAL*8  X, Y, Z
      INTEGER NODE
      REAL*4  SNI,DNXI,DNYI,DNZI
      REAL*4  SN,DNX,DNY,DNZ,DELTA
      INTEGER IUT0,IERR
      REAL*8  NN,NC,PSI,PSIC,WW
      REAL*4    EAP1,EAP2,EAP3,EBP
      INTEGER*4 IENP,JENP,NEP,MEP,MP
C
      INTEGER*4 MELM
      DIMENSION NEX(12)
      DIMENSION X(NP),Y(NP),Z(NP)
      DIMENSION NODE(N2,NE+1),
     *          SNI( N1,NE+1),DNXI(N1,NE+1),DNYI(N1,NE+1),DNZI(N1,NE+1),
     *          SN ( N1,NE+1),DNX (N1,NE+1),DNY (N1,NE+1),DNZ (N1,NE+1),
     *          DELTA(NE+1)
      DIMENSION NN (  N1,MGAUSS),NC  (  N1),
     *          PSI(3,N1,MGAUSS),PSIC(3,N1),WW(MGAUSS)
      DIMENSION EAP1(N2,MEP,NP),EAP2(3,N2,MEP,NP),EAP3(6,N2,MEP,NP)
      DIMENSION EBP(3,N2,MEP,NP)
      DIMENSION IENP(MEP,MP),JENP(MEP,MP),NEP(MP)
C      
C     ARGUMENT LISTINGS
C       (1) INPUT
C          MGAUSS      ; MAX. NUMBER OF GAUSS POINTS
C          IGAUSH      ; NUMBER OF GAUSS POINTS FOR HEX. 
C          IGAUSW      ; NUMBER OF GAUSS POINTS FOR WED. 
C          IGAUSP      ; NUMBER OF GAUSS POINTS FOR PYR. 
C          IGAUST      ; NUMBER OF GAUSS POINTS FOR TET. 
C
C          X       (IP); X-COORDINATES OF NODES
C          Y       (IP); Y-COORDINATES OF NODES
C          Z       (IP); Z-COORDINATES OF NODES
C          NODE  (I,IE); NODE NO. TABLE BASED ON ELEMENT
C
C          ME          ; MAX. NUMBER OF TOTAL ELEMENTS
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          NP          ; NUMBER OF TOTAL    NODES
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
C          IUT0        ; FILE NUMBER TO REPORT ERROR OCCURENCE
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
C      
******************
***** OTHERS *****
******************
      INTEGER NETET,NEPRD,NEWED,NEHEX
      INTEGER NTET,NPRD,NWED,NHEX
      INTEGER IE,J
C
***** ALIAS *****
      NETET=NEX(1)
      NEPRD=NEX(2)
      NEWED=NEX(3)
      NEHEX=NEX(4)
      NTET=NEX(5)
      NPRD=NEX(6)
      NWED=NEX(7)
      NHEX=NEX(8)
      NSKIP1=NEX( 9)
      NSKIP2=NEX(10)
      NSKIP3=NEX(11)
      NSKIP4=NEX(12)
C
      DO 1000 IE=1,NE
         DO 1100 J=1,N1
            SN  (J,IE)=0.0E0
            DNX (J,IE)=0.0E0
            DNY (J,IE)=0.0E0
            DNZ (J,IE)=0.0E0
 1100    CONTINUE
 1000 CONTINUE
C
      IERR=0
      IE  =1
      IELM=1
**********************************
***** MAKE INTEGRALS FOR TET *****
**********************************
      CALL SETTET(N1,IGAUST,NC,PSIC,NN,PSI,WW,IUT0,IERR)
      IF(IERR.NE.0) GOTO 9999
      CALL INT3DX(IE,NETET,N1,N2,NTET,NSKIP1,IGAUST,
     *            NC,PSIC,NN,PSI,WW,X,Y,Z,NODE(1,IE),
     *            SNI(1,IE),DNXI(1,IE),DNYI(1,IE),DNZI(1,IE),
     *            SN (1,IE),DNX (1,IE),DNY (1,IE),DNZ (1,IE),
     *            DELTA(IE),
     *            EAP1,EAP2,EAP3,EBP,MEP,MP,NP,IENP,JENP,NEP,
     *            IUT0,IERR)
      IF(IERR.NE.0) GOTO 9999
      IE=IE+NETET
      IELM=IELM+NSKIP1*NETET
C
**********************************
***** MAKE INTEGRALS FOR PYR *****
**********************************
      CALL SETPRD(N1,IGAUSP,NC,PSIC,NN,PSI,WW,IUT0,IERR)
      IF(IERR.NE.0) GOTO 9999
      CALL INT3DX(IE,NEPRD,N1,N2,NPRD,NSKIP2,IGAUSP,
     *            NC,PSIC,NN,PSI,WW,X,Y,Z,NODE(1,IE),
     *            SNI(1,IE),DNXI(1,IE),DNYI(1,IE),DNZI(1,IE),
     *            SN (1,IE),DNX (1,IE),DNY (1,IE),DNZ (1,IE),
     *            DELTA(IE),
     *            EAP1,EAP2,EAP3,EBP,MEP,MP,NP,IENP,JENP,NEP,
     *            IUT0,IERR)
      IF(IERR.NE.0) GOTO 9999
      IE=IE+NEPRD
      IELM=IELM+NSKIP2*NEPRD
C
**********************************
***** MAKE INTEGRALS FOR WED *****
**********************************
      CALL SETWED(N1,IGAUSW,NC,PSIC,NN,PSI,WW,IUT0,IERR)
      IF(IERR.NE.0) GOTO 9999
      CALL INT3DX(IE,NEWED,N1,N2,NWED,NSKIP3,IGAUSW,
     *            NC,PSIC,NN,PSI,WW,X,Y,Z,NODE(1,IE),
     *            SNI(1,IE),DNXI(1,IE),DNYI(1,IE),DNZI(1,IE),
     *            SN (1,IE),DNX (1,IE),DNY (1,IE),DNZ (1,IE),
     *            DELTA(IE),
     *            EAP1,EAP2,EAP3,EBP,MEP,MP,NP,IENP,JENP,NEP,
     *            IUT0,IERR)
      IF(IERR.NE.0) GOTO 9999
      IE=IE+NEWED
      IELM=IELM+NSKIP3*NEWED
C
**********************************
***** MAKE INTEGRALS FOR HEX *****
**********************************
      CALL SETHEX(N1,IGAUSH,NC,PSIC,NN,PSI,WW,IUT0,IERR)
      IF(IERR.NE.0) GOTO 9999
      CALL INT3DX(IE,NEHEX,N1,N2,NHEX,NSKIP4,IGAUSH,
     *            NC,PSIC,NN,PSI,WW,X,Y,Z,NODE(1,IE),
     *            SNI(1,IE),DNXI(1,IE),DNYI(1,IE),DNZI(1,IE),
     *            SN (1,IE),DNX (1,IE),DNY (1,IE),DNZ (1,IE),
     *            DELTA(IE),
     *            EAP1,EAP2,EAP3,EBP,MEP,MP,NP,IENP,JENP,NEP,
     *            IUT0,IERR)
      IF(IERR.NE.0) GOTO 9999
      IE=IE+NEHEX
      IELM=IELM+NSKIP4*NEHEX
C
      RETURN
C
 9999 CONTINUE
      WRITE(IUT0,*) 'ERROR OCCURED IN ELM3DX'
      IERR=1
      RETURN
C 
      END
