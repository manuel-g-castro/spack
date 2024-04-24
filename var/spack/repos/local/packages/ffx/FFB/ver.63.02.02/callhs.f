      SUBROUTINE CALLHS(MCOLOR,MCPART,NCOLOR,NCPART,LLOOP,
     *                  ME,NE,NP,NEX,N1,N2,NODE,
     *                  APRS,APRS0,ATEST0,AAVER0,
     *                  NCRS,ATESPC,AAVEPC,LTAB,
     *                  FILTER,GAMDYN,NAVDYN,MELM,
     *                  EAP1,EAP3,IENP,JENP,NEP,MEP,MP,
     *                  IPART,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *                  NPFREE,LPFREE,NPSLD2,LPSLD2,LPFIX,
     *                  RX,RY,LWORK,IUT0,IERR)
      IMPLICIT NONE
C
      INTEGER*4 ME,NE,NP,N1,N2,IPART,NAVDYN,IUT0,IERR,
     *          NDOM,MBPDOM
      INTEGER*4 NETET,NEPRD,NEWED,NEHEX,
     *          IEE1,IEE2,IEE3,IEE4,
     *          IE,IP,I,J,MAXBUF,IB,
     *          IPE,MEP,MP,NN,K
      REAL*4    GAMDYN
      REAL*4    DI,
     *          FACTOR,RLUMP,GAM2,COEF,AVENUM
C
      INTEGER*4 MCOLOR,MCPART
      INTEGER*4 NCOLOR(4),NCPART(MCOLOR,4),LLOOP(MCPART,MCOLOR,4)
      INTEGER*4 NCRS
      INTEGER*4 NEX(12),NODE(N2,NE),LTAB(N1,N2,NE)
      REAL*4    APRS (N1,N2,ME),APRS0 (NP),ATEST0(NP),AAVER0(NP)
      REAL*4    ATESPC(NCRS),AAVEPC(NCRS)
      REAL*4    FILTER(ME)
      INTEGER*4 MELM
      INTEGER*4 NPFREE,LPFREE(NPFREE),
     1          NPSLD2,LPSLD2(NPSLD2),LPFIX(NP),
     2          LDOM(NDOM),NBPDOM(NDOM),
     3          IPSLF(MBPDOM,NDOM),IPSND(MBPDOM,NDOM)
      REAL*4    RX(0:N2,NE),RY(0:N2,NE),LWORK(NP)
      REAL*4    EAP1(N2,MEP,NP),EAP3(6,N2,MEP,NP)
      INTEGER*4 IENP(MEP,MP),JENP(MEP,MP),NEP(MP)
C
      REAL*4    DIJ(8,8)
      DATA DIJ / 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     &           0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     &           0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     &           0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0,
     &           0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0,
     &           0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0,
     &           0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0,
     &           0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0 /
C
      CHARACTER*60 ERMSGC
     & /' ## SUBROUTINE CALLHS: FATAL      ERROR OCCURENCE; RETURNED' /
C
      INTEGER*4 IDIMP,NPFIX
      DATA IDIMP  / 3 /
      DATA NPFIX  / 0 /
      
C
C
C      CALCULATE ELEMENT MATRIX FOR PRESSURE EQUATION 
C         ( 3-D CALCULATION : SINGLE WORD & MULTI ELEMENT VERSION )
C                                           CODED BASED ON 'EMDFTC'
C                                              2009.01.13 Y.YAMADE
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          ME          ; MAX. NUMBER OF TOTAL ELEMENTS
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT
C          NE          ; NUMBER OF TOTAL ELEMENTS
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
C          LS2NOD(IENEW); RENUMBERD ELEMENTS ARRAY.
C                      IF IENEW IS A RENUMBERD INDEX FOR AN ELEMENT,
C                      THEN THE ORIGINAL INDEX, IE, OF THIS ELEMENT
C                      IN THE NODE ARRAY IS GIVEN BY
C                      IE=LS2NOD(IENEW)
C          NCRS   ; NUMBER OF NONZERO ELEMENTS IN MATRIX OF CRS FORMAT
C          LTAB(J1,J2,IE); CRS INDEX TABLE FOR NODE-BASE MATRIX
C                          COEFFICIENT
C
C          A. FREE BOUNDARY
C          NPFREE      ; NUMBER OF FREE BOUNDARY NODES
C          LPFREE (IBP); FREE BOUNDARY NODES
C
C          B. INTER-CONNECT BOUNDARY
C          IPART       ; SUB-DOMAIN NUMBER THAT THIS TASK SHOULD TAKE/IS
C                       TAKING CARE OF. IPART BEING SET ZERO MEANS THAT
C                       THE PROGRAM SHOULD RUN/IS RUNNING IN SERIAL 
C                       MODE.
C          LDOM  (IDOM); NEIBERING SUB-DOMAIN NUMBER
C          NBPDOM(IDOM); NUMBER OF INTER-CONNECT BOUNDARY NODES
C                       SHARING WITH THE IDOM'TH NEIBERING SUB-DOMAIN,
C                       LDOM(IDOM)
C          NDOM        ; NUMBER OF THE NERIBERING SUB-DOMAINS
C          IPSLF (IBP,IDOM); INTER-CONNECT BOUNDARY NODE NUMBER IN THE
C                           CALLING TASK'S SUB-DOMAIN, FOR THE IDOM'TH
C                           NEIBERING SUB-DOMAIN, LDOM(IDOM)
C          IPSND (IBP,IDOM); INTER-CONNECT BOUNDARY NODE NUMBER IN THE
C                           SUB-DOMAIN THAT IS RECEIVING THE CALLING
C                           TASK'S RESIDUALS.
C          MBPDOM      ; THE MAXIMUM NUMBER OF THE INTER-CONNECT 
C                       BOUNDARY NODES FOR ONE NEIBERING SUB-DOMAIN
C                       BELONGS TO
C
C       (2) OUTPUT
C          A     (IE,I,J);ELEMENT MATRIX FOR PRESSURE EQUATION
C          A0    (    IP);DIAGONAL TERM   OF PRESSURE EQUATION MATRIX
C          ATEST (I,J,IE):ELEMENT MATRIX FOR GRID-FILTERING
C          ATEST0(    IP):DIAGONAL TERM OF GRID-FILTERING
C          AAVER (I,J,IE):ELEMENT MATRIX FOR AVERAGING
C          AAVER0(    IP):DIAGONAL TERM OF AVERAGING
C          ATESPC(ICRS);NODE-BASE MATRIX COEFFICIENT FOR GRID-FILTERING
C          AAVEPC(ICRS);NODE-BASE MATRIX COEFFICIENT FOR AVERAGING
C
C       (3) WORK
C          RX          ; WORK
C          RY          ; WORK
C          LPFIX       ; WORK
C
      GAM2   = GAMDYN*GAMDYN
      AVENUM = FLOAT(NAVDYN)
      MAXBUF = NE*(N2+1)
C
      NETET=NEX(1)
      NEPRD=NEX(2)
      NEWED=NEX(3)
      NEHEX=NEX(4)
C
C   == TET. ==  
      IEE1=NETET 
C
C   == PYRAMID ==  
      IEE2=NETET+NEPRD
C
C   == WEDGE ==  
      IEE3=NETET+NEPRD+NEWED
C
C   == HEX. ==  
      IEE4=NETET+NEPRD+NEWED+NEHEX 
C
CCC   CAL. LAPLACE OPERATOR
C
      DO 1200 IE=1,NE
          DO 1100 J=1,N2
              DO 1000 I=1,N1
                  APRS(I,J,IE)=0.0E0
 1000         CONTINUE
 1100     CONTINUE
 1200 CONTINUE
C
      DO 1300 IP=1,NP
          APRS0 (IP)=0.0E0
          ATEST0(IP)=0.0E0
          AAVER0(IP)=0.0E0
 1300  CONTINUE     
C
C     OPERATION COUNTS:   FLOP /ELEMENT
C     DATA LOADINGS   :   WORDS/ELEMENT
C                      (  WORDS CONTIGUOUSLY,
C                         WORDS BY STRIDE, AND
C                         WORDS BY LIST )
C
      DO IP=1,NP
!ocl nosimd
!ocl noswp
          DO IPE=1,NEP(IP)
              IE =IENP(IPE,IP)
              I  =JENP(IPE,IP)
C
              IF(IE.LE.IEE1) THEN
                  NN = NEX(5)
              ELSE IF (IE.LE.IEE2) THEN
                  NN = NEX(6)
              ELSE IF (IE.LE.IEE3) THEN
                  NN = NEX(7)
              ELSE 
                  NN = NEX(8)
              ENDIF
C
              RLUMP=0.0E0
              DO K=1,NN
                  RLUMP = RLUMP + EAP1(K,IPE,IP)
              ENDDO
C
              FACTOR=(GAM2/24.0E0)*FILTER(IE)**2
C
              DO K=1,NN
                  DI = EAP3(1,K,IPE,IP)+EAP3(2,K,IPE,IP)
     *                +EAP3(3,K,IPE,IP)
                  APRS(I,K,IE) = RLUMP*DIJ(I,K) + FACTOR*DI
              ENDDO
              ATEST0(IP) = ATEST0(IP) + APRS(I,I,IE)
          ENDDO
      ENDDO
C
      IDIMP=1
      CALL DDCOMX(IPART,IDIMP,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *            ATEST0,ATEST0,ATEST0,NP,IUT0,IERR,RX,RY,MAXBUF)
      IF(IERR.NE.0) THEN
         WRITE(IUT0,*)
         WRITE(IUT0,*) ERMSGC
         RETURN
      ENDIF   
C
      CALL DSCALE(N1,N2,NE,NP,NEX,NODE,APRS,ATEST0,LWORK,IERR)
C
      DO 2800 IP=1,NP
         ATEST0(IP)=1.0E0/ATEST0(IP)
 2800 CONTINUE
C   
      CALL E2PMAT(MCOLOR,MCPART,NCOLOR,NCPART,LLOOP,
     *            N2,N1,NE,NEX,NCRS,APRS,ATESPC,LTAB,IUT0,IERR)
      IF (IERR.NE.0) THEN
         WRITE(IUT0,*)
         WRITE(IUT0,*) ERMSGC
         RETURN
      ENDIF
C
      DO IP=1,NP
!ocl nosimd
!ocl noswp
          DO IPE=1,NEP(IP)
              IE =IENP(IPE,IP)
              I  =JENP(IPE,IP)
              IF(IE.LE.IEE1) THEN
                  NN = NEX(5)
              ELSE IF (IE.LE.IEE2) THEN
                  NN = NEX(6)
              ELSE IF (IE.LE.IEE3) THEN
                  NN = NEX(7)
              ELSE 
                  NN = NEX(8)
              ENDIF
C
              RLUMP=0.0E0
              DO K=1,NN
                  RLUMP = RLUMP + EAP1(K,IPE,IP)
              ENDDO
C
              FACTOR=(GAM2/24.0E0)*FILTER(IE)**2
C
              DO K=1,NN
                  DI =EAP3(1,K,IPE,IP)+EAP3(2,K,IPE,IP)+EAP3(3,K,IPE,IP)
                  APRS(I,K,IE) = RLUMP*DIJ(I,K) + FACTOR*AVENUM*DI
              ENDDO
              AAVER0(IP) = AAVER0(IP) + APRS(I,I,IE)
          ENDDO
      ENDDO
C
      IDIMP=1
      CALL DDCOMX(IPART,IDIMP,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *            AAVER0,AAVER0,AAVER0,NP,IUT0,IERR,RX,RY,MAXBUF)
      IF(IERR.NE.0) THEN
         WRITE(IUT0,*)
         WRITE(IUT0,*) ERMSGC
         RETURN
      ENDIF   
C
      CALL DSCALE(N1,N2,NE,NP,NEX,NODE,APRS,AAVER0,LWORK,IERR)
C
      DO 3800 IP=1,NP
         AAVER0(IP)=1.0E0/AAVER0(IP)
 3800 CONTINUE
C
      CALL E2PMAT(MCOLOR,MCPART,NCOLOR,NCPART,LLOOP,
     *            N2,N1,NE,NEX,NCRS,APRS,AAVEPC,LTAB,IUT0,IERR)
      IF (IERR.NE.0) THEN
         WRITE(IUT0,*)
         WRITE(IUT0,*) ERMSGC
         RETURN
      ENDIF
C
      DO IP=1,NP
!ocl nosimd
!ocl noswp
          DO IPE=1,NEP(IP)
              IE =IENP(IPE,IP)
              I  =JENP(IPE,IP)
              IF(IE.LE.IEE1) THEN
                  NN = NEX(5)
              ELSE IF (IE.LE.IEE2) THEN
                  NN = NEX(6)
              ELSE IF (IE.LE.IEE3) THEN
                  NN = NEX(7)
              ELSE 
                  NN = NEX(8)
              ENDIF
C
              DO K=1,NN
                  DI = EAP3(1,K,IPE,IP)+EAP3(2,K,IPE,IP)
     *                +EAP3(3,K,IPE,IP)
                  APRS(I,K,IE) = DI
              ENDDO
              APRS0(IP) = APRS0(IP) + APRS(I,I,IE)
          ENDDO
      ENDDO
C
      IDIMP=1
      CALL DDCOMX(IPART,IDIMP,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *            APRS0,APRS0,APRS0,NP,IUT0,IERR,RX,RY,MAXBUF)
      IF(IERR.NE.0) THEN
         WRITE(IUT0,*)
         WRITE(IUT0,*) ERMSGC
         RETURN
      ENDIF   
C
      CALL DSCALE(N1,N2,NE,NP,NEX,NODE,APRS,APRS0,LWORK,IERR)
C
C
CCC    CLEAR OFF-DIAGONAL ELEMENTS IN THOSE LINES OF COEFFICIENT MATRIX
CCC   THAT CORRESPOND TO THE DIRECLET BOUNDARY NODES
C
C
C
      DO 5000 IP=1,NP
          LWORK(IP)=0
 5000 CONTINUE   
C
      DO 5100 IB=1,NPFREE
          IP=LPFREE(IB)
          LWORK(IP)=1
 5100 CONTINUE   
C
      DO 5200 IB=1,NPSLD2
          IP=LPSLD2(IB)
          LWORK(IP)=1
 5200 CONTINUE   
C
      NPFIX=0
      DO 5300 IP=1,NP
          IF(LWORK(IP).EQ.0) GOTO 5300
          NPFIX=NPFIX+1
          LPFIX(NPFIX)=IP
 5300 CONTINUE   
C
      CALL CLROFF(MCOLOR,MCPART,NCOLOR,NCPART,LLOOP,
     *            ME,NE,NP,NEX,NODE,APRS,
     *            N1,N2,NPFIX,LPFIX,LWORK,IUT0,IERR)
      IF(IERR.NE.0) THEN
         WRITE(IUT0,*)
         WRITE(IUT0,*) ERMSGC
         RETURN
      ENDIF   
C
      RETURN
      END
