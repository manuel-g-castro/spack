      SUBROUTINE TRACTX
     *   ( MCOLOR,MCPART,NCOLOR,NCPART,LLOOP,
     *     ME,MP,NE,NP,NODE,N1,N2,NEX,
     *     CRHO,CVEL,U,V,W,PN,VISC,MELM,EAP2,IENP,NODP,NEP,MEP,
     *     SN,CM, SIJN,FX,FY,FZ,TRAC,RX,RY,
     *     IPART ,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *     NPB,LPB,XPB,YPB,ZPB,
     *     IUT0,IERR)
C
      IMPLICIT NONE
C
CCCC  [INPUT:LOOP]
      INTEGER*4 MCOLOR,MCPART
      INTEGER*4 NCOLOR(4),NCPART(MCOLOR,4),LLOOP(MCPART,MCOLOR,4)
C
      INTEGER*4 ME,MP,NE,NP,N1,N2,MEP
      INTEGER*4 NODE(N2,NE),NEX(12)
      INTEGER*4 MELM
      REAL*4 CRHO,CVEL 
      REAL*4 U(NP),V(NP),W(NP),PN(NP),VISC(NE)
      REAL*4 SN(N1,NE),CM(NP)
      REAL*4 SIJN(MP,6),FX(NP),FY(NP),FZ(NP),
     *       RX(0:N2,ME),RY(0:N2,ME)
      REAL*8 TRAC(3,NPB)
C
      REAL*4 EAP2(3,N2,MEP,NP)
      INTEGER*4 IENP(MEP,MP),NODP(N2,MEP,NP),NEP(MP)
C
      INTEGER*4 IPART,NDOM,MBPDOM
      INTEGER*4 LDOM(NDOM),NBPDOM(NDOM),
     *          IPSLF(MBPDOM,NDOM),IPSND(MBPDOM,NDOM)
C
      INTEGER*4 NPB 
      INTEGER*4 LPB(NPB)
      REAL*4    XPB(NPB),YPB(NPB),ZPB(NPB) 
C
      INTEGER*4 IUT0,IERR
C
      INTEGER*4 IDUM,MAXBUF
C
      INTEGER*4 IB,IP
      REAL*4    VISP,COEF
C
      CHARACTER*60 ERMSGC
     & /' ## SUBROUTINE TRANCX: FATAL      ERROR REPORT   ; RETURNED' /
C
      IDUM=3
      MAXBUF = NE*(N2+1)
C
C
CCYY 1. COMPUTE NODAL VALUE OF STRAIN VELOCITY TENSOR
C
C
C  (1) DIAGONAL TERMS
C
C
      CALL NNXYZ( "DIAGONAL    ",MCOLOR,MCPART,NCOLOR,NCPART,LLOOP,
     *           ME,MP,NE,NP,NODE,N2,NEX,
     *           MELM,EAP2,IENP,NODP,NEP,MEP,
     *           FX,FY,FZ,U,V,W)
C
      CALL DDCOMX(IPART,IDUM,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     &            FX,FY,FZ,NP,IUT0,IERR,
     &            RX,RY,MAXBUF)
      IF(IERR.NE.0) THEN
          WRITE(IUT0,*)
          WRITE(IUT0,*) ERMSGC
          RETURN
      ENDIF   
C
      DO 1000 IP=1,NP
          SIJN(IP,1)=FX(IP)*CM(IP)
          SIJN(IP,2)=FY(IP)*CM(IP)
          SIJN(IP,3)=FZ(IP)*CM(IP)
 1000 CONTINUE   
C
C
C  (2) OFF-DIAGONAL TERMS
C
C
      CALL NNXYZ( "OFF-DIAGONAL",MCOLOR,MCPART,NCOLOR,NCPART,LLOOP,
     *           ME,MP,NE,NP,NODE,N2,NEX,
     *           MELM,EAP2,IENP,NODP,NEP,MEP,
     *           FX,FY,FZ,U,V,W)
C
      CALL DDCOMX(IPART,IDUM,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     &            FX,FY,FZ,NP,IUT0,IERR,
     &            RX,RY,MAXBUF)
      IF(IERR.NE.0) THEN
          WRITE(IUT0,*)
          WRITE(IUT0,*) ERMSGC
          RETURN
      ENDIF   
      DO 1100 IP=1,NP
         SIJN(IP,4)=FX(IP)*CM(IP)
         SIJN(IP,5)=FY(IP)*CM(IP)
         SIJN(IP,6)=FZ(IP)*CM(IP)
 1100 CONTINUE   
C
C
CCYY 2. COMPUTE NODAL VALUE OF VISCOCITY
C
C
      CALL NODLEX(NODE,ME,MP,NE,NP,N1,N2,NEX,SN,
     *            IPART,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *            VISC,FX,CM,IUT0,IERR,RX,RY,MAXBUF)
      IF(IERR.NE.0) THEN
          WRITE(IUT0,*)
          WRITE(IUT0,*) ERMSGC
          RETURN
      ENDIF   
C
C
CCYY 3. COMPUTE STRESS-TENSOR
C
C
      DO 1300 IP=1,NP
         VISP=FX(IP)
         SIJN(IP,1)=VISP*SIJN(IP,1)-PN(IP)
         SIJN(IP,2)=VISP*SIJN(IP,2)-PN(IP)
         SIJN(IP,3)=VISP*SIJN(IP,3)-PN(IP)
         SIJN(IP,4)=VISP*SIJN(IP,4)
         SIJN(IP,5)=VISP*SIJN(IP,5)
         SIJN(IP,6)=VISP*SIJN(IP,6)
 1300 CONTINUE   
C
C
CCYY 4. COMPUTE TRACTION AT BOUNDARY NODES
C
C
      COEF=CRHO*CVEL*CVEL
      DO 1400 IB=1,NPB
         IP=LPB(IB)
         TRAC(1,IB)= COEF*( XPB(IB)*SIJN(IP,1)
     *                     +YPB(IB)*SIJN(IP,6)
     *                     +ZPB(IB)*SIJN(IP,5))
         TRAC(2,IB)= COEF*( XPB(IB)*SIJN(IP,6)
     *                     +YPB(IB)*SIJN(IP,2)
     *                     +ZPB(IB)*SIJN(IP,4))
         TRAC(3,IB)= COEF*( XPB(IB)*SIJN(IP,5)
     *                     +YPB(IB)*SIJN(IP,4)
     *                     +ZPB(IB)*SIJN(IP,3))
 1400 CONTINUE   
C
      RETURN
      END
