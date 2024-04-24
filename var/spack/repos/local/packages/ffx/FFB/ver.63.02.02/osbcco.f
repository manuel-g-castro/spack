      SUBROUTINE OSBCCO(N1,N2,NE,NP,NSP,NS,NBESET,NPSET,
     *                  NODE,LOCAL,LBESET,LPSET1,
     *                  U,V,W,XNESET,YNESET,ZNESET,
     *                  COSBIN,COSBFR,OSBCOE,
     *                  IPART,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *                  RX,RY,WRK,LWRK,IUT0,IERR)
      IMPLICIT NONE
C
C[INPUT]
      INTEGER*4 ME,N1,N2,NE,NP,NSP,NS,NBESET,NPSET
      INTEGER*4 NODE(N2,NE),LOCAL(NSP,NS,4),
     *          LBESET(2,NBESET),LPSET1(NPSET),IUT0
      REAL*4    U(NP),V(NP),W(NP)
      REAL*4    XNESET(NBESET),YNESET(NBESET),ZNESET(NBESET)
      REAL*4    COSBIN,COSBFR
C
      INTEGER*4  IPART,NDOM,LDOM(NDOM),NBPDOM(NDOM),MBPDOM,
     *           IPSLF(MBPDOM,NDOM),IPSND(MBPDOM,NDOM)
C
C[OUTPUT]
      INTEGER*4 IERR
      REAL*4    OSBCOE(NP)
C
C[WORK]
      INTEGER*4 LWRK(NP)
      REAL*4    RX(N1,NE),RY(N1,NE),WRK(NP)
C
C[LOCAL]
      INTEGER*4 IP,IBE,IE,IS,IETYPE,NNPS,I,IPB
      INTEGER MAXBUF,IDUM
      REAL*4    UBUF,VBUF,WBUF,FACTOR
C
      MAXBUF = NE*N1
C
      DO 1000 IP=1,NP
          LWRK(IP)=0
 1000 CONTINUE   
C
      DO 1100 IBE = 1 , NBESET
          IE = LBESET(1,IBE)
          IS = LBESET(2,IBE)
          IF(     NODE(8,IE).GE.1) THEN ! HEX
             IETYPE = 4
          ELSE IF(NODE(6,IE).GE.1) THEN ! PRS
             IETYPE = 3
          ELSE IF(NODE(5,IE).GE.1) THEN ! PYR
             IETYPE = 2
          ELSE                          ! TET
             IETYPE = 1
          ENDIF   
          IF(LOCAL(4,IS,IETYPE).GE.1) THEN ! QUADRILATERAL
             NNPS = 4
          ELSE                             ! TRIANGLE
             NNPS = 3
          ENDIF
          UBUF=0.0E0
          VBUF=0.0E0
          WBUF=0.0E0
          DO 1200 I = 1 , NNPS
              IP = NODE(LOCAL(I,IS,IETYPE),IE)
              UBUF = UBUF + U (IP)
              VBUF = VBUF + V (IP)
              WBUF = WBUF + W (IP)
 1200     CONTINUE 
          UBUF=UBUF/FLOAT(NNPS)
          VBUF=VBUF/FLOAT(NNPS)
          WBUF=WBUF/FLOAT(NNPS)
          FACTOR = XNESET(IBE)*UBUF
     *            +YNESET(IBE)*VBUF
     *            +ZNESET(IBE)*WBUF
          IF(FACTOR.LT.0.0) GOTO 1100
C
          DO 1300 I = 1 , NNPS
              IP = NODE(LOCAL(I,IS,IETYPE),IE)
              LWRK(IP)=1
 1300     CONTINUE 
C
 1100 CONTINUE
C
      DO 2000 IP=1,NP
          WRK(IP)=FLOAT(LWRK(IP))
 2000 CONTINUE
C
      IDUM = 1
      CALL DDCOMX(IPART,IDUM,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *            WRK,WRK,WRK,NP,IUT0,IERR,RX,RY,MAXBUF)
C
      DO 2100 IP=1,NP
          IF(WRK(IP).GE.0.5E0) THEN
              LWRK(IP)=1
          ELSE
              LWRK(IP)=0
          ENDIF
 2100 CONTINUE
C
      DO 3000 IP=1,NP
          OSBCOE(IP)=1.0E0
 3000 CONTINUE
C
      DO 3100 IPB=1,NPSET
          IP=LPSET1(IPB)    
          IF(LWRK(IP).EQ.1) THEN
              OSBCOE(IP)=COSBIN
          ELSE
              OSBCOE(IP)=COSBFR
          ENDIF
 3100 CONTINUE   
C
      RETURN
      END
