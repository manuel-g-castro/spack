      SUBROUTINE SETOS0(IPART,NPART,MBPDOM,MDOM,MPSET,NE,LEFRM,
     *                  MCOMM,NDOMS,NDOMR,
     *                  LISTS,LISTR,NBDOMS,NBDOMR,LDOMS,LDOMR,
     *                  LOSPS,LOSPR ,LOSFMS,LOSFMR,LOSFDS,LOSFDR,
     *                  LTYPES,LTYPER, 
     *                  COEF1S,COEF2S,COEF3S,ERROSS,
     *                  COEF1R,COEF2R,COEF3R,
     *                  NPSET,LPSET1,LPSET2,LPSET3,LPSET4,
     *                  COEFP1,COEFP2,COEFP3,
     *                  NESET,LESET1,LESET2,LESET3,LESET4,
     *                  COEFE1,COEFE2,COEFE3,
     *                  IUT6,IERR,N2,NP,NODE,X,Y,Z)
      IMPLICIT NONE
C
C[INPUT]
      INTEGER*4 IPART,NPART,MBPDOM,MDOM,MPSET,NDOMS,NDOMR
      INTEGER*4 NE,LEFRM(NE)
      INTEGER*4 MCOMM 
      INTEGER*4 LDOMS(NDOMS),LDOMR(NDOMR),
     *          LISTS(NDOMS),LISTR(NDOMR),          
     *          NBDOMS(NDOMS),NBDOMR(NDOMR),
     *          LOSPS (MCOMM),LOSPR (MCOMM),
     *          LOSFMS(MCOMM),LOSFMR(MCOMM),
     *          LOSFDS(MCOMM),LOSFDR(MCOMM),
     *          LTYPES(MCOMM),LTYPER(MCOMM)
      REAL*4    COEF1S(MCOMM),COEF1R(MCOMM),
     *          COEF2S(MCOMM),COEF2R(MCOMM),
     *          COEF3S(MCOMM),COEF3R(MCOMM),
     *          ERROSS(MCOMM)
      INTEGER*4 IUT6
      INTEGER*4 N2,NP,NODE(N2,NE)
      REAL*4    X(NP),Y(NP),Z(NP)
C
C[INPUT-OUTPUT]
      INTEGER*4 NPSET,LPSET1(MPSET),LPSET4(MPSET)
      INTEGER*4 NESET,LESET1(MPSET),LESET4(MPSET)
C
C[OUTPUT]
      INTEGER*4 LPSET2(MPSET),LPSET3(MPSET)
      INTEGER*4 LESET2(MPSET),LESET3(MPSET)
      REAL*4    COEFP1(MPSET),COEFP2(MPSET),COEFP3(MPSET)
      REAL*4    COEFE1(MPSET),COEFE2(MPSET),COEFE3(MPSET)
      INTEGER*4 IERR
C
C[LOCAL]
      INTEGER*4 IDOM,IPSET,IESET,IBP,IBE,IERRA,IP,IE
      INTEGER*4 LWRK1P(MPSET),LWRK2P(MPSET)
      INTEGER*4 LWRK1E(MPSET),LWRK2E(MPSET)
      REAL*4    ERR1,ERR2
C
C
C     VERSION 2011.07.15 WRITTEN BY Y.YAMADE   
C
C     ARGUMENT LIST
C
C[INPUT]
C IPART          : DOMAIN NUMBER (1 - NPART) 
C NPART          : NUMBER OF DOMAINS
C MBPDOM         : MAX. NUMBER OF O.S. NODES FOR A DOMAIN  
C MPSET          : MAX. NUMBER OF O.S. NODES
C NDOMS          : NUMBER OF DOMAIN  FOR SENDING
C NDOMR          : NUMBER OF DOMAIN  FOR RECEIVING
C LOSFDR (I,IDOM) : RESULT OF SERCHING (0:NOT FOUND, ELM. NUM.:FOUND)
C COEF1R (I,IDOM) : LOCAL COORDINATE-1
C COEF2R (I,IDOM) : LOCAL COORDINATE-2
C COEF3R (I,IDOM) : LOCAL COORDINATE-3
C LOSFDS (I,IDOM) : RESULT OF SERCHING (IT WILL BE GOT BY COMMUNICATION)
C COEF1S (I,IDOM) : LOCAL COORDINATE-1
C COEF2S (I,IDOM) : LOCAL COORDINATE-2
C COEF3S (I,IDOM) : LOCAL COORDINATE-3
C
C[INPUT-OUTPUT]
C NPSET      : NUMBER OF OVERSET NOUSES
C LPSET1(IBP): OVERSET NODES LIST
C
C[OUTPUT]
C LPSET2(IBP): ELEMENT NUMBER OF PARENTS ELEMENTS OF OVERSET NODES
C LPSET3(IBP): DOMAIN NUMBER
C              0        : BOTH OF OVERSET NODES AND PARENTS ELEENTS 
C                         ARE WITHIN THE DOMAIN 
C              POSITIVE : ONLY PARENT ELEMENTS ARE WITHIN THE DOMAIN
C              NEGATIVE : ONLY OVERSET NODES   ARE WITHIN THE DOMAIN
C COEF1(IBP) : LOCAL COORDINATE-1  
C COEF2(IBP) : LOCAL COORDINATE-2 
C COEF3(IBP) : LOCAL COORDINATE-3 
C
      IERR=0
C
      DO 1000 IDOM=1,NDOMS
          DO 1100 IBP=1,NBDOMS(IDOM)
              IPSET=LOSPS(IBP+LISTS(IDOM)-1)
              IF(LOSFDS(IBP+LISTS(IDOM)-1).EQ.0) GOTO 1100
C
              IF(LTYPES(IBP+LISTS(IDOM)-1).EQ.1) THEN 
C
                  LOSPS(IBP+LISTS(IDOM)-1)=LPSET1(IPSET)
C
                  IF(LPSET2(IPSET).EQ.0 ) THEN
                      LPSET2(IPSET)= LOSFDS(IBP+LISTS(IDOM)-1)
                      LPSET3(IPSET)=-LDOMS (IDOM)
                      COEFP1(IPSET)= COEF1S(IBP+LISTS(IDOM)-1)
                      COEFP2(IPSET)= COEF2S(IBP+LISTS(IDOM)-1)
                      COEFP3(IPSET)= COEF3S(IBP+LISTS(IDOM)-1)
                      IF(IPART.EQ.LDOMS(IDOM)) LPSET3(IPSET)=0
                      LWRK1P(IPSET)=IBP
                      LWRK2P(IPSET)=IDOM
                  ELSE
                      ERR1=ERROSS(LWRK1P(IPSET)+LISTS(LWRK2P(IPSET))-1)
                      ERR2=ERROSS(IBP          +LISTS(IDOM         )-1)
                      IF(ERR1.LE.ERR2) THEN
                          LOSFDS(IBP+LISTS(IDOM)-1)=0   
                      ELSE 
                          LPSET2(IPSET)= LOSFDS(IBP+LISTS(IDOM)-1)
                          LPSET3(IPSET)=-LDOMS (IDOM)
                          COEFP1(IPSET)= COEF1S(IBP+LISTS(IDOM)-1)
                          COEFP2(IPSET)= COEF2S(IBP+LISTS(IDOM)-1)
                          COEFP3(IPSET)= COEF3S(IBP+LISTS(IDOM)-1)
                          LOSFDS(LWRK1P(IPSET)+LISTS(LWRK2P(IPSET))-1)=0
                          IF(IPART.EQ.LDOMS(IDOM)) LPSET3(IPSET)=0
                          LWRK1P(IPSET)=IBP
                          LWRK2P(IPSET)=IDOM
                      ENDIF
                  ENDIF 
C
              ELSE IF(LTYPES(IBP+LISTS(IDOM)-1).EQ.2) THEN    
C
                  IESET=IPSET
                  LOSPS(IBP+LISTS(IDOM)-1)=LESET1(IESET)
C
                  IF(LESET2(IESET).EQ.0 ) THEN
                      LESET2(IESET)= LOSFDS(IBP+LISTS(IDOM)-1)
                      LESET3(IESET)=-LDOMS (    IDOM)
                      COEFE1(IESET)= COEF1S(IBP+LISTS(IDOM)-1)
                      COEFE2(IESET)= COEF2S(IBP+LISTS(IDOM)-1)
                      COEFE3(IESET)= COEF3S(IBP+LISTS(IDOM)-1)
                      IF(IPART.EQ.LDOMS(IDOM)) LESET3(IESET)=0
                      LWRK1E(IESET)=IBP
                      LWRK2E(IESET)=IDOM
                  ELSE
                      ERR1=ERROSS(LWRK1E(IESET)+LISTS(LWRK2E(IESET))-1)
                      ERR2=ERROSS(IBP          +LISTS(IDOM)         -1)
                      IF(ERR1.LE.ERR2) THEN
                          LOSFDS(IBP+LISTS(IDOM)-1)=0   
                      ELSE 
                          LESET2(IESET)= LOSFDS(IBP+LISTS(IDOM)-1)
                          LESET3(IESET)=-LDOMS (    IDOM)
                          COEFE1(IESET)= COEF1S(IBP+LISTS(IDOM)-1)
                          COEFE2(IESET)= COEF2S(IBP+LISTS(IDOM)-1)
                          COEFE3(IESET)= COEF3S(IBP+LISTS(IDOM)-1)
                          LOSFDS(LWRK1E(IESET)+LISTS(LWRK2E(IESET))-1)=0
                          IF(IPART.EQ.LDOMS(IDOM)) LESET3(IESET)=0
                          LWRK1E(IESET)=IBP
                          LWRK2E(IESET)=IDOM
                      ENDIF
                  ENDIF 
C
              ENDIF 
 1100     CONTINUE   
 1000 CONTINUE   
C
      DO 1200 IBP=1,NPSET
CCYY---
CC        IF(LPSET2(IBP).EQ.0) IERR=1
CCYY---
          IF(LPSET2(IBP).EQ.0) THEN
              IP=LPSET1(IBP) 
              WRITE(IUT6,*) 'SETOS0: NOT FOUND P',X(IP),Y(IP),Z(IP)
          ENDIF
 1200 CONTINUE   
C
      DO 1210 IBE=1,NESET
CCYY--
CC        IF(LESET2(IBE).EQ.0) IERR=1
CCYY--
          IF(LESET2(IBE).EQ.0) THEN
              IE=LESET1(IBE) 
              WRITE(IUT6,*) 'SETOS0: NOT FOUND E',
     *        (  X(NODE(1,IE))+X(NODE(2,IE))
     *          +X(NODE(3,IE))+X(NODE(4,IE))
     *          +X(NODE(5,IE))+X(NODE(6,IE))
     *          +X(NODE(7,IE))+X(NODE(8,IE)) )/8.0E0,
     *        (  Y(NODE(1,IE))+Y(NODE(2,IE))
     *          +Y(NODE(3,IE))+Y(NODE(4,IE))
     *          +Y(NODE(5,IE))+Y(NODE(6,IE))
     *          +Y(NODE(7,IE))+Y(NODE(8,IE)) )/8.0E0,
     *        (  Z(NODE(1,IE))+Z(NODE(2,IE))
     *          +Z(NODE(3,IE))+Z(NODE(4,IE))
     *          +Z(NODE(5,IE))+Z(NODE(6,IE))
     *          +Z(NODE(7,IE))+Z(NODE(8,IE)) )/8.0E0
          ENDIF
 1210 CONTINUE   
C
      IF(IPART.EQ.0) RETURN
C
      CALL ERRCHK(IUT6,IPART,1,IERR,IERRA)
      IERR=IERRA
      IF(IERR.NE.0) THEN
          RETURN
      ENDIF
C
      CALL DDSET6(IPART,MCOMM,
     *            NDOMS,LDOMS,NBDOMS,LISTS,LOSPS,
     *            NDOMR,LDOMR,NBDOMR,LISTR,LOSPR,IERR)
C
      CALL DDSET6(IPART,MCOMM,
     *            NDOMS,LDOMS,NBDOMS,LISTS,LOSFDS,
     *            NDOMR,LDOMR,NBDOMR,LISTR,LOSFDR,IERR)
C
      DO 2000 IDOM=1,NDOMR
          IF(LDOMR(IDOM).EQ.IPART) GOTO 2000
          DO 2100 IBP=1,NBDOMR(IDOM)
              IF(LOSFDR(IBP+LISTR(IDOM)-1).EQ.0 ) GOTO 2100
C
              IF(LTYPER(IBP+LISTR(IDOM)-1).EQ.1) THEN
                  NPSET=NPSET+1
                  IF(NPSET.GT.MPSET) THEN
                      IERR=1
                      GOTO 2100
                  ENDIF
                  LPSET1(NPSET)= LOSPR (IBP+LISTR(IDOM)-1)
                  LPSET2(NPSET)= LOSFDR(IBP+LISTR(IDOM)-1)
                  LPSET3(NPSET)= LDOMR (    IDOM)
                  IE=LPSET2(NPSET)
                  LPSET4(NPSET)= LEFRM(IE)
                  COEFP1(NPSET)= COEF1R(IBP+LISTR(IDOM)-1)
                  COEFP2(NPSET)= COEF2R(IBP+LISTR(IDOM)-1)
                  COEFP3(NPSET)= COEF3R(IBP+LISTR(IDOM)-1)
              ELSE IF(LTYPER(IBP+LISTR(IDOM)-1).EQ.2) THEN
                  NESET=NESET+1
                  IF(NESET.GT.MPSET) THEN
                      IERR=1
                      GOTO 2100
                  ENDIF
                  LESET1(NESET)= LOSPR (IBP+LISTR(IDOM)-1)
                  LESET2(NESET)= LOSFDR(IBP+LISTR(IDOM)-1)
                  LESET3(NESET)= LDOMR (    IDOM)
                  IE=LESET2(NESET)
                  LESET4(NESET)= LEFRM(IE)
                  COEFE1(NESET)= COEF1R(IBP+LISTR(IDOM)-1)
                  COEFE2(NESET)= COEF2R(IBP+LISTR(IDOM)-1) 
                  COEFE3(NESET)= COEF3R(IBP+LISTR(IDOM)-1)
              ENDIF
 2100     CONTINUE   
 2000 CONTINUE   
C
 2200 CONTINUE   
      CALL ERRCHK(IUT6,IPART,1,IERR,IERRA)
      IERR=IERRA
      IF(IERR.NE.0) THEN
          RETURN
      ENDIF
C
      WRITE(IUT6,*) 'SETOS0: '
      WRITE(IUT6,*) 'SETOS0: NPSET',NPSET
      WRITE(IUT6,*) 'SETOS0: NESET',NESET
C
      RETURN
      END
