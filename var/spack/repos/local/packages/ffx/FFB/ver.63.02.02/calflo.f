C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.8.1                                   C
C                                                                      C
C  SUB ROUTINE : CALFLX                                                C
C                                                                      C
C                                       WRITTEN BY H.YOSHIMURA         C
C                                                                      C
C                                                                      C
C CONTACT ADDRESS : IIS, THE UNIVERSITY OF TOKYO, CISS                 C
C                                                                      C
C THERMO-FLUID ANALYSIS SOLVERS FOR LARGE-SCALE-ASSEMBLY               C
C                                                                      C
C======================================================================C
      SUBROUTINE CALFLO(ITIME,NE,NP,MMRF,MFRM,N2,NSP,NS,
     *                  NODE,LEFRM,LOCAL,
     *                  NBESET,LBESET,NESET,LESET4,
     *                  AESET,XNESET,YNESET,ZNESET,
     *                  NPSET,LPSET1,LPSET3,LPSET4,LWORK,
     *                  U,V,W,IUT6,FLSET,IERR)
C
      IMPLICIT NONE
C
C     [INPUT]
      INTEGER*4 ITIME,NE,NP,MMRF,MFRM,N2,NSP,NS
      INTEGER*4 NBESET,LBESET(2,NBESET),NESET,LESET4(NESET)
      INTEGER*4 NODE(N2,NE),LEFRM(NE),LOCAL(NSP,NS,4)
      INTEGER*4 IUT6
      REAL*4    AESET(NBESET),
     *          XNESET(NBESET),YNESET(NBESET),ZNESET(NBESET)
      REAL*4    U(NP),V(NP),W(NP)
      INTEGER*4 NPSET,LPSET1(NPSET),LPSET3(NPSET),
     *          LPSET4(NPSET),LWORK(NP)
C
C     [OUTPUT]
      INTEGER*4 IERR
      REAL*4    FLSET(MMRF,MMRF)
C
C     [LOCAL]
      INTEGER*4 IE,IS,IP,IPB,ISEND,
     *          IP1,IP2,IP3,IP4,IBE,IETYPE,IFRM1,IFRM2
      REAL*4    UF,VF,WF,UN,DQ,DUM
C
      IERR=0
C
      DO 100 IP=1,NP
          LWORK(IP)=0
 100  CONTINUE
C
      DO 200 IPB=1,NPSET
          IP    = LPSET1(IPB)
          ISEND = LPSET3(IPB)
          LWORK(IP)=LPSET4(IPB)
 200  CONTINUE
C
      DO 1000 IFRM1=1,MFRM
          DO 1100 IFRM2=1,MFRM
              FLSET(IFRM1,IFRM2)=0.0E0
 1100     CONTINUE
 1000 CONTINUE
C
      DO 2000 IBE=1,NBESET
         IE=LBESET(1,IBE)
         IS=LBESET(2,IBE)
C
         IF(     NODE(8,IE).GE.1) THEN ! HEX
            IETYPE=4
         ELSE IF(NODE(6,IE).GE.1) THEN ! PRS
            IETYPE=3
         ELSE IF(NODE(5,IE).GE.1) THEN ! PYR
            IETYPE=2
         ELSE                          ! TET
            IETYPE=1
         ENDIF
C
         IP1=NODE(LOCAL(1,IS,IETYPE),IE)
         IP2=NODE(LOCAL(2,IS,IETYPE),IE)
         IP3=NODE(LOCAL(3,IS,IETYPE),IE)
C
         IF ((IETYPE.EQ.1            ).OR. ! TRI
     *       (IETYPE.EQ.2.AND.IS.LE.4).OR.
     *       (IETYPE.EQ.3.AND.IS.LE.2)) THEN
            UF=(U(IP1)+U(IP2)+U(IP3))/3.0E0
            VF=(V(IP1)+V(IP2)+V(IP3))/3.0E0
            WF=(W(IP1)+W(IP2)+W(IP3))/3.0E0
         ELSE                              ! QUAD
            IP4=NODE(LOCAL(4,IS,IETYPE),IE)
            UF=(U(IP1)+U(IP2)+U(IP3)+U(IP4))/4.0E0
            VF=(V(IP1)+V(IP2)+V(IP3)+V(IP4))/4.0E0
            WF=(W(IP1)+W(IP2)+W(IP3)+W(IP4))/4.0E0
         ENDIF
C
            DQ=( UF*XNESET(IBE)
     *          +VF*YNESET(IBE)
     *          +WF*ZNESET(IBE) )*AESET(IBE) 
C
         IFRM1=LEFRM(IE)
         IFRM2=LWORK(IP1)
         IF(IFRM2.EQ.0) IFRM2=LWORK(IP2)
         IF(IFRM2.EQ.0) IFRM2=LWORK(IP3)
         IF(IFRM2.EQ.0) IFRM2=LWORK(IP4)
C
         FLSET(IFRM1,IFRM2)=FLSET(IFRM1,IFRM2)+DQ
C
 2000 CONTINUE
C
C
      DO 3000 IFRM1=1,MFRM
          DO 3100 IFRM2=1,MFRM
              CALL DDCOM2(FLSET(IFRM1,IFRM2),DUM)
              FLSET(IFRM1,IFRM2)=DUM
 3100     CONTINUE
 3000 CONTINUE
C
      DO 4000 IFRM1=1,MFRM
          WRITE(IUT6,'(A13,2I6,20E13.5)') 
     *    ' **CALFLO**: ', 
     *    ITIME,IFRM1,(FLSET(IFRM1,IFRM2),IFRM2=1,MFRM)
 4000 CONTINUE
C
      RETURN
      END
