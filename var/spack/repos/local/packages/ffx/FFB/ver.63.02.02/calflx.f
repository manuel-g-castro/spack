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
      SUBROUTINE CALFLX(NE,NP,NFACE,N2,NSP,NS,NODE,LOCAL,LFACE,
     *                  U,V,W,AVEC,FLAX,IUT6,IERR)
C
      IMPLICIT NONE
C
C     [INPUT]
      INTEGER*4 NE,NP,NFACE,N2,NSP,NS
      INTEGER*4 NODE(N2,NE),LOCAL(NSP,NS,4),LFACE(5,NFACE)
      REAL*4    U(NP),V(NP),W(NP),AVEC(4,NFACE)
C
C     [IN-OUTPUT]
      INTEGER*4 IUT6,IERR
      REAL*4    FLAX(NFACE)
C
C     [LOCAL]
      INTEGER*4 IFACE,IE,IS,IETYPE,IP1,IP2,IP3,IP4
      REAL*4    UF,VF,WF,AX,AY,AZ,DA
C
      IERR=0
C
      DO 1000 IFACE=1,NFACE
         IE=LFACE(1,IFACE)
         IS=LFACE(3,IFACE)
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
         AX = AVEC(1,IFACE)
         AY = AVEC(2,IFACE)
         AZ = AVEC(3,IFACE)
         DA = AVEC(4,IFACE)
         FLAX(IFACE)=DA*(AX*UF+AY*VF+AZ*WF)
C
 1000 CONTINUE
C
      RETURN
      END
