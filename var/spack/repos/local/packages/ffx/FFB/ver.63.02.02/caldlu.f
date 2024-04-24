      SUBROUTINE CALDLU(IPART,NE,NP,NFACE,N2,NSP,NS,NODE,
     *                  LOCAL,LFACE,NFINLT,NFFREE,LFINLT,LFFREE,
     *                  U,V,W,AVEC,FFA,DU,IERR)
C
      IMPLICIT NONE
C
C     [INPUT]
      INTEGER*4 IPART,NE,NP,NFACE,N2,NSP,NS
      INTEGER*4 NODE(N2,NE),LOCAL(NSP,NS,4),LFACE(5,NFACE)
      INTEGER*4 NFINLT,NFFREE,LFINLT(NFINLT),LFFREE(NFFREE)
      REAL*4    U(NP),V(NP),W(NP),AVEC(4,NFACE),FFA(NFACE)
C
C     [IN-OUTPUT]
      REAL*4    DU
      INTEGER*4 IERR

C
C     [LOCAL]
      INTEGER*4 IFACE,IE,IS,IBF,IETYPE,IP1,IP2,IP3,IP4
      REAL*4    QINLT,QFREE,AFREE,UF,VF,WF,AX,AY,AZ,DA,VAL1,VAL2,VAL3
C
      IERR=0
C
      QINLT=0.0E0
      DO 1000 IBF=1,NFINLT
         IFACE=LFINLT(IBF)
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
         QINLT=QINLT+DA*(AX*UF+AY*VF+AZ*WF)*FFA(IFACE)
C
 1000 CONTINUE
C
      QFREE=0.0E0
      AFREE=0.0E0
      DO 2000 IBF=1,NFFREE
         IFACE=LFFREE(IBF)
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
         QFREE=QFREE+DA*(AX*UF+AY*VF+AZ*WF)*FFA(IFACE)
         AFREE=AFREE+DA*FFA(IFACE)
C
 2000 CONTINUE
C
      IF(IPART.GE.1) THEN
         CALL DDCOM2(QINLT, VAL1)
         CALL DDCOM2(QFREE, VAL2)
         CALL DDCOM2(AFREE, VAL3)
         QINLT=VAL1
         QFREE=VAL2
         AFREE=VAL3
      ENDIF
C
      DU=(QINLT+QFREE)/AFREE
C
      RETURN
      END
