      SUBROUTINE CALFRX(IPART,N2,NE,NP,NS,NSP,NODE,LOCAL,
     *                  U,V,W,PN,
     *                  NEINLT,LEINLT,XNINLT,YNINLT,ZNINLT,AEINLT,
     *                  NEFREE,LEFREE,XNFREE,YNFREE,ZNFREE,AEFREE,
     *                  QINLT,QFREE,PINLT,IUT0,IERR)
      IMPLICIT NONE
C
      INTEGER*4 IPART,N2,NEINLT,NEFREE,NE,NP,NS,NSP,IUT0,IERR
      INTEGER*4 NODE(N2,NE),LOCAL(NSP,NS,4),
     *          LEINLT(2,NEINLT),LEFREE(2,NEFREE)  
      REAL*4    U(NP),V(NP),W(NP),PN(NP),
     *          XNINLT(NEINLT),YNINLT(NEINLT),ZNINLT(NEINLT),
     *          AEINLT(NEINLT),
     *          XNFREE(NEFREE),YNFREE(NEFREE),ZNFREE(NEFREE),
     *          AEFREE(NEFREE)
      REAL*4    QINLT,PINLT,QFREE,PFREE,BUF1,BUF2
C
      INTEGER*4 IBE,IE,IS,IP,IETYPE,NNPS,I
      REAL*4    UBUF,VBUF,WBUF,PBUF,DQ
C
      IERR=0
C
      QINLT=0.0E0
      PINLT=0.0E0
      DO 1000 IBE = 1 , NEINLT
          IE = LEINLT(1,IBE)
          IS = LEINLT(2,IBE)
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
          PBUF=0.0E0
          DO 1100 I = 1 , NNPS
              IP = NODE(LOCAL(I,IS,IETYPE),IE)
              UBUF = UBUF + U (IP)
              VBUF = VBUF + V (IP)
              WBUF = WBUF + W (IP)
              PBUF = PBUF + PN(IP)
 1100     CONTINUE 
          UBUF=UBUF/FLOAT(NNPS)
          VBUF=VBUF/FLOAT(NNPS)
          WBUF=WBUF/FLOAT(NNPS)
CCYYMOD.20131201---
          PBUF=PBUF/FLOAT(NNPS)
CCYYMOD.20131201---
          DQ =( UBUF*XNINLT(IBE)
     *         +VBUF*YNINLT(IBE)
     *         +WBUF*ZNINLT(IBE))*AEINLT(IBE)
          QINLT=QINLT+DQ
          PINLT=PINLT+DQ*PBUF
 1000 CONTINUE
      IF(IPART.GE.1)THEN
          CALL DDCOM2(QINLT,BUF1)
          CALL DDCOM2(PINLT,BUF2)
          QINLT=BUF1
          PINLT=BUF2
      ENDIF
      IF(QINLT.EQ.0.0E0) THEN
          PINLT=0.0E0
      ELSE
          PINLT=PINLT/QINLT
      ENDIF
C
C     IF(NEINLT.EQ.0) PINLT=0.0E0
C
      QFREE=0.0E0
      PFREE=0.0E0
      DO 2000 IBE = 1 , NEFREE
          IE = LEFREE(1,IBE)
          IS = LEFREE(2,IBE)
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
          PBUF=0.0E0
          DO 2100 I = 1 , NNPS
              IP = NODE(LOCAL(I,IS,IETYPE),IE)
              UBUF = UBUF + U (IP)
              VBUF = VBUF + V (IP)
              WBUF = WBUF + W (IP)
              PBUF = PBUF + PN(IP)
 2100     CONTINUE 
          UBUF=UBUF/FLOAT(NNPS)
          VBUF=VBUF/FLOAT(NNPS)
          WBUF=WBUF/FLOAT(NNPS)
CCYYMOD.20131201---
          PBUF=PBUF/FLOAT(NNPS)
CCYYMOD.20131201---
          DQ =-( UBUF*XNFREE(IBE)
     *          +VBUF*YNFREE(IBE)
     *          +WBUF*ZNFREE(IBE))*AEFREE(IBE)
          QFREE=QFREE+DQ
          PFREE=PFREE+DQ*PBUF
 2000 CONTINUE
      IF(IPART.GE.1)THEN
          CALL DDCOM2(QFREE,BUF1)
          CALL DDCOM2(PFREE,BUF2)
          QFREE=BUF1
          PFREE=BUF2
      ENDIF
      PFREE=PFREE/QFREE
C
      RETURN
      END
