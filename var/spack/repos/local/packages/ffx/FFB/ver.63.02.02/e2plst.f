C=======================================================================
      SUBROUTINE E2PLST(MEP,MPP,M8,M9,NP,NE,NEX,
     &  NODE, NEP, IENP, JENP, NPP, NPP2, IPNP,
     &  LTAB2, IUT0, IERR)
C=======================================================================
      IMPLICIT NONE
      INTEGER MEP, MPP, M8, M9, NP, NE, IUT0, IERR
      INTEGER NEX(8)
      INTEGER NODE(M8,NE)
      INTEGER NEP(NP), IENP(MEP,NP), JENP(MEP,NP)
      INTEGER NPP(NP), NPP2(NP), IPNP(MPP,NP)
      INTEGER LTAB2(M9,M8,NE)
      INTEGER ICRS, J1, J2, IE, IENEW, IP1, IP2, I, K
C
      INTEGER NETET,IES1,IEE1
      INTEGER NEPRD,IES2,IEE2
      INTEGER NEWED,IES3,IEE3
      INTEGER NEHEX,IES4,IEE4
C
      CHARACTER*60 ERMSGC
     & /' ## SUBROUTINE E2PLST: FATAL      ERROR REPORT   ; RETURNED'/
C
C     CALCULATE INDEX TABLE OF COMPRESSED ROW STRAGE (CRS) FORMAT
C     FOR OBTAINING NODE-BASE MATRIX COEFFICIENT 
C     FROM ELEMENT-BASE MATRIX COEFFICIENT
C                                              2009.12.01 RIST
C
C     ARGUMENT LISTINGS
C
C       (1) INPUT
C          MEP         ; THE FIRST DIMENSION OF ARRAY IENP,JENP
C          MPP         ; THE FIRST DIMENSION OF ARRAY IPNP
C          M8          ; THE SECOND DIMENSION OF ARRAY LTAB2
C          M9          ; THE FIRST DIMENSION OF ARRAY NODE,LTAB2
C          NP          ; NUMBER OF TOTAL    NODES
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          IEBTET      ; BASE INDEX OF TET. ELEMENTS IN ARRAY LTAB2
C          NETET1      ; NUMBER OF TET.    ELEMENTS
C          IEBPRD      ; BASE INDEX OF PYRAMID ELEMENTS IN ARRAY LTAB2
C          NEPRD1      ; NUMBER OF PYRAMID ELEMENTS
C          IEBWED      ; BASE INDEX OF WEGDE ELEMENTS IN ARRAY LTAB2
C          NEWED1      ; NUMBER OF WEGDE   ELEMENTS
C          IEBHEX      ; BASE INDEX OF HEX. ELEMENTS IN ARRAY LTAB2
C          NEHEX1      ; NUMBER OF HEX.    ELEMENTS
C          NODE  (I,IE); NODE NO. TABLE BASED ON ELEMENT
C          NEP     (IP); NUMBER OF ADJACENT ELEMENTS TO NODE IP
C          IENP(IEP,IP); ADJACENT ELEMENT NUMBER TO NODE IP
C          JENP(IEP,IP); LOCAL NODE NUMBER OF IP IN ELEMENT IENP(IEP,IP)
C          NPP     (IP); NUMBER OF ADJACENT NODES    TO NODE    IP
C          NPP2    (IP); BASE INDEX OF CRS FORMAT FOR EACH ROW
C          IPNP(IPP,IP); ADJACENT NODE    NUMBER TO NODE    IP
C          IUT0        ; FILE NUMBER TO REPORT ERROR OCCURRENCE
C
C       (2) OUTPUT
C          LTAB2(J1,J2,IE); CRS INDEX TABLE FOR NODE-BASE MATRIX
C                           COEFFICIENT
C          IERR        ; RETURN CODE TO REPORT ERROR OCCURENCE
C
C       (3) WORK
C
C
      IERR=0
      NETET=NEX(1)
      NEPRD=NEX(2)
      NEWED=NEX(3)
      NEHEX=NEX(4)
C
C   == TET. ==  
      IES1=1
      IEE1=NETET 
C
C   == PYRAMID ==  
      IES2=NETET+1
      IEE2=NETET+NEPRD
C
C   == WEDGE ==  
      IES3=NETET+NEPRD+1
      IEE3=NETET+NEPRD+NEWED
C
C   == HEX. ==  
      IES4=NETET+NEPRD+NEWED+1
      IEE4=NETET+NEPRD+NEWED+NEHEX 
C
CCYY
CCYY [2] Make Matrix and Table 
CCYY 
      DO 2200 IE=1,NE
        DO 2000 J1=1,M8
          DO 2100 J2=1,M8
            LTAB2(J1,J2,IE)=0
 2100     CONTINUE
 2000   CONTINUE
 2200 CONTINUE
C-----------------------------------------------------------------------
CCYY 4000: NODE LOOP
CCYY 4100: ADJACENT ELEMENT LOOP  
CCYY 4200: LOCAL NODE LOOP
CCYY 4300: ADJACENT NODE LOOP FOR SEARCH
C
C
      DO 4000 IP2=1,NP
          DO 4100 I=1,NEP(IP2)
              IE=IENP(I,IP2)
              J2=JENP(I,IP2)
              IENEW=IE
              DO 4200 J1=1,M8
                  IP1=NODE(J1,IE)
C
                  IF (IP1.LE.0) GOTO 4200
C
                  DO 4300 K=1,NPP(IP1)
                      IF(IP2.EQ.IPNP(K,IP1))THEN
                          ICRS=NPP2(IP1)+K
                          IF(LTAB2(J1,J2,IENEW).NE.0) THEN
                              IERR=1
                              WRITE(IUT0,*) ERMSGC
                              GOTO 9000
                          ENDIF
                          LTAB2(J1,J2,IENEW)=ICRS
                          GOTO 4200
                      ENDIF
 4300             CONTINUE
                  IERR=2
                  WRITE(IUT0,*) ERMSGC
                  GOTO 9000
 4200         CONTINUE
 4100     CONTINUE
 4000 CONTINUE
C-----------------------------------------------------------------------
      DO 3640 IE=IES1,IEE1
        DO 3440 J1=1,4
          DO 3540 J2=1,4
            IF(LTAB2(J1,J2,IE).EQ.0) THEN
               IERR=3
               WRITE(IUT0,*) ERMSGC
               GOTO 9000
            ENDIF
 3540     CONTINUE
 3440   CONTINUE
 3640 CONTINUE
C
      DO 3650 IE=IES2, IEE2
        DO 3450 J1=1,5
          DO 3550 J2=1,5
            IF(LTAB2(J1,J2,IE).EQ.0) THEN
               IERR=4
               WRITE(IUT0,*) ERMSGC
               GOTO 9000
            ENDIF
 3550     CONTINUE
 3450   CONTINUE
 3650 CONTINUE
C
      DO 3660 IE=IES3,IEE3
        DO 3460 J1=1,6
          DO 3560 J2=1,6
            IF(LTAB2(J1,J2,IE).EQ.0) THEN
               IERR=5
               WRITE(IUT0,*) ERMSGC
               GOTO 9000
            ENDIF
 3560     CONTINUE
 3460   CONTINUE
 3660 CONTINUE
C
      DO 3680 IE=IES4,IEE4
        DO 3480 J1=1,8
          DO 3580 J2=1,8
            IF(LTAB2(J1,J2,IE).EQ.0) THEN
               IERR=6
               WRITE(IUT0,*) ERMSGC
               GOTO 9000
            ENDIF
 3580     CONTINUE
 3480   CONTINUE
 3680 CONTINUE
C-----------------------------------------------------------------------
 9000 CONTINUE
      RETURN
      END
