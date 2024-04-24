C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.8.1                                   C
C                                                                      C
C  SUB ROUTINE : NEIBR2                                                C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C CONTACT ADDRESS : IIS, THE UNIVERSITY OF TOKYO, CISS                 C
C                                                                      C
C THERMO-FLUID ANALYSIS SOLVERS FOR LARGE-SCALE-ASSEMBLY               C
C                                                                      C
C======================================================================C
      SUBROUTINE NEIBR2(IMODE,NODE,NE,NP,
     *  N2,
     *  MAXE,MAXP,MAXEP,MAXPP,MAXEE,
     *  IENP,JENP,NEP,IPNP,NPP,IENE,NEE,LIST,NPPMAX,IUT0,IERR)
      IMPLICIT NONE
      INTEGER I4FLAG
      INTEGER IMODE,NE,NP,N2,MAXE,MAXP,MAXEP,MAXPP,MAXEE,IUT0
      INTEGER IERR
      INTEGER NODE(N2,NE)
      INTEGER IENP(MAXEP,NP),JENP(MAXEP,NP),NEP(NP)
      INTEGER IPNP(MAXPP,NP),NPP(NP)
      INTEGER IENE(MAXEE,NE),NEE(NE)
      INTEGER LIST(N2,N2,NE),NPPMAX
      INTEGER IE,I,IP,IEP,IER,IEE,IPP,IPR,INCERT,J,N
C
      CHARACTER*60 ERMSGB
     & / ' ## SUBROUTINE NEIBR2: FATAL      ERROR OCCURENCE; RETURNED'/
      CHARACTER*60 EREXP1
     & / ' NUMBER OF ELEMENTS ADJACENT TO 1 NODE    EXCEEDED LIMIT OF' /
      CHARACTER*60 EREXP2
     & / ' NUMBER OF NODES    ADJACENT TO 1 NODE    EXCEEDED LIMIT OF' /
      CHARACTER*60 EREXP3
     & / ' NUMBER OF ELEMENTS ADJACENT TO 1 ELEMENT EXCEEDED LIMIT OF' /
      CHARACTER*60 EREXP4
     & / ' LIST(J,I,IE) CANNOT BE MADE BECAUSE IPNP HAS NOT BEEN MADE '/
C
C
C      MAKE NEIBERING RELATION SPECIFYING LIST VECTOR
C         ( 2-D , 3-D CALCULATION & GRAPHICS )
C
C
C     NOTE 1; THIS SUBROUTINE IS APPLICABLE TO MIXED-ELEMENT MESH
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          IMODE       ; SPECIFIES THE LISTS TO BE MADE AS FOLLOWS
C                       IMODE MUST BE ONE OF
C                       ( 0  1  2  10  11  12  101  102  111  112 )
C                  AS TO THE FIRST  FIGURE,
C                   0 --- IPNP , NPP WILL NOT BE MADE
C                   1 --- IPNP , NPP WILL     BE MADE
C                   2 --- IPNP , NPP WILL     BE MADE ( ORDER OF IP )
C                  AS TO THE SECOND FIGURE,
C                   0 --- IENE , NEE WILL NOT BE MADE
C                   1 --- IENE , NEE WILL     BE MADE
C                  AS TO THE THIRD  FIGURE,
C                   0 --- LIST       WILL NOT BE MADE
C                   1 --- LIST       WILL     BE MADE
C                 NOTE 1 ; IENP , JENP , AND NEP WILL ALWAYS BE MADE
C                 NOTE 2 ; IPNP MUST BE MADE IN ORDER TO MAKE LIST
C                 NOTE 3 ; AS AN UNAUTHORIZED OPTION, -IMODE IS
C                         SUPPORTED. IF A NEGATIVE VALUE OF IMODE IS
C                         SPECIFIED, THE PROCESS OF MAKING IENP,JENP AND
C                         NEP WILL BE SKIPPED AND THE LISTS CORRESPON-
C                         DING TO /IMODE/ WILL BE MADE.
C          NODE  (I,IE); NODE NO. TABLE BASED ON ELEMENT
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          NP          ; NUMBER OF TOTAL    NODES
C          N2           ; THE FIRST DIMENSION OF ARRAY NODE
C          MAXE        ; THE MAXIMUM NUMBER OF ELEMENTS
C          MAXP        ; THE MAXIMUM NUMBER OF    NODES
C          MAXEP       ; THE FIRST DIMENSION OF ARRAY IENP,JENP
C          MAXPP       ; THE FIRST DIMENSION OF ARRAY IPNP
C          MAXEE       ; THE FIRST DIMENSION OF ARRAY IENE
C          IUT0        ; FILE NUMBER TO REPORT ERROR OCCURENCE
C
C       (2) OUTPUT
C          IENP(IEP,IP); ADJACENT ELEMENT NUMBER TO NODE IP
C                      ( IF NEP(IP).LT.MAXEP , THEN IENP(NEP(IP)+1,IP),
C                       IENP(MAXEP,IP) WILL BE SET TO AN IMAGINARY
C                       ELEMENT NO. BETWEEN NE+1,MAXE.)
C          JENP(IEP,IP); LOCAL NODE NUMBER OF IP IN ELEMENT IENP(IEP,IP)
C          NEP     (IP); NUMBER OF ADJACENT ELEMENTS TO NODE IP
C          IPNP(IPP,IP); ADJACENT NODE    NUMBER TO NODE    IP
C                      ( IF NPP(IP).LT.MAXPP , THEN IPNP(NPP(IP)+1,IP),
C                       IPNP(MAXPP,IP) WILL BE SET TO AN IMAGINARY
C                       NODE    NO. BETWEEN NP+1,MAXP.)
C          NPP     (IP); NUMBER OF ADJACENT NODES    TO NODE    IP
C          IENE(IEE,IE); ADJACENT ELEMENT NUMBER TO ELEMENT IE
C          NEE     (IE); NUMBER OF ADJACENT ELEMENTS TO ELEMENT IE
C          LIST(J,I,IE); THE POSITION IN IPNP(IPP,NODE(I,IE))
C                       OF NODE(J,IE) I.E. IPNP(LIST(J,I,IE),NODE(I,IE))
C                       IS EQUAL TO NODE(J,IE)
C          IERR        ; RETURN CODE TO REPORT ERROR OCCURENCE
C                   0 --- NORMAL TERMINATION
C                   1 --- A FATAL ERROR HAS OCCURED
C
C
      IERR = 0
C
      N = N2
C
CCYYMOD---
      IENP=1
      JENP=1
CCYYMOD---
C
C      (1) MAKE IENP(IEP,IP) , NEP(IP)
C
C
      IF(IMODE.LT.0) THEN
          IMODE = -IMODE
          GO TO 140
      ENDIF
C
C INITIAL
C
      DO 110 IP = 1 , NP
          NEP(IP) = 0
          IF(MAXE.GT.NE) THEN
          DO 100 IEP = 1 , MAXEP
CC            IENP(IEP,IP) = MOD(IEP-1+MAXEP*(IP-1),MAXE-NE)+1+NE
              IENP(IEP,IP) = MOD(IEP-1+MAXEP*(IP-1),MAXE-NE)+1
              JENP(IEP,IP) = MOD(IEP-1             ,N      )+1
  100     CONTINUE
          ENDIF
  110 CONTINUE
C
C ELEMENT LOOP
C
      DO 130 IE = 1 , NE
          DO 120 I = 1 , N
              IP = NODE(I,IE)
              IF(IP.EQ.0) GO TO 120
              NEP(IP) = NEP(IP)+1
              IF(NEP(IP).GT.MAXEP) THEN
                  WRITE(IUT0,*) ERMSGB
                  WRITE(IUT0,*) EREXP1, MAXEP
                  IERR = 1
                  RETURN
              ENDIF
              IENP(NEP(IP),IP) = IE
              JENP(NEP(IP),IP) = I
  120     CONTINUE
  130 CONTINUE
C
  140 CONTINUE
C
C
C      (2) MAKE IPNP(IPP,IP) , NPP(IP)
C
C
      IF(I4FLAG(IMODE,1).GE.1) THEN
C
C INITIAL
C
      DO 210 IP = 1 , NP
          NPP(IP) = 0
          IF(MAXP.GT.NP) THEN
          DO 200 IPP = 1 , MAXPP
              IPNP(IPP,IP) = MOD(IPP-1+MAXPP*(IP-1),MAXP-NP)+1+NP
  200     CONTINUE
          ENDIF
  210 CONTINUE
C
C
C      IN MODE 1
C
C
      IF(I4FLAG(IMODE,1).EQ.1) THEN
C
C DIAGONAL
C
      DO 220 IP = 1 , NP
          NPP   (IP) = 1
          IPNP(1,IP) = IP
  220 CONTINUE
C
C NODE LOOP
C
      DO 260 IP = 1 , NP
          DO 250 IEP = 1 , NEP(IP)
              IE = IENP(IEP,IP)
              DO 240 I = 1 , N
                  IPR = NODE(I,IE)
                  IF(IPR.EQ.0) GO TO 240
                  DO 230 IPP = 1 , NPP(IP)
                      IF(IPNP(IPP,IP).EQ.IPR) GO TO 240
  230             CONTINUE
                  NPP(IP) = NPP(IP)+1
                  IF(NPP(IP).GT.MAXPP) THEN
                      WRITE(IUT0,*) ERMSGB
                      WRITE(IUT0,*) EREXP2, MAXPP
                      IERR = 1
                      RETURN
                  ENDIF
                  IPNP(NPP(IP),IP) = IPR
  240         CONTINUE
  250     CONTINUE
  260 CONTINUE
      ENDIF
C
C
C      IN MODE 2
C
C
      IF(I4FLAG(IMODE,1).EQ.2) THEN
      DO 360 IP = 1 , NP
          DO 350 IEP = 1 , NEP(IP)
              IE = IENP(IEP,IP)
              DO 340 I = 1 , N
                  IPR    = NODE(I,IE)
                  IF(IPR.EQ.0) GO TO 340
                  INCERT = NPP(IP)+1
                  DO 310 IPP = 1 , NPP(IP)
                      IF(IPNP(IPP,IP).EQ.IPR) GO TO 340
                      IF(IPNP(IPP,IP).GT.IPR) THEN
                          INCERT = IPP
                          GO TO 320
                      ENDIF
  310             CONTINUE
  320             CONTINUE
                  NPP(IP) = NPP(IP)+1
                  IF(NPP(IP).GT.MAXPP) THEN
                      WRITE(IUT0,*) ERMSGB
                      WRITE(IUT0,*) EREXP2, MAXPP
                      IERR = 1
                      RETURN
                  ENDIF
                  DO 330 IPP = NPP(IP) , INCERT+1 , -1
                      IPNP(IPP,IP) = IPNP(IPP-1,IP)
  330             CONTINUE
                  IPNP(INCERT,IP) = IPR
  340         CONTINUE
  350     CONTINUE
  360 CONTINUE
      ENDIF
      ENDIF
C
C
C      (3) MAKE IENE(IEE,IE) , NEE(IE)
C
C
      IF(I4FLAG(IMODE,2).EQ.1) THEN
C
C INITIAL
C
      DO 400 IE = 1 , NE
          NEE(IE) = 0
  400 CONTINUE
C
C ELEMENT LOOP
C
      DO 440 IE = 1 , NE
          DO 430 I = 1 , N
              IP = NODE(I,IE)
              IF(IP.EQ.0) GO TO 430
              DO 420 IEP = 1 , NEP(IP)
                  IER = IENP(IEP,IP)
                  IF(IER.EQ.IE) GO TO 420
                  DO 410 IEE = 1 , NEE(IE)
                      IF(IENE(IEE,IE).EQ.IER) GO TO 420
  410             CONTINUE
                  NEE(IE) = NEE(IE)+1
                  IF(NEE(IE).GT.MAXEE) THEN
                      WRITE(IUT0,*) ERMSGB
                      WRITE(IUT0,*) EREXP3, MAXEE
                      IERR = 1
                      RETURN
                  ENDIF
                  IENE(NEE(IE),IE) = IER
  420         CONTINUE
  430     CONTINUE
  440 CONTINUE
      ENDIF
C
C
C      (4) MAKE LIST(J,I,IE)
C
C
      IF(I4FLAG(IMODE,3).EQ.1) THEN
      IF(I4FLAG(IMODE,1).EQ.0) THEN
          WRITE(IUT0,*) ERMSGB
          WRITE(IUT0,*) EREXP4
          IERR = 1
          RETURN
      ENDIF
C
      DO 540 IE = 1 , NE
          DO 530 I = 1 , N
              IP = NODE(I,IE)
              IF(IP.EQ.0) GO TO 530
              DO 520 J = 1 , N
                  DO 510 IPP = 1 , NPP(IP)
                      IF(IPNP(IPP,IP).EQ.NODE(J,IE)) THEN
                          LIST(J,I,IE) = IPP
                          GO TO 520
                      ENDIF
  510             CONTINUE
  520         CONTINUE
  530     CONTINUE
  540 CONTINUE
      ENDIF
C
      NPPMAX=0
      DO 600 IP=1,NP
         NPPMAX=MAX(NPPMAX,NPP(IP))
 600  CONTINUE
C
      RETURN
      END
