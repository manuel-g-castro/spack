C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    CYCLIC                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE CYCLIC(NPCL,LPCL1,LPCL2,IENP,JENP,NEP,MAXEP,NP,
     *                  IUT0,IERR)
      DIMENSION LPCL1(NPCL),LPCL2(NPCL),
     1          IENP(MAXEP,NP),JENP(MAXEP,NP),NEP(NP)
C
      CHARACTER*60 ERMSGB
     & / ' ## SUBROUTINE CYCLIC: FATAL      ERROR OCCURENCE; RETURNED' /
      CHARACTER*60 EREXP1
     & / ' NUMBER OF ELEMENTS ADJACENT TO ONE NODE EXCEEDED LIMIT OF ' /
C
C
C      MODIFY NEIBERING ELEMENT LISTS FOR CYCLIC BOUNDARY CONDITIONS
C         ( 2-D , 3-D CALCULATION & GRAPHICS )
C
C
C     NOTE 1 ; IF YOU ARE DEALING WITH A MATRIX EQUATION, CALLING THIS
C             SUBROUTINE WILL NOT SUFFICE BY ITSELF. YOU MUST ALSO
C             MODIFY THE GLOBAL COEFFICIENT MATRIX AND ITS CORRESPONDING
C             LIST VECTOR.
C
C     NOTE 2 ; TO MAKE CYCLIC BOUNDARY CONDITION EFFECTIVE, YOU
C             SHOULD USE LIST IENP, JENP AND NEP WHENEVER YOU SUPERPOSE
C             ELEMENT RESIDUAL TO A GLOBAL FORCE VECTOR, INCLUDING MASS
C             LUMPING PROCESS.
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          NPCL        ; NUMBER OF CYCLIC BOUNDARY NODES
C          LPCL1  (IBP); CYCLIC BOUNDARY NODES-1
C          LPCL2  (IBP); CYCLIC BOUNDARY NODES-2
C          MAXEP       ; THE FIRST DIMENSION OF ARRAY IENP,JENP
C          NP          ; NUMBER OF TOTAL     NODES
C          IUT0        ; FILE NUMBER TO REPORT ERROR OCCURENCE
C
C       (2) OUTPUT
C          IERR        ; RETURN CODE TO REPORT ERROR OCCURENCE
C                   0 --- NORMAL TERMINATION
C                   1 --- A FATAL ERROR HAS OCCURED
C
C       (3) INPUT-OUTPUT
C          IENP(IEP,IP); ADJACENT ELEMENT NUMBER TO NODE IP
C          JENP(IEP,IP); LOCAL NODE NUMBER OF IP IN ELEMENT IENP(IEP,IP)
C          NEP     (IP); NUMBER OF ADJACENT ELEMENTS TO NODE IP
C
C
      IERR = 0
C
C
C      (1) RECOGNIZE ADJACENT ELEMENTS TO LPCL1(IBP)
C         AS THE OUTER ELEMENTS OF LPCL2(IBP)
C
C
      DO 20 IPCL = 1 , NPCL
          IPA = LPCL1(IPCL)
          IPB = LPCL2(IPCL)
          DO 10 IEP = 1 , NEP(IPA)
CCYY.MOD---
              DO 11 IEPB = 1 , NEP(IPB)
                  IF(IENP(IEPB,IPB).EQ.IENP(IEP,IPA)) GO TO 10
   11         CONTINUE
CCYY.MOD---
              NEP(IPB) = NEP(IPB)+1
              IF(NEP(IPB).GT.MAXEP) THEN
                  WRITE(IUT0,*) ERMSGB
                  WRITE(IUT0,*) EREXP1, MAXEP
                  IERR = 1
                  RETURN
              ENDIF
              IENP(NEP(IPB),IPB) = IENP(IEP,IPA)
              JENP(NEP(IPB),IPB) = JENP(IEP,IPA)
   10     CONTINUE
   20 CONTINUE
C
C
C      (2) RECOGNIZE THE ORIGINAL ADJACENT ELEMENTS TO LPCL2(IBP))
C         AS THE OUTER ELEMENTS OF LPCL1(IBP)
C
C
CCYY.MOD---
CCYY.MOD      DO 50 IPCL = 1 , NPCL
      DO 50 IPCL = NPCL , 1 , -1
CCYY.MOD---
          IPA = LPCL2(IPCL)
          IPB = LPCL1(IPCL)
          DO 40 IEP = 1 , NEP(IPA)
              DO 30 IEPB = 1 , NEP(IPB)
                  IF(IENP(IEPB,IPB).EQ.IENP(IEP,IPA)) GO TO 40
   30         CONTINUE
              NEP(IPB) = NEP(IPB)+1
              IF(NEP(IPB).GT.MAXEP) THEN
                  WRITE(IUT0,*) ERMSGB
                  WRITE(IUT0,*) EREXP1, MAXEP
                  IERR = 1
                  RETURN
              ENDIF
              IENP(NEP(IPB),IPB) = IENP(IEP,IPA)
              JENP(NEP(IPB),IPB) = JENP(IEP,IPA)
   40     CONTINUE
   50 CONTINUE
C
C
      RETURN
      END
