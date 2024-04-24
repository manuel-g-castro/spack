C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    LUMPEE                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE LUMPEE(IDIM,E,IENP,JENP,NEP,MEP,ME,NP,N,
     *                  IPART,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,CM,
     *                  IUT0,IERR,WRK1,WRK2,MAXBUF)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION E(ME,N,N),IENP(MEP,NP),JENP(MEP,NP),NEP(NP),CM(NP)
C
      DIMENSION LDOM(NDOM),NBPDOM(NDOM),
     1          IPSLF(MBPDOM,NDOM),IPSND(MBPDOM,NDOM),
     2          WRK1(MAXBUF),WRK2(MAXBUF)
C
      CHARACTER*60 ERMSGC
     & / ' ## SUBROUTINE LUMPEE: FATAL      ERROR REPORT   ; RETURNED' /
C
C
C
C      LUMP ELEMENT MATRIX
C         ( 2-D & 3-D CALCULATION : SINGLE WORD VERSION )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          IDIM        ; SPECIFIES THE SPACE DIMENSION AS FOLLOWS
C                   2 --- TWO   DIMENSIONAL PROBLEM
C                   3 --- THREE DIMENSIONAL PROBLEM
C          E   (IE,I,J); ELEMENT MATRIX
C          IENP(IEP,IP); ADJACENT ELEMENT NUMBER TO NODE IP
C          JENP(IEP,IP); LOCAL NODE NUMBER OF IP IN ELEMENT IENP(IEP,IP)
C          NEP     (IP); NUMBER OF ADJACENT ELEMENTS TO NODE IP
C          MEP         ; THE FIRST DIMENSION OF ARRAY IENP,JENP
C          ME          ; MAX. NUMBER OF TOTAL ELEMENTS
C          NP          ; NUMBER OF TOTAL     NODES
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT
C
C          IPART       ; SUB-DOMAIN NUMBER THAT THIS TASK SHOULD TAKE/IS
C                       TAKING CARE OF. IPART BEING SET ZERO MEANS THAT
C                       THE PROGRAM SHOULD RUN/IS RUNNING IN SERIAL 
C                       MODE.
C          LDOM  (IDOM); NEIBERING SUB-DOMAIN NUMBER
C          NBPDOM(IDOM); NUMBER OF INTER-CONNECT BOUNDARY NODES
C                       SHARING WITH THE IDOM'TH NEIBERING SUB-DOMAIN,
C                       LDOM(IDOM)
C          NDOM        ; NUMBER OF THE NERIBERING SUB-DOMAINS
C          IPSLF (IBP,IDOM); INTER-CONNECT BOUNDARY NODE NUMBER IN THE
C                           CALLING TASK'S SUB-DOMAIN, FOR THE IDOM'TH
C                           NEIBERING SUB-DOMAIN, LDOM(IDOM)
C          IPSND (IBP,IDOM); INTER-CONNECT BOUNDARY NODE NUMBER IN THE
C                           SUB-DOMAIN THAT IS RECEIVING THE CALLING
C                           TASK'S RESIDUALS.
C          MBPDOM      ; THE MAXIMUM NUMBER OF THE INTER-CONNECT 
C                       BOUNDARY NODES FOR ONE NEIBERING SUB-DOMAIN
C          MAXBUF      ; SEND/RECEIVE BUFFER SIZE IN WORDS
C
C
C
C       (2) OUTPUT
C          CM(IP)      ; GLOBAL INVERSED LUMPED MATRIX
C
C       (4) WORK
C          WRK1  (IWRK); USED IN DDCOM3
C          WRK2  (IWRK); USED IN DDCOM3
C
C
C      CLEAR ARRAY
C
      DO 100 IP = 1 , NP
          CM(IP) = 0.E0
  100 CONTINUE
C
C      LUMP MATRIX
C
      DO 310 IEP = 1 , MEP
C
          IF(IDIM.EQ.2) THEN
              DO 200 IP = 1 , NP
                  IF(NEP(IP).GE.IEP) THEN
                      CM(IP) = CM(IP)+E(IENP(IEP,IP),JENP(IEP,IP),1)
     &                               +E(IENP(IEP,IP),JENP(IEP,IP),2)
     &                               +E(IENP(IEP,IP),JENP(IEP,IP),3)
     &                               +E(IENP(IEP,IP),JENP(IEP,IP),4)
                  ENDIF
  200         CONTINUE
          ELSE
              DO 300 IP = 1 , NP
                  IF(NEP(IP).GE.IEP) THEN
                      CM(IP) = CM(IP)+E(IENP(IEP,IP),JENP(IEP,IP),1)
     &                               +E(IENP(IEP,IP),JENP(IEP,IP),2)
     &                               +E(IENP(IEP,IP),JENP(IEP,IP),3)
     &                               +E(IENP(IEP,IP),JENP(IEP,IP),4)
     &                               +E(IENP(IEP,IP),JENP(IEP,IP),5)
     &                               +E(IENP(IEP,IP),JENP(IEP,IP),6)
     &                               +E(IENP(IEP,IP),JENP(IEP,IP),7)
     &                               +E(IENP(IEP,IP),JENP(IEP,IP),8)
                  ENDIF
  300         CONTINUE
          ENDIF
  310 CONTINUE
C
C      SUPERIMPOSE NEIBERING ELEMENT CONTRIBUTIONS
C
      IF(IPART.GE.1) THEN
          IDUM = 1
          CALL DDCOM3(IPART,IDUM,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *                CM,CM,CM,NP,IUT0,IERR,WRK1,WRK2,MAXBUF)
          IF(IERR.NE.0) THEN
              WRITE(IUT0,*)
              WRITE(IUT0,*) ERMSGC
              RETURN
          ENDIF
      ENDIF
C
C      INVERSE LUMPED MATRIX
C
      DO 400 IP = 1 , NP
          CM(IP) = 1.E0/CM(IP)
  400 CONTINUE
C
C
      RETURN
      END
