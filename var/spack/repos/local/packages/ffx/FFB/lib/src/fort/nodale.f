C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    NODALE                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE NODALE(IDIM,SN,CM,IENP,JENP,NEP,ME,MEP,NE,NP,N,
     *                  IPART,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *                  VALE,VALN,IUT0,IERR,BUFSND,BUFRCV,MAXBUF)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION SN(N,NE),CM(NP),IENP(MEP,NP),JENP(MEP,NP),NEP(NP)
C
      DIMENSION LDOM(NDOM),NBPDOM(NDOM),
     1          IPSLF(MBPDOM,NDOM),IPSND(MBPDOM,NDOM)
C
      DIMENSION VALE(NE),VALN(NP),BUFSND(MAXBUF),BUFRCV(MAXBUF)
C
      CHARACTER*60 ERMSGC
     & /' ## SUBROUTINE NODALE: FATAL      ERROR REPORT   ; RETURNED' /
C
C
C
C      ASSIGN ELEMENT VALUES TO NODES BASED ON THE WEIGHTED RESIDUALS
C         ( 3-D CALCULATION : SINGLE WORD VERSION )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          IDIM        ; SPECIFIES THE SPACE DIMENSION AS FOLLOWS
C                   2 --- TWO   DIMENSIONAL PROBLEM
C                   3 --- THREE DIMENSIONAL PROBLEM
C          SN    (I,IE); INTEGRATED ELEMENT VECTOR OF N
C          CM      (IP); INVERSED LUMPED MASS MATRIX
C
C          IENP(IEP,IP); ADJACENT ELEMENT NUMBER TO NODE IP
C                      ( IF NEP(IP).LT.MEP , THEN IENP(NEP(IP)+1,IP),
C                       IENP(MEP,IP) MUST BE SET TO AN IMAGINARY
C                       ELEMENT NO. BETWEEN NE+1,ME.)
C          JENP(IEP,IP); LOCAL NODE NUMBER OF IP IN ELEMENT IENP(IEP,IP)
C          NEP     (IP); NUMBER OF ADJACENT ELEMENTS TO NODE IP
C          ME          ; THE MAXIMUM NUMBER  OF ELEMETS
C          MEP         ; THE FIRST DIMENSION OF ARRAY IENP,JENP
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          NP          ; NUMBER OF TOTAL    NODES
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
C          MAXBUF      ; LENGTH OF THE PASSED COMMUNICATION BUFFERS
C                       'BUFSND' AND 'BUFRCV' IN WORDS. 'MAXBUF'
C                        MUST BE NO SMALLER THAN 4 TIMES THE TOTAL
C                        NUMBER OF INTER-CONNECT BOUNDARY NODES IN
C                        THE CALLING TASK
C
C          VALE(IE)    ; ELEMENT VALUE
C
C       (2) OUTPUT
C          VALN(IP)    ; NODE ASSIGNED VALUE
C
C          IERR        ; RETURN CODE TO REPORT ERROR OCCURENCE
C                   0 --- NORMAL TERMINATION
C                   1 --- A FATAL ERROR HAS OCCURED
C
C       (4) WORK
C          BUFSND(IBUF); HOLDS THE VALUES OF THE INTER-CONNECT
C                       BOUNDARY NODE NUMBER IN THE NEIBERING
C                       SUB-DOMAINS AND THE RESIDUALS OF THE
C                       CALLING TASK'S SUB-DOMAIN WHEN SENDING
C                       THE RESIDUALS
C                         
C          BUFRCV(IBUF); HOLDS THE VALUES OF THE INTER-CONNECT
C                       BOUNDAYR NODE NUMBER IN THE CALLING TASK'S
C                       SUB-DOMAIN AND THE RESIDUALS OF THE
C                       NEIBERING SUB-DOMAINS AT THE RECEIT OF THE
C                       RESIDUALS FROM THE NEIBERING SUB-DOMAINS
C
C
C CALCULATE WEIGHTED RESIDUALS
C
C
      CALL SUPUE1(IDIM,VALE,SN,IENP,JENP,NEP,ME,MEP,NE,NP,N,
     *            VALN,IUT0,IERR)
C
      IF(IERR.EQ.1) THEN
          WRITE(IUT0,*) ERMSGC
          RETURN
      ENDIF
C
C
C SUPERIMPOSE NEIBERING ELEMENT CONTRIBUTIONS
C
C
      IF(IPART.GE.1) THEN
          IDUM = 1
          CALL DDCOM3(IPART,IDUM,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *                VALN,VALN,VALN,NP,IUT0,IERR,BUFSND,BUFRCV,MAXBUF)
          IF(IERR.NE.0) THEN
              WRITE(IUT0,*)
              WRITE(IUT0,*) ERMSGC
              RETURN
          ENDIF
      ENDIF
C
C
C INVERSE MASS MATRIX
C
C
      DO 100 IP = 1 , NP
          VALN(IP) = CM(IP)*VALN(IP)
  100 CONTINUE
C
C
      RETURN
      END
