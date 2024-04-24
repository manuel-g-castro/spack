      SUBROUTINE DDMID2(IMODE,MPT,MDOM,NPMID,MAXBUF,
     *                  NPGAT,LPGAT,VGAT,
     *                  NPSCT,LESCT,CSCT,VSCT,
     *                  NGAT,LGAT,NPTGAT,IPMID,IPGAT,
     *                  NSCT,LSCT,NPTSCT,IEMID,IPSCT,
     *                  BUFRCV,BUFSND,IUT0,IERR)
      IMPLICIT NONE
      INTEGER*4 IMODE,
     *          MPT,MDOM,NPMID,MAXBUF,NGAT,NSCT,NPGAT,NPSCT,IUT0,IERR
      INTEGER*4 LGAT(MDOM),NPTGAT(MDOM),IPMID(MPT,MDOM),IPGAT(MPT,MDOM),
     *          LSCT(MDOM),NPTSCT(MDOM),IEMID(MPT,MDOM),IPSCT(MPT,MDOM)
C
      INTEGER*4 LPGAT(NPMID),LESCT(NPMID)
      REAL*4    VGAT(NPMID),VSCT(NPMID),CSCT(3,NPMID)
C
      REAL*4    BUFRCV(MAXBUF),BUFSND(MAXBUF)
      INTEGER*4 NSTART,IDOM,IPT,IRECV,ISEND,MSGTYP,MSGLEN,IPT2,ISTART
C
      INCLUDE 'mpif.h'
C
      INTEGER*4 MAXDOM
      PARAMETER ( MAXDOM = 10000 )
      INTEGER*4 MSGIDS(MAXDOM),MSGSTS(MPI_STATUS_SIZE,MAXDOM)
C
      INTEGER*4 NGATR,NGATS,NSCTR,NSCTS
C
      CHARACTER*60 ERMSGB
     & / ' ## SUBROUTINE DDMID1: FATAL     ERROR OCCURRENCE; RETURNED' /
      CHARACTER*60 EREXP1
     & / ' DIMENSION SIZE OF INTERNAL      ARRAYS IS NOT SUFFICIENT  ' /
      CHARACTER*60 EREXP2
     & / ' DIMENSION SIZE OF PASSED BUFFER ARRAYS IS NOT SUFFICIENT  ' /
      CHARACTER*60 EREXP3
     & / ' RECEIVED NODE NUMBER IS OUT OF THE GLOBAL NODE NUMBER     ' /
C
C
C     COMMUNICATE MID NODE DATA (MPI VERSION)
C 
C     WRITTEN BY Y.YAMADE 2011.01.03
C
C
C     ARGUMENT LISTINGS
C (1) INPUT
C INT *4   MPT         ; THE DIMENSION SIZE OF THE FIRST ELEMENTS OF THE 
C                        PASSED ARRAYS 'IPSET', 'IPGAT', 'IEMID', AND 'IPSCT'
C                        (I.E. THE MAXIMUM NUMBER OF THE MID NODES FOR A 
C                         SINGLE SUB-DOMAIN)
C INT *4   MDOM        ; MAX. NUMBER OF THE SUB-DOMAINS
C INT *4   MB          ; MAX. NUMBER OF GATHER/SCATTER DATA TO BE RETURNED 
C INT *4   MAXBUF      ; LENGTH OF THE PASSED COMMUNICATION BUFFERS
C                        'BUFSND' AND 'BUFRCV' IN WORDS. 
C INT *4   NGAT             ; NUMBER OF DOMAINS FOR GATHERING
C INT *4   LGAT      (IDOM) ; DOMAIN NUMBER     FOR GATHERING
C INT *4   NPTGAT    (IDOM) ; NUMBER OF GATHERED VALUES FOR THE 
C                             DOMAIN 'LGAT'
C INT *4   IPMID (IPT,IDOM) ; MIN NODE NUMBER IN THE DOMAIN 
C                             WHICH WILL RECEIVE GATHERED VALUES 
C INT *4   IPGAT (IPT,IDOM) ; INDICATES POSITION OF GATHERED DATA
C                             IN THE PASSING MID NODE ARRAYS
C REAL*4   BUFGAT      (IB) ; GATHERD VALUE TO BE SENT
C
C INT *4   NSCT             ; NUMBER OF DOMAINS FOR SCATTERING
C INT *4   LSCT      (IDOM) ; DOMAIN NUMBER     FOR SCATTERING
C INT *4   NPTSCT    (IDOM) ; NUMBER OF VALUES TO BE SCATTERED 
C                             FOR THE DOMAIN 'LGAT'
C INT *4   IEMID (IPT,IDOM) ; PARANT ELEMENT NUMBER IN THE DOMAIN
C                             WHICH WILL RECEIVE VALUES AND SCATTER IT 
C INT *4   IPSCT (IPT,IDOM) ; INDICATES POSITION OF DATA TO BE SENT AND 
C                             SCATTERED IN THE PASSING MID NODE ARRAYS
C REAL*4   BUFSCT      (IB) ; VALUE TO BE SENT AND SCATTERED
C REAL*4   CBUFST    (I,IB) ; COEEFECIENTS FOR SCATTERING
C
C INT *4   IUT0        ; FILE NUMBER TO WRITE ERROR MESSAGE
C
C (2) INPUT-OUTPUT
C INT *4   NPGAT       ; NUMBER OF GATHER GATA TO BE SENT AND RECIEVED 
C INT *4   LPGAT  (IB) ; MID NODE NUMBER
C REAL*4   VGAT   (IB) ; GATHERED VALUES AT MID NODES 
C INT *4   NPSCT       ; NUMBER OF SCATTER GATA TO BE SENT AND RECIEVED 
C INT *4   LESCT  (IB) ; PARENT ELEMENT NUMBER
C REAL*4   CSCT (I,IB) ; COEEFECIENTS FOR SCATTERING
C REAL*4   VSCT   (IB) ; VALUES TO BE SCATTERED
C
C (3) OUTPUT
C INT *4   IERR             ; RETURN CODE WHOSE VALUE WILL BE EITHER
C                   0 --- INDICATING SUCCESSFUL TERMINATION
C                OR 1 --- INDICATING OCCURRENCE OF SOME ERROR CONDITIONS
C
      IERR = 0
C
      IF(IMODE.EQ.1) THEN
          NGATS=NGAT
          NGATR=NSCT
          NSCTS=0
          NSCTR=0
      ELSE IF(IMODE.EQ.2) THEN
          NGATS=0
          NGATR=0
          NSCTS=NSCT
          NSCTR=NGAT
      ELSE 
          IERR=1
          RETURN
      ENDIF
C
C
C
C CHECK THE INTERNAL ARRAY SIZE
C
C
C
      IF(NGATR+NGATS+NSCTR+NSCTS.GT.MAXDOM) THEN
          WRITE(IUT0,*) ERMSGB
          WRITE(IUT0,*) EREXP1
          IERR = 1
          RETURN
      ENDIF
C
C
C
C POST FOR SCATTER DATA
C
C
C NOTE THAT FOLLOWING TWO VALUES ARE SAME
C (1) NUM. OF GATHER  DATA TO BE SENT 
C (2) NUM. OF SCATTER DATA TO BE RECIEVED 
C
      NSTART = 1
      DO 1000 IDOM = 1 , NSCTR
          MSGTYP = 2
          IRECV  = LGAT(IDOM)-1
          MSGLEN = 5*NPTGAT(IDOM)
C
          IF(NSTART+MSGLEN-1.GT.MAXBUF) THEN
              WRITE(IUT0,*) ERMSGB
              WRITE(IUT0,*) EREXP2
              IERR = 1
              RETURN
          ENDIF
C
#ifdef PRECEXP
          CALL MPI_IRECV(BUFRCV(NSTART),MSGLEN,MPI_REAL8,IRECV,MSGTYP,
     &                   MPI_COMM_WORLD,MSGIDS(IDOM),IERR)
#else
          CALL MPI_IRECV(BUFRCV(NSTART),MSGLEN,MPI_REAL ,IRECV,MSGTYP,
     &                   MPI_COMM_WORLD,MSGIDS(IDOM),IERR)
#endif
C
          NSTART = NSTART+MSGLEN
 1000 CONTINUE
C
C
C
C POST FOR GATHER DATA
C
C
C
C
C NOTE THAT FOLLOWING TWO VALUES ARE SAME
C (1) NUM. OF SCATTER DATA TO BE SENT 
C (2) NUM. OF GATHER  DATA TO BE RECIEVED 
C
      DO 1100 IDOM = 1 , NGATR
          MSGTYP = 2
          IRECV  = LSCT(IDOM)-1
          MSGLEN = 2*NPTSCT(IDOM)
C
          IF(NSTART+MSGLEN-1.GT.MAXBUF) THEN
              WRITE(IUT0,*) ERMSGB
              WRITE(IUT0,*) EREXP2
              IERR = 1
              RETURN
          ENDIF
C
#ifdef PRECEXP
          CALL MPI_IRECV(BUFRCV(NSTART),MSGLEN,MPI_REAL8,IRECV,MSGTYP,
     &                   MPI_COMM_WORLD,MSGIDS(NSCTR+IDOM),IERR)
#else
          CALL MPI_IRECV(BUFRCV(NSTART),MSGLEN,MPI_REAL ,IRECV,MSGTYP,
     &                   MPI_COMM_WORLD,MSGIDS(NSCTR+IDOM),IERR)
#endif
C
          NSTART = NSTART+MSGLEN
 1100 CONTINUE
C
C
C
C SET UP SEND BUFFER AND SEND GATHER DATA FOR EACH DOMAIN
C
C
C
C NOTE THAT FOLLOWING TWO VALUES ARE SAME
C 
      NSTART = 1
      DO 2000 IDOM = 1 , NGATS
          MSGTYP = 2
          ISEND  = LGAT(IDOM)-1
          MSGLEN = 2*NPTGAT(IDOM)
C
          IF(NSTART+MSGLEN-1.GT.MAXBUF) THEN
              WRITE(IUT0,*) ERMSGB
              WRITE(IUT0,*) EREXP2
              IERR = 1
              RETURN
          ENDIF
#ifdef USE_DETAIL
          call start_collection('ddmid1_2100')
#endif
!ocl norecurrence(BUFSND)
          DO 2100 IPT = 1 , NPTGAT(IDOM)
              BUFSND(NSTART+2*(IPT-1)  ) = IPMID(IPT,IDOM)
              BUFSND(NSTART+2*(IPT-1)+1) = VGAT(IPGAT(IPT,IDOM))
 2100     CONTINUE
#ifdef USE_DETAIL
          call stop_collection('ddmid1_2100')
#endif          
C
#ifdef PRECEXP
          CALL MPI_ISEND(BUFSND(NSTART),MSGLEN,MPI_REAL8,ISEND,MSGTYP,
     &                   MPI_COMM_WORLD,MSGIDS(NSCTR+NGATR+IDOM),IERR)
#else
          CALL MPI_ISEND(BUFSND(NSTART),MSGLEN,MPI_REAL ,ISEND,MSGTYP,
     &                   MPI_COMM_WORLD,MSGIDS(NSCTR+NGATR+IDOM),IERR)
#endif
C
          NSTART = NSTART+MSGLEN
 2000 CONTINUE
C
C
C
C SET UP SEND BUFFER AND SEND SCATTER DATA FOR EACH DOMAIN
C
C
C
      DO 2200 IDOM = 1 , NSCTS
          MSGTYP = 2
          ISEND  = LSCT(IDOM)-1
          MSGLEN = 5*NPTSCT(IDOM)
C
          IF(NSTART+MSGLEN-1.GT.MAXBUF) THEN
              WRITE(IUT0,*) ERMSGB
              WRITE(IUT0,*) EREXP2
              IERR = 1
              RETURN
          ENDIF
C
#ifdef USE_DETAIL
          call start_collection('ddmid1_2300')
#endif
!ocl norecurrence(BUFSND)          
          DO 2300 IPT = 1 , NPTSCT(IDOM)
              BUFSND(NSTART+5*(IPT-1)  ) = IEMID(IPT,IDOM)
              BUFSND(NSTART+5*(IPT-1)+1) = CSCT(1,IPSCT(IPT,IDOM))
              BUFSND(NSTART+5*(IPT-1)+2) = CSCT(2,IPSCT(IPT,IDOM))
              BUFSND(NSTART+5*(IPT-1)+3) = CSCT(3,IPSCT(IPT,IDOM))
              BUFSND(NSTART+5*(IPT-1)+4) = VSCT(  IPSCT(IPT,IDOM))
 2300     CONTINUE
#ifdef USE_DETAIL
          call stop_collection('ddmid1_2300')
#endif          
C
#ifdef PRECEXP
          CALL MPI_ISEND(BUFSND(NSTART),MSGLEN,MPI_REAL8,ISEND,MSGTYP,
     &                   MPI_COMM_WORLD,MSGIDS(NGATR+NSCTR+NGATS+IDOM),
     &                   IERR)
#else
          CALL MPI_ISEND(BUFSND(NSTART),MSGLEN,MPI_REAL ,ISEND,MSGTYP,
     &                   MPI_COMM_WORLD,MSGIDS(NGATR+NSCTR+NGATS+IDOM),
     &                   IERR)
#endif
C
          NSTART = NSTART+MSGLEN
 2200 CONTINUE
C
C
C
C WAIT FOR THE COMPLETION OF ALL THE REQUESTED COMMUNICATIONS
C
C
C
      CALL MPI_WAITALL(NGATR+NSCTR+NGATS+NSCTS,MSGIDS,MSGSTS,IERR)
C
C 
C IMPORTANT NOTES!
C        AFTER A NON-BLOCKING SEND/RECEIVE ROUTINE, SUCH AS 'MPI_ISEND'
C    OR 'MPI_IRECV', IS CALLED, THE COMMUNICATION REQUEST CREATED BY
C    THESE ROUTINES MUST BE FREED EITHER BY EXPLICITLY OR IMPLICITLY.
C   'MPI_REQUEST_FREE' FREES SUCH REQUEST EXPLICITLY, WHILE A ROUTINE
C    WHICH IDENTIFIES COMPLETION OF THE REQUEST, SUCH AS 'MPI_WAIT',
C    'MPI_WAITANY', OR 'MPI_WAITALL' IMPLICITLY FREES THE REQUEST.
C        THIS INTERFACE PROGRAM USES 'MPI_WAITALL' ROUTINES TO FREE SUCH
C    REQUESTS. PAY PARTICULAR ATTENTION IF YOU WISH TO, INSTEAD, USE
C   'MPI_REQUEST_FREE', BECAUSE 'MPI_REQUEST_FREE' FREES THE REQUESTS
C    REGARDLESS OF THE STATE OF THE PREVIOUSLY CALLED COMMUNICATION
C    ROUTINES, THUS SOMETIMES FREES REQUESTS WHICH HAVE NOT BEEN
C    COMPLETED.
C
C
C
C    STORE SCATTER DATA FROM RECIEVE BUFFER 
C
C
C     
#ifdef USE_DETAIL
      call start_collection('ddmid1_3000')
#endif
C
      NPSCT=0
      IF (NSCTR.EQ.0) GOTO 3300
C
      DO 3000 IDOM = 1, NSCTR
         NPSCT=NPSCT+NPTGAT(IDOM)
 3000 CONTINUE
      IF(NPSCT.GT.NPMID) THEN
         WRITE(IUT0,*) ERMSGB
         WRITE(IUT0,*) EREXP2
         IERR = 1
         RETURN
      ENDIF
C
      NSTART=1
      DO 3100 IDOM = 1 , NSCTR
         DO 3200 IPT = 1 , NPTGAT(IDOM)
            IPT2=NSTART+IPT-1
            ISTART=5*(NSTART+IPT-2)+1
            LESCT(  IPT2)=BUFRCV(ISTART  )+0.1
            CSCT (1,IPT2)=BUFRCV(ISTART+1)
            CSCT (2,IPT2)=BUFRCV(ISTART+2)
            CSCT (3,IPT2)=BUFRCV(ISTART+3)
            VSCT (  IPT2)=BUFRCV(ISTART+4)
 3200    CONTINUE
         NSTART=NSTART+NPTGAT(IDOM)
 3100 CONTINUE
C
 3300 CONTINUE
#ifdef USE_DETAIL
      call stop_collection('ddmid1_3000')
#endif      
C
C
C
C    STORE SCATTER DATA FROM RECIEVE BUFFER 
C
C
C
#ifdef USE_DETAIL
      call start_collection('ddmid1_4000')
#endif
C
      NPGAT=0
      IF (NGATR.EQ.0) GOTO 4300
C
      DO 4000 IDOM = 1, NGATR
         NPGAT=NPGAT+NPTSCT(IDOM)
 4000 CONTINUE
      IF(NPGAT.GT.NPMID) THEN
         WRITE(IUT0,*) ERMSGB
         WRITE(IUT0,*) EREXP2
         IERR = 1
         RETURN
      ENDIF
C
      NSTART=1
      DO 4100 IDOM = 1 , NGATR        
         DO 4200 IPT = 1 , NPTSCT(IDOM)
            IPT2=NSTART+IPT-1
            ISTART=2*(NSTART+IPT-2)+1
            LPGAT(IPT2)=BUFRCV(ISTART  )+0.1
            VGAT (IPT2)=BUFRCV(ISTART+1)
 4200    CONTINUE
         NSTART=NSTART+NPTSCT(IDOM)
 4100 CONTINUE
C
 4300 CONTINUE
#ifdef USE_DETAIL
      call stop_collection('ddmid1_4000')
#endif      
C
      RETURN
      END
