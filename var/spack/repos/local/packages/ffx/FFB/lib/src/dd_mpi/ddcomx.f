      SUBROUTINE DDCOMX(IPART,IDIM,LDOM,NBPDOM,NDOM,IPSLF,IPSND,MBPDOM,
     *                  FX,FY,FZ,NP,IUT0,IERR,BUFSND,BUFRCV,MAXBUF)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION LDOM(NDOM),NBPDOM(NDOM),IPSLF(MBPDOM,NDOM),
     1          IPSND(MBPDOM,NDOM), FX(NP),FY(NP),FZ(NP),
     2          BUFSND(MAXBUF),BUFRCV(MAXBUF)
C
      INCLUDE 'mpif.h'
C
      PARAMETER ( MAXDOM = 10000 )
      INTEGER*4 MSGIDS(MAXDOM),MSGSTS(MPI_STATUS_SIZE,MAXDOM)
C
C
      CHARACTER*60 ERMSGB
     & / ' ## SUBROUTINE DDCOMX: FATAL     ERROR OCCURRENCE; RETURNED' /
      CHARACTER*60 EREXP1
     & / ' DIMENSION SIZE OF INTERNAL      ARRAYS IS NOT SUFFICIENT  ' /
      CHARACTER*60 EREXP2
     & / ' DIMENSION SIZE OF PASSED BUFFER ARRAYS IS NOT SUFFICIENT  ' /
      CHARACTER*60 EREXP3
     & / ' RECEIVED NODE NUMBER IS OUT OF THE GLOBAL NODE NUMBER     ' /
#ifdef USE_TIMER
      include 'timer.h'
      real*8 :: tstart0
#endif      
C
C
C      EXCHANGE X, Y, AND Z RESIDUALS AMONG THE NEIGHBORING SUB-DOMAINS
C     AND SUPERIMPOSE THE EXCHANGED RESIDUALS TO THE CALLING TASK'S
C     RESIDUALS, FOR DOMAIN-DECOMPOSITION PROGRAMMING MODEL
C
C                            ( MPI VERSION )
C
C
C NOTE 1; ALL 'MPI' ROUTINES RETURN AN ERROR CODE 'IERR' WHICH INDICATES
C       THE STATUS OF ITS EXECUTION. THIS SUBROUTINE IGNORES SUCH ERROR
C       CODE AND RETURNS THE SEQUENCE TO THE CALLING PROGRAM UNIT,
C       REGARDLESS OF THE VALUE OF THE 'MPI' RETURN CODE.
C
C NOTE 2; SOME COMPILERS, SUCH AS OFORT90 IN HI-UXMPP, SUPPORT AUTOMATIC
C       PRECISION EXPANSION, WHERE ALL THE CONSTANTS, VARIABLES AND
C       ARRAYS OF 4-BYTE PRECISION (REAL*4) ARE AUTOMATICALLY CONVERTED
C       TO THOSE OF 8-BYTE PRECISION (REAL*8) WITH UNFORMATTED I/O DATA 
C       BEING KEPT AS THEY ARE (IF SO SPECIFIED). WHEN USING SUCH 
C       FEATURES (FUNCTIONS) OF A COMPILER, SPECIAL CARE IS NEEDED
C       BECAUSE A COUPLE OF MPI SUBROUTINES CALLED IN THIS SUBPROGRAM
C       ACCEPT THE DATA TYPE (DATA PRECISION) AS THEIR INPUT AND
C       PERFORM THE OPERATIONS ACCORDING TO THIS INPUT VALUE. THIS
C       INTERFACE SUPPORTS THE AUTOMATIC PRECISION EXPANSION MENTIONED
C       ABOVE. IF YOU WISH TO USE SUCH FEATURE, ADD '-DPRECEXP' OPTION
C       WHEN INVOKING 'cpp' FOR PRI-PROCESSING THIS SOURCE PROGRAM FILE.
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C INT *4   IPART       ; SUB-DOMAIN NUMBER THAT THE CALLING TASK IS
C                       TAKING CARE OF
C           NOTES ; ARGUMENT 'IPART' IS NOT CURRENTLY USED. IT IS
C                  RETAINED FOR A POSSIBLE FUTURE USE.
C INT *4   IDIM             ; SPACE DIMENSION ( 1, 2, OR 3 )
C INT *4   LDOM      (IDOM) ; NEIGHBORING SUB-DOMAIN NUMBER
C INT *4   NBPDOM    (IDOM) ; NUMBER OF INTER-CONNECT BOUNDARY NODES
C                            SHARING WITH THE IDOM'TH NEIGHBORING
C                            SUB-DOMAIN, LDOM(IDOM)
C INT *4   NDOM             ; NUMBER OF THE NEIGHBORING SUB-DOMAINS
C INT *4   IPSLF (IBP,IDOM) ; INTER-CONNECT BOUNDARY NODE NUMBER IN THE
C                            CALLING TASK'S SUB-DOMAIN, FOR THE IDOM'TH
C                            NEIGHBORING SUB-DOMAIN, LDOM(IDOM)
C INT *4   IPSND (IBP,IDOM) ; INTER-CONNECT BOUNDARY NODE NUMBER IN THE
C                            SUB-DOMAIN THAT IS RECEIVING THE CALLING
C                            TASK'S RESIDUALS.
C INT *4   MBPDOM           ; THE DIMENSION SIZE OF THE FIRST ELEMENTS
C                            OF THE PASSED ARRAYS 'IPSLF' AND 'IPSND'
C                            (I.E. THE MAXIMUM NUMBER OF THE
C                             INTER-CONNECT BOUNDARY NODES FOR A
C                             NEIGHBORING SUB-DOMAIN)
C INT *4   NP               ; NUMBER OF THE TOTAL NODES IN THE CALLING
C                            TASK'S SUB-DOMAIN
C INT *4   IUT0             ; FILE NUMBER TO WRITE ERROR MESSAGE
C INT *4   MAXBUF           ; LENGTH OF THE PASSED COMMUNICATION BUFFERS
C                            'BUFSND' AND 'BUFRCV' IN WORDS. 'MAXBUF'
C                             MUST BE NO SMALLER THAN 4 TIMES THE TOTAL
C                             NUMBER OF INTER-CONNECT BOUNDARY NODES IN
C                             THE CALLING TASK
C
C       (2) OUTPUT
C INT *4   IERR             ; RETURN CODE WHOSE VALUE WILL BE EITHER
C                   0 --- INDICATING SUCCESSFUL TERMINATION
C                OR 1 --- INDICATING OCCURRENCE OF SOME ERROR CONDITIONS
C
C       (3) INPUT-OUTPUT
C REAL*4   FX(IP)           ; X-DIRECTION RESIDUAL VECTOR
C REAL*4   FY(IP)           ; Y-DIRECTION RESIDUAL VECTOR
C REAL*4   FZ(IP)           ; Z-DIRECTION RESIDUAL VECTOR
C
C       (4) WORK
C REAL*4   BUFSND(IBUF)     ; HOLDS THE VALUES OF THE INTER-CONNECT
C                            BOUNDARY NODE NUMBER IN THE NEIGHBORING
C                            SUB-DOMAINS AND THE RESIDUALS OF THE
C                            CALLING TASK'S SUB-DOMAIN WHEN SENDING
C                            THE RESIDUALS
C                         
C REAL*4   BUFRCV(IBUF)     ; HOLDS THE VALUES OF THE INTER-CONNECT
C                            BOUNDARY NODE NUMBER IN THE CALLING TASK'S
C                            SUB-DOMAIN AND THE RESIDUALS OF THE
C                            NEIGHBORING SUB-DOMAINS AT THE RECEIPT OF
C                            THE RESIDUALS FROM THE NEIGHBORING
C                            SUB-DOMAINS



      !-----------------------------------------------------
      ! before barrier
      !-----------------------------------------------------
#ifdef USE_BARRIER
#ifdef USE_TIMER
      tstart   = MPI_WTIME()
      tstart0  = tstart
      nddcomxb = nddcomxb + 1
#endif      
      call MPI_BARRIER(MPI_COMM_WORLD, IERR)
#ifdef USE_TIMER
      tend = MPI_WTIME()
      tddcomxb = tddcomxb + (tend - tstart)
#endif      
#endif


      
      !-----------------------------------------------------
      ! ddcomx1 (pre-check part)
      !-----------------------------------------------------
#ifdef USE_TIMER
      tstart  = MPI_WTIME()
      nddcomx = nddcomx + 1
#endif
#ifdef USE_DETAIL
C     call start_collection('ddcomx1')
#endif      
      IERR = 0

      IF(IDIM.EQ.0) THEN
          NSKIP=1
      ELSE IF(IDIM.EQ.1) THEN
          NSKIP=1
      ELSE IF(IDIM.EQ.2) THEN 
          NSKIP=2
      ELSE IF(IDIM.EQ.3) THEN 
          NSKIP=3
      ELSE
          WRITE(IUT0,*) EREXP1
          IERR = 1
          RETURN
      ENDIF
C
C
C CHECK THE INTERNAL ARRAY SIZE
C
C
C
      IF(2*NDOM.GT.MAXDOM) THEN
          WRITE(IUT0,*) ERMSGB
          WRITE(IUT0,*) EREXP1
          IERR = 1
          RETURN
      ENDIF

#ifdef USE_DETAIL
      call stop_collection('ddcomx1')
#endif
#ifdef USE_TIMER
      tend = MPI_WTIME()
      tddcomx1 = tddcomx1 + (tend - tstart)
      tstart = tend
#endif



      !-----------------------------------------------------
      ! ddcomx2 (mpi_irecv)
      !-----------------------------------------------------
C
C
C
C POST ALL THE EXPECTED RECEIVES
C
C
C
      NSTART = 1
      DO 110 IDOM = 1 , NDOM
          MSGTYP = 1
          IRECV  = LDOM(IDOM)-1
          MSGLEN = NSKIP*NBPDOM(IDOM)
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
  110 CONTINUE
#ifdef USE_TIMER
      tend     = MPI_WTIME()
      tddcomx2 = tddcomx2 + (tend - tstart)
      tstart   = tend
#endif



      !-----------------------------------------------------
      ! ddcomx3 (packing to send buffer
      !-----------------------------------------------------
#ifdef USE_DETAIL
C     call start_collection('ddcomx3')
#endif
C
!      CALL MPI_BARRIER(MPI_COMM_WORLD,IERR)
C
C
C
C SET UP THE SEND BUFFER
C
C
CC    CALL FTRACE_REGION_BEGIN("ddcom3:200-210")
!CDIR PARALLEL DO PRIVATE(NSTART,IP,IPS)
      DO 210 IDOM = 1 , NDOM
         NSTART  = 0
         DO 205 ITMP = 2 , IDOM
            NSTART = NSTART + NBPDOM(ITMP-1)*NSKIP
 205     CONTINUE
!CDIR NOINNER
         IF(IDIM.EQ.0) THEN
!ocl norecurrence(BUFSND)
            DO IBP=1,NBPDOM(IDOM)
               NSTART2 = NSTART + NSKIP * (IBP-1)
               IP      = IPSLF(IBP,IDOM)
               IPS     = IPSND(IBP,IDOM)
               BUFSND(NSTART2+1) = IPS
            ENDDO
         ELSE IF(IDIM.EQ.1) THEN
!ocl norecurrence(BUFSND)
            DO IBP=1,NBPDOM(IDOM)
               NSTART2 = NSTART + NSKIP * (IBP-1)
               IP      = IPSLF(IBP,IDOM)
               BUFSND(NSTART2+1) = FX(IP)
            ENDDO
         ELSE IF(IDIM.EQ.2) THEN
!ocl norecurrence(BUFSND)
            DO IBP=1,NBPDOM(IDOM)
               NSTART2 = NSTART + NSKIP * (IBP-1)
               IP      = IPSLF(IBP,IDOM)
               BUFSND(NSTART2+1) = FX(IP)
               BUFSND(NSTART2+2) = FY(IP)
            ENDDO
         ELSE IF(IDIM.EQ.3) THEN
!ocl norecurrence(BUFSND)
            DO IBP=1,NBPDOM(IDOM)
               NSTART2 = NSTART + NSKIP * (IBP-1)
               IP      = IPSLF(IBP,IDOM)
               BUFSND(NSTART2+1) = FX(IP)
               BUFSND(NSTART2+2) = FY(IP)
               BUFSND(NSTART2+3) = FZ(IP)
            ENDDO
         ENDIF
 210  CONTINUE
#ifdef USE_DETAIL
      call stop_collection('ddcomx3')
#endif
#ifdef USE_TIMER
      tend     = MPI_WTIME()
      tddcomx3 = tddcomx3 + (tend - tstart)
      tstart   = tend
#endif



      !-----------------------------------------------------
      ! ddcom4 (MPI_SEND)
      !-----------------------------------------------------
C
C
C
C SEND THE RESIDUALS
C
C
C
      NSTART = 1
      DO 220 IDOM = 1 , NDOM
          MSGTYP = 1
          ISEND  = LDOM(IDOM)-1
          MSGLEN = NSKIP*NBPDOM(IDOM)

#ifdef PRECEXP
          CALL MPI_ISEND(BUFSND(NSTART),MSGLEN,MPI_REAL8,ISEND,MSGTYP,
     &                   MPI_COMM_WORLD,MSGIDS(NDOM+IDOM),IERR)
#else
          CALL MPI_ISEND(BUFSND(NSTART),MSGLEN,MPI_REAL ,ISEND,MSGTYP,
     &                   MPI_COMM_WORLD,MSGIDS(NDOM+IDOM),IERR)
#endif
C
          NSTART = NSTART+MSGLEN 
  220 CONTINUE
#ifdef USE_TIMER
      tend     = MPI_WTIME()
      tddcomx4 = tddcomx4 + (tend - tstart)
      tstart   = tend
#endif


      
      !-----------------------------------------------------
      ! ddcomx5 (MPI_WAITALL)
      !-----------------------------------------------------
C
C
C
C WAIT FOR THE COMPLETION OF ALL THE REQUESTED COMMUNICATIONS
C
C
C
#ifdef USE_DETAIL
      call start_collection('ddcomx5')
#endif
      CALL MPI_WAITALL(2*NDOM,MSGIDS,MSGSTS,IERR)
#ifdef USE_DETAIL
      call stop_collection('ddcomx5')
#endif
#ifdef USE_TIMER
      tend     = MPI_WTIME()
      tddcomx5 = tddcomx5 + (tend - tstart)
      tstart   = tend
#endif



      !-----------------------------------------------------
      ! ddcomx6 (un-packing from receive buffer)
      !-----------------------------------------------------
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
CCTTDEBG
CCTTDEBG END
C
C SUPERIMPOSE THE RECEIVED RESIDUALS
C
C
      NSTART = 0
      DO IDOM = 1 , NDOM
         IF(IDIM .EQ. 0) THEN
            DO IBP = 1, NBPDOM(IDOM)
               NSTART2 = NSTART + NSKIP * (IBP-1)
               IPSND(IBP,IDOM)=BUFRCV(NSTART2+1)+0.1
            ENDDO
            NSTART = NSTART + NSKIP * NBPDOM(IDOM)
            
         ELSE IF(IDIM .EQ. 1) THEN
!ocl norecurrence(FX)
            DO IBP = 1, NBPDOM(IDOM)
               IP = IPSND(IBP,IDOM)
               IF(IP.LT.1 .OR. IP.GT.NP) THEN
                  IERR = 1
               ENDIF
               NSTART2 = NSTART + NSKIP * (IBP-1)
               FX(IP) = FX(IP) + BUFRCV(NSTART2+1)
            ENDDO
            NSTART = NSTART + NSKIP * NBPDOM(IDOM)
            
         ELSE IF(IDIM .EQ. 2) THEN
!ocl norecurrence(FX,FY)
            DO IBP = 1, NBPDOM(IDOM)
               IP = IPSND(IBP,IDOM)
               IF(IP.LT.1 .OR. IP.GT.NP) THEN
                  IERR = 1
               ENDIF
               NSTART2 = NSTART + NSKIP * (IBP-1)
               FX(IP) = FX(IP) + BUFRCV(NSTART2+1)
               FY(IP) = FY(IP) + BUFRCV(NSTART2+2)
            ENDDO
            NSTART = NSTART + NSKIP * NBPDOM(IDOM)
            
         ELSE IF(IDIM .EQ. 3) THEN
#ifdef USE_TIMER
C           call start_collection('ddcomx.S.I.3D')
#endif            
!ocl norecurrence(FX,FY,FZ)
            DO IBP = 1, NBPDOM(IDOM)
               IP = IPSND(IBP,IDOM)
               IF(IP.LT.1 .OR. IP.GT.NP) THEN
                  IERR = 1
               ENDIF
               NSTART2 = NSTART + NSKIP * (IBP-1)
               FX(IP) = FX(IP) + BUFRCV(NSTART2+1)
               FY(IP) = FY(IP) + BUFRCV(NSTART2+2)
               FZ(IP) = FZ(IP) + BUFRCV(NSTART2+3)
            ENDDO
#ifdef USE_TIMER
C           call stop_collection('ddcomx.S.I.3D')
#endif            
            NSTART = NSTART + NSKIP * NBPDOM(IDOM)
         ENDIF
CCTTDEBG END
C     
C          IF(IDIM.EQ.1) THEN
C            FX(IP) = FX(IP)+BUFRCV(NSTART+1)
C          ELSE IF(IDIM .EQ. 2) THEN 
C            FX(IP) = FX(IP)+BUFRCV(NSTART+1)
C            FY(IP) = FY(IP)+BUFRCV(NSTART+2)
C          ELSE IF(IDIM .EQ. 3) THEN 
C            FX(IP) = FX(IP)+BUFRCV(NSTART+1)
C            FY(IP) = FY(IP)+BUFRCV(NSTART+2)
C            FZ(IP) = FZ(IP)+BUFRCV(NSTART+3)
C          ENDIF
      ENDDO
C
#ifdef USE_TIMER
      tend     = MPI_WTIME()
      tddcomx6 = tddcomx6 + (tend - tstart)
      tstart   = tend
#endif



      !-----------------------------------------------------
      ! after barrier
      !-----------------------------------------------------
#ifdef USE_BARRIER
#ifdef USE_TIMER
      tstart   = MPI_WTIME()
      nddcomxa = nddcomxa + 1
#endif      
      call MPI_BARRIER(MPI_COMM_WORLD, IERR)
#ifdef USE_TIMER
      tend = MPI_WTIME()
      tddcomxa = tddcomxa + (tend - tstart)
#endif      
#endif


      
      IF(IERR .eq. 1) THEN
        WRITE(IUT0,*) ERMSGB
        WRITE(IUT0,*) EREXP3
        RETURN
      ENDIF

#ifdef USE_TIMER
      tend = MPI_WTIME()
      tddcomx0 = tddcomx0 + (tend - tstart0)
#endif      
C
C     IPART = IPART
      RETURN
      END
