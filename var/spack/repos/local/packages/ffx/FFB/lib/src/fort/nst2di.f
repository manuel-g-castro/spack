C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    NST2DI                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE NST2DI(IUT0 , IUT5  , IERR  , MAXB  , MTIME , MRK ,MRL,
     *         MDAMP , MSMPL , MRESV , COMGEN, FILEMS, FILEIF, 
     *         FILEFF, FILEIM, FILEFM, FILEGF, FILEGM, FILEHS,
     *         ITYPE , IMODEL, IFORM , IPRESS, IMASS , IGAUSS,
     *         VISCM , VKAPM , PRT   , DT    , C     , BETA  ,
     *         EPS   , ALF   , NMAX  , NMIN  ,
     *         NTIME , INTFSV, INTPRN, ISTART,
     *         TFINAL, UFINAL, VFINAL,
     *         JMARK , JDRAG , JDAMP , JSMPL , JSHIFT,
     *         NBU   , NBV   , NBP   , NBT   ,
     *         LBU   , LBV   , LBP   , LBT   ,
     *         BBU   , BBV   , BBP   , BBT   , NEN   , LEN   , LSN   ,
     *         NPAIR , LPAIR , NSRC  , LSRC  , BSRC  ,
     *         INTMSV, IRELES, JVALID,
     *         INTRLS, XMR   , YMR   , NMR   , NCYCLE,
     *         XMINRS, YMINRS, XMAXRS, YMAXRS, NMARK ,
     *         XMINMK, YMINMK, XMAXMK, YMAXMK,
     *         AFRONT, NEB   , LEB   , LSB   ,
     *         NDAMP , LDAMP , FACT  , NSHIFT, LSHIFT ,
     *         NSMPL , LSMPL , XSMPL  ,YSMPL , DATHIS , DATEFD, NHIST)
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION LBU(MAXB),LBV(MAXB),LBP(MAXB),LBT(MAXB),
     1          BBU(MAXB),BBV(MAXB),BBP(MAXB),BBT(MAXB),
     2          LEN(MAXB),LSN(MAXB),
     3          LPAIR(2,MAXB),LSRC(MAXB),BSRC(MAXB),
     4          XMR(MRK),YMR(MRK),LEB(MAXB),LSB(MAXB),
     5          LDAMP(MDAMP),FACT(MDAMP),LSHIFT(MAXB),
     6          LSMPL(2,MSMPL),XSMPL(MSMPL),YSMPL(MSMPL)
C
      CHARACTER*60 FILEMS, FILEIF, FILEFF, FILEIM, FILEFM, 
     1             FILEGF, FILEGM, FILEHS
C
      CHARACTER*40 COMGEN
      CHARACTER*40 DATHIS(MRESV+MSMPL)
      CHARACTER*20 DATEFD(MRESV+MSMPL)
      CHARACTER* 4 COMSKP, REF 
      DATA REF / 'SKIP' /
C
      CHARACTER*72 ERMSG
     & /' *** SUBROUTINE NST2DI REPORTS A FATAL ERROR OCCURENCE ***' /
      CHARACTER*72 EREXP1
     & /' TOO MANY TIME INTEGRATIONS       HAVE BEEN SPECIFIED     ' /
      CHARACTER*72 EREXP2
     & /' TOO MANY BOUNDARY NODES          HAVE BEEN SPECIFIED     ' /
      CHARACTER*72 EREXP3
     & /' TOO MANY MARKER RELEASING POINTS HAVE BEEN SPECIFIED     ' /
      CHARACTER*72 EREXP4
     & /' TOO MANY MARKERS TO BE TRACED    HAVE BEEN SPECIFIED     ' /
      CHARACTER*72 EREXP5
     & /' TOO MANY ELEMENTS SPECIFIED TO DAMP EDDY VISCOSITY       ' /
      CHARACTER*72 EREXP6
     & /' TOO MANY SAMPLING POINTS         HAVE BEEN SPECIFIED     ' /
C
C
C      READ CALCULATION PARAMETERS
C         ( 2-D CALCULATION )
C
C
C     ARGUMENT LISTINGS
C      ( SEE MAIN PROGRAM ' NST2D ' )
C
C
      IERR = 0

      REWIND IUT5
       READ(IUT5,6400) COMGEN
       READ(IUT5,   *) ITYPE  , IMODEL , IFORM
       READ(IUT5,   *) IPRESS , IMASS  , IGAUSS
       READ(IUT5,   *) VISCM  , VKAPM  , PRT
       READ(IUT5,   *) DT     , C      , BETA
       READ(IUT5,   *) EPS    , ALF    , NMAX   , NMIN
       READ(IUT5,   *) NTIME  , INTFSV , INTPRN , ISTART
       READ(IUT5,   *) TFINAL , UFINAL , VFINAL
       READ(IUT5,   *) JMARK  , JDRAG  , JDAMP  , JSMPL , JSHIFT
C
       IF(NTIME.GT.MTIME) THEN
           WRITE(IUT0,6300) ERMSG
           WRITE(IUT0,6300) EREXP1 
           IERR = 1 
           RETURN
       ENDIF
C
       READ(IUT5,6500) COMSKP
       READ(IUT5,   *) NBU , NBV , NBP , NBT , NBS , NEN , NPAIR , NSRC
C
       IF(MAX(NBU,NBV,NBP,NBT,NEN,NPAIR,NSRC).GT.MAXB) THEN
           WRITE(IUT0,6300) ERMSG
           WRITE(IUT0,6300) EREXP2 
           IERR = 1 
           RETURN
       ENDIF
C
       IF(NBU.GE.1) THEN
           READ(IUT5,6500) COMSKP
           READ(IUT5,8300) ( LBU(IB) , BBU(IB) , IB = 1 , NBU )
       ENDIF
C
       IF(NBV.GE.1) THEN
           READ(IUT5,6500) COMSKP
           READ(IUT5,8300) ( LBV(IB) , BBV(IB) , IB = 1 , NBV )
       ENDIF
C
       IF(NBP.GE.1) THEN
           READ(IUT5,6500) COMSKP
           READ(IUT5,8300) ( LBP(IB) , BBP(IB) , IB = 1 , NBP )
       ENDIF
C
       IF(NBT.GE.1) THEN
           READ(IUT5,6500) COMSKP
           READ(IUT5,8300) ( LBT(IB) , BBT(IB) , IB = 1 , NBT )
       ENDIF
C
       IF(NBS.GE.1) THEN
           READ(IUT5,6500) COMSKP
           READ(IUT5,8300) ( IDUM    , DUM     , IB = 1 , NBS )
       ENDIF
C
       IF(NEN.GE.1) THEN
           READ(IUT5,6500) COMSKP
           READ(IUT5,8000) ( LEN (IB) , LSN(IB) , IB = 1 , NEN )
       ENDIF
C
       IF(NPAIR.GE.1) THEN
           READ(IUT5,6500) COMSKP
           READ(IUT5,8000) ((LPAIR(I,IPAIR),I=1,2) , IPAIR = 1 , NPAIR)
       ENDIF
C
       IF(NSRC .GE.1) THEN
           READ(IUT5,6500) COMSKP
           READ(IUT5,8300) ( LSRC(ISRC) , BSRC(ISRC) , ISRC = 1 , NSRC )
       ENDIF
C
       READ(IUT5,6500) COMSKP
       IF(COMSKP.NE.REF) THEN
           READ(IUT5,   *) INTMSV , IRELES , JVALID
C
           IF(IRELES.EQ.1) THEN
               READ(IUT5,   *) INTRLS , NMR
               IF(NMR.GT.MRL) THEN
                   WRITE(IUT0,6300) ERMSG
                   WRITE(IUT0,6300) EREXP3 
                   IERR = 1 
                   RETURN
               ENDIF
               READ(IUT5,8100) ( XMR(IMR) , YMR(IMR) , IMR = 1 , NMR )
           ENDIF
C
           IF(IRELES.EQ.2) THEN
               READ(IUT5,   *) XMINRS , YMINRS , XMAXRS , YMAXRS , NMARK
               IF(NMARK.GT.MRK) THEN
                   WRITE(IUT0,6300) ERMSG
                   WRITE(IUT0,6300) EREXP4 
                   IERR = 1 
                   RETURN
               ENDIF
           ENDIF
C
           IF(IRELES.EQ.3) THEN
               READ(IUT5,   *) XMINRS , YMINRS , XMAXRS , YMAXRS , NMR
               READ(IUT5,   *) INTRLS , NCYCLE
               IF(NMR.GT.MRL) THEN
                   WRITE(IUT0,6300) ERMSG
                   WRITE(IUT0,6300) EREXP3 
                   IERR = 1 
                    RETURN
               ENDIF
           ENDIF
C
           IF(JVALID.EQ.1) THEN
               READ(IUT5,   *) XMINMK , YMINMK , XMAXMK , YMAXMK
           ENDIF
       ENDIF
C
       READ(IUT5,6500) COMSKP
       IF(COMSKP.NE.REF) THEN
           READ(IUT5,   *) AFRONT , NEB
           READ(IUT5,8000) ( LEB (IB) , LSB(IB) , IB = 1 , NEB )
       ENDIF
C
       READ(IUT5,6500) COMSKP
       IF(COMSKP.NE.REF) THEN
           READ(IUT5,   *) NDAMP
           IF(NDAMP.GT.MDAMP) THEN
               WRITE(IUT0,6300) ERMSG
               WRITE(IUT0,6300) EREXP5 
               IERR = 1 
               RETURN
           ENDIF
           READ(IUT5,8300) (LDAMP(IDAMP),FACT(IDAMP),IDAMP = 1 , NDAMP)
       ENDIF
C
       NHIST = MRESV
       READ(IUT5,6500) COMSKP
       IF(COMSKP.NE.REF) THEN
           READ(IUT5,   *) NSMPL
           IF(NSMPL.GT.MSMPL) THEN
               WRITE(IUT0,6300) ERMSG
               WRITE(IUT0,6300) EREXP6 
               IERR = 1 
               RETURN
           ENDIF
           DO 10 ISMPL = 1 , NSMPL
               READ(IUT5,   *) LSMPL(1,ISMPL),XSMPL(ISMPL),YSMPL(ISMPL)
               NHIST = NHIST+1
               WRITE(DATHIS(NHIST),7000) DATEFD(LSMPL(1,ISMPL)),
     &                                   XSMPL(ISMPL),YSMPL(ISMPL)
   10      CONTINUE
       ENDIF
C
       READ(IUT5,6500) COMSKP
       IF(COMSKP.NE.REF) THEN
           READ(IUT5,   *) NSHIFT
           IF(NSHIFT.GT.MAXB) THEN
               WRITE(IUT0,6300) ERMSG
               WRITE(IUT0,6300) EREXP5 
               IERR = 1 
               RETURN
           ENDIF
           READ(IUT5,8000) ( LSHIFT(I) , I = 1 , NSHIFT )
       ENDIF
C
       READ(IUT5,6500) COMSKP
       READ(IUT5,'(A60)') FILEMS
       READ(IUT5,'(A60)') FILEIF
       READ(IUT5,'(A60)') FILEFF
       READ(IUT5,'(A60)') FILEIM
       READ(IUT5,'(A60)') FILEFM
       READ(IUT5,'(A60)') FILEGF
       READ(IUT5,'(A60)') FILEGM
       READ(IUT5,'(A60)') FILEHS
C
      IDUM = IDUM 
      DUM  = DUM
C
C
      RETURN
 6300 FORMAT(A72)
 6400 FORMAT(A40)
 6500 FORMAT(A4 )
 7000 FORMAT(A16, 3H X=,1PE9.2, 3H Y=,1PE9.2)
 8000 FORMAT(14I5)
 8100 FORMAT(6D12.5)
CCYY--- 
CCYY 8300 FORMAT(4(I5,X,D12.5))
 8300 FORMAT(4(I5,1X,D12.5))
CCYY---
      END
