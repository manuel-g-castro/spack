      SUBROUTINE SETFFO(IPART,N2,NE,NP,NEX,NS,NSP,N2D,LOCAL,NODE,
     *                  NEFFO,LEFFO1,LEFFO2,
     *                  NPFFO,LPFFO1,LPFFO2,
     *                  NFFO,MEFFOI,NDGCOE,NEFFOI,LEFFOI,
     *                  X,Y,Z,VOL,FFOVOL,FFOAIN,FFODIM,FFODIR,
     *                  AXFFOI,AYFFOI,AZFFOI,
     *                  LWRK1,LWRK2,LMASK,AFFO,IUT6,IUT0,IERR)  
      IMPLICIT NONE
      INTEGER*4 IPART,N2,NE,NP,NEX,NS,NSP,N2D,
     *          NFFO,NEFFO,NPFFO,
     *          MEFFOI,IUT6,IUT0,IERR
      INTEGER*4 LOCAL(NSP,NS,4),NODE(N2,NE),
     *          LEFFO1(NEFFO),LEFFO2(NEFFO),
     *          LPFFO1(NPFFO),LPFFO2(NPFFO),
     *          NDGCOE(NFFO),NEFFOI(NFFO),
     *          LEFFOI(2,MEFFOI,NFFO)
      REAL*8    X(NP),Y(NP),Z(NP)
      REAL*4    VOL(NE),
     *          FFOVOL(NFFO),FFOAIN(NFFO),FFODIM(NFFO),FFODIR(3,NFFO),
     *          AXFFOI(MEFFOI,NFFO),AYFFOI(MEFFOI,NFFO),
     *          AZFFOI(MEFFOI,NFFO)
C
      INTEGER*4 LWRK1(NE),LWRK2(NE),LMASK(NE)
      REAL*4    AFFO(MEFFOI) 
C
      INTEGER*4 MLST
      DATA MLST /2/
C
      INTEGER*4 I,IP,IE,IFFO,IBE,IBP,NPIN
      REAL*4    BUF, BUFX ,BUFY ,BUFZ,
     *          BUFA,BUFXA,BUFYA,BUFZA
CC
CC
      IERR=0
CC
CC [1] INITILIZE
CC
      DO 1000 IFFO=1,NFFO
          FFOVOL(IFFO)=0.0E0   
          FFOAIN(IFFO)=0.0E0   
          FFODIM(IFFO)=0.0E0   
 1000 CONTINUE   
CC
CC [2] SET VOLUME
CC
      DO 2000 IBE=1,NEFFO
          IE  =LEFFO1(IBE)
          IFFO=LEFFO2(IBE)
          FFOVOL(IFFO)=FFOVOL(IFFO)+VOL(IE)
 2000 CONTINUE
      IF(IPART.GE.1) THEN
          DO 2100 IFFO=1,NFFO
              CALL DDCOM2(FFOVOL(IFFO),BUFA)
              FFOVOL(IFFO) = BUFA
 2100     CONTINUE
      ENDIF
CC
CC [3] MAKE INLET SURFACE LIST
CC
          WRITE(IUT6,*)  '** SUMMARY OF FLUID FORCE OBJECTS **'
          WRITE(IUT6,'(A3,3A12,3A10)')
     *          'ID','VOLUME','SURFACE','LENGTH',
     *          'X-DIR.','Y-DIR.','Z-DIR.'
C 
      DO 3000 IFFO=1,NFFO
C
          NPIN=0
          DO 3100 IBP=1,NPFFO
          IF(LPFFO2(IBP).NE.IFFO) GOTO 3100
              NPIN=NPIN+1
              IP=LPFFO1(IBP)
              LWRK1(NPIN)=IP
 3100     CONTINUE
C
          DO 3200 IE=1,NE
              LMASK(IE)=0
 3200     CONTINUE
          DO 3300 IBE=1,NEFFO
              IF(LEFFO2(IBE).NE.IFFO) GOTO 3300
              IE  =LEFFO1(IBE)
              LMASK(IE)=1     
 3300     CONTINUE
C
          CALL SRFEX2(MEFFOI,MLST,NE,NP,N2,NEX,NS,NSP,N2D,
     *                LWRK1,NPIN,LOCAL,NODE,LMASK,
     *                LEFFOI(1,1,IFFO),NEFFOI(IFFO),
     *                NP,LWRK2,IUT0,IERR)
          CALL CSIN3X(LOCAL,X,Y,Z,NODE,NE,NP,N2,NEX,NS,NSP,
     *                LEFFOI(1,1,IFFO),NEFFOI(IFFO),
     *                AXFFOI(1,IFFO),AYFFOI(1,IFFO),AZFFOI(1,IFFO),
     *                AFFO)
C
          DO 3400 IBE=1,NEFFOI(IFFO)
              AXFFOI(IBE,IFFO)=AXFFOI(IBE,IFFO)*AFFO(IBE)
              AYFFOI(IBE,IFFO)=AYFFOI(IBE,IFFO)*AFFO(IBE)
              AZFFOI(IBE,IFFO)=AZFFOI(IBE,IFFO)*AFFO(IBE)
 3400     CONTINUE
C
          BUFX=0.0E0 
          BUFY=0.0E0 
          BUFZ=0.0E0 
          DO 3500 IBE=1,NEFFOI(IFFO)
              FFOAIN(IFFO)=FFOAIN(IFFO)+AFFO  (IBE     )
              BUFX        =BUFX        +AXFFOI(IBE,IFFO)
              BUFY        =BUFY        +AYFFOI(IBE,IFFO)
              BUFZ        =BUFZ        +AZFFOI(IBE,IFFO)
 3500     CONTINUE
C
          IF(IPART.GE.1) THEN
              CALL DDCOM2(FFOAIN(IFFO),BUFA)
              CALL DDCOM2(BUFX,BUFXA)
              CALL DDCOM2(BUFY,BUFYA)
              CALL DDCOM2(BUFZ,BUFZA)
              FFOAIN(IFFO)=BUFA
              BUFX=BUFXA
              BUFY=BUFYA
              BUFZ=BUFZA
          ENDIF
C
          BUF=BUFX*BUFX+BUFY*BUFY+BUFZ*BUFZ
          IF(BUF.LE.0.0E0) THEN
              IERR=1
              RETURN              
          ENDIF
          FFODIR(1,IFFO)=BUFX/SQRT(BUF)
          FFODIR(2,IFFO)=BUFY/SQRT(BUF)
          FFODIR(3,IFFO)=BUFZ/SQRT(BUF)
          FFODIM(IFFO)=FFOVOL(IFFO)/FFOAIN(IFFO)
C
          WRITE(IUT6,'(I3,3F12.4,3F10.2)') 
     *    IFFO,FFOVOL(IFFO),FFOAIN(IFFO),FFODIM(IFFO),
     *    FFODIR(1,IFFO),FFODIR(2,IFFO),FFODIR(3,IFFO)
 3000 CONTINUE
          WRITE(IUT6,*)  '****'
C
      RETURN
      END
