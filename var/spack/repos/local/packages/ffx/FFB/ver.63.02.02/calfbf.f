      SUBROUTINE CALFBF(IPART,N2,NE,NP,NSP,NS,NFFO,MEFFOI,MDGCOE,
     *                  LOCAL,NODE,
     *                  NEFFO,LEFFO1,LEFFO2,
     *                  NEFFOI,LEFFOI,NDGCOE,
     *                  U,V,W,
     *                  FFOAIN,FFOFR,FFODP,FFODIM,FFODIR,
     *                  AXFFOI,AYFFOI,AZFFOI,
     *                  FXFFO,FYFFO,FZFFO,COEFFO)
      IMPLICIT NONE
      INTEGER*4 IPART,N2,NE,NP,NSP,NS,NFFO,MEFFOI,MDGCOE
      INTEGER*4 LOCAL(NSP,NS,4),NODE(N2,NE),
     *          NEFFO,LEFFO1(NEFFO),LEFFO2(NEFFO),
     *          NEFFOI(NFFO),LEFFOI(2,MEFFOI,NFFO),NDGCOE(NFFO)
      REAL*4    U(NP),V(NP),W(NP)
      REAL*4    FFOAIN(NFFO),FFOFR(NFFO),FFODP(NFFO),
     *          FFODIM(NFFO),FFODIR(3,NFFO),
     *          AXFFOI(MEFFOI,NFFO),AYFFOI(MEFFOI,NFFO),
     *          AZFFOI(MEFFOI,NFFO),
     *          FXFFO(NE),FYFFO(NE),FZFFO(NE),COEFFO(0:MDGCOE,NFFO)
      INTEGER*4 IFFO,IE,IS,IBE,IETYPE,I,NNPS
      REAL*4    UBUF,VBUF,WBUF,QBUF,FBUF,FXBUF,FYBUF,FZBUF,BUFA
CC
CC [1] INITIALIZE FLUID FORCE
CC
      DO 1000 IE=1,NE
          FXFFO(IE)=0.0E0
          FYFFO(IE)=0.0E0
          FZFFO(IE)=0.0E0
 1000 CONTINUE
C
      IF(NFFO.EQ.0) RETURN
CC
CC [2] CAL. FLOW RATE
CC
      DO 2000 IFFO=1,NFFO
          FFOFR(IFFO)=0.0E0
C 
          DO 2100 IBE=1,NEFFOI(IFFO)
              IE=LEFFOI(1,IBE,IFFO)
              IS=LEFFOI(2,IBE,IFFO)
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
C
              UBUF=0.0E0
              VBUF=0.0E0
              WBUF=0.0E0
              DO 2200 I=1,NNPS
                  UBUF=UBUF+U(NODE(LOCAL(I,IS,IETYPE),IE))
                  VBUF=VBUF+V(NODE(LOCAL(I,IS,IETYPE),IE))
                  WBUF=WBUF+W(NODE(LOCAL(I,IS,IETYPE),IE))
 2200         CONTINUE
              UBUF=UBUF/FLOAT(NNPS)    
              VBUF=VBUF/FLOAT(NNPS)    
              WBUF=WBUF/FLOAT(NNPS)    
              QBUF= UBUF*AXFFOI(IBE,IFFO)
     *             +VBUF*AYFFOI(IBE,IFFO)
     *             +WBUF*AZFFOI(IBE,IFFO)
              FFOFR(IFFO)=FFOFR(IFFO)+QBUF
 2100     CONTINUE   
          IF(IPART.GE.1)THEN
              CALL DDCOM2(FFOFR(IFFO),BUFA)
              FFOFR(IFFO) = BUFA
          ENDIF
 2000 CONTINUE   
CC
CC [3] CAL. PRESSURE DIFFERENCE
CC
      DO 3000 IFFO=1,NFFO
          UBUF=FFOFR(IFFO)/FFOAIN(IFFO)
          FFODP(IFFO)=0.0E0
          DO 3100 I=0,NDGCOE(IFFO)
              FFODP(IFFO)=FFODP(IFFO)
     *                    +COEFFO(I,IFFO)*(UBUF**FLOAT(I))
 3100     CONTINUE       
 3000 CONTINUE   
CC
CC [4] SET FLUID FORCE
CC
      DO 4000 IBE=1,NEFFO
              IE  =LEFFO1(IBE)
              IFFO=LEFFO2(IBE)
              FXFFO(IE)=FFODIR(1,IFFO)*FFODP(IFFO)/FFODIM(IFFO) 
              FYFFO(IE)=FFODIR(2,IFFO)*FFODP(IFFO)/FFODIM(IFFO) 
              FZFFO(IE)=FFODIR(3,IFFO)*FFODP(IFFO)/FFODIM(IFFO) 
 4000 CONTINUE
C
      RETURN
      END
