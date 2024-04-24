      SUBROUTINE FLD3X2(IMODE,ME,NE,NP,NEX,N1,N2,
     *                  FXYZ,FE,NODE,DNXI,DNYI,DNZI, 
     *                  IUT0,IERR,DNXYZ)
C
      IMPLICIT NONE
C
      INTEGER*4 IMODE,ME,NE,NP,NEX,N1,N2,NODE,IUT0,IERR
C
      REAL*4    FXYZ,FE,DNXI,DNYI,DNZI,DNXYZ
C
      INTEGER*4 NETET,NEPRD,NEWED,NEHEX,
     *          IES1,IEE1,IES2,IEE2,IES3,IEE3,IES4,IEE4,IE,J
C
      DIMENSION NEX(8)
      DIMENSION FXYZ(3,NP),FE(NE)
      DIMENSION NODE(N2,NE),
     1          DNXI(N1,ME), DNYI(N1,ME), DNZI(N1,ME),
     1          DNXYZ(8,3,NE) 
C
C      CALCULATE VELOCITY FIELD CHARACTERISTICS
C         ( 3-D CALCULATION : SINGLE WORD & MULTI ELEMENT VERSION )
C                                           CODED BASED ON 'FIELD3'
C                                              2009.01.13 Y.YAMADE
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          IMODE       ; SPECIFIES THE TYPE OF FIELD DATA AS FOLLOWS
C                   1 --- ELEMENT DIVERGENT
C                   NOTE THAT CURRENTLLY ONLY IMODE=1 IS SUPPORTED. 
C
C          ME          ; MAX. NUMBER OF TOTAL ELEMENTS
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          NP          ; NUMBER OF TOTAL    NODES
C          NEX(I)      ; INCLUDES NUMBER OF ELEMENTS AND NUMBER OF LOCAL NODES
C                        AS FOLOOWS
C          NEX(1)      ; NUMBER OF TET.    ELEMENTS
C          NEX(2)      ; NUMBER OF PYRAMID ELEMENTS
C          NEX(3)      ; NUMBER OF WEGDE   ELEMENTS
C          NEX(4)      ; NUMBER OF HEX.    ELEMENTS
C          NEX(5)      ; NUMBER OF LOCAL NODES IN A TET.    ELEMENT (=4)
C          NEX(6)      ; NUMBER OF LOCAL NODES IN A PYRAMID ELEMENT (=5)
C          NEX(7)      ; NUMBER OF LOCAL NODES IN A WEGDE   ELEMENT (=6)
C          NEX(8)      ; NUMBER OF LOCAL NODES IN A HEX.    ELEMENT (=8)
C
C          U       (IP); X-DIR. VELOCITY COMPONENT
C          V       (IP); Y-DIR. VELOCITY COMPONENT
C          W       (IP); Z-DIR. VELOCITY COMPONENT
C
C          NODE(I,IE)  ; NODE NO. TABLE BASED ON ELEMENT
C          DNXI(I,IE)  ; ELEMENT CENTER VALUE OF NX
C          DNYI(I,IE)  ; ELEMENT CENTER VALUE OF NY
C          DNZI(I,IE)  ; ELEMENT CENTER VALUE OF NZ
C
C      (2) OUTPUT
C          FE    (IE)  ; FIELD DATA
C
C
      IF(IMODE.NE.1) THEN
          WRITE(IUT0,*) 'INVALID VALUE FOR IMODE: ERROR'
          IERR=1
          RETURN
      ENDIF
C
      NETET=NEX(1)
      NEPRD=NEX(2)
      NEWED=NEX(3)
      NEHEX=NEX(4)
      NE   =NETET+NEPRD+NEWED+NEHEX
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
C
C     (1) CALCULATE DIVERGENT
C
CC==== TET. ====     
C
C     OPERATION COUNTS:  23 FLOP /ELEMENT
C     DATA LOADINGS   :  24 WORDS/ELEMENT
C                      (  0 WORDS CONTIGUOUSLY,
C                        12 WORDS BY STRIDE, AND
C                        12 WORDS BY LIST )
C
      DO 110 IE = IES1 , IEE1
         FE(IE) = DNXI(1,IE)*FXYZ(1,(NODE(1,IE)))
     &           +DNXI(2,IE)*FXYZ(1,(NODE(2,IE)))
     &           +DNXI(3,IE)*FXYZ(1,(NODE(3,IE)))
     &           +DNXI(4,IE)*FXYZ(1,(NODE(4,IE)))
C
     &           +DNYI(1,IE)*FXYZ(2,(NODE(1,IE)))
     &           +DNYI(2,IE)*FXYZ(2,(NODE(2,IE)))
     &           +DNYI(3,IE)*FXYZ(2,(NODE(3,IE)))
     &           +DNYI(4,IE)*FXYZ(2,(NODE(4,IE)))
C
     &           +DNZI(1,IE)*FXYZ(3,(NODE(1,IE)))
     &           +DNZI(2,IE)*FXYZ(3,(NODE(2,IE)))
     &           +DNZI(3,IE)*FXYZ(3,(NODE(3,IE)))
     &           +DNZI(4,IE)*FXYZ(3,(NODE(4,IE)))
  110 CONTINUE
C
CC==== PYRAMID ====     
C
C     OPERATION COUNTS:  29 FLOP /ELEMENT
C     DATA LOADINGS   :  30 WORDS/ELEMENT
C                      (  0 WORDS CONTIGUOUSLY,
C                        15 WORDS BY STRIDE, AND
C                        15 WORDS BY LIST )
C
      DO 120 IE = IES2 , IEE2
         FE(IE) = DNXI(1,IE)*FXYZ(1,(NODE(1,IE)))
     &           +DNXI(2,IE)*FXYZ(1,(NODE(2,IE)))
     &           +DNXI(3,IE)*FXYZ(1,(NODE(3,IE)))
     &           +DNXI(4,IE)*FXYZ(1,(NODE(4,IE)))
     &           +DNXI(5,IE)*FXYZ(1,(NODE(5,IE)))
C
     &           +DNYI(1,IE)*FXYZ(2,(NODE(1,IE)))
     &           +DNYI(2,IE)*FXYZ(2,(NODE(2,IE)))
     &           +DNYI(3,IE)*FXYZ(2,(NODE(3,IE)))
     &           +DNYI(4,IE)*FXYZ(2,(NODE(4,IE)))
     &           +DNYI(5,IE)*FXYZ(2,(NODE(5,IE)))
C
     &           +DNZI(1,IE)*FXYZ(3,(NODE(1,IE)))
     &           +DNZI(2,IE)*FXYZ(3,(NODE(2,IE)))
     &           +DNZI(3,IE)*FXYZ(3,(NODE(3,IE)))
     &           +DNZI(4,IE)*FXYZ(3,(NODE(4,IE)))
     &           +DNZI(5,IE)*FXYZ(3,(NODE(5,IE)))
  120 CONTINUE
C
CC==== WEDGE ====     
C
C     OPERATION COUNTS:  35 FLOP /ELEMENT
C     DATA LOADINGS   :  36 WORDS/ELEMENT
C                      (  0 WORDS CONTIGUOUSLY,
C                        18 WORDS BY STRIDE, AND
C                        18 WORDS BY LIST )
C
      DO 130 IE = IES3 , IEE3
         FE(IE) = DNXI(1,IE)*FXYZ(1,(NODE(1,IE)))
     &           +DNXI(2,IE)*FXYZ(1,(NODE(2,IE)))
     &           +DNXI(3,IE)*FXYZ(1,(NODE(3,IE)))
     &           +DNXI(4,IE)*FXYZ(1,(NODE(4,IE)))
     &           +DNXI(5,IE)*FXYZ(1,(NODE(5,IE)))
     &           +DNXI(6,IE)*FXYZ(1,(NODE(6,IE)))
C
     &           +DNYI(1,IE)*FXYZ(2,(NODE(1,IE)))
     &           +DNYI(2,IE)*FXYZ(2,(NODE(2,IE)))
     &           +DNYI(3,IE)*FXYZ(2,(NODE(3,IE)))
     &           +DNYI(4,IE)*FXYZ(2,(NODE(4,IE)))
     &           +DNYI(5,IE)*FXYZ(2,(NODE(5,IE)))
     &           +DNYI(6,IE)*FXYZ(2,(NODE(6,IE)))
C
     &           +DNZI(1,IE)*FXYZ(3,(NODE(1,IE)))
     &           +DNZI(2,IE)*FXYZ(3,(NODE(2,IE)))
     &           +DNZI(3,IE)*FXYZ(3,(NODE(3,IE)))
     &           +DNZI(4,IE)*FXYZ(3,(NODE(4,IE)))
     &           +DNZI(5,IE)*FXYZ(3,(NODE(5,IE)))
     &           +DNZI(6,IE)*FXYZ(3,(NODE(6,IE)))
  130 CONTINUE
C
CC==== HEX. ====     
C
C     OPERATION COUNTS:  47 FLOP /ELEMENT
C     DATA LOADINGS   :  48 WORDS/ELEMENT
C                      (  0 WORDS CONTIGUOUSLY,
C                        24 WORDS BY STRIDE, AND
C                        24 WORDS BY LIST )
C
      CALL USTSTA(27)
!oclcache_sector_size(3,13)
!oclcache_subsector_assign(FXYZ)
!ocl parallel
      DO 140 IE = IES4, IEE4
!fj>
!ocl prefetch_read(FXYZ(1,(NODE(1,IE+32))),level=1)      
!ocl prefetch_read(FXYZ(1,(NODE(2,IE+32))),level=1)      
!ocl prefetch_read(FXYZ(1,(NODE(3,IE+32))),level=1)      
!ocl prefetch_read(FXYZ(1,(NODE(4,IE+32))),level=1)      
!ocl prefetch_read(FXYZ(1,(NODE(5,IE+32))),level=1)      
!ocl prefetch_read(FXYZ(1,(NODE(6,IE+32))),level=1)      
!ocl prefetch_read(FXYZ(1,(NODE(7,IE+32))),level=1)      
!ocl prefetch_read(FXYZ(1,(NODE(8,IE+32))),level=1)      
!fj<
         DO 150 J=1,8
            FE(IE) = FE(IE) +
     &               DNXYZ(J,1,IE)*FXYZ(1,(NODE(J,IE))) +
     &               DNXYZ(J,2,IE)*FXYZ(2,(NODE(J,IE))) +
     &               DNXYZ(J,3,IE)*FXYZ(3,(NODE(J,IE)))
 150     CONTINUE
 140  CONTINUE
!oclend_cache_subsector
!oclend_cache_sector_size
      CALL USTEND(27)
      RETURN
      END
