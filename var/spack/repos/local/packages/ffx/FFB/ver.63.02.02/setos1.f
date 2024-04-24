      SUBROUTINE SETOS1(IPART,NPART,N2,NE,NP,NEX,MBPDOM,MDOM,
     *                  MCOMM,NODE,LEFRM,X,Y,Z,
     *                  XMINE,YMINE,ZMINE,XMAXE,YMAXE,ZMAXE,
     *                  NDOMS,NDOMR,NBDOMS,NBDOMR,LISTS,LISTR,
     *                  LDOMS,LDOMR,LOSPR,
     *                  LOSFMR,XOVERR,YOVERR,ZOVERR,
     *                  LOSFDS,COEF1S,COEF2S,COEF3S,ERROSR,
     *                  LOSFDR,COEF1R,COEF2R,COEF3R,ERROSS,
     *                  MZ,MBLK,NDIVX,NDIVY,NDIVZ,NEZ,IELIST,ISTART,
     *                  XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,
     *                  JNTFND,EPSOS,IUT0,IUT6,IERR)
      IMPLICIT NONE
C
C[INPUT]
      INTEGER*4 IPART,NPART,N2,NE,NP,NEX(8),MBPDOM,MDOM
      INTEGER*4 MCOMM,NODE(N2,NE),LEFRM(NE)
      INTEGER*4 IEATTR(NE)
      REAL*4    X(NP),Y(NP),Z(NP),
     *          XMINE(NE),YMINE(NE),ZMINE(NE),
     *          XMAXE(NE),YMAXE(NE),ZMAXE(NE)
      INTEGER*4 NDOMS,NDOMR,
     *          NBDOMS(NDOMS),NBDOMR(NDOMR),
     *          LISTS (NDOMS),LISTR (NDOMR),
     *          LDOMS (MDOM ),LDOMR (MDOM ),
     *          LOSFMR(MCOMM),LOSPR(MCOMM)
      REAL*4    XOVERR(MCOMM),YOVERR(MCOMM),
     *          ZOVERR(MCOMM)
      INTEGER*4 JNTFND
      REAL*4    EPSOS
      INTEGER*4 IUT6,IUT0
C
C[INPUT:BLOCK]
      INTEGER*4 MZ,MBLK,NDIVX,NDIVY,NDIVZ,
     *          NEZ(MZ),IELIST(MBLK),ISTART(MZ)
      REAL*4    XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX
C
C[OUTPUT]
      INTEGER*4 LOSFDS(MCOMM),LOSFDR(MCOMM)
      REAL*4    COEF1S(MCOMM),COEF2S(MCOMM),
     *          COEF3S(MCOMM),COEF1R(MCOMM),
     *          COEF2R(MCOMM),COEF3R(MCOMM),
     *          ERROSR(MCOMM),ERROSS(MCOMM)
      INTEGER*4 IERR
C
C[LOCAL]
      INTEGER*4 LDUM1(MCOMM),LDUM2(MCOMM)
      INTEGER*4 IDOM,IBP,IE,IP,IFRM
      INTEGER*4 INX,INY,INZ,IZ,NBUF
      REAL*4    XP,YP,ZP,CBUF1,CBUF2,CBUF3,ERR
      REAL*4 TINY
C
C
C     VERSION 2011.07.15 WRITTEN BY Y.YAMADE   
C
C     ARGUMENT LIST
C
C[INPUT]
C IPART          : DOMAIN NUMBER (1 - NPART) 
C NPART          : NUMBER OF DOMAINS
C MBPDOM         : MAX. NUMBER OF O.S. NODES FOR A DOMAIN  
C NDOMS          : NUMBER OF DOMAIN  FOR SENDING
C NDOMR          : NUMBER OF DOMAIN  FOR RECEIVING
C NBDOMS(I,IDOM) : NUMBER OF NODES   FOR SENDING TO THE DOMAIN OF IDOM
C NBDOMR(I,IDOM) : NUMBER OF NODES   FOR RECEIVING FROM THE DOMAIN OF IDOM 
C LDOMS (  IDOM) : DOMAIN LIST       FOR SENDING 
C LDOMR (  IDOM) : DOMAIN LIST       FOR RECEIVING FROM THE DOMAIN OF IDOM
C XOVERR(I,IDOM) : X-COORDINATE      FOR RECEIVING FROM THE DOMAIN OF IDOM  
C YOVERR(I,IDOM) : Y-COORDINATE      FOR RECEIVING FROM THE DOMAIN OF IDOM  
C ZOVERR(I,IDOM) : Z-COORDINATE      FOR RECEIVING FROM THE DOMAIN OF IDOM  
C
C[OUTPUT]
C LOSFDR (I,IDOM) : RESULT OF SERCHING (0:NOT FOUND, ELM. NUM.:FOUND)
C COEF1R (I,IDOM) : LOCAL COORDINATE-1
C COEF2R (I,IDOM) : LOCAL COORDINATE-2
C COEF3R (I,IDOM) : LOCAL COORDINATE-3
C LOSFDS (I,IDOM) : RESULT OF SERCHING (IT WILL BE GOT BY COMMUNICATION)
C COEF1S (I,IDOM) : LOCAL COORDINATE-1
C COEF2S (I,IDOM) : LOCAL COORDINATE-2
C COEF3S (I,IDOM) : LOCAL COORDINATE-3
C
      IERR=0
C
C
      CALL DDSYNC
      CALL USTSTA(33)
C
      NBUF=0
C
      DO 2000 IDOM=1,NDOMR
C
CC        WRITE(IUT6,'(A15,3I8)')
CC   *    ' **SETOS1** : ',IDOM,NDOMR,NBDOMR(IDOM)
C
          DO 2100 IBP=1,NBDOMR(IDOM)
              IFRM=LOSFMR(IBP+LISTR(IDOM)-1)
C
              XP  =XOVERR(IBP+LISTR(IDOM)-1)
              YP  =YOVERR(IBP+LISTR(IDOM)-1)
              ZP  =ZOVERR(IBP+LISTR(IDOM)-1)
C
              IE=0
              CBUF1=0.0E0
              CBUF2=0.0E0
              CBUF3=0.0E0
              ERR  =0.0E0
C
CC              IF( XP.GT.XMAX+(XMAX-XMIN)*0.01 ) GOTO 2100 
CC              IF( YP.GT.YMAX+(YMAX-YMIN)*0.01 ) GOTO 2100 
CC              IF( ZP.GT.ZMAX+(ZMAX-ZMIN)*0.01 ) GOTO 2100 
CC              IF( XP.LT.XMIN-(XMAX-XMIN)*0.01 ) GOTO 2100 
CC              IF( YP.LT.YMIN-(YMAX-YMIN)*0.01 ) GOTO 2100 
CC              IF( ZP.LT.ZMIN-(ZMAX-ZMIN)*0.01 ) GOTO 2100 
C
              INX = NDIVX*(XP-XMIN)/(XMAX-XMIN)+1
              INY = NDIVY*(YP-YMIN)/(YMAX-YMIN)+1
              INZ = NDIVZ*(ZP-ZMIN)/(ZMAX-ZMIN)+1
C
              IF(INX.LT.1) INX = 1
              IF(INY.LT.1) INY = 1
              IF(INZ.LT.1) INZ = 1
C
              IF(INX.GT.NDIVX) INX = NDIVX
              IF(INY.GT.NDIVY) INY = NDIVY
              IF(INZ.GT.NDIVZ) INZ = NDIVZ
C
              IZ = (INZ-1)*(NDIVX*NDIVY)+(INY-1)*NDIVX+INX
              IF(NEZ(IZ).EQ.0) GOTO 2110
C
              TINY=1.0E-4
              CALL FNDOS(N2,NE,NEZ(IZ),IELIST(ISTART(IZ)),TINY,
     *                   NP,NEX,X,Y,Z,NODE,IFRM,LEFRM,
     *                   XP,YP,ZP,XMINE,YMINE,ZMINE,XMAXE,YMAXE,ZMAXE,
     *                   IE,CBUF1,CBUF2,CBUF3,ERR,IUT0,IERR)
C
              IF(IE.EQ.0) THEN
              TINY=1.0E-2
              CALL FNDOS(N2,NE,NEZ(IZ),IELIST(ISTART(IZ)),TINY,
     *                   NP,NEX,X,Y,Z,NODE,IFRM,LEFRM,
     *                   XP,YP,ZP,XMINE,YMINE,ZMINE,XMAXE,YMAXE,ZMAXE,
     *                   IE,CBUF1,CBUF2,CBUF3,ERR,IUT0,IERR)
              ENDIF
C
              IF(IE.EQ.0) THEN
              TINY=1.0E-1
              CALL FNDOS(N2,NE,NEZ(IZ),IELIST(ISTART(IZ)),TINY,
     *                   NP,NEX,X,Y,Z,NODE,IFRM,LEFRM,
     *                   XP,YP,ZP,XMINE,YMINE,ZMINE,XMAXE,YMAXE,ZMAXE,
     *                   IE,CBUF1,CBUF2,CBUF3,ERR,IUT0,IERR)
              ENDIF
C
              IF(IE.EQ.0) THEN
              TINY=4.0E-1
              CALL FNDOS(N2,NE,NEZ(IZ),IELIST(ISTART(IZ)),TINY,
     *                   NP,NEX,X,Y,Z,NODE,IFRM,LEFRM,
     *                   XP,YP,ZP,XMINE,YMINE,ZMINE,XMAXE,YMAXE,ZMAXE,
     *                   IE,CBUF1,CBUF2,CBUF3,ERR,IUT0,IERR)
              ENDIF
C
              IF(IE.EQ.0) THEN
              TINY=4.0E-0
              CALL FNDOS(N2,NE,NEZ(IZ),IELIST(ISTART(IZ)),TINY,
     *                   NP,NEX,X,Y,Z,NODE,IFRM,LEFRM,
     *                   XP,YP,ZP,XMINE,YMINE,ZMINE,XMAXE,YMAXE,ZMAXE,
     *                   IE,CBUF1,CBUF2,CBUF3,ERR,IUT0,IERR)
              ENDIF
C
              IF(IE.EQ.0.AND.JNTFND.EQ.1) THEN
              CALL FNDOS2(N2,NE,NEZ(IZ),IELIST(ISTART(IZ)),
     *                    NP,NEX,X,Y,Z,NODE,IFRM,LEFRM,
     *                    XP,YP,ZP,EPSOS,
     *                    IE,CBUF1,CBUF2,CBUF3,ERR,IUT0,IERR)
                  IF(IE.NE.0) NBUF=NBUF+1
              ENDIF
C
 2110     CONTINUE   
              LOSFDR(IBP+LISTR(IDOM)-1)=IE
              COEF1R(IBP+LISTR(IDOM)-1)=CBUF1
              COEF2R(IBP+LISTR(IDOM)-1)=CBUF2
              COEF3R(IBP+LISTR(IDOM)-1)=CBUF3
              ERROSR(IBP+LISTR(IDOM)-1)=ERR
C
              IF(IPART.EQ.0) THEN
                  LOSFDS(IBP+LISTR(IDOM)-1)=IE
                  COEF1S(IBP+LISTR(IDOM)-1)=CBUF1
                  COEF2S(IBP+LISTR(IDOM)-1)=CBUF2
                  COEF3S(IBP+LISTR(IDOM)-1)=CBUF3
                  ERROSS(IBP+LISTR(IDOM)-1)=ERR
              ENDIF
 2100     CONTINUE   
C
 2000 CONTINUE   
C
      WRITE(IUT6,*) 'SETOS1:NUM. OF O.S. DATA BY NEAREST MODE:',NBUF
C
      CALL USTEND(33)
C
      CALL DDSYNC
      CALL USTSTA(34)
      CALL USTSTA(35)
C
      CALL DDSET6(IPART,MCOMM,
     *            NDOMR,LDOMR,NBDOMR,LISTR,LOSFDR,
     *            NDOMS,LDOMS,NBDOMS,LISTS,LOSFDS,IERR)
C
      CALL DDSET6(IPART,MCOMM,
     *            NDOMR,LDOMR,NBDOMR,LISTR,COEF1R,
     *            NDOMS,LDOMS,NBDOMS,LISTS,COEF1S,IERR)
C
      CALL DDSET6(IPART,MCOMM,
     *            NDOMR,LDOMR,NBDOMR,LISTR,COEF2R,
     *            NDOMS,LDOMS,NBDOMS,LISTS,COEF2S,IERR)
C
      CALL DDSET6(IPART,MCOMM,
     *            NDOMR,LDOMR,NBDOMR,LISTR,COEF3R,
     *            NDOMS,LDOMS,NBDOMS,LISTS,COEF3S,IERR)
C
      CALL DDSET6(IPART,MCOMM,
     *            NDOMR,LDOMR,NBDOMR,LISTR,ERROSR,
     *            NDOMS,LDOMS,NBDOMS,LISTS,ERROSS,IERR)
      CALL DDSYNC
      CALL USTEND(34)
      CALL USTEND(35)
C
      RETURN
      END
