C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.8.1                                   C
C                                                                      C
C  SUB ROUTINE : ZONE3E                                                C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C CONTACT ADDRESS : IIS, THE UNIVERSITY OF TOKYO, CISS                 C
C                                                                      C
C THERMO-FLUID ANALYSIS SOLVERS FOR LARGE-SCALE-ASSEMBLY               C
C                                                                      C
C======================================================================C
      SUBROUTINE ZONE3E(X,Y,Z,NODE,NE,NP,N2,NDIVX,NDIVY,NDIVZ,
     *                  EPS,BLKMIN,MAX,MZ,
     *                  IELIST,ISTART,NEZ,NEZMAX,NEZAVR,NZEFF,NZ,
     *                  XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,IEBUF,IUT0,IERR)
C
      IMPLICIT NONE
C
      INTEGER*4 NODE,NE,NP,N2,NDIVX,NDIVY,NDIVZ,MAX,MZ,
     *          IELIST,ISTART,NEZ,NEZMAX,NEZAVR,NZEFF,NZ,
     *          IEBUF,IUT0,IERR
      REAL*4    X,Y,Z,EPS,BLKMIN,XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX
C
      INTEGER*4 I,IZ,IBUF,IE,IP,NBUF,
     *          INXMIN,INYMIN,INZMIN,INXMAX,INYMAX,INZMAX,INX,INY,INZ
      REAL*4    XEMIN,YEMIN,ZEMIN,XEMAX,YEMAX,ZEMAX,DELX,DELY,DELZ
C
      DIMENSION X(NP),Y(NP),Z(NP),NODE(N2,NE),IELIST(MAX),ISTART(MZ),
     1          NEZ(MZ),IEBUF(MAX)
C
      CHARACTER*60 ERMSGB
     & / ' ## SUBROUTINE ZONE3E: FATAL      ERROR OCCURENCE; RETURNED' /
C
      CHARACTER*60 EREXP1
     & / ' NUMBER OF ZONES TO BE GENERATED HAS EXCEEDED THE LIMIT OF ' /
      CHARACTER*60 EREXP2
     & / ' NUMBER OF ARRAY ELEMENTS HAS EXCEEDED THE LIMIT OF        ' /
C
C
C      GENERATE ZONAL ELEMENT LIST IN ONE-DIMENSIONAL ARRAY
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          X       (IP); X-DIR. COORDINATE OF NODE
C          Y       (IP); Y-DIR. COORDINATE OF NODE
C          Z       (IP); Z-DIR. COORDINATE OF NODE
C          NODE  (I,IE); NODE TABLE
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          NP          ; NUMBER OF TOTAL    NODES
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT
C
C          NDIVX       ; NUMBER OF ZONE DIVISIONS IN X-DIRECTION
C          NDIVY       ; NUMBER OF ZONE DIVISIONS IN Y-DIRECTION
C          NDIVZ       ; NUMBER OF ZONE DIVISIONS IN Z-DIRECTION
C          EPS         ; OVERLAPING MARGIN OF TWO ADJACENT ZONES
C
C          MAX         ; DIMENSION LIMIT FOR ARRAYS 'IELIST' AND 'IEBUF'
C          MZ          ; MAXIMUM NUMBER OF ZONES TO BE GENERATED
C
C       (2) OUTPUT
C          IELIST(IBUF); ELEMENT NUMBERS INCLUDED IN EACH ZONE
C          ISTART  (IZ); ENTRY POINT IN ZONE LIST ARRAY 'IELIST'
C          NEZ     (IZ); NUMBER OF ELEMENTS INCLUDED IN ZONE 'IZ'
C          NEZMAX      ; MAXIMUM NUMBER OF ELEMENTS IN A ZONE
C          NEZAVR      ; AVERAGE NUMBER OF ELEMENTS IN AN EFFECTIVE ZONE
C          NZEFF       ; NUMBER OF EFFECTIVE ZONES
C          NZ          ; NUMBER OF ZONES GENERATED (=NDIVX*NDIVY*NDIVZ)
C
C          XMIN        ; X-DIRECTION LOWER BOUND OF THE GIVEN MESH
C          YMIN        ; Y-DIRECTION LOWER BOUND OF THE GIVEN MESH
C          ZMIN        ; Z-DIRECTION LOWER BOUND OF THE GIVEN MESH
C
C          XMAX        ; X-DIRECTION UPPER BOUND OF THE GIVEN MESH
C          YMAX        ; Y-DIRECTION UPPER BOUND OF THE GIVEN MESH
C          ZMAX        ; Z-DIRECTION UPPER BOUND OF THE GIVEN MESH
C
C          IERR        ; RETURN CODE WHOSE VALUE WILL BE EITHER
C                   0 --- INDICATING SUCCESSFUL TERMINATION
C                OR 1 --- INDICATING OCCURRENCE OF SOME ERROR CONDITIONS
C
C       (4) WORK
C          IEBUF (IBUF); TEMPORARILY STORES ZONE NUMBERS FOR ELEMENTS
C
C
      IERR = 0
C
C
C
C CHECK NUMBER OF ZONES TO BE GENERATED
C
C
C
      NZ = NDIVX*NDIVY*NDIVZ
      IF(NZ.GT.MZ) THEN
          WRITE(IUT0,*) ERMSGB
          WRITE(IUT0,*) 'NDIVX',NDIVX
          WRITE(IUT0,*) 'NDIVX',NDIVY
          WRITE(IUT0,*) 'NDIVX',NDIVZ
          WRITE(IUT0,*) EREXP1, MZ
          IERR = 1
          RETURN
      ENDIF
C
      DO 50 I=1,MAX
          IELIST(I)=0 
          IEBUF (I)=0 
 50   CONTINUE   
C
      DELX = EPS*(XMAX-XMIN)/NDIVX
      DELY = EPS*(YMAX-YMIN)/NDIVY
      DELZ = EPS*(ZMAX-ZMIN)/NDIVZ
C
      IF(DELX.LT.BLKMIN) DELX=BLKMIN
      IF(DELY.LT.BLKMIN) DELY=BLKMIN
      IF(DELZ.LT.BLKMIN) DELZ=BLKMIN
C
C
C
C FIRST, SCATTER EACH ELEMENTS TO APPROPRIATE ZONE(S)
C
C
C
      DO 200 IZ = 1 , NZ
          NEZ(IZ) = 0
  200 CONTINUE
C
      IBUF = 0
      DO 250 IE = 1 , NE
          DO 210 I = 1 , N2
              IP = NODE(I,IE)
              IF (IP.EQ.0) GOTO 210
              IF(I.EQ.1 .OR. X(IP).LE.XEMIN) XEMIN = X(IP)
              IF(I.EQ.1 .OR. Y(IP).LE.YEMIN) YEMIN = Y(IP)
              IF(I.EQ.1 .OR. Z(IP).LE.ZEMIN) ZEMIN = Z(IP)
C
              IF(I.EQ.1 .OR. X(IP).GE.XEMAX) XEMAX = X(IP)
              IF(I.EQ.1 .OR. Y(IP).GE.YEMAX) YEMAX = Y(IP)
              IF(I.EQ.1 .OR. Z(IP).GE.ZEMAX) ZEMAX = Z(IP)
  210     CONTINUE
C
          INXMIN = NDIVX*(XEMIN-XMIN-DELX)/(XMAX-XMIN)+1
          INYMIN = NDIVY*(YEMIN-YMIN-DELY)/(YMAX-YMIN)+1
          INZMIN = NDIVZ*(ZEMIN-ZMIN-DELZ)/(ZMAX-ZMIN)+1
C
          INXMAX = NDIVX*(XEMAX-XMIN+DELX)/(XMAX-XMIN)+1
          INYMAX = NDIVY*(YEMAX-YMIN+DELY)/(YMAX-YMIN)+1
          INZMAX = NDIVZ*(ZEMAX-ZMIN+DELZ)/(ZMAX-ZMIN)+1
C
          IF(INXMIN.LT.1) INXMIN = 1
          IF(INYMIN.LT.1) INYMIN = 1
          IF(INZMIN.LT.1) INZMIN = 1
C
          IF(INXMAX.GT.NDIVX) INXMAX = NDIVX
          IF(INYMAX.GT.NDIVY) INYMAX = NDIVY
          IF(INZMAX.GT.NDIVZ) INZMAX = NDIVZ
C
          DO 240 INZ = INZMIN , INZMAX
              DO 230 INY = INYMIN , INYMAX
                  DO 220 INX = INXMIN , INXMAX
                      IZ = (INZ-1)*(NDIVX*NDIVY)+(INY-1)*NDIVX+INX
                      NEZ(IZ) = NEZ(IZ)+1
C
                      IBUF = IBUF+1
C
                      IF(IBUF.GT.MAX) THEN
                          WRITE(IUT0,*) ERMSGB
                          WRITE(IUT0,*) EREXP2, MAX
                          WRITE(IUT0,*) '    IE=', IE, '    NE=', NE
                          IERR = 1
                          RETURN
                      ENDIF
C
                      IEBUF(IBUF) = IZ
  220             CONTINUE
  230         CONTINUE
  240     CONTINUE
C
          IBUF = IBUF+1
          IF(IBUF.GT.MAX) THEN
              WRITE(IUT0,*) ERMSGB
              WRITE(IUT0,*) EREXP2, MAX
              WRITE(IUT0,*) '    IE=', IE, '    NE=', NE
              IERR = 1
              RETURN
          ENDIF
          IEBUF(IBUF) = 0
  250 CONTINUE
      NBUF = IBUF
C
      NEZAVR = 0
      NEZMAX = 0
      NZEFF  = 0
C
      DO 260 IZ = 1 , NZ
          IF(NEZ(IZ).EQ.0) GO TO 260
          NZEFF = NZEFF+1
          NEZAVR = NEZAVR+NEZ(IZ)
          IF(NEZ(IZ).GE.NEZMAX) NEZMAX = NEZ(IZ)
  260 CONTINUE
C
      IF(NZEFF.NE.0) THEN
          NEZAVR = NEZAVR/NZEFF
      ELSE
          NEZAVR = 0
      ENDIF 
C
C
C
C THEN, COMPUTE POINTERS TO EACH ZONE ENTRY ADDRESS (IN WORDS)
C
C
C
      ISTART(1) = 1
      DO 310 IZ = 2 , NZ
          ISTART(IZ) = ISTART(IZ-1)+NEZ(IZ-1)
  310 CONTINUE
C
C
C
C FINALLY, SCAN SCATTERED LIST 'IEBUF(IBUF)' TO GENERATE ZONE LIST 
C
C
C
      DO 400 IZ = 1 , NZ
          NEZ(IZ) = 0
  400 CONTINUE
C
      IE = 1
      DO 410 IBUF = 1 , NBUF
          IF(IEBUF(IBUF).EQ.0) THEN
              IE = IE+1
              GO TO 410
          ENDIF
C
          IZ = IEBUF(IBUF)
          NEZ(IZ) = NEZ(IZ)+1
          IELIST(ISTART(IZ)+NEZ(IZ)-1) = IE
  410 CONTINUE
C
C
      RETURN
      END
