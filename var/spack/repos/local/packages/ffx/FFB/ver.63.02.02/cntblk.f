C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.8.1                                   C
C                                                                      C
C  SUB ROUTINE : CNTBLK                                                C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C CONTACT ADDRESS : IIS, THE UNIVERSITY OF TOKYO, CISS                 C
C                                                                      C
C THERMO-FLUID ANALYSIS SOLVERS FOR LARGE-SCALE-ASSEMBLY               C
C                                                                      C
C======================================================================C
      SUBROUTINE CNTBLK(X,Y,Z,NODE,NE,NP,N2,NDIVX,NDIVY,NDIVZ,
     *                  EPS,BLKMIN,
     *                  XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,
     *                  OSXMIN,OSYMIN,OSZMIN,
     *                  OSXMAX,OSYMAX,OSZMAX,MBLK)
      IMPLICIT NONE
C
      INTEGER*4 NODE,NE,NP,N2,NDIVX,NDIVY,NDIVZ,MBLK
C
      REAL*4    X,Y,Z,EPS,BLKMIN,
     *          XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,
     *          OSXMIN,OSYMIN,OSZMIN,
     *          OSXMAX,OSYMAX,OSZMAX
C
      INTEGER*4 IP,IBUF,IE,I,
     *          INXMIN,INYMIN,INZMIN,INXMAX,INYMAX,INZMAX,
     *          INX,INY,INZ,IZ
C
      REAL*4    DELX,DELY,DELZ,XEMIN,YEMIN,ZEMIN,XEMAX,YEMAX,ZEMAX
C
      DIMENSION X(NP),Y(NP),Z(NP),NODE(N2,NE)
C
C
C      COUNT BLOCK DATA SIZE
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
C          MBLK     ; BLOCL DATA SIZE 
C
C
C
C
C FIND COMPUTAIONAL MESH BOUNDS AND SET OVERLAPPING MARGINS
C
C
C
      XMIN = X(1)
      YMIN = Y(1)
      ZMIN = Z(1)
C
      XMAX = X(1)
      YMAX = Y(1)
      ZMAX = Z(1)
C
      DO 100 IP = 2 , NP
          IF(X(IP).LE.XMIN) XMIN = X(IP)
          IF(Y(IP).LE.YMIN) YMIN = Y(IP)
          IF(Z(IP).LE.ZMIN) ZMIN = Z(IP)
C
          IF(X(IP).GE.XMAX) XMAX = X(IP)
          IF(Y(IP).GE.YMAX) YMAX = Y(IP)
          IF(Z(IP).GE.ZMAX) ZMAX = Z(IP)
  100 CONTINUE
C
      IF(XMIN.LT.OSXMIN) XMIN= OSXMIN
      IF(YMIN.LT.OSYMIN) YMIN= OSYMIN
      IF(ZMIN.LT.OSZMIN) ZMIN= OSZMIN
      IF(XMAX.GT.OSXMAX) XMAX= OSXMAX
      IF(YMAX.GT.OSYMAX) YMAX= OSYMAX
      IF(ZMAX.GT.OSZMAX) ZMAX= OSZMAX
C
      IF(XMIN.GT.XMAX) XMIN=XMAX
      IF(YMIN.GT.YMAX) YMIN=YMAX
      IF(ZMIN.GT.ZMAX) ZMIN=ZMAX
C
CC      XMIN= OSXMIN
CC      YMIN= OSYMIN
CC      ZMIN= OSZMIN
CC      XMAX= OSXMAX
CC      YMAX= OSYMAX
CC      ZMAX= OSZMAX
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
                      IBUF = IBUF+1
  220             CONTINUE
  230         CONTINUE
  240     CONTINUE
C
          IBUF = IBUF+1
  250 CONTINUE
      MBLK=IBUF
      MBLK=MBLK+100000
C
      RETURN
      END
