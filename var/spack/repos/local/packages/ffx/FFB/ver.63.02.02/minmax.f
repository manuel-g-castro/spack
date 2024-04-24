      SUBROUTINE MINMAX(N2,NE,NP,NEX,X,Y,Z,NODE,
     *                  XMINE,YMINE,ZMINE,XMAXE,YMAXE,ZMAXE)
      IMPLICIT NONE
      
***** DEFINE ARGUMENTS *****
      INTEGER N2,NE,NP,NEX(8)
      REAL X, Y, Z
      INTEGER NODE
      REAL XMINE, YMINE, ZMINE
      REAL XMAXE, YMAXE, ZMAXE
      DIMENSION X(NP),Y(NP),Z(NP)
      DIMENSION NODE(N2,NE)
      DIMENSION XMINE(NE)
      DIMENSION YMINE(NE)
      DIMENSION ZMINE(NE)
      DIMENSION XMAXE(NE)
      DIMENSION YMAXE(NE)
      DIMENSION ZMAXE(NE)
      
***** OBJECTS *****
      REAL XMIN, XMAX, YMIN, YMAX, ZMIN, ZMAX
      INTEGER IN, IE, IEBEG, IEEND
      INTEGER NEHEX,NEPRD,NEWED,NETET     
      INTEGER  NHEX, NPRD, NWED, NTET     
     
***** DATA *****
      NETET=NEX(1)
      NEPRD=NEX(2)
      NEWED=NEX(3)
      NEHEX=NEX(4)
       NTET=NEX(5)
       NPRD=NEX(6)
       NWED=NEX(7)
       NHEX=NEX(8)

********************
***** TET LOOP *****
********************
      IEBEG=1
      IEEND=NETET
      DO IE=IEBEG,IEEND
         XMIN=+1E30
         XMAX=-1E30
         YMIN=+1E30
         YMAX=-1E30
         ZMIN=+1E30
         ZMAX=-1E30
         
         DO IN=1,NTET
            XMIN =MIN(XMIN,X(NODE(IN,IE)))
            XMAX =MAX(XMAX,X(NODE(IN,IE)))
            YMIN =MIN(YMIN,Y(NODE(IN,IE)))
            YMAX =MAX(YMAX,Y(NODE(IN,IE)))
            ZMIN =MIN(ZMIN,Z(NODE(IN,IE)))
            ZMAX =MAX(ZMAX,Z(NODE(IN,IE)))
         ENDDO
         
         XMINE(IE)=XMIN
         XMAXE(IE)=XMAX
         YMINE(IE)=YMIN
         YMAXE(IE)=YMAX
         ZMINE(IE)=ZMIN
         ZMAXE(IE)=ZMAX
      ENDDO
      
********************
***** PYR LOOP *****
********************
      IEBEG=IEBEG+NETET
      IEEND=IEEND+NEPRD
      DO IE=IEBEG,IEEND
         XMIN=+1E30
         XMAX=-1E30
         YMIN=+1E30
         YMAX=-1E30
         ZMIN=+1E30
         ZMAX=-1E30
         
         DO IN=1,NPRD
            XMIN =MIN(XMIN,X(NODE(IN,IE)))
            XMAX =MAX(XMAX,X(NODE(IN,IE)))
            YMIN =MIN(YMIN,Y(NODE(IN,IE)))
            YMAX =MAX(YMAX,Y(NODE(IN,IE)))
            ZMIN =MIN(ZMIN,Z(NODE(IN,IE)))
            ZMAX =MAX(ZMAX,Z(NODE(IN,IE)))
         ENDDO
         
         XMINE(IE)=XMIN
         XMAXE(IE)=XMAX
         YMINE(IE)=YMIN
         YMAXE(IE)=YMAX
         ZMINE(IE)=ZMIN
         ZMAXE(IE)=ZMAX
      ENDDO

********************
***** WED LOOP *****
********************
      IEBEG=IEBEG+NEPRD
      IEEND=IEEND+NEWED
      DO IE=IEBEG,IEEND
         XMIN=+1E30
         XMAX=-1E30
         YMIN=+1E30
         YMAX=-1E30
         ZMIN=+1E30
         ZMAX=-1E30
         
         DO IN=1,NWED
            XMIN =MIN(XMIN,X(NODE(IN,IE)))
            XMAX =MAX(XMAX,X(NODE(IN,IE)))
            YMIN =MIN(YMIN,Y(NODE(IN,IE)))
            YMAX =MAX(YMAX,Y(NODE(IN,IE)))
            ZMIN =MIN(ZMIN,Z(NODE(IN,IE)))
            ZMAX =MAX(ZMAX,Z(NODE(IN,IE)))
         ENDDO
         
         XMINE(IE)=XMIN
         XMAXE(IE)=XMAX
         YMINE(IE)=YMIN
         YMAXE(IE)=YMAX
         ZMINE(IE)=ZMIN
         ZMAXE(IE)=ZMAX
      ENDDO

********************
***** HEX LOOP *****
********************
      IEBEG=IEBEG+NEWED
      IEEND=IEEND+NEHEX
      DO IE=IEBEG,IEEND
         XMIN=+1E30
         XMAX=-1E30
         YMIN=+1E30
         YMAX=-1E30
         ZMIN=+1E30
         ZMAX=-1E30
         
         DO IN=1,NHEX
            XMIN =MIN(XMIN,X(NODE(IN,IE)))
            XMAX =MAX(XMAX,X(NODE(IN,IE)))
            YMIN =MIN(YMIN,Y(NODE(IN,IE)))
            YMAX =MAX(YMAX,Y(NODE(IN,IE)))
            ZMIN =MIN(ZMIN,Z(NODE(IN,IE)))
            ZMAX =MAX(ZMAX,Z(NODE(IN,IE)))
         ENDDO
         
         XMINE(IE)=XMIN
         XMAXE(IE)=XMAX
         YMINE(IE)=YMIN
         YMAXE(IE)=YMAX
         ZMINE(IE)=ZMIN
         ZMAXE(IE)=ZMAX
      ENDDO

      RETURN
      END
