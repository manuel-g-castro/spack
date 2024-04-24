      SUBROUTINE CHKOS2(NP,NPSET,IFATTR,LPSET1,LPSET4,
     *                  X,Y,Z,IPATTR,OMEGA,TIME,OSBOX,
     *                  IUT0,IUT6,IERR)
      IMPLICIT NONE 
C      
C[INPUT]
      INTEGER*4 NP,NPSET,LPSET1(NPSET),LPSET4(NPSET),
     *          IPATTR(NP),IFATTR(*),
     *          IUT0,IUT6
      REAL*4    X(NP),Y(NP),Z(NP),OMEGA,TIME,DX,DY,DZ
C      
C[OUTPUT]
      INTEGER*4 IERR
      REAL*4    OSBOX(6)
     *          
C      
C[WORK]
      INTEGER*4  IPB,IP
      REAL*4     XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,COSA,SINA,
     *           XBUF,YBUF,ZBUF
C
      IF(NPSET.EQ.0) THEN
          XMIN= 1.0E10
          YMIN= 1.0E10
          ZMIN= 1.0E10
          XMAX= 1.0E10
          YMAX= 1.0E10
          ZMAX= 1.0E10
          OSBOX(1)=XMIN
          OSBOX(2)=YMIN
          OSBOX(3)=ZMIN
          OSBOX(4)=XMAX
          OSBOX(5)=YMAX
          OSBOX(6)=ZMAX
          RETURN
      ENDIF
C
      COSA=COS(OMEGA*TIME)
      SINA=SIN(OMEGA*TIME)
C
      XMIN=-1.0E10
      YMIN=-1.0E10
      ZMIN=-1.0E10
      XMAX= 1.0E10
      YMAX= 1.0E10
      ZMAX= 1.0E10
C
      DO 1000 IPB=1,NPSET
          IP=LPSET1(IPB)
C
          XBUF=X(IP)
          YBUF=Y(IP)
          ZBUF=Z(IP) 
C
          IF(IPATTR(IP)         .GE. 0 .AND.
     *       IFATTR(LPSET4(IPB)).EQ.-1) THEN
              XBUF= COSA*X(IP)+SINA*Y(IP)
              YBUF=-SINA*X(IP)+COSA*Y(IP)
              ZBUF=      Z(IP) 
          ENDIF
C
          IF(IPATTR(IP)         .EQ.-1 .AND.
     *       IFATTR(LPSET4(IPB)).GE. 0) THEN
              XBUF= COSA*X(IP)-SINA*Y(IP)
              YBUF= SINA*X(IP)+COSA*Y(IP)
              ZBUF=      Z(IP) 
          ENDIF
C
          IF(XBUF.LE.XMIN) XMIN=XBUF
          IF(YBUF.LE.YMIN) YMIN=YBUF
          IF(ZBUF.LE.ZMIN) ZMIN=ZBUF
          IF(XBUF.GE.XMAX) XMAX=XBUF
          IF(YBUF.GE.YMAX) YMAX=YBUF
          IF(ZBUF.GE.ZMAX) ZMAX=ZBUF
C
 1000 CONTINUE
C
      DX=XMAX-XMIN
      DY=YMAX-YMIN
      DZ=ZMAX-ZMIN
      OSBOX(1)=XMIN-DX*0.1E0
      OSBOX(2)=YMIN-DY*0.1E0
      OSBOX(3)=ZMIN-DZ*0.1E0
      OSBOX(4)=XMAX+DX*0.1E0
      OSBOX(5)=YMAX+DY*0.1E0
      OSBOX(6)=ZMAX+DZ*0.1E0
C
      RETURN
      END
