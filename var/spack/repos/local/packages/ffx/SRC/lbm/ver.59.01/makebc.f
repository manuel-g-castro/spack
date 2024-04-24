      SUBROUTINE MAKEBC(JGEOM,NP,NG,NC,M1,LBTYPE,LVEL,LREV,
     *                  MBC,LLEVEL,LPOSI,NBC,LBC,
     *                  MPBOUN,NPBOUN,LPBOUN,QBOUN,
     *                  LWORK1,LWORK2,LWORK,
     *                  NPG,DSCALE,XP,YP,ZP,QCELL,
     *                  NTRIG,XTRIG,YTRIG,ZTRIG,IMODQ,EPSTRG,
     *                  IUT6,IUT0,IERR)
      IMPLICIT NONE
      INTEGER*4 JGEOM,NP,NG,NC,M1,LBTYPE(6),MBC,LLEVEL(NC),LPOSI(3,NC)
      INTEGER*4 LVEL(3,NP),LREV(NP)
      INTEGER*4 NBC(NC),LBC(5,MBC,NC)
      INTEGER*4 MPBOUN,NPBOUN(NC),LPBOUN(5,MPBOUN,NC)
      REAL*4    QBOUN(MPBOUN,NC)
      INTEGER*4 LWORK1(0:NG+2,0:NG+2,0:NG+2),
     *          LWORK2(  NG+1,  NG+1,  NG+1),
     *          LWORK(NP,NG+1,NG+1,NG+1)
C
      INTEGER*4 NPG,NTRIG
      REAL*4    XP(NPG),YP(NPG),ZP(NPG),
     *          XTRIG(NTRIG),YTRIG(NTRIG),ZTRIG(NTRIG),
     *          QCELL(M1,0:NG+1,0:NG+1,0:NG+1)
      REAL*8    DSCALE
C
      INTEGER*4 IMODQ
      REAL*8    EPSTRG   
C
      INTEGER*4 IUT6,IUT0,IERR
C
C
C     ====FUNCTION====
C     MAKE BOUNDARY CONDITION DATA
C
C     ====VARIABLES LIST====
C[IN]
C     NP             :NUMBER OF PARTICLES
C     NG             :CUBE SIZE (=2^N)
C     NC             :NUMBER OF CUBES IN SUB-DOMAIN
C     LBTYPE(6)      :BOUNDARY TYPE AT -X,+X,-Y,+Y,-Z,+Z BOUNDARY FACE
C     LLEVEL(IC)     :LEVEL OF CUBES, WHICH INDICATE THE GRID RESOLUTION. 
C                    :LEVEL=1 CORRESPONTDS THE FINEST GRID SIZE. A GRID SIZE
C                    :WILL BE TWICE WITH ONE INCREMENT OF THE LEVEL.
C     LPOSI(3,IC)    :INDICATES THE POSITIONS OF CUBES, WHICH ARE NORMALIZED 
C                    :BY THE MINIMUM CUBE SIZE.
C     NBC(IC)        :NUMBER OF B.C. GROUPS IN CUBES
C     LLEVEL(IC)     :LEVEL OF CUBES, WHICH INDICATE THE GRID RESOLUTION. 
C                     LEVEL=1 CORRESPONTDS THE FINEST GRID SIZE. A GRID SIZE
C                     WILL BE TWICE WITH ONE INCREMENT OF THE LEVEL.
C     LBC(II,IBC,IC) :ATTRIBUTE DATA OF B.C. GROUPS
C                      II=1 B.C. GROUP ID (1-26) IN AN ADJACENT CUBE
C                      II=2 SUB-DOMAIN NUMBER OF AN ADJACENT CUBE
C                      II=3 CUBE NUMBER OF AN ADJACENT CUBE IN A DOMAIN
C                      II=4 RELATIVE LEVEL OF AN ADJACENT CUBE
C                       (-1: FINE, 0:SAME, 1:COARSE)
C                      II=5 POSITION IN COARSER CUBE
C     MPBOUN         :MAX. NUMBER OF BOUNDARY GRUID
C
C[OUT]
C     NPBOUN(IC)      :NUMBER OF BOUNDARY GRID    
C     LPBOUN(I,IPB,IC):BOUNDARY GRID LIST
C                      1: POSITION OF BOUNDRY GRID IN I-DIRECTION 
C                      2: POSITION OF BOUNDRY GRID IN J-DIRECTION 
C                      3: POSITION OF BOUNDRY GRID IN K-DIRECTION 
C                      4: DIRECTION TO INNER GRID (2-15)
C                      5: BOUNDARY TYPE (ITYPE)
C                         ITYPE=1: WALL
C                         ITYPE=2: INLET
C                         ITYPE=3: MOVING-WALL
C                         ITYPE=4: FREE
C                         ITYPE=5: SYMMETRIC 
C    QBOUN(IPB,IC)    :NON-DIMENSIONAL DISTANCE TO INNER GRIDS 
C
C
      INTEGER*4 I,IPB,IC
C
      WRITE(IUT6,*) 
      WRITE(IUT6,*)
     * ' [1] MAKEBC: MAKE B.C. DATA BOUNDING BOX '
      DO 1000 IC=1,NC
          CALL MKBC01(NP,LVEL,LREV,NG,NBC(IC),LBC(1,1,IC),LBTYPE,
     *                MPBOUN,NPBOUN(IC),LPBOUN(1,1,IC),QBOUN(1,IC),
     *                LWORK1,LWORK2,IERR)
          WRITE(IUT6,'(A12,2I8)') 'MKBC01: ', IC,NPBOUN(IC)
 1000  CONTINUE    
C
      IF(JGEOM.EQ.0) RETURN
C
      WRITE(IUT6,*) 
      WRITE(IUT6,*)
     * ' [2] MAKEBC: MAKE INTERSECT DATA '
      CALL MKBC02(NP,NG,NC,M1,MBC,LLEVEL,LPOSI,DSCALE,
     *            NTRIG,XTRIG,YTRIG,ZTRIG,
     *            MPBOUN,NPBOUN,LPBOUN,QBOUN,
     *            QCELL,LWORK,IMODQ,EPSTRG,IUT6,IUT0,IERR)
C
      RETURN
      END
