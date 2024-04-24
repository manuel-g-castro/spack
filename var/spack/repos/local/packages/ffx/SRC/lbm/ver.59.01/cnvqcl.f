      SUBROUTINE CNVQCL(NG,MPBOUN,M1,QCELL,
     *                  NPBOUN,LPBOUN,QBOUN,IMODQ,IUT6,IERR)
      IMPLICIT NONE
      INTEGER*4 NG,MPBOUN,M1
      REAL*4    QCELL(M1,0:NG+1,0:NG+1,0:NG+1)
      INTEGER*4 NPBOUN,LPBOUN(5,MPBOUN),IMODQ
      REAL*4    QBOUN(MPBOUN)
      INTEGER*4 IUT6,IERR
C
      INTEGER*4 LISTP(2,13)
      DATA LISTP /  4,  2,
     *              5,  3,
     *              7,  6,
     *             14,  8,
     *             15,  9,
     *             12, 10, 
     *             13, 11,
     *             26, 24,
     *             25, 27,
     *             22, 20,
     *             21, 23,
     *             18, 16,
     *             17, 19/      
C
      INTEGER*4 LCELL(3,2,13)
      DATA LCELL / 0,0,0, 1,0,0,
     *             0,0,0, 0,1,0,
     *             0,0,0, 0,0,1,
     *             0,0,0, 1,1,1,
     *             1,0,0, 0,1,1,
     *             1,1,0, 0,0,1,
     *             0,1,0, 1,0,1,
     *             0,0,0, 0,1,1,
     *             0,0,1, 0,1,0,
     *             0,0,0, 1,0,1,
     *             0,0,1, 1,0,0,
     *             0,0,0, 1,1,0,
     *             0,1,0, 1,0,0/  
C
C[INPUT]
C     NG                : CUBE SIZE (=2^N)
C     MPBOUN            :
C    QCELL(II,I,J,K,IC) : DISTANCE BETWEEN HOST-GRID AND INTERSECT-POINT
C
C[OUTPUT]
C    NPBOUN(IC)       NUMBER OF BOUNDARY GRID    
C    LPBOUN(I,IPB,IC) BOUNDARY GRID LIST
C                     1: POSITION OF BOUNDRY GRID IN I-DIRECTION 
C                     2: POSITION OF BOUNDRY GRID IN J-DIRECTION 
C                     3: POSITION OF BOUNDRY GRID IN K-DIRECTION 
C                     4: DIRECTION TO INNER GRID (2-15)
C                     5: BOUNDARY TYPE (ITYPE)
C                        ITYPE=1: WALL
C                        ITYPE=2: INLET
C                        ITYPE=3: MOVING-WALL
C                        ITYPE=4: FREE
C                        ITYPE=5: SYMMETRIC 
C    QBOUN(IPB,IC) NON-DIMENSIONAL DISTANCE TO INNER GRIDS 
C
C[LOCAL]
C   LISTP :PARTICL VELOCITY ID
C   LCELL :STARTING AND ENDING POSITION OF LINES BETWEEN GRIDS IN A CEll
C
      INTEGER*4 ITYPE
      DATA ITYPE /11/
C
      INTEGER*4 IC,I,J,K,II,NBC1,NBC2,I1,J1,K1,I2,J2,K2
C
      DO 1100 K =0,NG+1 
      DO 1200 J =0,NG+1 
      DO 1300 I =0,NG+1 
C
          DO 1400 II=1,M1 
              IF(QCELL(II,I,J,K).GE.1.0E1) GOTO 1400
              IF(NPBOUN+2.GT.MPBOUN) THEN
                  WRITE(IUT6,*) ' INSIFFICIENT MEMORY: STOP',MPBOUN
                  IERR=1
                  RETURN
              ENDIF
              NBC1=NPBOUN-1
              NBC2=NPBOUN
C
              I1=I+LCELL(1,1,II)
              J1=J+LCELL(2,1,II)
              K1=K+LCELL(3,1,II)
              IF( I1.GE.1 .AND. I1.LE.NG+1 .AND.
     *            J1.GE.1 .AND. J1.LE.NG+1 .AND.
     *            K1.GE.1 .AND. K1.LE.NG+1) THEN
                  NPBOUN=NPBOUN+1
                  LPBOUN(1,NPBOUN)=I1
                  LPBOUN(2,NPBOUN)=J1
                  LPBOUN(3,NPBOUN)=K1
                  LPBOUN(4,NPBOUN)=LISTP(1,II)
                  LPBOUN(5,NPBOUN)=ITYPE
                   QBOUN(  NPBOUN)=QCELL(II,I,J,K)
                  IF(IMODQ.EQ.1) QBOUN(NPBOUN) = 1.0
              ENDIF
C
              I2=I+LCELL(1,2,II)
              J2=J+LCELL(2,2,II)
              K2=K+LCELL(3,2,II)
              IF( I2.GE.1 .AND. I2.LE.NG+1 .AND.
     *            J2.GE.1 .AND. J2.LE.NG+1 .AND.
     *            K2.GE.1 .AND. K2.LE.NG+1) THEN
                  NPBOUN=NPBOUN+1
                  LPBOUN(1,NPBOUN)=I2
                  LPBOUN(2,NPBOUN)=J2
                  LPBOUN(3,NPBOUN)=K2
                  LPBOUN(4,NPBOUN)=LISTP(2,II)
                  LPBOUN(5,NPBOUN)=ITYPE
                   QBOUN(  NPBOUN)=1.0E0-QCELL(II,I,J,K)
                  IF(IMODQ.EQ.1) QBOUN(NPBOUN) = 1.0
              ENDIF
 1400     CONTINUE
C
 1300 CONTINUE   
 1200 CONTINUE   
 1100 CONTINUE   
C
      RETURN
      END
