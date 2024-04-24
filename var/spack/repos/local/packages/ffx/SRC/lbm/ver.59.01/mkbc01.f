      SUBROUTINE MKBC01(NP,LVEL,LREV,NG,NBC,LBC,
     *                  LBOUN,MPBOUN,NPBOUN,LPBOUN,QBOUN,
     *                  LWORK1,LWORK2,IERR)
      IMPLICIT NONE
C
      INTEGER*4 NP,LVEL(3,NP),LREV(NP),NG,NBC,LBC(5,NBC),LBOUN(6),
     *          MPBOUN,NPBOUN,LPBOUN(5,MPBOUN),IERR
      INTEGER*4 LWORK1(0:NG+2,0:NG+2,0:NG+2)
      INTEGER*4 LWORK2(  NG+1,  NG+1,  NG+1)
      REAL*4    QBOUN(MPBOUN)
C
C    LBC(II,IBC)   ATTRIBUTE DATA OF B.C. GROUPS
C                   II=1 B.C. GROUP ID (1-26) IN AN ADJACENT CUBE
C                   II=2 SUB-DOMAIN NUMBER OF AN ADJACENT CUBE
C                   II=3 CUBE NUMBER OF AN ADJACENT CUBE IN A DOMAIN
C                   II=4 RELATIVE LEVEL OF AN ADJACENT CUBE
C                    (-1: FINE, 0:SAME, 1:COARSE)
C                   II=5 POSITION IN COARSER CUBE
C
C    LLEVEL        LEVEL OF CUBES, WHICH INDICATE THE GRID RESOLUTION. 
C                  LEVEL=1 CORRESPONTDS THE FINEST GRID SIZE. A GRID SIZE
C                  WILL BE TWICE WITH ONE INCREMENT OF THE LEVEL.
C    LPOSI(3)      INDICATES THE POSITIONS OF CUBES, WHICH ARE NORMALIZED 
C                  BY THE MINIMUM CUBE SIZE.
C    NPBOUN        NUMBER OF BOUNDARY GRID    
C    LPBOUN(I,IPB) BOUNDARY GRID LIST
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
C    QBOUN(IPB)   NON-DIMENSIONAL DISTANCE TO INNER GRIDS 
C
      INTEGER*4 ID,ISIZE,I,J,K,IB,I1,J1,K1,I2,J2,K2,N1,N2,ITYPE,
     *          ITYPE1,ITYPE2,
     *          NSB,NSI,NEB,NEI,IS,IE,JS,JE,KS,KE,
     *          IBTYPE,IP,ICHK,ICHK1,ICHK2,ICHK3
C
      INTEGER*4 LBWORK(26)
C
      IERR=0
C
      DO 1000 IB=1,26
          LBWORK(IB)=1
 1000 CONTINUE
C
      DO 1100 IB=1,NBC
          ID=LBC(1,IB)
          LBWORK(ID)=0
 1100 CONTINUE
C
      DO 2000 I=0,NG+2
      DO 2100 J=0,NG+2
      DO 2200 K=0,NG+2
          IF(     I.EQ.0 .OR. I.EQ.NG+2
     *       .OR. J.EQ.0 .OR. J.EQ.NG+2
     *       .OR. K.EQ.0 .OR. K.EQ.NG+2 ) THEN
              LWORK1(I,J,K)=0
          ELSE
              LWORK1(I,J,K)=1
          ENDIF
 2200 CONTINUE   
 2100 CONTINUE   
 2000 CONTINUE   
C
      NSB=0
      NSI=1
      NEB=NG+2
      NEI=NG+1
      DO 2300 IB=1,26
          IF(LBWORK(IB).EQ.1) GOTO 2300    
C
          IF(IB.EQ.1) THEN
              IS=NSB;IE=NSB;JS=NSI;JE=NEI;KS=NSI;KE=NEI
          ELSE IF(IB.EQ.2) THEN
              IS=NEB;IE=NEB;JS=NSI;JE=NEI;KS=NSI;KE=NEI
          ELSE IF(IB.EQ.3) THEN
              IS=NSI;IE=NEI;JS=NSB;JE=NSB;KS=NSI;KE=NEI
          ELSE IF(IB.EQ.4) THEN
              IS=NSI;IE=NEI;JS=NEB;JE=NEB;KS=NSI;KE=NEI
          ELSE IF(IB.EQ.5) THEN
              IS=NSI;IE=NEI;JS=NSI;JE=NEI;KS=NSB;KE=NSB
          ELSE IF(IB.EQ.6) THEN
              IS=NSI;IE=NEI;JS=NSI;JE=NEI;KS=NEB;KE=NEB
          ELSE IF(IB.EQ.7) THEN
              IS=NSI;IE=NEI;JS=NSB;JE=NSB;KS=NSB;KE=NSB
          ELSE IF(IB.EQ.8) THEN
              IS=NSI;IE=NEI;JS=NEB;JE=NEB;KS=NSB;KE=NSB
          ELSE IF(IB.EQ.9) THEN
              IS=NSI;IE=NEI;JS=NEB;JE=NEB;KS=NEB;KE=NEB
          ELSE IF(IB.EQ.10) THEN
              IS=NSI;IE=NEI;JS=NSB;JE=NSB;KS=NEB;KE=NEB
          ELSE IF(IB.EQ.11) THEN
              IS=NSB;IE=NSB;JS=NSI;JE=NEI;KS=NSB;KE=NSB
          ELSE IF(IB.EQ.12) THEN
              IS=NEB;IE=NEB;JS=NSI;JE=NEI;KS=NSB;KE=NSB
          ELSE IF(IB.EQ.13) THEN
              IS=NEB;IE=NEB;JS=NSI;JE=NEI;KS=NEB;KE=NEB
          ELSE IF(IB.EQ.14) THEN
              IS=NSB;IE=NSB;JS=NSI;JE=NEI;KS=NEB;KE=NEB
          ELSE IF(IB.EQ.15) THEN
              IS=NSB;IE=NSB;JS=NSB;JE=NSB;KS=NSI;KE=NEI
          ELSE IF(IB.EQ.16) THEN
              IS=NEB;IE=NEB;JS=NSB;JE=NSB;KS=NSI;KE=NEI
          ELSE IF(IB.EQ.17) THEN
              IS=NEB;IE=NEB;JS=NEB;JE=NEB;KS=NSI;KE=NEI
          ELSE IF(IB.EQ.18) THEN
              IS=NSB;IE=NSB;JS=NEB;JE=NEB;KS=NSI;KE=NEI
          ELSE IF(IB.EQ.19) THEN
              IS=NSB;IE=NSB;JS=NSB;JE=NSB;KS=NSB;KE=NSB
          ELSE IF(IB.EQ.20) THEN
              IS=NEB;IE=NEB;JS=NSB;JE=NSB;KS=NSB;KE=NSB
          ELSE IF(IB.EQ.21) THEN
              IS=NEB;IE=NEB;JS=NEB;JE=NEB;KS=NSB;KE=NSB
          ELSE IF(IB.EQ.22) THEN
              IS=NSB;IE=NSB;JS=NEB;JE=NEB;KS=NSB;KE=NSB
          ELSE IF(IB.EQ.23) THEN
              IS=NSB;IE=NSB;JS=NSB;JE=NSB;KS=NEB;KE=NEB
          ELSE IF(IB.EQ.24) THEN
              IS=NEB;IE=NEB;JS=NSB;JE=NSB;KS=NEB;KE=NEB
          ELSE IF(IB.EQ.25) THEN
              IS=NEB;IE=NEB;JS=NEB;JE=NEB;KS=NEB;KE=NEB
          ELSE IF(IB.EQ.26) THEN
              IS=NSB;IE=NSB;JS=NEB;JE=NEB;KS=NEB;KE=NEB
          ENDIF
C
          DO 2400 K=KS,KE
          DO 2500 J=JS,JE
          DO 2600 I=IS,IE
             LWORK1(I,J,K)=1
 2600     CONTINUE
 2500     CONTINUE
 2400     CONTINUE
C
 2300 CONTINUE
C
      DO 3000 I=1,NG+1
      DO 3100 J=1,NG+1
      DO 3200 K=1,NG+1
          LWORK2(I,J,K)=0
 3200 CONTINUE   
 3100 CONTINUE   
 3000 CONTINUE   
C
      N1=1
      N2=NG+1
      NPBOUN =0
      DO 4000 ID=1,6
C
          IF(LBWORK(ID).EQ.0) GOTO 4000
C
          IF(ID.EQ.1) THEN
              IS=N1;IE=N1;JS=N1;JE=N2;KS=N1;KE=N2
          ELSE IF(ID.EQ.2) THEN
              IS=N2;IE=N2;JS=N1;JE=N2;KS=N1;KE=N2
          ELSE IF(ID.EQ.3) THEN
              IS=N1;IE=N2;JS=N1;JE=N1;KS=N1;KE=N2
          ELSE IF(ID.EQ.4) THEN
              IS=N1;IE=N2;JS=N2;JE=N2;KS=N1;KE=N2
          ELSE IF(ID.EQ.5) THEN
              IS=N1;IE=N2;JS=N1;JE=N2;KS=N1;KE=N1
          ELSE IF(ID.EQ.6) THEN
              IS=N1;IE=N2;JS=N1;JE=N2;KS=N2;KE=N2
          ENDIF
C
          DO 4100 K=KS,KE
          DO 4200 J=JS,JE
          DO 4300 I=IS,IE
              ITYPE1=LWORK2(I,J,K)
              ITYPE2=LBOUN(ID)
              IF(ITYPE1.EQ.0) THEN
                  LWORK2(I,J,K)=ITYPE2
              ELSE
                  LWORK2(I,J,K)=MIN(ITYPE1,ITYPE2)
              ENDIF
 4300     CONTINUE
 4200     CONTINUE
 4100     CONTINUE
C
 4000 CONTINUE
C
      NPBOUN =0
      DO 5000 K=1,NG+1
      DO 5100 J=1,NG+1
      DO 5200 I=1,NG+1
          IBTYPE=LWORK2(I,J,K)
          IF(IBTYPE.EQ.0) GOTO 5200
C
          DO 5300 IP=2,NP
C
              I1=I+LVEL(1,LREV(IP))
              J1=J+LVEL(2,LREV(IP))
              K1=K+LVEL(3,LREV(IP))
              I2=I+LVEL(1,IP)
              J2=J+LVEL(2,IP)
              K2=K+LVEL(3,IP)
C
              ICHK1=0
              ICHK2=0
              IF(LWORK1(I1,J1,K1).EQ.0 ) ICHK1=1
              IF(LWORK1(I2,J2,K2).EQ.1 ) ICHK2=1
C
C             ICHK=ICHK1*ICHK2
              ICHK=ICHK1
              IF(ICHK.EQ.1) THEN
                  NPBOUN=NPBOUN+1
                  IF(NPBOUN.GT.MPBOUN) THEN
                      IERR=1
                      RETURN
                  ENDIF 
                  LPBOUN(1,NPBOUN)=I
                  LPBOUN(2,NPBOUN)=J
                  LPBOUN(3,NPBOUN)=K
                  LPBOUN(4,NPBOUN)=IP
                  LPBOUN(5,NPBOUN)=IBTYPE
                   QBOUN(  NPBOUN)=0.0
              ENDIF
 5300     CONTINUE
C
 5200 CONTINUE
 5100 CONTINUE
 5000 CONTINUE
C
      RETURN
      END
