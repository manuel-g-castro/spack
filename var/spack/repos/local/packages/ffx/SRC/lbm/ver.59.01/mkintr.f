      SUBROUTINE MKINTR(NP,NG,MBC,M1,LLEVEL,LPOSI,DSCALE,
     *                  NTRIG,XTRIG,YTRIG,ZTRIG,
     *                  MPBOUN,NPBOUN,LPBOUN,QBOUN,
     *                  LWORK,QCELL,IMODQ,EPSTRG,IUT6,IUT0,IERR)
      IMPLICIT NONE
C
C     FUNCTION: MAKE INTERSECT DATA IN THE CUBE 
C
C
C[INPUT]
      INTEGER*4 NP,NG,MBC,LLEVEL,LPOSI(3),MPBOUN,M1
      REAL*8    DSCALE
      INTEGER*4 NTRIG 
      REAL*4    XTRIG(NTRIG),YTRIG(NTRIG),ZTRIG(NTRIG)
      INTEGER*4 IMODQ
      REAL*8    EPSTRG   
      INTEGER*4 IUT6,IUT0
C
C[INPUT-OUTPUT]
      INTEGER*4 NPBOUN,LPBOUN(5,MPBOUN)
      REAL*4    QBOUN(MPBOUN)
C
C[OUTPUT]
      INTEGER*4 IERR
C
C[WORK]
      INTEGER*4 LWORK(NP,NG+1,NG+1,NG+1)
      REAL*4    QCELL(M1,0:NG+1,0:NG+1,0:NG+1)
C
C[LOCAL]
      INTEGER*4 IP,IP1,IP2,IP3,ITMP1,ITMP2,IB,I,J,K,II,NPBOUN1
      REAL*8    DD,DG,BB1(6),BB2(6)
      INTEGER*4 NBLIST,LBLIST(3,MPBOUN)
      REAL*8    QLIST(3,M1,MPBOUN)
      REAL*8    XA,XC,XD,XE,X0,
     *          YA,YC,YD,YE,Y0,
     *          ZA,ZC,ZD,ZE,Z0,
     *          XX1,XX2,XX3,YY1,YY2,YY3,ZZ1,ZZ2,ZZ3,B1,B2,B3,
     *          C11,C12,C21,C22
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
      REAL*8 EPS1,EPS2
      DATA EPS1 /1.0D-3/
      DATA EPS2 /2.0D-1/
C
      EPS2=EPSTRG
C
      DD=REAL(DSCALE)
      DG=(DD*2.0E0**(LLEVEL-1))/FLOAT(NG)
      BB1(1)=DBLE(DSCALE)*LPOSI(1)-DG
      BB1(2)=DBLE(DSCALE)*LPOSI(2)-DG
      BB1(3)=DBLE(DSCALE)*LPOSI(3)-DG
      BB1(4)=BB1(1)+(DD*2.0E0**(LLEVEL-1))+DG
      BB1(5)=BB1(2)+(DD*2.0E0**(LLEVEL-1))+DG
      BB1(6)=BB1(3)+(DD*2.0E0**(LLEVEL-1))+DG
C
      BB2(1)= 1.0D8
      BB2(2)= 1.0D8
      BB2(3)= 1.0D8
      BB2(4)=-1.0D8
      BB2(5)=-1.0D8
      BB2(6)=-1.0D8
C
      DO 1000 IP=1,NTRIG/3
          IP1=3*(IP-1)+1
          IP2=3*(IP-1)+2
          IP3=3*(IP-1)+3
          BB2(1)=MIN(BB2(1),XTRIG(IP1),XTRIG(IP2),XTRIG(IP3))
          BB2(2)=MIN(BB2(2),YTRIG(IP1),YTRIG(IP2),YTRIG(IP3))
          BB2(3)=MIN(BB2(3),ZTRIG(IP1),ZTRIG(IP2),ZTRIG(IP3))
          BB2(4)=MAX(BB2(4),XTRIG(IP1),XTRIG(IP2),XTRIG(IP3))
          BB2(5)=MAX(BB2(5),YTRIG(IP1),YTRIG(IP2),YTRIG(IP3))
          BB2(6)=MAX(BB2(6),ZTRIG(IP1),ZTRIG(IP2),ZTRIG(IP3))
 1000 CONTINUE
C
      CALL CHKBB(ITMP1,BB1,BB2)
      CALL CHKBB(ITMP2,BB2,BB1)
C     IF(ITMP1.EQ.0 .AND. ITMP2.EQ.0) RETURN     
C
      DO 1100 K=0,NG+1
      DO 1200 J=0,NG+1
      DO 1300 I=0,NG+1
      DO 1400 II=1,M1
          QCELL(II,I,J,K)=1.0E2
 1400 CONTINUE
 1300 CONTINUE
 1200 CONTINUE
 1100 CONTINUE
C
      DO 2000 IP = 1,NTRIG/3
          IP1=3*(IP-1)+1
          IP2=3*(IP-1)+2
          IP3=3*(IP-1)+3
          BB2(1)=MIN(XTRIG(IP1),XTRIG(IP2),XTRIG(IP3))
          BB2(2)=MIN(YTRIG(IP1),YTRIG(IP2),YTRIG(IP3))
          BB2(3)=MIN(ZTRIG(IP1),ZTRIG(IP2),ZTRIG(IP3))
          BB2(4)=MAX(XTRIG(IP1),XTRIG(IP2),XTRIG(IP3))
          BB2(5)=MAX(YTRIG(IP1),YTRIG(IP2),YTRIG(IP3))
          BB2(6)=MAX(ZTRIG(IP1),ZTRIG(IP2),ZTRIG(IP3))
          CALL CHKBB(ITMP1,BB1,BB2)
          CALL CHKBB(ITMP2,BB2,BB1)
C         IF(ITMP1.EQ.0 .AND. ITMP2.EQ.0) GOTO 2000
C
          CALL USTSTA(25)
          CALL ITLIST(NG,LLEVEL,LPOSI,DSCALE,
     *                XTRIG(IP1),YTRIG(IP1),ZTRIG(IP1),
     *                MPBOUN,NBLIST,LBLIST)
          CALL USTEND(25)
C
          CALL USTSTA(26)
          XC=XTRIG(IP1)
          XD=XTRIG(IP2)
          XE=XTRIG(IP3)
          YC=YTRIG(IP1)
          YD=YTRIG(IP2)
          YE=YTRIG(IP3)
          ZC=ZTRIG(IP1)
          ZD=ZTRIG(IP2)
          ZE=ZTRIG(IP3)
C
          YY1=XC-XD
          YY2=YC-YD
          YY3=ZC-ZD
          ZZ1=XC-XE
          ZZ2=YC-YE
          ZZ3=ZC-ZE
C
          DO 2100 IB=1,NBLIST
              I=LBLIST(1,IB)
              J=LBLIST(2,IB)
              K=LBLIST(3,IB)
              X0=DD*DBLE(LPOSI(1))+DG*DBLE(I-1)  
              Y0=DD*DBLE(LPOSI(2))+DG*DBLE(J-1)  
              Z0=DD*DBLE(LPOSI(3))+DG*DBLE(K-1)  
C
CCC II=1
              B1=XC-( X0+DG*DBLE(LCELL(1,1,1)) )
              B2=YC-( Y0+DG*DBLE(LCELL(2,1,1)) )
              B3=ZC-( Z0+DG*DBLE(LCELL(3,1,1)) )
              C11=YY2
              C12=ZZ2
              C21=YY3
              C22=ZZ3
              XX2=( C22*B2-C12*B3)/(C11*C22-C12*C21)
              XX3=(-C21*B2+C11*B3)/(C11*C22-C12*C21)
              XX1=(B1-YY1*XX2-ZZ1*XX3)/DG
              QLIST(1,1,IB)=XX1
              QLIST(2,1,IB)=XX2
              QLIST(3,1,IB)=XX3
C
CCC II=2
              B1=XC-( X0+DG*DBLE(LCELL(1,1,2)) )
              B2=YC-( Y0+DG*DBLE(LCELL(2,1,2)) )
              B3=ZC-( Z0+DG*DBLE(LCELL(3,1,2)) )
              C11=YY1
              C12=ZZ1
              C21=YY3
              C22=ZZ3
              XX2=( C22*B1-C12*B3)/(C11*C22-C12*C21)
              XX3=(-C21*B1+C11*B3)/(C11*C22-C12*C21)
              XX1=(B2-YY2*XX2-ZZ2*XX3)/DG
              QLIST(1,2,IB)=XX1
              QLIST(2,2,IB)=XX2
              QLIST(3,2,IB)=XX3
C
CCC II=3
              B1=XC-( X0+DG*DBLE(LCELL(1,1,3)) )
              B2=YC-( Y0+DG*DBLE(LCELL(2,1,3)) )
              B3=ZC-( Z0+DG*DBLE(LCELL(3,1,3)) )
              C11=YY1
              C12=ZZ1
              C21=YY2
              C22=ZZ2
              XX2=( C22*B1-C12*B2)/(C11*C22-C12*C21)
              XX3=(-C21*B1+C11*B2)/(C11*C22-C12*C21)
              XX1=(B3-YY3*XX2-ZZ3*XX3)/DG
              QLIST(1,3,IB)=XX1
              QLIST(2,3,IB)=XX2
              QLIST(3,3,IB)=XX3
C
CCC II=4
              B1=XC-( X0+DG*DBLE(LCELL(1,1,4)) )
              B2=YC-( Y0+DG*DBLE(LCELL(2,1,4)) )
              B3=ZC-( Z0+DG*DBLE(LCELL(3,1,4)) )
              C11=YY2-YY1
              C12=ZZ2-ZZ1
              C21=YY3-YY1
              C22=ZZ3-ZZ1
              XX2=( C22*(B2-B1)-C12*(B3-B1))/(C11*C22-C12*C21)
              XX3=(-C21*(B2-B1)+C11*(B3-B1))/(C11*C22-C12*C21)
              XX1=(B1-YY1*XX2-ZZ1*XX3)/DG
              QLIST(1,4,IB)=XX1
              QLIST(2,4,IB)=XX2
              QLIST(3,4,IB)=XX3
C
CCC II=5
              B1=XC-( X0+DG*DBLE(LCELL(1,1,5)) )
              B2=YC-( Y0+DG*DBLE(LCELL(2,1,5)) )
              B3=ZC-( Z0+DG*DBLE(LCELL(3,1,5)) )
              C11=YY2+YY1
              C12=ZZ2+ZZ1
              C21=YY3+YY1
              C22=ZZ3+ZZ1
              XX2=( C22*(B2+B1)-C12*(B3+B1))/(C11*C22-C12*C21)
              XX3=(-C21*(B2+B1)+C11*(B3+B1))/(C11*C22-C12*C21)
              XX1=(-B1+YY1*XX2+ZZ1*XX3)/DG
              QLIST(1,5,IB)=XX1
              QLIST(2,5,IB)=XX2
              QLIST(3,5,IB)=XX3
C
CCC II=6
              B1=XC-( X0+DG*DBLE(LCELL(1,1,6)) )
              B2=YC-( Y0+DG*DBLE(LCELL(2,1,6)) )
              B3=ZC-( Z0+DG*DBLE(LCELL(3,1,6)) )
              C11=YY2-YY1
              C12=ZZ2-ZZ1
              C21=YY3+YY1
              C22=ZZ3+ZZ1
              XX2=( C22*(B2-B1)-C12*(B3+B1))/(C11*C22-C12*C21)
              XX3=(-C21*(B2-B1)+C11*(B3+B1))/(C11*C22-C12*C21)
              XX1=(-B1+YY1*XX2+ZZ1*XX3)/DG
              QLIST(1,6,IB)=XX1
              QLIST(2,6,IB)=XX2
              QLIST(3,6,IB)=XX3
C
CCC II=7
              B1=XC-( X0+DG*DBLE(LCELL(1,1,7)) )
              B2=YC-( Y0+DG*DBLE(LCELL(2,1,7)) )
              B3=ZC-( Z0+DG*DBLE(LCELL(3,1,7)) )
              C11=YY2+YY1
              C12=ZZ2+ZZ1
              C21=YY3-YY1
              C22=ZZ3-ZZ1
              XX2=( C22*(B2+B1)-C12*(B3-B1))/(C11*C22-C12*C21)
              XX3=(-C21*(B2+B1)+C11*(B3-B1))/(C11*C22-C12*C21)
              XX1=( B1-YY1*XX2-ZZ1*XX3)/DG
              QLIST(1,7,IB)=XX1
              QLIST(2,7,IB)=XX2
              QLIST(3,7,IB)=XX3
C
              IF(NP.EQ.15) GOTO 2100
C
CCC II=8
              B1=XC-( X0+DG*DBLE(LCELL(1,1,8)) )
              B2=YC-( Y0+DG*DBLE(LCELL(2,1,8)) )
              B3=ZC-( Z0+DG*DBLE(LCELL(3,1,8)) )
              C11=YY1
              C12=ZZ1
              C21=YY3-YY2
              C22=ZZ3-ZZ2
              XX2=( C22*(B1)-C12*(B3-B2))/(C11*C22-C12*C21)
              XX3=(-C21*(B1)+C11*(B3-B2))/(C11*C22-C12*C21)
              XX1=( B2-YY2*XX2-ZZ2*XX3)/DG
              QLIST(1,8,IB)=XX1
              QLIST(2,8,IB)=XX2
              QLIST(3,8,IB)=XX3
C
CCC II=9
              B1=XC-( X0+DG*DBLE(LCELL(1,1,9)) )
              B2=YC-( Y0+DG*DBLE(LCELL(2,1,9)) )
              B3=ZC-( Z0+DG*DBLE(LCELL(3,1,9)) )
              C11=YY1
              C12=ZZ1
              C21=YY3+YY2
              C22=ZZ3+ZZ2
              XX2=( C22*(B1)-C12*(B3+B2))/(C11*C22-C12*C21)
              XX3=(-C21*(B1)+C11*(B3+B2))/(C11*C22-C12*C21)
              XX1=( B2-YY2*XX2-ZZ2*XX3)/DG
              QLIST(1,9,IB)=XX1
              QLIST(2,9,IB)=XX2
              QLIST(3,9,IB)=XX3
C
CCC II=10
              B1=XC-( X0+DG*DBLE(LCELL(1,1,10)) )
              B2=YC-( Y0+DG*DBLE(LCELL(2,1,10)) )
              B3=ZC-( Z0+DG*DBLE(LCELL(3,1,10)) )
              C11=YY2
              C12=ZZ2
              C21=YY3-YY1
              C22=ZZ3-ZZ1
              XX2=( C22*(B2)-C12*(B3-B1))/(C11*C22-C12*C21)
              XX3=(-C21*(B2)+C11*(B3-B1))/(C11*C22-C12*C21)
              XX1=( B1-YY1*XX2-ZZ1*XX3)/DG
              QLIST(1,10,IB)=XX1
              QLIST(2,10,IB)=XX2
              QLIST(3,10,IB)=XX3
C
CCC II=11
              B1=XC-( X0+DG*DBLE(LCELL(1,1,11)) )
              B2=YC-( Y0+DG*DBLE(LCELL(2,1,11)) )
              B3=ZC-( Z0+DG*DBLE(LCELL(3,1,11)) )
              C11=YY2
              C12=ZZ2
              C21=YY3+YY1
              C22=ZZ3+ZZ1
              XX2=( C22*(B2)-C12*(B3+B1))/(C11*C22-C12*C21)
              XX3=(-C21*(B2)+C11*(B3+B1))/(C11*C22-C12*C21)
              XX1=( B1-YY1*XX2-ZZ1*XX3)/DG
              QLIST(1,11,IB)=XX1
              QLIST(2,11,IB)=XX2
              QLIST(3,11,IB)=XX3
C
CCC II=12
              B1=XC-( X0+DG*DBLE(LCELL(1,1,12)) )
              B2=YC-( Y0+DG*DBLE(LCELL(2,1,12)) )
              B3=ZC-( Z0+DG*DBLE(LCELL(3,1,12)) )
              C11=YY2-YY1
              C12=ZZ2-ZZ1
              C21=YY3
              C22=ZZ3
              XX2=( C22*(B2-B1)-C12*(B3))/(C11*C22-C12*C21)
              XX3=(-C21*(B2-B1)+C11*(B3))/(C11*C22-C12*C21)
              XX1=( B1-YY1*XX2-ZZ1*XX3)/DG
              QLIST(1,12,IB)=XX1
              QLIST(2,12,IB)=XX2
              QLIST(3,12,IB)=XX3
C
CCC II=13
              B1=XC-( X0+DG*DBLE(LCELL(1,1,13)) )
              B2=YC-( Y0+DG*DBLE(LCELL(2,1,13)) )
              B3=ZC-( Z0+DG*DBLE(LCELL(3,1,13)) )
              C11=YY2+YY1
              C12=ZZ2+ZZ1
              C21=YY3
              C22=ZZ3
              XX2=( C22*(B2+B1)-C12*(B3))/(C11*C22-C12*C21)
              XX3=(-C21*(B2+B1)+C11*(B3))/(C11*C22-C12*C21)
              XX1=( B1-YY1*XX2-ZZ1*XX3)/DG
              QLIST(1,13,IB)=XX1
              QLIST(2,13,IB)=XX2
              QLIST(3,13,IB)=XX3
C
 2100     CONTINUE   
          CALL USTEND(26)
C
          CALL USTSTA(27)
          DO 2300 IB=1,NBLIST
              I=LBLIST(1,IB)
              J=LBLIST(2,IB)
              K=LBLIST(3,IB)
              DO 2400 II=1,M1
                  XX1=QLIST(1,II,IB)
                  XX2=QLIST(2,II,IB)
                  XX3=QLIST(3,II,IB)
                  IF(      XX1.GE.-EPS1 .AND. XX1.LE.1.0+EPS1
     *               .AND. XX2.GE.-EPS2 .AND. XX2.LE.1.0+EPS2
     *               .AND. XX3.GE.-EPS2 .AND. XX3.LE.1.0+EPS2
     *            ) THEN
                      QCELL(II,I,J,K)=XX1
                  ENDIF 
 2400         CONTINUE   
 2300     CONTINUE   
          CALL USTEND(27)
C
 2000 CONTINUE   
C
C
      CALL USTSTA(28)
      NPBOUN1=NPBOUN
      CALL CNVQCL(NG,MPBOUN,M1,QCELL,NPBOUN,LPBOUN,QBOUN,IMODQ,
     *            IUT6,IERR)
      CALL USTEND(28)
C
      CALL USTSTA(29)
      CALL MODIFQ(NP,NG,NPBOUN1,NPBOUN,
     *            LPBOUN,QBOUN,LWORK)
      CALL USTEND(29)
C
      RETURN
      END
