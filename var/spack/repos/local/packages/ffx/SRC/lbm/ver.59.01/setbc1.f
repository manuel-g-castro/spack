      SUBROUTINE SETBC1(ITIME,NG,NP,MBC,MBC1,MBC2,MBC3,
     *                  ILVL,LBC,TAU3,LVEL,
     *                  F,FEQ,NBC1,NBC2,NBC3,
     *                  FBC1,FBC1W,FBC2,FBC2W,FBC3,FBC3W,IUT6)
      IMPLICIT NONE
      INTEGER*4 ITIME,NG,NP,MBC,MBC1,MBC2,MBC3,ILVL,LBC(5),LVEL(3,NP),
     *          NBC1,NBC2,NBC3,IUT6
      REAL*8    F  (0:NG+2,0:NG+2,0:NG+2,NP)
      REAL*8    FEQ(0:NG+2,0:NG+2,0:NG+2,NP)
      REAL*8    TAU3(3)
      REAL*8    FBC1 (NP,NG+1,NG+1,MBC1),
     *          FBC1W(NP,NG+1,NG+1,MBC1),
     *          FBC2 (NP,NG+1,     MBC2),
     *          FBC2W(NP,NG+1,     MBC2),
     *          FBC3 (NP,          MBC3),
     *          FBC3W(NP,          MBC3)
C
C     ====FUNCTION====
C     SET CUBE-B.C. DATA TO WORK REIGION FBC[1,2,3]
C
C     ====VARIABLES LIST====
C[IN]
C     ITIME          :TIME STEP
C     NG             :CUBE SIZE (=2^N)
C     NP             :NUMBER OF PARTICLES (=15)
C     NC             :NUMBER OF CUBES IN SUB-DOMAIN
C     MBC            :MAX. NUMBER OF B.C GROUPS IN CUBES (=152)
C     MBC1           :MAX. NUMBER OF FACE-TYPE  B.C. GROUPS (=24)
C     MBC2           :MAX. NUMBER OF EDGE-TYPE  B.C. GROUPS (=24)
C     MBC3           :MAX. NUMBER OF POINT-TYPE B.C. GROUPS (= 8)
C     ILVL           :LEVEL OF CUBES, WHICH INDICATE THE GRID RESOLUTION. 
C                     LEVEL=1 CORRESPONTDS THE FINEST GRID SIZE. A GRID SIZE
C                     WILL BE TWICE WITH ONE INCREMENT OF THE LEVEL.
C     LBC(II,IBC,IC) :ATTRIBUTE DATA OF B.C. GROUPS
C                      II=1 B.C. GROUP ID (1-26) IN AN ADJACENT CUBE
C                      II=2 SUB-DOMAIN NUMBER OF AN ADJACENT CUBE
C                      II=3 CUBE NUMBER OF AN ADJACENT CUBE IN A DOMAIN
C                      II=4 RELATIVE LEVEL OF AN ADJACENT CUBE
C                       (-1: FINE, 0:SAME, 1:COARSE)
C                      II=5 POSITION IN COARSER CUBE
C     LVEL(3,NP)     :NORMALIZED VELOCITY OF PARTCLES (INTEGER)
C     F  (I,J,K,IP)  :PARTICLE DISTRIBUTION FUNCTION
C     FEQ(I,J,K,IP)  :PARTICLE EQUILIBRIUM DISTRIBUTION FUNCTION
C     TAU3(1)        :RELAXTATION TIME OF A FINER  CUBE  
C     TAU3(2)        :RELAXTATION TIME OF A SELF   CUBE  
C     TAU3(3)        :RELAXTATION TIME OF A COARSE CUBE  
C
C[IN-OUT]
C     NBC1 :NUMBER OF SOTRED FACE-TYPE  B.C. DATA
C     NBC2 :NUMBER OF SOTRED EDGE-TYPE  B.C. DATA
C     NBC3 :NUMBER OF SOTRED POINT-TYPE B.C. DATA                 
C     FBC1 (IP,I,J,IB,IC) :WORK REGION FOR FACE-TYPE  B.C. GROUPS
C     FBC2 (IP,I,  IB,IC) :WORK REGION FOR EDGE-TYPE  B.C. GROUPS
C     FBC3 (IP,    IB,IC) :WORK REGION FOR POINT-TYPE B.C. GROUPS
C     FBC1W(IP,I,J,IB,IC) :WORK REGION FOR FACE-TYPE  B.C. GROUPS
C     FBC2W(IP,I,J IB,IC) :WORK REGION FOR EDGE-TYPE  B.C. GROUPS
C     FBC3W(IP,    IB,IC) :WORK REGION FOR POINT-TYPE B.C. GROUPS

       INTEGER  ID,JC,ILVL2,ILEVEL,IPOSI,I,J,K,IP
C
      ID    = LBC(1)
      JC    = LBC(3)
      ILVL2 = LBC(4)
      IPOSI = LBC(5)
C
      IF(ID.LE.6) THEN
         NBC1=NBC1+1
      ELSE IF(ID.GE.7 .AND. ID.LE.18) THEN
         NBC2=NBC2+1
      ELSE
         NBC3=NBC3+1
      ENDIF
C
CCCC [FACE-TYPE B.C.]
      IF(ID.EQ.  1) 
     * CALL CBC001(ITIME,NG,NP,ILVL,ILVL2,IPOSI,F,FEQ,
     *             TAU3,LVEL, 
     *             FBC1(1,1,1,NBC1),FBC1W(1,1,1,NBC1))
      IF(ID.EQ.  2) 
     * CALL CBC002(ITIME,NG,NP,ILVL,ILVL2,IPOSI,F,FEQ,
     *             TAU3,LVEL, 
     *             FBC1(1,1,1,NBC1),FBC1W(1,1,1,NBC1))
      IF(ID.EQ.  3) 
     * CALL CBC003(ITIME,NG,NP,ILVL,ILVL2,IPOSI,F,FEQ,
     *             TAU3,LVEL, 
     *             FBC1(1,1,1,NBC1),FBC1W(1,1,1,NBC1))
      IF(ID.EQ.  4) 
     * CALL CBC004(ITIME,NG,NP,ILVL,ILVL2,IPOSI,F,FEQ,
     *             TAU3,LVEL, 
     *             FBC1(1,1,1,NBC1),FBC1W(1,1,1,NBC1))
      IF(ID.EQ.  5) 
     * CALL CBC005(ITIME,NG,NP,ILVL,ILVL2,IPOSI,F,FEQ,
     *             TAU3,LVEL, 
     *             FBC1(1,1,1,NBC1),FBC1W(1,1,1,NBC1))
      IF(ID.EQ.  6) 
     * CALL CBC006(ITIME,NG,NP,ILVL,ILVL2,IPOSI,F,FEQ,
     *             TAU3,LVEL, 
     *             FBC1(1,1,1,NBC1),FBC1W(1,1,1,NBC1))
C
CCCC [EDGE-TYPE B.C.]
      IF(ID.EQ.  7) 
     * CALL CBC007(ITIME,NG,NP,ILVL,ILVL2,IPOSI,F,FEQ,
     *             TAU3,LVEL, 
     *             FBC2(1,1,NBC2),FBC2W(1,1,NBC2))
      IF(ID.EQ.  8) 
     * CALL CBC008(ITIME,NG,NP,ILVL,ILVL2,IPOSI,F,FEQ,
     *             TAU3,LVEL, 
     *             FBC2(1,1,NBC2),FBC2W(1,1,NBC2))
      IF(ID.EQ.  9) 
     * CALL CBC009(ITIME,NG,NP,ILVL,ILVL2,IPOSI,F,FEQ,
     *             TAU3,LVEL, 
     *             FBC2(1,1,NBC2),FBC2W(1,1,NBC2))
      IF(ID.EQ. 10)
     * CALL CBC010(ITIME,NG,NP,ILVL,ILVL2,IPOSI,F,FEQ,
     *             TAU3,LVEL, 
     *             FBC2(1,1,NBC2),FBC2W(1,1,NBC2))
      IF(ID.EQ. 11) 
     * CALL CBC011(ITIME,NG,NP,ILVL,ILVL2,IPOSI,F,FEQ,
     *             TAU3,LVEL, 
     *             FBC2(1,1,NBC2),FBC2W(1,1,NBC2))
      IF(ID.EQ. 12) 
     * CALL CBC012(ITIME,NG,NP,ILVL,ILVL2,IPOSI,F,FEQ,
     *             TAU3,LVEL, 
     *             FBC2(1,1,NBC2),FBC2W(1,1,NBC2))
      IF(ID.EQ. 13) 
     * CALL CBC013(ITIME,NG,NP,ILVL,ILVL2,IPOSI,F,FEQ,
     *             TAU3,LVEL, 
     *             FBC2(1,1,NBC2),FBC2W(1,1,NBC2))
      IF(ID.EQ. 14) 
     * CALL CBC014(ITIME,NG,NP,ILVL,ILVL2,IPOSI,F,FEQ,
     *             TAU3,LVEL, 
     *             FBC2(1,1,NBC2),FBC2W(1,1,NBC2))
      IF(ID.EQ. 15) 
     * CALL CBC015(ITIME,NG,NP,ILVL,ILVL2,IPOSI,F,FEQ,
     *             TAU3,LVEL, 
     *             FBC2(1,1,NBC2),FBC2W(1,1,NBC2))
      IF(ID.EQ. 16) 
     * CALL CBC016(ITIME,NG,NP,ILVL,ILVL2,IPOSI,F,FEQ,
     *             TAU3,LVEL, 
     *             FBC2(1,1,NBC2),FBC2W(1,1,NBC2))
      IF(ID.EQ. 17) 
     * CALL CBC017(ITIME,NG,NP,ILVL,ILVL2,IPOSI,F,FEQ,
     *             TAU3,LVEL, 
     *             FBC2(1,1,NBC2),FBC2W(1,1,NBC2))
      IF(ID.EQ. 18) 
     * CALL CBC018(ITIME,NG,NP,ILVL,ILVL2,IPOSI,F,FEQ,
     *             TAU3,LVEL, 
     *             FBC2(1,1,NBC2),FBC2W(1,1,NBC2))
C
CCCC [POINT-TYPE B.C.]
      IF(ID.EQ. 19) 
     * CALL CBC019(ITIME,NG,NP,ILVL,ILVL2,IPOSI,F,FEQ,
     *             TAU3,LVEL, 
     *             FBC3(1,NBC3),FBC3W(1,NBC3))
      IF(ID.EQ. 20) 
     * CALL CBC020(ITIME,NG,NP,ILVL,ILVL2,IPOSI,F,FEQ,
     *             TAU3,LVEL, 
     *             FBC3(1,NBC3),FBC3W(1,NBC3))
      IF(ID.EQ. 21) 
     * CALL CBC021(ITIME,NG,NP,ILVL,ILVL2,IPOSI,F,FEQ,
     *             TAU3,LVEL, 
     *             FBC3(1,NBC3),FBC3W(1,NBC3))
      IF(ID.EQ. 22) 
     * CALL CBC022(ITIME,NG,NP,ILVL,ILVL2,IPOSI,F,FEQ,
     *             TAU3,LVEL, 
     *             FBC3(1,NBC3),FBC3W(1,NBC3))
      IF(ID.EQ. 23) 
     * CALL CBC023(ITIME,NG,NP,ILVL,ILVL2,IPOSI,F,FEQ,
     *             TAU3,LVEL, 
     *             FBC3(1,NBC3),FBC3W(1,NBC3))
      IF(ID.EQ. 24) 
     * CALL CBC024(ITIME,NG,NP,ILVL,ILVL2,IPOSI,F,FEQ,
     *             TAU3,LVEL, 
     *             FBC3(1,NBC3),FBC3W(1,NBC3))
      IF(ID.EQ. 25) 
     * CALL CBC025(ITIME,NG,NP,ILVL,ILVL2,IPOSI,F,FEQ,
     *             TAU3,LVEL, 
     *             FBC3(1,NBC3),FBC3W(1,NBC3))
      IF(ID.EQ. 26) 
     * CALL CBC026(ITIME,NG,NP,ILVL,ILVL2,IPOSI,F,FEQ,
     *             TAU3,LVEL, 
     *             FBC3(1,NBC3),FBC3W(1,NBC3))
C
      RETURN
      END
