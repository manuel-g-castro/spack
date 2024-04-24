      SUBROUTINE SETBC2(IPART,NG,NP,NC,MBC,MBC1,MBC2,MBC3,NBC,LBC,
     *                  MDOM,MPB,NDOM,LDOM,F,FBC1,FBC2,FBC3,BUFSND,
     *                  NBPSND,NBPRCV,IUT6)
      IMPLICIT NONE
      INTEGER*4 IPART,MBC,MBC1,MBC2,MBC3,MPB,MDOM,
     *          NC,NG,NP,NBC(NC),LBC(5,MBC,NC)
      INTEGER*4 NDOM,LDOM(MDOM),NBPSND(MDOM),NBPRCV(MDOM)
      REAL*8    F(0:NG+2,0:NG+2,0:NG+2,NP,NC),
     *          FBC1(NP,NG+1,NG+1,MBC1,NC),
     *          FBC2(NP,NG+1,     MBC2,NC),
     *          FBC3(NP,          MBC3,NC)
      REAL*8    BUFSND(MPB,*)
      INTEGER*4 IUT6
C
      INTEGER*4 IBC1,IBC2,IBC3,IBC,IC,JJ,
     *          ID,JC,JPART,ILEVEL,IPOSI,IDOM,IERR
      INTEGER*4 I,J,K,IP
      INTEGER*4 II(MDOM)
C
C     ====FUNCTION====
C     SET CUBE-B.C. DATA FROM THE SELF-CUBE 
C
C     ====VARIABLES LIST====
C[IN]
C     IPART          :DOMAIN NUMBER THAT THIS TASK SHOULD COMPUTE/IS COMPUTING. 
C                     IPART BEING SET ZERO MEANS THAT THE PROGRAM SHOULD RUN/IS 
C                     RUNNING IN SERIAL MODE.
C     NG             :CUBE SIZE (=2^N)
C     NP             :NUMBER OF PARTICLES (=15)
C     NC             :NUMBER OF CUBES IN SUB-DOMAIN
C     MBC            :MAX. NUMBER OF B.C GROUPS IN CUBES (=152)
C     MBC1           :MAX. NUMBER OF FACE-TYPE  B.C. GROUPS (=24)
C     MBC2           :MAX. NUMBER OF EDGE-TYPE  B.C. GROUPS (=24)
C     MBC3           :MAX. NUMBER OF POINT-TYPE B.C. GROUPS (= 8)
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
C     MDOM           :MAX. NUMBER OF THE NEIGHBORING SUB-DOMAINS
C     MPB             NUMBER OF DATA TO BE RECEIVED FROM THE NEIGHBORING 
C                     SUB-DOMAINS
C     NDOM           :NUMBER OF THE NEIGHBORING SUB-DOMAINS
C     LDOM(IDOM)     :NEIGHBORING SUB-DOMAIN NUMBER
C     FBC1 (IP,I,J,IB,IC) :WORK REGION FOR FACE-TYPE  B.C. GROUPS
C     FBC2 (IP,I,  IB,IC) :WORK REGION FOR EDGE-TYPE  B.C. GROUPS
C     FBC3 (IP,    IB,IC) :WORK REGION FOR POINT-TYPE B.C. GROUPS
C     FBC1W(IP,I,J,IB,IC) :WORK REGION FOR FACE-TYPE  B.C. GROUPS
C     FBC2W(IP,I,J IB,IC) :WORK REGION FOR EDGE-TYPE  B.C. GROUPS
C     FBC3W(IP,    IB,IC) :WORK REGION FOR POINT-TYPE B.C. GROUPS
C 
C[OUT]
C     F(I,J,K,IP)      :PARTICLE DISTRIBUTION FUNCTION
C     WRKSND(IPB,IDOM) :WORK REGION FOR RECEIVING THE PARTICLE DISTRIBUTION FUNCTION
C
      IERR=0
C
      IF(IPART.NE.0) THEN
      DO 100 IDOM=1,NDOM
          II    (  IDOM)=1
          BUFSND(1,IDOM)=0
  100 CONTINUE     
      ENDIF
C
C
      DO 1000 IC=1,NC
      IBC1=0
      IBC2=0
      IBC3=0
      DO 1100 IBC=1,NBC(IC)
          ID    =LBC(1,IBC,IC)
          JPART =LBC(2,IBC,IC)
          JC    =LBC(3,IBC,IC)
          ILEVEL=LBC(4,IBC,IC)
          IPOSI =LBC(5,IBC,IC)
C
          IF(ID.LE.6) THEN
              IBC1=IBC1+1
          ELSE IF (ID.GE.7 .AND. ID.LE.18) THEN
              IBC2=IBC2+1
          ELSE
              IBC3=IBC3+1
          ENDIF
C
          IF(IPART.EQ.0 .OR. IPART.EQ.JPART) THEN
CC[SELF DOMAIN]
C         FACE-CONNECTION
             IF(ID.EQ.01) 
     *       CALL CBC101(NG,NP,ILEVEL,IPOSI,
     *                   F(0,0,0,1,JC),FBC1(1,1,1,IBC1,IC))
             IF(ID.EQ.02) 
     *       CALL CBC102(NG,NP,ILEVEL,IPOSI,
     *                   F(0,0,0,1,JC),FBC1(1,1,1,IBC1,IC))
             IF(ID.EQ.03) 
     *       CALL CBC103(NG,NP,ILEVEL,IPOSI,
     *                   F(0,0,0,1,JC),FBC1(1,1,1,IBC1,IC))
             IF(ID.EQ.04) 
     *       CALL CBC104(NG,NP,ILEVEL,IPOSI,
     *                   F(0,0,0,1,JC),FBC1(1,1,1,IBC1,IC))
             IF(ID.EQ.05) 
     *       CALL CBC105(NG,NP,ILEVEL,IPOSI,
     *                   F(0,0,0,1,JC),FBC1(1,1,1,IBC1,IC))
             IF(ID.EQ.06) 
     *       CALL CBC106(NG,NP,ILEVEL,IPOSI,
     *                   F(0,0,0,1,JC),FBC1(1,1,1,IBC1,IC))
C
C         EDGE-CONNECTION
             IF(ID.EQ.07) 
     *       CALL CBC107(NG,NP,ILEVEL,IPOSI,
     *                   F(0,0,0,1,JC),FBC2(1,1,IBC2,IC))
             IF(ID.EQ.08) 
     *       CALL CBC108(NG,NP,ILEVEL,IPOSI,
     *                   F(0,0,0,1,JC),FBC2(1,1,IBC2,IC))
             IF(ID.EQ.09) 
     *       CALL CBC109(NG,NP,ILEVEL,IPOSI,
     *                   F(0,0,0,1,JC),FBC2(1,1,IBC2,IC))
             IF(ID.EQ.10) 
     *       CALL CBC110(NG,NP,ILEVEL,IPOSI,
     *                   F(0,0,0,1,JC),FBC2(1,1,IBC2,IC))
             IF(ID.EQ.11) 
     *       CALL CBC111(NG,NP,ILEVEL,IPOSI,
     *                   F(0,0,0,1,JC),FBC2(1,1,IBC2,IC))
             IF(ID.EQ.12) 
     *       CALL CBC112(NG,NP,ILEVEL,IPOSI,
     *                   F(0,0,0,1,JC),FBC2(1,1,IBC2,IC))
             IF(ID.EQ.13) 
     *       CALL CBC113(NG,NP,ILEVEL,IPOSI,
     *                   F(0,0,0,1,JC),FBC2(1,1,IBC2,IC))
             IF(ID.EQ.14) 
     *       CALL CBC114(NG,NP,ILEVEL,IPOSI,
     *                   F(0,0,0,1,JC),FBC2(1,1,IBC2,IC))
             IF(ID.EQ.15) 
     *       CALL CBC115(NG,NP,ILEVEL,IPOSI,
     *                   F(0,0,0,1,JC),FBC2(1,1,IBC2,IC))
             IF(ID.EQ.16) 
     *       CALL CBC116(NG,NP,ILEVEL,IPOSI,
     *                   F(0,0,0,1,JC),FBC2(1,1,IBC2,IC))
             IF(ID.EQ.17) 
     *       CALL CBC117(NG,NP,ILEVEL,IPOSI,
     *                   F(0,0,0,1,JC),FBC2(1,1,IBC2,IC))
             IF(ID.EQ.18) 
     *       CALL CBC118(NG,NP,ILEVEL,IPOSI,
     *                   F(0,0,0,1,JC),FBC2(1,1,IBC2,IC))
C
C         POINT-CONNECTION
             IF(ID.EQ.19)  
     *       CALL CBC119(NG,NP,ILEVEL,IPOSI,
     *                   F(0,0,0,1,JC),FBC3(1,IBC3,IC))
             IF(ID.EQ.20)  
     *       CALL CBC120(NG,NP,ILEVEL,IPOSI,
     *                   F(0,0,0,1,JC),FBC3(1,IBC3,IC))
             IF(ID.EQ.21)
     *       CALL CBC121(NG,NP,ILEVEL,IPOSI,
     *                   F(0,0,0,1,JC),FBC3(1,IBC3,IC))
             IF(ID.EQ.22)
     *       CALL CBC122(NG,NP,ILEVEL,IPOSI,
     *                   F(0,0,0,1,JC),FBC3(1,IBC3,IC))
             IF(ID.EQ.23)
     *       CALL CBC123(NG,NP,ILEVEL,IPOSI,
     *                   F(0,0,0,1,JC),FBC3(1,IBC3,IC))
             IF(ID.EQ.24)
     *       CALL CBC124(NG,NP,ILEVEL,IPOSI,
     *                   F(0,0,0,1,JC),FBC3(1,IBC3,IC))
             IF(ID.EQ.25)
     *       CALL CBC125(NG,NP,ILEVEL,IPOSI,
     *                   F(0,0,0,1,JC),FBC3(1,IBC3,IC))
             IF(ID.EQ.26)
     *       CALL CBC126(NG,NP,ILEVEL,IPOSI,
     *                   F(0,0,0,1,JC),FBC3(1,IBC3,IC))
C
          ELSE 
C
CC[OTHER DOMAIN]
C
           IF(IPART.EQ.0) GOTO 1000
C
           DO 1200 IDOM=1,NDOM
               IF(LDOM(IDOM).EQ.JPART) GOTO 1300
 1200      CONTINUE
 1300      CONTINUE
C 
              BUFSND(         1,IDOM)=INT(BUFSND(1,IDOM))+1
              BUFSND(II(IDOM)+1,IDOM)=JC
              BUFSND(II(IDOM)+2,IDOM)=ID
              BUFSND(II(IDOM)+3,IDOM)=ILEVEL
              BUFSND(II(IDOM)+4,IDOM)=IPOSI
              II(IDOM)=II(IDOM)+4
C
               JJ=II(IDOM)+1
              IF(ID.LE.6) THEN
C             FACE-CONNECTION
                  CALL CBC201(NG,NP,BUFSND(JJ,IDOM),FBC1(1,1,1,IBC1,IC))
                  II(IDOM)=II(IDOM)+NP*(NG+1)*(NG+1)
              ELSE IF (ID.GE.7 .AND. ID.LE.18) THEN
C             EDGE-CONNECTION
                  CALL CBC207(NG,NP,BUFSND(JJ,IDOM),FBC2(1,1,IBC2,IC))
                  II(IDOM)=II(IDOM)+NP*(NG+1)
              ELSE
C             POINT-CONNECTION
                  CALL CBC219(NG,NP,BUFSND(JJ,IDOM),FBC3(1,IBC3,IC))
                  II(IDOM)=II(IDOM)+NP
              ENDIF
C
           ENDIF
C
 1100      CONTINUE
 1000  CONTINUE
C
      RETURN
      END
