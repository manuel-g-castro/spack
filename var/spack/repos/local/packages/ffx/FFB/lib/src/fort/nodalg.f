C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    NODALG                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE NODALG(VALE,NODE,NE,NP,N,VALN,IWORK,WORK)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION VALE(NE),NODE(N,NE),VALN(NP),IWORK(NP),WORK(NP)
C
C
C      ASSIGN ELEMENT VARIABLE TO EACH NODE
C         ( 2-D , 3-D GRAPHICS )
C
C
C     NOTE 1; VARIABLE VALUE AT EACH NODE WILL BE CALCULATED AS THE
C            SIMPLE AVERAGE OF VALUES AT THOSE ELEMENTS ADJACENT TO
C            THAT NODE.
C
C     NOTE 2; VALN(IP) IS PERMITTED TO BE THE SAME ARRAY AS VALE(IE).
C
C     NOTE 3; THIS SUBROUTINE IS APPLICABLE TO MIXED-ELEMENT MESH.
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          VALE(IE)    ; ELEMENT VALUE
C          NODE  (I,IE); NODE NO. TABLE BASED ON ELEMENT
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          NP          ; NUMBER OF TOTAL    NODES
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT
C
C       (2) OUTPUT
C          VALN(IP)    ; NODE ASSIGNED VALUE
C
C       (3) WORK
C          IWORK(IP)   ; REQUIRED SIZE = NUMBER OF TOTAL NODES
C           WORK(IP)   ; REQUIRED SIZE = NUMBER OF TOTAL NODES
C
C
      DO 100 IP = 1 , NP
          IWORK(IP) = 0
           WORK(IP) = 0.E0
  100 CONTINUE
C
      DO 210 IE = 1 , NE
          DO 200 I = 1 , N
              IP = NODE(I,IE)
              IF(IP.EQ.0) GO TO 200
              IWORK(IP) = IWORK(IP)+1
               WORK(IP) =  WORK(IP)+VALE(IE)
  200     CONTINUE
  210 CONTINUE
C
      DO 300 IP = 1 , NP
          VALN(IP) = WORK(IP)/IWORK(IP)
  300 CONTINUE
C
C
      RETURN
      END
