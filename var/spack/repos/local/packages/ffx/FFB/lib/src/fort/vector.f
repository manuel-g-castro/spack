C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    VECTOR                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE VECTOR(IDIM,VN,NODE,NE,N,VEC)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION VN(*),NODE(N,NE),VEC(N,NE)
C
C
C      SET ELEMENT VECTOR FOR VECTOR OPERATIONS
C         ( 2-D , 3-D CALCULATION )
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          IDIM        ; SPECIFIES THE SPACE DIMENSION AS FOLLOWS
C                   2 --- TWO   DIMENSIONAL PROBLEM
C                   3 --- THREE DIMENSIONAL PROBLEM
C          VN      (IP); NODALLY DEFINED VARIABLE
C          NODE  (I,IE); NODE NO. TABLE BASED ON ELEMENT
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT
C
C       (2) OUTPUT
C          VEC   (I,IE); ELEMENT VECTOR OF VN(IP)
C
C
      IF(IDIM.EQ.2) THEN
          DO 100 IE = 1 , NE
             VEC(1,IE) = VN(NODE(1,IE))
             VEC(2,IE) = VN(NODE(2,IE))
             VEC(3,IE) = VN(NODE(3,IE))
             VEC(4,IE) = VN(NODE(4,IE))
  100     CONTINUE
C
      ELSE
          DO 200 IE = 1 , NE
             VEC(1,IE) = VN(NODE(1,IE)) 
             VEC(2,IE) = VN(NODE(2,IE)) 
             VEC(3,IE) = VN(NODE(3,IE))
             VEC(4,IE) = VN(NODE(4,IE)) 
             VEC(5,IE) = VN(NODE(5,IE))
             VEC(6,IE) = VN(NODE(6,IE))
             VEC(7,IE) = VN(NODE(7,IE))
             VEC(8,IE) = VN(NODE(8,IE))
  200     CONTINUE
      ENDIF
C
C
      RETURN
      END
