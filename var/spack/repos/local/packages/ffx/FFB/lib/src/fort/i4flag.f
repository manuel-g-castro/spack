C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    I4FLAG                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      FUNCTION I4FLAG(INT4,IGET)
      IMPLICIT INTEGER*4(I-N)
      PARAMETER ( MBASE = 10 )
      DIMENSION IBASE(MBASE)
C
C
C      RETURN THE DECIMAL NUMBER RESIDING IN A SPECIFIED POSITION OF
C     A PASSED INTEGER
C         ( 2-D , 3-D CALCULATION & GRAPHICS )
C
C
C     NOTE 1 ; IGET MUST BE UNDER OR EQUAL TO 10 (AND GREATER THAN 0).
C             IF THE ABOVE CONDITION IS NOT MET, AN VALUE OF -1 WILL
C             BE RETURNED AS FUNCTION RETURN VALUE.
C
C     NOTE 2 ; IF NO NUMBER RESIDES IN THE SPECIFIED POSITION OF THE
C             PASSED INTEGER, AN VALUE OF 0 WILL BE RETURNED AS 
C             FUNCTION RETURN VALUE.
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          INT4        ; 4-BYTE INTEGER NUMBER
C          IGET        ; SPECIFIES THE POSITION FROM THE LOWEST
C
C       (5) RETURN VALUE
C          I4FLAG      ; DECIMAL NUMBER RESIDING IN THE SPECIFIED
C                       POSITION OF THE PASSED INTEGER NUMBER
C
C
      DATA IBASE /          1 ,
     &                     10 ,
     &                    100 ,
     &                   1000 ,
     &                  10000 ,
     &                 100000 ,
     &                1000000 ,
     &               10000000 ,
     &              100000000 ,
     &             1000000000 /
C
      IF(IGET.LE.0 .OR. IGET.GE.11) THEN
          I4FLAG = -1
          RETURN
      ENDIF
C
      IF(INT4.GE.0) THEN
          IDUM =  INT4
      ELSE
          IDUM = -INT4
      ENDIF
C
      I4FLAG = MOD(IDUM/IBASE(IGET),10)
C
C
      RETURN
      END
