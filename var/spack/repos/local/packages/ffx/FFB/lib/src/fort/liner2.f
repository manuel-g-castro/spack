C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    LINER2                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE LINER2(XS,YS,XE,YE,NPNT,X,Y,SR,NODE,NE,NP,N,
     *                  IENE,NEE,MAXEE,RPNT,XPNT,YPNT,IEPNT,SRPNT,
     *                  NMAX,EPS)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION X(NP),Y(NP),SR(NP),NODE(N,NE),IENE(MAXEE,NE),NEE(NE),
     1          RPNT(NPNT),XPNT(NPNT),YPNT(NPNT),IEPNT(NPNT),SRPNT(NPNT)
C
      MTWRN = 0
C
C
C      DEDUCE SCALAR DISTRIBUTION ALONG A GIVEN LINE
C         ( 2-D GRAPHICS )
C
C
C     NOTE 1 ; NPNT MUST BE GREATER THAN 1.
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          XS          ; ONE       EDGE POINT OF THE INTERPOLATION LINE
C          YS          ; ONE       EDGE POINT OF THE INTERPOLATION LINE
C          XE          ; THE OTHER EDGE POINT OF THE INTERPOLATION LINE
C          YE          ; THE OTHER EDGE POINT OF THE INTERPOLATION LINE
C          NPNT        ; NUMBER OF THE POINTS WHERE INTERPOLATION WILL
C                       BE DONE, INCLUDING BOTH EDGE POINTS
C          X       (IP); X-COORDINATES        OF NODES
C          Y       (IP); Y-COORDINATES        OF NODES
C          SR      (IP); SCALAR FIELD DEFINED AT NODES
C          NODE  (I,IE); NODE TABLE
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          NP          ; NUMBER OF TOTAL NODES
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT
C          IENE(IEE,IE); ADJACENT ELEMENT NUMBER TO ELEMENT IE
C          NEE     (IE); NUMBER OF ADJACENT ELEMENTS TO ELEMENT IE
C          MAXEE       ; THE FIRST DIMENSION OF ARRAY IENE
C          NMAX        ; MAXIMUM ITERATION NUMBER FOR SUBROUTINE INTERP
C          EPS         ; CONVERGENCE CRITERIA     FOR SUBROUTINE INTERP
C
C       (2) OUTPUT
C          RPNT  (IPNT); NORMALISED POSITIONS WHERE INTERPOLATION DONE
C          XPNT  (IPNT); X-DIR.     POSITIONS WHERE INTERPOLATION DONE
C          YPNT  (IPNT); Y-DIR.     POSITIONS WHERE INTERPOLATION DONE
C          IEPNT (IPNT); ELEMENT NO.'S        WHERE INTERPOLATION DONE
C          SRPNT (IPNT); INTERPOLATED SCALAR DISTRIBUTION
C
C      NOTE ; IF A POINT 'IPNT' IS NOT INCLUDED IN THE COMPUTATION
C            DOMAIN AT ALL, IEPNT(IPNT) WILL BE SET TO 0 AND
C            INTERPOLATION AT THE POINT WILL BE SKIPPED.
C
C
C      (1) CALCULATE THE COORDINATES WHERE INTERPOLATION WILL BE DONE
C
C
      DO 10 IPNT = 1 , NPNT
          RPNT(IPNT) = FLOAT(IPNT-1)/FLOAT(NPNT-1)
          XPNT(IPNT) = XS+RPNT(IPNT)*(XE-XS)
          YPNT(IPNT) = YS+RPNT(IPNT)*(YE-YS)
   10 CONTINUE
C
C
C      (2) FIND ELEMENT'S NO.'S WHERE THE ABOVE COORDINATES ARE INCLUDED
C
C
      DO 20 IPNT = 1 , NPNT
          IEPNT(IPNT) = 0
   20 CONTINUE
C
      DO 50 IPNT = 1 , NPNT
          XP = XPNT(IPNT)
          YP = YPNT(IPNT)
          IF(IPNT.EQ.1) GO TO 35
          IF(IEPNT(IPNT-1).NE.0) THEN
C
C SEARCH THE CURRENT ELEMENT
C
              CALL INCLUD(IEPNT(IPNT-1),X,Y,NODE,NE,NP,N,XP,YP,IRN)
              IF(IRN.EQ.1) THEN
                  IEPNT(IPNT) = IEPNT(IPNT-1)
                  GO TO 50
              ENDIF
C
C SEARCH THE AMBIENT ELEMENTS AROUND THE CURRENT ELEMENT
C
              DO 30 IEE = 1 , NEE(IEPNT(IPNT-1))
                  CALL INCLUD(IENE(IEE,IEPNT(IPNT-1)),X,Y,NODE,NE,NP,N,
     *                        XP,YP,IRN)
                  IF(IRN.EQ.1) THEN
                      IEPNT(IPNT) = IENE(IEE,IEPNT(IPNT-1))
                      GO TO 50
                  ENDIF
   30         CONTINUE
          ENDIF
C
   35     CONTINUE
C
C SEARCH ALL THE ELEMENTS
C
          DO 40 IE = 1 , NE
              CALL INCLUD(IE,X,Y,NODE,NE,NP,N,XP,YP,IRN)
              IF(IRN.EQ.1) THEN
                  IEPNT(IPNT) = IE
                  GO TO 50
              ENDIF
   40     CONTINUE
   50 CONTINUE
C
C
C      (3) INTERPOLATE SCALAR VALUES AT THE ABOVE COORDINATES
C
C
      DO 60 IPNT = 1 , NPNT
          SRPNT(IPNT) = 0.E0
   60 CONTINUE
C
      DO 70 IPNT = 1 , NPNT
          IF(IEPNT(IPNT).NE.0) THEN
          CALL INTERP(SR,X,Y,NODE,NE,NP,N,
     *                IEPNT(IPNT),XPNT(IPNT),YPNT(IPNT),
     *                SRPNT(IPNT),GDUM,EDUM,NMAX,EPS,MTWRN,IWRN,IRN)
          ENDIF
   70 CONTINUE
C
C
      RETURN
      END
