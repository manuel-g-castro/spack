C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    MAXENT                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE MAXENT(IMODE,X,ND,DT,MMAX,FMIN,FMAX,NF,
     *                  PWS,F,MINM,Y,FPE,R,RR,RFPE,IUT0,IERR)
      IMPLICIT REAL*4(A-H,O-Z)
      DIMENSION X(ND),PWS(NF+1),F(NF+1),
     1          Y(0:ND),FPE(MMAX),R(MMAX),RR(MMAX),RFPE(MMAX)
C
      CHARACTER*60 ERMSGB
     & /' ## SUBROUTINE MAXENT: FATAL      ERROR OCCURENCE; RETURNED' /
      CHARACTER*60 EREXP1
     & /' SPECIFIED OPERATION MODE IS NOT SUPPORTED                 ' /
C
C
C      CALCULATE POWER SPECTRUM OF GIVEN DATA BY THE MAXIMUM ENTROPY
C     METHOD USING BURG'S ALGORITHM
C
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          IMODE       ; DETERMINES THE DIMENSION OF AUTO REGRESSION
C                       MODEL AS FOLLOWS:
C                   1 --- USE MINIMUM PREDICTION ERROR ALGORITHM
C                   2 --- USE SPECIFIED VALUE BY THE CALLING PROGRAM
C          X        (I); INPUT HISTORY DATA
C          ND          ; NUMBER OF SAMPLING POINTS FOR HISTORY DATA
C          DT          ; SAMPLING INTERVAL OF HISTORY DATA
C          MMAX        ; MAXIMUM DIMENSION OF AUTO REGRESSION MODEL
C          FMIN        ; MINIMUM FREQUENCY FOR SPECTRUM CALCULATION
C          FMAX        ; MAXIMUM FREQUENCY FOR SPECTRUM CALCULATION
C          NF          ; NUMBER OF POWER SPECTRUM CALCULATION POINTS
C          IUT0        ; FILE NUMBER TO WRITE  ERROR MESSAGE
C
C       (2) OUTPUT
C          PWS     (IF); POWER SPECTRUM
C          F       (IF); FREQUENCY WHERE POWER SPECTRUM IS CALCULATED
C          MINM        ; DETERMINED DIMENSION OF AUTO REGRESSION MODEL
C          IERR        ; RETURN CODE WHOSE VALUE WILL BE EITHER
C            0       --- INDICATING SUCCESSFUL TERMINATION
C        OR  1       --- INDICATING OCCURENCE OF SOME ERROR CONDITIONS
C
C       (4) WORK
C          Y        (I); STORES COPY OF INPUT HISTORY DATA
C          FPE      (M); STORES FINAL PREDICTION ERROR
C          R        (M); STORES AUTO REGRESSION COEFFICIENTS
C          RR       (M); STORES PREVIOUS AUTO REGRESSION COEFFICIENTS
C          RFPE     (M); STORES AUTO REGRESSION COEFFICIENTS AT MINIMUM
C                       PREDICTION ERROR
C
C
      IERR  = 0
C
      COEFF = 2.E0*3.141592*DT
      DF    = (FMAX-FMIN)/FLOAT(NF)
C
C CHECK PASSED PARAMETERS
C
      IF(IMODE.NE.1 .AND. IMODE.NE.2) THEN
          WRITE(IUT0,*) ERMSGB
          WRITE(IUT0,*) EREXP1
          IERR = 1
          RETURN
      ENDIF
C
C SET INITIAL VALUES
C
      SUM = 0.E0
      DO 10 I = 1 , ND
          SUM = SUM+X(I)
   10 CONTINUE
      AV = SUM/FLOAT(ND)
C
      SUM = 0.E0
      DO 20 I = 1 , ND
          Z      = X(I)-AV
          X(I)   = Z
          Y(I-1) = Z
          SUM    = SUM+Z*Z
   20 CONTINUE
      PM     = SUM/FLOAT(ND)
      FPEMIN = FLOAT(ND+1)/FLOAT(ND-1)*PM
C
C INCREASE DIMENSION OF AUTO REGRESSION MODEL
C
      DO 160 M = 1 , MMAX
          SUMN = 0.E0
          SUMD = 0.E0
          DO 110 I = 1 , ND-M
              SUMN = SUMN+X(I)*Y(I)
              SUMD = SUMD+X(I)*X(I)+Y(I)*Y(I)
  110     CONTINUE
C
          RM   = -2.E0*SUMN/SUMD
          R(M) = RM
          PM   = PM*(1.E0-RM*RM)
C
          DO 120 I = 1 , M-1
              R(I) = RR(I)+RM*RR(M-I)
  120     CONTINUE
C
          DO 130 I = 1 , M
              RR(I) = R(I)
  130     CONTINUE
C
          DO 140 I = 1 , ND-M-1
              X(I) = X(I)+RM*Y(I)
              Y(I) = Y(I+1)+RM*X(I+1)
  140     CONTINUE
C
          FPE(M) = FLOAT(ND+M+1)/FLOAT(ND-M-1)*PM
C
          IF(IMODE.EQ.1 .AND. FPE(M).LT.FPEMIN .OR.
     &       IMODE.EQ.2 .AND. M.EQ.MMAX) THEN
              FPEMIN = FPE(M)
              MINM   = M
              PMM    = PM
              DO 150 I = 1 , M
                  RFPE(I) = R(I)
  150         CONTINUE
          ENDIF
  160 CONTINUE
C
C CALCULATE POWER SPECTRUM
C
      DO 210 IF = 0 , NF
          F(IF+1) = FMIN+DF*FLOAT(IF)
C
          SUM1 = 1.E0
          SUM2 = 0.E0
          DO 200 J = 1 , MINM
              SUM1 = SUM1+RFPE(J)*COS(COEFF*F(IF+1)*J)
              SUM2 = SUM2+RFPE(J)*SIN(COEFF*F(IF+1)*J)
  200     CONTINUE
          PWS(IF+1) = PMM*DT/(SUM1*SUM1+SUM2*SUM2)
  210 CONTINUE
C
C
      RETURN
      END
