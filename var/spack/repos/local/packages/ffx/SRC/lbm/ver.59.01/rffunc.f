      SUBROUTINE RFFUNC(NG,NGH,NP,F,FWRK)
      IMPLICIT NONE
      INTEGER*4 NG,NGH,NP
      REAL*8    F(0:NG+2,0:NG+2,0:NG+2,NP)
      REAL*8    FWRK(NP,0:NGH+2,0:NGH+2,0:NGH+2)
C
      INTEGER*4 I,J,K,I1,I2,J1,J2,K1,K2,IP
      REAL*8    COEF
      DATA COEF /1.25D-1/
C
      DO 1000 IP=1,NP
      DO 1100 K =0,NG+2
          IF(MOD(K,2).EQ.0) THEN
              K1=K/2
              K2=K1+1
          ELSE
              K1=(K+1)/2
              K2=K1
          ENDIF
C
          DO 1200 J =0,NG+2
              IF(MOD(J,2).EQ.0) THEN
                  J1=J/2
                  J2=J1+1
              ELSE
                  J1=(J+1)/2
                  J2=J1
              ENDIF
C
                  DO 1300 I =0,NG+2
                      IF(MOD(I,2).EQ.0) THEN
                          I1=I/2
                          I2=I1+1
                      ELSE
                          I1=(I+1)/2
                          I2=I1
                      ENDIF
C
                      F(I,J,K,IP)=COEF*( FWRK(IP,I1,J1,K1)
     *                                  +FWRK(IP,I2,J1,K1)   
     *                                  +FWRK(IP,I1,J2,K1)   
     *                                  +FWRK(IP,I2,J2,K1)   
     *                                  +FWRK(IP,I1,J1,K2)   
     *                                  +FWRK(IP,I2,J1,K2)   
     *                                  +FWRK(IP,I1,J2,K2)   
     *                                  +FWRK(IP,I2,J2,K2) )  
 1300             CONTINUE
 1200         CONTINUE
 1100     CONTINUE
 1000 CONTINUE
C
      RETURN
      END
