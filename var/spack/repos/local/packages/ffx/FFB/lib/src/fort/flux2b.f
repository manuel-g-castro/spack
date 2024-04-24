C======================================================================C
C                                                                      C
C SOFTWARE NAME : FRONTFLOW_BLUE.1.0                                   C
C                                                                      C
C  SUB ROUTINE    FLUX2B                                               C
C                                                                      C
C                                       WRITTEN BY C.KATO              C
C                                                                      C
C                                                                      C
C Contact address: The University of Tokyo, FSIS project               C  
C                                                                      C
C======================================================================C
      SUBROUTINE FLUX2B(IMODE,X,Y,V1,V2,NODE,NE,NP,N,LINE,NLINE,D,FLUX,
     *                  FLUXWK,IUT0,IERR)
      IMPLICIT REAL*4(A-H,O-Z)
      CHARACTER*60 ERMSG
      CHARACTER*60 EREXP
      DIMENSION X(NP),Y(NP),V1(NP),V2(NP),NODE(N,NE),LINE(NLINE),
     1          FLUX(NLINE),FLUXWK(NLINE)
C
      DIMENSION XE(4),YE(4),SE(4),DNX(4),DNY(4),G(4),E(4)
      DATA      G /  0.E0 ,  1.E0 ,  0.E0 , -1.E0 /
      DATA      E / -1.E0 ,  0.E0 ,  1.E0 ,  0.E0 /
      DATA ERMSG
     & /' *** SUBROUTINE FLUX2B REPORTS A FATAL ERROR OCCURENCE ***'/
      DATA EREXP
     & /' SPECIFIED LINE SEGMENT IS NOT A BORDERLINE SEGMENT.      '/
C
C
C      CALCULATE DIFFUSION FLUX ALONG A BORDERLINE SEGMENT IN 2D FIELD
C         ( 2-D GRAPHICS )
C
C
C     NOTES ; FOR A VECTOR FLUX, FLUX2B WILL CALCULATE FLUX OF THE
C            COMPONENT OF THE GIVEN VECTOR WHICH IS ALIGNED TO THE
C            LINE SEGMENT VECTOR. THE DIRECTION OF THE LINE SEGMENT
C            WILL BE DETERMINED SUCH THAT A VECTOR OBTAINED BY ROTATING
C            THE LINE SEGMENT VECTOR BY POSITIVE 90 DEGREES BE AN INWARD
C            NORMAL VECTOR AT THE LOCATION.
C
C     NOTES ; FLUX IS DEFINED POSITIVE IF IT IS OUTWARD VECTOR ( .IE.
C            DIRECTED TO OUTSIDE THE COMPUTATIONAL DOMAIN )
C
C     NOTES : ALL PAIRS OF LINE(ILINE),LINE(ILINE+1) (ILINE=1-
C            NLINE-1 MUST BE A SUBSET OF BORDERLINES. IF THIS IS
C            NOT THE CASE, FLUX2B WILL TERMINATE THE EXECUTION
C            AND RETURN THE SEQUENCE AFTER REPORTING AN ERROR
C            MESSAGE TO THE FILE NUMBER SPECIFIED BY IUT0.
C
C     ARGUMENT LISTINGS
C       (1) INPUT
C          IMODE       ; SPECIFIES TYPE OF FLUX TO BE CALCULATED
C                   1 --- FLUX OF A SCALAR QUANTITY
C                   2 --- FLUX OF A VECTOR QUANTITY
C          X(IP)       ; X-COORDINATES OF NODES
C          Y(IP)       ; Y-COORDINATES OF NODES
C          V1(IP)      ; 1ST COMPONENT OF VECTOR/SCALAR
C          V2(IP)      ; 2ND COMPONENT OF VECTOR
C          NODE(I,NE)  ; NODE TABLE BASED ON ELEMENT
C          NE          ; NUMBER OF TOTAL ELEMENTS
C          NP          ; NUMBER OF TOTAL NODES
C          N           ; NUMBER OF NODES ASSIGNED TO ONE ELEMENT
C          LINE(ILINE) ; NODE NUMBERS WHICH CONSTITUES LINE SEGMENTS
C          NLINE       ; NUMBER OF NODES TO CONSTRUCT LINE SEGMENTS
C          D           ; DIFFUSION COEFFICIENT
C          IUT0        ; DEVICE NUMBER TO REPORT ERROR OCCURENCE
C
C       (2) OUTPUT
C          FLUX(ILINE) ; FLUX AT NODES
C          IERR        ; RETURN CODE TO REPORT ERROR OCCURENCE
C                   0 --- NORMAL TERMINATION
C                   1 --- A FATAL ERROR HAS OCCURED
C
C       (4) WORK
C          FLUXWK(ILINE); FLUX AT CENTER OF ELEMENT'S SIDE
C
C
      IERR = 0
C
      DO 100 ILINE = 1 , NLINE-1
          IP1 = LINE(ILINE)
          IP2 = LINE(ILINE+1)
          NFACE = 0
          DO 20 IEF = 1 , NE
              DO 10 I = 1 , N
                  IF(NODE(I         ,IEF).EQ.IP1 .AND.
     &               NODE(MOD(I,N)+1,IEF).EQ.IP2
     &               .OR.
     &               NODE(I         ,IEF).EQ.IP2 .AND.
     &               NODE(MOD(I,N)+1,IEF).EQ.IP1) THEN
                      NFACE = NFACE+1
                      IE = IEF
                      IS = I
                      GO TO 20
                  ENDIF
   10         CONTINUE
   20     CONTINUE
          IF(NFACE.NE.1) THEN
              WRITE(IUT0,*) ERMSG
              WRITE(IUT0,*) EREXP
              IERR = 1
              RETURN
          ENDIF
C
          DO 30 I = 1 , N
               XE(I) = X(NODE(I,IE))
               YE(I) = Y(NODE(I,IE))
   30     CONTINUE
          CALL DERIVL(G(IS),E(IS),XE,YE,N,DNX,DNY)
C
          ISP = IS+1
          IF(ISP.EQ.5) ISP = 1
          ISM = IS-1
          IF(ISM.EQ.0) ISM = 4
          XS = XE(ISP)-XE(IS)
          YS = YE(ISP)-YE(IS)
          XC = XE(ISM)-XE(IS)
          YC = YE(ISM)-YE(IS)
          AS = SQRT(XS*XS+YS*YS)
          DIRECT = 1.E0
          IF(-YS*XC+XS*YC.LT.0.E0) DIRECT = -DIRECT
          XS = DIRECT*XS/AS
          YS = DIRECT*YS/AS
C
          DO 40 I = 1 , N
              IF(IMODE.EQ.1) THEN
                  SE(I) = V1(NODE(I,IE))
              ELSE
                  SE(I) = XS*V1(NODE(I,IE))+YS*V2(NODE(I,IE))
              ENDIF
   40     CONTINUE
          FLUXWK(ILINE) = 0.E0
          DO 50 I = 1 , N
              FLUXWK(ILINE) = FLUXWK(ILINE)
     &                       +D*SE(I)*(-YS*DNX(I)+XS*DNY(I))
   50     CONTINUE
  100 CONTINUE
C
      FLUX(1)     = 1.5*FLUXWK(1)      -0.5*FLUXWK(2)
      FLUX(NLINE) = 1.5*FLUXWK(NLINE-1)-0.5*FLUXWK(NLINE-2)
      DO 200 ILINE = 2 , NLINE-1
              FLUX(ILINE) = 0.5*(FLUXWK(ILINE-1)+FLUXWK(ILINE))
  200 CONTINUE
C
C
      RETURN
      END
