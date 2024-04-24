      subroutine int3bx(N2,X, Y, Z,NODE,N,Psi,xi,nv,w,enon,en,                  
     $     SNB,DNXB,DNYB,DNZB,IERR)
      implicit none
      
***** define arguments *****
      INTEGER N2
      real*8  X, Y, Z
      integer NODE
      real*8  N, Psi
      real*8  xi, nv, w
      integer enon, en
      real*4 SNB, DNXB, DNYB, DNZB
      dimension X(*)
      dimension Y(*)
      dimension Z(*)
      dimension NODE(N2,*)
C
      dimension N(8,4)
      dimension Psi(3,8,4)
      dimension xi(3,4)
      dimension nv(3)
      dimension w(4)
      dimension SNB (8)
      dimension DNXB(8)
      dimension DNYB(8)
      dimension DNZB(8)
      
***** objects *****
      real*8 Xe(3,8)
      real*8 J(3,3)
      real*8 detJ
      real*8 invJ(3,3)
      real*8 Phi(3,8)
      real*8 t(3)
      real*8 abst
      real*8 area
      INTEGER IERR
      REAL*8  tmp
      integer in, ip
      integer II,JJ,KK
      
***** make Xe *****
      do in=1,enon
         Xe(1,in) =X(NODE(in,en))
         Xe(2,in) =Y(NODE(in,en))
         Xe(3,in) =Z(NODE(in,en))
      enddo
C
      DO IN=1,ENON
         SNB( IN)=0.0E0
         DNXB(IN)=0.0E0
         DNYB(IN)=0.0E0
         DNZB(IN)=0.0E0
      ENDDO
C
**********loop**********
      DO IP=1,4

          DO II=1,3
             DO JJ=1,3
                TMP=0.0D0
                DO KK=1,ENON
                   TMP =TMP +PSI(II,KK,IP)*XE(JJ,KK)
                ENDDO
                J(II,JJ)=TMP
             ENDDO
          ENDDO
C
         
C
         detJ =J(1,1)*J(2,2)*J(3,3) -J(1,1)*J(2,3)*J(3,2)
     $        +J(1,2)*J(2,3)*J(3,1) -J(1,2)*J(2,1)*J(3,3)
     $        +J(1,3)*J(2,1)*J(3,2) -J(1,3)*J(2,2)*J(3,1)

         invJ(1,1) =(J(2,2)*J(3,3)-J(2,3)*J(3,2))/detJ
         invJ(1,2) =(J(1,3)*J(3,2)-J(1,2)*J(3,3))/detJ
         invJ(1,3) =(J(1,2)*J(2,3)-J(1,3)*J(2,2))/detJ
         invJ(2,1) =(J(2,3)*J(3,1)-J(2,1)*J(3,3))/detJ
         invJ(2,2) =(J(1,1)*J(3,3)-J(1,3)*J(3,1))/detJ
         invJ(2,3) =(J(1,3)*J(2,1)-J(1,1)*J(2,3))/detJ
         invJ(3,1) =(J(2,1)*J(3,2)-J(2,2)*J(3,1))/detJ
         invJ(3,2) =(J(1,2)*J(3,1)-J(1,1)*J(3,2))/detJ
         invJ(3,3) =(J(1,1)*J(2,2)-J(1,2)*J(2,1))/detJ

         DO II=1,3
             DO JJ=1,ENON
                 TMP=0.0E0
                 DO KK=1,3
                     TMP =TMP +INVJ(II,KK)*PSI(KK,JJ,IP)
                 ENDDO
                 PHI(II,JJ)=TMP
             ENDDO
         ENDDO

         DO II=1,3
             TMP=0.0E0
             DO KK=1,3
                 TMP=TMP+INVJ(II,KK)*NV(KK)
             ENDDO
            T(II)=TMP
         ENDDO
C
         abst=sqrt( t(1)**2 +t(2)**2 +t(3)**2 )

         area =w(ip)*detJ*abst ! vol
C
         if(detJ .lt. 0) then
            write(*,*) "[ERROR]@int3bx: detJ=", detJ, " < 0"
            IERR=1
            RETURN
         endif
         
***** addto *****
         DO IN=1,ENON
            SNB( IN) = SNB (IN) +REAL(AREA*N(IN,IP) )
            DNXB(IN) = DNXB(IN) +REAL(AREA*PHI(1,IN))
            DNYB(IN) = DNYB(IN) +REAL(AREA*PHI(2,IN))
            DNZB(IN) = DNZB(IN) +REAL(AREA*PHI(3,IN))
         ENDDO
      ENDDO
C      
      RETURN
      END
