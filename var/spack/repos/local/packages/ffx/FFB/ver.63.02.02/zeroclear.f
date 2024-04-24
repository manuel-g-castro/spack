      subroutine zeroclear(A, SZ)
      implicit none

      ! argument
      integer*4 :: SZ
      real*4    :: A(SZ)

      ! local
      integer*4 :: D, P, I

      D = SZ / 8

!$omp parallel private(P)
!$omp do
      do I=1,8
         P = 1 + D*(I-1)
         call zeroclear1(A(P), D)
      enddo
!$omp end do
!$omp end parallel

      return
      end subroutine zeroclear
