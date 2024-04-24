      subroutine cnstvc(vc,x,isymop,u,dsplmt,nvc,nvcmx,g,dg)
c-----------------------------------------------------------------------
c     Given a new position x, this program generates positions that
c     are equivalent by symmetry and checks if they are already
c     listed in vc. If it is not the case add these new data to
c     vc and increase the number of data nvc.
c     Coded by H. Akai, Tokyo, Aug. 9, 2021
c-----------------------------------------------------------------------
      implicit real*8 (a-h,o-z)
      real*8 vc(5,nvcmx),x(4),u(9,48),dsplmt(3,48),xx(3),g(3,3),b(3)
      integer isymop(48)
      logical same
      data tiny/1d-6/
c     --- generate all the positions that are equivalent by symmetry.
      dg=dg+1d0
      do 10 iop=1,48
      if(isymop(iop) .eq. 1) then
c     --- operate the rotation and associated dispacement
      call rotatm(x,xx,u(1,iop),1)
      do 20 i=1,3
   20 xx(i)=xx(i)-dsplmt(i,iop)
c     --- convert from cartesian cordinates to a representaion
c         using the unit cell vectors a, b, and c.
c         form.
      do 30 i=1,3
      b(i)=0d0
      do 40 j=1,3
c     --- g are the unit reciprocal vectors
   40 b(i)=b(i)+xx(j)*g(j,i)
   30 b(i)=b(i)-dble(floor(b(i)+1d-10))
      do 50 i=1,3
   50 xx(i)=b(i)
c     write(*,'(2i3,3f12.7)')int(dg),iop,xx
c     --- if the same position has not been yet listed, this position
c         is added to the list as a new position.
      same=.false.
      do 60 j=1,nvc
      if((xx(1)-vc(1,j))**2+(xx(2)-vc(2,j))**2+(xx(3)-vc(3,j))**2
     &   .lt. tiny) then
c     write(*,'(a,2i3)')'the same position',j,int(vc(5,j))
      same=.true.
      exit
      endif
   60 continue
      if(.not. same) then
      nvc=nvc+1
c     if(nvc .gt. nvcmx)  call errtrp(1,'cnstvc','vc table overflows')
      call equarr(xx,vc(1,nvc),3)
      vc(4,nvc)=x(4)
      vc(5,nvc)=dg
      endif
      endif
   10 continue
      end
