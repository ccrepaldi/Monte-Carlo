program main
  use mtmod
  implicit none
  integer, parameter :: dp=kind(1.d0), Nf=38, Nc=100, Nd=5, Nh=10
  character(len=80) :: filename
  integer :: i,j,k,ios
  real(dp), dimension(Nf) :: x,y
  real(dp) :: xm_true,xmean,dxm,u,temp,Fk(Nf),means(Nd)
  integer :: Dk(Nf),hj(Nh)

  ! Read the data ****************************************

  filename='distr1.txt'

  !Nf=38 ! Number of points in the distribution (declared as a parameter)

  open(unit=1,file=filename,iostat=ios,status='old')
  if (ios/=0) then
     print '(a,a)','*** Error in opening file ',trim(filename)
     stop
  end if
  
  do i=1,Nf
     read(1,*,iostat=ios) x(i),y(i)
     if (ios<0) exit
  end do
  close(1)

  !do i=1,Nf
  !   print *, x(i), y(i)
  !end do

  ! *******************************************************

  xm_true=sum(x)/38 ! get the true mean of the distribution

  !Nc=100 ! number of counts in 1 distribution (declared as a parameter)

  !Nd=5 ! number of distributions (declared as a parameter)

  !temp=0.d0

  y(:)=y(:)/sum(y) ! normalizes the distribution

  do k=1,Nf
  !
  !   temp=temp+(y(k)+y(k+1))/2*abs(x(k)-x(k+1))
  !
  !   Fk(k)=temp  ! get the normalized cumulative distribution

     Fk(k)=sum(y(1:k))  ! get the normalized cumulative distribution

  end do

  !do i=1,Nf
  !   print *, Fk(i)
  !end do

  hj=0

  dxm=100.d0
  hj(:)=0
  do i=1,Nd
     Dk(:)=0.d0
     do j=1,Nc
        u=grnd()
        do k=1,Nf
           if((u.le.Fk(k)).and.(u.gt.Fk(k-1))) then
              Dk(k)=Dk(k)+1 ! generate 1 count
           end if
        end do
     end do
     !do k=1,Nf
     !   if (i==3) print *, Dk(k) ! checking vector Dk (OK)
     !end do
     xmean=0.d0
     do k=1,Nf
        xmean=xmean+Dk(k)*x(k)
     end do
     xmean=xmean/Nc
     means(i)=xmean
     !print *, sum(Dk), Nc, xmean, xm_true ! checking values
     do j=1,Nh
        if (j==int(xmean/dxm)) hj(j)=hj(j)+1 ! doesnt make any sense
     end do

     print *, hj
  end do


end program main
