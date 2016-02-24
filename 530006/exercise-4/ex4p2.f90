program main
  use mtmod
  implicit none
  integer, parameter :: dp=kind(1.d0)
  character(len=80) :: filename
  integer :: i,j,k,hj,ios
  real(dp), dimension(38) :: x,y
  real(dp) :: xm_true,dxm,u,temp,Fk(37)
  integer :: Nd, Nc, Nf

  ! Read the data ****************************************

  filename='distr1.txt'

  Nf=38

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

  Nc=100 ! number of counts in 1 distribution

  Nd=5 ! number of distributions

  temp=0.d0

  do k=1,Nf-1

     temp=temp+(y(k)+y(k+1))/2*abs(x(k)-x(k+1))

     Fk(k)=temp  ! get the normalized cumulative distribution

  end do

  y(:)=y(:)/temp ! normalizes the distribution

    temp=0.d0

  do k=1,Nf-1

     temp=temp+(y(k)+y(k+1))/2*abs(x(k)-x(k+1))

     Fk(k)=temp  ! get the normalized cumulative distribution

  end do

  !do i=1,37
  !
  !   print *, Fk(i)
  !
  !end do

  hj=0

  dxm=0.1d0

  do i=1,Nd
     do j=1,Nc
        u=grnd()
        do k=1,Nf-1
           if((u.le.Fk(k)).and.(u.gt.Fk(k-1))) then
           end if
        end do
     end do
  end do


end program main
