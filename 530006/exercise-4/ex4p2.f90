program main
  use mtmod
  implicit none
  integer, parameter :: dp=kind(1.d0)
  character(len=80) :: filename
  integer :: i,j,k,hj,ios
  real(dp), dimension(38) :: x,y
  real(dp) :: xm_true, Fk,dxm
  integer :: Nd, Nc, Nf

  ! Read the data

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

  do i=1,Nf
     print *, x(i), y(i)
  end do

  xm_true=sum(x)/38

  Nc=30 ! number of counts in 1 distribution

  Nd=5 ! number of distributions

  Fk=sum(y)

  hj=0

  dxm=0.1

  do i=1,Nd
     do j=1,Nc

  print *, xm_true, Fk

  


contains


end program main
