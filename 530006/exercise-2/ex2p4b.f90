program natural_random
  implicit none
  character(len=1000500) :: string
  character(len=1) :: num
  character(len=80) :: fname
  integer :: randn
  real, parameter :: r=1.0, deg2rad=0.01745329251
  real :: x,y
  integer :: n,i,ios

  !fname='e.txt'
  !fname='Pi.txt'
  fname='sqrt2.txt'
  !fname='sqrt3.txt'
  
  open(unit=2,file=fname, access='stream',status='old',action='read')
  read(2) string
  close(2)
  string(2:2)='7' ! removes the '.' (dot) and replaces it
  
  ! Our problem:
  x=0.0
  y=0.0

  n=3 ! after the dot
  do i=1,100000
     num=string(n:n)
     read(num,*,iostat=ios) randn
     if (ios<0) then ! new line or other character
        num=string(n+1:n+1)
        read(num,*) randn
     end if
     x=x+r*cos(36.0*randn*deg2rad)
     y=y+r*sin(36.0*randn*deg2rad)
     write(45,*) x,y
     n=n+1
  end do

end program natural_random
