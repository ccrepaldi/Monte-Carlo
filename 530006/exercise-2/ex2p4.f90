program natural_random
  implicit none
  character(len=1000500) :: string
  !character(len=6) :: num ! for generating a real number
  character(len=1) :: num
  character(len=80) :: fname
  !real :: randn ! for generating a real number
  integer :: randn
  real :: r
  integer :: n,i,ios

  !fname='e.txt'
  !fname='Pi.txt'
  !fname='sqrt2.txt'
  fname='sqrt3.txt'
  
  open(unit=2,file=fname, access='stream',status='old',action='read')
  read(2) string
  close(2)
  string(2:2)='7' ! removes the '.' (dot) and replaces it
  
!!$  ! Version 0
!!$  ! Generates random numbers between 0 and 1
!!$  do i=1,100
!!$
!!$     call random_number(r)
!!$     n=r*1e6
!!$     num=string(n:n+6)
!!$     read(num,*) randn
!!$
!!$     print *, randn*1e-6
!!$
!!$  end do

  !print *, string !OK

!!$  ! Version 1: for generating a lot more points
!!$  do i=1,1000000
!!$     call random_number(r)
!!$     n=r*1e6
!!$     num=string(n:n)
!!$     read(num,*,iostat=ios) randn
!!$     if (ios<0) then
!!$        num=string(n+1:n+1)
!!$        read(num,*) randn
!!$     end if
!!$     write(44,*) randn
!!$  end do

  ! Our problem:

  !call random_number(r)
  !n=r*1e6 ! get a index to use as a seed
  n=3 ! after the dot
  do i=1,1000000
!!$     if (n.gt.1000500) then
!!$        call random_number(r)
!!$        n=r*1e6
!!$     end if
     num=string(n:n)
     read(num,*,iostat=ios) randn
     if (ios<0) then ! new line or other character
        num=string(n+1:n+1)
        read(num,*) randn
     end if
     write(44,*) randn
     n=n+1
  end do

end program natural_random
