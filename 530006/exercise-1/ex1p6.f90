program testrnd
  use rng
  use mtmod
  implicit none
  real(rk) :: x,y
  integer :: i,count
  
  !call sgrnd(42) ! mtg
  !call assignseed(8540585_ik) ! lcg or pmg
  call assignseed(42_ik) ! new seed to try

!!$  ! Part 1
!!$  do i=1,10000
!!$
!!$     x=(2.0*lcg()/12386880)-1.0 ! lcg
!!$     !x=(2.0*pmg()/2147483647)-1.0 ! pmg
!!$     !x=(2.0*grnd())-1.0 ! mtg
!!$     y=(2.0*lcg()/12386880)-1.0 ! lcg
!!$     !y=(2.0*pmg()/2147483647)-1.0 ! pmg
!!$     !y=(2.0*grnd())-1.0 ! mtg
!!$     
!!$     write(2,*) x,y
!!$
!!$  end do

  ! Part 2

  count=0

  do
     x=(2.0*lcg()/12386880)-1.0 ! lcg
     !x=(2.0*pmg()/2147483647)-1.0 ! pmg
     !x=(2.0*grnd())-1.0 ! mtg
     y=(2.0*lcg()/12386880)-1.0 ! lcg
     !y=(2.0*pmg()/2147483647)-1.0 ! pmg
     !y=(2.0*grnd())-1.0 ! mtg

     if ((x>-0.01.and.x<0.01).and.(y>-0.01.and.y<0.01)) then
        count=count+1
        write(2,*) x,y
        write(6,*) count
     end if

     if (count==1000) exit

  end do

end program testrnd
