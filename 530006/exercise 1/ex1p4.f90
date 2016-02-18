program testrnd
  use rng
  use mtmod
  implicit none
  real(rk) :: r,sum,mean
  integer :: count,i
  
  call assignseed(42_ik) ! lcg and pmg
  !call sgrnd(42) ! mersenne twister

  count=0
  sum=0
  
  do i=1,1000000

     count=count+1

     !r=1.0*lcg()/12386880
     r=1.0*pmg()/2147483647
     !r=grnd()
     sum=sum+r

     if (mod(count,50)==0) then
        write(1,*) count, sum/count
     end if

  end do

end program testrnd
