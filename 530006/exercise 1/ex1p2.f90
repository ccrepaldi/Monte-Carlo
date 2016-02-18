program testrnd
  use rng
  use mtmod
  implicit none
  real(rk) :: r1,r2,r3
  integer :: i
  
  call assignseed(42_ik)
  r1=1.0*lcg()/12386880
  print '(a,x,i0,x,a,x,f10.7)', "LCG - seed:", oldseed, " r1:", r1

  call assignseed(123_ik)
  r2=1.0*pmg()/2147483647
  print '(a,x,i0,x,a,x,f10.7)', "PM - seed:", oldseed, " r2:", r2

  call sgrnd(666)
  r3=grnd()
  print '(a,x,f10.7)', "MT - seed: 666, r3:", r3
  
end program testrnd
