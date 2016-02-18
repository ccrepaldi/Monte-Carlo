program testrnd
  use rng
  use mtmod
  implicit none
  integer(ik) :: r1,a,b
  integer :: count
  real :: first,newrnd
  
  call assignseed(10850665_ik)

  r1=lcg()
  print *, "LCG - seed:", oldseed, " r1:", 1.0*r1/12386880

  print *, "Period:"

  a=r1

    count=0
  do
     count=count+1
     b=lcg()
     if (b==a) then
        exit
     end if
  end do

  print *, count

  call assignseed(10850665_ik)

  a=pmg()

  print *, "PMG - seed:", oldseed, " r2:", 1.0*a/2147483647

  print *, "Period:"

    count=0
  do
     count=count+1
     b=pmg()
     if (b==a) then
        exit
     end if
  end do

  print *, count

  call sgrnd(10850665)

  first=grnd()

  print *, "MTG - seed:", 10850665, " r3:", first

  print *, "Period:"

    count=0
  do
     count=count+1
     newrnd=grnd()
     if (newrnd==first) then
        exit
     end if
  end do

  print *, count

end program testrnd
