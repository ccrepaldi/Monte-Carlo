module lcgrandom ! Random number generator
! Author: Caike Crepaldi.
  implicit none
  integer,parameter :: ik=selected_int_kind(15)
  integer,parameter :: rk=selected_real_kind(15,307)
  integer(ik), private :: seed
  integer(ik) :: oldseed

contains

  subroutine assignseed(new_seed)
    implicit none
    integer(ik), intent(in) :: new_seed

    oldseed=seed

    seed=new_seed

  end subroutine assignseed

  integer(ik) function lcg(a,c,m)
    implicit none
    integer(ik),intent(in) :: a,c,m
    integer(ik) :: rndint

    rndint=mod((a*seed+c),m)

    ! print *, rndint
    call assignseed(rndint)
    !lcg=1.0*rndint/m
    lcg=rndint

    return

  end function lcg

end module lcgrandom

program testrnd
  use lcgrandom
  implicit none
  integer(ik) :: r1,r2,a,c,m
  integer :: count

  ! Passes the three rules

  m=12386880
  c=12103
  a=258061
  
  call assignseed(10850665_ik)

  r1=lcg(a,c,m)
  print *, "LCG 1 - Passes all the 3 rules"

  print *, "Period:"

    count=0
  do
     count=count+1
     r2=lcg(a,c,m)
     if (r2==r1) then
        exit
     end if
  end do

  print *, count

!!$  !*************************************
!!$
!!$  ! Fails the three rules
!!$
!!$  m=12386880
!!$  c=864
!!$  a=51871
!!$
!!$  call assignseed(10850665_ik)
!!$
!!$  
!!$
!!$  r1=lcg(a,c,m)
!!$  print *, "LCG - Fails all the 3 rules"
!!$
!!$  print *, "Period:"
!!$
!!$    count=0
!!$  do
!!$     count=count+1
!!$     r2=lcg(a,c,m)
!!$     if (r2==r1) then
!!$        exit
!!$     end if
!!$  end do
!!$
!!$  print *, count

  !*************************************

  ! Fails the 1st rule

  m=12386880
  c=43010
  a=258061

  call assignseed(10850665_ik)

  r1=lcg(a,c,m)
  print *, "LCG 2 - Fails the 1st rule"

  print *, "Period:"

    count=0
  do
     count=count+1
     r2=lcg(a,c,m)
     if (r2==r1) then
        exit
     end if
  end do

  print *, count

  !*************************************

  ! Fails the 2nd rule

  m=12386880
  c=12103
  a=103741

  call assignseed(10850665_ik)

  r1=lcg(a,c,m)
 print *, "LCG 3 - Fails the 2nd rule"

  print *, "Period:"

    count=0
  do
     count=count+1
     r2=lcg(a,c,m)
     if (r2==r1) then
        exit
     end if
  end do

  print *, count

    !*************************************

  ! Fails the 3rd rule

  m=12386880
  c=12103
  a=129031

  call assignseed(10850665_ik)

  r1=lcg(a,c,m)
  print *, "LCG 4 - Fails the 3rd rule"

  print *, "Period:"

    count=0
  do
     count=count+1
     r2=lcg(a,c,m)
     if (r2==r1) then
        exit
     end if
  end do

  print *, count


end program testrnd
