module rng ! Random number generator
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

  integer(ik) function lcg()
    implicit none
     integer(ik) :: m=12386880,c=12103,a=258061
    ! integer :: m=16,a=5,c=3 ! seed= 7, period=16
    ! integer :: m=2303,a=330,c=100 ! period = m
    integer(ik) :: rndint

    rndint=mod((a*seed+c),m)

    ! print *, rndint
    call assignseed(rndint)
    !lcg=1.0*rndint/m
    lcg=rndint

    return

  end function lcg

  integer(ik) function pmg()
    implicit none
    integer(ik) :: m,c,a
    integer(ik) :: q,r
    integer(ik) :: rndint

    ! Park-Miller parameter values
    a=16807
    c=0
    m=2147483647

    ! Schrage approximate factorization algorithm

    q=m/a
    r=mod(m,a)

    !print *, m              ! debugging
    !print *, q, r, (a*q+r)
    
    rndint=a*seed-a*(seed/q)*q-r*(seed/q)

    if (rndint<0) rndint=rndint+m

    call assignseed(rndint) ! assign new seed

    !print *, rndint
    
    ! pmg=1.0*rndint/m

    pmg=rndint

    return

  end function pmg

end module rng
