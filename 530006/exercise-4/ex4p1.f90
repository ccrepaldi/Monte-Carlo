!
! MCMC Algorithm
!
module values
  implicit none
  integer, parameter :: dp=kind(1.d0)
  real(dp), parameter :: pi=4.d0*atan(1.d0), k_b=8.6173324*1e-5, T=300, V=1000
end module values
  
program main
  use mtmod
  use values
  implicit none
  real(dp) :: dx,x,x0,u,p,p_prime, x_prime
  integer :: N, i, run
  integer :: Nmax
  real(dp) :: xmean, xsum, rms, xtheor
  integer :: narg
  character(len=80) :: arg

  narg=command_argument_count()

  if (narg==1) then
     call get_command_argument(1, arg)
     read(arg,*) Nmax
  else
     Nmax=1e6
  end if

  call sgrnd( 42 )

  rms=0.0

  run_loop: do run=1,500

     xsum=0.d0

     !print *, run


     ! Step 0

     x=0.001d0 ! some value, so that f(x)>0
     !dx=0.01d0
     dx=0.1d0 ! optimal value (tested for 1000 points)
     p=f(x)

     N=0

     !print *, "Initialization:"
     !print *, "x", x, "p", f(x), "dx", dx

     do while (N.lt.Nmax)

        N=N+1

        ! Step 1

        x0=x

        ! Step 2

        u=2.d0*grnd()-1.d0

        ! Step 3

        x_prime=x+(u*dx)

        ! Step 4

        p_prime=f(x_prime)

        if ((p_prime/p).ge.1.d0) then
           x=x_prime
           ! Step 5
           p=f(x)
           !print *, "x_prime accepted (1)", x_prime, p_prime
        else
           u=grnd()
           if ((p_prime/p).ge.u) then
              x=x_prime
              ! Step 5
              p=f(x)
              !print *, "x_prime accepted (2)", x_prime, p_prime
           else
              x=x0
              !print *, "x_prime not accepted (1)", x
           end if
        end if

        ! Step 6

        ! Do statistics on the current x and relevant properties

        !write(11,*) x

        xsum=xsum+x

     end do

     xmean = xsum/Nmax

     xtheor=3.d0/2*k_b*T

     rms=rms+(xmean-xtheor)**2

  end do run_loop

  rms=rms/run
  rms=sqrt(rms)

  print *, rms

contains

  real(dp) function f(E)
    use values
    implicit none
    real(dp), intent(in) :: E
    real(dp) :: a, b, c
    integer :: N

    N=Nmax

    a=(2.d0/sqrt(pi))

    b=1.d0*N/V

    c=exp(-E/(k_b*T))

    f=a*b*sqrt(E)/(k_b*T)**(3.d0/2)*c
    

  end function f


end program main
