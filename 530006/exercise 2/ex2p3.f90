program non_uniform_rn
  use rng
  use mtmod
  implicit none
  integer, parameter :: iter=1e6, M=1.15, b=10
  real :: x,y,yf
  real :: int
  integer :: i, count

  ! Note: Here we use the Mersenne Twister as a RN generator.

  call simpson(f,0.0,real(b),int,1000) ! integrates the function f(x)

  call sgrnd(66642)

  count=0

  do i=1,iter
     x=grnd()*b
     y=grnd()*M
     yf=f(x)/abs(int) ! works with the normalized version of f(x)
     if (y<yf) then
        write(31,*) x
        count=count+1
     end if
  end do

  print '(a,i0,a,i0)', 'Number of hits: ', count, '/', iter


contains

  real function f(x)
    implicit none
    real, intent(in) :: x

    f=abs(1-(x**4)/10*exp(-x/2)*(sin(5*x))**2)

  end function f

  ! Quick integration routine by simpson's method
  ! source: http://ww2.odu.edu/~agodunov/computing/programs/book2/Ch03/simpson.f90
  ! PS: I modified it a little bit (double precision -> real)

  subroutine simpson(f,a,b,integral,n)
    !==========================================================
    ! Integration of f(x) on [a,b]
    ! Method: Simpson rule for n intervals  
    ! written by: Alex Godunov (October 2009)
    !----------------------------------------------------------
    ! IN:
    ! f   - Function to integrate (supplied by a user)
    ! a	  - Lower limit of integration
    ! b	  - Upper limit of integration
    ! n   - number of intervals
    ! OUT:
    ! integral - Result of integration
    !==========================================================
    implicit none
    real f, a, b, integral,s
    real h, x
    integer nint
    integer n, i

    ! if n is odd we add +1 to make it even
    if((n/2)*2.ne.n) n=n+1

    ! loop over n (number of intervals)
    s = 0.0
    h = (b-a)/dfloat(n)
    do i=2, n-2, 2
       x   = a+dfloat(i)*h
       s = s + 2.0*f(x) + 4.0*f(x+h)
    end do
    integral = (s + f(a) + f(b) + 4.0*f(a+h))*h/3.0
    return
  end subroutine simpson

end program non_uniform_rn
