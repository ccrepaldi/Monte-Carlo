program main
  use mtmod
  implicit none
  integer, parameter :: dp = selected_real_kind(15, 307)
  real(dp) :: r(10)
  integer :: grid_size, ninterval
  real, parameter :: box=1.0
  integer :: vector_size
  real(dp), parameter :: I_exact=0.7034804524336
  real(dp) :: fmean, fsum, x,y, sigma, V, Ve,f2,f2mean,dI
  integer :: count,i,j,k
  integer :: my_base,current_seed

  print "(40('-'))"

  print *, 'ITEM (A)'

  print *

  print '(T5,a)', 'BASE 7'

  call halton_sequence(10,r,501,7)
  
  write(*,'(T12,f8.4)') r

  print *

  !print "(40('-'))"

  print '(T5,a)', 'BASE 13'

  call halton_sequence(10,r,501,13)

  write(*,'(T12,f8.4)') r

  print "(40('-'))"

  call init_method()

  
  print *, 'ITEM (B)'

  print *

  my_base=19

  vector_size=1e2

  print '(T10,a,T28,a)', 'N', 'dI'

  do while (vector_size.le.1e6)

     ! *********************************************************

     !            HIT-AND-MISS
     
     do i=1,vector_size
        x=hrn(11)
        y=hrn(19)*10
        if (y<f(x)) count=count+1
     end do
     Ve=10
     V=Ve*count/vector_size
     !sigma=Ve*sqrt(count-1.0*count**2/vector_size)/vector_size
     dI=abs(I_exact-V)
     print '(a, T8,i0,T18,e20.13)', 'HM', vector_size, dI
     write(41,*) vector_size, dI

     ! *********************************************************

     !            DIRECT SAMPLING

     call init_method()

     my_base=11

     do i=1,vector_size
        x=hrn(my_base)
        fsum=fsum+f(x)
        f2=f2+(f(x))**2
        count=count+1
     end do
     fmean=fsum/count
     f2mean=f2/count
     Ve=1.0
     V=Ve*fmean
     !sigma=Ve*sqrt((f2mean-fmean**2)/count)
     dI=abs(I_exact-V)
     print '(a, T8,i0,T18,e20.13)', 'DS', vector_size, dI
     write(42,*) vector_size, dI

     call init_method()

     ! *********************************************************

     !            STRATIFIED SAMPLING

     call init_method()

     my_base=11

     grid_size=vector_size

     do i=1,grid_size
        x=(i+hrn(my_base)-1)*(box/grid_size)
        fsum=fsum+f(x)
        f2=f2+(f(x))**2
        count=count+1
     end do
     fmean=fsum/count
     f2mean=f2/count
     Ve=1.0
     V=Ve*fmean
     !sigma=Ve*sqrt((f2mean-fmean**2)/count)
     print '(a, T8,i0,T18,e20.13)', 'SS',vector_size, dI
     write(43,*) vector_size, dI

     call init_method()

     ! *********************************************************

     !            PARTIALLY STRATIFIED SAMPLING

     call init_method()

     my_base=11
     grid_size=100

     ninterval=vector_size/grid_size
     
     do j=1,ninterval
        do i=1,grid_size
           x=(i+hrn(my_base)-1)*(box/grid_size)
           fsum=fsum+f(x)
           f2=f2+(f(x))**2
           count=count+1
        end do
     end do
     fmean=fsum/count
     f2mean=f2/count
     Ve=1.0
     V=Ve*fmean
     dI=abs(I_exact-V)
     !sigma=Ve*sqrt((f2mean-fmean**2)/count)
     print '(a, T8,i0,T18,e20.13)', 'PSS',vector_size, dI
     write(44,*) vector_size, dI

     ! *********************************************************

     !            IMPORTANCE SAMPLING

     my_base=11
     
     do i=1,vector_size
        x=hrn(my_base)**(3.0/2.0)
        fsum=fsum+(f(x)/g(x))
        f2=f2+(f(x)/g(x))**2
        count=count+1
     end do
     fmean=fsum/count
     f2mean=f2/count
     Ve=1.0
     V=Ve*fmean
     dI=abs(I_exact-V)
     !sigma=Ve*sqrt((f2mean-fmean**2)/count)
     print '(a, T8,i0,T18,e20.13)', 'IS',vector_size, dI
     write(45,*) vector_size, dI

     vector_size=vector_size*10

     if (vector_size.le.1e6) print "(38('-'))"

  end do


contains

  real function halton(index, base)
    implicit none
    integer, intent(in) :: index, base
    integer :: i
    real :: f
    halton=0.0
    f = 1
    i = index
    do while (i>0)
       f = f / base
       halton = halton + f * mod(i, base)
       i = floor(1.0*i/base)
    end do
    
  end function halton

  real(dp) function hrn(base)
    implicit none
    integer, intent(in) :: base
    integer :: seed=666

    hrn=halton(seed,base)
    seed=seed+1

  end function hrn

  subroutine halton_sequence(n,x,a,b)
    implicit none
    integer, intent(in) :: n
    real(dp), dimension(n), intent(out) :: x
    integer, intent(in) :: b,a
    integer :: i

    do i=1,n
       x(i)=halton(a+i-1,b)
    end do
    
  end subroutine halton_sequence

  real(dp) function f(x)
    implicit none
    real(dp), intent(in) :: x
    real(dp) :: a,b

    a=1.d0/3
    b=1.d0/4

    f=1/((x**a)+(x**b))

  end function f

  
  real(dp) function g(x)
    implicit none
    real(dp), intent(in) :: x

    ! function g normalized in the interval [0,1]

    g=2.d0/3*x**(-1.d0/3)

  end function g

  subroutine init_method()
    implicit none

    count=0
    fsum=0.d0
    f2=0.d0


  end subroutine init_method

end program main
