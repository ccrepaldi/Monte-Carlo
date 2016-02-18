program main
  use mtmod
  implicit none
  integer, parameter :: dp = selected_real_kind(15, 307)
  integer :: grid_size, ninterval
  integer, parameter :: nsample=100
  real, parameter :: box=1.0
  integer :: vector_size
  real(dp), parameter :: I_exact=0.7034804524336
  real(dp) :: start, finish
  real(dp) :: dI
  real(dp) :: fmean, fsum, x,y, sigma, V, Ve,f2,f2mean
  real(dp) :: Vi(nsample), Vmean, s,stdmean
  integer :: count,i,j,k

  vector_size=1e2

  print *, 'ITEM (A)'

  print *

  print '(T10,a,T28,a,T52,a)', 'N', 'dI', 'dt'

  call sgrnd(666)

  do while (vector_size.le.1e6)

     ! *********************************************************

     !            HIT-AND-MISS

     call init_method()

     call cpu_time(start)

     do i=1,vector_size
        x=grnd()
        y=grnd()*10
        if (y<f(x)) count=count+1
     end do
     Ve=10
     V=Ve*count/vector_size
     dI=abs(I_exact-V)
     call cpu_time(finish)
     !sigma=Ve*sqrt(count-1.0*count**2/vector_size)/vector_size
     print '(a, T8,i0,T18,e20.13,T42,e20.13)', 'HM',vector_size, dI, finish-start
     write(31,*) vector_size, dI, finish-start

     ! *********************************************************

     !            DIRECT SAMPLING

     call init_method()

     call cpu_time(start)
    
     do i=1,vector_size
        x=grnd()
        fsum=fsum+f(x)
        f2=f2+(f(x))**2
        count=count+1
     end do
     fmean=fsum/count
     f2mean=f2/count
     Ve=1.0
     V=Ve*fmean
     dI=abs(I_exact-V)
     call cpu_time(finish)
     !sigma=Ve*sqrt((f2mean-fmean**2)/count)
     print '(a, T8,i0,T18,e20.13,T42,e20.13)', 'DS',vector_size, dI, finish-start
      write(32,*) vector_size, dI, finish-start

     ! *********************************************************

     !            STRATIFIED SAMPLING

     call init_method()

     grid_size=vector_size
     
     call cpu_time(start)

     do i=1,grid_size
        x=(i+grnd()-1)*(box/grid_size)
        fsum=fsum+f(x)
        f2=f2+(f(x))**2
        count=count+1
     end do
     fmean=fsum/count
     f2mean=f2/count
     Ve=1.0
     V=Ve*fmean
     dI=abs(I_exact-V)
     call cpu_time(finish)
     !sigma=Ve*sqrt((f2mean-fmean**2)/count)
     print '(a, T8,i0,T18,e20.13,T42,e20.13)', 'SS',vector_size, dI, finish-start
      write(33,*) vector_size, dI, finish-start

     call init_method()

     ! *********************************************************

     !            PARTIALLY STRATIFIED SAMPLING

     call init_method()

     grid_size=100

     ninterval=vector_size/grid_size
     
     call cpu_time(start)
     do j=1,ninterval
        do i=1,grid_size
           x=(i+grnd()-1)*(box/grid_size)
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
     call cpu_time(finish)
     !sigma=Ve*sqrt((f2mean-fmean**2)/count)
     print '(a, T8,i0,T18,e20.13,T42,e20.13)', 'PSS',vector_size, dI, finish-start
      write(34,*) vector_size, dI, finish-start

     ! *********************************************************

     !            IMPORTANCE SAMPLING

     call init_method()

     call cpu_time(start)
     ! importance sampling method
     do i=1,vector_size
        x=grnd()**(3.0/2.0)
        fsum=fsum+(f(x)/g(x))
        f2=f2+(f(x)/g(x))**2
        count=count+1
     end do
     fmean=fsum/count
     f2mean=f2/count
     Ve=1.0
     V=Ve*fmean
     dI=abs(I_exact-V)
     call cpu_time(finish)
     !sigma=Ve*sqrt((f2mean-fmean**2)/count)
     print '(a, T8,i0,T18,e20.13,T42,e20.13)', 'IS',vector_size, dI, finish-start
      write(35,*) vector_size, dI, finish-start

      vector_size=vector_size*10

      if (vector_size.le.1e6) print "(62('-'))"

     ! *********************************************************

  end do

  print *
  
  print *, 'ITEM (B)'

  print *

  vector_size=1e3

  ! *********************************************************

  !            HIT-AND-MISS

  do j=1,nsample

     call init_method()

     do i=1,vector_size
        x=grnd()
        y=grnd()*10
        if (y<f(x)) count=count+1
     end do
     Ve=10
     Vi(j)=Ve*count/vector_size

  end do

  Vmean = sum(Vi)/nsample
  s=0.d0
  do k=1,nsample
     s = s + (Vi(k)-Vmean)**2
  end do
  sigma=sqrt(1.d0/(nsample-1)*s)
  stdmean=sigma/sqrt(1.d0*nsample)

  print '(a,2x,es20.13)', 'HM: Standart Deviation of the Mean =', stdmean

  ! *********************************************************

  !            DIRECT SAMPLING

  do j=1,nsample

     call init_method()

     do i=1,vector_size
        x=grnd()
        fsum=fsum+f(x)
        f2=f2+(f(x))**2
        count=count+1
     end do
     fmean=fsum/count
     f2mean=f2/count
     Ve=1.0
     Vi(j)=Ve*fmean

  end do
  Vmean = sum(Vi)/nsample
  s=0.d0
  do k=1,nsample
     s = s + (Vi(k)-Vmean)**2
  end do
  sigma=sqrt(1.d0/(nsample-1)*s)
  stdmean=sigma/sqrt(1.d0*nsample)

  print '(a,2x,es20.13)', 'DS: Standart Deviation of the Mean =', stdmean

  ! *********************************************************

  !            STRATIFIED SAMPLING

  do j=1,nsample

     call init_method()

     grid_size=vector_size

     do i=1,grid_size
        x=(i+grnd()-1)*(box/grid_size)
        fsum=fsum+f(x)
        f2=f2+(f(x))**2
        count=count+1
     end do
     fmean=fsum/count
     f2mean=f2/count
     Ve=1.0
     Vi(j)=Ve*fmean
     
  end do
  Vmean = sum(Vi)/nsample
  s=0.d0
  do k=1,nsample
     s = s + (Vi(k)-Vmean)**2
  end do
  sigma=sqrt(1.d0/(nsample-1)*s)
  stdmean=sigma/sqrt(1.d0*nsample)

  print '(a,2x,es20.13)', 'SS: Standart Deviation of the Mean =', stdmean

  ! *********************************************************

  !            PARTIALLY STRATIFIED SAMPLING

   grid_size=100

   ninterval=vector_size/grid_size

  do k=1,nsample

     call init_method()

     ninterval=vector_size/grid_size
     
     do j=1,ninterval
        do i=1,grid_size
           x=(i+grnd()-1)*(box/grid_size)
           fsum=fsum+f(x)
           f2=f2+(f(x))**2
           count=count+1
        end do
     end do
     fmean=fsum/count
     f2mean=f2/count
     Ve=1.0
     Vi(j)=Ve*fmean

  end do

  Vmean = sum(Vi)/nsample
  s=0.d0
  do k=1,nsample
     s = s + (Vi(k)-Vmean)**2
  end do
  sigma=sqrt(1.d0/(nsample-1)*s)
  stdmean=sigma/sqrt(1.d0*nsample)

  print '(a,2x,es20.13)', 'PSS: Standart Deviation of the Mean =', stdmean

       ! *********************************************************

  !            IMPORTANCE SAMPLING

  do j=1,nsample

     call init_method()

     ! importance sampling method
     do i=1,vector_size
        x=grnd()**(3.0/2.0)
        fsum=fsum+(f(x)/g(x))
        f2=f2+(f(x)/g(x))**2
        count=count+1
     end do
     fmean=fsum/count
     f2mean=f2/count
     Ve=1.0
     Vi(j)=Ve*fmean

  end do

  Vmean = sum(Vi)/nsample
  s=0.d0
  do k=1,nsample
     s = s + (Vi(k)-Vmean)**2
  end do
  sigma=sqrt(1.d0/(nsample-1)*s)
  stdmean=sigma/sqrt(1.d0*nsample)

  print '(a,2x,es20.13)', 'IS: Standart Deviation of the Mean =', stdmean
  


contains

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
