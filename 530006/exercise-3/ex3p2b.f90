program main
  use mtmod
  implicit none
  integer, parameter :: vector_size=1e6
  real(8) :: x,s
  integer :: i,j,k,count,Ndim
  real(8) :: V,hitrate,sigma,fsum,fmean,pi,f2,f2mean
  real(8) :: Ve(14)

  pi=4.0*atan(1.0)

  ! this time we are going to use V^(M) with the exact values.

  Ve=[2.d0,pi,4*pi/3,pi**2/2,8.d0/15*pi**2,&
       &1.d0/6*pi**3,16.d0/105*pi**3,1.d0/24*pi**4,32.d0/945*pi**4,&
       &1.d0/120*pi**5,64.d0/10395*pi**5,1.d0/720*pi**6,&
       &128.d0/135135*pi**6,1.d0/5040*pi**7]

  print '(a,T22,a)', 'Ndim', 'V'

  do Ndim=2,15

  ! function f(x)=sqrt(r**2-(|x|**2)), with x=(x1,x2,...,xN)
  ! r=1.0, f(x)=sqrt(1.0-(x1**2+x2^2+x3**2+...+xN**2))


     call sgrnd(666)

     fsum=0.0
     f2=0.0
     count=0

     ! Loop for [vector_size] [Ndim]-dimensional random numbers between 0 and 1

     do j=1,vector_size

        s=0.0 ! s=|x|**2=x1**2+x2^2+x3**2+...+xN**2

        do i=1,Ndim-1
           x=2*grnd()-1
           s=s+x**2
        end do
        
        if (s.le.1.0) then
           fsum=fsum+sqrt(1.0-s)
           f2=f2+(1.0-s)
           count=count+1
        end if
     end do

     fmean=fsum/count

     f2mean=f2/count

     V=2*Ve(Ndim-1)*fmean

     sigma=Ve(Ndim-1)*sqrt((f2mean-fmean**2)/count)

     print '(i0,T8,f10.6,T21,a,x,f10.6)', Ndim, V, '+/-', sigma

  end do


contains


end program main
