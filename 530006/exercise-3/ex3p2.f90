program main
  use mtmod
  implicit none
  integer, parameter :: vector_size=1e6
  real(8) :: x,s
  integer :: i,j,k,count,Ndim
  real(8) :: V,Ve,hitrate,sigma,fsum,fmean,pi,f2,f2mean

  pi=4.0*atan(1.0)

  Ve=2.0

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

     V=2*Ve*fmean

     sigma=Ve*sqrt((f2mean-fmean**2)/count)

     print '(i0,T8,f10.6,T21,a,x,f10.6)', Ndim, V, '+/-', sigma

     Ve=V

  end do


contains


end program main
