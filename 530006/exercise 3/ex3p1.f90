program main
  use mtmod
  implicit none
  integer, parameter :: vector_size=1e6
  real(8), dimension(vector_size) :: x,s,r
  integer :: i,j,k,count,Ndim
  real(8) :: V,cubevol,hitrate,sigma

  print '(a,T22,a)', 'Ndim', 'V'
  
  do Ndim=1,15


     call sgrnd(666)

     s(:)=0.0

     do i=1,Ndim
        do j=1,vector_size
           x(j)=2*grnd()-1
           s(j)=s(j)+x(j)**2
        end do
     end do
     do i=1,vector_size
        r(i)=sqrt(s(i))
     end do

     count=0

     do k=1,vector_size
        if (r(k).le.1) count=count+1
     end do

     cubevol=2**Ndim

     V=cubevol*count/vector_size

     hitrate=V/cubevol

     sigma=cubevol*sqrt(count-1.0*count**2/vector_size)/vector_size

     print '(i0,T8,f10.6,T21,a,x,f10.6)', Ndim, V, '+/-', sigma

  end do


contains


end program main
