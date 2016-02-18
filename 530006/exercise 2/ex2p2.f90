program correlation_test
  use rng
  use mtmod
  implicit none
  integer, parameter :: vector_size=1e5
  real, dimension(vector_size) :: Corr, randn
  real :: a,b,c,sa,sb,sc
  integer :: i, k

  call assignseed(42_ik)

  do i=1,vector_size
     !randn(i)=1.0*lcg()/12386880 ! normal lcg
     !randn(i)=1.0*pmg()/2147483647 ! pmg
     randn(i)=1.0*lcg_table()/12386880 ! lcg with the 32 elements table trick
  end do

  print *, 'Random numbers stored'

  do k=1,vector_size-1

     if (mod(k,1000)==0) print '(a,i0,a)', 'Calculating Corr(k=', k, ')'

     sa=0.0
     do i=1,(vector_size-k+1)
        sa=sa+randn(i+k)*randn(i)
     end do
     a=sa/(vector_size-k)

     sb=0.0
     do i=1,vector_size
        sb=sb+randn(i)
     end do
     b=sb/vector_size

     sc=0.0
     do i=1,vector_size
        sc=sc+randn(i)**2
     end do
     c=sc/vector_size

     Corr(k)=(a-b**2)/(c-b**2)

     write(1,*) Corr(k)

  end do

end program correlation_test
