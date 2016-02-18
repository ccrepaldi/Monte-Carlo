program chi2_rng
  use mtmod
  use rng
  implicit none
  integer, parameter :: vector_size=1000
  integer, parameter :: points=1e6
  integer, parameter :: Nbins=60
  integer, parameter :: dof=Nbins-1 ! degrees of freedom
  real, parameter :: E=1.0*points/Nbins
  real :: chi2(vector_size)
  integer :: i,j,qbin
  integer, dimension(Nbins) :: bin
  integer(ik) :: seed
  real :: s, mean, median, randn

  seed=666


  new_seed: do j=1,vector_size

     seed=seed+(5*j) ! lcg or pmg
     call assignseed(seed) ! lcg or pmg
     !call sgrnd(666+5*j) ! mtg

     s=0.0
     bin(:)=0.0

      do i=1,points
        !randn=1.0*lcg()/12386880 ! lcg
        randn=1.0*pmg()/2147483647 ! pmg
        !randn=grnd() ! mtg 
        qbin = INT( Nbins * (randn) / (1.0) + 1.D0 )
        bin(qbin) = bin(qbin) + 1
        if (j==42) write(2,*) randn
     end do

     do i=1,Nbins
        s=s+(bin(i)-E)**2
     end do
     
     chi2(j)=s/E
     write(1,*) chi2(j)

  end do new_seed

  write(6,"('+',50('-'),'+')")

  write(6,"('|',T5,a,T52,'|')") 'NUMBER OF RANDOM NUM:'
  print "('|',T30,i0,T52,'|')", points
  write(6,"('|',T5,a,T52,'|')") 'NUMBER OF BINS:'
  print "('|',T30,i0,T52,'|')", Nbins
  write(6,"('|',T5,a,T52,'|')") 'DEGREES OF FREEDOM:'
  print "('|',T30,i0,T52,'|')", dof
  write(6,"('|',T5,a,T52,'|')") 'EXPECTED VALUE:'
  print "('|',T30,f10.3,T52,'|')", E
  write(6,"('|',T5,a,T52,'|')") 'CHI-SQUARED MEAN:'
  
  mean=sum(chi2)/vector_size
  
  print "('|',T30,f10.3,T52,'|')", mean
  write(6,"('|',T5,a,T52,'|')") 'CHI-SQUARED MEDIAN:'
  
  call sort(chi2, vector_size)
  median=(chi2(500)+chi2(501))/2
  
  print "('|',T30,f10.3,T52,'|')", median
  write(6,"('|',T5,a,T52,'|')") 'LOWER ONE-SIDED:'
  print "('|',T30,'<',X,f10.3,T52,'|')", chi2(50)
  write(6,"('|',T5,a,T52,'|')") 'UPPER ONE-SIDED:'
  print "('|',T30,'>',X,f10.3,T52,'|')", chi2(950)
  
  write(6,"('+',50('-'),'+')")

contains

  integer function findmin(x,start,size)
    implicit none
    integer, intent(in) :: start,size ! END=SIZE
    real, intent(in) :: x(size)
    real :: minimum
    integer :: location
    integer :: i

    minimum=x(start)
    location=start

    do i=start+1,size
       if ( x(i).lt.minimum ) then
          minimum=x(i)
          location=i
       end if
    end do
    findmin=location
  end function findmin

  subroutine swap(a,b)
    implicit none
    real, intent(inout) :: a,b
    real :: temp

    temp=a
    a=b
    b=temp

  end subroutine swap

  subroutine sort(x,size)
    implicit none
    integer, intent(in) :: size
    real, intent(inout) :: x(size)
    integer :: location
    integer :: i

    do i=1,size-1
       location=findmin(x,i,size)
       call swap(x(i),x(location))
    end do

  end subroutine sort

  
  
end program chi2_rng
