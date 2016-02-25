program main
  use mtmod
  implicit none
  integer, parameter :: dp=kind(1.d0), Nf=11, Nc=100, Nd=1000, Nh=10000
  character(len=80) :: filename
  integer :: i,j,k,ios
  real(dp), dimension(Nf) :: x,y
  real(dp) :: xm_true,xmean,dxm,u,Fk(Nf),means(Nd),hj(Nh),Hk(Nh),xl,xu
  real(dp) :: stdv,stdm
  integer :: Dk(Nf)

  ! Read the data ****************************************

  filename='poisson.txt'

  !Nf=11 ! Number of points in the distribution (declared as a parameter)

  open(unit=1,file=filename,iostat=ios,status='old')
  if (ios/=0) then
     print '(a,a)','*** Error in opening file ',trim(filename)
     stop
  end if
  
  do i=1,Nf
     read(1,*,iostat=ios) x(i),y(i)
     if (ios<0) exit
  end do
  close(1)

  !do i=1,Nf
  !   print *, x(i), y(i)
  !end do

  ! *******************************************************
  
  xm_true=0.d0
  do i=1,Nf
     xm_true=xm_true+x(i)*y(i)
  end do
  xm_true=xm_true/sum(y) ! get the true mean of the distribution

  stdv=0.d0

  do i=1,Nf
     stdv=stdv+nint(100*y(i))*(xm_true-x(i))**2
  end do
  stdv=stdv/(nint(100*sum(y))-1)
  stdv=sqrt(stdv)

  stdm=stdv/sqrt(real(nint(100*sum(y))))

  !print *, xm_true

  !Nc=100 ! number of counts in 1 distribution (declared as a parameter)

  !Nd=5 ! number of distributions (declared as a parameter)

  !y(:)=y(:)/sum(y) ! normalizes the distribution ! Not needed

  do k=1,Nf

     Fk(k)=sum(y(1:k))  ! get the normalized cumulative distribution

  end do

  !do i=1,Nf
  !   print *, Fk(i)
  !end do

  dxm=0.001d0
  hj(:)=0.d0
  do i=1,Nd
     Dk(:)=0.d0
     do j=1,Nc
        u=grnd()
        do k=1,Nf
           if((u.le.Fk(k)).and.(u.gt.Fk(k-1))) then
              Dk(k)=Dk(k)+1 ! generate 1 count
           end if
        end do
     end do
     !do k=1,Nf
     !   if (i==3) print *, Dk(k) ! checking vector Dk (OK)
     !end do
     xmean=0.d0
     do k=1,Nf
        xmean=xmean+Dk(k)*x(k)
     end do
     xmean=xmean/Nc
     means(i)=xmean
     !print *, sum(Dk), Nc, xmean, xm_true ! checking values
     do j=1,Nh
        if (j==int(xmean/dxm)) hj(j)=hj(j)+1 ! doesnt make any sense
     end do
  end do
  !print *, means
  !print *, sum(hj), Nd ! should be equal, OK
  !do k=1,Nh
  !   print *, hj(k)
  !end do
  hj(:)=hj(:)/sum(hj) ! normalizes the distribution
  do k=1,Nh
     Hk(k)=sum(hj(1:k))  ! get the normalized cumulative distribution
  end do
  !do k=1,Nh
  !   print *, Hk(k)
  !end do

  do k=1,Nh
     if ((0.16.le.Hk(k)).and.(0.16.gt.Hk(k-1))) xl=dxm*k
  end do

  do k=1,Nh
     if ((0.84.le.Hk(k)).and.(0.84.gt.Hk(k-1))) xu=dxm*k
  end do

  !print *, xl, xu !OK

  print *, trim("True mean of the distribution: "), xm_true

  print *, trim("Lower bound uncertainty: "), abs(xm_true-xl)
  print *, trim("Upper bound uncertainty: "), abs(xm_true-xu)

  print *, trim("Gaussian distr., Std. deviation of the mean: "), stdm


end program main
