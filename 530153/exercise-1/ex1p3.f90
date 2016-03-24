!
! Drunken Sailor
!
! Need 3 command line arguments:
! * N: number of steps
! * nwalks: number of random walks to be simulated
! * wind: direction of the wind
!
!
! There is one optional argument:
! * seed: Mersenne twister seed
!
! Written by Caike Crepaldi.
! Based on the FORTRAN routine randomwalk1d
!
program drunkensailor
  use mtmod
  implicit none
  integer, parameter            :: dp=kind(1.d0)
  !real(dp), parameter           :: pi=3.1415926535897932
  integer                       :: N, nwalks, iwalk, istep, seed, failure
  real                          :: x, y, l
  real                          :: dir, median
  real, allocatable             :: t(:)
  character 			:: buf*80
  integer       		:: narg, wind

  narg=command_argument_count()

  print *,'drunkensailor: 2-dimensional Random Walk'

  ! Command line parsing
  if (narg < 3) then                                        
     print *,'Usage: drunkensailor N nwalks wind [seed]'
     print *,'The argument [ARGUMENT] is optional.'
     print *, 'Note: wind can be 0 (no wind), 1 (west wind) or 2 (east wind).'
     STOP                                                          
  endif

  call getarg(1,buf); read (unit=buf,fmt=*) N ! number of steps/ iterations
  call getarg(2,buf); read (unit=buf,fmt=*) nwalks ! number of walks
  call getarg(3,buf); read (unit=buf,fmt=*) wind ! wind direction
  if (narg>3) then
     call getarg(4,buf); read (unit=buf,fmt=*) seed
  else
     seed=8540585
  end if

  if (wind.lt.0.or.wind.gt.2) then
     print *, 'Invalid wind value:'
     print *, 'wind can be 0 (no wind), 1 (west wind) or 2 (east wind).'
     STOP
  end if

  allocate(t(nwalks))
  t(:)=0.0
  failure=0

  ! Random walk
  l=1.d0
  call sgrnd(seed)
  
  print *,'Doing random walk to',N,'steps',nwalks,' times'

  WALK_LOOP: do iwalk=1, nwalks

     ! New walk                                                                
     x = 0.0
     y = 0.0
     
     STEP_LOOP: do istep = 1,N ! LOOP OVER STEPS IN WALK iwalk

        dir = grnd() ! direction

        if (mod(istep,100).eq.0) then
           
           select case (wind)
              
           case (0) ! no wind
              if ((dir/0.25).le.1) then
                 x=x+l
              else if ((dir/0.25).le.2) then
                 x=x-l
              else if ((dir/0.25).le.3) then
                 y=y+l
              else if ((dir/0.25).le.4) then
                 y=y-l
              end if
           case (1) ! west wind
              x=x+1     ! gets blown to the right
           case (2) ! east wind
              x=x-1     ! gets blown to the left
              
           end select
           
        else

           if ((dir/0.25).le.1) then
              x=x+l
           else if ((dir/0.25).le.2) then
              x=x-l
           else if ((dir/0.25).le.3) then
              y=y+l
           else if ((dir/0.25).le.4) then
              y=y-l
           end if

        end if

        if (x.ge.15*l) then
           t(iwalk)=istep
           EXIT
        end if

     end do STEP_LOOP

     if (x<15) then
        failure=failure+1
     end if

     !write(1,*) t(iwalk)
     
  end do WALK_LOOP

  print *, 'Failures', failure

  call sort(t, nwalks)
  if (mod(nwalks,2).eq.0) then
     median=(t(nwalks/2)+t((nwalks/2)+1))/2
  else
     median=t(floor(nwalks/2.0)+1)
  end if

  print *, '<t>', sum(t)/(nwalks-failure), 'min'
  print *, 'median(t)', median, 'min'

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

end program drunkensailor

