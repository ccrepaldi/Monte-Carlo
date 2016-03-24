!
! Random Walk in 2 dimensions
!
! Need 4 command line arguments:
! * N: number of steps
! * nwalks: number of random walks to be simulated
! * l1: step length 1
! * l2: step length 2
!
! Note that both step lengths are equally probable
!
! There is one optional argument:
! * seed: Mersenne twister seed
!
! Written by Caike Crepaldi.
! Based on the FORTRAN routine randomwalk1d
!
program randomwalk2d
  use mtmod
  implicit none
  integer, parameter            :: dp=kind(1.d0)
  real(dp), parameter           :: pi=3.1415926535897932
  integer                       :: N, nwalks, iwalk, istep, seed
  real(dp)                      :: x, y, r2, l, l1, l2
  real(dp)                      :: r2sum, dir, len
  character 			:: buf*80
  integer       		:: narg

  narg=command_argument_count()

  print *,'randomwalk2d: 2-dimensional Random Walk'

  ! Command line parsing
  if (narg < 4) then                                        
     print *,'Usage: randomwalk3d N nwalks l1 l2 [seed]'
     print *,'Both step lengths, l1 and l2, are equally probable.'
     print *,'The argument [ARGUMENT] is optional.'
     STOP                                                          
  endif

  call getarg(1,buf); read (unit=buf,fmt=*) N ! number of steps/ iterations
  call getarg(2,buf); read (unit=buf,fmt=*) nwalks ! number of walks
  call getarg(3,buf); read (unit=buf,fmt=*) l1 ! grid/ step length 1
  call getarg(4,buf); read (unit=buf,fmt=*) l2 ! grid/ step length 2
  if (narg>4) then
     call getarg(5,buf); read (unit=buf,fmt=*) seed
  else
     seed=8540585
  end if

  ! Random walk
  r2sum = 0.0
  call sgrnd(seed)
  
  print *,'Doing random walk to',N,'steps',nwalks,' times'

  WALK_LOOP: do iwalk=1, nwalks

     ! New walk                                                                
     x = 0.0
     y = 0.0
     
     STEP_LOOP: do istep = 1,N ! LOOP OVER STEPS IN WALK iwalk

        dir = 2*pi*grnd() ! direction

        len = grnd() ! step length

        if (len.le.0.5) then
           l=l1
        else
           l=l2
        end if

        x = x+l*cos(dir)
        y = y+l*sin(dir)

     end do STEP_LOOP


     r2=x*x+y*y

     r2sum=r2sum+r2

  end do WALK_LOOP

  print *,'<R^2(N)>',r2sum/nwalks, 'N', N
  print *, 'l1', l1, 'l2', l2

  ! Einstein relation (2-dimensional case)
  ! <R^2(t)>=<x^2+y^2>=4Dt
  
  print *, 'D',(r2sum/nwalks)/(4.d0*N)

end program randomwalk2d

