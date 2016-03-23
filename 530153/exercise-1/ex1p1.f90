!
! Random Walk in 3 dimensions
!
! Need 3 command line arguments:
! * N: number of steps
! * nwalks: number of random walks to be simulated
! * l: step length
!
! There is one optional argument:
! * seed: Mersenne twister seed
!
! Written by Caike Crepaldi.
! Based on the FORTRAN routine randomwalk1d
!
program randomwalk3d
  use mtmod
  implicit none
  integer, parameter            :: dp=kind(1.d0)
  integer                       :: N, nwalks, iwalk, istep, seed
  real(dp)                      :: x, y, z, r2, l
  real(dp)                      :: r2sum, dir
  character 			:: buf*80
  integer       		:: narg

  narg=command_argument_count()

  print *,'randomwalk3d: 3-dimensional Random Walk'

  ! Command line parsing
  if (narg < 3) then                                        
     print *,'Usage: randomwalk3d N nwalks l [seed]'
     print *,'The argument [ARGUMENT] is optional.'
     STOP                                                          
  endif

  call getarg(1,buf); read (unit=buf,fmt=*) N ! number of steps/ iterations
  call getarg(2,buf); read (unit=buf,fmt=*) nwalks ! number of walks
  call getarg(3,buf); read (unit=buf,fmt=*) l ! grid/ step length
  if (narg>3) then
     call getarg(4,buf); read (unit=buf,fmt=*) seed
  else
     seed=8540585
  end if

  ! Random walk
  r2sum = 0.0
  call sgrnd(seed)
  
  print *,'Doing random walk to',N,'steps',nwalks,' times'

  r2sum=0.0

  WALK_LOOP: do iwalk=1, nwalks

     ! New walk                                                                
     x = 0.0
     y = 0.0
     z = 0.0
     
     STEP_LOOP: do istep = 1,N ! LOOP OVER STEPS IN WALK iwalk

        dir = grnd() ! direction

        if ((dir/0.1667).le.1) then
           x=x+l
        else if ((dir/0.1667).le.2) then
           x=x-l
        else if ((dir/0.1667).le.3) then
           y=y+l
        else if ((dir/0.1667).le.4) then
           y=y-l
        else if ((dir/0.1667).le.5) then
           z=z+l
        else if ((dir/0.1667).le.6) then
           z=z-l
        end if

     end do STEP_LOOP


     r2=x*x+y*y+z*z

     r2sum=r2sum+r2

  end do WALK_LOOP

  print *,'<R^2(N)>',r2sum/nwalks, 'l', l, 'N', N

  ! Einstein relation (3-dimensional case)
  ! <R^2(t)>=<x^2+y^2+z^2>=6Dt
  
  print *, 'D',(r2sum/nwalks)/(6.d0*N)

end program randomwalk3d

