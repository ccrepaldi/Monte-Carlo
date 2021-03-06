!
! Self-Avoiding Random Walk (SAW) in 3 dimensions
!
! Written by Caike Crepaldi.
! Based on the FORTRAN routine randomwalk1d
!
program saw3d
  use mtmod
  implicit none
  integer, parameter            :: N=50, nwalks=1000, ntries=10
  integer, parameter            :: size=2*N+1
  integer                       :: iwalk, istep, seed
  integer                       :: x,y,z,m,try,i
  real                          :: r2(nwalks), ni(ntries), r2mean(ntries)
  real                          :: r2sum, dir, w(nwalks), std, sigma, sqtotal
  integer                       :: space(size,size,size), itry
  !character 			:: buf*80
  !integer       		:: narg

  !narg=command_argument_count()
  
  print *,'saw3d: 3-dimensional Self-Avoiding Random Walk'

  print *,'Simulating a polymer with',N,'monomers',nwalks,' times'

  !if (narg>0) then
  !   call getarg(1,buf); read (unit=buf,fmt=*) seed
  !else
  seed=8540585
  !end if

  do itry=1,ntries

     ! Random walk
     r2sum = 0.0
     call sgrnd(seed+17*itry)

     WALK_LOOP: do iwalk=1, nwalks

        ! New walk                                                         
        x = 0
        y = 0
        z = 0
        w(iwalk)=1.0
        space(:,:,:)=0
        space(N+1,N+1,N+1)=1 ! origin of the space has the first monomer

        STEP_LOOP: do istep = 1,N ! LOOP OVER STEPS IN WALK iwalk

           m=6

           ! space(x+N+1,y+N+1,z+N+1) current position in space

           m=m-(space(x+N+2,y+N+1,z+N+1)+space(x+N,y+N+1,z+N+1)+&
                &space(x+N+1,y+N+2,z+N+1)+space(x+N+1,y+N,z+N+1)+&
                &space(x+N+1,y+N+1,z+N+2)+space(x+N+1,y+N+1,z+N))

           if (m==0) then

              w(iwalk)=0.0
              EXIT

           else if (m==5.or.m==6) then

              !w(iwalk)=w(iwalk)

              try=1

              do while (try==1)

                 dir = grnd() ! direction

                 if ((dir/0.1667).le.1) then
                    x=x+1
                 else if ((dir/0.1667).le.2) then
                    x=x-1
                 else if ((dir/0.1667).le.3) then
                    y=y+1
                 else if ((dir/0.1667).le.4) then
                    y=y-1
                 else if ((dir/0.1667).le.5) then
                    z=z+1
                 else if ((dir/0.1667).le.6) then
                    z=z-1
                 end if

                 try=space(x+N+1,y+N+1,z+N+1)

              end do

              space(x+N+1,y+N+1,z+N+1)=1

           else if (m.ge.1.and.m.lt.5) then

              w(iwalk)=w(iwalk)*(m/5.0)

              try=1

              do while (try==1)

                 dir = grnd() ! direction

                 if ((dir/0.1667).le.1) then
                    x=x+1
                 else if ((dir/0.1667).le.2) then
                    x=x-1
                 else if ((dir/0.1667).le.3) then
                    y=y+1
                 else if ((dir/0.1667).le.4) then
                    y=y-1
                 else if ((dir/0.1667).le.5) then
                    z=z+1
                 else if ((dir/0.1667).le.6) then
                    z=z-1
                 end if

                 try=space(x+N+1,y+N+1,z+N+1)

              end do

              space(x+N+1,y+N+1,z+N+1)=1

           end if

        end do STEP_LOOP

        r2(iwalk)=x*x+y*y+z*z

     end do WALK_LOOP

     do iwalk=1,nwalks

        r2sum=r2sum+r2(iwalk)*w(iwalk)

     end do

     r2mean(itry)=r2sum/sum(w)
     ni(itry)=log(r2sum/sum(w))/(2*log(1.0*N))

  end do

  sqtotal=0.0

  do i=1,ntries
     sqtotal = sqtotal + (ni(i) - sum(ni)/ntries) * (ni(i) - sum(ni)/ntries)
  end do
  std = sqrt(sqtotal/(ntries-1))
  sigma = std/sqrt(1.0*ntries)

  print *, '<R^2(N)>',sum(r2mean)/ntries, 'N', N
  print *, 'ni', sum(ni)/ntries, '+/-', sigma

end program saw3d

