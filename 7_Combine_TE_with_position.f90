!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!                                                                                     !
!                          OUTPUT FILE EXTRACTION PROGRAM                             ! 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!  This program reads the output file of the main program and converts it into user   !
!  defined format, so that it can be read in any visualization software such as ovito !
!                                                                                     !
!                          modified :: 21st August 2020                               !
!                                                                                     !
! Mohammad Samsuzzaman                                                                !
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
              
	program readfile
	implicit none
	!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	   real :: z
           real*8 :: b,dt,F,boxlen,pi
           character(len=30)::aa,bb,cc,dd,ee,ff,gg
	   integer::pnum,nfiles,i,j,m,time,interval,nfiles_expected,equilibrium,k1,j1
	   real*8, allocatable,dimension(:)::xold,yold,vxold,vyold,theta,Fx,TE_individual
	   integer :: initial_file,final_file	   
	   !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~		   
	   open(unit=1, file = 'output',form="unformatted", action='read')
	   open(unit=2,file="nfiles.dat",form="formatted", action='read')
	   open(unit=3,file="Particle_number_and_TE.dat",form="formatted", action='read')
	   open(unit=4,file="Position_and_TE.dat",form="formatted", action='write')
	   open(unit=5,file="files_selected.dat",form="formatted", action='read')
	   
           !~~~~~~~~~~~~~~~~~~~~~ Values of the variables ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
           read(unit=2,fmt=*)nfiles        ! Total number of configurations
           read(unit=5,fmt=*)initial_file,final_file
           pnum=1000
           allocate(xold(1:pnum),yold(1:pnum),vxold(1:pnum),vyold(1:pnum))
           allocate(theta(1:pnum),Fx(1:pnum))
           allocate(TE_individual(1:pnum))
           z=1.0d0
           pi=4.0d0*atan(1.0d0)
           !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            
           initial_file = initial_file + 3 !(3 becoz , initial 3 files are not created in JIDT)      
            
           
            do i=1,nfiles          
                         		        
	       do j=1,pnum
		  read(unit=1)xold(j),yold(j),vxold(j),vyold(j)		  
	       end do
	       
	      if((i > initial_file) .and.( i .le. final_file))then

	         write(*,*)i
	         
	         write(unit=4,fmt=*)pnum
	         write(unit=4,fmt=*)
	         
	         do m=1,pnum
	          read(unit=3,fmt=*)k1,j1,TE_individual(m)
	          write(unit=4,fmt=*)xold(m),yold(m),vxold(m),vyold(m),TE_individual(m)!,k1,j1
                 end do

               end if 
               
            end do 
             
          !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
           end program readfile
