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
	   integer::pnum,nfiles,i,j,time,interval,nfiles_expected,equilibrium
	   integer :: initial_file,final_file
	   real*8, allocatable,dimension(:)::xold,yold,vxold,vyold,theta,Fx	   
	   !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~		   
	   open(unit=1, file = 'output',form="unformatted", action='read')
	   open(unit=2,file="nfiles.dat",form="formatted", action='read')
	   open(unit=4,file="positionsx.txt")
	   open(unit=5,file="positionsy.txt")
	   open(unit=8,file="headings.txt")
	   open(unit=9,file="files_selected.dat",form="formatted", action='write')
           !~~~~~~~~~~~~~~~~~~~~~ Values of the variables ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
           read(unit=2,fmt=*)nfiles        ! Total number of configurations
           pnum=1000
           allocate(xold(1:pnum),yold(1:pnum),vxold(1:pnum),vyold(1:pnum))
           allocate(theta(1:pnum),Fx(1:pnum))
           z=1.0d0
           pi=4.0d0*atan(1.0d0)
           !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            
!           equilibrium = 3987
           initial_file = 10
           final_file = 310
           write(unit=9,fmt=*)initial_file,final_file       
           
            do i=1,nfiles           
                         		        
	       do j=1,pnum
		  read(unit=1)xold(j),yold(j),vxold(j),vyold(j)
		  theta(j)= datan(vyold(j)/vxold(j))
		  if(vxold(j).lt. 0.0d0)theta(j)=theta(j)+pi
		  theta(j) = (theta(j)*180.0d0)/pi 
	       end do
	       
	       if((i > initial_file) .and.( i .le. final_file))then

	         write(*,*)i
	         write(unit=4,fmt=*)(xold(j),j=1,pnum)
	         write(unit=5,fmt=*)(yold(j),j=1,pnum)
	         write(unit=8,fmt=*)(theta(j),j=1,pnum)
!	         equilibrium = equilibrium + 100
               end if 
               
            end do 
             
          !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
           end program readfile
