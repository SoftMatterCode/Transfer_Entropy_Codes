!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!                                                                                     !
!                 PROGRAM TO DETERMINE NUMBER OF CONFIGURATIONS                       ! 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!  This program reads the output file of the main program and tells us the number of  !
!  configuration files. This program requires program_parameters.dat, output file     !
!                                                                                     !
!                          modified :: 14th March 2022                                !
!                                                                                     !
! Mohammad Samsuzzaman                                                                !
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
  
        program readfile
	    implicit none
           !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	    real*8 :: b,dt,F,boxlen
	    integer :: nlines,io,pnum,nfiles,time,interval
            character(len=30)::aa,bb,cc,dd,ee,ff,gg
	   !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	    open(unit=1, file = 'local_transfer_entropy_raw.txt', action='read', position='rewind',form="formatted")		
	    open(unit=4,file="TE_nfiles.dat")
           !~~~~~~~~~~~~~~~~~~~~~ Values of the variables ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	    nlines = 0
	   !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
           do
	      read(unit=1,fmt=*,iostat=io)  !READ(1,*,iostat=io) use this for formatted type
	      if(io/=0) exit
	         nlines = nlines + 1
	   end do

	   close(unit=1)

           write(unit=4,fmt=*)nlines
	  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   end program readfile
