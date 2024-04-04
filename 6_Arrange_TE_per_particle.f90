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

	   integer:: i,j,k,nfiles,max_time,min_time,pnum,count_pnum,tot_files
	   real*8, allocatable,dimension(:)::file_index,time_index,target_index,source_index,theta,TE
	   real*8, allocatable,dimension(:)::TE_individual	   
	   !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~		   
	   open(unit=1, file = 'local_transfer_entropy_raw.txt',form="formatted", action='read')
	   open(unit=2,file="TE_nfiles.dat",form="formatted", action='read')
	   open(unit=3,file="Particle_number_and_TE.dat",form="formatted", action='write')
	   open(unit=4,file="total_files_for_TE.dat",form="formatted", action='write')
           !~~~~~~~~~~~~~~~~~~~~~ Values of the variables ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
           read(unit=2,fmt=*)nfiles        ! Total number of configurations

            ! [file_index, time_index, target_index, source_index]

           allocate(file_index(1:nfiles),time_index(1:nfiles),target_index(1:nfiles),source_index(1:nfiles))
           allocate(theta(1:nfiles),TE(1:nfiles))

           pnum=1000
           allocate(TE_individual(1:pnum))
           !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~       
           
           do i=1,nfiles         
              read(unit=1,fmt=*)file_index(i),time_index(i),target_index(i),source_index(i),TE(i)
           end do 
           
           max_time = maxval(time_index)
           min_time = minval(time_index)
           
           tot_files = 0
           
           do k = min_time,max_time
             write(*,*)k
             tot_files = tot_files + 1
             
             do j = 1,pnum
               
               count_pnum = 0
             
               do i = 1,nfiles
                
                  if(time_index(i) == k) then
               
                    if(target_index(i) == j) then
                  
                      TE_individual(j) = TE_individual(j) + TE(i)
                  
                      count_pnum = count_pnum + 1
                  
                    end if
                 
                  end if 
               end do
               
               TE_individual(j) = TE_individual(j)/count_pnum
               write(unit=3,fmt=*) k,j,TE_individual(j)
               
             end do
!               write(unit=3,fmt=*)
           end do     
           
           write(unit=4,fmt=*) tot_files
             
            write(*,*)sum(TE)/nfiles 
          !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
           end program readfile
