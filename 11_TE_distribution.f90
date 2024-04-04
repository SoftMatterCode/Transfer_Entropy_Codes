!***************************************************************
!
!   Distribution of TE 
!
!***************************************************************
  program mbvel
  implicit none

   !****************************Variable declaration*************
   integer , parameter :: numberofbin = 500  
   integer:: pnum,initial_file,final_file                                    !pnum is number of particle 
   integer :: i ,k,lastfilenumber,ithbin ,firstfilenumber                           
   real*8:: maxTE, minTE, binsize, probabilitysum,sumv=0.0 ,sqrv=0.0,var=0.0,mean=0.0
   real*8, allocatable,dimension(:):: prob
   real*8, allocatable,dimension(:)::x,y,vx,vy,TE
   character(len=1024)::filein,format1   
   
   open(unit=1,file="Position_and_Exponential_TE.dat",form="formatted", action='read')
   open(unit=2,file="files_selected.dat",form="formatted", action='read')
   read(unit=2,fmt=*)initial_file,final_file
   open(unit=3,file="files_for_tail_area.dat",form="formatted", action='write')
   initial_file = initial_file + 3 !(3 becoz , initial 3 files are not created in JIDT)
   initial_file = initial_file + 1 ! (coz it was greater than so we use one more) 
   pnum=1000
   probabilitysum = 0.0d0
   ithbin = 0
   allocate(x(1:pnum),y(1:pnum),vx(1:pnum),vy(1:pnum),TE(1:pnum))
   allocate(prob(1:numberofbin))
   write(unit=3,fmt=*)initial_file,final_file
         
  !``````````````````````````````````````````````````````````````````````````````    
   do k = initial_file,final_file
           
              write(*,*)k
              
              read(unit=1,fmt=*)
              read(unit=1,fmt=*)
              do i=1,pnum         
                 read(unit=1,fmt=*)x(i),y(i),vx(i),vy(i),TE(i)
              end do                                                     
	
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
       prob = 0.0d0
       maxTE= maxval(TE) 
       minTE = minval(TE)                           !calculating the size of each bin
       binsize = ((maxTE - minTE)/numberofbin)         
       write(*,*)"maxTE=",maxTE,"minTE=",minTE
       probabilitysum = 0.0d0
       ithbin = 0
       
       do i=1,pnum       
         ithbin = anint((TE(i)-minTE)/binsize)      !distributing particles in           
         prob(ithbin) =  1 + prob(ithbin)               !different bins
       end do

       !*********************Normalizing the TE *****************       
       
       do ithbin = 1, numberofbin
         probabilitysum = probabilitysum + prob(ithbin)
       end do 
        
       probabilitysum = probabilitysum*binsize
       
         format1="(a6,'.',i0.1)"                             !format1="(a4,'.',i0.4)"
	 write(filein,format1)'TEdist',k
	 open(unit=5,file=trim(filein))
	  
       do ithbin = 1 , numberofbin
         write(unit=5,fmt=*) (minTE + binsize*ithbin), (prob(ithbin))/probabilitysum 
       end do
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
    end do
    
 
 end program mbvel

