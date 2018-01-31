

PROGRAM window

  USE wd_globals

  implicit none
 
  integer,dimension(:),allocatable :: time_array
  integer :: numTIME
  character*5 :: charTIME
  integer :: it
  
  integer,dimension(:),allocatable :: sza_array
  integer :: numSZA
  character*2 :: charSZA
  integer :: is
  
  character(len=:), allocatable :: fp_out
  character*128 :: fn_out
  integer :: fu_out
  
  real :: cpu_time_start
  real :: cpu_time_finish
 
  call cpu_time(cpu_time_start)
 
  !fp_out = "/home/nina/W2W/WINDOW_tAll/output/" !auto-allocated !NILEN
  fp_out = "/project/meteo/work/Nina.Crnivec/WINDOW_tAll/output/" !auto-allocated !MERLIN
  !if(.not. allocated(fp_out)) allocate(character(len=len(fp_out)) :: fp_out) 
      
  if(G_Flag_sol .eq. 1) then !solar
    write(6,*) "Processing SOLAR"
    sza_array = (/ 0, 30, 60 /) 
  else !thermal, set sza to dummy value
    write(6,*) "Processing THERMAL"
    sza_array = (/ 99 /)
  end if 
  numSZA = size(sza_array)
  if(.not. allocated(sza_array)) allocate(sza_array(numSZA))
  
  time_array = (/ 5100, 6600, 8400, 10500, 11400, 12300, 13500, 15000, 17400, 25800 /)
  !time_array = (/ 5100, 6600, 8400, 10500, 11400, 12300, 13500, 15000, 17400 /)
  !time_array = (/ 7200 /) !Testing
  !time_array = (/ 5100 /) !Testing
  numTIME = size(time_array)
  if(.not. allocated(time_array)) allocate(time_array(numTIME))
  
  do is=1,numSZA
    write(charSZA,"(i2.2)") sza_array(is)
    write(6,*) "is, sza_array(is), charSZA = ", is, sza_array(is), charSZA
  
    if (G_Flag_sol .eq. 1) then 
      fn_out="data_windows_2800m_sza"//charSZA//".out"
    else
      fn_out="data_windows_2800m_thermal.out"
    end if
    
    fu_out = 22
    open(unit=fu_out,status='unknown',file=fp_out//fn_out)
  
    do it=1,numTIME
      write(charTIME,"(i5.5)") time_array(it)
      write(6,*) "it, time_array(it), charTIME = ", it, time_array(it), charTIME
      call process_time(charSZA, charTIME, fu_out)
    end do 
    close(fu_out) 
  end do 
  
  if(allocated(time_array)) deallocate(time_array)
  if(allocated(sza_array)) deallocate(sza_array)
  if(allocated(fp_out)) deallocate(fp_out)
 
  write(6,*)
  write(6,*) "The END"
  write(6,*)
  
  call cpu_time(cpu_time_finish)
  print '("Time: start = ",f6.3," seconds.")', cpu_time_start
  print '("Time: finish = ",f6.3," seconds.")', cpu_time_finish
  print '("Time: finish - start = ",f6.3," seconds.")', cpu_time_finish - cpu_time_start
  
END PROGRAM window
