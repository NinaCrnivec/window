subroutine read_inp_file_hr_sol(filename,hr3Darray)
  USE wd_globals
  implicit none

  character*128,intent(in) :: filename
  real,dimension(G_numX,G_numX,G_numHgt),intent(inout) :: hr3Darray
  
  ! Local variables:
  integer :: iStatus
  integer :: fileunit
  real :: rDum, hrVal
  integer :: indX, indY, indZ
  
   
  fileunit=11
  open(unit=fileunit,status='old',file=filename) 
  DO
    READ(fileunit,*,IOSTAT=iStatus) rDum, indX, indY, indZ, hrVal
    IF (iStatus > 0)  THEN
      write(6,*) "Error reading input file."
      close(fileunit)
      STOP
    ELSE IF (iStatus < 0) THEN
      write(6,*) "End of file successfully reached."
      EXIT
    ELSE
      ! Remark: in abs.spc files enumeration starts with 0;
      indX = indX+1
      indY = indY+1
      indZ = indZ+1-G_indZcld_min+1 !!!Potencialna napaka;
        
      if ((indZ .lt. 1) .or. (indZ .gt. G_numHgt)) then
         write(6,*) "Error! indZ = ", indZ
         close(fileunit)
         STOP
      end if
      
      hr3Darray(indX,indY,indZ) = hrVal        
    END IF
  END DO 
  close(fileunit)
  
end subroutine read_inp_file_hr_sol