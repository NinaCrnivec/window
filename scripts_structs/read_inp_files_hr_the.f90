subroutine read_inp_files_hr_the(charTIME, ffname, hr3Darray)
  USE wd_globals
  implicit none

  character*5,intent(in) :: charTIME
  character*5,intent(in) :: ffname !mcipa, mysti
  real,dimension(G_numX,G_numX,G_numHgt),intent(inout) :: hr3Darray
  
  !Recall globals: G_numHgt, G_indZcld_min, G_indZcld_max;
  
  ! Local variables:
  integer :: iz, numZdomain
  real,dimension(160) :: hgt_array
  real :: dz
  real :: hgt
  character*5 :: charHGT
  
  character(len=:),allocatable :: fpath
  character*128 :: filename
  integer :: iStatus, fileunit
  real :: rDum1, rDum2, hrVal
  integer :: indX, indY
  
  fpath = "/home/nina/W2W/daLibRSVN/01e_dataAnalysis/dataabs_thermal_RENAMED/" !auto-allocated
  
  !if (.not. allocated(fpath)) allocate(character(len=len(fpath)) :: fpath)
  
  numZdomain = 160
  dz = 0.025
   
  hgt_array(1) = 0.0
  do iz=2,numZdomain
    hgt_array(iz) = hgt_array(iz-1) + dz
  end do
  
  ! Calculate initial height of the cloud layer:
  hgt = hgt_array(G_indZcld_min)
  !Example for t=07200: G_indZcld_min=36, hgt_array(36)=0.8750;
  write(6,*) "initial hgt = ", hgt
  
  do iz=1,G_numHgt
    !write(6,*) "iz, G_numHgt = ", iz, G_numHgt
    write(charHGT, '(f5.3)'), hgt
    !write(6,*) "hgt, charHGT = ", hgt, charHGT
    filename = ffname//'_t'//charTIME//'_out_'//charHGT//'.abs.spc'
    fileunit=12
    open(unit=fileunit,status='old',file=fpath//filename)
    DO
      READ(fileunit,*,IOSTAT=iStatus) rDum1, indX, indY, rDum2, hrVal 
      IF (iStatus > 0)  THEN
	write(6,*) "Error reading input file. iStatus = ", iStatus
	write(6,*) "Filename = ", filename
	close(fileunit)
	STOP
      ELSE IF (iStatus < 0) THEN
	!write(6,*) "End of file successfully reached."
	EXIT
      ELSE
	! Remark: in abs.spc files enumeration starts with 0;
	indX = indX+1
	indY = indY+1
	hr3Darray(indX,indY,iz) = hrVal        
      END IF
    END DO 
    close(fileunit)

    hgt = hgt + dz
  end do
  
  if(allocated(fpath)) deallocate(fpath)
  
end subroutine read_inp_files_hr_the
