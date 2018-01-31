subroutine process_time(charSZA, charTIME, fu_out)

  USE wd_globals
  
  implicit none
  
  INTERFACE
  
    subroutine read_inp_file_hr_sol(filename,hr3Darray)
      USE wd_globals
      implicit none
      character*128,intent(in) :: filename
      real,dimension(G_numX,G_numX,G_numHgt),intent(inout) :: hr3Darray
    end subroutine read_inp_file_hr_sol
    
    subroutine read_inp_files_hr_the(charTIME, ffname, hr3Darray)
      USE wd_globals
      implicit none
      character*5,intent(in) :: charTIME
      character*5,intent(in) :: ffname
      real,dimension(G_numX,G_numX,G_numHgt),intent(inout) :: hr3Darray
    end subroutine read_inp_files_hr_the
  
    subroutine process_wd (wd_number, xc_sta, yc_sta, xc_end, yc_end, lwc3D, hr_mcipa, hr_mysti, fileunit_out) 
      USE wd_globals 
      implicit none
      integer,intent(in) :: wd_number, xc_sta, yc_sta, xc_end, yc_end  
      real,dimension(G_numX, G_numX, G_numHgt),intent(in) :: lwc3D, hr_mcipa, hr_mysti 
      integer,intent(in) :: fileunit_out 
    end subroutine process_wd
    
  END INTERFACE
  
  character*2,intent(in) :: charSZA
  character*5,intent(in) :: charTIME
  integer,intent(in) :: fu_out
  
  ! Local variables:
  integer,parameter :: numX_wd = 112 ! 2800 m
  
  integer :: ix_ini ! initial;
  integer :: ix_fin ! final; ix_fin = G_numX-numX_wd+1
  integer :: ix_stp ! step;

  integer :: wd_number
  integer :: xc_sta, yc_sta
  integer :: xc_end, yc_end
  
  integer :: ix, iy, iz
  
  character(len=:),allocatable :: fp_inp_wc
  character(len=:),allocatable :: fp_inp_mcipa ! for solar only
  character(len=:),allocatable :: fp_inp_mysti ! for solar only
  
  character*128 :: fn_inp_wc
  character*128 :: fn_inp_mcipa
  character*128 :: fn_inp_mysti
  
  integer :: fu_inp_wc
  
  integer :: iStatus
  integer :: indXcld, indYcld, indZcld	
  real :: lwcVal, rDum
  real,dimension(:,:,:),allocatable :: lwc3D
  
  real,dimension(:,:,:),allocatable :: hr_mcipa
  real,dimension(:,:,:),allocatable :: hr_mysti
  
  
  !fp_inp_wc = "/home/nina/W2W/daLibRSVN/05_cc25m_varTimes/wcFILES_3D_RENAMED/" !auto-allocated !NILEN
  fp_inp_wc = "/project/meteo/work/Nina.Crnivec/05_cc25m_varTimes/wcFILES_3D_RENAMED/" !auto-allocated !MERLIN
  !if (.not. allocated(fp_inp_wc)) allocate(character(len=len(fp_inp_wc)) :: fp_inp_wc)
 
  
  if (G_Flag_sol .eq. 1) then
    !fp_inp_mcipa = "/home/nina/W2W/daLibRSVN/01c_ccSolar_afglusInt_N5_tAll/dataabs_mcipa/" !1D ! NILEN
    !fp_inp_mysti = "/home/nina/W2W/daLibRSVN/01c_ccSolar_afglusInt_N5_tAll/dataabs_mysti/" !3D ! NILEN
    
    fp_inp_mcipa = "/project/meteo/work/Nina.Crnivec/01c2_ccSolar_afglusInt_tAll_HighRes/dataabs_mcipa/" !1D ! MERLIN
    fp_inp_mysti = "/project/meteo/work/Nina.Crnivec/01c2_ccSolar_afglusInt_tAll_HighRes/dataabs_mysti/" !3D ! MERLIN
    
    !fn_inp_mcipa = "mcipa_t"//charTIME//"_navg001_sza"//charSZA//".out.abs.spc" !1D ! NILEN
    !fn_inp_mysti = "mysti_t"//charTIME//"_navg001_sza"//charSZA//".out.abs.spc" !3D ! NILEN
    
    fn_inp_mcipa = "mcipa_navg001_t"//charTIME//"_sza"//charSZA//".out.abs.spc" !1D ! MERLIN
    fn_inp_mysti = "mysti_navg001_t"//charTIME//"_sza"//charSZA//".out.abs.spc" !3D ! MERLIN
  end if
  
  !if (.not. allocated(fp_inp_mcipa)) allocate(character(len=len(fp_inp_mcipa)) :: fp_inp_mcipa)
  !if (.not. allocated(fp_inp_mysti)) allocate(character(len=len(fp_inp_mysti)) :: fp_inp_mysti)
      
  G_indZcld_min = 10000000
  G_indZcld_max = -10000000
  
  fn_inp_wc = "wc3D_dx25m_time_"//charTIME//".dat"
  fu_inp_wc = 18
  
  open(unit=fu_inp_wc,status='old',file=fp_inp_wc//fn_inp_wc)
  read(fu_inp_wc,*) 
  read(fu_inp_wc,*) 
  DO
    READ(fu_inp_wc,*,IOSTAT=iStatus) indXcld, indYcld, indZcld, lwcVal, rDum
    IF (iStatus > 0)  THEN
      write(6,*) "Error reading the input file!"
      close(fu_inp_wc)
      STOP
    ELSE IF (iStatus < 0) THEN
      write(6,*) "End of input cloud file successfully reached."
      EXIT
    ELSE
      if(indZcld .lt. G_indZcld_min) G_indZcld_min = indZcld
      if(indZcld .gt. G_indZcld_max) G_indZcld_max = indZcld
    END IF
  END DO
  close(fu_inp_wc)

  write(6,*) "G_indZcld_min = ", G_indZcld_min
  write(6,*) "G_indZcld_max = ", G_indZcld_max
  
  G_numHgt = G_indZcld_max - G_indZcld_min + 1
  write(6,*) "G_numHgt = ", G_numHgt
  
  G_numTot_wd = numX_wd*numX_wd*G_numHgt 
  
  !Allocate arrays:
  if(.not. allocated(lwc3D)) allocate(lwc3D(G_numX, G_numX, G_numHgt))
  if(.not. allocated(hr_mcipa)) allocate(hr_mcipa(G_numX, G_numX, G_numHgt))
  if(.not. allocated(hr_mysti)) allocate(hr_mysti(G_numX, G_numX, G_numHgt))

  
  ! Set arrays to zero initially:
  do ix=1,G_numX
    do iy=1,G_numX
      do iz=1,G_numHgt
	lwc3D(ix,iy,iz) = 0.0
	hr_mcipa(ix,iy,iz) = 0.0
	hr_mysti(ix,iy,iz) = 0.0
      end do
    end do
  end do
  
  open(unit=fu_inp_wc,status='old',file=fp_inp_wc//fn_inp_wc)
  read(fu_inp_wc,*)
  read(fu_inp_wc,*) 
  DO
    READ(fu_inp_wc,*,IOSTAT=iStatus) indXcld, indYcld, indZcld, lwcVal, rDum
    IF (iStatus > 0)  THEN
      write(6,*) "Error reading the input file!"
      close(fu_inp_wc)
      STOP
    ELSE IF (iStatus < 0) THEN
      write(6,*) "End of input cloud file successfully reached."
      EXIT
    ELSE
      !indZcld = indZcld - 35 + 2 ! thermal
      !indZcld = indZcld - 35 ! solar
      !G_indZcld_min = 36;
      indZcld = indZcld - G_indZcld_min + 1
      lwc3D(indXcld, indYcld, indZcld) = lwcVal  
    END IF
  END DO
  close(fu_inp_wc)
  
  
  
  if (G_Flag_sol .eq. 1) then
    call read_inp_file_hr_sol(fp_inp_mcipa//fn_inp_mcipa, hr_mcipa) !HR 1D
    call read_inp_file_hr_sol(fp_inp_mysti//fn_inp_mysti, hr_mysti) !HR 3D
  else
    call read_inp_files_hr_the(charTIME, "mcipa", hr_mcipa) !HR 1D
    call read_inp_files_hr_the(charTIME, "mysti", hr_mysti) !HR 3D
  end if
  
  !hr_mcipa and hr_mysti = arrays of 1D and 3D heating rates, same dimensions solar / thermal;
  ! (Take the two additional levels out);
  
  ! Indices of cells, total: 1-256;	
  ! Windows:
  ! 1st window:    1->112; = 1-> 1+112-1
  ! 2nd window:    2->113; = 2-> 2+112-1
  ! 3th window:    3->114; = 3-> 3+112-1
  ! ...
  ! Last window: 145->256; = 145->numX_wd+144
  ! ...
  !xc_sta [1...145]
  !yc_sta [1...145]
  
  ix_ini = 1
  ix_fin = G_numX - numX_wd + 1 
  ix_stp = 32 ! 1, 2, 4, 8, 16, 32
  
  wd_number = 0
  
  DO xc_sta = ix_ini, ix_fin, ix_stp
    DO yc_sta = ix_ini, ix_fin, ix_stp
    
      !Calculate end coordinates of window:
      xc_end = xc_sta + numX_wd - 1
      yc_end = yc_sta + numX_wd - 1
        
      wd_number = wd_number + 1
      !write(6,*) "wd_number = ", wd_number
      
      !TODO: Prepare input files for twostream and twomaxrnd:
      !if (G_Flag_prp_inp .eq. 1) then
        !write(6,*) "Preparing input for twostream and twomaxrnd"
	!call prp_inp_two_window(wd_number, xc_sta, yc_sta, xc_end, yc_end, lwc3D) !todo
	!call prp_inp_tmr_window(wd_number, xc_sta, yc_sta, xc_end, yc_end, lwc3D) !todo
      !end if
      
      call process_wd(wd_number, xc_sta, yc_sta, xc_end, yc_end, lwc3D, hr_mcipa, hr_mysti, fu_out)
      
    END DO
  END DO
  
  if(allocated(lwc3D)) deallocate(lwc3D) 
  if(allocated(hr_mcipa)) deallocate(hr_mcipa)
  if(allocated(hr_mysti)) deallocate(hr_mysti)
  
  if(allocated(fp_inp_wc)) deallocate(fp_inp_wc)
  
  if(allocated(fp_inp_mcipa)) deallocate(fp_inp_mcipa)
  if(allocated(fp_inp_mysti)) deallocate(fp_inp_mysti)

  
end subroutine process_time
