subroutine process_wd(wd_number, xc_sta, yc_sta, xc_end, yc_end, lwc3D, hr_mcipa, hr_mysti, fileunit_out)

  USE wd_globals

  implicit none
  
  integer,intent(in) :: wd_number, xc_sta, yc_sta, xc_end, yc_end  
  real,dimension(G_numX, G_numX, G_numHgt),intent(in) :: lwc3D, hr_mcipa, hr_mysti
  integer,intent(in) :: fileunit_out
  
  ! Local variables:
  real :: HR1D_wd, HR1D_wd_cloud, HR1D_wd_clear ! window averages of 1D HR
  real :: HR3D_wd, HR3D_wd_cloud, HR3D_wd_clear ! window averages of 3D HR
  real :: avgLWP_wd, avgLWP_wd_cloud
  real :: totCC_wd
  
  ! LWP = integral LWC-ja po visini;
  ! LWP = sum(LWC(iz)*dz(iz));
  ! LWC units [g/m3]
  ! LWP units [g/m2]
  
  real :: dz ! in m
  real :: avgLWP_wd_inc
  integer :: count_totCC
  
  real :: dx
  real :: dx_wd
  real :: area_pix
  real :: area_wd
  
  integer :: countTot_cloud_wd, countTot_clear_wd
  integer :: countTot_wd_test
  
  integer :: ix,iy,iz
    
  dz = 25.0
  
  dx = 25.0 ! [m]
  dx_wd = 2800.0 ! [m]
      
  area_pix = dx*dx
  area_wd = dx_wd*dx_wd

  HR1D_wd = 0.0
  HR3D_wd = 0.0

  HR1D_wd_cloud = 0.0
  HR3D_wd_cloud = 0.0

  HR1D_wd_clear = 0.0
  HR3D_wd_clear = 0.0

  avgLWP_wd = 0.0
  avgLWP_wd_cloud = 0.0
      
  countTot_wd_test = 0 
  countTot_cloud_wd = 0
  countTot_clear_wd = 0
     
  do ix=xc_sta, xc_end
    do iy=yc_sta, yc_end
      do iz=1,G_numHgt
      
	HR1D_wd = HR1D_wd + hr_mcipa(ix,iy,iz)
        HR3D_wd = HR3D_wd + hr_mysti(ix,iy,iz)
        countTot_wd_test = countTot_wd_test + 1
            
        if(lwc3D(ix,iy,iz) .gt. 0.0) then !cloudy pixel
	  HR1D_wd_cloud = HR1D_wd_cloud + hr_mcipa(ix,iy,iz)
          HR3D_wd_cloud = HR3D_wd_cloud + hr_mysti(ix,iy,iz)
	  countTot_cloud_wd = countTot_cloud_wd + 1
              
          avgLWP_wd_inc = lwc3D(ix,iy,iz)*dz
          avgLWP_wd = avgLWP_wd + avgLWP_wd_inc
          avgLWP_wd_cloud = avgLWP_wd_cloud + avgLWP_wd_inc       
	else !clear-sky pixel
          HR1D_wd_clear = HR1D_wd_clear + hr_mcipa(ix,iy,iz)
          HR3D_wd_clear = HR3D_wd_clear + hr_mysti(ix,iy,iz)
	  countTot_clear_wd = countTot_clear_wd + 1
	end if
                          
      end do
    end do 
  end do 
      
  ! CALCULATE "WINDOW" AVERAGES:
  HR1D_wd = HR1D_wd/REAL(G_numTot_wd)
  HR3D_wd = HR3D_wd/REAL(G_numTot_wd)
      
  HR1D_wd_cloud = HR1D_wd_cloud/REAL(countTot_cloud_wd)
  HR3D_wd_cloud = HR3D_wd_cloud/REAL(countTot_cloud_wd)
      
  HR1D_wd_clear = HR1D_wd_clear/REAL(countTot_clear_wd)
  HR3D_wd_clear = HR3D_wd_clear/REAL(countTot_clear_wd)
      
  avgLWP_wd = avgLWP_wd/REAL(G_numTot_wd)
  avgLWP_wd_cloud = avgLWP_wd_cloud/REAL(countTot_cloud_wd)
    
  count_totCC = 0
  totCC_wd = 0.0
      
  ! Determine total cloud cover of the window:
  do ix=xc_sta, xc_end
    do iy=yc_sta, yc_end
      do iz=1,G_numHgt
	if (lwc3D(ix,iy,iz) .gt. 0.0) then  !cloudy pixel
	  count_totCC = count_totCC + 1
          ! Get out of the loop over iz as soon as you find the first cloudy cell!
          EXIT ! OK, with this exit we exit only the loop over iz, not the entire loop.
	end if	                         
      end do
    end do 
  end do 
         
  ! Calculate total cloud cover of the window:
  totCC_wd = (count_totCC*area_pix/area_wd)*100.0
      
  82 format(i10, 2(i6), 6(f12.6), 2(f12.6), f12.4)

  write(fileunit_out,82) wd_number, xc_sta, yc_sta, HR1D_wd, HR3D_wd, HR1D_wd_cloud, HR3D_wd_cloud, &
      HR1D_wd_clear, HR3D_wd_clear, avgLWP_wd, avgLWP_wd_cloud, totCC_wd

end subroutine process_wd
  