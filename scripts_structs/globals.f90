MODULE wd_globals
  !integer,parameter :: dp=kind(0.d0)   ! double precision
  !integer,parameter :: sp=kind(0.0 )   ! single precision
  integer,parameter :: G_Flag_sol = 1 ! 1 = solar, 0 or otherwise thermal;
  !integer,parameter :: G_Flag_prp_inp = 1 ! prepare input for twostream and twomaxrnd (1=yes, 0=no)
  integer,parameter :: G_numX = 256
  integer :: G_numHgt 
  integer :: G_indZcld_min 
  integer :: G_indZcld_max 
  integer :: G_numTot_wd !=G_numX_wd*G_numX_wd*G_numHgt
  !integer :: G_numHor_wd !=G_numX_wd*G_numX_wd
END MODULE wd_globals

!G_numHgt = 17 !17=solar, 19=thermal