#=====================================================================
# Makefile cosmo_window 
# Ver: 1.00  $NC 01.02.2018
#====================================================================

# Compile & Link:
CL= gfortran

# Compile only:
CC= gfortran -c 

# Compile only, full check:
WCC= gfortran -c -fcheck=all

# Link:
LNK= gfortran 

# Binary name:
BINARY= run_window

# Global module:
GLOB= globals.f90 

# List of object files:
OBJ= globals.o read_inp_file_hr_sol.o read_inp_files_hr_the.o process_window.o process_time.o main_window.o


all:
	make cosmo_window

clean:
	rm *.o
	rm *.mod
	rm ${BINARY}
	
globals.o: globals.f90
	${CC} globals.f90

read_inp_file_hr_sol.o: read_inp_file_hr_sol.f90 ${GLOB} 
	${CC} read_inp_file_hr_sol.f90

read_inp_files_hr_the.o: read_inp_files_hr_the.f90 ${GLOB}  
	${CC} read_inp_files_hr_the.f90
	
process_window.o: process_window.f90 ${GLOB} 
	${CC} process_window.f90
	
process_time.o: process_time.f90 ${GLOB} 
	${CC} process_time.f90
	
main_window.o: main_window.f90 ${GLOB} 
	${CC} main_window.f90

	
#Link all modules:
cosmo_window: ${OBJ}
	${LNK} -o ${BINARY} ${OBJ}
	
	
# ----------------------- END ----------------------------

