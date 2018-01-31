#!/bin/bash

echo "Compiling"

happy_bin="run_window"

#ulimit -s 32768
#mmod="-fmax-stack-var-size=4096" 
#mmod="-fcheck=all"
mmod=" "

rm *.o
rm *.mod
rm $happy_bin

gfortran -c $mmod globals.f90 
gfortran -c $mmod process_time.f90
gfortran -c $mmod process_window.f90
gfortran -c $mmod read_inp_file_hr_sol.f90
gfortran -c $mmod read_inp_files_hr_the.f90

gfortran $mmod globals.o process_time.o process_window.f90 read_inp_file_hr_sol.o read_inp_files_hr_the.o main_window.f90 -o $happy_bin



