#!/bin/bash
gfortran -O3 gen_xdmf_points.f90 && ./a.out
gfortran -O3 gen_xdmf_box.f90  && ./a.out
rm a.out
