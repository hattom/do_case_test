#!/bin/bash

script=./do_case.F90

export OMP_NUM_THREADS=4
export FFLAGS="-O2 -fopenmp -ftree-vectorize -march=znver1 -mtune=generic"
for version in A B C D E
do
  echo $version
  gfortran ${FFLAGS} -Dversion${version} ${script} && time ./a.out
  gfortran ${FFLAGS} -Dversion${version} -S ${script} && mv $(basename ${script} .F90){,_${version}}.s
  echo ""; echo "";
done
