#!/bin/bash

script=./do_case.F90
version_list=${1:-A B C D E X}
rc_list=${2:-R C}

echo "md5sum: $(md5sum ${script})"

export OMP_NUM_THREADS=${OMP_NUM_THREADS:-$(nproc)}
FFLAGS=${FFLAGS:- -O2 -fopenmp -ftree-vectorize -march=znver1 -mtune=generic}
for rc in $(echo ${rc_list} | tr '[:upper:]' '[:lower:]')
do
  echo "R/C: ${rc}"
  if [ ${rc} == "r" ]; then
    DFLAGS="-DROW_MAJ"
  else
    DFLAGS="-DCOL_MAJ"
  fi
  TMP_FFLAGS="${FFLAGS} ${DFLAGS}"
  for version in ${version_list}
  do
    echo $version
    echo gfortran ${TMP_FFLAGS} -Dversion${version} ${script}
    gfortran ${TMP_FFLAGS} -Dversion${version} ${script} && time ./a.out
    gfortran ${TMP_FFLAGS} -Dversion${version} -S ${script} && mv $(basename ${script} .F90){,_${version}_${rc}}.s
    echo ""; echo "";
  done
done
