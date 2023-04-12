#!/bin/bash
#
for fortran in $(ls *.f)
do 
  mkdir -p ${fortran%??}
  cp $fortran ${fortran%??}

  cd ${fortran%??}
  fsplit ${fortran}
  rm ${fortran}

  gfortran  -g -fdec -std=legacy -C -c *.f
  ar rcv ${fortran%??}.a *.o
  ranlib ${fortran%??}.a

  cd ..
  cp ${fortran%??}/*.a .
  rm -rf ${fortran%??}
done
