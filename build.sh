#!/bin/sh
#

dir_root=$(pwd)
COMPILER=${COMPILER:-intel}

module purge
source ./detect_machine.sh
module use ${dir_root}/modulefiles
module load build_${MACHINE}_${COMPILER}.lua
module list 

build_root=${dir_root}/build
rm -rf "${build_root}"
mkdir -p "${build_root}"
cd "${build_root}" || exit 1

cmake .. -DCMAKE_INSTALL_PREFIX=.

make VERBOSE=1 -j 1 
make install

exit
