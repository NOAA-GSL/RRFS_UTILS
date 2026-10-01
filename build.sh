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

if command -v llvm-ar >/dev/null 2>&1; then
  cmake .. -DCMAKE_INSTALL_PREFIX=. -DCMAKE_AR="$(command -v llvm-ar)" -DCMAKE_RANLIB="$(command -v llvm-ranlib)"
else
  cmake .. -DCMAKE_INSTALL_PREFIX=.
fi

make VERBOSE=1 -j 1 
make install

exit
