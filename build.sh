#!/bin/sh
#

dir_root=$(pwd)

################# Hera or Ursa ####################
if [[ -d /scratch3 ]]; then
    if [[ -d /apps/slurm_hera ]]; then
        platform=hera
    else
        platform=ursa
    fi
    source /etc/profile.d/modules.sh

################# Jet ####################
elif [[ -d /jetmon ]] ; then
    source /etc/profile.d/modules.sh
    platform=jet

################# Cheyenne ####################
elif [[ -d /glade ]] ; then
    source /etc/profile.d/modules.sh
    platform=cheyenne

################# MSU HPC2 ####################
elif [[ -d /work/noaa ]] ; then
    hoststr=$(hostname)
    if [[ "$hoststr" == "hercules"* ]]; then
        platform=hercules
    else
        platform=orion
    fi

################# Gaea C6 ####################
elif [[ -d /gpfs/f6 ]] ; then ### gaea c6
    platform=gaeaC6

################# WCOSS2 ####################
elif [[ -d /lfs ]] ; then  ### orion
    platform=wcoss2

################# Generic ####################
else
    echo -e "\nunknown machine"
    exit 9
fi

if [ ! -f $modulefile ]; then
    echo "modulefiles $modulefile does not exist"
    exit 10
fi

#source $modulefile
set -x

module purge
module use ${dir_root}/modulefiles
module load build_${platform}_intel.lua
module list 

build_root=${dir_root}/build
mkdir -p ${build_root}
cd ${build_root}

cmake .. -DCMAKE_INSTALL_PREFIX=.

make VERBOSE=1 -j 1 
make install

exit
