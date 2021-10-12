#!/bin/bash
#SBATCH -J build
#SBATCH -N 1
#SBATCH --partition=vsc3plus_0064
#SBATCH --qos=vsc3plus_0064
##SBATCH --account=lv71418
#SBATCH --mail-user=francesco.rota@unibz.it
#SBATCH --mail-type=BEGIN,END
##SBATCH --time-min=3

module purge
module load intel/18 intel-mkl/2018 R/3.6.2 gdal/2.4.1 proj/4.9.3 gcc

## Rscript $HOME/data/biomod4alps/launch_build_model.R all 30
Rscript $HOME/data/biomod4alps/launch_mean_LGM.R -c 20

