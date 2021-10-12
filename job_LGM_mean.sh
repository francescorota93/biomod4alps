#!/bin/bash
#SBATCH -J mean_LGM
#SBATCH -N 1
#SBATCH --partition=vsc3plus_0256
#SBATCH --qos=vsc3plus_0256
##SBATCH --account=lv71418
#SBATCH --mail-user=francesco.rota@unibz.it
#SBATCH --mail-type=BEGIN,END
##SBATCH --time-min=3

module purge
module load intel/18 intel-mkl/2018 R/3.6.2 gdal/2.4.1 proj/4.9.3 gcc

Rscript $HOME/data/biomod4alps/LGM_mean.R -c 20
