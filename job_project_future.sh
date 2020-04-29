#!/bin/bash
#SBATCH -J future
#SBATCH -N 1
#SBATCH --partition=mem_0128
#SBATCH --qos=normal_0128
##SBATCH --account=lv71284
##SBATCH --mail-user=giulio.genova@eurac.edu
#SBATCH --mail-user=francesco.rota@education.unibz.it
#SBATCH --mail-type=BEGIN,END
##SBATCH --time-min=3

module purge
module load intel/18 intel-mkl/2018 R/3.6.2 gdal/2.4.1 proj/4.9.3

## Rscript $HOME/data/biomod4alps/launch_project_future.R all 30

Rscript $HOME/data/biomod4alps/launch_project_future.R -c 25  -w $HOME/data/ --scriptdir $HOME/data/biomod4alps -s 3 4 --models  GAM  RF 
