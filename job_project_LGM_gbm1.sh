#!/bin/bash
#SBATCH -J LGM
#SBATCH -N 1
#SBATCH --partition=vsc3plus_0256
#SBATCH --qos=vsc3plus_0256
##SBATCH --account=lv71284
##SBATCH --mail-user=giulio.genova@eurac.edu
#SBATCH --mail-user=francesco.rota@education.unibz.it
#SBATCH --mail-type=BEGIN,END
##SBATCH --time-min=3

module purge
module load intel/18 intel-mkl/2018 R/3.6.2 gdal/2.4.1 proj/4.9.3

## Rscript $HOME/data/biomod4alps/launch_project_LGM.R all 30

Rscript $HOME/data/biomod4alps/launch_project_LGM.R -c 20  -w $HOME/data/ --scriptdir $HOME/data/biomod4alps -s 1 6 --models GBM --temprastdir $HOME/temp_rast_dir       
