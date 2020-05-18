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

<<<<<<< HEAD
Rscript $HOME/data/biomod4alps/launch_project_future.R -c 25  -w $HOME/data/ --scriptdir $HOME/data/biomod4alps -s 6 7 8  --models  GLM GBM  --outdir $GLOBAL/models_future 
=======
Rscript $HOME/data/biomod4alps/launch_project_future.R -c 25  -w $HOME/data/ --scriptdir $HOME/data/biomod4alps -s 6 7 8  --models  GAM CTA  --temprastdir $HOME/data/biomod4alps/temp_rast_dir        
>>>>>>> 311d2278b6e36275747e911682403c073609f43e
