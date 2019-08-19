#!/bin/bash                                                                     
                                                                                
#SBATCH -t 24:00:00
#SBATCH -N 1 # nodes
#SBATCH -n 1 # tasks
#SBATCH --cpus-per-task=28 # number of cores in node
#SBATCH -o O.%x.%j
#SBATCH -e E.%x.%j
#SBATCH --mem 50000
#SBATCH --mail-type=ALL
#SBATCH --mail-user=mvanega1@asu.edu

cd ~/SA/megadapt3/src/r/
module purge
module load r/3.5.2
module load gdal/2.1.3
R CMD INSTALL --no-multiarch --with-keep.source megadaptr

cd scenarios/
Rscript sensitivity_analysis_run.R
cp SAresults SAresults.$SLURM_JOB_ID
