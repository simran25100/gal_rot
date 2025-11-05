#!/bin/tcsh
#SBATCH --job-name=stan25_ncirc_core32
#SBATCH --mail-type=ALL
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=4
#SBATCH --mem=64G
#SBATCH --time=144:00:00
#SBATCH --output=%x.out
#SBATCH --error=%x.err
#SBATCH --partition=core32

echo "------------------------------------------------------"
echo -n "Job is running on node(s): "
scontrol show hostnames $SLURM_NODELIST
echo "------------------------------------------------------"
echo "SLURM: Job running on host(s): $SLURM_SUBMIT_HOST"
echo "SLURM: originating queue: $SLURM_JOB_PARTITION"
echo "SLURM: executing job in working directory: $SLURM_SUBMIT_DIR"
echo "SLURM: job identifier: $SLURM_JOB_ID"
echo "SLURM: job name: $SLURM_JOB_NAME"
echo "SLURM: node file: $SLURM_JOB_NODELIST"
echo "SLURM: current home directory: $HOME"
echo "SLURM: PATH = $PATH"
echo "SLURM: LD_LIBRARY_PATH = $LD_LIBRARY_PATH"
echo "------------------------------------------------------"

source /etc/profile       # ensures modules command is available
module purge              # optional: clear old modules
eval '/soft/R-4.4.2/bin/Rscript'
module load R-4.4.2

Rscript /beegfs/car/bxster25/oc-sscu/plxparam_stan_x/plxparam_stan25.R

echo ------------------------------------------------------
echo Job ends
