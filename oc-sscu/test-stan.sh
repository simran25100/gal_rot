#!/bin/tcsh
#SBATCH --job-name=ncirc_test_core32
#SBATCH --mail-type=ALL
#SBATCH --nodes=2
#SBATCH --ntasks-per-node=4
#SBATCH --mem=64G
#SBATCH --time=12:00:00
#SBATCH --output=stan_test_core32.out
#SBATCH --error=stan_test_core32.err
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

# Check paths
which Rscript

Rscript /beegfs/car/bxster25/plxparam_stan_x/test_plxparam_stan.R

echo "------------------------------------------------------"
echo "Job ends"
