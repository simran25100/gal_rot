#!/bin/tcsh
#SBATCH --job-name=stan1_circ_core32
#SBATCH --mail-type=ALL
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=4
#SBATCH --mem=64G
#SBATCH --time=7-00:00:00
#SBATCH --output=%x.out
#SBATCH --error=%x.err
#SBATCH --partition=core32

echo "------------------------------------------------------"
echo -n "Job is running on node(s): " 
scontrol show hostnames $SLURM_NODELIST
echo "------------------------------------------------------"
echo "SLURM: job id is $SLURM_JOB_ID"
echo "SLURM: job name is $SLURM_JOB_NAME"
echo "SLURM: working directory is $SLURM_SUBMIT_DIR"
echo "SLURM: nodes allocated = $SLURM_JOB_NODELIST"
echo "SLURM: tasks per node = $SLURM_TASKS_PER_NODE"
echo "SLURM: current home directory is $HOME"
echo "SLURM: PATH = $PATH"
echo "LD_LIBRARY_PATH = $LD_LIBRARY_PATH"
echo "------------------------------------------------------"

source /etc/profile       # ensures modules command is available
module purge              # optional: clear old modules
eval '/soft/R-4.4.2/bin/Rscript'
module load R-4.4.2

# Check paths
which Rscript

Rscript /beegfs/car/bxster25/circ-oc-sscu/plxparam_stan_x/plxparam_stan1.R

echo "------------------------------------------------------"
echo "Job ends"
