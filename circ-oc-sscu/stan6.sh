#!/bin/tcsh
#SBATCH --job-name=stan6_circ_core32
#SBATCH --mail-type=ALL
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=4
#SBATCH --mem=64G
#SBATCH --time=7-00:00:00
#SBATCH --output=%x.out
#SBATCH --error=%x.err
#SBATCH --partition=core32

echo ------------------------------------------------------
echo -n 'Job is running on node '; cat $SLURM_NODELIST
echo ------------------------------------------------------
echo "SLURM: qsub is running on $SLURM_SUBMIT_HOST"
echo "SLURM: originating queue is $SLURM_JOB_PARTITION"
echo "SLURM: executing queue is $SLURM_JOB_PARTITION"
echo "SLURM: working directory is $SLURM_SUBMIT_DIR"
echo "SLURM: job identifier is $SLURM_JOB_ID"
echo "SLURM: job name is $SLURM_JOB_NAME"
echo "SLURM: node file is $SLURM_NODELIST"
echo "SLURM: current home directory is $HOME"
echo "SLURM: PATH = $PATH"
echo ------------------------------------------------------

source /etc/profile       # ensures modules command is available
module purge              # optional: clear old modules
eval '/soft/R-4.4.2/bin/Rscript'
module load R-4.4.2

# Check paths
which Rscript

Rscript /beegfs/car/bxster25/circ-oc-sscu/plxparam_stan_x/plxparam_stan6.R

echo ------------------------------------------------------
echo Job ends
