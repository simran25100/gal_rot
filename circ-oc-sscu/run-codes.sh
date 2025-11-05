#!/bin/tcsh
# queuing the stan scripts from 1-25
echo "Queue rstan circular scripts"

foreach i (`seq 1 25`)
  sbatch "stan${i}.sh"
end

echo "Jobs submitted successfully! :)"

