#!/bin/tcsh
# This script renames jobnames in all stan*.sh files to stan<number>_circ_core32

for f in stan*.sh; do
  num=$(echo "$f" | grep -o '[0-9]\+')
  sed -i "s/^#SBATCH --job-name=.*/#SBATCH --job-name=stan${num}_circ_core32/" "$f"
done

echo "✅ Job names updated."
