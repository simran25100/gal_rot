#!/bin/tcsh
# This script renames jobnames in all stan*.sh files to stan<number>_circ_core32

for f in stan{1..25}.sh; do
  echo "🔧 Updating job name in $f..."
  sed -i "s/\(#SBATCH --job-name=stan[0-9]*\)_circ_core32/\1_ncirc_core32/" "$f"
done

echo "✅ Job names updated."
