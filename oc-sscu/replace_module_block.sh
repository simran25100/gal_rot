#!/bin/tcsh
# replace_module_block.sh
# Replaces the openmpi + R module block with your new environment setup in all .sh files

for f in *.sh; do
  [ -f "$f" ] || continue
  echo "Updating $f ..."

source /etc/profile       # ensures modules command is available
module purge              # optional: clear old modules
eval '/soft/R-4.4.2/bin/Rscript'
module load R-4.4.2

# Check paths
which Rscript
eval '\''/soft/R-4.4.2/bin/Rscript'\''\
module load R-4.4.2\
\
# Check paths\
which Rscript
}' "$f"
done

echo "✅ Replacement complete for all .sh files."
