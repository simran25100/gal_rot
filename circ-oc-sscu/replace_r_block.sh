#!/bin/tcsh
# replace_r_block.sh
source /etc/profile       # ensures modules command is available
module purge              # optional: clear old modules
eval '/soft/R-4.4.2/bin/Rscript'
module load R-4.4.2

# Check paths
which Rscript

for f in *.sh; do
  [ -f "$f" ] || continue
  echo "Updating $f ..."
  
source /etc/profile       # ensures modules command is available
module purge              # optional: clear old modules
eval '/soft/R-4.4.2/bin/Rscript'
module load R-4.4.2

# Check paths
which Rscript
source /etc/profile       # ensures modules command is available\
module purge              # optional: clear old modules\
eval '\''/soft/R-4.4.2/bin/Rscript'\''\
source /etc/profile       # ensures modules command is available
module purge              # optional: clear old modules
eval '/soft/R-4.4.2/bin/Rscript'
module load R-4.4.2

# Check paths
which Rscript
\
# Check paths\
which Rscript' "$f"
done

echo "✅ Replacement complete for all .sh files."
