#!/bin/tcsh
# remove_rscript_block.sh
# This deletes the Rscript/module block from all .sh files in the current directory

for f in *.sh; do
  sed -i 's|#!/bin/tcsh|#!/bin/sh|g' "$f"
done

echo "✅ Block removed from all .sh files."
