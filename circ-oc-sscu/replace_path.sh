#!/bin/tcsh

# Loop over all .sh files in the current directory
foreach f (*.sh)
    echo "Processing $f ..."
    sed -i '/echo LD PATH = \$LD_LIBRARY_PATH/d' "$f"
end

echo "✅ All files updated successfully."
