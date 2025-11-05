#!/bin/bash
# update_paths.sh
# Updates paths in all .R files for circ-oc-sscu version

echo "🔧 Updating paths in .R files..."

for f in *.R; do
  sed -i \
    -e 's/max_treedepth = 12/max_treedepth = 14/g' \
    -e 's/iter = 10000,/iter = 12000,/g' \
    "$f"
done

echo "🎉 All .R files have been updated successfully!"
