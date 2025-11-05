#!/bin/sh
for f in *.sh; do
  [ -e "$f" ] || continue   # skip if no .sh files
  sed -i -e 's|^#!/bin/sh -f|#!/bin/tcsh|' "$f"
done
