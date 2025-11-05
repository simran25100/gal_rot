#!/bin/sh

for f in *.sh; do
sed -i -e 's|#!/bin/bash -f|#!/bin/bash|' "$f"
done

for f in *.sh; do
  sed -i \
    -e 's|#!/bin/tcsh|#!/bin/tcsh|' "$f"
done
