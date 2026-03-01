#!/bin/bash -eu
# SPDX-License-Identifier: MPL-2.0
# ClusterFuzzLite build script for did-you-actually-do-that

cd $SRC/did-you-actually-do-that

# Build fuzz targets with cargo-fuzz
cargo +nightly fuzz build

# Copy fuzz targets to output directory
for target in fuzz/target/*/release/fuzz_*; do
    if [ -f "$target" ] && [ -x "$target" ]; then
        cp "$target" $OUT/
    fi
done
