# SPDX-License-Identifier: MPL-2.0
# ClusterFuzzLite Dockerfile for Rust fuzzing

FROM gcr.io/oss-fuzz-base/base-builder-rust

# Copy source code
COPY . $SRC/did-you-actually-do-that

# Set working directory
WORKDIR $SRC/did-you-actually-do-that

# Copy build script
COPY .clusterfuzzlite/build.sh $SRC/
