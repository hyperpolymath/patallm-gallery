FROM gcr.io/oss-fuzz-base/base-builder-rust
RUN apt-get update && apt-get install -y make autoconf automake libtool
COPY . $SRC/llm-unify-core
WORKDIR $SRC/llm-unify-core
COPY .clusterfuzzlite/build.sh $SRC/
