# SPDX-License-Identifier: AGPL-3.0-or-later
# Build stage
FROM docker.io/library/rust:1.83-slim AS builder

WORKDIR /build

# Install build dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    pkg-config \
    libssl-dev \
    libsqlite3-dev \
    && rm -rf /var/lib/apt/lists/*

# Copy source
COPY . .

# Build release binary
RUN cargo build --release --package llm-unify-cli

# Runtime stage
FROM cgr.dev/chainguard/wolfi-base:latest

LABEL org.opencontainers.image.source="https://github.com/hyperpolymath/llm-unify"
LABEL org.opencontainers.image.description="Unified interface for multi-platform LLM conversation management (ALPHA)"
LABEL org.opencontainers.image.licenses="AGPL-3.0-or-later"

# Install runtime dependencies
RUN apk add --no-cache sqlite-libs

# Copy binary from builder
COPY --from=builder /build/target/release/llm-unify /usr/local/bin/llm-unify

# Create non-root user
RUN adduser -D -u 1000 llmunify
USER llmunify

WORKDIR /home/llmunify

ENTRYPOINT ["/usr/local/bin/llm-unify"]
