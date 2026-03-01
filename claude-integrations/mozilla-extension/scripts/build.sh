#!/bin/bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Build script for Claude Mozilla Extension

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
BUILD_DIR="$PROJECT_DIR/dist"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m'

echo -e "${BLUE}Building Claude Mozilla Extension${NC}"

# Clean
rm -rf "$BUILD_DIR"
mkdir -p "$BUILD_DIR"

# Function to build a variant
build_variant() {
    local variant=$1
    local manifest_file=$2
    local output_name=$3

    echo -e "${BLUE}Building $variant...${NC}"

    local variant_dir="$BUILD_DIR/$variant"
    mkdir -p "$variant_dir"

    # Copy shared source files
    cp -r "$PROJECT_DIR/src/core" "$variant_dir/"
    cp -r "$PROJECT_DIR/src/ui" "$variant_dir/"
    cp -r "$PROJECT_DIR/src/background" "$variant_dir/"
    cp -r "$PROJECT_DIR/icons" "$variant_dir/"

    # Copy manifest
    cp "$manifest_file" "$variant_dir/manifest.json"

    # Create XPI/ZIP
    cd "$variant_dir"
    zip -r -q "../$output_name" ./*
    cd "$PROJECT_DIR"

    echo -e "${GREEN}  Created: dist/$output_name${NC}"
}

# Build MV3 for Firefox
build_variant "firefox-mv3" \
    "$PROJECT_DIR/manifest-v3/manifest.json" \
    "claude-mozilla-firefox-mv3.xpi"

# Build MV2 for Firefox (legacy)
build_variant "firefox-mv2" \
    "$PROJECT_DIR/manifest-v2/manifest.json" \
    "claude-mozilla-firefox-mv2.xpi"

# Build for Thunderbird
build_variant "thunderbird" \
    "$PROJECT_DIR/manifest-v2/manifest-thunderbird.json" \
    "claude-mozilla-thunderbird.xpi"

# Build for SeaMonkey (uses MV2)
build_variant "seamonkey" \
    "$PROJECT_DIR/manifest-v2/manifest.json" \
    "claude-mozilla-seamonkey.xpi"

# Summary
echo ""
echo -e "${GREEN}Build complete!${NC}"
echo ""
echo "Output files:"
ls -la "$BUILD_DIR"/*.xpi

echo ""
echo "Installation:"
echo "  Firefox: about:debugging > Load Temporary Add-on > select .xpi"
echo "  Thunderbird: Tools > Add-ons > Install from file"
echo "  SeaMonkey: Tools > Add-ons Manager > Install from file"
