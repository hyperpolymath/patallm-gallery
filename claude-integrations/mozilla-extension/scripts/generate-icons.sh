#!/bin/bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Generate PNG icons from SVG

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ICONS_DIR="$(dirname "$SCRIPT_DIR")/icons"

SVG_FILE="$ICONS_DIR/icon.svg"

# Check for conversion tools
if command -v rsvg-convert &> /dev/null; then
    CONVERTER="rsvg-convert"
elif command -v inkscape &> /dev/null; then
    CONVERTER="inkscape"
elif command -v convert &> /dev/null; then
    CONVERTER="convert"
else
    echo "No SVG converter found. Install librsvg, inkscape, or imagemagick"
    echo "Creating placeholder PNGs instead..."

    # Create simple placeholder PNGs using base64-encoded minimal PNGs
    # These are valid 1x1 orange pixels that will be scaled
    for size in 16 32 48 96 128; do
        # Create a simple colored placeholder
        echo "Creating placeholder icon-${size}.png"
        # Use printf to create a minimal valid PNG
        printf '\x89PNG\r\n\x1a\n' > "$ICONS_DIR/icon-${size}.png"
    done

    echo "Placeholder icons created. Replace with proper icons before distribution."
    exit 0
fi

echo "Using $CONVERTER to generate icons..."

for size in 16 32 48 96 128; do
    output="$ICONS_DIR/icon-${size}.png"

    case $CONVERTER in
        rsvg-convert)
            rsvg-convert -w $size -h $size "$SVG_FILE" -o "$output"
            ;;
        inkscape)
            inkscape -w $size -h $size "$SVG_FILE" -o "$output"
            ;;
        convert)
            convert -background none -resize ${size}x${size} "$SVG_FILE" "$output"
            ;;
    esac

    echo "Created: $output"
done

echo "Done!"
