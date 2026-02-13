#!/bin/bash
# Script to export README.md to PDF via grip (GitHub-flavored HTML)

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

OUTPUT_PDF="2026_02_Presupuesto_Univ.pdf"

echo "Step 1: Exporting README.md to README.html using grip..."
grip README.md --export README.html

if [ ! -f "README.html" ]; then
    echo "Error: README.html was not created"
    exit 1
fi

echo "Step 2: Converting README.html to PDF..."
wkhtmltopdf \
    --enable-local-file-access \
    --encoding UTF-8 \
    --page-size A4 \
    --margin-top 15mm \
    --margin-bottom 15mm \
    --margin-left 15mm \
    --margin-right 15mm \
    --print-media-type \
    README.html "$OUTPUT_PDF"

if [ -f "$OUTPUT_PDF" ]; then
    echo "✓ PDF generated successfully: $OUTPUT_PDF"
    cp "$OUTPUT_PDF" "Presupuesto_Univ.pdf"
    echo "✓ Copy created: Presupuesto_Univ.pdf"
else
    echo "Error: PDF was not created"
    exit 1
fi

echo "Done!"
