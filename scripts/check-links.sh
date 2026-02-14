#!/bin/bash
# Check documentation links in markdown files
# Usage: ./scripts/check-links.sh

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

cd "$PROJECT_ROOT"

echo "=== Checking documentation links in faber_tweann ==="
echo ""

ERRORS=0
WARNINGS=0

# Check internal file references
echo "Checking internal file references..."
for md_file in $(find . -name "*.md" -not -path "./_build/*" -not -path "./deps/*"); do
    # Extract markdown links like [text](path)
    while IFS= read -r link; do
        # Skip external URLs
        if [[ "$link" =~ ^https?:// ]] || [[ "$link" =~ ^mailto: ]]; then
            continue
        fi

        # Skip anchor-only links
        if [[ "$link" =~ ^# ]]; then
            continue
        fi

        # Skip .html links (these are for HexDocs/ExDoc generated docs)
        if [[ "$link" =~ \.html$ ]] || [[ "$link" =~ \.html# ]]; then
            continue
        fi

        # Remove anchor from link
        link_path="${link%%#*}"

        # Skip empty links
        if [[ -z "$link_path" ]]; then
            continue
        fi

        # Get directory of current file
        file_dir="$(dirname "$md_file")"

        # Resolve relative path
        if [[ "$link_path" =~ ^/ ]]; then
            resolved_path=".$link_path"
        else
            resolved_path="$file_dir/$link_path"
        fi

        # Check if file exists
        if [[ ! -e "$resolved_path" ]]; then
            echo "  BROKEN: $md_file -> $link"
            ((ERRORS++))
        fi
    done < <(grep -oP '\]\(\K[^)]+' "$md_file" 2>/dev/null || true)
done

echo ""
echo "Checking external URLs (sampling)..."

# Sample some external URLs
EXTERNAL_URLS=(
    "https://hex.pm/packages/faber_tweann"
    "https://hexdocs.pm/faber_tweann/"
    "https://github.com/rgfaber/faber-tweann"
    "https://onnx.ai/"
    "https://www.springer.com/gp/book/9781461444626"
)

for url in "${EXTERNAL_URLS[@]}"; do
    if command -v curl &> /dev/null; then
        HTTP_CODE=$(curl -s -o /dev/null -w "%{http_code}" --max-time 10 "$url" 2>/dev/null || echo "000")
        if [[ "$HTTP_CODE" == "200" ]] || [[ "$HTTP_CODE" == "301" ]] || [[ "$HTTP_CODE" == "302" ]]; then
            echo "  OK: $url"
        elif [[ "$HTTP_CODE" == "000" ]]; then
            echo "  TIMEOUT: $url"
            ((WARNINGS++))
        else
            echo "  WARN ($HTTP_CODE): $url"
            ((WARNINGS++))
        fi
    fi
done

echo ""
echo "Checking SVG diagram references..."
for md_file in $(find . -name "*.md" -not -path "./_build/*" -not -path "./deps/*"); do
    while IFS= read -r svg_ref; do
        # Get directory of current file
        file_dir="$(dirname "$md_file")"

        # Resolve path
        if [[ "$svg_ref" =~ ^/ ]]; then
            resolved_path=".$svg_ref"
        else
            resolved_path="$file_dir/$svg_ref"
        fi

        if [[ ! -f "$resolved_path" ]]; then
            echo "  MISSING SVG: $md_file -> $svg_ref"
            ((ERRORS++))
        fi
    done < <(grep -oP '!\[.*?\]\(\K[^)]+\.svg' "$md_file" 2>/dev/null || true)
done

echo ""
echo "=== Summary ==="
echo "Errors: $ERRORS"
echo "Warnings: $WARNINGS"

if [[ $ERRORS -gt 0 ]]; then
    exit 1
fi

exit 0
