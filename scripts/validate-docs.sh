#!/bin/bash
# Validate documentation links and SVG references

set -e

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO_ROOT"

echo "=== Documentation Validation ==="
echo ""

# Track errors
ERRORS=0

# Check for SVG references that might be broken (exclude http URLs and _build)
echo "Checking SVG file references..."
while IFS= read -r line; do
    file=$(echo "$line" | cut -d: -f1)
    svg_ref=$(echo "$line" | grep -oP '\]\([^)]+\.svg\)' | tr -d '()[]' | sed 's/^!//')

    if [[ -n "$svg_ref" && "$svg_ref" != http* && "$svg_ref" != https* ]]; then
        dir=$(dirname "$file")
        full_path="$dir/$svg_ref"

        if [[ ! -f "$full_path" ]]; then
            echo "  BROKEN: $file references $svg_ref (not found at $full_path)"
            ERRORS=$((ERRORS + 1))
        fi
    fi
done < <(grep -rn "\.svg)" --include="*.md" --exclude-dir=_build 2>/dev/null)

echo ""

# Check for .md file references (exclude http URLs and _build)
echo "Checking .md file references..."
while IFS= read -r line; do
    file=$(echo "$line" | cut -d: -f1)
    md_ref=$(echo "$line" | grep -oP '\]\([^)]+\.md[^)]*\)' | tr -d '()[]' | sed 's/^!//' | head -1)

    if [[ -n "$md_ref" && "$md_ref" != http* && "$md_ref" != https* ]]; then
        # Remove any #anchors
        md_ref=$(echo "$md_ref" | cut -d'#' -f1)
        dir=$(dirname "$file")
        full_path="$dir/$md_ref"

        if [[ ! -f "$full_path" ]]; then
            echo "  BROKEN: $file references $md_ref (not found at $full_path)"
            ERRORS=$((ERRORS + 1))
        fi
    fi
done < <(grep -rn "\](.*\.md" --include="*.md" --exclude-dir=_build 2>/dev/null | grep -v "http")

echo ""

# Check for ASCII diagrams in key documentation files (exclude _build and CODE_QUALITY)
echo "Checking for ASCII diagrams that may need SVG replacement..."
ASCII_DIAGRAM_FILES=$(grep -l "┌\|┐\|└\|┘" --include="*.md" -r . --exclude-dir=_build 2>/dev/null | grep -v CODE_QUALITY || true)

if [[ -n "$ASCII_DIAGRAM_FILES" ]]; then
    echo "  Files with ASCII diagrams:"
    for f in $ASCII_DIAGRAM_FILES; do
        echo "    - $f"
    done
    echo "  Note: Some ASCII diagrams are acceptable (e.g., code examples, simple trees)"
fi

echo ""

# Summary
if [[ $ERRORS -gt 0 ]]; then
    echo "=== FAILED: $ERRORS broken links found ==="
    exit 1
else
    echo "=== PASSED: All documentation links are valid ==="
fi
