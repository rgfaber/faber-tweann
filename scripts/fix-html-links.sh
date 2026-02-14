#!/usr/bin/env bash
# Fix .html links to .md in markdown files
# Ex_doc automatically converts .md to .html

set -e
cd "$(dirname "$0")/.."

echo "Fixing .html links to .md in guides..."

# Replace .html) with .md) in all guide markdown files
for file in guides/*.md; do
    if grep -q '\.html)' "$file"; then
        echo "  Fixing: $file"
        sed -i 's/\.html)/.md)/g' "$file"
    fi
done

echo "Done!"
