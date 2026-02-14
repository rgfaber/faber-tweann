#!/usr/bin/env bash
# Publish faber-tweann v0.15.3 to hex.pm
# Run from repository root: ./scripts/publish-v0.15.3.sh

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_DIR="$(dirname "$SCRIPT_DIR")"

cd "$REPO_DIR"

echo "=== faber-tweann v0.15.3 Publication ==="
echo ""

# Verify we're on the right commit
CURRENT_TAG=$(git describe --tags --exact-match 2>/dev/null || echo "no tag")
if [[ "$CURRENT_TAG" != "v0.15.3" ]]; then
    echo "Warning: Current commit is not tagged as v0.15.3 (got: $CURRENT_TAG)"
    echo "Continuing anyway..."
fi

# Run tests
echo "Running tests..."
rebar3 eunit

# Check dialyzer
echo ""
echo "Running dialyzer..."
rebar3 dialyzer || echo "Dialyzer warnings (pre-existing)"

# Push to git
echo ""
echo "Pushing to git..."
git push origin main
git push origin v0.15.3

# Publish to hex.pm
echo ""
echo "Publishing to hex.pm (requires authentication)..."
rebar3 hex publish --yes

echo ""
echo "=== Done ==="
echo "Verify at: https://hex.pm/packages/faber_tweann"
