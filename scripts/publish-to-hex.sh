#!/usr/bin/env bash
set -euo pipefail

# Publish faber_tweann to hex.pm
# Usage: ./scripts/publish-to-hex.sh
#
# Requires: ~/.config/rebar3/hex.config with api_key set

cd "$(dirname "$0")/.."

VERSION=$(grep -oP '{vsn,\s*"\K[^"]+' src/faber_tweann.app.src)
echo "==> Publishing faber_tweann v${VERSION} to hex.pm..."

echo "==> Building faber_tweann..."
rebar3 compile

echo "==> Running tests..."
# Tests must pass. Do not add tolerance here.
#
# History: this block previously swallowed failures, attributing them to
# "optional NIF tests" requiring faber_nn_nifs. That explanation was wrong.
# The 20 failures were in network_onnx_tests and had no NIF involvement: the
# #network{} record gained neuron_meta and internal_state for CfC/LTC support,
# and network_onnx matched it as a fixed-arity tuple. Suppressing the failure
# let a broken export path ship to hex in v1.2.0.
rebar3 eunit

echo "==> Building docs..."
rebar3 ex_doc

echo "==> Publishing to hex.pm..."
rebar3 hex publish --yes

echo "==> Done! faber_tweann v${VERSION} published to hex.pm"
echo "==> View at: https://hex.pm/packages/faber_tweann"
