#!/bin/sh
# Build the Rust NIFs for faber_tweann.
#
# Usage: priv/build-nifs.sh [BASEDIR]
# Called from rebar.config pre_hooks during compilation.
#
# Absorbed from the faber-nn-nifs package in v2.0.0. The NIFs are built from
# source, never downloaded: fetching prebuilt artifacts couples the package to
# a specific glibc and has bitten us before.
set -eu

BASEDIR="${1:-.}"
PRIV_DIR="${BASEDIR}/priv"
NATIVE_DIR="${BASEDIR}/native"

# When compiling inside _build/, native/ is not symlinked but src/ is.
# Follow the src symlink back to the source root to find native/.
if [ ! -d "${NATIVE_DIR}" ] && [ -L "${BASEDIR}/src" ]; then
    SRC_TARGET=$(readlink -f "${BASEDIR}/src")
    SOURCE_ROOT=$(dirname "${SRC_TARGET}")
    if [ -d "${SOURCE_ROOT}/native" ]; then
        NATIVE_DIR="${SOURCE_ROOT}/native"
    fi
fi

mkdir -p "${PRIV_DIR}"

build_nif() {
    CRATE_NAME="$1"
    NIF_FILE="${PRIV_DIR}/lib${CRATE_NAME}.so"
    CRATE_DIR="${NATIVE_DIR}/${CRATE_NAME}"

    if [ -f "${NIF_FILE}" ]; then
        return 0
    fi

    if [ ! -d "${CRATE_DIR}" ]; then
        echo "[${CRATE_NAME}] ERROR: no crate source at ${CRATE_DIR}" >&2
        return 1
    fi

    if ! command -v cargo >/dev/null 2>&1; then
        echo "[${CRATE_NAME}] ERROR: Rust toolchain not found." >&2
        echo "[${CRATE_NAME}] faber_tweann builds its NIFs from source and requires cargo." >&2
        echo "[${CRATE_NAME}] Install: curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh" >&2
        echo "[${CRATE_NAME}] To build without native acceleration, set:" >&2
        echo "[${CRATE_NAME}]     FABER_TWEANN_SKIP_NIF=1" >&2
        echo "[${CRATE_NAME}] and configure {faber_tweann, [{nif_impl, fallback}]} at runtime." >&2
        return 1
    fi

    echo "[${CRATE_NAME}] Building NIF from source..."
    cargo build --release --manifest-path "${CRATE_DIR}/Cargo.toml"

    if [ -f "${CRATE_DIR}/target/release/lib${CRATE_NAME}.so" ]; then
        cp "${CRATE_DIR}/target/release/lib${CRATE_NAME}.so" "${NIF_FILE}"
    elif [ -f "${CRATE_DIR}/target/release/lib${CRATE_NAME}.dylib" ]; then
        cp "${CRATE_DIR}/target/release/lib${CRATE_NAME}.dylib" "${NIF_FILE}"
    else
        echo "[${CRATE_NAME}] ERROR: build succeeded but no shared library found." >&2
        return 1
    fi

    echo "[${CRATE_NAME}] Built ${NIF_FILE}"
}

if [ "${FABER_TWEANN_SKIP_NIF:-0}" = "1" ]; then
    echo "[faber_tweann] FABER_TWEANN_SKIP_NIF=1, skipping NIF build."
    echo "[faber_tweann] The pure Erlang fallback must be selected explicitly:"
    echo "[faber_tweann]     {faber_tweann, [{nif_impl, fallback}]}"
    exit 0
fi

build_nif faber_nn_nifs
