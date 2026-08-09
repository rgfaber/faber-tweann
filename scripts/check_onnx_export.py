#!/usr/bin/env python3
"""Load every .onnx written by check_onnx_export.escript, check it against the
ONNX checker, run it, and compare with the evaluator's own output.

Two sides of a boundary. Exit 1 if any model fails to load, fails the checker,
or disagrees numerically."""
import json
import sys
from pathlib import Path

import numpy as np
import onnx
import onnxruntime as ort

TOL = 1e-5


def check(stem: Path) -> bool:
    name = stem.name
    truth = json.loads(stem.with_suffix(".json").read_text())
    try:
        model = onnx.load(str(stem.with_suffix(".onnx")))
    except Exception as e:
        print(f"{name:14s} LOAD FAILED: {type(e).__name__}: {e}")
        return False
    try:
        onnx.checker.check_model(model, full_check=True)
    except Exception as e:
        print(f"{name:14s} CHECKER FAILED: {type(e).__name__}: {e}")
        return False
    try:
        sess = ort.InferenceSession(str(stem.with_suffix(".onnx")),
                                    providers=["CPUExecutionProvider"])
        feed = {sess.get_inputs()[0].name:
                np.array([truth["input"]], dtype=np.float32)}
        got = np.array(sess.run(None, feed)[0]).flatten()
    except Exception as e:
        print(f"{name:14s} RUN FAILED: {type(e).__name__}: {e}")
        return False
    want = np.array(truth["expected"], dtype=np.float32)
    if got.shape != want.shape:
        print(f"{name:14s} SHAPE MISMATCH: onnx {got.shape} vs evaluator {want.shape}")
        return False
    delta = float(np.max(np.abs(got - want)))
    if delta > TOL:
        print(f"{name:14s} NUMERIC MISMATCH: max|delta| = {delta:.3e}")
        print(f"{'':14s}   onnx      = {got.tolist()}")
        print(f"{'':14s}   evaluator = {want.tolist()}")
        return False
    print(f"{name:14s} ok  (max|delta| = {delta:.2e})")
    return True


def main() -> int:
    d = Path(sys.argv[1])
    stems = sorted({p.with_suffix("") for p in d.glob("*.onnx")})
    if not stems:
        print(f"no .onnx files in {d}")
        return 1
    results = [check(s) for s in stems]
    print(f"\n{sum(results)}/{len(results)} models agree with the evaluator")
    return 0 if all(results) else 1


if __name__ == "__main__":
    sys.exit(main())
