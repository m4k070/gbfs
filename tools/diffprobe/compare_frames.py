#!/usr/bin/env python3
"""gbfs と PyBoy のフレーム連番 (PPM) を対応付けて差分を出す。

同じ入力列を両者に流したとき、同じ連番が同じフレームに対応する。
4 階調に量子化して MAE を出し、差分の大きいフレームだけを報告する。

使い方: compare_frames.py <gbfs_dir> <pyboy_dir>
"""
import glob
import os
import sys

import numpy as np


def read_ppm(path):
    with open(path, "rb") as f:
        data = f.read()
    parts = data.split(b"\n", 3)
    w, h = map(int, parts[1].split())
    body = parts[3] if len(parts) > 3 else b""
    arr = np.frombuffer(body[: w * h * 3], dtype=np.uint8)
    if arr.size < w * h * 3:
        return None
    return arr.reshape(h, w, 3).astype(np.int16)


def quantize(img):
    lum = img.mean(axis=2)
    return np.clip((lum / 64.0).astype(np.int16), 0, 3)


def main():
    if len(sys.argv) < 3:
        print(__doc__)
        return 1
    d1, d2 = sys.argv[1], sys.argv[2]
    p1 = sorted(glob.glob(os.path.join(d1, "*.ppm")))
    p2 = sorted(glob.glob(os.path.join(d2, "*.ppm")))
    n = min(len(p1), len(p2))
    print(f"gbfs={len(p1)} pyboy={len(p2)} 比較={n}")
    if n == 0:
        print("PPM が見つかりません")
        return 1
    worst = []
    for i in range(n):
        a, b = read_ppm(p1[i]), read_ppm(p2[i])
        if a is None or b is None:
            continue
        mae = float(np.abs(quantize(a) - quantize(b)).mean())
        worst.append((mae, i, os.path.basename(p1[i])))
    worst.sort(reverse=True)
    print("=== 差分の大きい順 (上位 10) ===")
    for mae, i, name in worst[:10]:
        print(f"  #{i:4d} {name} MAE={mae:.4f}")
    same = sum(1 for m, _, _ in worst if m == 0.0)
    print(f"完全一致: {same}/{n} フレーム")
    return 0


if __name__ == "__main__":
    sys.exit(main())
