#!/usr/bin/env python3
"""比較したフレームの MAE を区間ごとに集計する。

compare_frames.py が「差分上位」を出すのに対し、こちらは時系列で見て
「どこから一致し始めるか」を判定する。ファイル名からフレーム番号を復元する。

使い方: mae_profile.py <gbfs_dir> <pyboy_dir>
"""
import glob
import os
import re
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
    return np.clip((img.mean(axis=2) / 64.0).astype(np.int16), 0, 3)


def frame_no(path):
    m = re.search(r"(\d+)\.ppm$", os.path.basename(path))
    return int(m.group(1)) if m else -1


def main():
    d1, d2 = sys.argv[1], sys.argv[2]
    f1 = {frame_no(p): p for p in glob.glob(os.path.join(d1, "*.ppm"))}
    f2 = {frame_no(p): p for p in glob.glob(os.path.join(d2, "*.ppm"))}
    common = sorted(set(f1) & set(f2))
    print(f"gbfs={len(f1)} pyboy={len(f2)} 共通フレーム={len(common)}")

    rows = []
    for k in common:
        a, b = read_ppm(f1[k]), read_ppm(f2[k])
        if a is None or b is None:
            continue
        rows.append((k, float(np.abs(quantize(a) - quantize(b)).mean())))

    # 100 フレーム刻みで集計
    print("=== 区間ごとの平均 MAE ===")
    buckets = {}
    for k, mae in rows:
        buckets.setdefault(k // 100, []).append(mae)
    for b in sorted(buckets):
        v = buckets[b]
        nz = sum(1 for x in v if x > 0)
        print(f"  {b*100:4d}-{b*100+99:4d}: 平均={sum(v)/len(v):.4f} 不一致={nz}/{len(v)}")

    nz = [(k, m) for k, m in rows if m > 0]
    print(f"=== 不一致フレーム: {len(nz)}/{len(rows)} ===")
    if nz:
        print(f"  最後の不一致: フレーム {nz[-1][0]} (MAE={nz[-1][1]:.4f})")
        tail = [(k, m) for k, m in nz if k >= 200]
        print(f"  フレーム 200 以降の不一致: {len(tail)} / {sum(1 for k, _ in rows if k >= 200)}")


if __name__ == "__main__":
    main()
