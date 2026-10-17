#!/usr/bin/env python3
"""PyBoy 側のプローブ: state をロード (または起動) し、入力列を流して VRAM/座標/レジスタを出す。

gbfs_seq.fsx と同じ指標を出して突き合わせるのが目的。

使い方:
  pyboy_probe.py [state] [--boot] [--inputs "right:16,up:8"] [--dump PREFIX] [--every N] [--reglog FILE]
  --boot を付けると state をロードせず素の起動から始める。
  --reglog は 1 フレームごとの LCDC/STAT/SCY/SCX/LY/LYC/WY/WX + 座標 + VRAM ハッシュを書く。
"""
import argparse
import os
import sys

os.environ.setdefault("SDL_VIDEODRIVER", "dummy")

import numpy as np  # noqa: E402
from pyboy import PyBoy  # noqa: E402

ROM = "/home/makoto/work/gb-roms/Pocket Monsters - Crystal Version (Japan).gbc"


def vram_hash(p):
    h = 0
    for a in range(0x8000, 0xA000):
        h = (h * 31 + p.memory[a]) & 0xFFFFFFFFFFFFFFFF
    return h


def pos(p):
    return p.memory[0xD4D9], p.memory[0xD4DA], p.memory[0xD4D1]


def regs(p):
    return (p.memory[0xFF40], p.memory[0xFF41], p.memory[0xFF42], p.memory[0xFF43],
            p.memory[0xFF44], p.memory[0xFF45], p.memory[0xFF4A], p.memory[0xFF4B])


def save_ppm(p, path):
    arr = np.asarray(p.screen.ndarray)[:, :, :3].astype(np.uint8)
    with open(path, "wb") as f:
        f.write(b"P6\n160 144\n255\n")
        f.write(arr.tobytes())


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("state", nargs="?", default="")
    ap.add_argument("--boot", action="store_true")
    ap.add_argument("--frames", type=int, default=0)
    ap.add_argument("--inputs", default="")
    ap.add_argument("--dump", default="")
    ap.add_argument("--every", type=int, default=5)
    ap.add_argument("--reglog", default="")
    args = ap.parse_args()

    p = PyBoy(ROM, window="null", sound_emulated=False)
    p.set_emulation_speed(0)
    if not args.boot:
        if not args.state:
            print("state を指定するか --boot を付けてください", file=sys.stderr)
            return 2
        with open(args.state, "rb") as f:
            p.load_state(f)

    w = open(args.reglog, "w") if args.reglog else None

    def logregs(n):
        if w:
            lcdc, stat, scy, scx, ly, lyc, wy, wx = regs(p)
            x, y, d = pos(p)
            w.write(f"{n} {lcdc:02X} {stat:02X} {scy:02X} {scx:02X} {ly:02X} {lyc:02X} "
                    f"{wy:02X} {wx:02X} {x} {y} {d} {vram_hash(p):016X}\n")

    print(f"loaded: frame={p.frame_count} pos={pos(p)} vram={vram_hash(p):016X} "
          f"lcdc={p.memory[0xFF40]:02X} wy={p.memory[0xFF4A]} wx={p.memory[0xFF4B]}")

    n = 0
    logregs(n)

    def advance(cnt):
        nonlocal n
        for _ in range(cnt):
            p.tick()
            n += 1
            if args.dump and n % args.every == 0:
                save_ppm(p, f"{args.dump}_{n:05d}.ppm")
            logregs(n)

    if args.inputs:
        for spec in [s for s in args.inputs.split(",") if s.strip()]:
            btn, _, holds = spec.partition(":")
            btn = btn.strip()
            holds = int(holds) if holds else 16
            if btn in ("-", ""):
                advance(holds)
                continue
            p.button_press(btn)
            advance(holds)
            p.button_release(btn)

    if args.frames:
        advance(args.frames)

    print(f"after {n} frames: frame={p.frame_count} pos={pos(p)} vram={vram_hash(p):016X} "
          f"lcdc={p.memory[0xFF40]:02X} wy={p.memory[0xFF4A]} wx={p.memory[0xFF4B]}")
    if args.dump:
        save_ppm(p, f"{args.dump}_{n:05d}.ppm")
        print(f"dumped to {args.dump}_*.ppm")
    if w:
        w.flush()
        w.close()
        print(f"reglog -> {args.reglog}")
    p.stop()


if __name__ == "__main__":
    main()
