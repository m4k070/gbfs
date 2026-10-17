#!/usr/bin/env python3
"""レジスタログ (gbfs_seq.fsx --reglog / pyboy_probe.py --reglog) を層ごとに比較する。

列: 0=n 1=LCDC 2=STAT 3=SCY 4=SCX 5=LY 6=LYC 7=WY 8=WX 9=x 10=y 11=dir 12=vram
LY は 1 フレーム内のカウンタなので比較から外す (サンプリング位置で変わる)。
"""
import sys


def load(path):
    return [l.split() for l in open(path) if l.strip()]


def main():
    gp = sys.argv[1] if len(sys.argv) > 1 else "/tmp/g_rt.log"
    pp = sys.argv[2] if len(sys.argv) > 2 else "/tmp/p_rt.log"
    g, pb = load(gp), load(pp)
    n = min(len(g), len(pb))
    print(f"行数 gbfs={len(g)} pyboy={len(pb)} 比較={n}")

    cols = {1: "LCDC", 3: "SCY", 4: "SCX", 7: "WY", 8: "WX", 12: "VRAM"}
    first = {}
    for i in range(n):
        for c, name in cols.items():
            if g[i][c] != pb[i][c] and name not in first:
                first[name] = (g[i][0], g[i][c], pb[i][c])
    print("=== 各指標が最初に食い違うフレーム (frame, gbfs, pyboy) ===")
    for name in cols.values():
        print(f"  {name:5s}: {first.get(name, '一致')}")

    def win(row):
        return bool(int(row[1], 16) & 0x20)

    def runs(rows):
        out, start = [], None
        for i in range(n):
            v = win(rows[i])
            if v and start is None:
                start = i
            if not v and start is not None:
                out.append((start, i - 1))
                start = None
        if start is not None:
            out.append((start, n - 1))
        return out

    print("=== WIN 有効期間 (フレーム番号, 先頭 8 件) ===")
    print("  gbfs :", runs(g)[:8])
    print("  pyboy:", runs(pb)[:8])

    d = [i for i in range(n) if g[i][1] != pb[i][1]]
    print(f"LCDC 不一致フレーム数: {len(d)} / {n}")
    print("  先頭 10:", [(g[i][0], g[i][1], pb[i][1]) for i in d[:10]])

    w = [i for i in range(n) if g[i][7] != pb[i][7] or g[i][8] != pb[i][8]]
    print(f"WY/WX 不一致フレーム数 (bit7=144 は『画面外』を意味する): {len(w)} / {n}")
    print("  先頭 10:", [(g[i][0], g[i][7], pb[i][7], g[i][8], pb[i][8]) for i in w[:10]])


if __name__ == "__main__":
    main()
