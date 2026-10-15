#!/usr/bin/env python3
"""gbfs の移動履歴 (trace JSONL) から地図 PNG を作り、FIFO に書き続ける。

配信のオーバーレイ用。ffmpeg の image2pipe 入力に流し込む。

使い方:
    gbfs-map-live.py [trace.jsonl] [out-fifo|-] [interval_sec]

- 固定ウィンドウ内だけを描くので、探索が進んでも見た目が飛ばない
- 白: 訪問済み / 赤: 現在地 / 青: 壁(blocked が記録された方向の隣マス) / 暗: 未訪問
- trace の内容が変わったときだけ描き直して 1 枚書く (変化がなければ FIFO も静か)
- ignored (戦闘・メニュー中の入力) は壁ではないので描かない

trace を読めない間も待ち続けるので、起動順は問わない。
"""
import json
import os
import struct
import sys
import time
import zlib

TRACE = sys.argv[1] if len(sys.argv) > 1 else "/tmp/gbfs-trace.jsonl"
OUT = sys.argv[2] if len(sys.argv) > 2 else "-"
INTERVAL = float(sys.argv[3]) if len(sys.argv) > 3 else 1.0

# 表示ウィンドウ (Route 29 の探索範囲を余裕をもって覆う)。変更しても座標系は同じ。
X0, X1, Y0, Y1 = 10, 70, 5, 25
SCALE = 5                      # 1 マス = 5 px
W = (X1 - X0 + 1) * SCALE
H = (Y1 - Y0 + 1) * SCALE
INSET = 1                      # マス間に 2px の隙間を作って格子に見せる

BG = (14, 14, 20)
VISITED = (222, 222, 230)
CURRENT = (235, 70, 60)
WALL = (80, 120, 210)

DIRS = {"up": (0, -1), "down": (0, 1), "left": (-1, 0), "right": (1, 0)}


def encode_png(w, h, px):
    """px: bytearray(w*h*3) の RGB を PNG バイト列にする (Pillow 不要)。"""
    stride = w * 3
    raw = bytearray()
    for y in range(h):
        raw.append(0)                       # フィルタタイプ 0 (None)
        raw += px[y * stride:(y + 1) * stride]

    def chunk(tag, data):
        body = tag + data
        return struct.pack(">I", len(data)) + body + struct.pack(">I", zlib.crc32(body) & 0xFFFFFFFF)

    ihdr = struct.pack(">IIBBBBB", w, h, 8, 2, 0, 0, 0)   # 8bit RGB
    return (b"\x89PNG\r\n\x1a\n"
            + chunk(b"IHDR", ihdr)
            + chunk(b"IDAT", zlib.compress(bytes(raw), 6))
            + chunk(b"IEND", b""))


def read_trace():
    """trace を読み、(訪問マス, 壁, 現在地) を返す。壊れた行は黙って飛ばす。"""
    visited = set()
    walls = set()
    current = None
    try:
        with open(TRACE) as f:
            for line in f:
                line = line.strip()
                if not line.startswith("{"):
                    continue
                try:
                    d = json.loads(line)
                except ValueError:
                    continue
                x, y = d.get("x", 0), d.get("y", 0)
                if x == 0 and y == 0:      # マップ遷移中の無効値
                    continue
                visited.add((x, y))
                current = (x, y)
                note = d.get("note", "")
                if "walk" in note and "blocked" in note and "ignored" not in note:
                    parts = note.split()
                    if len(parts) >= 2 and parts[1] in DIRS:
                        walls.add(((x, y), parts[1]))
    except FileNotFoundError:
        pass
    return visited, walls, current


def render(visited, walls, current):
    buf = bytearray(W * H * 3)
    for i in range(0, len(buf), 3):
        buf[i], buf[i + 1], buf[i + 2] = BG

    def put(cx, cy, color):
        gx, gy = cx - X0, cy - Y0
        if not (0 <= gx <= X1 - X0 and 0 <= gy <= Y1 - Y0):
            return
        px0, py0 = gx * SCALE + INSET, gy * SCALE + INSET
        for yy in range(py0, py0 + SCALE - 2 * INSET):
            if not (0 <= yy < H):
                continue
            row = yy * W * 3
            for xx in range(px0, px0 + SCALE - 2 * INSET):
                if not (0 <= xx < W):
                    continue
                o = row + xx * 3
                buf[o], buf[o + 1], buf[o + 2] = color

    for (x, y) in visited:
        put(x, y, VISITED)
    for ((x, y), di) in walls:
        dx, dy = DIRS[di]
        put(x + dx, y + dy, WALL)
    if current:
        put(current[0], current[1], CURRENT)
    return encode_png(W, H, buf)


def main():
    out = None
    if OUT != "-":
        # FIFO は O_RDWR で開く。O_WRONLY だと「読み手が現れるまで」open がブロックし、
        # ffmpeg 側の O_RDONLY と噛み合わずデッドロックする (音声 FIFO と同じ理由)。
        fd = os.open(OUT, os.O_RDWR)
        out = os.fdopen(fd, "wb", buffering=0)
    last = None
    while True:
        visited, walls, current = read_trace()
        # 変化がなくても毎回書く。ffmpeg の image2pipe は 1 フレーム = 1 秒の pts を
        # 刻むので、書いた回数と実時間を合わせておくと overlay の同期が安定する。
        data = render(visited, walls, current)
        try:
            if out is not None:
                out.write(data)
            else:
                sys.stdout.buffer.write(data)
                sys.stdout.buffer.flush()
        except (BrokenPipeError, OSError):
            return
        last = (len(visited), len(walls), current)
        time.sleep(INTERVAL)


if __name__ == "__main__":
    main()
