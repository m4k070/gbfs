# gbfs ↔ PyBoy 差分プローブ

自作エミュレータと参照実装を **同じ入力列** で走らせ、状態と描画を突き合わせるための道具。

## なぜ repo にあるか

`~/.hermes/cache/scratch/` は 24 時間で削除される。実際に 2 日空けたらプローブも
対応付け済みの state ペアも消えていた。**再利用する道具はここに置く。**

## 使い方

```bash
# 両方を素の起動から同じ入力列で回す (state ペアを再生成する場合もこれ)
nix develop -c dotnet fsi tools/diffprobe/gbfs_seq.fsx --boot \
    --inputs "-:600" --reglog /tmp/g_rt.log --dump
~/.venvs/pyboy/bin/python tools/diffprobe/pyboy_probe.py --boot \
    --inputs="-:600" --reglog /tmp/p_rt.log --dump /tmp/pyboyseq/p_ --every 5

# 描画の差分 (4 階調に量子化した MAE) と、区間ごとの収束の見方
python tools/diffprobe/compare_frames.py /tmp/gbfsseq /tmp/pyboyseq
python tools/diffprobe/mae_profile.py   /tmp/gbfsseq /tmp/pyboyseq

# レジスタ (LCDC/SCY/SCX/WY/WX) の層ごとの比較
python tools/diffprobe/compare_reglog.py /tmp/g_rt.log /tmp/p_rt.log
```

## 注意

- **必ず前回のダンプを消してから回す** (`rm -rf /tmp/gbfsseq /tmp/pyboyseq`)。
  混ざるとファイル数が食い違い、比較が無効になる
- `pyboy_probe.py` の `--inputs` は値が `-` で始まるので **`--inputs="-:600"` と `=` 形式**で渡す
- 長い fsi 実行は**前景 + ファイルへリダイレクト**。背景 terminal ジョブは出力が消える
- 画面の色差は **RGB555→888 の展開差** が主: こちらは `(v<<3)|(v>>2)` (標準)、
  PyBoy は `v<<3` (暗い)。**バグではない**
- パレット RAM ($FF68-$FF6B) は両者 1 バイトも違わない (実測)
