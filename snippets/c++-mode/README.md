# C++ Snippets Index

`snippets/c++-mode/` 配下 83 個の yasnippet 一覧。コンテスト本番、問題に詰まったときの参照用。
左カラムは **yasnippet trigger key** (入力後 `TAB` で展開)。複雑度は概算。

---

## 0. テンプレ・補助

| key | 内容 |
|---|---|
| `main` | `int main(argc, argv)` の雛形 |
| `macro` | マクロ集 (入出力, デバッグ, ループ短縮) |
| `macrov2` | マクロ v2 (C++20 ranges 対応) |
| `be` | `v.begin(), v.end()` |
| `yesno` | "Yes" / "No" 出力 |
| `removeif` | `std::remove_if` テンプレ |
| `unique` | `std::unique` + `erase` |
| `perm` | `next_permutation` 全列挙 |
| `randomstr` | ランダム文字列生成 |
| `xorshift` | xorshift 乱数 |

---

## 1. グラフ

### 1.1 探索・連結性

| key | アルゴリズム | 計算量 | 用途 |
|---|---|---|---|
| `tsort` | トポロジカルソート | O(V+E) | DAG 順序付け、閉路検出 |
| `scc` | 強連結成分分解 | O(V+E) | 有向グラフを DAG 化 |
| `lowlink` | 橋・関節点 | O(V+E) | 2 辺/2 頂点連結成分分解 |
| `cowgame` | 差分制約系 | Bellman-Ford | `d[j]-d[i]<=c` 形式の不等式を最大化 |

### 1.2 最短路・最小全域木

| key | アルゴリズム | 計算量 | 用途 |
|---|---|---|---|
| `kruskal` | Kruskal MST | O(E log E) | 最小全域木 |

(Dijkstra/BFS/Floyd-Warshall は素で書く想定でスニペット無し)

### 1.3 フロー・マッチング

| key | アルゴリズム | 計算量 | 用途 |
|---|---|---|---|
| `bimatch` | 二部マッチング (Hungarian DFS) | O(VE) | 簡単な二部マッチング |
| `hopcroftkarp` | Hopcroft-Karp | O(E√V) | 大きい二部マッチング |
| `dinic` | Dinic 最大流 | O(V²E) | 最大流、二部マッチング |
| `maxflow` | Ford-Fulkerson 最大流 | O(F·E) | 小さめ最大流、min-cut |
| `mincostmaxflow` | 最小費用最大流 | SPFA 反復 | 割当問題、輸送問題 |

---

## 2. 木

### 2.1 基本処理

| key | アルゴリズム | 計算量 | 用途 |
|---|---|---|---|
| `LCA` | 最小共通祖先 (Doubling) | <O(N log N), O(log N)> | パス長, 祖先判定 |
| `eulertour` | オイラーツアー | O(N) | 部分木クエリを区間クエリに |
| `subtreequery` | 部分木クエリ補助 (preorder/postorder) | O(N) | `eulertour` の使い分け版 |
| `hld` | Heavy-Light Decomposition | <O(N), O(log²N)> | パス上の区間更新/クエリ |
| `auxiliarytree` | 仮想木 (Virtual Tree) | O(\|X\| log N) | 指定頂点集合のみ圧縮した木 |

### 2.2 動的・分解

| key | アルゴリズム | 計算量 | 用途 |
|---|---|---|---|
| `linkcuttree` | Link-Cut Tree | O(log N) amortized | 森の動的 link/cut + パスクエリ |
| `treecentroid` | 木の重心分解 | O(N log N) | パス系オフラインクエリの分解 |
| `rerooting` | 全方位木 DP | O(N) | 全頂点を根とした答えを一気に |
| `treeeccentricity` | 離心率 / 直径 | O(N) | 木の直径・各頂点の最遠距離 |
| `cycledecomposition` | 巡回置換分解 | O(N) | 置換 → サイクル列, 最小 swap |

---

## 3. 区間クエリ

### 3.1 接頭辞和

| key | 用途 | クエリ |
|---|---|---|
| `prefixsum` | 1D | O(1) 区間和 |
| `prefixsum2d` | 2D | O(1) 矩形和 |
| `prefixsum3d` | 3D | O(1) 直方体和 |

### 3.2 BIT / セグメント木

| key | 構造 | 用途 |
|---|---|---|
| `bit` | Fenwick / BIT | 点更新 + 接頭辞和 |
| `segtree` | セグ木 | 点更新 + 区間モノイド |
| `segtreev2` | セグ木 (非再帰) | 同上, 高速 |
| `lazysegtree` | 遅延セグ木 | 区間更新 + 区間クエリ |
| `lazysegtreev2` | 遅延セグ木 (改良) | 同上 |
| `atcoderlazysegtree` | AtCoder Library 使用例 | ACL 利用テンプレ |
| `dualsegtree` | 双対セグ木 | 区間更新 + 点取得 |
| `dynamicsegtree` | 動的セグ木 | 座標が疎な場合 (hash map ベース) |
| `rollbacksegtree` | ロールバック付き | 永続不要だが巻き戻したい時 |
| `segtreebeats` | Segment Tree Beats | 区間 chmin/chmax/add/sum |
| `mergesorttree` | マージソート木 | 区間内 K 番目, 値帯統計 |

### 3.3 平方分割系

| key | アルゴリズム | 計算量 | 用途 |
|---|---|---|---|
| `sqrtdecomposition` | 平方分割 | O(√N)/op | セグ木で書けない汎用更新 |
| `mos` | Mo's algorithm | O((N+Q)√N) | オフライン区間クエリ |

---

## 4. 文字列

| key | アルゴリズム | 計算量 | 用途 |
|---|---|---|---|
| `zalgorithm` | Z アルゴリズム | O(N) | 全接尾辞 LCP, 周期検出 |
| `suffixarray` | Suffix Array + LCP | O(N log N) 構築 | 部分文字列比較・出現数 |
| `suffixautomaton` | Suffix Automaton | O(N) | 異なる部分文字列数, 出現回数 |
| `prefixtree` | Trie | O(\|S\|) 挿入/検索 | 単語辞書, XOR 最大 (二進 Trie) |
| `ahocorasick` | Aho-Corasick | O(N+M+Z) | 複数パターン同時マッチ |

---

## 5. データ構造

### 5.1 Union-Find 系

| key | バリアント | 用途 |
|---|---|---|
| `unionfind` | 通常 | 連結性, MST 構築 |
| `weightedunionfind` | 重み付き | 差/比の制約管理, 矛盾検出 |
| `rollbackunionfind` | ロールバック可能 | オフライン削除, 永続風 |
| `persistentunionfind` | 永続 | 過去のバージョンを参照 |

### 5.2 ヒープ・木

| key | データ構造 | 用途 |
|---|---|---|
| `skewheap` | Skew Heap | マージ可能 priority queue |
| `splaytree` | Splay Tree (順序統計) | rank, k 番目要素 |
| `orderedset` | GNU pbds ordered_set | `find_by_order`, `order_of_key` |
| `cartesiantree` | Cartesian Tree | スタックで O(N) 構築, 範囲最小区間 |
| `slidemin` | スライド最小値 | deque で O(N), 区間幅固定の min/max |

### 5.3 集合演算

| key | アルゴリズム | 用途 |
|---|---|---|
| `xorbasis` | XOR 基底 | XOR 線形独立な部分集合抽出 |
| `bitsubset` | ビット部分集合列挙 | `for (sub = S; sub; sub = (sub-1)&S)` |

---

## 6. 数論・数式

| key | 内容 | 備考 |
|---|---|---|
| `modint` | ModInt 自動演算 | mod を型に閉じ込める |
| `modinverse` | mod 逆元 | 拡張 Euclid / フェルマー |
| `arbitrarymodbinomialcoefficient` | 合成数 mod での二項係数 | CRT + Lucas |
| `prime` | エラトステネス + 区間篩 | 素数列挙, 素因数分解 |
| `math` | 数学関数集 | 組合せ, CRT, オイラー関数 |
| `fraction` | 有理数比較 | オーバーフロー安全 |
| `int128` | `__int128` 入出力 | 64bit 越えの積 |
| `bigint` | Boost 任意精度整数 | bigint 必要時 (重い) |
| `matrix` | 行列 (積, 累乗) | 線形漸化式の高速化 |
| `grundy` | Grundy 数 | Nim 系ゲームの先手必勝判定 |

---

## 7. DP・最適化

| key | アルゴリズム | 用途 |
|---|---|---|
| `lis` | LIS O(N log N) | 最長増加部分列 |
| `cht` | Convex Hull Trick | DP 式 `dp[i] = min(a_j·x + b_j)` |
| `slopetrick` | Slope Trick | 分区線形凸関数の min を維持 |
| `trisearch` | 三分探索 | 単峰関数の極値 (整数/実数) |
| `maxrectangle` | ヒストグラム最大長方形 | スタック O(N), 2D 最大矩形にも |

---

## 8. 幾何

| key | 内容 |
|---|---|
| `geo` | 複素数ベース幾何 (円判定, 交点, 偏角ソート) |

---

## 9. グリッド・補助

| key | 内容 |
|---|---|
| `didj` | 4/8 方向走査 + 範囲チェックループ |
| `grid2d` | 2D 配列の回転 / 層 |
| `ring` | 環状配置の 2 点間距離 `{逆回り回数, 順回り回数}` |
| `doubling` | ダブリング (固定 N) | `T[i][k] = 2^k ステップ後の遷移先` |
| `doublingv2` | ダブリング (テンプレート, 要約付き) | 遷移 + モノイド要約をまとめて持つ |

---

## 困ったときの逆引き

### 「制約 N が小さい (N ≤ 20)」
→ bit DP, `perm`, `bitsubset`, 全探索

### 「制約 N が中 (N ≤ 5000)」
→ O(N²) DP, `bimatch`, `lis`

### 「制約 N が大 (N ≤ 2·10⁵)」
→ セグ木系, `bit`, `sqrtdecomposition`, `mos`, `LCA`, `hld`

### 「区間更新 + 区間取得」
→ `lazysegtree` / `lazysegtreev2` / `atcoderlazysegtree`

### 「区間 chmin/chmax/add/sum が混在」
→ `segtreebeats`

### 「座標が大きく疎」
→ `dynamicsegtree`, 座圧 + `segtree`

### 「複数パターン文字列検索」
→ `ahocorasick`

### 「部分文字列数を数える」
→ `suffixautomaton`

### 「全頂点を根にした答え」
→ `rerooting`

### 「木上のパスクエリ」
→ `hld` + 各種セグ木

### 「部分木クエリ」
→ `eulertour` + `bit` or `segtree`

### 「複数ソース間の最大流/最小カット」
→ `dinic` / `maxflow`

### 「割当問題、重み付き二部マッチング」
→ `mincostmaxflow`

### 「ゲーム理論で勝者判定」
→ `grundy`

### 「DP が `dp[i] = min(a_j·x_i + b_j)` の形」
→ `cht`

### 「DP が分区線形凸で和の最小化」
→ `slopetrick`

### 「差分制約 (`x_j - x_i ≤ c`)」
→ `cowgame`

### 「オフラインで区間クエリを Q 個まとめて」
→ `mos`

### 「過去の状態を参照したい」
→ `persistentunionfind`, `rollbacksegtree`

### 「単峰関数の極値」
→ `trisearch`

### 「大きな数 (64bit 越え)」
→ `int128`, それでも足りなければ `bigint`

### 「順位/k 番目クエリ」
→ `orderedset`, `splaytree`

### 「`(x+y) mod N` 上の距離」
→ `ring`

### 「グリッドの隣接 4/8 マス」
→ `didj`

---

## メモ

- `trinaly_search` の trigger は `trisearch` (短い方)。
- `int128` の trigger は `gnu128bitint`。
- `lca` の trigger は **大文字** `LCA`。
