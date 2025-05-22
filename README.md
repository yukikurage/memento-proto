# Memento言語プロトタイプ

Mementoは、型安全な関数型プログラミング言語のプロトタイプです。シンプルな構文と型推論を備え、JavaScriptにコンパイルされます。

## 特徴

- 静的型付け（`number`型と`bool`型）
- 型注釈の省略可能（型推論）
- ラムダ式による関数定義
- パイプライン演算子（`|>`）による関数適用
- JavaScriptへのコンパイル

## 必要なもの

- [Stack](https://docs.haskellstack.org/en/stable/)（Haskellビルドツール）
- [Node.js](https://nodejs.org/)（コンパイル結果の実行用）

## インストール

```bash
git clone https://github.com/yukikurage/memento-proto.git
cd memento-proto
stack build
```

## 使い方

### ビルドスクリプト

```bash
# コンパイラーのビルド
./build.sh build

# .mmtファイルをコンパイル
./build.sh compile examples/inference.mmt

# すべての例をコンパイル
./build.sh all

# コンパイル済みのJavaScriptを実行
./build.sh run examples/inference.mmt

# ビルド・コンパイル・実行を連続して行う
./build.sh examples/inference.mmt

# 出力ディレクトリをクリア
./build.sh clean
```

## 構文例

### 変数定義

```
// 型注釈あり
42 |> x : number;

// 型推論（numberと推論される）
10 |> y;
```

### ラムダ式（関数）定義

```
// 型注釈あり
(x : number; x + 1) |> increment;

// 型推論（numberを引数に取る関数と推論される）
(n; n * 2) |> double;
```

### 関数適用

```
// パイプライン演算子による適用
5 |> increment;

// 複数の関数を連結
5 |> increment |> double;
```

### 条件分岐

```
if (x < 10) then (x + 1) else (x * 2)
```

## 出力

コンパイルされたJavaScriptファイルは`dist/js`ディレクトリに出力されます。
