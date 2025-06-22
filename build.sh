#!/bin/bash

# 色付きの出力用
GREEN='\033[0;32m'
RED='\033[0;31m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# エラー終了関数
die() {
  echo -e "${RED}エラー:${NC} $*" >&2
  exit 1
}

# 成功メッセージ
success() {
  echo -e "${GREEN}成功:${NC} $*"
}

# 情報メッセージ
info() {
  echo -e "${BLUE}情報:${NC} $*"
}

# コンパイラーをビルド
build() {
  info "コンパイラーのビルド中..."
  stack build || die "コンパイラーのビルドに失敗しました"
  success "コンパイラーのビルドが完了しました"
}

# .mmt ファイルを JavaScript にコンパイル
compile() {
  local input=$1

  if [ ! -f "$input" ]; then
    die "ファイル '$input' が見つかりません"
  fi

  local output="dist/js/$(basename "${input%.mmt}.js")"

  info "コンパイル中: $input -> $output"
  mkdir -p dist/js

  stack exec -- memento-proto "$input" || die "$input のコンパイルに失敗しました"

  if [ -f "$output" ]; then
    success "$output にコンパイルされました"
  else
    die "出力ファイル $output が生成されませんでした"
  fi
}

# コンパイル済みの JavaScript ファイルを実行
run() {
  local input=$1
  local output="dist/js/$(basename "${input%.mmt}.js")"

  if [ ! -f "$output" ]; then
    die "実行ファイル '$output' が見つかりません。先にコンパイルしてください"
  fi

  info "実行中: $output"
  node "$output" || die "実行に失敗しました"
  success "実行が完了しました"
}

# ビルドしてからコンパイルして実行
build_and_run() {
  local input=$1
  build && compile "$input" && run "$input"
}

# すべての例をコンパイル
compile_all_examples() {
  info "examples ディレクトリのすべての .mmt ファイルをコンパイル中..."

  local examples_dir="examples"
  mkdir -p dist/js

  local count=0
  local failed=0

  for file in "$examples_dir"/**/*.mmt; do
    if [ -f "$file" ]; then
      info "コンパイル中: $file"
      if stack exec -- memento-proto "$file"; then
        success "$file のコンパイルが完了しました"
        ((count++))
      else
        echo -e "${RED}失敗:${NC} $file のコンパイルに失敗しました" >&2
        ((failed++))
      fi
    fi
  done

  if [ $count -eq 0 ] && [ $failed -eq 0 ]; then
    info "コンパイルするファイルが見つかりませんでした"
  else
    if [ $failed -eq 0 ]; then
      success "すべての例のコンパイルが完了しました ($count ファイル)"
    else
      die "$failed ファイルのコンパイルに失敗しました ($count ファイルは成功)"
    fi
  fi
}

case "$1" in
  "build")
    build
    ;;
  "compile")
    if [ -z "$2" ]; then
      compile_all_examples
    else
    compile "$2"
    fi
    ;;
  "all")
    compile_all_examples
    ;;
  "run")
    if [ -z "$2" ]; then
      die "実行するファイル名を指定してください"
    else
    run "$2"
    fi
    ;;
  "clean")
    info "dist/js ディレクトリをクリーンアップしています..."
    rm -rf dist/js
    success "クリーンアップが完了しました"
    ;;
  *)
    if [ -n "$1" ]; then
      build_and_run "$1"
    else
      echo "使用法: ./build.sh [コマンド] [ファイル]"
      echo ""
      echo "コマンド:"
      echo "  build              コンパイラーをビルド"
      echo "  compile [file.mmt] 指定した .mmt ファイルをコンパイル (ファイル名省略時は全ての例をコンパイル)"
      echo "  all                すべての例をコンパイル"
      echo "  run file.mmt       コンパイル済みの JavaScript を実行"
      echo "  clean              dist/js ディレクトリをクリーンアップ"
      echo ""
      echo "または:"
      echo "  ./build.sh file.mmt  ビルド、コンパイル、実行を連続して行う"
    fi
    ;;
esac
