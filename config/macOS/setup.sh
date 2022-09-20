#!/bin/bash

set -e

cd "$(dirname "$0")"

# デフォルトファイルの読み込み
find defaults -perm +0111 -type f -print0 | while IFS= read -r -d '' i; do
    echo "default $(basename "$i")"
    sh "$i"
done
