#!/bin/bash

set -e

cd "$(dirname "$0")"

# デフォルトファイルの読み込み
# for i in $(find defaults -perm +0111 -type f); do
find defaults -perm +0111 -type f | while IFS= read -r -d '' i; do
    echo "default $(basename "$i")"
    sh "$i"
done