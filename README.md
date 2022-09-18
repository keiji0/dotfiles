# dotfiles

## セットアップ

### インストール

```sh
git clone git@github.com:keiji0/dotfiles.git && cd dotfiles # 配置したディレクトリが起点となる
./install
```

### メイン構成を設定

セットアップ対象のメイン構成の作成し設定します。

```
dotconfig set-main main-personal # 個人用の設定をメイン構成に設定
```

追加のメイン構成が必要な場合は`config/main-XXX`と構成を追加します。

### セットアップを実行

メイン構成に従ってソフトウェアをインストール/設定が実行されます。

```sh
dotconfig setup main
```

## 構成について

構成とはプログラムの設定やインストーラーなどが保存されているディレクトリになります。
`$DOTDIR/config`配下にディレクトリを作ることで新たな構成を追加することができます。

### dotconfig

`dotconfig`は構成を適用してくれるコマンドになります。
また、構成を作る上でヘルパー機能を提供されるので構成内で使用したりします。