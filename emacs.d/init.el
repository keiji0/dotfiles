
;; * 基本的な設定
;; ディレクトリの位置や設定や環境変数の設定を行う

(require 'cl)

(defun concat-path (elm &rest elms)
  "OS非依存なパスを構築する"
  (if (not elms)
      (expand-file-name elm)
    (apply 'concat-path
           (expand-file-name (car elms) elm)
           (cdr elms))))

(defun emacs-home (&rest elms)
  ".emacs.dが設定されているパスを生成する"
  (apply 'concat-path user-emacs-directory elms))

(defun emacs-var-dir (&rest elms)
  "elispが動的に生成するデータなどを保管するパスを生成する"
  (apply 'emacs-home "var" elms))

(defun emacs-share-dir (&rest elms)
  "静的でelispから参照されるだけのデータを保管するパスを生成する"
  (apply 'emacs-home "share" elms))

(defun emacs-libs-dir (&rest elms)
  "パッケージ管理されない独自elispを保管するパスを生成する"
  (apply 'emacs-home "elisp" elms))

(defun emacs-local-dir (&rest elms)
  "プラットフォームに依存するプログラムやデータを補完するパスを生成する"
  (apply 'emacs-home "local" elms))

;; elisp配下
(loop for f in (directory-files (emacs-libs-dir) t)
      when (and (file-directory-p f)
                (not (member (file-name-nondirectory f) '("." ".."))))
      do (add-to-list 'load-path f))
(add-to-list 'load-path (emacs-libs-dir))
(add-to-list 'load-path (emacs-home "themes"))
(add-to-list 'custom-theme-load-path (emacs-home "themes"))

;; 環境変数のロード
(when (require 'exec-path-from-shell)
  (setq exec-path-from-shell-variables '("PATH" "MANPATH"));
  (exec-path-from-shell-initialize)
  )


;; * 必須関数や互換性を維持するための関数定義

;; Emacs 24.4以前は存在しないため自力で定義する
(unless (fboundp 'with-eval-after-load)
  (defmacro with-eval-after-load (file &rest body)
    "eval-after-loadのprogn不要な便利マクロ"
    `(eval-after-load ,file
       `(funcall (function ,(lambda () ,@body))))))


;; * デフォルトEmacsの設定
;; このセクションは外部依存がなく最低限の挙動を設定する

;; エンコーディング関連
(progn
  (set-language-environment 'utf-8)
  (prefer-coding-system 'utf-8)
  )

;; バックアップの設定
(progn
  ;; バックアップ(<file>~)の無効化
  (setq make-backup-files nil)
  ;; 自動保存ファイルの無効化
  (setq auto-save-default nil)
  ;; ファイルの自動保存は無効化
  (setq auto-save-list-file-prefix nil)
  )

;; キーマップの設定
(progn
  ;; macOSオンリー
  (when (eq system-type 'darwin)
    ;; commandキーをmetaキーとして使う
    (setq mac-command-modifier 'meta)
    )
  ;; 実行のyes/or入力がかったるいのでy/sに変更
  (fset 'yes-or-no-p 'y-or-n-p)
  ;; バックスペースに変更
  (global-set-key (kbd "C-h") 'delete-backward-char)
  ;; ヘルプは地味に使うので割り当てる
  (global-set-key (kbd "M-?") 'help-for-help)
  )

;; エディタとしての設定
(progn
  ;; タブ幅の設定
  (setq default-tab-width 4)
  ;; タブをスペースで扱う
  (setq-default indent-tabs-mode nil)
  ;; 自動インデント
  (electric-indent-mode)
  ;; カーソル付近のファイルパスを開く
  (ffap-bindings)
  ;; 行末の無駄な空白を保存時に削除
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  )

;; サーバーの起動
(when (require 'server)
  (unless (server-running-p)
    (server-start)))

;; 見かけの設定
(progn
  ;; ツールバーは表示しない
  (tool-bar-mode 0)
  ;; emacs起動時のメッセージを非表示にする
  (setq inhibit-startup-message t)
  ;; ベルをならさない
  (setq ring-bell-function 'ignore)
  ;; 画面へのスクロールが一気に移動しないようにする
  (setq scroll-conservatively 35)
  (setq scroll-step 1)
  ;; スクロールされるまでの画面端とカーソルのマージンを指定する
  (make-variable-buffer-local 'scroll-margin)
  (setq-default scroll-margin 3)
  ;; 行溢れした文字は折り返さずに表示する
  (setq-default truncate-lines t)
  (setq runcate-partial-width-windows t)
  ;; 現在行をハイライト
  (make-variable-buffer-local 'global-hl-line-mode)
  (global-hl-line-mode)
  (setq-default global-hl-line-mode t)
  ;; 対応するカッコをハイライトする
  (show-paren-mode t)
  (setq show-paren-style 'parenthesis)
  ;; ファイル保存時に最終行に改行を入れる
  (setq require-final-newline t)

  ;; GUI時の設定
  (when window-system
    ;; 横幅のあるディスプレイなのでスクロールバーを表示する
    (toggle-scroll-bar nil)
    ;; フリンジ(ウィンドウの縦ライン)の幅を調整する
    (fringe-mode (cons 1 1))
    )
  )


;; * EELPA(macs Lisp Package Archive) - パッケージの設定
;; https://www.emacswiki.org/emacs/ELPA

(when (require 'package)
  ;; パッケージアーカイブを追加する
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (package-initialize)
  ;;(package-refresh-contents)
  (package-install 'evil)
  (package-install 'evil-leader)
  (package-install 'powerline)
  (package-install 'session)
  (package-install 'go-mode)
  (package-install 'go-autocomplete)
  (package-install 'undo-tree)
  (package-install 'multi-term)
  (package-install 'neotree)
  (package-install 'spacemacs-theme)
  (package-install 'spaceline)
  (package-install 'page-break-lines)
  (package-install 'smartparens)
  (package-install 'company)
  (package-install 'irony)
  (package-install 'irony-eldoc)
  (package-install 'flycheck-irony)
  (package-install 'company-irony)
  (package-install 'persp-mode)
  (package-install 'projectile)
  (package-install 'helm-projectile)
  (package-install 'magit)
  (package-install 'yasnippet)
  (package-install 'ido-vertical-mode)
  (package-install 'comment-dwim-2)
  )

;; 必須ライブラリ
(defvar sdb-directory (emacs-var-dir "sdb"))
(require 'sdb)


;; * Themeや色に関する設定

;; フォントの設定
(when window-system
  (cond ((eq system-type 'darwin)
         ;; * Source Han Code JP
         ;; https://github.com/adobe-fonts/source-han-code-jp
         ;; AdobeのSource Code Proをベースにした日本語用のフォント
         ;; Ricty(Inconsolata)よりセリフがすくなくすっきりとみやすい
         ;; あと日本語フォントとの統一感がすごくよい
         (set-default-font "Source Han Code JP 11")
         (setq-default line-spacing nil)
         )))

;; 主要テーマを設定
;; https://github.com/nashamri/spacemacs-theme
(setq spacemacs-theme-comment-bg nil) ; コメントの背景カラーを無効にする
(load-theme 'spacemacs-dark t)

;; * Powerline
;; https://github.com/milkypostman/powerline
(when (require 'powerline)
  ;; モードラインが若干乱れて表示されるのを直す
  ;; https://github.com/milkypostman/powerline/issues/54
  (setq ns-use-srgb-colorspace nil)
  ;; モードラインを形状を変える
  (setq powerline-default-separator 'bar)
  ;; モードラインの設定
  (when (require 'spaceline-config)
    (spaceline-spacemacs-theme))
  ;; モードラインに表示したいくないマイナーモードを指定
  (add-hook 'after-init-hook
            (lambda ()
              (mapc (lambda (mode)
                      (setq minor-mode-alist
                            (cons (list mode "") (assq-delete-all mode minor-mode-alist))))
                    '(undo-tree-mode
                      helm-mode
                      highlight-symbol-mode
                      smartparens-mode
                      eldoc-mode
                      company-mode
                      projectile-mode)
                    )))
  )

;; 改ページ(^L)をわかりやすく表示
;; https://github.com/purcell/page-break-lines
(when (require 'page-break-lines)
  (global-page-break-lines-mode))

;; フレーム状態の復帰
(progn
  (add-hook 'sdb-before-commit-hook
            (lambda ()
              (sdb-set "frame-parameters"
                       (mapcar (lambda (x) (assoc x (frame-parameters)))
                               '(width height top left)))))
  (add-hook 'after-init-hook
            (lambda ()
              (dolist (kv (sdb-get "frame-parameters" nil))
                (add-to-list 'initial-frame-alist kv)))))


;; * Session - セッション保存の設定
;; http://www.emacswiki.org/emacs/session.el

(when (require 'session)
  (add-hook 'after-init-hook 'session-initialize)

  ;; セッション情報を保存するディレクトリ
  (setq session-save-file (emacs-var-dir "session.el"))
  ;; ファイルを前回保存した時ではなく、閉じた時のカーソル位置を記録する
  (setq session-undo-check -1)
  ;; 履歴の保存件数
  (setq history-length 3000)
  (setq session-initialize '(de-saveplace session menus places))
  ;; シンボリックファイルであっても実体パスを参照しない
  (setq session-use-truenames nil)

  ;; セッション管理する情報
  (setq session-globals-include
        '(
          ;; キルリング
          (kill-ring 100)
          ;; カーソル位置
          (session-file-alist 100 t)
          (session-globals-max-size 100000)
          ;; 開いたファイルのパス
          (file-name-history 300))
        )
  )


;; * Recent - 開いたファイル一覧を保存
;; https://www.emacswiki.org/emacs/RecentFiles

(when (require 'recentf)
  (setq recentf-save-file (emacs-var-dir "recentf.el"))
  (recentf-mode 1)
  )


;; * Evil
;; https://bitbucket.org/lyro/evil/wiki/Home

;; evilでLeaderキーを使用するためのライブラリを読み込む
(when (require 'evil-leader)
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  )

;; Evil用のキーマップを定義
(when (require 'evil)
  (evil-mode 1)

  ;; *や#で単語単位ではなくシンボル単位で検索する
  (setq-default evil-symbol-word-search t)

  ;; グローバルモード
  (when (eq system-type 'darwin)
    (global-set-key (kbd "M-w") 'kill-buffer)
    )

  ;; モーションモード
  (progn
    (define-key evil-motion-state-map (kbd ";") 'evil-ex)
    (define-key evil-motion-state-map (kbd "C-i") 'evil-jump-item)
    (define-key evil-motion-state-map (kbd "RET") nil)
    )

  ;; ノーマルモード
  (progn
    ;; persp
    (define-key evil-normal-state-map (kbd "C-t") nil)
    (define-key evil-normal-state-map (kbd "C-t n") 'persp-next)
    (define-key evil-normal-state-map (kbd "C-t p") 'persp-prev)
    (define-key evil-normal-state-map (kbd "C-t s") 'persp-frame-switch)
    (define-key evil-normal-state-map (kbd "C-t c") 'persp-frame-switch)
    (define-key evil-normal-state-map (kbd "C-t r") 'persp-rename)
    (define-key evil-normal-state-map (kbd "C-t k") 'persp-kill)
    )

  ;; インサートモード
  (progn
    (define-key evil-insert-state-map (kbd "C-y") nil)
    (define-key evil-insert-state-map (kbd "C-j") 'newline-and-indent)
    (define-key evil-insert-state-map (kbd "C-k") 'kill-line)
    )

  ;; ミニバッファモード
  (define-key minibuffer-local-map (kbd "C-w") 'evil-delete-backward-word)

  ;; ウィンドウ関連
  (define-key evil-window-map "-" 'evil-window-split)
  (define-key evil-window-map "|" 'evil-window-vsplit)
  (define-key evil-window-map "0" 'delete-window)
  (define-key evil-window-map "1" 'delete-other-windows)

  ;; Leader
  ;; ノーマルモード時にもっともよく利用するキーを設定
  (progn
    ;; ファイル移動に利用する、すでに開いているファイルをバッファとしてアクセスできるのでminiを使う
    (evil-leader/set-key "f" 'helm-mini)
    (evil-leader/set-key "g" 'helm-projectile)
    ;; 普段はhelm-miniで事足りるが、まとまってバッファを整理したりする
    (evil-leader/set-key "b" 'bs-show)
    (evil-leader/set-key "x" 'kill-buffer)
    (evil-leader/set-key "t" 'multi-term)
    (evil-leader/set-key "v" 'magit-status)
    )
  )


;; * persp-mode
;; https://github.com/Bad-ptr/persp-mode.el
;; ワークスペースの管理ができるパッケージ。
;; さらにEmacs内の様々な状態を保存することができる。

(with-eval-after-load "persp-mode-autoloads"
  ;; switch off the animation of restoring window configuration
  (add-hook 'after-init-hook #'(lambda () (persp-mode 1)))
  (setq wg-morph-on nil)

  ;; 保存するディレクトリを変更
  (setq persp-save-dir (emacs-var-dir "persp/"))
  )


;; * Project mager

;; * projectile
;; https://github.com/bbatsov/projectile
(when (require 'projectile)
  (projectile-global-mode)
  (setq projectile-known-projects-file (emacs-var-dir "projectile" "projectile-bookmarks.eld"))
  )


;; * ido

(with-eval-after-load 'ido
  (ido-mode t)
  (ido-vertical-mode 1)
  (setq ido-max-window-height 0.75)
  (setq ido-save-directory-list-file (emacs-var-dir "ido.last"))
  ;;(define-key ido-common-completion-map (kbd "C-w") 'ido-delete-backward-updir)
  )


;; * Helm
;; https://github.com/emacs-helm/helm

(progn
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  )

(with-eval-after-load 'helm
  (define-key helm-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-map (kbd "C-w") 'evil-delete-backward-word)
  )

(with-eval-after-load 'helm-buffers
  (advice-add 'helm-skip-boring-buffers :filter-return 'my-helm-buffers-filter)
  (defun my-helm-buffers-filter (buffer-list)
    "helm-buffers-listから除外したいバッファをフィルタリングする"
    ;; https://github.com/emacs-helm/helm/issues/975#issuecomment-93302364
    (delq nil (mapcar
               (lambda (buffer)
                 (if (eq (with-current-buffer buffer major-mode) 'dired-mode)
                     nil
                   buffer))
               buffer-list)))
  )


;; * Company mode
;; http://company-mode.github.io/
;; 補完機能支援パッケージ、auto-completeと同等の機能を持つが
;; 言語支援系の補完パッケージが多いためこちらを利用することにする。

(when (require 'company)
  (global-company-mode 1)
  ;; 自動補完をしない
  (setq company-idle-delay nil)

  ;; キーバインドの設定
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-w") 'evil-delete-backward-word)
  (define-key company-active-map (kbd "C-s") 'company-filter-candidates)
  (define-key company-active-map (kbd "C-h") nil)
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)

  ;; タブキーで補完開始
  ;; https://github.com/company-mode/company-mode/issues/94
  (progn
    (defun check-expansion ()
      (save-excursion
        (if (looking-at "\\_>") t
          (backward-char 1)
          (if (looking-at "\\.") t
            (backward-char 1)
            (if (looking-at "->") t nil)))))

    (defun do-yas-expand ()
      (let ((yas/fallback-behavior 'return-nil))
        (yas/expand)))

    (defun tab-indent-or-complete ()
      (interactive)
      (if (minibufferp)
          (minibuffer-complete)
        (if (or (and (boundp yas/minor-mode) (not yas/minor-mode))
                (null (do-yas-expand)))
            (if (check-expansion)
                (company-complete-common)
              (indent-for-tab-command)))))

    (global-set-key (kbd "TAB") 'tab-indent-or-complete))
  )


;; * エディタ支援

;; * yasnippet
;; https://github.com/joaotavora/yasnippet
(when (require 'yasnippet)
  ;; 常に有効にする
  (yas-global-mode 1)
  ;; ユーザ定義のスニペットの置き場所の追加
  (add-to-list 'yas-snippet-dirs (emacs-share-dir "snippets"))
  (setq yas-prompt-functions '(yas-ido-prompt))
  )

;; * comment-dwim-2
;; https://github.com/remyferre/comment-dwim-2
;; デフォルトのcomment-dwimの非リージョン選択時に行末にコメントをする仕様を回避するパッケージ
;; これを導入するとリージョン選択していなくても選択時と同様のコメントアウトがされるようになる
(global-set-key (kbd "M-;") 'comment-dwim-2)

;; * flycheck
(when (require 'flycheck)
  (global-flycheck-mode)
  )

;; * Smartparens
;; https://github.com/Fuco1/smartparens
;; カッコの自動挿入

(when (require 'smartparens-config)
  (smartparens-global-mode t)
  )


;; * Compilation mode

(with-eval-after-load 'compile
  (evil-make-overriding-map compilation-mode-map 'normal)
  (evil-define-key 'normal compilation-mode-map
    "j" 'compilation-next-error
    "k" 'compilation-previous-error
    "v" 'compilation-display-error
    )
  )


;; * Search

;; * grep mode
 (with-eval-after-load 'grep
  ;; キーバインドの設定
  ;; grep-modeのキーバインドはcompilation-mode-mapを元にしているので共通のキーバインドはそちらで定義
  ;; (evil-make-overriding-map grep-mode-map 'normal)
  ;; (evil-define-key 'normal grep-mode-map
  ;;     "j" 'next-error-no-select
  ;;     "k" 'previous-error-no-select
  ;;     (kbd "<C-return>") 'neotree-change-root
  ;;     )
  )


;; * 文書作成

;; * org-mode
(with-eval-after-load 'org
  (setq org-directory (concat-path "~" "gdrive" "Archive" "org"))
  (setq org-default-notes-file (concat-path org-directory "notes.org"))
  ;; 見出しの余分な*を消す
  (setq org-hide-leading-stars t)
  (define-key global-map "\C-cc" 'org-capture)
  (define-key global-map "\C-ca" 'org-agenda)
  )


;; * Shell

;; * Multi Term
;; https://www.emacswiki.org/emacs/MultiTerm
;; ansi-termの拡張、複数端末を起動できる
(with-eval-after-load 'multi-term
  ;; シェルのカレントディレクトリを追随する設定
  ;; .zshrcのchpwdに以下を出力する
  ;; echo -e "\033AnSiTu" $(whoami)
  ;; echo -e "\033AnSiTc" $(pwd)
  ;; echo -e "\033AnSiTh" $(hostname)

  ;; シェルパスの設定
  (setq multi-term-program shell-file-name)
  ;; ターミナルモードのフック
  (add-hook 'term-mode-hook
            '(lambda ()
               ;; 画面がガクガクなるのでマージンはとらない
               (setq scroll-margin 0)
               ;; カーソル行のハイライトをしない
               (setq global-hl-line-mode nil)
               ))

  ;; ターミナルに奪われないキーを設定
  (setq term-unbind-key-list '("M-x" "C-x" "C-c" "M-:"))
  ;; ターミナルモードのキーバインド
  (setq term-bind-key-alist
        '(
          ("C-c C-c" . term-interrupt-subjob)
          ("C-c C-e" . term-send-esc)
          ;;("C-p" . previous-line)
          ;;("C-n" . next-line)
          ;;("C-s" . isearch-forward)
          ;;("C-r" . isearch-backward)
          ("C-m" . term-send-return)
          ;;("C-y" . term-paste)
          ("M-f" . term-send-forward-word)
          ("M-b" . term-send-backward-word)
          ("M-o" . term-send-backspace)
          ("M-p" . term-send-up)
          ("M-n" . term-send-down)
          ("M-M" . term-send-forward-kill-word)
          ("M-N" . term-send-backward-kill-word)
          ("<C-backspace>" . term-send-backward-kill-word)
          ("M-r" . term-send-reverse-search-history)
          ("M-d" . term-send-delete-word)
          ("M-," . term-send-raw)
          ("M-." . comint-dynamic-complete)
          ))
  ;; Evilの無効化
  (evil-set-initial-state 'term-mode 'emacs)
  )


;; * Buffer manager

;; * bs-mode
;; https://www.emacswiki.org/emacs/BufferSelection
;; バッファ選択機能としてではなくバッファの整理するのに利用する。
;; 選択UIとしてはhelm-buffers-listからアクセスする。
(with-eval-after-load 'bs
  ;; キーバインドの設定
  (evil-make-overriding-map bs-mode-map 'normal)
  (evil-define-key 'normal bs-mode-map
    "j" 'bs-down
    "k" 'bs-up
    )
  )


;; * File manager

;; * Dired
;; https://www.emacswiki.org/emacs/DiredMode
(with-eval-after-load 'dired
  ;; diredの拡張機能を使う
  (require 'dired-x)

  ;; ファイルリストのlsのデフォルトオプション
  (setq dired-listing-switches "-lHF")
  ;; ファイルリストの表示切り替えオプションの一覧
  (defvar dired-listing-switches-list (list "-lFHA" dired-listing-switches))

  (lexical-let ((current-index 0))
    (defun my-dired-listing-toggle ()
      "実行ごとにdired表示切り替えを行う関数"
      (interactive)
      (let ((len (length dired-listing-switches-list) ))
        (when (< 0 len)
          (let ((index (% current-index len)))
            (setq dired-listing-switches (nth index dired-listing-switches-list))
            (setq current-index (+ index 1))
            (dired-sort-other dired-listing-switches))))))

  (defun dired-toggle-mark (arg)
    "マークをトグルする"
    (interactive "P")
    (let ((dired-marker-char
           (if (save-excursion (beginning-of-line)
                               (looking-at " "))
               dired-marker-char ?\040)))
      (dired-mark arg)))

  ;; キーバインド
  (evil-make-overriding-map dired-mode-map 'normal)
  (evil-define-key 'normal dired-mode-map
    "j" 'dired-next-line
    "k" 'dired-previous-line
    "m" 'dired-toggle-mark ; これでuキーに空きができる
    "u" 'dired-up-directory
    "." 'my-dired-listing-toggle
    )
  )


;; * Help mode

(with-eval-after-load 'help-mode
  (evil-make-overriding-map help-mode-map 'motion)
  (evil-add-hjkl-bindings help-mode-map 'motion
    (kbd "TAB") 'forward-button
    (kbd "C-o") 'help-go-back
    (kbd "0") (lookup-key evil-motion-state-map "0")
    ))


;; * Version control

;; * magit
;; https://github.com/magit/magit
;; (with-eval-after-load 'magit
;;   ;; 初期ステートがemacsなので解除する
;;   (setq evil-emacs-state-modes (delq 'magit-status-mode evil-emacs-state-modes))
;;
;;   (evil-make-overriding-map magit-mode-map 'normal)
;;   (evil-define-key 'normal magit-mode-map
;;     "j" 'magit-section-forward
;;     "k" 'magit-section-backward
;;     )
;;   ;; status-mode
;;   (evil-make-overriding-map magit-status-mode-map 'normal)
;;   (evil-define-key 'normal magit-status-mode-map
;;     "j" )
;;   )


;; * neotree
;; https://github.com/jaypei/emacs-neotree

(with-eval-after-load 'neotree
  (setq neo-show-updir-line nil)
  (setq neo-theme 'nerd)

  ;; キーバインドの設定
  (evil-make-overriding-map neotree-mode-map 'normal)
  (evil-define-key 'normal neotree-mode-map
    "g" 'neotree-refresh
    "." 'neotree-hidden-file-toggle
    "u" 'neotree-select-up-node
    "d" 'neotree-delete-node
    "j" 'neotree-next-line
    "k" 'neotree-previous-line
    (kbd "<C-return>") 'neotree-change-root
    )
  )


;; * Elisp

(with-eval-after-load 'lisp-mode
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (evil-leader/set-key-for-mode 'emacs-lisp-mode "e" 'eval-last-sexp)
  )


;; * c/c++ mode

;; c/c++共通の設定
(add-hook 'c-mode-common-hook
          (lambda ()
            ;; インデント幅
            (setq c-basic-offset 4)
            ;; スペースでインデントする
            (setq indent-tabs-mode nil)
               (c-toggle-auto-hungry-state 1)

            ))
;; c++固有の設定
(add-hook 'c++-mode-hook
          (lambda ()
            ))

;; * irony-mode
;; コード補完支援ツール、elispとclangライブラリを使ったサーバーで構成されている。
;;
;; インストールするにはllvmと位置を指定する必要がある
;; M-x irony-install-server
;; $ cmake -DLIBCLANG_LIBRARY\=$HOMEBREW_DIR/opt/llvm/lib/libclang.dylib \
;;         -DLIBCLANG_INCLUDE_DIR\=HOMEBREW_DIR/opt/llvm/include \
;;         -DCMAKE_INSTALL_PREFIX\=$HOME/.emacs.d/opt/irony \
;;         $HOME/.emacs.d/elpa/irony-20160825.1209/server \
;;      && cmake --build . --use-stderr --config Release --target install
(progn
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  ;; 先にirony関連のディレクトリを決定する
  (setq irony-user-dir (emacs-local-dir "irony"))
  (with-eval-after-load 'irony
    ;; 各モードフックを設定
    (add-to-list 'company-backends 'company-irony)
    (add-hook 'flycheck-mode-hook #'flycheck-irony-setup)

    (defun my-irony-mode-hook ()
      (define-key irony-mode-map [remap completion-at-point]
        'irony-completion-at-point-async)
      (define-key irony-mode-map [remap complete-symbol]
        'irony-completion-at-point-async))
    (add-hook 'irony-mode-hook 'my-irony-mode-hook)
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
    ))


;; * Golang

(require 'go-mode-autoloads)
(with-eval-after-load 'go-mode
  ;; 保存時にgofmtを実行
  (add-hook 'before-save-hook 'gofmt-before-save)
  ;; goの補完処理を読み込む
  (require 'go-autocomplete)
  )


;; * Perl

(with-eval-after-load 'cperl-mode
  (setq cperl-indent-level 4)
  (setq cperl-continued-statement-offset 4)
  (setq cperl-close-paren-offset -4)
  (setq cperl-label-offset -4)
  (setq cperl-comment-column 40)
  (setq cperl-highlight-variables-indiscriminately t)
  (setq cperl-indent-parens-as-block t)
  (setq cperl-tab-always-indent nil)
  (setq cperl-font-lock t)
  )


;; * Scheme

(setq scheme-program-name "/usr/local/bin/gosh -i")
(add-hook 'scheme-mode-hook
          (lambda ()
            (evil-leader/set-key-for-mode 'scheme-mode "e" 'scheme-send-definition)
            ))


;; * 起動画面
(cd (expand-file-name "~"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/gdrive/Archive/org/org.org")))
 '(session-use-package t nil (session)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
