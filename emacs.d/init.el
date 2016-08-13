
;; 基本的な設定

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

;; elisp配下
(loop for f in (directory-files (emacs-libs-dir) t)
      when (and (file-directory-p f)
		(not (member (file-name-nondirectory f) '("." ".."))))
      do (add-to-list 'load-path f))
(add-to-list 'load-path (emacs-libs-dir))
(add-to-list 'load-path (emacs-home "themes"))
(add-to-list 'custom-theme-load-path (emacs-home "themes"))

;; 環境変数のロード
(require 'exec-path-from-shell)
(setq exec-path-from-shell-variables '("PATH" "MANPATH"));
(exec-path-from-shell-initialize)


;; デフォルトEmacsの設定
;; このセクションは外部依存がなく最低限の挙動を設定する

;; エンコーディング関連
(progn
  (set-language-environment 'Japanese)
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
  ;; 自動インデント
  (electric-indent-mode)
  ;; カーソル付近のファイルパスを開く
  (ffap-bindings)
  ;; Buffer関連の設定
  (progn
    ;; 死ぬほど便利なバッファの切り替えコマンドを有効にする
    (iswitchb-mode 1)
    ;; 補完対象にふくめないバッファ名、*から始まるやつ
    (add-to-list 'iswitchb-buffer-ignore "\\`\\*")
    ;; バッファリストはbsを使う
    (define-key global-map "\C-x\C-b" 'bs-show)
    )
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
  ;; 横幅のあるディスプレイなのでスクロールバーを表示する
  (toggle-scroll-bar nil)
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
  (global-hl-line-mode t) 
  ;; 対応するカッコをハイライトする
  (show-paren-mode t)
  (setq show-paren-style 'parenthesis)
  ;; 現在のカーソル行をハイライトを無効にする
  (setq global-hl-line-mode t)
  ;; ファイル保存時に最終行に改行を入れる
  (setq require-final-newline t)
  ;; フォントの設定
  (when window-system
    (cond ((eq system-type 'darwin)
	   (set-default-font "Dejavu Sans Mono 13")
	   (set-fontset-font nil
			     'japanese-jisx0208
			     ;; (font-spec :family "Hiragino Mincho Pro")) ;; font
			     (font-spec :family "Hiragino Kaku Gothic ProN"))
	   (setq-default line-spacing 1)
	   ))
    )
  )


;; パッケージの設定
(when (require 'package)
  ;; パッケージアーカイブを追加する
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (package-initialize)
  ;;(package-refresh-contents)
  (package-install 'evil)
  (package-install 'evil-leader)
  (package-install 'powerline)
  (package-install 'session)
  (package-install 'auto-complete)
  (package-install 'go-mode)
  (package-install 'go-autocomplete)
  (package-install 'undo-tree)
  (package-install 'tabbar)
  (package-install 'multi-term)
  )


;; Themeや色に関する設定

;;(load-theme 'deeper-blue t)
(load-theme 'tomorrow-night t)
;;(load-theme 'tomorrow-night-eighties t)
;;(load-theme 'tomorrow-night-blue t)
;;(load-theme 'tomorrow-night-bright t)
;;(load-theme 'tomorrow-day t)

;; Powerline
(when (require 'powerline)
  ;; モードラインのテーマを設定
  ;;(powerline-default-theme)
  (powerline-center-evil-theme)
  ;; モードラインが若干乱れて表示されるのを直す
  ;; https://github.com/milkypostman/powerline/issues/54
  (setq ns-use-srgb-colorspace nil)
  )


;; セッション保存の設定
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


;; Evil

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

  ;; ノーマルモード
  (define-key evil-motion-state-map ";" 'evil-ex) 
  (define-key evil-motion-state-map "\C-i" 'evil-jump-item)
  (define-key evil-motion-state-map "gb" 'bs-show)

  ;; Leader
  (evil-leader/set-key "f" 'find-file)
  (evil-leader/set-key "b" 'switch-to-buffer)
  (evil-leader/set-key "v" 'magit-status)
  (evil-leader/set-key "x" 'kill-buffer)

  ;; インサートモード
  (define-key evil-insert-state-map (kbd "C-y") nil)
  (define-key evil-insert-state-map (kbd "C-j") 'newline-and-indent)

  ;; モーション
  (define-key evil-motion-state-map (kbd "RET") nil)

  ;; ミニバッファモード
  (define-key minibuffer-local-map "\C-w" 'evil-delete-backward-word)

  ;; ウィンドウ関連
  (define-key evil-window-map "-" 'evil-window-split)
  (define-key evil-window-map "|" 'evil-window-vsplit)
  (define-key evil-window-map "0" 'delete-window)
  (define-key evil-window-map "1" 'delete-other-windows)

  ;; 干渉するモードのキーバインドの設定
  ;; motion, emacs, normal
  (evil-set-initial-state 'bs-mode 'motion)
  (evil-set-initial-state 'grep-mode 'motion)
  )


;; Auto Complete

(when (require 'auto-complete-config)
  (ac-config-default)
  ;; 補完開始キーの設定
  (ac-set-trigger-key "TAB")
  ;; 自動補完を無効にする
  (setq ac-auto-start nil)
  ;; 補完推測のキャッシュファイルの指定
  (setq ac-comphist-file (emacs-var-dir "ac-comphist.el"))
  ;; メジャーモードごとの補完辞書ファイルのディレクトリ指定
  (add-to-list 'ac-dictionary-directories (emacs-share-dir "ac-dict"))
  )


;; Tabbar

(when (require 'tabbar)
  (tabbar-mode)
  ;; マウスホイール無効
  (tabbar-mwheel-mode nil)                  

  ;; 画像を表示しない
  (setq tabbar-use-images nil)
  (dolist (btn '(tabbar-buffer-home-button
				 tabbar-scroll-left-button
				 tabbar-scroll-right-button))
	(set btn (cons (cons "" nil)
				   (cons "" nil))))

  ;; タブに表示するバッファを設定
  (setq tabbar-buffer-list-function
		(lambda ()
		  ;; " *"から始まるバッファを除外
		  (remove-if
		   (lambda(buffer)
			 (find (aref (buffer-name buffer) 0) " *"))
		   (buffer-list))))

  ;; キーバインドの設定
  (global-set-key (kbd "<C-tab>") 'tabbar-forward-tab)
  (global-set-key (kbd "<C-S-tab>") 'tabbar-backward-tab)

  ;; グループ化しない
  (setq tabbar-buffer-groups-function nil)

  ;; タブバーの文字列を変更する
  (defun my-tabbar-buffer-tab-label (tab)
	"デフォルト関数の両端にスペースをつける"
	(format "  %s  " (tabbar-buffer-tab-label tab)))
  (setq tabbar-tab-label-function 'my-tabbar-buffer-tab-label)
  (setq tabbar-separator '(0.3))

  ;; 表示設定
  (set-face-attribute
   'tabbar-default nil
   :background "gray20"
   :foreground "gray20"
   :height 1.1
   :box nil)
  (set-face-attribute
   'tabbar-unselected nil
   :background "gray30"
   :foreground "white"
   :box nil)
  (set-face-attribute
   'tabbar-selected nil
   :background "gray75"
   :foreground "black"
   :box nil)
  (set-face-attribute
   'tabbar-highlight nil
   :background "white"
   :foreground "black"
   :underline nil
   :box nil)
  )


;; multi term

(when (require 'multi-term)
  ;; シェルパスの設定
  (setq multi-term-program shell-file-name)
  ;; ターミナルモードのフック
  (add-hook 'term-mode-hook
			'(lambda ()
			   ;; 画面がガクガクなるのでマージンはとらない
			   (setq scroll-margin 0)
			   ))
  ;; ターミナルに奪われないキーを設定
  (setq term-unbind-key-list '("M-x" "C-x" "C-c"))
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


;; Dired

(eval-after-load 'dired
  '(progn
	 ;; diredの拡張機能を使う
	 (require 'dired-x)

	 ;; ファイルリストのlsのデフォルトオプション
	 (setq dired-listing-switches "-lHF")
	 ;; ファイルリストの表示切り替えオプションの一覧
	 (defvar dired-listing-switches-array ["-lHF" "-lFHa"])

	 (lexical-let ((current-index 0))
	   (defun my:dired-listing-toggle ()
		 "実行ごとにdired表示切り替えを行う関数"
		 (interactive)
		 (let ((len (length dired-listing-switches-array) ))
		   (when (< 0 len)
			 (let ((index (% current-index len)))
			   (setq dired-listing-switches (aref dired-listing-switches-array index))
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

	 (defun dired-open-in-accordance-with-situation ()
	   "ファイルなら別バッファで、ディレクトリなら同じバッファで開く"
	   (interactive)
	   (let ((file (dired-get-filename)))
		 (if (file-directory-p file)
			 (dired-find-alternate-file)
		   (dired-find-file))))
	 (defun my-dired-up-directory ()
	   (interactive)
	   (find-alternate-file ".."))
	 (put 'dired-find-alternate-file 'disabled nil)

     ;; Explorer のようにファイル名の 1 文字目で検索する
     (when (require 'dired-ex-isearch)
       (require 'highline))

	 ;; キーバインド
	 (progn
       (evil-define-key 'motion dired-mode-map "/" 'dired-ex-isearch)

	   ;; RET 標準の dired-find-file では dired バッファが複数作られるので
	   ;; dired-find-alternate-file を代わりに使う
	   (define-key dired-mode-map (kbd "<return>") 'dired-open-in-accordance-with-situation)
	   (define-key dired-mode-map (kbd "<C-return>") 'dired-find-file)

	   (define-key dired-mode-map "m" 'dired-toggle-mark) ;; これでuキーに空きができる
	   (define-key dired-mode-map "u" 'my-dired-up-directory) ;; 上の階層のディレクトリへ移動
	   (define-key dired-mode-map "." 'my:dired-listing-toggle) ;; 表示リストを切り替える
	   )
     ))


;; Elisp

(progn
  (when evil-mode
	(evil-leader/set-key-for-mode 'emacs-lisp-mode "e" 'eval-last-sexp)
	))


;; Golang

(require 'go-mode-autoloads)
(eval-after-load 'go-mode
  '(progn
     ;; 保存時にgofmtを実行
     (add-hook 'before-save-hook 'gofmt-before-save)
     ;; goの補完処理を読み込む
     (require 'go-autocomplete)
     ))


;; Perl

(defalias 'perl-mode 'cperl-mode)
(setq cperl-indent-level 4)
(setq cperl-continued-statement-offset 4)
(setq cperl-close-paren-offset -4)
(setq cperl-label-offset -4)
(setq cperl-comment-column 40)
(setq cperl-highlight-variables-indiscriminately t)
(setq cperl-indent-parens-as-block t)
(setq cperl-tab-always-indent nil)
(setq cperl-font-lock t)


;; Scheme

(setq scheme-program-name "/usr/local/bin/gosh -i")
(add-hook 'scheme-mode-hook
		  (lambda ()
			(evil-leader/set-key-for-mode 'scheme-mode "e" 'scheme-send-definition)
			))


;; 起動画面
(cd (emacs-home))
