
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
  (global-set-key "\C-h" 'delete-backward-char)
  ;; ヘルプは地味に使うので割り当てる
  (global-set-key "\M-?" 'help-for-help)
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
  (setq scroll-margin 3)
  ;; 行溢れした文字は折り返さずに表示する
  (setq-default truncate-lines t)
  (setq runcate-partial-width-windows t)
  ;; 現在行をハイライト
  (global-hl-line-mode t) 
  ;; 対応するカッコをハイライトする
  (show-paren-mode t)
  (setq show-paren-style 'parenthesis)
  ;; 現在のカーソル行をハイライトを無効にする
  (setq global-hl-line-mode nil)
  ;; ファイル保存時に最終行に改行を入れる
  (setq require-final-newline t)
  ;; フォントの設定
  (when window-system
    (cond ((eq system-type 'darwin)
	   (set-default-font "Dejavu Sans Mono 14")
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
  )


;; Themeや色に関する設定

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

  ;; ノーマルモード
  (define-key evil-motion-state-map ";" 'evil-ex) 
  (define-key evil-motion-state-map "\C-i" 'evil-jump-item)
  (define-key evil-motion-state-map "gb" 'bs-show)

  ;; Leader
  (evil-leader/set-key "b" 'switch-to-buffer)
  (evil-leader/set-key "v" 'magit-status)
  (evil-leader/set-key "x" 'kill-buffer)
  (evil-leader/set-key "s" 'eshell)

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


;; Dired

(eval-after-load 'dired
  '(progn
	 ;; diredの拡張機能を使う
	 (require 'dired-x)

	 (defvar dired-listing-switches-array ["-lHF" "-lFHa"]
	   "ファイルリストの表示切り替えオプションの一覧")

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

	 ;; スペースでマークする (FD like)
	 (defun dired-toggle-mark (arg)
	   "Toggle the current (or next ARG) files."
	   ;; S.Namba Sat Aug 10 12:20:36 1996
	   (interactive "P")
	   (let ((dired-marker-char
			  (if (save-excursion (beginning-of-line)
								  (looking-at " "))
				  dired-marker-char ?\040)))
		 (dired-mark arg)))

     ;; Explorer のようにファイル名の 1 文字目で検索する
     (when (require 'dired-ex-isearch)
       (require 'highline))

	 ;; キーバインド
	 (progn
       (evil-define-key 'motion dired-mode-map "/" 'dired-ex-isearch)
	   (define-key dired-mode-map "m" 'dired-toggle-mark) ;; これでuキーに空きができる
	   (define-key dired-mode-map "u" 'dired-up-directory) ;; 上の階層のディレクトリへ移動
	   (define-key dired-mode-map "u" 'dired-up-directory) ;; 上の階層のディレクトリへ移動
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
