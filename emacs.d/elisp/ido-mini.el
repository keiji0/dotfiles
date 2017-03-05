;; ido-mini.el v0.1
;; バッファ、ブックマーク、最近開いたファイルを候補にido補完する
;; helm-miniに似たソースを提供するパッケージ


;;; Code:
(require 'ido)
(require 'bookmark)
(require 'recentf)
(require 'dash)
(require 'cl)

(defun ido-mini-source ()
  "バッファ > ブックマーク > 最近開いたファイルの順にリストを取得する"
  (let ((cur-file (buffer-file-name)))
    (mapcar 'cdr
            (-remove (lambda (x) (equal cur-file (car x)))
                     (delete-duplicates
                      `(;; 訪問中のバッファ一覧
                        ,@(mapcar (lambda (x)
                                    (cons (expand-file-name (buffer-file-name x))
                                          (propertize (format "%s [buffer]" (buffer-name x))
                                                      :value x
                                                      :type 'buffer)))
                                  (-filter (lambda (x)
                                             (buffer-file-name x))
                                           (buffer-list)))
                        ;; ブックマーク一覧
                        ,@(mapcar (lambda (x)
                                    (let ((bk (bookmark-get-bookmark x)))
                                      (cons (expand-file-name (bookmark-get-filename bk))
                                            (propertize (format "%s [bookmark]" x)
                                                        :value (expand-file-name (bookmark-get-filename bk))
                                                        :type 'bookmark))))
                                  (bookmark-all-names))
                        ;; 最近開いたファイル
                        ,@(mapcar (lambda (x)
                                    (cons (expand-file-name x)
                                          (propertize x
                                                      :value x
                                                      :type 'recentf)))
                                  recentf-list)
                        ;; 終了
                        (propertize "---------------------[END]---------------------"
                                    :value nil
                                    :type terminate)
                        )
                      :test (lambda (b a) (string-equal (car a) (car b)))
                      :from-end t)))))

(defun ido-mini ()
  "helm-miniのようなidoの便利コマンド"
  (interactive)
  (let* ((res (ido-completing-read "ido-mini: " (ido-mini-source) nil t)))
    (when res
      (let ((type (get-text-property 0 :type res))
            (value (get-text-property 0 :value res)))
        (case type
          ((buffer)
           (set-window-buffer (selected-window) value))
          ((bookmark)
           (find-file value))
          ((bookmark-command)
           (call-interactively 'bookmark-bmenu-list))
          ((recentf)
           (find-file value))
          )))))

(provide 'ido-mini)


;;; ido-mini.el ends here
