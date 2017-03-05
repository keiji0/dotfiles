;; edit-command.el v0.1
;; 独自の編集コマンドを定義

(defun upcase-sexp ()
  "point付近のsexpを大文字に切り替える"
  (interactive)
  (let ((reg (bounds-of-thing-at-point 'sexp)))
    (upcase-region (car reg) (cdr reg))))

(defun downcase-sexp ()
  "point付近のsexpを小文字に切り替える"
  (interactive)
  (let ((reg (bounds-of-thing-at-point 'sexp)))
    (downcase-region (car reg) (cdr reg))))

(defun non-regexp-occur (regexp &optional nlines)
  "非正規表現版のoccur"
  (interactive (occur-read-primary-args))
  (occur-1 (regexp-quote regexp) nlines (list (current-buffer))))

(provide 'edit-command)
