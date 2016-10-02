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

(provide 'edit-command)
