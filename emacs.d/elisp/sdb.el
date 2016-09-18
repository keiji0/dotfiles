;; sdb.el v0.1

(defvar sdb-directory "~/.emacs.d/sdb" "sdbが値を保存するディレクトリ")
(defvar sdb-before-commit-hook '() "sdbがファイルにコミットするまでに実行されるフック")

(lexical-let ((db (make-hash-table :test 'equal)))
  (defun sdb-clear ()
    (clrhash db))

  (defun sdb-get (key &optional def)
    (gethash key db def))

  (defun sdb-set (key val)
    (puthash key val db))

  (defun sdb-commit ()
    (unless (file-exists-p sdb-directory)
      (make-directory sdb-directory))
    (run-hooks 'sdb-before-commit-hook)
    (maphash (lambda (key val)
               (with-temp-file (expand-file-name key sdb-directory)
                 (prin1 val (current-buffer))))
             db)
    (message "sdb-commit"))

  (defun sdb-load ()
    (when (file-exists-p sdb-directory)
      (mapcar (lambda (file)
                   (let ((key (file-name-base file)))
                     (with-temp-buffer
                       (insert-file-contents file)
                       (goto-char (point-min))
                       (sdb-set key (read (current-buffer))))))
              (directory-files sdb-directory t directory-files-no-dot-files-regexp))))
  )

(sdb-load)

(unless noninteractive
  (add-hook 'kill-emacs-hook #'sdb-commit))

(provide 'sdb)
