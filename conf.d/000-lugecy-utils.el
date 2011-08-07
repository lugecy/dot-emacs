;;;; 必ずウィンドウ分割してバッファを表示する(shell-pop.el での挙動)
(defun pop-up-split-window (buf)
  (split-window)
  (other-window 1)
  (switch-to-buffer buf))

;;;; エスケープシーケンスや printf フォーマットをハイライト - リタマス
;;;; http://d.hatena.ne.jp/mooz/20100313/p1
(defun ly:escape-seq-char-fontlock-setup ()
  (font-lock-add-keywords nil
                          '(("\\\\[areEfntv]" 0 font-lock-keyword-face t)
                            ("%-?[0-9]*\\.?[0-9]*l?[csduoxfegp]" 0 font-lock-type-face t))
                          t))

;;;; printf-debug 補助関数
(defvar tap-log-buffer "*tap log*")
(defmacro ly:tap (sexp &optional loging tag)
  "Return obj value and message."
  (let ((sym (cond ((consp sexp) (symbol-name (car sexp)))
                   ((symbolp sexp) (symbol-name sexp))
                   ((atom sexp) "`atom")))
        (tap-value (gensym)))
    `(let ((,tap-value ,sexp))
       (prog1 ,tap-value
         (ly:tap-msg ,tap-value ,sym ,loging ,tag)))))

(defun ly:tap-msg (value sym &optional loging tag)
  (let ((standard-output (cond ((eq loging t) (get-buffer-create tap-log-buffer))
                               ((bufferp loging) loging)
                               ((and (stringp loging) (get-buffer loging)))
                               (t t))))
    (with-current-buffer (get-buffer-create tap-log-buffer)
      (goto-char (point-min))
      (and (not (eq standard-output t))
           (princ (format "======%s======\n" (or tag ""))))
      (condition-case nil
          (princ (format "%s => %s%s" sym (prin1-to-string value) (if (not (eq standard-output t)) "\n" "")))
        (error nil)))))

(defmacro ly:tapl (&rest sexp)
  `(ly:tap ,@sexp t))

;; tap macro insert utility
(ly:eval-after-load 'smartchr
  (defun ly:tap-insert (&optional arg)
    (interactive "P")
    (save-excursion
      (unless (looking-at "\\(\\_<\\|\\s(\\)")
        (backward-sexp))
      (paredit-wrap-round)
      (insert (if (> (prefix-numeric-value arg) 1) "ly:tap " "ly:tapl "))
      (backward-up-list) (indent-sexp)))

  (defun ly:smartchr-tap-insert ()
    (interactive)
    (smartchr-make-struct
     :insert-fn (lambda ()
                  (ly:tap-insert current-prefix-arg))
     :cleanup-fn (lambda ()
                   (when (looking-at "(")
                     (down-list) (forward-sexp)
                     (paredit-raise-sexp)))))
  (define-key emacs-lisp-mode-map (kbd ":") (smartchr '(":" ly:smartchr-tap-insert))))

;;;; non gc benchmark-run
(defmacro benchmark-run-no-gc (&rest form)
  `(let ((gc-cons-threshold (* 1024 1024 32)))
     (garbage-collect)
     (benchmark-run ,@form)))

;;;; リストを1行1sexpに整形する
(defun ly:pp-list ()
  (interactive)
  (flet ((inner-string-p () (nth 3 (syntax-ppss (point)))))
    (save-excursion
      (let (list-beg)
        (unless (eq (char-after) ?\()
          (when (inner-string-p)
            (goto-char (1- (scan-sexps (point) -1))))
          (backward-up-list))
        (setq list-beg (point))
        (down-list)
        (ignore-errors
          (while t
            (insert "\n") (forward-sexp)))
        (goto-char list-beg)
        (indent-sexp)))))
