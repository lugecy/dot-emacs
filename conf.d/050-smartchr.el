(require 'smartchr)
;;;; point位置のsexpを括弧で囲う動作を"("連打で実現する
(defun ly:smartchr-paren-first ()
  (interactive)
  (smartchr-make-struct
   :insert-fn 'paredit-open-round
   :cleanup-fn (lambda ()
                 (delete-char 1)
                 (delete-backward-char 1)
                 (when (and (char-after)
                            (= (char-after) ?\ ))
                   (delete-char 1)))))

(defun ly:smartchr-paren-second ()
  (interactive)
  (smartchr-make-struct
   :insert-fn 'paredit-wrap-round
   :cleanup-fn (lambda ()
                 (paredit-splice-sexp))))

(ly:eval-after-load 'paredit
  (define-key paredit-mode-map (kbd "(") (smartchr '(ly:smartchr-paren-first "((`!!'))" ly:smartchr-paren-second))))

