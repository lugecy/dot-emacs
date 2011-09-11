(require 'zencoding-mode)
(ly:eval-after-load 'sgml-mode
  (add-hook 'html-mode-hook 'zencoding-mode))
(define-key zencoding-mode-keymap (kbd "C-j") nil)
(ly:eval-after-load 'yasnippet
  (defun zencoding-transform-yas-zero (ast)
    (let* ((leaf-count 0)
           (zencoding-leaf-function
            (lambda ()
              (if (= leaf-count 0)
                  (prog1 "$0" (incf leaf-count))
                ""))))
      (zencoding-transform ast)))

  (defun zencoding-expand-yas-zero ()
    (interactive)
    (let ((expr (zencoding-expr-on-line)))
      (if expr
          (let* ((markup (zencoding-transform-yas-zero (car (zencoding-expr (first expr)))))
                 (filled (replace-regexp-in-string "><" ">\n<" markup)))
            (delete-region (second expr) (third expr))
            (insert filled)
            (indent-region (second expr) (point))
            (yas/expand-snippet
             (buffer-substring (second expr) (point))
             (second expr) (point))))))

  (define-key zencoding-mode-keymap (kbd "<C-return>") 'zencoding-expand-yas-zero)
  (define-key zencoding-mode-keymap (kbd "<M-return>") 'zencoding-expand-line))
