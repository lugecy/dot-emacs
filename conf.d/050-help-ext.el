(ly:eval-after-load 'info (safe-loading 'info+))
(ly:eval-after-load 'help (safe-loading 'help+))
(ly:eval-after-load 'help-fns (safe-loading 'help-fns+))
(ly:eval-after-load 'help-mode (safe-loading 'help-mode+))

;;;; help-map redefine
(define-key help-map (kbd "c") 'describe-key-briefly)
(define-key help-map (kbd "C-c") 'describe-command)
(define-key help-map (kbd "C-k") 'describe-keymap)
(set-face-foreground 'info-reference-item "Black")
