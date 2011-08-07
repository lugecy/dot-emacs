(safe-loading "dabbrev-ja")
(safe-loading 'el-expectations)
(safe-loading 'lispxmp)
(with-loading 'highlight-context-line
  (set-face-background 'highlight-context-line-face "SteelBlue"))
(autoload 'rhtml-mode "rhtml-mode" nil t)
(with-loading 'text-translator-load
  (setq text-translator-auto-selection-func
        'text-translator-translate-by-auto-selection-enja)
  (autoload 'text-translator-all-by-auto-selection "text-translator" nil t))
(with-loading 'quick-yes
  (define-key quick-yes-map (kbd "C-j") 'quick-yes-answer-yes))
(with-loading 'sml-modeline
  (set-face-background 'sml-modeline-end-face "DarkSlateGrey")
  (set-face-foreground 'sml-modeline-vis-face "white")
  (set-face-background 'sml-modeline-vis-face "blue1")
  (sml-modeline-mode 1))
(safe-loading 'edit-list)
(with-loading 'zlc
  (define-key completion-list-mode-map (kbd "C-k") 'delete-completion-window))
(with-loading 'auto-highlight-symbol
  (global-set-key (kbd "C-c s a") 'auto-highlight-symbol-mode)
  (define-key goto-map (kbd "M-a") 'auto-highlight-symbol-mode))
(with-loading 'jaunte
  (setq jaunte-keys (mapcar #'identity "jklasdfgh")))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))

;;;; local-elisp
(safe-loading "hah")
