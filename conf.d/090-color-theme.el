(require 'color-theme)
;; (color-theme-initialize)
;; (safe-loading "color-theme-ir-black")
;; (color-theme-ir-black)
;; (safe-loading 'zenburn)
;; (color-theme-zenburn)
(require 'color-theme-solarized)
(color-theme-solarized-dark)
(ly:eval-after-load 'magit
  (set-face-foreground 'magit-log-graph "Green"))

(add-hook 'window-setup-hook
          (lambda ()
            (modify-frame-parameters (selected-frame) '((cursor-color . "Green")))))
