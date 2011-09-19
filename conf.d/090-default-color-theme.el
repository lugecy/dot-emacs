(when (= (length color-theme-history) 0)
  ;; (color-theme-initialize)
  ;; (safe-loading "color-theme-ir-black") (color-theme-ir-black)
  (and (require 'zenburn) (color-theme-zenburn))
  ;; (and (require 'color-theme-solarized) (color-theme-solarized-dark))
  ;; (ly:eval-after-load 'magit
  ;;   (set-face-foreground 'magit-log-graph "Green"))
  )
;;;; for viper???
;; (add-hook 'window-setup-hook
;;           (lambda ()
;;             (modify-frame-parameters (selected-frame) '((cursor-color . "Green")))))
