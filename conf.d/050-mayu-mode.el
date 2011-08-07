;;;; mayu-mode
;; (require 'mayu-mode)
(autoload 'mayu-mode "mayu-mode" nil t)
(setq auto-mode-alist (cons '("\\.mayu$" . mayu-mode) auto-mode-alist))

