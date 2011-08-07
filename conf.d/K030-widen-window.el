;;;; winden-window.el (フォーカスがあるwindowを拡大表示する)
(autoload 'widen-current-window "widen-window" nil t)
(eval-after-load "widen-window"
  '(custom-set-variables '(ww-ratio 0.7)
                         '(ww-nonwide-modes '(ediff-mode moccur-mode moccur-grep-mode))))

;; (defadvice anything (around disable-ww-mode activate)
;;   (ad-deactivate-regexp "widen-window")
;;   (unwind-protect
;;       ad-do-it
;;     (ad-activate-regexp "widen-window")))
;;
;; (add-hook 'ediff-prepare-buffer-hook (lambda () (widen-window-mode nil)))
;; (global-widen-window-mode t)

