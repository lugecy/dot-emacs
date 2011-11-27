;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; C-h = backspace
(keyboard-translate ?\C-h 'backspace)

;;;; for terminal key translate (only mintty?)
(unless window-system
  (define-key input-decode-map "\e[1;5j" (kbd "C-:"))
  (define-key input-decode-map "\e[1;5k" (kbd "C-;"))
  (define-key input-decode-map "\e[1;5n" (kbd "C-."))
  (define-key input-decode-map "\e[1;5l" (kbd "C-,"))
  (define-key input-decode-map "\e[1;6x" (kbd "C-("))
  (define-key input-decode-map "\e[1;6y" (kbd "C-)"))
  (define-key input-decode-map "\e[1;7k" (kbd "C-M-;")))

;;;; help-for-help
(global-set-key (kbd "C-c h h") 'help-command)

;;;; window operation
(global-set-key (kbd "<f8>") 'delete-other-windows)
(global-set-key (kbd "<C-f8>") 'kill-buffer-and-window)
(global-set-key (kbd "C-q") 'wincmd-keymap)


(define-key isearch-mode-map "\C-e" 'isearch-yank-point-symbol)

;;;; コメントアウトキーマップ
(global-set-key (kbd "C-c ;") 'comment-dwim)
;; (global-set-key (kbd "C-c C-;") 'comment-dwim)

(global-set-key (kbd "C-c y") 'djcb-duplicate-line)

;;;; 時折刺さるのでview-hello-fileを"C-c h h"から発動不可に
(define-key help-map (kbd "h") 'undefined)

;;;; C-M-V が押しにくいので
(global-set-key (kbd "C-M-y") 'scroll-other-window-down)

(global-set-key (kbd "C-c C-r") 'ly:yank-before-point-target)

;;;; anything
(global-set-key (kbd "C-;") 'anything-for-open)
(global-set-key (kbd "C-c C-;") 'anything-for-current-buffer)
(global-set-key (kbd "C-M-;") 'anything-elisp-apropos)
(global-set-key (kbd "M-y") 'anything-show-kill-ring) ;; default yank-pop
(global-set-key (kbd "C-c C-/") 'anything-for-current-buffer-with-prefix)
(unless window-system
  (global-set-key (kbd "C-c C-_") 'anything-for-current-buffer-with-prefix))

;;;; auto-complete
(when (fboundp 'ac-set-trigger-key)
  (ac-set-trigger-key "TAB"))

(global-set-key (kbd "C-M-SPC") 'bm-toggle)
(global-set-key (kbd "C-c C-SPC") 'bm-toggle)
(global-set-key (kbd "M-n") 'bm-next)
(global-set-key (kbd "M-p") 'bm-previous)

;;;; 050-fastnav.el
(global-set-key (kbd "M-z") fastnav-sub-map)

;;;; 050-highlight.el
(global-set-key (kbd "C-c s s") 'highlight-symbol-at-point)
(global-set-key (kbd "C-c s [") 'highlight-symbol-prev)
(global-set-key (kbd "C-c s ]") 'highlight-symbol-next)
(global-set-key (kbd "C-c s r") 'highlight-symbol-query-replace)
(global-set-key (kbd "C-c s p") 'highlight-symbol-jump-prev-hi-lock-symbol)
(global-set-key (kbd "C-c s n") 'highlight-symbol-jump-next-hi-lock-symbol)
(global-set-key (kbd "C-c s j") 'highlight-symbol-walk-mode)

(global-set-key (kbd "C-c s l") 'hlt-highlight)

;;;; 050-historyf.el
(global-set-key (kbd "C-x p") 'historyf-back)
(global-set-key (kbd "C-x P") 'historyf-forward)
(global-set-key (kbd "C-x C-;") 'anything-historyf)

;;;; 050-iedit.el
;; (global-set-key (kbd "C-;") 'iedit-mode)

;;;; 050-ielm.el
(global-set-key (kbd "M-:") 'ielm-with-current-buffer)

;;;; 050-magit.el
(global-set-key (kbd "C-c j") magit-global-map)

;;;; 050-org.el
(global-set-key (kbd "<C-f9>") 'org-remember)
(global-set-key (kbd "C-c a") 'org-agenda)

;;;; 050-scroll.el
(global-set-key "\C-m" 'sane-newline)

;;;; 050-sequential-command.el
(defalias 'lugecy-beginning-of-line
  (if (featurep 'ce-scroll)
      'ce-beginning-of-line
    (lookup-key global-map (kbd "C-a"))))
(global-set-key (kbd "C-a") 'seq-home)
(global-set-key (kbd "C-e") 'seq-end)

;;;; 050-thing-commands.el
(global-set-key (kbd "C-M-@") 'cycle-thing-region) ;; default mark-word
(ly:eval-after-load 'popup
  (global-set-key (kbd "C-M-@") 'mark-thing-popup))

;; (global-set-key (kbd "C-w") 'kill-region-dwim)
;; (define-key viper-insert-global-user-map (kbd "C-w") 'kill-region-dwim)
(global-set-key (kbd "M-w") 'kill-ring-save-dwim)
;; (global-set-key (kbd "C-w") 'unix-werase-or-kill) ;; default kill-region
(global-set-key (kbd "C-w") 'wrap-unix-werase-or-kill)

;;;; 050-undo-redo.el
(global-set-key (kbd "C-.") 'redo)
(global-set-key (kbd "C-c p i") 'point-undo)
(global-set-key (kbd "C-c p u") 'point-redo)

;;;; 050-yasnippet.el
(global-set-key (kbd "M-e") nil)        ;default: forward-sentence
(setq yas/trigger-key "M-e")
(yas/trigger-key-reload "TAB")          ;new bind and unbind "TAB" (default-key)
(setq yas/next-field-key '("M-e")
      yas/prev-field-key '("M-E"))
(yas/keymap-reload)

(global-set-key (kbd "C-c C-y") 'anything-c-yas-complete)
(ly:eval-after-load "sequential-command"
  (define-key global-map "\C-x\C-y" 'yas/oneshot-snippet))

;;;; 051-anything-c-moccur.el
(global-set-key (kbd "M-i") 'anything-c-moccur-occur-by-moccur) ;バッファ内検索 default: tab-to-tab-stop

;;;; 059-trivial-loading.el
(global-set-key (kbd "C-c s a") 'auto-highlight-symbol-mode)
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

;;;; 092-onekey-template.el
(global-set-key (kbd "C-x RET") 'one-key-menu-C-x-RET)
(global-set-key (kbd "C-x 4") 'one-key-menu-C-x-4)
(global-set-key (kbd "C-x 5") 'one-key-menu-C-x-5)
(global-set-key (kbd "C-x n") 'one-key-menu-C-x-n)
(global-set-key (kbd "C-x r") 'one-key-menu-C-x-r)
(global-set-key (kbd "C-x v") 'one-key-menu-C-x-v)
(global-set-key (kbd "M-g") 'one-key-menu-M-g)
(global-set-key (kbd "C-x a") 'one-key-menu-C-x-a)
(global-set-key (kbd "C-x ?") 'one-key-menu-C-x)
