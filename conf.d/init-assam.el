;;;; Emacs 23.x font setting
(if (window-system)
    (progn
      (set-default-font "VL Gothic-12")
      (set-fontset-font "fontset-default"
                        'japanese-jisx0208
                        '("VL Gothic" . "unicode-bmp")))
  (setq initial-frame-alist
        ;; font value is (x-list-fonts "VL Gothinc-12")
        (append '((font . "-unknown-VL Gothic-normal-normal-normal-*-*-120-*-*-*-0-iso10646-1"))
                initial-frame-alist)))
(setq initial-frame-alist
      (append `(
                (width . 105)
                (height . 35)
                (alpha . (75 75))
                )
              initial-frame-alist))
(setq default-frame-alist initial-frame-alist)

;;;; uim.el
;; (when (string-match "linux" system-configuration)
;;   (require 'uim)
;;   (global-set-key (kbd "C-x C-j") 'uim-mode)
;;   (eval-after-load "auto-complete"
;;     '(add-to-list 'ac-trigger-commands 'uim-process-input)))

;;;; ibus.el
;; (require 'ibus)
(autoload 'ibus-mode "ibus" nil t)
(ly:eval-after-load 'ibus
  (ibus-define-common-key (kbd "C-j") t)
  (defadvice ibus-mode-on (after ibus-on-with-enable activate)
    (call-interactively 'ibus-enable)))
(global-set-key (kbd "C-x C-j") 'ibus-mode)

;; sdic
;; 英和検索で使用する辞書
;;(setq sdic-eiwa-dictionary-list
;;      `((sdicf-client
;;         ,(expand-file-name "../../etc/sdic/gene.sdic" (invocation-directory)))
;;        (sdicf-client
;;         ,(expand-file-name "../../etc/sdic/eedict.sdic" (invocation-directory)))))
;;;; 和英検索で使用する辞書
;;(setq sdic-waei-dictionary-list
;;      `((sdicf-client
;;         ,(expand-file-name "../../etc/sdic/jedict.sdic" (invocation-directory)))))
;;

;;;; migemo.el
(when (executable-find "cmigemo")
  (require 'migemo)
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs" "-i" "\g"))
  (setq migemo-dictionary (expand-file-name "../../etc/cmigemo/migemo-dict" (invocation-directory)))
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil))

;;;; shell / ansi-term
(setq shell-file-name "bash")

;;;; anything source local setting
(add-to-list 'anything-sources 'anything-c-source-locate t)

;;;; daemon起動時にterminalでの文字化け防止・その他
(defun ly:set-terminal-configure (frame &optional keep-window-configuration)
  (with-selected-frame frame
    (set-terminal-coding-system 'utf-8-unix)
    (keyboard-translate ?\C-h 'backspace)
    (set-terminal-parameter frame 'background-mode 'dark)))
(add-hook 'after-make-frame-functions 'ly:set-terminal-configure t)
