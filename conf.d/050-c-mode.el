;;;; インデント設定
(add-hook 'c-mode-common-hook 'ly:c-mode-style-configure)
(defun ly:c-mode-style-configure ()
  (c-set-style "bsd")
  (setq c-basic-offset 4)
  (setq tab-width 4))

;;;; c-context-open-line の妙な？挙動を抑制する
(defadvice c-context-open-line (around non-syntax-open-line activate)
  (let ((c-syntactic-indentation nil))
    ad-do-it))

;;;; make用のキーマップ
(defun ly:c-mode-keymap-configure ()
  (define-key c-mode-map (kbd "C-c C-j") 'compile)
  (define-key c-mode-map (kbd "C-o") 'c-context-open-line)
  (define-key c-mode-map (kbd "C-c o") 'c-set-offset)
  (define-key c-mode-map (kbd "C-c C-o") 'open-line))
(add-hook 'c-initialization-hook 'ly:c-mode-keymap-configure)

;;;; c-eldoc configure
(autoload 'c-turn-on-eldoc-mode "c-eldoc")
(setq c-eldoc-cpp-command "cpp")
(setq c-eldoc-includes "`pkg-config gtk+-2.0 --cflags` -I./ -I../ ")
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)

;;;; escape-sequence font-lock
(add-hook 'c-mode-hook 'ly:escape-seq-char-fontlock-setup)

;;;; cache clear func
(defun c-eldoc-clear-cache-current-buffer ()
  (interactive)
  (let ((p-buffer (cache-gethash (current-buffer) c-eldoc-buffers)))
    (when (bufferp p-buffer)
      (kill-buffer p-buffer)
      (cache-puthash (current-buffer) nil c-eldoc-buffers))))
