;; -*- coding: utf-8-unix -*-
;; (require 'yasnippet)
(require 'yas-jit)
;; (load "yasnippet-compiled-snippet")
(setq yas/root-directory (expand-file-name "snippets" user-emacs-directory))
;; (dolist (d yas/root-directory)
;;   (yas/load-directory d))
;; (yas/reload-all)
(setq yas/jit-cache-snippets nil)
(yas/jit-load)
(require 'yas-andy-bundle)
(setq yas/prompt-functions '(yas/dropdown-prompt yas/completing-prompt))
(yas/global-mode 1)

(require 'anything-c-yasnippet)
(setq anything-c-yas-display-key-on-candidate t)
(setq anything-c-yas-match-name-and-key t)

;;;; my snippent (move to ~/.emacs.d/snippets)
;; (yas/define 'elisp-mode "lkk" "(lookup-key ${1:global-map} (kbd \"$2\"))$0" "(lookup-key ...)")

;; yasnippetで同じパターンを連続入力する - (rubikitch loves (Emacs Ruby CUI))
;; http://d.hatena.ne.jp/rubikitch/20090702/1246477577
(defvar yas/oneshot-snippet nil)
(defun yas/register-oneshot-snippet (s e)
  (interactive "r")
  (setq yas/oneshot-snippet (buffer-substring-no-properties s e))
  (delete-region s e)
  (yas/expand-oneshot-snippet)
  (message "%s" (substitute-command-keys "Press \\[yas/expand-oneshot-snippet] to expand.")))

(defun yas/expand-oneshot-snippet ()
  (interactive)
  (if (string> yas/version "0.6.0")
      (yas/expand-snippet yas/oneshot-snippet)
    (yas/expand-snippet (point) (point) yas/oneshot-snippet)))

(defun yas/oneshot-snippet ()
  "If `transient-mark-mode' is enabled and region is selected,
register the region as oneshot snippet, Otherwise expand it."
  (interactive)
  (if (region-active-p)
      (yas/register-oneshot-snippet (region-beginning) (region-end))
    (yas/expand-oneshot-snippet)))

;;;; 括弧の中でyas/expandした時に外側の括弧を削除する
(defun yas/before-lisp-surround-delete ()
  (when (and (memq major-mode '(emacs-lisp-mode lisp-interaction-mode inferior-emacs-lisp-mode))
             (eq (char-before start) ?\()
             (string-match "\\`(\\(.\\|\n\\)+)\n*\\'" template))
    (setq template (replace-regexp-in-string "\\`(" "" (replace-regexp-in-string ")\\(\n*\\)\\'" "\\1" template)))))
(add-hook 'yas/before-expand-snippet-hook 'yas/before-lisp-surround-delete)

;;;; for yasnippet-0.6.1c utils function
(defun yas/snippet-table-to-templates-alist (table)
  (when table
    (let ((acc (list)))
      (maphash (lambda (key namehash)
                 (maphash (lambda (name template)
                            (push (cons key template) acc))
                          namehash))
               (yas/snippet-table-hash table))
      acc)))

;; yas/keymap用のreload関数を定義(for yas/trigger-key-reload)
(defun yas/keymap-reload ()
  (setq yas/keymap
        (let ((map (make-sparse-keymap)))
          (mapc #'(lambda (binding)
                    (yas/define-some-keys (car binding) map (cdr binding)))
                `((,yas/next-field-key     . yas/next-field-or-maybe-expand)
                  (,yas/prev-field-key     . yas/prev-field)
                  ("C-g"                   . yas/abort-snippet)
                  (,yas/skip-and-clear-key . yas/skip-and-clear-or-delete-char)))
          map)))

;;;; テンプレート展開中であることをわかりやすくする
;;; change mode-line-face
;; (defvar ly:yas-before-expand-modeline-face nil)
;; (defvar ly:yas-expanding-modeline-face "White")
;; (defun ly:yas-expanding-face-before-hook ()
;;   (setq ly:yas-before-expand-modeline-face (face-attribute 'mode-line :background))
;;   (set-face-background 'mode-line ly:yas-expanding-modeline-face))
;; (defun ly:yas-expanding-face-after-hook ()
;;   (set-face-background 'mode-line ly:yas-before-expand-modeline-face)
;;   (setq ly:yas-before-expand-modeline-face nil))
;; (add-hook 'yas/before-expand-snippet-hook 'ly:yas-expanding-face-before-hook)
;; (add-hook 'yas/after-exit-snippet-hook 'ly:yas-expanding-face-after-hook)
;;; change blink interval
(defvar ly:yas-backup-blink-time nil)
(defvar ly:yas-before-blink-time nil)
(defvar ly:yas-visualize-blink-time (cons 0.1 0.1))
(defun ly:yas-visualize-expanding-pre-func ()
  (unless ly:yas-backup-blink-time
    (setq ly:yas-backup-blink-time (cons blink-cursor-interval blink-cursor-delay)))
  (setq ly:yas-before-blink-time (cons blink-cursor-interval blink-cursor-delay))
  (setq blink-cursor-interval (car ly:yas-visualize-blink-time)
        blink-cursor-delay (cdr ly:yas-visualize-blink-time)))
(defun ly:yas-visualize-expanding-post-func ()
  (setq blink-cursor-interval (car ly:yas-before-blink-time)
        blink-cursor-delay (cdr ly:yas-before-blink-time))
  (setq ly:yas-before-blink-time nil))
(defun ly:yas-visualize-reset-blink-time ()
  (interactive)
  (when ly:yas-backup-blink-time
    (setq blink-cursor-interval (car ly:yas-backup-blink-time)
          blink-cursor-delay (cdr ly:yas-backup-blink-time))))
(add-hook 'yas/before-expand-snippet-hook 'ly:yas-visualize-expanding-pre-func)
(add-hook 'yas/after-exit-snippet-hook 'ly:yas-visualize-expanding-post-func)

;;;; with auto-complete
(defvar ly:yas-expanding-flg nil)
(defun ly:yas-expanding-flg-on ()
  (interactive)
  (setq ly:yas-expanding-flg t))
(defun ly:yas-expanding-flg-off ()
  (interactive)
  (setq ly:yas-expanding-flg nil))
(add-hook 'yas/before-expand-snippet-hook 'ly:yas-expanding-flg-on)
(add-hook 'yas/after-exit-snippet-hook 'ly:yas-expanding-flg-off)
(ly:eval-after-load 'auto-complete-yasnippet
  (defadvice ac-yasnippet-candidate (around ly:ac-candidiate-disable activate)
    (unless ly:yas-expanding-flg
      ad-do-it)))
