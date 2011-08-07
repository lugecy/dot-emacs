;; -*- coding: utf-8-unix -*-
(require 'yasnippet)
;; (load "yasnippet-compiled-snippet")
(setq yas/prompt-functions '(yas/dropdown-prompt yas/ido-prompt yas/completing-prompt))

(require 'anything-c-yasnippet)
(setq anything-c-yas-display-key-on-candidate t)
(setq anything-c-yas-match-name-and-key t)
(global-set-key (kbd "C-c C-y") 'anything-c-yas-complete) 
(setq yas/root-directory (list "~/.emacs.d/my-snippet"))
;; (yas/reload-all)
(require 'yas-andy-bundle)
(yas/global-mode 1)

;;;; my snippent
(yas/define 'ruby-mode "desc" "describe ${1} do
  $0
end" "describe ... do ... end")
(yas/define 'ruby-mode "it" "it ${1:should} do
  $0
end" "it \"...\" do ... end")
(yas/define 'ruby-mode "bef" "before do
  $0
end" "before do ... end")
(yas/define 'elisp-mode "llt" "(let ($1)
  $0)" "(let (...) ...)" nil nil)
(yas/define 'elisp-mode "lkk" "(lookup-key ${1:global-map} (kbd \"$2\"))$0" "(lookup-key ...)")

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

(ly:eval-after-load "sequential-command"
  (define-key global-map "\C-x\C-y" 'yas/oneshot-snippet))

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

;; TABはauto-completeで使っているので<tab>へ変更
(setq yas/trigger-key "<tab>"
      yas/next-field-key '("<tab>"))
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
(yas/trigger-key-reload "TAB")
(yas/keymap-reload)
