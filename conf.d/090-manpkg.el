(defvar ly:manpkg-target "ly:manpkg-local-elisp")
(defvar ly:manpkg-origin "ly:manpkg-elisp")
(defvar ly:manpkg-ancestor "ly:manpkg-ancestor")
(defvar ly:manpkg-site-lisp-dirname "elisp/")
(defvar ly:manpkg-local-lisp-dirname "local-elisp/")
(defvar ly:manpkg-origin-branch "master")
(defvar ly:auto-install-exclude-path (expand-file-name (substring ly:manpkg-local-lisp-dirname 0 -1) user-emacs-directory))

(defadvice auto-install-get-path (around ainst-custom activate)
  (let ((load-path (remove-if (lambda (path) 
                                (eq (compare-strings path nil (length ly:auto-install-exclude-path)
                                                     ly:auto-install-exclude-path nil nil) t))
                              load-path)))
    ad-do-it))

(defun ly:manpkg-get-buffer (fname bufname)
  (with-current-buffer (get-buffer-create bufname)
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert-file-contents fname)
    (current-buffer)))

(defun ly:manpkg-merge-cleanup ()
  (dolist (bufname (list ly:manpkg-target ly:manpkg-origin ly:manpkg-ancestor))
    (when (get-buffer bufname)
      (let ((win (get-buffer-window bufname)))
        (when win
          (delete-window win))
        (kill-buffer bufname))))
  (remove-hook 'ediff-quit-merge-hook 'ly:manpkg-merge-cleanup))

(defun ly:manpkg-merge-to-local-elisp-1 (path rev)
  (require 'ediff)
  (let* ((ancestor-buf (get-buffer-create ly:manpkg-ancestor))
         (target-file (concat ly:manpkg-local-lisp-dirname path))
         (origin-file (concat ly:manpkg-site-lisp-dirname path))
         (default-directory (expand-file-name user-emacs-directory))
         target-buf origin-buf)
    (when (eq (shell-command (format "git show %s:%s" rev origin-file) ancestor-buf) 0)
      (setq target-buf (ly:manpkg-get-buffer target-file ly:manpkg-target)
            origin-buf (ly:manpkg-get-buffer origin-file ly:manpkg-origin))
      (ediff-merge-buffers-with-ancestor target-buf origin-buf ancestor-buf nil nil target-file)
      (add-hook 'ediff-quit-merge-hook 'ly:manpkg-merge-cleanup))))

(defun ly:manpkg-merge-to-local-elisp (path rev)
  (interactive
   (list (read-file-name "path: " (expand-file-name ly:manpkg-site-lisp-dirname user-emacs-directory) nil t)
         (read-string (format "rev(default: %s): " ly:manpkg-origin-branch) nil nil ly:manpkg-origin-branch)))
  (ly:manpkg-merge-to-local-elisp-1
   (substring (expand-file-name path) (length (expand-file-name ly:manpkg-site-lisp-dirname user-emacs-directory)))
   rev))

(defun ly:manpkg-diff-local-elisp (local-path)
  (interactive
   (list (read-file-name "path: " (expand-file-name ly:manpkg-local-lisp-dirname user-emacs-directory) nil t)))
  (let* ((postfix (substring (expand-file-name local-path) (length (expand-file-name ly:manpkg-local-lisp-dirname user-emacs-directory))))
         (site-path (concat (expand-file-name ly:manpkg-site-lisp-dirname user-emacs-directory) postfix)))
    (ediff-files site-path local-path)))

(defadvice auto-install-buffer-save (after check-local-elisp activate)
  (when buffer-file-name
    (let ((filename (file-name-nondirectory buffer-file-name))
          (ly:auto-install-exclude-path "dummy"))
      (when (and filename
                 (string-match ly:manpkg-local-lisp-dirname (auto-install-get-path filename)))
        (message "Need to merge files in local-elisp!!!")))))
