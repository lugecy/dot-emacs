(require 'guess-style)
(defun ly:elisp-guess-style ()
  (when (and buffer-file-name
             (save-excursion
               (goto-char (point-min))
               (search-forward "\t" nil t)))
    (let ((message-log-max nil))
      (guess-style-guess-variable 'tab-width))))

(defun elisp-guess-style-setup ()
  (add-hook 'emacs-lisp-mode-hook 'ly:elisp-guess-style))
(add-hook 'after-init-hook 'elisp-guess-style-setup t)
