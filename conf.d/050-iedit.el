(require 'iedit)
(global-set-key (kbd "C-;") 'iedit-mode)

(defun iedit-mode-in-defun (&optional arg)
  "Function documantion."
  (interactive "P")
  (save-restriction
    (widen)
    (narrow-to-defun)
    (iedit-mode arg)))

(defadvice iedit-mode (around for-in-defun activate)
  "for iedit-mode-in-defun."
  (if (eq (car current-prefix-arg) 16)
      (progn
        (setq current-prefix-arg nil)
        (iedit-symbol-mode))
    ad-do-it))

(defun iedit-symbol-mode (&optional arg)
  (interactive "P")
  (iedit-mode arg (lambda (exp)
                    (let (overlays)
                      (goto-char (point-min))
                      (while (re-search-forward (concat "\\_<" exp "\\_>") nil t)
                        (push (iedit-make-occurrence-overlay (match-beginning 0) (match-end 0))
                              overlays))
                      (nreverse overlays)))))
