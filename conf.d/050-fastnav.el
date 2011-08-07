(require 'fastnav)
(defvar fastnav-sub-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-f") 'zap-to-char-forward)
    (define-key map (kbd "M-F") 'zap-to-char-backward)
    (define-key map "\M-z" 'zap-up-to-char-forward)
    (define-key map "\M-Z" 'zap-up-to-char-backward)
    (define-key map "\M-s" 'jump-to-char-forward)
    (define-key map "\M-S" 'jump-to-char-backward)
    (define-key map "\M-r" 'replace-char-forward)
    (define-key map "\M-R" 'replace-char-backward)
    (define-key map "\M-i" 'insert-at-char-forward)
    (define-key map "\M-I" 'insert-at-char-backward)
    (define-key map "\M-j" 'execute-at-char-forward)
    (define-key map "\M-J" 'execute-at-char-backward)
    (define-key map "\M-k" 'delete-char-forward)
    (define-key map "\M-K" 'delete-char-backward)
    (define-key map "\M-m" 'mark-to-char-forward)
    (define-key map "\M-M" 'mark-to-char-backward)
    map)
  "Fastnav keymap.")
(global-set-key (kbd "M-z") fastnav-sub-map)

(defun zap-to-char-forward (arg)
  "Kill text to the ARG'th occurence of a character queried
interactively."
  (interactive "p")
  (let ((args (highlight-read-char "Zap up to char: " arg
				   'zap-up-to-char-forward
				   'zap-up-to-char-backward)))
    (delete-region (point)
		   (progn
		     (apply 'search-char-forward args)
		     (1+ (point))))))

(defun zap-to-char-backward (arg)
  "Kill text backward up to the ARG'th occurence of a character
queried interactively."
  (interactive "p")
  (let ((args (highlight-read-char-backward "Zap up to char backward: " arg
					    'zap-up-to-char-forward
					    'zap-up-to-char-backward)))
    (delete-region (point)
		   (progn
		     (apply 'search-char-backward args)
		     (1- (point))))))

