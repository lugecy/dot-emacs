(require 'paredit)
(dolist (hook '(emacs-lisp-mode-hook lisp-interaction-mode-hook))
  (add-hook hook 'enable-paredit-mode))

;;;; paredit-with-viper
(eval-after-load "viper"
  '(progn
	 (defadvice paredit-mode (after viper-compat activate)
	   (if paredit-mode
		   (progn
			 (viper-add-local-keys 'insert-state `((,(kbd "C-d") . paredit-forward-delete)
												   (,(kbd "<backspace>") . paredit-backward-delete))))
		 (viper-add-local-keys 'insert-state `((,(kbd "C-d") . nil)
											   (,(kbd "<backspace>") . nil)))))))

