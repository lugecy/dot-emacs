;;;; ielm (include standard lisp library)
(safe-loading 'split-root)
(defvar ielm-last-buffer nil)
(defvar ielm-last-window nil)
(defvar ielm-last-window-configure nil)
(defun ielm-with-current-buffer (&optional args)
  "Pop up ielm-buffer, for eval-expression.
If argument is non-nil, means use basic eval-expression.
When argument is negative value, enable insert eval-expression."
  (interactive "p")
  (require 'ielm)
  (if current-prefix-arg
      (progn
        (if (>= args 0) (setq current-prefix-arg nil))
        (call-interactively 'eval-expression))
    (let ((buf (current-buffer))
          (w (get-buffer-window "*ielm*")))
      (if (equal "*ielm*" (buffer-name))
          ;; pop-out
          (progn
            (set-window-configuration ielm-last-window-configure)
            (and (window-live-p ielm-last-window) (select-window ielm-last-window)
                 (buffer-live-p ielm-last-buffer) (switch-to-buffer ielm-last-buffer)))
        ;; pop-in
        (setq ielm-last-buffer buf
              ielm-last-window (selected-window))
        (if w
            (select-window w)
          ;; emulate ielm function (for ewm.el pop-to-buffer advice)
          (setq ielm-last-window-configure (current-window-configuration))
          (let (old-point)
            (unless (comint-check-proc "*ielm*")
              (with-current-buffer (get-buffer-create "*ielm*")
                (unless (zerop (buffer-size)) (setq old-point (point)))
                (inferior-emacs-lisp-mode)))
            (pop-up-split-root-window "*ielm*" 0.2)
            (set (make-local-variable 'ac-menu-height) (min (1- (/ (window-height) 2)) ac-menu-height))
            (when old-point (push-mark old-point))))
        (goto-char (point-max))
        (ielm-change-working-buffer buf)))))
(global-set-key (kbd "M-:") 'ielm-with-current-buffer)

(defun lugecy-setup-ielm-mode ()
  "Configure function for ielm-mode"
  (setq comint-input-ignoredups t
        comint-input-ring-size 1000)
  (setq ac-sources '(ac-source-words-in-buffer+ ac-source-symbols))
  (and (featurep 'yasnippet) (add-to-list 'ac-sources 'ac-source-yasnippet))
  (paredit-mode t)
  (when ielm-mode-input-history
    (setq comint-input-ring (let ((ring (make-ring comint-input-ring-size))
                                  (seq (reverse ielm-mode-input-history)))
                              (dotimes (count (length seq))
                                (when (or (ring-empty-p ring)
                                          (not (equal (ring-ref ring 0) (elt seq count))))
                                  (ring-insert ring (elt seq count))))
                              ring)))
  (when (featurep 'anything)
    (define-key inferior-emacs-lisp-mode-map (kbd "C-c C-:") 'anything-ielm-history))
  (when (featurep 'anything-el-swank-fuzzy)
    (define-key inferior-emacs-lisp-mode-map (kbd "M-TAB") 'anything-dwim-complete-elisp-symbol)))
(add-hook 'ielm-mode-hook 'lugecy-setup-ielm-mode)

;;;; font-lock configure
(font-lock-add-keywords 'inferior-emacs-lisp-mode
                        '((ly:elisp-font-lock-function
                           (1 'my-face-elisp-macro nil t)
                           (2 'my-face-elisp-subr nil t)
                           (3 'my-face-elisp-pri-func nil t)
                           (4 'my-face-elisp-func nil t)))
                        t)
(add-hook 'ielm-mode-hook 'highlight-cl-add-font-lock-keywords)

;;;; for viper
(eval-after-load "viper"
  `(progn
     (setq viper-insert-state-mode-list (delete 'inferior-emacs-lisp-mode viper-insert-state-mode-list))
     (add-to-list 'viper-emacs-state-mode-list 'inferior-emacs-lisp-mode)))

;;;; save history
(defvar ielm-mode-input-history nil)    ; -historyで終わるsymbolをsession.elは収集する
(defadvice ielm-send-input (after save-history activate)
  "advice-document"
  (ielm-sync-input-histroy (substring-no-properties (ring-ref comint-input-ring 0))
                           'ielm))

(defadvice eval-expression (before share-ielm-history activate)
  "advice-document"
  (let ((new-element ))
    (ielm-sync-input-histroy (format "%S" (ad-get-arg 0))
                             'eval-expression)))

(defun ielm-sync-input-histroy (element from)
  (cond ((eq from 'ielm)
         (setq read-expression-history (cons element (delete element read-expression-history))))
        ((eq from 'eval-expression)
         (when (get-buffer "*ielm*")
           (with-current-buffer "*ielm*"
             (comint-add-to-input-history element)))))
  (setq ielm-mode-input-history (cons element (delete element ielm-mode-input-history))))

;;;; auto-complete insert
(add-to-list 'ac-modes 'inferior-emacs-lisp-mode t)

;;;; for anything
(defvar anything-source-ielm-input-history
  '((name . "Inferier Emacs Lisp Mode Input History")
    (candidates . ielm-mode-input-history)
    (candidate-number-limit . 1000)
    (action . insert)
    (multiline)))
(defun anything-ielm-history ()
  "Function documantion."
  (interactive)
  (anything '(anything-source-ielm-input-history)))

;;;; frameの底にwindowを表示する
(defun pop-up-split-root-window (buffer &optional height)
  (let ((root-win (split-root-window (max (truncate (* (frame-height)
                                                       (or height 0.3)))
                                          5))))
    (when root-win
      (set-window-buffer root-win buffer)
      (select-window root-win))))

;;;; ielmでpp-to-stringの代わりにcl-prettyprintを使う
;;;; listを一行一要素で表示させたい
(defun cl-pp-to-string (form)
  (with-temp-buffer
    (cl-prettyprint form)
    (buffer-string)))

(defadvice ielm-eval-input (around pp-use-cl-pp activate)
  (letf (((symbol-function 'pp-to-string)
          (symbol-function 'cl-pp-to-string)))
    ad-do-it))

