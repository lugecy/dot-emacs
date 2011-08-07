;;;; eldoc.el (elisp関数の引数を簡易表示)
(autoload 'turn-on-eldoc-mode "eldoc" nil t)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(eval-after-load "eldoc"
  '(set-face-foreground 'eldoc-highlight-function-argument "Salmon"))

;;;; eldocのトリガーコマンドを追加
(eval-after-load "eldoc"
  '(progn
     (eldoc-add-command-completions "ce-")
     (eval-after-load "viper"
       '(eldoc-add-command-completions "viper-forward-" "viper-backward-"))))

;;;; elisp関数を分かりやすく色付け
(defface my-face-elisp-macro '((t (:foreground "sea green"))) nil)
(defface my-face-elisp-subr '((t (:foreground "cyan"))) nil)
(defface my-face-elisp-pri-func '((t (:foreground "light sky blue"))) nil)
(defface my-face-elisp-func '((t (:foreground "deep sky blue"))) nil)

;; borrowed from highlight-cl.el
(defun ly:elisp-macro-p (def)
  (let ((key (car-safe def)))
    (when key
      (or (eq key 'macro)
          (and (eq key 'autoload)
               (memq (nth 4 def) '(macro t)))))))

(defun ly:symbol-advised-p (sym)
  (and (symbolp sym) (featurep 'advice)
       (ad-get-advice-info sym)))

(defun ly:symbol-real-function-sym (function)
  (let ((adviced (ly:symbol-advised-p function)))
    (or (and adviced
             (let ((origname (cdr (assq 'origname adviced))))
               (and (fboundp origname) origname)))
        (and (fboundp function)         ;for alias
             (symbolp (symbol-function function))
             (ly:symbol-real-function-sym (symbol-function function)))
        function)))

(defun ly:elisp-font-lock-function (limit)
  (let (find-flg)
    (while (and (not find-flg)
                (re-search-forward "['(]\\([^() \n]+\\)" limit t))
      (and (not (memq (get-text-property 0 'face (match-string 1))
                      '(font-lock-comment-face font-lock-warning-face)))
           (let ((sym-func (condition-case nil
                               (symbol-function
                                (ly:symbol-real-function-sym (intern-soft (match-string-no-properties 1))))
                             (error nil))))
             (and sym-func
                  (setq find-flg (cond ((ly:elisp-macro-p sym-func) 1)
                                       ((subrp sym-func) 2)
                                       ((byte-code-function-p sym-func) 3)
                                       ((functionp sym-func) 4)
                                       (t nil)))))
           (and find-flg
                (let ((new-match-list (nconc (list (match-beginning 0) (match-end 0))
                                             (make-list (* (1- find-flg) 2) nil)
                                             (list (match-beginning 1) (match-end 1) (current-buffer)))))
                  (set-match-data new-match-list)))))
    find-flg))

(mapc (lambda (mode)
        (font-lock-add-keywords mode
                                '((ly:elisp-font-lock-function
                                   (1 'my-face-elisp-macro nil t)
                                   (2 'my-face-elisp-subr nil t)
                                   (3 'my-face-elisp-pri-func nil t)
                                   (4 'my-face-elisp-func nil t)))
                                t))
      '(lisp-interaction-mode emacs-lisp-mode))

;;;; cl-package関数・マクロに下線を
(require 'highlight-cl)
(add-hook 'emacs-lisp-mode-hook 'highlight-cl-add-font-lock-keywords)
(add-hook 'lisp-interaction-mode-hook 'highlight-cl-add-font-lock-keywords)

;;;; anything-lisp-complete-symbol
(ly:eval-after-load 'anything-el-swank-fuzzy
  (dolist (keymap (list emacs-lisp-mode-map lisp-interaction-mode-map))
    (define-key keymap (kbd "M-TAB") 'anything-dwim-complete-elisp-symbol)))
(defun anything-dwim-complete-elisp-symbol ()
  (interactive)
  (let ((start (car (bounds-of-thing-at-point 'symbol))))
    (call-interactively (if (and start
                                 (>= (- (point) start) 3))
                            'anything-el-swank-fuzzy-complete-symbol
                          'anything-lisp-complete-symbol))))

;;;; 自作のload関数のfeature部の色付け
(mapc (lambda (mode)
        (font-lock-add-keywords mode
                                '(("(\\(safe-loading\\|with-loading\\)\\>[ 	']*\\(\\sw+\\)?"
                                   (2 font-lock-constant-face nil t)))))
      '(lisp-interaction-mode emacs-lisp-mode))

;;;; escape-sequence font-lock
(add-hook 'emacs-lisp-mode-hook 'ly:escape-seq-char-fontlock-setup)
(add-hook 'lisp-interaction-mode-hook 'ly:escape-seq-char-fontlock-setup)

;;;; point位置のelisp function or variable の定義位置に移動する
(ly:eval-after-load 'thingatpt+
  (dolist (keymap (list emacs-lisp-mode-map lisp-interaction-mode-map))
    (define-key keymap (kbd "C-c C-.") 'find-fn-or-var-nearest-point)))
