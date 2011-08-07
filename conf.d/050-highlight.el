;;;; highlight series
(require 'highlight-symbol)
(global-set-key (kbd "C-c s s") 'highlight-symbol-at-point)
(global-set-key (kbd "C-c s [") 'highlight-symbol-prev)
(global-set-key (kbd "C-c s ]") 'highlight-symbol-next)
(global-set-key (kbd "C-c s r") 'highlight-symbol-query-replace)
(global-set-key (kbd "C-c s p") 'highlight-symbol-jump-prev-hi-lock-symbol)
(global-set-key (kbd "C-c s n") 'highlight-symbol-jump-next-hi-lock-symbol)
(global-set-key (kbd "C-c s j") 'highlight-symbol-walk-mode)

;;;; 前後のhighlight-symbolされている箇所へjump
(defun highlight-symbol-jump-next-hi-lock-symbol ()
  (interactive)
  (highlight-symbol-jump-hilock-pos 1))

(defun highlight-symbol-jump-prev-hi-lock-symbol ()
  (interactive)
  (highlight-symbol-jump-hilock-pos -1))

(defun highlight-symbol-jump-hilock-pos (dir)
  (let ((regexp (highlight-symbol-make-jump-regexp)))
    (if regexp
        (let ((target (progn (forward-char dir) ;for now point on target
                             (re-search-forward regexp nil t dir))))
          (unless target
            (goto-char (if (< 0 dir) (point-min) (point-max)))
            (setq target (re-search-forward regexp nil nil dir)))
          (when target
            (let* ((target-str (match-string-no-properties 0))
                   (offset (if (< 0 dir) (length target-str) 0))
                   )
              (goto-char (- target offset))))))))

(defun highlight-symbol-make-jump-regexp ()
  (when highlight-symbol-list
    (let* ((replace-str-list (list (car highlight-symbol-border-pattern)
                                   (cdr highlight-symbol-border-pattern)))
           (raw-symbol-list (mapcar (lambda (str)
                                      (replace-regexp-in-string (regexp-opt replace-str-list)
                                                                ""
                                                                str))
                                    highlight-symbol-list)))
      (regexp-opt raw-symbol-list))))

;;;; highlight-された箇所を徘徊するためのmode
(defvar highlight-symbol-walk-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "M-[") 'highlight-symbol-prev)
    (define-key keymap (kbd "M-]") 'highlight-symbol-next)
    (define-key keymap (kbd "M-p") 'highlight-symbol-jump-prev-hi-lock-symbol)
    (define-key keymap (kbd "M-n") 'highlight-symbol-jump-next-hi-lock-symbol)
    (define-key keymap (kbd "M-RET") 'highlight-symbol-walk-mode)
    keymap))

(define-minor-mode highlight-symbol-walk-mode
  "For highlight-symbol point walk around mode."
  :lighter " Hi-Walk"
  (if highlight-symbol-walk-mode
      (unless highlight-symbol-list
        (message "No exists highlight-symbol")
        (highlight-symbol-walk-mode -1))))

;;;; for replace in defun
(defadvice highlight-symbol-query-replace (around for-in-defun activate)
  "highlight-symbol-query-replace only defun region."
  (if current-prefix-arg
      (save-restriction
        (narrow-to-defun)
        ad-do-it)
    ad-do-it))

;;;;
(require 'highlight)
(global-set-key (kbd "C-c s l") 'hlt-highlight)

(require 'highlight-parentheses)
(custom-set-variables '(hl-paren-colors '("firebrick1" "yellow" "ForestGreen" "orange4")))
(add-hook 'emacs-lisp-mode-hook 'highlight-parentheses-mode)
(add-hook 'lisp-interaction-mode-hook 'highlight-parentheses-mode)
(ly:eval-after-load 'ielm
  (add-hook 'inferior-emacs-lisp-mode-hook 'highlight-parentheses-mode))
