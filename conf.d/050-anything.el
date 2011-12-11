(require 'anything-config)
(require 'anything-match-plugin)
(require 'anything-complete)
(require 'anything-show-completion)
(setq anything-candidate-number-limit 100) ; default 50
(define-key isearch-mode-map [?\C-:] 'anything-for-current-buffer)
(define-key anything-map "\C-r" 'anything-execute-persistent-action)
(define-key anything-map (kbd "C-M-p") 'anything-previous-source)
(define-key anything-map (kbd "C-M-n") 'anything-next-source)
(define-key anything-map (kbd "<C-return>") 'anything-select-3rd-action)
(define-key anything-map (kbd "C-j") 'anything-exit-minibuffer) ; auto-completeからの流れ
(define-key anything-map (kbd "C-M-j") 'anything-select-3rd-action)
(define-key anything-map (kbd "C-l") 'anything-next-source)
(define-key anything-map (kbd "M-t") 'anything-toggle-resplit-window)
(setq anything-enable-shortcuts 'prefix)
(define-key anything-map (kbd "@") 'anything-select-with-prefix-shortcut)
(remove-hook 'kill-emacs-hook 'anything-c-adaptive-save-history)

(defun anything-at-point-raw (&optional any-sources any-input any-prompt any-resume any-preselect any-buffer)
  "Same as `anything' except when C-u is pressed, the initial input is the symbol at point."
  (interactive)
  (anything any-sources
            (if current-prefix-arg
                (thing-at-point 'symbol) ; Modify
              any-input)
            any-prompt any-resume any-preselect any-buffer))

(defvar anything-for-open-source-list
  '(anything-c-source-ffap-guesser
    anything-c-source-buffers-list
    anything-c-source-escreen-for-open
    anything-c-source-historyf
    anything-c-source-recentf
    anything-c-source-files-in-current-dir+
    anything-c-source-bookmarks))
(add-to-list 'anything-c-source-buffers-list '(candidate-number-limit . 30) t)
(setq anything-c-source-recentf         ; enable shortcuts
      (delete-if (lambda (pair) (eq (car pair) 'disable-shortcuts))
                 anything-c-source-recentf))
(defun anything-for-open ()
  (interactive)
  (anything-at-point-raw anything-for-open-source-list nil nil nil nil
                         "*anything for open*"))

;; for anything-complete
(anything-lisp-complete-symbol-set-timer 150)
(anything-read-string-mode 1)

;; anything-select-2nd-action-or-end-of-line がfileとbufferで同じ物を選択するように変更
(anything-c-arrange-type-attribute 'buffer
  `((action
     ("Switch to buffer" . switch-to-buffer)
     ("Switch to buffer other window" . switch-to-buffer-other-window)
     ,(when (featurep 'escreen)
        '("Display buffer in escreen" . anything-find-buffer-on-escreen))

     ("Switch to buffer other frame" . switch-to-buffer-other-frame)
     REST)))
(anything-c-arrange-type-attribute 'file
  `((action
     ("Find file" . anything-find-many-files)
     ("Find file other window" . find-file-other-window)
     ,(when (featurep 'escreen)
        '("Find file in escreen" . anything-escreen-find-file))
     ("Find file other frame" . find-file-other-frame)
     REST)))

;;;; for escreen.el
(defun anything-find-buffer-on-escreen (candidate)
  (escreen-create-screen)
  (switch-to-buffer (get-buffer candidate)))
(defun anything-escreen-find-file (candidate)
  (escreen-create-screen)
  (find-file candidate))

;;;; anything for programing src jump
(defvar anything-elisp-apropos-sources '(anything-c-source-imenu
                                         anything-c-source-apropos-emacs-commands
                                         anything-c-source-apropos-emacs-functions
                                         anything-c-source-apropos-emacs-variables
                                         anything-c-source-info-elisp
                                         anything-c-source-info-cl
                                         anything-c-source-info-pages
                                         ))
(defun anything-elisp-apropos ()
  "anything for programing source jump"
  (interactive)
  (require 'info)
  (let ((anything-input-idle-delay 0.2))
    (anything-at-point anything-elisp-apropos-sources
                       nil nil nil nil "*anything elisp apropos")))

(defun anything-man-pages ()
  (interactive)
  (anything-other-buffer 'anything-c-source-man-pages "*anything manpages"))

;;;; anything-for-current-buffer-sources
(defvar anything-for-current-buffer-sources '(anything-c-source-imenu
                                              anything-c-source-bm
                                              anything-c-source-mark-ring
                                              anything-c-source-emacs-lisp-toplevels
                                              anything-c-source-occur
                                              ))
(defun anything-for-current-buffer ()
  (interactive)
  (let ((anything-candidate-number-limit 2000)
        (input-string (if isearch-mode isearch-string nil)))
    (if isearch-mode (isearch-exit))
    (anything-at-point anything-for-current-buffer-sources input-string
                       nil nil nil "*anything cur-buf*")))
(defun anything-for-current-buffer-with-prefix ()
  (interactive)
  (let ((current-prefix-arg t)
        (anything-for-current-buffer-sources (append anything-for-current-buffer-sources
                                                     (cond ((memq major-mode '(emacs-lisp-mode lisp-interaction-mode))
                                                            '(anything-c-source-emacs-variable-at-point anything-c-source-emacs-function-at-point))
                                                           ((memq major-mode '(c-mode ruby-mode cperl-mode))
                                                            (when (anything-c-etags-get-tag-file)
                                                              '(anything-c-source-etags-select)))
                                                           (t nil)))))
    (anything-for-current-buffer)))

(defadvice anything-c-adaptive-store-selection (around anything-c-adaptive-store-selection-filter activate)
  (let* ((source (anything-get-current-source))
         (source-name (or (assoc-default 'type source)
                          (assoc-default 'name source))))
    (unless (equal source-name "Occur")
      ad-do-it)))

;; 元のanything-occurの実装ではcurrent-bufferのundo-listにゴミが混じるので
;; それを修正するためにミラーバッファを用意する
;; redefine
(defun anything-c-occur-init ()
  (let ((buffer (get-buffer-create " *anything-occur-mirror-buffer*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert-buffer anything-current-buffer))
    (anything-candidate-buffer buffer)))

;; バッファにmarkが無い場合にerrorが出ないようにする (redefine)
(defun anything-c-source-mark-ring-candidates ()
  (flet ((get-marks (pos)
                    (save-excursion
                      (goto-char pos)
                      (beginning-of-line)
                      (let ((line  (car (split-string (thing-at-point 'line) "[\n\r]"))))
                        (when (string= "" line)
                          (setq line  "<EMPTY LINE>"))
                        (format "%7d: %s" (line-number-at-pos) line)))))
    (with-current-buffer anything-current-buffer
      (loop
       with marks = (if (marker-position (mark-marker))
                        (cons (mark-marker) mark-ring)
                      mark-ring)
       with recip = nil
       for i in marks
       for m = (get-marks i)
       unless (member m recip)
       collect m into recip
       finally return recip))))

;;;; descbinds-anything.el
(require 'descbinds-anything)
(descbinds-anything-install)

;;; bindings all-view
(defadvice descbinds-anything (around unlimited-descbinds-anything activate)
  "disable anything-candidate-number-limit."
  (let ((anything-candidate-number-limit nil))
    ad-do-it))

;;;; for etags
(autoload 'anything-etags-select "anything-etags" nil t)
(autoload 'anything-etags-select-from-here "anything-etags" nil t)
(autoload 'anything-yaetags-find-tag "anything-yaetags" nil t)

;;;; for ipa
(autoload 'anything-ipa "anything-ipa" nil t)
(autoload 'anything-ipa-global "anything-ipa" nil t)

;;;; 普段は左端にanything-windowが表示されるようにしてみた
;;;; 横が狭い場合には縦にsplit-rootする
(require 'split-root)
(defvar anything-compilation-window-height-percent 35.0)
(defvar anything-compilation-window-minwidth 50)
(defun anything-compilation-window-root (buf)
  (if (minibufferp)
      (anything-default-display-buffer buf)
    (let ((height (frame-height)) (width (frame-width)) (percent (/ anything-compilation-window-height-percent 100.0))
          win args)
      (if (> width (* height 2))
          (setq args (list (max (truncate (* width percent))
                                anything-compilation-window-minwidth)
                           t t))
        (setq args (list (truncate (* height percent)) nil nil)))
      (setq win (apply 'split-root-window args))
      (set-window-buffer win buf))))
(setq anything-display-function 'anything-compilation-window-root)

;;;; 入力を保存 / 色付けしたkill-bufferソース
(defun anything-kill-buffers (&optional init-input)
  "Preconfigured `anything' to kill buffer you selected."
  (interactive)
  (anything
   '(((name . "Kill Buffers")
      (candidates . anything-c-buffer-list)
      (candidate-transformer anything-c-highlight-buffers)
      (action
       ("Kill Buffer" . (lambda (candidate)
                          (kill-buffer candidate)
                          (anything-kill-buffers anything-input))))
      ))
   init-input nil))

;;;; anything-el-swank-fuzzy
(require 'anything-el-swank-fuzzy)
;; 初回実行時のエラー回避(どこのバグ？)
(defadvice aeswf-complete-symbol-meta-source-init (before ly:bugfix activate)
  (get-buffer-create "*anything complete*"))

;;;; at-point-rawの代替手段？
(defun ly:anything-toggle-word-wrap ()
  (interactive)
  (when (minibufferp)
    (save-excursion
      (goto-char (minibuffer-prompt-end))
      (if (looking-at (regexp-quote "\\b"))
          (while (re-search-forward "\\\\b ?" nil t)
            (replace-match ""))
        (insert "\\b")
        (forward-sexp)
        (insert "\\b")))))
(define-key anything-map (kbd "M-w") 'ly:anything-toggle-word-wrap)

;;;; fix anything-etag(for pop-tag-mark)
(defadvice anything-c-etags-default-action (before for-pop-tag-mark-fix activate)
  (require 'etags)
  (with-current-buffer anything-current-buffer
    (ring-insert find-tag-marker-ring (point-marker))))

;;;; gnupack-emacs上でanything-windowが再描写されないバグを回避
;;;; redefine
(when (eq system-type 'windows-nt)
  (defun anything-show-candidate-number (&optional name)
    "Used to display candidate number in mode-line (bugfix redefine)."
    (with-anything-window
      (propertize
       (format "[%s %s]"
               (anything-approximate-candidate-number 'in-current-source)
               (or name "Candidate(s)"))
       'face 'anything-candidate-number))))
