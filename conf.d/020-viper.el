;;;; preload configure
(setq viper-mode t)
(setq viper-inhibit-startup-message t)
(setq viper-expert-level 5)
(setq viper-ex-style-motion nil)
(setq viper-syntax-preference 'extended)  ;; default reformed-vi

(require 'viper)
(setq viper-vi-style-in-minibuffer nil) ;; minibufferではEmacsモードに
(setq viper-vi-state-mode-list (delq 'diff-mode viper-vi-state-mode-list))
(add-to-list 'viper-emacs-state-mode-list 'diff-mode t)
;; configure keymap
(define-key viper-insert-global-user-map "\C-l" 'viper-exit-insert-state)
(define-key viper-insert-global-user-map "\C-d" 'delete-char)
(define-key viper-vi-global-user-map "\C-f" 'forward-char)
(define-key viper-vi-global-user-map "\C-b" 'backward-char)
(define-key viper-vi-global-user-map (kbd "C-u") 'universal-argument) ; default: viper-scroll-down
(define-key viper-vi-global-user-map "\C-e" 'move-end-of-line) ; default: viper-scroll-up-one
;; u/d SPC/n/b を scrollに
(define-key viper-vi-global-user-map (kbd "u") 'viper-scroll-down)
(define-key viper-vi-global-user-map (kbd "b") 'viper-scroll-screen-back)
(define-key viper-vi-global-user-map (kbd "SPC") 'viper-scroll-screen)
(define-key viper-vi-global-user-map (kbd "n") 'viper-scroll-screen)

;; for vimpulse
(setq vimpulse-want-vi-keys-in-buffmenu nil
      vimpulse-want-vi-keys-in-dired nil
      vimpulse-want-vi-keys-in-Info nil
      vimpulse-want-vi-keys-in-help nil)
(setq vimpulse-enhanced-paren-matching nil)
(require 'vimpulse)
(define-key viper-vi-basic-map (kbd "TAB") nil)
(define-key viper-vi-basic-map (kbd "C-o") nil)
(ly:eval-after-load 'undo-tree
  (define-key undo-tree-visualizer-map [remap undo-tree-visualizer-scroll-left] nil)
  (define-key undo-tree-visualizer-map [remap undo-tree-visualizer-scroll-right] nil))
(defun ly:viper-remap-from-global-map (keymap key)
  (define-key keymap key (lookup-key global-map key)))
(defun ly:init-viper-keymap ()
  (ly:viper-remap-from-global-map viper-vi-global-user-map (kbd "C-w"))
  (ly:viper-remap-from-global-map viper-vi-global-user-map (kbd "C-v"))
  (ly:viper-remap-from-global-map viper-vi-global-user-map (kbd "C-p"))
  (ly:viper-remap-from-global-map viper-vi-global-user-map (kbd "C-r"))
  (ly:viper-remap-from-global-map viper-vi-global-user-map (kbd "C-t"))
  (ly:viper-remap-from-global-map viper-insert-global-user-map (kbd "C-p"))
  (ly:viper-remap-from-global-map viper-insert-global-user-map (kbd "C-n")))
(add-hook 'after-init-hook 'ly:init-viper-keymap)

;; ;; for text-obj
;; ;; http://www.geocities.jp/emacsjjjj/viper/
;; ;; のpatchからテキストオブジェ部分だけを抽出，advice定義に直したもの
;; (require 'viper-textobj)
;; ce-scrollかつvisual-mode-linewiseの時のカーソル移動できない対策
;; (remove-hook 'post-command-hook '(lambda ()
;;   			(if (and viper-visual-mode viper-visual-mode-linewise)
;;   			    (beginning-of-line))))

;; (eval-after-load "skk"
;;  #'(skk-wrap-newline-command vimpulse-enter-indents)) ;; skk-henkan-mode中のEnter未実行問題対策
(eval-after-load "ce-scroll"
  #'(progn
      (define-key viper-vi-global-user-map "j" 'ce-next-line) ; default: viper-next-line
      (define-key viper-vi-global-user-map "k" 'ce-previous-line) ; default: viper-previous-line
      (define-key viper-vi-global-user-map (kbd "C-v") 'ce-scroll-up))) ; default find-file-other-frame

;;;; for edebug
(defmacro with-viper-state (state &rest body)
  (declare (indent 1))
  `(let ((orig-state (substring (symbol-name viper-current-state) 0 -6)))
     (when (and (boundp viper-mode) viper-mode
                (not (eq viper-current-state (quote ,(intern-soft (concat (symbol-name state) "-state"))))))
       (,(intern (concat "viper-change-state-to-" (symbol-name state)))))
     (unwind-protect
         ,@body
       (when (and (boundp viper-mode) viper-mode
                  (not (eq viper-current-state (intern-soft (concat orig-state "-state")))))
         (funcall (intern-soft (concat "viper-change-state-to-" orig-state)))))))

(defadvice edebug-recursive-edit (around for-viper-collaborate activate)
  (with-viper-state emacs
    ad-do-it))

;;;; [2010-10-13] 左クリック->左クリックによってtransient-mark-modeがoffになるのを防止
;; transient-mark-mode = (only . t) な場合に対応していないのが原因
(defadvice vimpulse-transient-remember (after fix-mouse-drag-region activate)
  (when (and (eq this-command 'mouse-drag-region)
             (eq (car-safe transient-mark-mode) 'only))
    (setq vimpulse-visual-vars-alist
          (assq-delete-all 'transient-mark-mode vimpulse-visual-vars-alist))))

;;;; 現在のファイル・カーソル位置の詳細を表示する
(define-key help-map (kbd "<f2>") 'viper-info-on-file)
