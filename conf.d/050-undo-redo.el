;;;; undoの回数を増やす
(setq undo-limit 100000) ; default: 20000
(setq undo-strong-limit 130000) ; default: 30000

;;;; redo.el
(require 'redo+ nil t)

;;;; point-undo.el configure
(require 'point-undo)

;;;; undo-tree.el
(require 'undo-tree)
(define-key undo-tree-map (kbd "C-.") 'undo-tree-redo)
;; for enable C-x r map (register jump, etc...)
(define-key undo-tree-map (kbd "C-x r") nil)
;; undo-tree-visual-mode での keymap
(define-key undo-tree-visualizer-map (kbd "SPC") 'scroll-up)
(define-key undo-tree-visualizer-map (kbd "<backspace>") 'scroll-down)
(define-key undo-tree-visualizer-map (kbd "-") 'text-scale-adjust)
(define-key undo-tree-visualizer-map (kbd "=") 'text-scale-adjust)
;; collaborate viper
(eval-after-load "viper"
  '(add-to-list 'viper-emacs-state-mode-list 'undo-tree-visualizer-mode t))

;; for conserted viper-mode (non like Vi style undo)
;; Vi like なUndo(insertモードでの編集作業が一つのundoになる) にしたい場合は
;; remove-undo-mark ではなく viper-adjust-undo を使用するべし
(ly:eval-after-load 'viper
  (defun undo-tree-viper-remove-undo-mark ()
    "Remove viper-buffer-undo-list-mark from buffer-undo-list.
'(nil foo nil viper nil undo-tree-canary) => '(nil foo nil undo-tree-canary)"
    (when viper-undo-needs-adjustment
      (let ((inhibit-quit t))
        (setq viper-undo-needs-adjustment nil)
        (when (listp buffer-undo-list)
          (let (find-pos)
            (while (setq find-pos (position viper-buffer-undo-list-mark buffer-undo-list))
              (let ((before-list (subseq buffer-undo-list 0 (if (nth (1- find-pos) buffer-undo-list)
                                                                find-pos
                                                              (1- find-pos))))
                    (after-list (subseq buffer-undo-list (1+ find-pos))))
                (setq buffer-undo-list (nconc before-list after-list)))))))))

  (defadvice undo-list-transfer-to-tree (before remove-viper-mark activate)
    (when (boundp viper-undo-needs-adjustment)
      (undo-tree-viper-remove-undo-mark)))

  (defadvice viper-change-state-to-vi (before remove-viper-mark activate)
    (when (boundp viper-undo-needs-adjustment)
      (undo-tree-viper-remove-undo-mark))))

;; undo-tree 0.3 にも存在する transfer-to-tree の無限ループ bug 対策
(defadvice undo-list-pop-changeset (after care-infinite-loop activate)
  (when (equal buffer-undo-list '(undo-tree-canary))
    (setq buffer-undo-list (list nil 'undo-tree-canary))))
;; vimpulse の line-visual-mode との相性対策
(defadvice undo-tree-undo (before care-vimplus activate)
  (when (eq vimpulse-visual-mode 'line)
    (error "Forviden When vimpulse line visual mode on.")))
(defadvice undo-tree-redo (before care-vimplus activate)
  (when (eq vimpulse-visual-mode 'line)
    (error "Forviden When vimpulse line visual mode on.")))

;;;; uthist.el
;; (safe-loading "uthist")
;; (uthist-install)

;; (defvar uthist-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map (kbd "u") 'undo-tree-visualize)
;;     (define-key map (kbd "o") 'uthist-checkout)
;;     (define-key map (kbd "i") 'uthist-checkin)
;;     (define-key map (kbd "s") 'uthist-save)
;;     map))
;; ;; skk-mode中に"C-x u"にskk-undoがバインドされてしまい，uthist-map上のコマンドが実行されない問題に対処
;; (defun uthist-command-select ()
;;   (interactive)
;;   (let* ((char (read-event))
;;          (command (lookup-key uthist-map (vector char))))
;;     (if (commandp command)
;;         (call-interactively command)
;;       (let (message-log-max)
;;         (message "%s is undefined on uthist-map" (format-kbd-macro (this-command-keys-vector)))))))
;; (define-key undo-tree-map (kbd "C-x u") 'uthist-command-select)
;; (define-key undo-tree-visualizer-map (kbd "o") 'uthist-checkout)
;; (define-key undo-tree-visualizer-map (kbd "i") 'uthist-checkin)

(defadvice undo-tree-visualize (after undo-tree-text-adjust activate)
  (viper-change-state-to-emacs)
  (text-scale-increase -3)
  (recenter -5))

;;; 途中経過用のcheckin(":"付き)をまとめて削除
(defun uthist-remove-tmp-entry ()
  (interactive)
  (dolist (name (loop for k being the hash-keys in uthist-node-repo
                      if (string-match "^:" k)
                      collect k))
    (remhash name uthist-node-repo)))

;;;; auto-installでインストールした場合にuthisを発動させないようにする
(ly:eval-after-load 'auto-install
  (defadvice auto-install-buffer-save (before disable-uthist-save activate)
    "Disable when auto-install process buffer."
    (when (eq major-mode 'auto-install-minor-mode)
      (setq buffer-undo-list nil))))

;;;; space を undo の区切りにする
(defun ly:undo-space-boundary ()
  (when (and (symbolp this-command)
             (string-match "self-insert-command" (symbol-name this-command))
             (eq last-command-event ?\ ))
    (undo-boundary)))
(add-hook 'pre-command-hook 'ly:undo-space-boundary)

;;;; Undo-treeのmode-line表示をできるだけ前に
(defun ly:undo-tree-modeline-fix ()
  (let ((elm '(undo-tree-mode undo-tree-mode-lighter)))
    (setq minor-mode-alist (cons elm (delete elm minor-mode-alist)))))
(add-hook 'after-init-hook 'ly:undo-tree-modeline-fix t)

