(safe-loading 'magit)
(define-key magit-status-mode-map (kbd "w") 'magit-wazzup)
(define-key magit-status-mode-map (kbd "O") 'magit-log-all)
(define-key magit-mode-map (kbd "C-M-u") 'magit-goto-parent-section)
(setq magit-completing-read-function 'magit-builtin-completing-read)
(setq magit-global-map
      (let ((map (make-sparse-keymap)))
        (define-key map "i" 'magit-status)
        (define-key map "l" 'magit-log)
        map))
(global-set-key (kbd "C-c j") magit-global-map)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'magit-log-edit-mode t))
(ly:eval-after-load 'viper
  (dolist (mode '(magit-key-mode magit-show-branches-mode))
    (add-to-list 'viper-emacs-state-mode-list mode)))

(defadvice vc-next-action (around next-action-active-magit-status activate)
  "If vc-backend is Git, next-action exec magit-status."
  (if (or (eq 'Git (car-safe (or (and buffer-file-name (vc-deduce-fileset))
                                 (and (fboundp 'vc-dired-deduce-fileset) (vc-dired-deduce-fileset)))))
          (vc-find-root default-directory ".git"))
      (and (require 'magit nil t) (magit-status (magit-get-top-dir default-directory)))
    ad-do-it))

;;;; magit-log-all が廃止になったので自前で定義
(defun magit-log-all (&optional arg)
  (interactive "P")
  (magit-display-log arg "--all"))

;;;; confirmブランチへのmergeをワンステップで
(defun ly:magit-push-upstream-branch (upstream)
  "This branch merge to upstream-branch (confirm, pu etc..)"
  (interactive (list (completing-read (format "push to (default: %s): " "pu")
                                      '("pu" "confirm" "master") nil nil nil nil "pu")))
  (let ((cur-branch (magit-get-current-branch)))
    (unless (string= cur-branch upstream)
      (let ((magit-refresh-pending t))  ;refresh need onece
        (magit-checkout upstream))
      (magit-merge cur-branch))))
(define-key magit-mode-map (kbd "C-c C-u") 'ly:magit-push-upstream-branch)

;;;; 大きなhunkを表示する時に先頭をwindow-topに移動させるようにする
(defun ly:magit-move-section-and-recenter ()
  (let ((sec (magit-current-section)))
    (when (and (eq 'hunk (magit-section-type sec))
               (not (magit-section-hidden sec))
               (> (- (line-number-at-pos
                      (magit-section-end sec))
                     (line-number-at-pos
                      (magit-section-beginning sec)))
                  (window-height)))
      (recenter 0))))

(defadvice magit-goto-next-section (after view-recenter activate)
  (ly:magit-move-section-and-recenter))

(defadvice magit-goto-previous-section (after view-recenter activate)
  (ly:magit-move-section-and-recenter))

;;;; dot-emacs-directoryのmagit-statusを開く
(defun ly:magit-status-dot-emacs ()
  (interactive)
  (magit-status user-emacs-directory))

;;;; commit-message用のテンプレート起動コマンド
(defvar ly:magit-commit-msg-snippet-alist
  '(("conf-single" . ly:magit-msg-snip-staged-filename)
    ("update-elisp" . ly:magit-msg-snip-update-elisp)
    ("local-merge" . ly:magit-msg-snip-local-merge)))
(defun ly:magit-commit-msg-snippet (snippet-name)
  (interactive (list (popup-menu* (mapcar #'car ly:magit-commit-msg-snippet-alist))))
  (let ((func (assoc-default snippet-name ly:magit-commit-msg-snippet-alist))
        (staged-files (magit-git-lines "diff" "--cached" "--name-only")))
    (insert (funcall func staged-files))))
(define-key magit-log-edit-mode-map (kbd "C-c TAB") 'ly:magit-commit-msg-snippet)

;;;; commit-logに変更したファイル名のtagを挿入する
(defun ly:magit-msg-snip-staged-filename (staged-files)
  (if (> (length staged-files) 1)
      (error "staged file is not single.")
    (let ((tag (file-name-sans-extension (file-name-nondirectory (car staged-files)))))
      (format "[%s] " tag))))

;;;; gitによるupdate/local-mergeのcommit-messageを挿入する
(defvar ly:magit-updated-info-exclude '("git-repo.yaml"))
(defun ly:magit-updated-dirname (filename-list)
  (let ((dirs (delete-dups (mapcar #'file-name-directory
                                   (set-difference filename-list ly:magit-updated-info-exclude :test #'equal)))))
    (when (= (length dirs) 1)
      (car dirs))))

(defun ly:magit-updated-info (staged-files)
  (let* ((dir (or (ly:magit-updated-dirname staged-files)
                  (read-directory-name
                   "Updated-Dir: " (concat user-emacs-directory "elisp/") nil t)))
         (package (car (last (split-string dir "/" t)))))
    (list dir package)))

(defun ly:magit-msg-snip-update-elisp (staged-files)
  (destructuring-bind (dir package)
      (ly:magit-updated-info staged-files)
    (format "update %s from %s"
            package
            (replace-regexp-in-string
             "\n\\'" ""
             (shell-command-to-string
              (format "(cd %s && git rev-parse HEAD)" dir))))))

(defun ly:magit-msg-snip-local-merge (staged-files)
  (destructuring-bind (dir package)
      (ly:magit-updated-info staged-files)
    (format "[%s] update merge to local-elisp" package)))

;;;; fix CRLF hunk for magit apply
(defadvice magit-apply-hunk-item* (around hunk-fix-eol activate)
  (let ((default-process-coding-system
          (cons 'utf-8-dos buffer-file-coding-system)))
    ad-do-it))
