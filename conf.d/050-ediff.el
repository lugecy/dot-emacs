;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; configure ediff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; diff-mode setting
(setq diff-switches "-u")
;; ediffで新しいフレームを作らない
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; mercurial like diff face
(ly:eval-after-load 'diff-mode
  (custom-set-faces
   '(diff-added
     ((((class color) (background light))
       :foreground "blue1")
      (((class color) (background dark))
       :foreground "green")))
   '(diff-removed
     ((((class color) (background light))
       :foreground "red")
      (((class color) (background dark))
       :foreground "red")))))
(add-hook 'diff-mode-hook 'diff-mode-mercurial-face-setting)
(defun diff-mode-mercurial-face-setting ()
  "configure diff-mode face like mercurial color diff"
  (defface diff-header-removed-file
    '((t :foreground "red"))
    "mercurial diff like highlight diff-header-removed-file")
  (defface diff-header-added-file
    '((t :foreground "green"))
    "mercurial diff like highlight diff-header-added-file")
  (defface diff-file-header-removed-file
    '((t :foreground "red" :inherit diff-file-header))
    "mercurial diff like highlight diff-file-header-removed-file")
  (defface diff-file-header-added-file
    '((t :foreground "green" :inherit diff-file-header))
    "mercurial diff like highlight diff-file-header-added-file")
  (setq diff-font-lock-keywords
	(append
	 '(("^diff .+\n" . 'diff-header)
	   ("^\\(---\\) \\([^\t\n]+?\\)\\(?:\t.*\\| \\(\\*\\*\\*\\*\\|----\\)\\)?\n"
	    (0 'diff-header)
	    (1 'diff-header-removed-file prepend)
	    (2 (if (not (match-end 5)) 'diff-file-header-removed-file) prepend))
	   ("^\\(\\+\\+\\+\\) \\([^\t\n]+?\\)\\(?:\t.*\\| \\(\\*\\*\\*\\*\\|----\\)\\)?\n"
	    (0 'diff-header)
	    (1 'diff-header-added-file prepend)
	    (2 (if (not (match-end 5)) 'diff-file-header-added-file) prepend)))
	 diff-font-lock-keywords)))

;;;; 3way-merge 時のコンフクリトマークを探すコマンド
(defun search-conflict-place ()
  "Function documantion."
  (interactive)
  (let (forward back)
    (setq forward (search-forward-regexp "=\\{7\\}" nil t))
    (unless forward
      (setq back (search-backward-regexp "=\\{7\\}" nil t))
      (unless back (message "No exist confilct mark")))))

;;;; ediff-three-merged-buffer
(setq ediff-combination-pattern
      '("<<<<<<< local" A "####### base" Ancestor "=======" B ">>>>>>> remote"))
(defun ediff-three-way-split-and-merge-buffer ()
  (interactive)
  (let ((merged-buffer (current-buffer))
        (local-mark "<<<<<<<")
        (ance-mark "=======")
        (remote-mark ">>>>>>>")
        (multiline-match "\\(\\(.\\|\n\\)*?\\)")
        pos local-contents remote-contents
        local-buffer remote-buffer)
    (save-excursion
      ;; search and generate local and remote buffer string
      (setq pos (goto-char (point-min)))
      (while (re-search-forward local-mark nil t)
        (beginning-of-line)
        (let ((contents (buffer-substring-no-properties pos (point))))
          (setq local-contents  (concat local-contents contents)
                remote-contents (concat remote-contents contents)))
        (re-search-forward (concat "^" local-mark ".*\n"
                                   multiline-match ;match part 1, local-part
                                   ance-mark "\n"
                                   multiline-match ;match part 3, remote-part
                                   remote-mark ".*\n")
                           nil t)
        (setq local-contents  (concat local-contents (match-string-no-properties 1))
              remote-contents (concat remote-contents (match-string-no-properties 3))
              pos (point)))
      ;; from last-mark to buffer-end
      (let ((contents (buffer-substring-no-properties pos (point-max))))
        (setq local-contents  (concat local-contents contents)
              remote-contents (concat remote-contents contents)))
      ;; prepare buffer
      (setq local-buffer  (get-buffer-create "*ediff 3way local*")
            remote-buffer (get-buffer-create "*ediff 3way remote*"))
      (with-current-buffer local-buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert-string local-contents)))
      (with-current-buffer remote-buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert-string remote-contents))))
    (ediff-merge-buffers local-buffer remote-buffer nil nil (buffer-file-name merged-buffer))))

;; (expectations
;;   (expect "foo\nbar\nbaz\n"
;;     (stub ediff-merge-buffers => nil)
;;     (with-temp-buffer
;;       (insert "foo\n<<<<<<<\nbar\n=======\nBAR\n>>>>>>>\nbaz\n")
;;       (ediff-three-way-split-and-merge-buffer))
;;     (with-current-buffer "*ediff 3way local*" (buffer-string)))
;;   (expect "foo\nBAR\nbaz\n"
;;     (stub ediff-merge-buffers => nil)
;;     (with-temp-buffer
;;       (insert "foo\n<<<<<<<\nbar\n=======\nBAR\n>>>>>>>\nbaz\n")
;;       (ediff-three-way-split-and-merge-buffer))
;;     (with-current-buffer "*ediff 3way remote*" (buffer-string)))
;;  )
