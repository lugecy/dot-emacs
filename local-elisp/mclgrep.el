;;; mclgrep --- clgrep for multiple files

(require 'clgrep)

(defcustom mclgrep:target-files nil
  "mclgrep target files.")

(defalias 'mclgrep 'mclgrep:item)

(defun mclgrep:item (query &optional rev)
  "ChangeLog grep."
  (interactive "smclgrep (item): \nP")
  (let ((blgrep-target-p (lambda () (blg-changelog-looking-at "^\t"))))
    (mclgrep:item-common query rev "mclgrep:item")))

(defun mclgrep:item-common (query rev grep-func)
  (let ((blgrep-point-min    #'blg-changelog-point-min)
        (blgrep-beg-of-block (lambda ()
                               (re-search-backward blg-changelog-item-header-regexp nil t)))
        (blgrep-end-of-block (lambda ()
                               (forward-char 1)
                               (re-search-forward blg-changelog-header-regexp nil 'move)
                               (beginning-of-line)
                               (skip-syntax-backward "->")
                               (forward-line 1)))
        (blgrep-up-block     (lambda ()
                               (re-search-backward blg-changelog-entry-header-regexp nil t))))
    (mclgrep:blgrep query rev #'clgrep-mode grep-func 'head-and-body 1)))

(defun mclgrep:blgrep (query rev mode grep-func structure &optional blank-num)
  (unless (memq structure '(block head-and-body hierarchy))
    (error "Unsupported structure type"))
  (or (numberp blank-num) (setq blank-num 0))
  (setq blgrep-highlight-regexp nil)
  (let* ((scan-result (cons 0 nil))
         (quasi-view (string= query ""))
         (query (if quasi-view blg-changelog-item-header-regexp query)))
    (dolist (file mclgrep:target-files)
      (with-current-buffer (mclgrep:get-file-buffer file)
        (save-excursion
          (destructuring-bind (count . tree)
              (or (blgrep-scan query blank-num structure)
                  (cons nil nil))
            (when count
              (setq scan-result
                    (cons (+ count (car scan-result))
                          (nconc (cdr scan-result)
                                 (mapcar (lambda (x) (cons file x)) tree)))))))))
    (when (= (car scan-result) 0)
      (setq blgrep-file-local-variable nil)
      (error "No matches for `%s'" query))
    (unless quasi-view
      (setq blgrep-highlight-regexp query))
    (let ((count (car scan-result))
          (block-tree (cdr scan-result)))
      (message "%d matched" count)
      (mclgrep:blgrep-print block-tree rev grep-func)))
  (funcall mode)
  (use-local-map mclgrep-map)
  (jit-lock-register 'blgrep-highlight-match))

(defun mclgrep:get-file-buffer (filename)
  (or (get-file-buffer filename)
      (get-buffer filename)
      (if (file-exists-p filename)
          (find-file-noselect filename)
        (error "No such file `%s'" filename))))

(defun mclgrep:blgrep-print (tree rev grep-func)
  "Switch to blgrep buffer and print TREE."
  ;; preparation
  (blgrep-switch-to-buffer grep-func)
  ;; print
  (unless rev
    (setq tree (nreverse tree)))
  (insert (mclgrep:blgrep-concat-tree tree))
  ;; after treatment
  (goto-char (point-min))
  (when (looking-at "\n+")
    (replace-match ""))
  (setq buffer-read-only t)
  (set-buffer-modified-p nil))

(defvar mclgrep:entry-file-marker "  >>> ")

(defun mclgrep:blgrep-concat-tree (tree)
  (blgrep-concat-tree
   (loop for (file entry . items) in (sort tree
                                           (lambda (a b)
                                             (not (string< (nth 1 a) (nth 1 b)))))
         collect (cons (replace-regexp-in-string
                        "\\(\n\n\\)\\'" (format "%s%s\\1" mclgrep:entry-file-marker file)
                        entry)
                       items))))

(defun mclgrep:jump (&optional win-frame)
  "Jump to the source at point."
  (interactive)
  (let (cl-list (pos (point)) source-file)
    ;; To get cl-list
    (save-excursion
      ;; item
      (if (blg-changelog-looking-at blg-changelog-entry-header-regexp)
          (progn
            (end-of-line)
            (setq pos (- (point) pos))
            (beginning-of-line))
        (let ((beg (progn (skip-syntax-forward "->")
                          (end-of-line)
                          (blg-changelog-previous-item)
                          (beginning-of-line) (point)))
              (end (progn (end-of-line)
                          (when (re-search-forward blg-changelog-header-regexp nil t)
                            (beginning-of-line)
                            (skip-syntax-backward "->"))
                          (point))))
          (setq cl-list (list (buffer-substring-no-properties beg end))
                pos (- end pos))
          (blg-changelog-backward-entry)))
      ;; entry
      (setq cl-list (cons (buffer-substring-no-properties
                           (point) (progn
                                     (search-forward mclgrep:entry-file-marker (line-end-position))
                                     (match-beginning 0)))
                          cl-list))
      (setq source-file (buffer-substring-no-properties
                         (point) (line-end-position))))
    ;; Jump to text
    (let ((blgrep-buffer-file-name source-file))
      (blgrep-goto-source-at-point cl-list pos t win-frame))))

(defadvice blg-changelog-jump (around for-mclgrep activate)
  (if (save-excursion
        (goto-char (point-min))         ;goto entry-header
        (search-forward mclgrep:entry-file-marker (line-end-position) t))
      (mclgrep:jump)
    ad-do-it))

(defvar mclgrep-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map clgrep-mode-map)
    (substitute-key-definition 'blg-changelog-jump 'mclgrep:jump map clgrep-mode-map)
    map)
  "For mclgrep map.")

(provide 'mclgrep)
