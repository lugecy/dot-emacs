(require 'uthist)
(require 'el-expectations)

(defmacro uthist-with-test-env (&rest body)
  `(with-temp-buffer
     (buffer-enable-undo)
     (undo-tree-mode 1)
     ,@body))

(defmacro uthist-with-test-file-env (filename &rest body)
  (declare (indent 1))
  `(with-current-buffer (find-file-literally ,filename)
     (buffer-enable-undo)
     (undo-tree-mode 1)
     (prog1 (progn ,@body)
       (set-buffer-modified-p nil)
       (kill-buffer (current-buffer)))))

(defmacro uthist-with-test-file-session (&rest body)
  `(let* ((fname "/tmp/uthist-test")
          (hist-fname (make-uthist-file-name fname)))
     (when (file-exists-p fname)
       (delete-file fname))
     (when (file-exists-p hist-fname)
       (delete-file hist-fname))
     ,@body))

(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (expect "foo bar"
        (mocklet ((completing-read => "test"))
          (uthist-with-test-env
           (insert "foo ") (undo-boundary)
           (insert "bar") (undo-boundary)
           (uthist-checkin)
           (undo-tree-undo 2)
           (insert "hoge ") (undo-boundary)
           (insert "fuga") (undo-boundary)
           (uthist-checkout)
           (buffer-string))))
      (expect "foo bar"
        (uthist-with-test-file-session
         (uthist-with-test-file-env fname
           (insert "foo ") (undo-boundary)
           (insert "bar") (undo-boundary)
           (undo-tree-undo 2)
           (insert "hoge ") (undo-boundary)
           (insert "fuga") (undo-boundary)
           (write-region (point-min) (point-max) (buffer-file-name) nil t)
           (uthist-save))
         (uthist-with-test-file-env fname
           (uthist-recover)
           (undo-tree-undo 2)
           (undo-tree-switch-branch 1)
           (undo-tree-redo 2)
           (buffer-string))))
      (expect "foo bar"
        (mocklet ((completing-read => "test"))
          (uthist-with-test-file-session
           (uthist-with-test-file-env fname
             (insert "foo ") (undo-boundary)
             (insert "bar") (undo-boundary)
             (uthist-checkin)
             (undo-tree-undo 2)
             (insert "hoge ") (undo-boundary)
             (insert "fuga") (undo-boundary)
             (write-region (point-min) (point-max) (buffer-file-name) nil t)
             (uthist-save))
           (uthist-with-test-file-env fname
             (uthist-recover)
             (uthist-checkout)
             (buffer-string)))))
      (expect "hoge fuga"
        (uthist-with-test-file-session
         (uthist-with-test-file-env fname
           (insert "foo ") (undo-boundary)
           (insert "bar") (undo-boundary)
           (uthist-save)
           (undo-tree-undo 2)
           (insert "hoge ") (undo-boundary)
           (insert "fuga") (undo-boundary)
           (write-region (point-min) (point-max) (buffer-file-name) nil t)
           (uthist-save))
         (with-current-buffer (find-file-literally fname)
           (erase-buffer)
           (insert "foo bar")
           (write-region (point-min) (point-max) (buffer-file-name) nil t)
           (set-buffer-modified-p nil)
           (kill-buffer (current-buffer)))
         (uthist-with-test-file-env fname
           (uthist-recover)
           (undo-tree-undo 2)
           (undo-tree-switch-branch 0)
           (undo-tree-redo 2)
           (buffer-string) )))
      (expect "hoge fuga"
        (uthist-with-test-file-session
         (uthist-with-test-file-env fname
           (insert "foo ") (undo-boundary)
           (insert "bar") (undo-boundary)
           (uthist-save)
           (undo-tree-undo 2)
           (insert "hoge ") (undo-boundary)
           (insert "fuga") (undo-boundary)
           (insert " buka") (undo-boundary)
           (undo-tree-undo 1) (undo-tree-redo 1)
           (insert "boke") (undo-boundary)
           (write-region (point-min) (point-max) (buffer-file-name) nil t)
           (uthist-save))
         (with-current-buffer (find-file-literally fname)
           (erase-buffer)
           (insert "foo bar")
           (write-region (point-min) (point-max) (buffer-file-name) nil t)
           (set-buffer-modified-p nil)
           (kill-buffer (current-buffer)))
         (uthist-with-test-file-env fname
           (uthist-recover)
           (undo-tree-undo 2)
           (undo-tree-switch-branch 0)
           (undo-tree-redo 2)
           (buffer-string) )))
      (desc "for pending recover")
      (expect "foo bar\n"
        (uthist-with-test-file-session
         (uthist-with-test-file-env fname
           (insert "foo ") (undo-boundary)
           (insert "bar\n") (undo-boundary)
           (write-region nil nil (buffer-file-name) nil t)
           (uthist-save))
         (with-current-buffer (find-file-literally fname)
           (goto-char (point-max))
           (insert "baz\n")
           (write-region nil nil (buffer-file-name) nil t)
           (set-buffer-modified-p nil)
           (kill-buffer (current-buffer)))
         (uthist-with-test-file-env fname
           (mocklet ((y-or-n-p => t))
             (uthist-recover))
           (delete-file (uthist-get-root-state-filename (md5 "")))
           (condition-case err
               (progn
                 (undo-tree-undo 1)
                 (buffer-string))
             (error err)))))
      (expect "foo\n-----\nbar\n@@@@@\nfooter notes\n"
        (uthist-with-test-file-session
         (uthist-with-test-file-env fname
           (insert "foo\n") (undo-boundary)
           (insert "-----\n") (undo-boundary)
           (insert "bar\n") (undo-boundary)
           (insert "@@@@@\n") (undo-boundary)
           (insert "footer notes\n") (undo-boundary)
           (write-region nil nil (buffer-file-name) nil t)
           (uthist-save))
         (with-current-buffer (find-file-literally fname)
           (erase-buffer)
           (insert "-----\n")
           (insert "baz\n")
           (insert "@@@@@\n")
           (insert "footer notes\n")
           (insert "add lines\n")
           (write-region nil nil (buffer-file-name) nil t)
           (set-buffer-modified-p nil)
           (kill-buffer (current-buffer)))
         (uthist-with-test-file-env fname
           (mocklet ((y-or-n-p => t))
             (uthist-recover))
           (delete-file (uthist-get-root-state-filename (md5 "")))
           (condition-case err
               (progn
                 (undo-tree-undo 3)
                 (buffer-string))
             (error err)))))
      (expect "foo bar\nbaz\n"
        (uthist-with-test-file-session
         (uthist-with-test-file-env fname
           (insert "foo ") (undo-boundary)
           (insert "bar\n") (undo-boundary)
           (write-region nil nil (buffer-file-name) nil t)
           (uthist-save))
         (with-current-buffer (find-file-literally fname)
           (goto-char (point-max))
           (insert "baz\n")
           (write-region nil nil (buffer-file-name) nil t)
           (set-buffer-modified-p nil)
           (kill-buffer (current-buffer)))
         (uthist-with-test-file-env fname
           (mocklet ((y-or-n-p => t))
             (uthist-recover))
           (uthist-save))
         (with-current-buffer (find-file-literally fname)
           (erase-buffer)
           (insert "foo bar\n")
           (write-region nil nil (buffer-file-name) nil t))
         (uthist-with-test-file-env fname
           (uthist-recover)
           (undo-tree-redo 1)
           (buffer-string))))
      (expect (no-error)
        (uthist-test))
      )))

;;;; Utility Functions
(defun uthist-tree-decode-list-equal (load-list-1 load-list-2)
  (let ((queue (list (cons (cons (car load-list-1) (cdr load-list-1))
                           (cons (car load-list-2) (cdr load-list-2))))) ; (node . next-list)
        now)
    (catch 'fail
      (while queue
        (setq now (car queue))
        (let ((n-c (car now))
              (r-c (cdr now)))
          (unless (and (uthist-node-equal (car n-c) (car r-c))
                       (uthist-check-node-link (car n-c))
                       (uthist-check-node-link (car r-c)))
            (throw 'fail nil))
          (cond
           ((and (consp (car (cdr n-c)))
                 (consp (car (cdr r-c))))
            (loop for norecur-b in (cdr n-c)
                  for recur-b in (cdr r-c)
                  do
                  (setq queue (nconc queue
                                     (list (cons (cons (car norecur-b)
                                                       (cdr norecur-b))
                                                 (cons (car recur-b)
                                                       (cdr recur-b))))))))
           ((and (cdr n-c)
                 (cdr r-c))
            (setq queue (nconc queue
                               (list (cons (cons (car (cdr n-c))
                                                 (cdr (cdr n-c)))
                                           (cons (car (cdr r-c))
                                                 (cdr (cdr r-c))))))))
           ((or (and (cdr n-c) (not (cdr r-c)))
                (and (not (cdr n-c)) (cdr r-c)))
            (throw 'fail nil))))
        (setq queue (cdr queue)))
      t)))

(defun uthist-tree-equal (tree-a tree-b)
  (let ((queue (list (cons (undo-tree-root tree-a) (undo-tree-root tree-b)))) ; (node . next-list)
        now-cell)
    (catch 'fail
      (while queue
        (setq now-cell (pop queue))
        (let ((a-n (car now-cell))
              (b-n (cdr now-cell)))
          (unless (and (uthist-node-equal a-n b-n)
                       (uthist-check-node-link a-n)
                       (uthist-check-node-link b-n))
            (throw 'fail nil))
          (unless (eq (length (undo-tree-node-next a-n))
                      (length  (undo-tree-node-next b-n)))
            (throw 'fail nil))
          (loop for a-nn in (undo-tree-node-next a-n)
                for b-nn in (undo-tree-node-next b-n)
                do
                (setq queue (nconc queue (list (cons a-nn b-nn)))))))
      t)))

(defun uthist-node-equal (node-a node-b)
  (and (undo-tree-node-p node-a)
       (undo-tree-node-p node-b)
       (equal (undo-tree-node-timestamp node-a)
              (undo-tree-node-timestamp node-b))
       (equal (undo-tree-node-undo node-a)
              (undo-tree-node-undo node-b))
       (equal (undo-tree-node-redo node-a)
              (undo-tree-node-redo node-b))))

(defun uthist-check-node-link (node)
  (and (loop for n in (undo-tree-node-next node)
             always (eq node (undo-tree-node-previous n)))
       (if (undo-tree-node-previous node)
           (loop for n in (undo-tree-node-next (undo-tree-node-previous node))
                 thereis (eq node n))
         t)))

(defun uthist-tree-liner-size (&optional tree)
  (or tree (setq tree buffer-undo-tree))
  (loop with current = (undo-tree-current tree)
        with i = 1
        for n = (undo-tree-node-previous current) then (undo-tree-node-previous n)
        while (undo-tree-node-previous n)
        do (incf i)
        finally return i))

(defun uthist-count-tree-node-num (&optional tree)
  (or tree (setq tree buffer-undo-tree))
  (let ((num 0))
    (uthist-walk-undo-tree (undo-tree-root tree)
                           (lambda (n) (incf num)))
    num))

(defun uthist-benchmark-save ()
  (format "(buffer=%s size=%s node-num=%s %s)"
          (buffer-name)
          (undo-tree-size buffer-undo-tree)
          (uthist-count-tree-node-num buffer-undo-tree)
          (benchmark-run 1 (uthist-save))))
