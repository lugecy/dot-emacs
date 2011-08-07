;;; uthist.el --- recover undo-tree across emacs sessions

;; Copyright (C) 2010 lugecy <lugecy@gmail.com>

;; Author: lugecy <lugecy@gmail.com>
;; URL: http://github.com/lugecy/uthist
;; Keywords: convenience, undo, redo, history
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package makes buffer-undo-tree restorable between emacs sessions.
;; Moreover, multiple state of buffer can be managed.
;;
;; Write the following code to your .emacs:
;;
;; (require 'uthist)
;; (uthist-install)

;;; Commands:
;;
;; `uthist-checkin'
;;   Register current state of buffer.
;; `uthist-checkout'
;;   Restore buffer to registered state.
;; `uthist-save'
;;   Save undo-tree and ref-repository.
;; `uthist-recover'
;;   Recover undo-tree and ref-repository.
;; `uthist-diff-between-current'
;;   Display diff between registered state and current state.

;;; Code:

(eval-when-compile (require 'cl))
(require 'undo-tree)
(require 'diff-mode)

;;;; Utility Functions
(defmacro uthist-aif (p true-clause &rest false-clause)
  (declare (indent 2))
  `(let ((it ,p))
     (if it ,true-clause ,@false-clause)))

(defun uthist-walk-undo-tree (root func)
  (unless (undo-tree-node-p root)
    (error "Wrong type argument: undo-tree-node-p, %s" root))
  (loop for queue = (list root) then (append (cdr queue) (undo-tree-node-next (car queue)))
        while queue
        do (funcall func (car queue))))

(defun uthist-find-walk-undo-tree (root find-func)
  (catch 'uthist-find-walk
    (uthist-walk-undo-tree root
                           (lambda (node)
                             (when (funcall find-func node)
                               (throw 'uthist-find-walk node))))))

;;;; Node Reference Repository Code
(defvar uthist-ref-repo nil "uthist reference repository.")
(make-variable-buffer-local 'uthist-ref-repo)

(defvar uthist-save-point-ref-name "@save-point")
(defvar uthist-previous-current-ref-name "@pre-cur")
(defvar uthist-root-ref-name "@root")

(defvar uthist-lighter " UT+")

(defvar uthist-root-node-register nil)
(make-variable-buffer-local 'uthist-root-node-register)
(defvar uthist-root-backup-directory "root-obj")

(defun uthist-make-ref-repo ()
  (make-hash-table :test 'equal :weakness 'value))

(defun uthist-register-ref (name node &optional repo)
  (or repo (setq repo uthist-ref-repo))
  (puthash name node repo))

(defun uthist-register-ref-to-current (name &optional repo)
  (uthist-register-ref name (undo-tree-current buffer-undo-tree) repo))

(defun uthist-md5-format (&optional md5)
  (format "@md5-%s" (or md5 (md5 (current-buffer)))))

(defun uthist-md5-format-match (str)
  (string-match "^@md5-" str))

(defun uthist-get-repo-keys (repo)
  (nreverse (loop for k being the hash-keys in repo
                  unless (uthist-md5-format-match k)
                  collect k)))

(defun uthist-update-undo-tree ()
  (when (eq buffer-undo-list t)
    (error "No undo information in this buffer."))
  (unless undo-tree-mode
    (error "Not enable undo-tree-mode."))
  (unless buffer-undo-tree
    (setq buffer-undo-tree (make-undo-tree)))
  (undo-list-transfer-to-tree)
  (unless uthist-ref-repo
    (setq uthist-ref-repo (uthist-make-ref-repo)))
  (puthash uthist-root-ref-name (undo-tree-root buffer-undo-tree) uthist-ref-repo))

(defun uthist-checkin (&optional ref-name)
  "Register current state of buffer into repository."
  (interactive)
  (with-current-buffer (or undo-tree-visualizer-buffer (current-buffer))
    (uthist-update-undo-tree)
    (let ((name (or ref-name
                    (completing-read "checkin name: " (uthist-get-repo-keys uthist-ref-repo)))))
      (uthist-register-ref-to-current name uthist-ref-repo)
      (uthist-register-ref-to-current (uthist-md5-format) uthist-ref-repo)))
  (when undo-tree-visualizer-buffer
    (uthist-refresh-visualize undo-tree-visualizer-buffer)))

(defun uthist-checkout (&optional ref-name)
  "Restore buffer to registered state."
  (interactive)
  (with-current-buffer (or undo-tree-visualizer-buffer (current-buffer))
    (unless buffer-undo-tree
      (error "Not exist buffer-undo-tree."))
    (uthist-update-undo-tree)
    (let ((name-list (uthist-get-repo-keys uthist-ref-repo))
          (pre-cur (undo-tree-current buffer-undo-tree))
          name node)
      (unless name-list
        (error "Non exist registed node."))
      (setq name (or ref-name
                     (completing-read "registed name: " (append (delete uthist-root-ref-name name-list) (list uthist-root-ref-name)) nil t))
            node (gethash name uthist-ref-repo))
      (unless node
        (error "Not Found node [%s]." name))
      (undo-tree-set node)
      (uthist-register-ref uthist-previous-current-ref-name pre-cur uthist-ref-repo)))
  (when undo-tree-visualizer-buffer
    (uthist-refresh-visualize undo-tree-visualizer-buffer)))

(defun uthist-refresh-visualize (buf)
  (with-current-buffer buf
    (undo-tree-kill-visualizer)
    (undo-tree-visualize)))

(defun uthist-delete-reference ()
  "Delete reference of registered state."
  (interactive)
  (with-current-buffer (or undo-tree-visualizer-buffer (current-buffer))
    (unless buffer-undo-tree
      (error "Not exist buffer-undo-tree."))
    (unless uthist-ref-repo
      (error "No exist uthist-ref-repo."))
    (uthist-aif (completing-read "registed name: " (uthist-get-repo-keys uthist-ref-repo) nil t)
        (progn
          (remhash it uthist-ref-repo)
          (when undo-tree-visualizer-buffer
            (uthist-refresh-visualize undo-tree-visualizer-buffer))))))

;;;; UTHist Code
(defvar uthist-directory
  (expand-file-name
   (concat
    (if (boundp 'user-emacs-directory)
        user-emacs-directory
      "~/.emacs.d")
    "/uthist")))

(defun make-uthist-file-name (file)
  (setq file (convert-standard-filename (expand-file-name file)))
  (if (eq (aref file 1) ?:)
      (setq file (concat "/"
                         "drive_"
                         (char-to-string (downcase (aref file 0)))
                         (replace-regexp-in-string "\\\\" "/" (substring file 2)))))
  (setq file (expand-file-name
              (subst-char-in-string
               ?/ ?!
               (replace-regexp-in-string "!" "!!" file))
              uthist-directory)))

(defun uthist-install ()
  (unless (file-directory-p uthist-directory)
    (make-directory uthist-directory t))
  (add-hook 'after-save-hook 'uthist-save)
  (add-hook 'find-file-hook 'uthist-recover))

(defun uthist-uninstall ()
  (remove-hook 'after-save-hook 'uthist-save)
  (remove-hook 'find-file-hook 'uthist-recover))

;;;; Undo-list Encode/Decode Code
(defun uthist-walk-undo-list (function tree)
  (if (consp tree)
      (let ((value (funcall function tree)))
        (if (eq value tree)
            (let* ((cons (cons (uthist-walk-undo-list function (car tree)) nil))
                   (cur cons)
                   cdr)
              (while tree
                (setq cdr (cdr tree))
                (if (consp cdr)
                    (let (next)
                      (setq next (cons (uthist-walk-undo-list function (car cdr)) nil))
                      (setcdr cur next)
                      (setq cur next)
                      (setq tree cdr))
                  (setcdr cur (uthist-walk-undo-list function cdr))
                  (setq tree nil)))
              cons)
          value))
    (if tree
        (funcall function tree))))

(defun uthist-undo-list-encode (tree)
  (uthist-walk-undo-list
   (lambda (a)
     (cond ((and (consp a)
                 (eq (car a) 'apply))
            (if (condition-case nil
                    (read-from-string (prin1-to-string a))
                  (error nil))
                a
              '(apply uthist-unsupport-msg)))
           ((markerp a)
            (cons (if (marker-insertion-type a) 'marker* 'marker)
                  (marker-position a)))
           (t
            a)))
   tree))

(defun uthist-undo-list-decode (tree)
  (uthist-walk-undo-list
   (lambda (a)
     (if (consp a)
         (cond
          ((eq (car a) 'marker)
           (set-marker (make-marker) (cdr a)))
          ((eq (car a) 'marker*)
           (let ((marker (make-marker)))
             (set-marker marker (cdr a))
             (set-marker-insertion-type marker t)
             marker))
          (t
           a))
       a))
   tree))

(defun uthist-unsupport-msg ()
  (message "Unsupported encode undo action include.")
  (push '(apply uthist-unsupport-msg) buffer-undo-list))

;;;; Undo-tree Encode/Decode Code
(defun uthist-node-encode (node)
  "Encode undo-tree-node to printable tree."
  (vector nil nil
          (uthist-undo-list-encode (undo-tree-node-undo node))
          (uthist-undo-list-encode (undo-tree-node-redo node))
          (if (eq (undo-tree-current buffer-undo-tree) node)
              'current
            (undo-tree-node-timestamp node))
          (undo-tree-node-branch node)
          nil))

(defun uthist-tree-encode ()
  "Encode undo-tree for printable tree."
  (when buffer-undo-tree
    (uthist-tree-encode-1 (undo-tree-root buffer-undo-tree))))

(defun uthist-tree-encode-1 (root)
  (let* ((p (cons (uthist-node-encode root) nil))
         (dumped-list p)
         (queue (list (cons (undo-tree-node-next root) p)))
         q)
    (while queue                        ;queue is list of (node . insert position)
      (setq q (cdr (car queue)))
      (let ((nexts (car (car queue))))
        (while (eq (length nexts) 1)    ; when single branch
          (setcdr q (cons (uthist-node-encode (car nexts)) nil)) ; (n1 n2 ....
          (setq q (cdr q))                                       ;        ^--< q
          (setq nexts (undo-tree-node-next (car nexts))))
        (mapc (lambda (n)               ;when multiple branch
                (let ((x (cons (uthist-node-encode n) nil)))
                  (setcdr q (cons x nil)) ; (n1 n2 (x1) (x2) ..)
                  (setq q (cdr q))        ;                  ^--< q
                  (setq queue (nconc queue
                                     (list (cons (undo-tree-node-next n) x))))))
              (reverse nexts)))
      (setq queue (cdr queue)))
    dumped-list))

(defun uthist-connect-node-link (node prev next)
  (and prev (setf (undo-tree-node-previous node) prev))
  (and next (push next (undo-tree-node-next node))))

(defun uthist-node-decode (node prev next)
  "Decode undo-tree-node from printable tree."
  (uthist-connect-node-link node prev next)
  (setf (undo-tree-node-undo node) (uthist-undo-list-decode (undo-tree-node-undo node)))
  (setf (undo-tree-node-redo node) (uthist-undo-list-decode (undo-tree-node-redo node)))
  (+ (undo-list-byte-size (undo-tree-node-undo node))
     (undo-list-byte-size (undo-tree-node-redo node))))

(defun uthist-find-encoded-current-node (root)
  (uthist-find-walk-undo-tree root
                              (lambda (n)
                                (eq (undo-tree-node-timestamp n) 'current))))

(defun uthist-tree-decode (dumped-list)
  "Decode undo-tree from printable tree."
  (let* ((size (uthist-tree-decode-1 dumped-list))
         (tree (make-undo-tree))
         (root (car dumped-list))
         (cur (uthist-find-encoded-current-node root)))
    (setf (undo-tree-root tree) root)
    (setf (undo-tree-node-timestamp cur) (current-time))
    (setf (undo-tree-current tree) cur)
    (setf (undo-tree-size tree) size)
    tree))

(defun uthist-tree-decode-1 (dumped-list)
  (let* ((queue (list (list (car dumped-list) (cdr dumped-list) nil)))
         (size 0)
         node next-list prev)
    (while queue
      (setq node (first (car queue))
            next-list (second (car queue))
            prev (third (car queue)))
      (while (and node
                  (not (consp (car next-list))))
        (incf size (uthist-node-decode node prev (car next-list)))
        (setq prev node
              node (car next-list)
              next-list (cdr next-list)))
      (when node
        (incf size (uthist-node-decode node prev nil))
        (loop for branch in next-list
              do
              (uthist-connect-node-link node nil (car branch))
              (setq queue (nconc queue (list (list (car branch) (cdr branch) node))))))
      (setq queue (cdr queue)))
    size))

;;;; Node Reference Repository Encode/Decode Code
(defun uthist-prepare-save-repo ()
  (let ((fill-to-node (or (gethash uthist-save-point-ref-name uthist-ref-repo)
                          (gethash uthist-root-ref-name uthist-ref-repo))))
    (uthist-checkin-state-tag)
    (uthist-checkin-save-point)
    (uthist-fill-tree fill-to-node)))

(defun uthist-clear-ref-to-current ()
  (let ((current (undo-tree-current buffer-undo-tree))
        dups)
    (maphash (lambda (k v)
               (when (and (eq v current)
                          (string-match "^@" k)
                          (not (string= uthist-root-ref-name k)))
                 (push k dups)))
             uthist-ref-repo)
    (dolist (k dups)
      (remhash k uthist-ref-repo))))

(defun uthist-checkin-state-tag ()
  (uthist-clear-ref-to-current)
  (uthist-register-ref-to-current (uthist-md5-format))
  (uthist-register-ref-to-current (format-time-string "@%Y%m%d-%H%M%S" (current-time))))

(defun uthist-checkin-save-point ()
  (uthist-register-ref-to-current uthist-save-point-ref-name))

(defun uthist-repo-encode (repo tree)
  (let ((new-db (list))
        (cache (make-hash-table :test #'eq))
        (num 0))
    (uthist-walk-undo-tree (undo-tree-root tree)
                           (lambda (n)
                             (puthash n num cache)
                             (incf num)))
    (maphash (lambda (k v)
               (unless (or (string= uthist-save-point-ref-name k)
                           (string= uthist-previous-current-ref-name k)
                           (null (gethash v cache)))
                 (push (cons k (gethash v cache)) new-db)))
             repo)
    new-db))

(defun uthist-repo-decode (repo tree)
  (let ((new-db (uthist-make-ref-repo))
        (cache (make-hash-table))
        (num 0))
    (uthist-walk-undo-tree (undo-tree-root tree)
                           (lambda (n)
                             (puthash num n cache)
                             (incf num)))
    (loop for (k . v) in repo
          when (gethash v cache)
          do (puthash k it new-db))
    new-db))

(defun uthist-fill-tree (to-node)
  (let ((buf (current-buffer))
        (tree buffer-undo-tree)
        (repo uthist-ref-repo)
        (current (undo-tree-current buffer-undo-tree)))
    (with-temp-buffer
      (insert-buffer-substring buf)
      (buffer-enable-undo)
      (setq buffer-undo-list '(nil undo-tree-canary))
      (let ((path (make-hash-table :test 'eq))
            (n to-node))
        (puthash (undo-tree-root tree) t path)
        (while (and n (not (eq n (undo-tree-root tree))))
          (puthash n t path)
          (setq n (undo-tree-node-previous n)))
        ;; set n to cross point
        (setq n current)
        (while (not (gethash n path))
          (setq n (undo-tree-node-previous n)))
        ;; fill redo-element action
        (while (not (eq current n))
          (setq current (uthist-undo-emulate tree current)))
        (when (eq current (undo-tree-root tree))
          (uthist-register-ref (uthist-md5-format) (undo-tree-root tree) repo))))))

(defun uthist-undo-emulate (undo-tree node &optional arg)
  (let ((undo-in-progress t)
        (current node))
    (dotimes (i (or arg 1))
      (if (null (undo-tree-node-previous current))
          (error "No further undo information")
        (uthist-undo-tree-update-redo undo-tree current)
        (undo-boundary)
        ;; need rewrite undo (for yas/revive)
        (uthist-undo-tree-update-undo undo-tree current)
        (undo-boundary)
        (uthist-undo-tree-update-redo undo-tree current)
        (undo-boundary)
        (setq current (undo-tree-node-previous current))))
    current))

(defun uthist-undo-tree-update-redo (tree node)
  (primitive-undo 1 (undo-copy-list (undo-tree-node-undo node)))
  (when (undo-tree-node-redo node)
    (decf (undo-tree-size tree) (undo-list-byte-size (undo-tree-node-redo node))))
  (setf (undo-tree-node-redo node) (undo-list-pop-changeset))
  (incf (undo-tree-size tree) (undo-list-byte-size (undo-tree-node-redo node))))

(defun uthist-undo-tree-update-undo (tree node)
  (primitive-undo 1 (undo-copy-list (undo-tree-node-redo node)))
  (when (undo-tree-node-undo node)
    (decf (undo-tree-size tree) (undo-list-byte-size (undo-tree-node-undo node))))
  (setf (undo-tree-node-undo node) (undo-list-pop-changeset))
  (incf (undo-tree-size tree) (undo-list-byte-size (undo-tree-node-undo node))))

;;;; UTHist Save Driver Code
(defun uthist-save ()
  "Save undo-tree and ref-repository."
  (interactive)
  (when (and undo-tree-mode (consp buffer-undo-list))
    (let ((print-circle nil))
      (uthist-update-undo-tree)
      (uthist-prepare-save-repo)
      (when (uthist-root-change-p)
        (setq uthist-root-node-register (cons (undo-tree-root buffer-undo-tree) (uthist-save-root-state))))
      (condition-case var
          (let* ((file (make-uthist-file-name (buffer-file-name)))
                 (contents `((undo-tree . ,(uthist-tree-encode))
                             (repo . ,(uthist-repo-encode uthist-ref-repo buffer-undo-tree))
                             (root-md5 . ,(cdr uthist-root-node-register)))))
            (with-temp-buffer
              (print contents (current-buffer))
              (write-region (point-min) (point-max) file nil 0)
              (set-file-modes file ?\600))
            (set (make-local-variable 'undo-tree-mode-lighter) uthist-lighter))
        (error (message "[%s]: uthist-save error. %s" (buffer-name) var))))))

;;;; UTHist Recover Driver Code
(defun uthist-set-current (tree node)
  (let ((n node))
    (while (progn
             (when (undo-tree-node-previous n)
               (setf (undo-tree-node-branch (undo-tree-node-previous n))
                     (undo-tree-position
                      n (undo-tree-node-next (undo-tree-node-previous n))))
               (setq n (undo-tree-node-previous n)))))
    (setf (undo-tree-current tree) node)))

(defun uthist-recover ()
  "Recover undo-tree and ref-repository."
  (interactive)
  (when undo-tree-mode
    (let ((buffer (current-buffer))
          (file (make-uthist-file-name (buffer-file-name))))
      (if (not (file-exists-p file))
          (message "Undo-Tree history file doesn't exists.")
        (when (or (null buffer-undo-tree)
                  (yes-or-no-p (format "[%s] buffer-undo-tree is not empty. Do you want to recover now? " (buffer-name buffer))))
          (let* ((pair (uthist-recover-1 file buffer))
                 (undo-tree (car pair))
                 (repo (cadr pair))
                 (undo-list (caddr pair))
                 (root-key (cadddr pair)))
            (if (not (undo-tree-p undo-tree))
                (message "[%s] File digest doesn't match, so undo-tree history will be discarded." (buffer-name buffer))
              (setq buffer-undo-tree undo-tree
                    buffer-undo-list '(nil undo-tree-canary)
                    uthist-ref-repo repo)
              (when (and root-key (file-exists-p (uthist-get-root-state-filename root-key)))
                (setq uthist-root-node-register (cons (undo-tree-root undo-tree) root-key)))
              (when undo-list
                (let ((pre-cur (undo-tree-current buffer-undo-tree)))
                  (setq buffer-undo-list (append undo-list buffer-undo-list))
                  (undo-list-transfer-to-tree)
                  (uthist-fill-tree pre-cur))
                (uthist-register-ref-to-current (uthist-md5-format))
                (uthist-register-ref-to-current (format-time-string "@pending-%Y%m%d-%H%M%S" (current-time))))
              (uthist-checkin-save-point)
              (set (make-local-variable 'undo-tree-mode-lighter) uthist-lighter)
              (message "[%s] Undo-tree recover success.%s" (buffer-name buffer) (if undo-list " new pending!" "")))))))))

(defun uthist-recover-1 (histfile buffer)
  (let ((digest (md5 buffer)))
    (with-temp-buffer
      (insert-file-contents histfile)
      (goto-char (point-min))
      (let ((alist (condition-case nil
                       (read (current-buffer))
                     (error (message "undotreehist read error.") nil))))
        (when alist
          (let* ((encoded-repo (assoc-default 'repo alist))
                 (repo-md5-key (uthist-md5-format digest))
                 (recoverable-p (assoc-default repo-md5-key encoded-repo))
                 (root-md5-key (assoc-default 'root-md5 alist))
                 undo-tree repo undo-list grafting-p)
            (when (and (not recoverable-p)
                       root-md5-key)
              (setq grafting-p t
                    recoverable-p t))
            (when recoverable-p
              (setq undo-tree (condition-case var
                                  (uthist-tree-decode (assoc-default 'undo-tree alist))
                                (error (message "tree-load error: %s" var) nil)))
              (when grafting-p
                (setq undo-list (uthist-generate-graft-undolist buffer undo-tree root-md5-key))))
            (when (and encoded-repo undo-tree)
              (setq repo (uthist-repo-decode encoded-repo undo-tree)))
            (uthist-aif (and undo-tree repo
                             (or undo-list (gethash repo-md5-key repo)))
                (progn
                  (unless undo-list
                    (unless (eq it (undo-tree-current undo-tree))
                      (uthist-set-current undo-tree it)))
                  (list undo-tree repo undo-list root-md5-key)))))))))

;;;; Test Function
(defun uthist-test ()
  (require 'cl)
  (loop for f to 10
        for ref-alist = nil
        with filename = "/tmp/uthist-test"
        with undotreehist-filename = (make-uthist-file-name filename)
        with contents do
        (if (file-exists-p filename)
            (delete-file filename))
        (if (file-exists-p undotreehist-filename)
            (delete-file undotreehist-filename))
        (with-current-buffer (find-file-literally filename)
          (undo-tree-mode 1)
          (loop for i to 1000
                for c = (random 12) do
                (ignore-errors
                  (case c
                    (0 (loop for j to 10 do
                             (insert (make-string (1+ (random 20))
                                                  (+ (random 26) 65)))))
                    (1 (newline))
                    (2 (insert "\t"))
                    (3 (forward-line))
                    (4 (forward-line -1))
                    (5 (kill-line))
                    (6 (kill-paragraph -1))
                    (7 (yank))
                    (8 (kill-region (+ (point-min) (random (point-max))) (+ (point-min) (random (point-max)))))
                    (9 (undo-tree-undo (random 5)))
                    (10 (undo-tree-redo (random 5)))
                    (11 (let ((name (format "test%s" i)))
                          (uthist-checkin name)
                          (push (cons name (buffer-string)) ref-alist))))))
          (write-region (point-min) (point-max) (buffer-file-name) nil t)
          (uthist-save)
          ;; fill-tree test
          (when (uthist-find-walk-undo-tree (undo-tree-root buffer-undo-tree)
                                            (lambda (node)
                                              (and (not (eq node (undo-tree-root buffer-undo-tree)))
                                                   (null (undo-tree-node-redo node)))))
            (error "Test failed #%s" f))
          (kill-buffer (current-buffer)))
        (with-current-buffer (find-file-literally filename)
          (undo-tree-mode 1)
          (uthist-recover)
          ;; reach root test
          (ignore-errors
            (while (prog1 t (undo-tree-undo))))
          (setq contents (buffer-string))
          ;; ref-repo checkout test
          (unless (loop for (ref-name . bufstr) in ref-alist
                        always (progn (uthist-checkout ref-name)
                                      (string= bufstr (buffer-string))))
            (error "Test failed #%s" f))
          (set-buffer-modified-p nil)
          (kill-buffer (current-buffer))
          (if (string= contents "")
              (message "Test succeeded #%s" f)
            (error "Test failed #%s" f)))))

;;;; View ref-name in undo-tree-visualize
(defun uthist-visualize-labeling ()
  (let ((repo (buffer-local-value 'uthist-ref-repo undo-tree-visualizer-buffer)))
    (when repo
      (setq uthist-ref-repo repo)
      (save-excursion
        (remove-overlays)
        (maphash (lambda (k v)
                   (unless (or (uthist-md5-format-match k)
                               (null v))
                     (let* ((marker (undo-tree-node-marker v))
                            (pos (and marker (marker-position marker)))
                            (nbeg (and pos (previous-single-property-change (1+ pos) 'undo-tree-node)))
                            (nend (and pos (or (next-single-property-change pos 'undo-tree-node)
                                               (point-max))))
                            (ov (and nbeg nend (make-overlay nbeg nend))))
                       (when ov
                         (overlay-put ov 'after-string (format " %s" k))))))
                 repo)))))

(defadvice undo-tree-draw-tree (after utdb-label activate)
  (uthist-visualize-labeling))

;;;; uthist-diff Utility
(defun uthist-get-undo-list-between-node (node &optional from)
  (let ((path (make-hash-table :test 'eq))
        (n node)
        (current (or from (undo-tree-current buffer-undo-tree)))
        prev undo-list)
    (puthash node t path)
    (while n
      (when prev
        (puthash n (undo-tree-position
                    prev (undo-tree-node-next n)) path))
      (setq prev n)
      (setq n (undo-tree-node-previous n)))
    (setq n current)
    (while (not (gethash n path))
      (setq n (undo-tree-node-previous n)))
    (while (not (eq current n))
      (setq undo-list (nconc undo-list (cons nil (undo-copy-list (undo-tree-node-undo current)))))
      (setq current (undo-tree-node-previous current)))
    (while (not (eq current node))
      (setq current (nth (gethash current path) (undo-tree-node-next current)))
      (setq undo-list (nconc undo-list (cons nil (undo-copy-list (undo-tree-node-redo current))))))
    undo-list))

(defvar uthist-diff-buffer "*uthist-diff*")
(defun uthist-diff-between-current ()
  "Display diff between registered state and current state."
  (interactive)
  (window-configuration-to-register ?e)
  (with-current-buffer (or undo-tree-visualizer-buffer (current-buffer))
    (uthist-update-undo-tree)
    (let* ((name (completing-read "Name: " (uthist-get-repo-keys uthist-ref-repo) nil t))
           (buf (current-buffer))
           (undo-list (uthist-get-undo-list-between-node (gethash name uthist-ref-repo))))
      (with-current-buffer (get-buffer-create uthist-diff-buffer)
        (buffer-disable-undo)
        (erase-buffer)
        (insert-buffer-substring buf)
        (buffer-enable-undo)
        (let ((undo-in-progress t))
          (while undo-list
            (setq undo-list (primitive-undo 1 undo-list))))))
    (require 'ediff)
    (ediff-buffers (current-buffer) uthist-diff-buffer)))

;;;; UTHist Graft Recover Code
(defun uthist-get-root-state-filename (key)
  (expand-file-name key (expand-file-name uthist-root-backup-directory uthist-directory)))

(defun uthist-generate-graft-undolist (buffer undo-tree root-md5-key)
  (let ((cur-state-buf (uthist-get-old-current-state root-md5-key undo-tree)))
    (when cur-state-buf
      (let* ((old-cur-file (uthist-make-temp-file cur-state-buf))
             (diff-buf (uthist-graft-diff old-cur-file (buffer-file-name buffer))))
        (unwind-protect
            (uthist-convert-diff-to-undolist diff-buf)
          (delete-file old-cur-file)
          (kill-buffer diff-buf)
          (kill-buffer cur-state-buf))))))

(defun uthist-save-root-state ()
  (let ((root-obj-dir (expand-file-name uthist-root-backup-directory uthist-directory))
        (root-buf (uthist-get-node-state (current-buffer) (undo-tree-root buffer-undo-tree) (undo-tree-current buffer-undo-tree)))
        fname md5-key)
    (unless (file-directory-p root-obj-dir)
      (make-directory root-obj-dir t))
    (setq md5-key (md5 root-buf)
          fname (uthist-get-root-state-filename md5-key))
    (with-current-buffer root-buf
      (write-region (point-min) (point-max) fname nil 0))
    (uthist-aif (cdr uthist-root-node-register)
        (let ((pre-fname (uthist-get-root-state-filename it)))
          (when (file-exists-p pre-fname)
            (delete-file pre-fname))))
    (kill-buffer root-buf)
    md5-key))

(defun uthist-diffhunk-to-undo-element ()
  (destructuring-bind (buf line-offset pos old new &optional switched)
      (diff-find-source-location nil t)
    (list pos (cons (car new) (car pos)))))

(defun uthist-convert-diff-to-undolist (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (point-max))
      (cdr (loop while (ignore-errors (diff-hunk-prev 1) t)
                 append (cons nil (uthist-diffhunk-to-undo-element)))))))

(defun uthist-make-temp-file (buffer)
  (let ((fname (make-temp-file "uthist-")))
    (with-temp-file fname
      (insert-buffer-substring buffer))
    fname))

(defun uthist-graft-diff (old-file now-file)
  (let ((buf (generate-new-buffer "*uthist-graft-diff*"))
        (command (combine-and-quote-strings (list "diff" old-file now-file))))
    (with-current-buffer buf
      (erase-buffer)
      (insert "Index: " now-file "\n")
      (diff-mode))
    (call-process shell-file-name nil buf nil shell-command-switch
                  command)
    buf))

(defun uthist-get-node-state (buf target now)
  (let ((target-buffer (get-buffer-create " *uthist-node-state*"))
        (undo-list (uthist-get-undo-list-between-node target now)))
    (with-current-buffer target-buffer
      (buffer-disable-undo)
      (erase-buffer)
      (insert-buffer-substring-no-properties buf)
      (buffer-enable-undo)
      (let ((undo-in-progress t))
        (while undo-list
          (setq undo-list (primitive-undo 1 undo-list)))))
    target-buffer))

(defun uthist-root-change-p ()
  (not (eq (car uthist-root-node-register) (undo-tree-root buffer-undo-tree))))

(defun uthist-get-old-current-state (root-key undo-tree)
  (let ((root-file (uthist-get-root-state-filename root-key)))
    (when (file-exists-p root-file)
      (with-temp-buffer
        (insert-file-contents root-file)
        (uthist-get-node-state (current-buffer) (undo-tree-current undo-tree) (undo-tree-root undo-tree))))))

(provide 'uthist)
;;; uthist.el ends here
