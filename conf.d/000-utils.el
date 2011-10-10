;;;; C-w の挙動をshell compatible に
;; EmacsWiki: Default Killing And Yanking
;; http://www.emacswiki.org/emacs/DefaultKillingAndYanking
(defun unix-werase-or-kill (arg)
  (interactive "*p")
  (if (and transient-mark-mode
           mark-active)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))

;;;; Vimの'*'検索をC-sでエミュレート
(defun isearch-yank-point-symbol ()
  (interactive)
  (isearch-yank-string (thing-at-point 'symbol)))

;;;; 変更前のファイルと現在との差分を取る
(defun diff-with-original (ediff)
  "Examin diff of current buffer with original file.
        If with prefix, do interactive merge using `ediff-with-original'. "
  (interactive "P")
  (window-configuration-to-register ?e)
  (if ediff
      (ediff-with-original)
    ;; simple diff view with diff-mode
    (require 'ediff)
    (let ((diff-buf (get-buffer-create (format "*diff %s*" (buffer-file-name))))
          (ediff-diff-options "-u --strip-trailing-cr") ;; is it your favourite?
          (tmpfile (ediff-make-temp-file (current-buffer))))
      (save-excursion
        (set-buffer diff-buf)
        (setq buffer-read-only nil)
        (buffer-disable-undo)
        (erase-buffer))
      (ediff-set-actual-diff-options)
      (ediff-make-diff2-buffer diff-buf
                               (buffer-file-name)
                               tmpfile)
      (delete-file tmpfile)
      (set-buffer diff-buf)
      (if (< (buffer-size) 1)
          (message "No differences with original file.")
        (condition-case nil
            (progn
              (require 'diff-mode)
              (diff-mode))
          (error))
        (goto-char 1)
        (pop-to-buffer diff-buf)))))
(defun ediff-with-original ()
  (interactive)
  (window-configuration-to-register ?e)
  ;; interactive merge using ediff
  (let ((file buffer-file-name)
        (buf (current-buffer))
        (orig-buf (get-buffer-create (concat "*orig " buffer-file-name "*"))))
    (set-buffer orig-buf)
    (setq buffer-read-only nil)
    (buffer-disable-undo)
    (erase-buffer)
    (insert-file file)
    (setq buffer-read-only t)
    (set-buffer-modified-p nil)
    (ediff-buffers orig-buf buf)))

;;;; 日付を挿入する
(defun my-insert-time ()
  (interactive)
  (insert (format-time-string "[%Y-%m-%d]")))

;;;; emacs-fu: duplicating lines and commenting them
;;;; http://emacs-fu.blogspot.com/2010/01/duplicating-lines-and-commenting-them.html
(defun djcb-duplicate-line (&optional commentfirst)
  "comment line at point; if COMMENTFIRST is non-nil, comment the original"
  (interactive "P")
  (beginning-of-line)
  (push-mark)
  (end-of-line)
  (let ((str (buffer-substring (region-beginning) (region-end))))
    (when commentfirst
      (comment-region (region-beginning) (region-end)))
    (insert-string
     (concat (if (= 0 (forward-line 1)) "" "\n") str "\n"))
    (forward-line -1)))

;;;; 必ずウィンドウ分割してバッファを表示する(shell-pop.el での挙動)
(defun pop-up-split-window (buf)
  (split-window)
  (other-window 1)
  (switch-to-buffer buf))

;;;; エスケープシーケンスや printf フォーマットをハイライト - リタマス
;;;; http://d.hatena.ne.jp/mooz/20100313/p1
(defun ly:escape-seq-char-fontlock-setup ()
  (font-lock-add-keywords nil
                          '(("\\\\[areEfntv]" 0 font-lock-keyword-face t)
                            ("%-?[0-9]*\\.?[0-9]*l?[csduoxfegp]" 0 font-lock-type-face t))
                          t))

;;;; printf-debug 補助関数
(defvar tap-log-buffer "*tap log*")
(defmacro ly:tap (sexp &optional loging tag)
  "Return obj value and message."
  (let ((sym (cond ((consp sexp) (symbol-name (car sexp)))
                   ((symbolp sexp) (symbol-name sexp))
                   ((atom sexp) "`atom")))
        (tap-value (gensym)))
    `(let ((,tap-value ,sexp))
       (prog1 ,tap-value
         (ly:tap-msg ,tap-value ,sym ,loging ,tag)))))

(defun ly:tap-msg (value sym &optional loging tag)
  (let ((standard-output (cond ((eq loging t) (get-buffer-create tap-log-buffer))
                               ((bufferp loging) loging)
                               ((and (stringp loging) (get-buffer loging)))
                               (t t))))
    (with-current-buffer (get-buffer-create tap-log-buffer)
      (goto-char (point-min))
      (and (not (eq standard-output t))
           (princ (format "======%s======\n" (or tag ""))))
      (condition-case nil
          (princ (format "%s => %s%s" sym (prin1-to-string value) (if (not (eq standard-output t)) "\n" "")))
        (error nil)))))

(defmacro ly:tapl (&rest sexp)
  `(ly:tap ,@sexp t))

;; tap macro insert utility
(ly:eval-after-load 'smartchr
  (defun ly:tap-insert (&optional arg)
    (interactive "P")
    (save-excursion
      (unless (looking-at "\\(\\_<\\|\\s(\\)")
        (backward-sexp))
      (paredit-wrap-round)
      (insert (if (> (prefix-numeric-value arg) 1) "ly:tap " "ly:tapl "))
      (backward-up-list) (indent-sexp)))

  (defun ly:smartchr-tap-insert ()
    (interactive)
    (smartchr-make-struct
     :insert-fn (lambda ()
                  (ly:tap-insert current-prefix-arg))
     :cleanup-fn (lambda ()
                   (when (looking-at "(")
                     (down-list) (forward-sexp)
                     (paredit-raise-sexp)))))
  (define-key emacs-lisp-mode-map (kbd ":") (smartchr '(":" ly:smartchr-tap-insert))))

;;;; non gc benchmark-run
(defmacro benchmark-run-no-gc (&rest form)
  `(let ((gc-cons-threshold (* 1024 1024 32)))
     (garbage-collect)
     (benchmark-run ,@form)))

;;;; リストを1行1sexpに整形する
(defun ly:pp-list ()
  (interactive)
  (flet ((inner-string-p () (nth 3 (syntax-ppss (point)))))
    (save-excursion
      (let (list-beg)
        (unless (eq (char-after) ?\()
          (when (inner-string-p)
            (goto-char (1- (scan-sexps (point) -1))))
          (backward-up-list))
        (setq list-beg (point))
        (down-list)
        (ignore-errors
          (while t
            (insert "\n") (forward-sexp)))
        (goto-char list-beg)
        (indent-sexp)))))

;;;; diredから好きなオプションでgit logを起動した後，vc-log-vier-modeを起動する関数
(defun ly:vc-enable-log-view-mode ()
  (interactive)
  (unless (boundp 'vc-short-log) ;vc-short-logがboundされてない時errorになる…？
    (defvar vc-short-log nil))
  (set (make-local-variable 'vc-log-view-type) 'long)
  (vc-git-log-view-mode)
  (set (make-local-variable 'log-view-vc-backend) 'Git))

;;;; fiber.exeなんてない人のための関連付け実行 on bash and ntemacs - 冷凍庫
;;;; http://d.hatena.ne.jp/sr10/20110118/1295280250
;;;; anything-congif.el に似たような関数あり
(defun x-open (file)
  "open file on system association."
  (interactive
   (list (read-file-name "Open file: " nil default-directory)))
  (message "Opening %s..." file)
  (cond ((not window-system)
         (find-file file))
        ((eq system-type 'windows-nt)
         ;; (call-process "cmd.exe" nil 0 nil "/c" "start" "" (convert-standard-filename (expand-file-name file)))
         (w32-shell-execute "open" (replace-regexp-in-string ;for UNC paths
                                    "/" "\\"
                                    (replace-regexp-in-string ; strip cygdrive paths
                                     "/cygdrive/\\(.\\)" "\\1:"
                                     (expand-file-name file) nil nil) nil t))
         )
        ((eq system-type 'darwin)
         (call-process "open" nil 0 nil file))
        (t
         (call-process "xdg-open" nil 0 nil file)))
  (recentf-add-file file)
  (message "Opening %s...done" file))

;;;; 先に挿入先を決めてから，regionで囲った範囲をkill/yankする
(defun ly:yank-before-point-target ()
  (interactive)
  (if (> (recursion-depth) 0)
      (exit-recursive-edit)
    (let ((pos (point))
          content)
      (recursive-edit)
      (when (region-active-p)
        (kill-region (region-beginning) (region-end))
        (goto-char pos)
        (yank)))))
