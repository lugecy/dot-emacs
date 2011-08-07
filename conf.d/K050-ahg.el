(require 'ahg)
(eval-after-load "viper"
  '(progn
     (dolist (mode '(ahg-diff-mode ahg-command-mode ahg-status-mode
                                   ahg-short-log-mode ahg-log-mode ahg-glog-mode
                                   ahg-mq-patches-mode))
       (add-to-list 'viper-emacs-state-mode-list mode t))))

;;;; for mercurial record
;;;; diff-mode時にahg-recordコマンドを実行
;;;; diff-split-hunk した場合の動作保証なしというか動かないと思われ
(define-key ahg-diff-mode-map (kbd "R") 'ahg-record)
(defvar ahg-record-hunk-list nil)
(defconst ahg-diff-file-hunk-header "^+++ b/\\(.*\\)$")
(defvar ahg-commit-after-hook nil)
;; (defconst diff-hunk-header-re-unified
;;   "^@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? \\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@")

(defun ahg-collect-hunk ()
  (interactive)
  (setq ahg-record-hunk-list nil)
  (save-excursion
    (let ((end (goto-char (point-max))))
      (while (re-search-backward ahg-diff-file-hunk-header nil t)
        (let ((hunk-list (list (match-string 1))))
          (save-excursion
            (while (re-search-forward diff-hunk-header-re-unified end t)
              (setq hunk-list (cons (match-string 0) hunk-list))))
          (setq ahg-record-hunk-list (append (reverse hunk-list) ahg-record-hunk-list)))
        (setq end (point))))))

(defun ahg-record ()
 "Run hg record. Pops up a buffer for editing the log file."
  (interactive)
  (ahg-collect-hunk)
  (let ((buf (generate-new-buffer "*aHg-record-log*")))
    (ahg-log-edit
     'ahg-record-callback
     (lambda () ahg-record-hunk-list)
     buf)))

(defvar ahg-record-filter-commit-hunk-flg nil)
(defun ahg-record-filter (process output)
  (interactive)
  ;; debug code
  ;; (process-filter-insert-emu process "\n---head---\n")
  ;; (process-filter-insert-emu process output)
  ;; (process-filter-insert-emu process "\n---foot---\n")
  ;;
  (let ((record-prompt (regexp-quote "[Ynsfdaq?]"))
        (beg 0))
    (while (and (< beg (length output)) (string-match "^.*$" output beg)) ;; 行毎に区切る
      (setq beg (1+ (match-end 0)))
      (let ((line (match-string 0 output))
            (hunk (car ahg-record-hunk-list)))
        (if hunk
            (let ((hunk-mark (regexp-quote hunk)))
              (cond ((string-match (concat "^" hunk-mark) line)
                     (setq ahg-record-filter-commit-hunk-flg t))
                    ((string-match record-prompt line)
                     (cond ((string-match (concat "^examine.*changes to '" hunk-mark "'") line)
                            (process-send-string process "y\n")
                            (setq ahg-record-hunk-list (cdr ahg-record-hunk-list)))
                           ((and (string-match (concat "^record.*change .+ " record-prompt) line)
                                 ahg-record-filter-commit-hunk-flg)
                            (process-send-string process "y\n")
                            (setq ahg-record-hunk-list (cdr ahg-record-hunk-list))
                            (setq ahg-record-filter-commit-hunk-flg nil))
                           (t
                            (process-send-string process "n\n")))
                     )
                    ))
          (cond ((string-match record-prompt line)
                 (process-send-string process "n\n"))
                ((string-match "\\$ +$" line)
                 (process-send-string process "exit\n"))))
        ))))

(defun ahg-record-callback ()
  (interactive)
  (setq ahg-record-filter-commit-hunk-flg nil)
  (let* ((msg (ahg-parse-commit-message))
         (log-file (if ahg-commit-use-file (ahg-make-commit-logfile msg) nil))
         (args (if log-file (list "-l" log-file) (list "-m" (shell-quote-argument msg)))))
    (ahg-command-interactive
     "record" args
     ;; filter function
     'ahg-record-filter
     ;; sentinel function
     (lexical-let ((aroot (ahg-root))
                   (n (count-if (lambda (hunk-mark) (string-match diff-hunk-header-re-unified hunk-mark)) (log-edit-files)))
                   (log-buffer (current-buffer))
                   (log-file log-file)) ;; add by lugecy
       (lambda (process status)
         (if (string= status "finished\n")
             (progn
               (ahg-status-maybe-refresh aroot)
               (message "Successfully committed %s."
                        (if (> n 0)
                            (format "%d hunk%s" n (if (> n 1) "s" ""))
                          "all modified files"))
               (kill-buffer (process-buffer process))
               (kill-buffer log-buffer)
               (when log-file
                 (delete-file log-file))
               (setq ahg-record-filter-commit-hunk-flg nil)
               (run-hooks 'ahg-commit-after-hook)) ;; add by lugecy
           (ahg-show-error process))))
     )))

(defun ahg-command-interactive (command args filter sentinel
                                        &optional buffer use-shell
                                        no-show-message
                                        report-untrusted)
  (unless buffer (setq buffer (generate-new-buffer "*ahg-command-interactive*")))
  (with-current-buffer buffer
    (setq mode-line-process
          (list (concat ":" (propertize "%s" 'face '(:foreground "#DD0000"))))))
  (unless no-show-message (message "aHg: executing hg '%s' command..." command))
  (let ((lang (getenv "LANG")))
    (unless ahg-i18n (setenv "LANG"))
    (let ((process
           (start-process (concat "*ahg-command-" command "*") buffer
                          (or explicit-shell-file-name shell-file-name)
                          "-i")))
      (when ahg-subprocess-coding-system
        (set-process-coding-system process ahg-subprocess-coding-system))
      (set-process-filter
       process filter)
      (set-process-sentinel
       process
       (lexical-let ((sf sentinel)
                     (cmd command)
                     (no-show-message no-show-message))
         (lambda (p s)
           (unless no-show-message
             (message "aHg: executing hg '%s' command...done"
                      cmd))
           (setq mode-line-process nil)
           (funcall sf p s))))
      (process-send-string process (mapconcat 'concat (append (list ahg-hg-command command)
                                                              args) " "))
      (process-send-string process "\n"))
    (setenv "LANG" lang)))

;;;; commit時にqpop/qpushするかを尋ねるようにする
(defun ahg-qpop-no-sentinel ()
  (interactive)
  (let ((force-flg (and (ahg-uncommitted-changes-p)
                        (ahg-y-or-n-p "Exist local changes, Force qpop?"))))
    (with-temp-buffer
      (if (= (ahg-call-process "qpop" (if force-flg (list "-f" "-a") (list "-a"))) 0)
          (message "aHg: executing hg qpop command.....done")
        (message "aHg: error hg qpop")))))

(defun ahg-qpush-no-sentinel ()
  (interactive)
  (let ((force-flg (and (ahg-uncommitted-changes-p)
                        (ahg-y-or-n-p "Exist local changes, Force qpush?"))))
    (with-temp-buffer
      (if (= (ahg-call-process "qpush" (if force-flg (list "-f" "-a") (list "-a"))) 0)
          (message "aHg: executing hg qpush command.....done")
        (message "aHg: error hg qpush")))))

(defun ahg-exists-applied-p ()
  (with-temp-buffer
    (if (= (ahg-call-process "qapplied") 0)
        (> (point-max) (point-min)) ; bufferが空かの確認
      nil)))

(defun ahg-exists-unapplied-p ()
  (with-temp-buffer
    (if (= (ahg-call-process "qunapplied") 0)
        (> (point-max) (point-min)) ; bufferが空かの確認
      nil)))

(defun ahg-commit-before-qpop ()
  (when (and (ahg-exists-applied-p)
             (ahg-y-or-n-p "HG qpop all?"))
    (save-window-excursion
      (ahg-qpop-no-sentinel))))
(add-hook 'log-edit-done-hook 'ahg-commit-before-qpop)

(defun ahg-commit-after-qpush ()
  (when (and (ahg-exists-unapplied-p)
             (ahg-y-or-n-p "HG qpush all?"))
    (save-window-excursion
      (ahg-qpush-no-sentinel)
      (ahg-status-maybe-refresh (ahg-root)))))
(add-hook 'ahg-commit-after-hook 'ahg-commit-after-qpush)

;;;; hg record コマンドからdiff buffer を生成する
(define-key ahg-status-mode-map (kbd "R") 'ahg-record-diff)
(defun ahg-record-diff ()
  (interactive)
  (setq ahg-record-diff-end-command-flg nil
        ahg-record-diff-file-header nil)
  (let ((buffer (get-buffer-create "*aHg record diff*"))
        (curdir default-directory))
    (with-current-buffer buffer
      (setq default-directory (file-name-as-directory curdir)))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (ahg-push-window-configuration))
    (ahg-command-interactive "record" nil
                             'ahg-record-diff-parse ; filter func
                             (lambda (process status) ; sentinel func
                               (if (string= status "finished\n")
                                   (progn
                                     (pop-to-buffer (process-buffer process))
                                     (ahg-diff-mode)
                                     (set-buffer-modified-p nil)
                                     (setq buffer-read-only t)
                                     (setq ahg-record-diff-end-command-flg nil)
                                     (goto-char (point-min)))
                                 (ahg-show-error process)))
                             buffer))
  )

(defvar ahg-record-diff-end-command-flg nil)
(defvar ahg-record-diff-file-header nil)
(defun ahg-record-diff-parse (process output)
  ;; debug code
  ;; (process-filter-insert-emu process "\n---head---\n")
  ;; (process-filter-insert-emu process output)
  ;; (process-filter-insert-emu process "\n---foot---\n")
  ;;
  (let ((record-prompt (regexp-quote "[Ynsfdaq?]"))
        (beg 0))
    (while (and (< beg (length output)) (string-match "^.*$" output beg)) ;; 行毎に区切る
      (setq beg (1+ (match-end 0)))
      (let ((line (match-string 0 output)))
        (cond ((string-match "\\(^diff --git \\(a/[^ ]+\\) \\(b/[^ ]+\\)$\\)" ;; file header parts
                             line)
               (setq ahg-record-diff-file-header
                     (concat (match-string 1 line) "\n"
                             "--- " (match-string 2 line) "\n"
                             "+++ " (match-string 3 line) "\n")))
              ((string-match "^[0-9]+ hunks, [0-9]+ lines changed$" line)
               (process-filter-insert-emu process ahg-record-diff-file-header)
               (setq ahg-record-diff-file-header nil))
              ((string-match "^new file mode [0-9]+$" line)
               (process-filter-insert-emu process (replace-regexp-in-string "^--- .*" (concat line "\n" "--- /dev/null")
                                                                            ahg-record-diff-file-header)))
              ((string-match (concat "^examine.*changes .+ " record-prompt) line)
               (setq ahg-record-diff-end-command-flg t)
               (if ahg-record-diff-file-header
                   (progn
                     (process-send-string process "n\n")
                     (setq ahg-record-diff-file-header nil))
                 (process-send-string process "y\n")))
              ((string-match (concat "^record.*change .+ " record-prompt) line)
               (process-send-string process "n\n"))
              ((string-match "\\$ +$" line)
               (if ahg-record-diff-end-command-flg
                   (process-send-string process "exit\n"))
               (setq ahg-record-diff-end-command-flg nil))
              ((string-match "^[ @+-]" line)
               (process-filter-insert-emu process (concat line "\n")))
              )))))

(defun process-filter-insert-emu (process string)
  (with-current-buffer (process-buffer process)
    (let ((moving (= (point) (process-mark process))))
      (save-excursion
        ;; テキストを挿入し、プロセスマーカを進める
        (goto-char (process-mark process))
        (insert string)
        (set-marker (process-mark process) (point)))
      (if moving (goto-char (process-mark process))))))

;;;; ahg-generic-command 実行時に与えるglobal optionを設定する
(setq ahg-hg-command-generic-arguments (list "--config" "pager.pager=cat"))

;;;; ahg-annotate
(define-key ahg-global-map (kbd "a") 'ahg-annotate)
(define-derived-mode ahg-annotate-mode nil "ahg-annotate"
  "Major mode to show revision number log.

Commands:
\\{ahg-annotate-mode-map}
"
  (buffer-disable-undo)
  (toggle-read-only t)
  (define-key ahg-annotate-mode-map (kbd "<return>") 'ahg-annotate-show-rev-number-log)
  (define-key ahg-annotate-mode-map (kbd "q") 'ahg-buffer-quit))

(defun ahg-annotate ()
  (interactive)
  (let ((buffer (get-buffer-create "*ahg annotate*"))
        (command-list (list (buffer-file-name)))
        (line-num (line-number-at-pos)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (ahg-push-window-configuration)))
    (ahg-generic-command
     "annotate" command-list
     (lexical-let ((line-num line-num))
       (lambda (process status)
         (if (string= status "finished\n")
             (progn
               (pop-to-buffer (process-buffer process))
               (ahg-annotate-mode)
               (goto-line line-num)))))
     buffer)))

(defun ahg-annotate-get-rev-num-on-line ()
  "From xhg-annotate-get-rev-num-on-line"
  (let ((cur-line (buffer-substring (point-at-bol) (point-at-eol))))
    (when (string-match "^ *[0-9]*" cur-line)
      (replace-regexp-in-string " " "" (match-string 0 cur-line)))))

(defvar ahg-log-with-patch-flg nil)
(defun ahg-annotate-show-rev-number-log ()
  (interactive)
  (let ((rev (ahg-annotate-get-rev-num-on-line))
        (ahg-log-with-patch-flg t))
    (ahg-log rev nil "-p")))

;;;; log + patch view on ahg-short-log mode
(define-key ahg-short-log-mode-map (kbd "P") 'ahg-short-log-view-details-with-patch)
(defun ahg-short-log-view-details-with-patch ()
  (interactive)
  (let ((rev (ahg-short-log-revision-at-point))
        (ahg-log-with-patch-flg t))
    (ahg-log rev nil "-p")))
