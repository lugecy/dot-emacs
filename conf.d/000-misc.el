;;;; ツールバーを消す
(tool-bar-mode -1)

;;;; スクロールバーを左に表示
(set-scroll-bar-mode 'left)

;;;; C-h = backspace
(keyboard-translate ?\C-h 'backspace)

;;;; help-for-help
(global-set-key (kbd "C-c h h") 'help-command)

;;;; Visual Bell を使う
(setq visible-bell t)

;;;; 勝手にリージョンを作成させないために・・ emacs23 ではdefault
(transient-mark-mode t)

;;;; C-u C-SPC C=SPC .. で pop-mark-command を連続実行になるように
(setq set-mark-command-repeat-pop t)

;;;; region の色指定(viper-visual-modeと区別がつくように)
(set-face-background 'region "RoyalBlue3")

;;;; 対応する括弧を強調表示
(show-paren-mode t)
(setq show-paren-style 'mixed)

;;;; C-w の挙動をshell compatible に
;; EmacsWiki: Default Killing And Yanking
;; http://www.emacswiki.org/emacs/DefaultKillingAndYanking
(defun unix-werase-or-kill (arg)
  (interactive "*p")
  (if (and transient-mark-mode
           mark-active)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))
;; (global-set-key (kbd "C-w") 'unix-werase-or-kill) ;; default kill-region

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
  (setq diff-header-removed-file-face 'diff-header-removed-file)
  (defface diff-header-added-file
    '((t :foreground "green"))
    "mercurial diff like highlight diff-header-added-file")
  (setq diff-header-added-file-face 'diff-header-added-file)
  (defface diff-file-header-removed-file
    '((t :foreground "red" :inherit diff-file-header))
    "mercurial diff like highlight diff-file-header-removed-file")
  (setq diff-file-header-removed-file-face 'diff-file-header-removed-file)
  (defface diff-file-header-added-file
    '((t :foreground "green" :inherit diff-file-header))
    "mercurial diff like highlight diff-file-header-added-file")
  (setq diff-file-header-added-file-face 'diff-file-header-added-file)
  (setq diff-font-lock-keywords
	(append
	 '(("^diff .+\n" . diff-header-face)
	   ("^\\(---\\) \\([^\t\n]+?\\)\\(?:\t.*\\| \\(\\*\\*\\*\\*\\|----\\)\\)?\n"
	    (0 diff-header-face)
	    (1 diff-header-removed-file-face prepend)
	    (2 (if (not (match-end 5)) diff-file-header-removed-file-face) prepend))
	   ("^\\(\\+\\+\\+\\) \\([^\t\n]+?\\)\\(?:\t.*\\| \\(\\*\\*\\*\\*\\|----\\)\\)?\n"
	    (0 diff-header-face)
	    (1 diff-header-added-file-face prepend)
	    (2 (if (not (match-end 5)) diff-file-header-added-file-face) prepend)))
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

;;;; バックアップファイルを別フォルダに隔離
;(setq make-backup-files t)
(setq backup-directory-alist
      (cons (cons "\\.*$" (expand-file-name "~/.emacs.d/backup"))
            backup-directory-alist))

;;;; 行数・桁を表示
;;(line-number-mode t)
(column-number-mode t)

;;;; 行番号を表示
;; (when (require 'linum nil t)
;;   (setq linum-delay t)
;;   (setq linum-eager nil))

;;;; タブ幅の設定
(setq-default tab-width 4)
(setq-default tab-stop-list
              '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
;; 特定のモードではインデントにタブを使わない(ソフトタブモード)
(defun ly:turn-off-indent-tabs-mode ()
  (setq indent-tabs-mode nil))
(dolist (hook (list 'emacs-lisp-mode-hook
                    'lisp-interaction-mode-hook
                    'org-mode-hook))
  (add-hook hook 'ly:turn-off-indent-tabs-mode))

;;;; 行末の空白を強調表示
;; (setq-default show-trailing-whitespace t)
(defun ly:turn-on-show-trailing-whitespace ()
  (setq show-trailing-whitespace t))
(dolist (hook (list 'emacs-lisp-mode-hook 'lisp-interaction-mode-hook
                    'ruby-mode-hook 'c-mode-common-hook 'python-mode-hook
                    'sh-mode-hook 'makefile-mode-hook
                    'change-log-mode-hook 'howm-mode-hook 'diff-mode-hook))
  (add-hook hook 'ly:turn-on-show-trailing-whitespace))

;;;; for whitespace-mode
(eval-after-load "whitespace"
  '(progn
     (set-face-background 'whitespace-tab "snow4")
     (setq whitespace-style (delq 'tab-mark whitespace-style))))

;;;; Vimの'*'検索をC-sでエミュレート
(defun isearch-yank-point-symbol ()
  (interactive)
  (isearch-yank-string (thing-at-point 'symbol)))
(define-key isearch-mode-map "\C-e" 'isearch-yank-point-symbol)

;;;; imenu
(require 'imenu)
(setq imenu-auto-rescan t)
;; (global-set-key [?\C-c ?\C-.] 'imenu)

;;;; completionバッファの色付け
;;;; tips
;;;; http://homepage1.nifty.com/blankspace/emacs/tips.html
(defadvice display-completion-list (after display-completion-list-highlight activate)
  (let* ((str-list (mapcar (lambda(x) (cond ((stringp x) x)
                                            ((symbolp x) (symbol-name x))
                                            ((listp x) (concat (car x)
                                                               (cadr x)))))
                           (ad-get-arg 0)))
         (str (car str-list)))
    (mapc (lambda (x)
            (while (or (> (length str) (length x))
                       (not (string= (substring str 0 (length str))
                                     (substring   x 0 (length str)))))
              (setq str (substring str 0 -1))))
          str-list)
    (save-current-buffer
      (set-buffer "*Completions*")
      (save-excursion
        (re-search-forward "Possible completions are:" (point-max) t)
        (while (re-search-forward (concat "[ \n]\\<" str) (point-max) t)
          (let ((o1 (make-overlay (match-beginning 0) (match-end 0)))
                (o2 (make-overlay (match-end 0)       (1+ (match-end 0)))))
            (overlay-put o1 'face '(:foreground "HotPink3"))
            (overlay-put o2 'face '(:foreground "white" :background "DeepSkyBlue4"))))))))

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

;;;; wdired.el(標準添付)
(require 'wdired)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

;;;; コメントアウトキーマップ
(global-set-key (kbd "C-c ;") 'comment-dwim)
(global-set-key (kbd "C-c C-;") 'comment-dwim)

;;;; for Cygwin shell
(setq shell-command-switch "-c")
(setq explicit-bash-args '("--noediting" "-i"))
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(setq comint-prompt-read-only t)

;;;; 日付を挿入する
(defun my-insert-time ()
  (interactive)
  (insert (format-time-string "[%Y-%m-%d]")))
;; (define-key global-map [f5] 'my-insert-time)

;;;; ファイルを保存した時に勝手にパーミッションが変更されるのを防ぐ
;; (setq vc-mistrust-permissions t)
;; 不要なvc-backendを無効に
(setq vc-handled-backends '(Git Hg RCS))

;;;; バッファの行数をモードラインに表示する
(loop for s on mode-line-position
      if (eq (caar s) 'size-indication-mode)
      do (setcdr s (cons '(:eval (format "/%d" (count-lines (point-min) (point-max)))) (cdr s)))
      and return nil)

;;;; message buffer の長さを伸長 (default: 100)
(setq messages-buffer-max-lines 1000)

;;;; 同名のbuffer-nameを判別しやすくする
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
;; mode-lineのbuffer-nameをdirectory名まで常に表示する
(setq uniquify-min-dir-content 1)

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
(global-set-key (kbd "C-c y") 'djcb-duplicate-line)

;;;; minibufferでの入力履歴から重複を削除
(setq history-delete-duplicates t)

;;;; vc-git bugfix
;;;; ファイルがリネームされていた際にfile名を正しく見付けらなくなるbugをfix
(ly:eval-after-load "vc-git"
  (defun vc-git-annotate-extract-revision-at-line ()
    (save-excursion
      (move-beginning-of-line 1)
      (when (looking-at "\\([0-9a-f^][0-9a-f]+\\) \\(\\([^( ]+\\) \\)?")
        (let ((revision (match-string-no-properties 1)))
          (if (match-beginning 2)
              (cons revision
                    (expand-file-name (match-string-no-properties 3)
                                      (vc-git-root default-directory)))
            revision))))))

;;;; 内容の無いファイルを削除
;;;; http://www.bookshelf.jp/soft/meadow_24.html#SEC264
(defun delete-file-if-no-contents ()
  (when (and
         (buffer-file-name (current-buffer))
         (= (point-min) (point-max)))
    (when (y-or-n-p "Delete file and kill buffer?")
      (delete-file
       (buffer-file-name (current-buffer)))
      (kill-buffer (current-buffer)))))
(add-hook 'after-save-hook 'delete-file-if-no-contents)

;;;; recentf.el
(require 'recentf)
(setq recentf-max-menu-items 10)
(setq recentf-max-saved-items 1000)
(setq recentf-exclude '("\\.git.*" "\\.hg.*" "\\.svn.*" ".emacs.bmk$" "\\.howm-key$"))
(setq recentf-auto-cleanup 300)
(recentf-mode 1)
;; ディレクトリも履歴に追加する
;; 人は俺を「recentfマスター」と呼ぶ - (rubikitch loves (Emacs Ruby CUI))
;; http://d.hatena.ne.jp/rubikitch/20091224/recentf
(defun recentf-add-dired-directory ()
  (when (and (stringp dired-directory)
             (equal "" (file-name-nondirectory dired-directory)))
    (recentf-add-file dired-directory)))
(add-hook 'dired-mode-hook 'recentf-add-dired-directory)

;;;; yankと同時にindentも行う (C-u付きでindent無しでyank)
;; EmacsWiki: Auto Indentation
;; http://www.emacswiki.org/emacs/AutoIndentation
(dolist (command '(insert-for-yank))    ;or (yank yank-pop)
  (eval `(defadvice ,command (after indent-region activate)
           (and (not current-prefix-arg)
                (member major-mode '(emacs-lisp-mode lisp-mode
                                                     clojure-mode    scheme-mode
                                                     haskell-mode    ruby-mode
                                                     rspec-mode      python-mode
                                                     c-mode          c++-mode
                                                     objc-mode       latex-mode
                                                     plain-tex-mode  org-mode))
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))

;;;; kill-emacs 時に確認する
(setq confirm-kill-emacs 'yes-or-no-p)

;;;; 時折刺さるのでview-hello-fileを"C-c h h"から発動不可に
(define-key help-map (kbd "h") 'undefined)

;;;; C-M-V が押しにくいので
(global-set-key (kbd "C-M-y") 'scroll-other-window-down)

;;;; python-modeでffap-guessが固まるので(emacs.pyが必要)
(ly:eval-after-load 'python
  (ly:eval-after-load 'ffap
    (setq ffap-alist (assq-delete-all 'python-mode ffap-alist))))

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
(global-set-key (kbd "C-c C-r") 'ly:yank-before-point-target)
