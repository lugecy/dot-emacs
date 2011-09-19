;;;; Language configure
(set-keyboard-coding-system 'cp932)
(prefer-coding-system 'utf-8)
(set-file-name-coding-system 'cp932)
(setq default-buffer-file-coding-system 'utf-8-unix)

;;;; input-method configure
(setq default-input-method "W32-IME")
;; IME状態のモードライン表示
(setq-default w32-ime-mode-line-state-indicator "[Aa]")
(setq w32-ime-mode-line-state-indicator-list '("[Aa]" "[あ]" "[Aa]"))
;; IMEの初期化
(w32-ime-initialize)
;; with mayu
(defun ly:input-method-active-p ()
  (or (and (boundp 'viper-special-input-method)
                   viper-special-input-method)
      current-input-method))
(defun ly:enable-input-method ()
  (interactive)
  (unless (ly:input-method-active-p)
    (toggle-input-method)))
(defun ly:disable-input-method ()
  (interactive)
  (when (ly:input-method-active-p)
    (toggle-input-method)))
(global-set-key (kbd "<insert>") 'ly:enable-input-method)
(global-set-key (kbd "<pause>") 'ly:disable-input-method)

;;;; clmemo
(defvar clmemo-directory "d:/Document/clmemo/")
(setq user-full-name "lugecy"
      user-mail-address "lugecy@GogoTea")
(when (ly:custom-conf-load "clmemo")
  (setq clmemo-file-name (concat clmemo-directory "memo/clmemo.howm"))
  (setq clmemo-bookmark-suck-file-name (expand-file-name "memo/bookmark-suck.txt" clmemo-directory))
  (setq mclgrep:target-files (list clmemo-file-name clmemo-bookmark-suck-file-name))
  (clmemo-recent-header-picks)
  (global-set-key (kbd "<f5> <f5>") 'ly:clmemo-add-bookmark-obj-from-clipboard))
(autoload 'snap-record "snap" "generate general bookmark." t)
(autoload 'snap-play "snap" "generate general bookmark." t)
(ly:eval-after-load 'snap
  (setq snap-abbrev `(("clmemo" "" ,clmemo-directory))))
;;; (thing-at-point 'snap)がバッファにsnap-regexpに該当するマークが無い場合に
;;; 誤動作するバグを修正
(put 'snap 'bounds-of-thing-at-point 'bounds-of-thing-snap-at-point)
(defun bounds-of-thing-snap-at-point ()
  (if (thing-at-point-looking-at snap-regexp)
      (let ((beg (match-beginning 0))
            (end (match-end 0)))
        (cons beg end))))

;;;; disable backup/autosave
(setq make-backup-files nil
      backup-inhibited t
      auto-save-default nil)

;;;; shell configure
(setq shell-file-name "bash.exe")
(setq shell-command-switch "-c")
(setq explicit-shell-file-name "bash.exe")

;;;; for rgrep
(setq grep-find-use-xargs t)

;;;; cygwin configure
(require 'cygwin-mount)
(cygwin-mount-activate)
(ly:eval-after-load 'magit
  (setq magit-use-cygwin-git-flag t))

;;;; for anything-filelist
(setq anything-c-filelist-file-name "d:/tmp/cygwin.filelist")
(add-to-list 'anything-for-open-source-list 'anything-c-source-filelist t)

;;;; for inf-ruby
(setq ruby-program-name "f_irb --inf-ruby-mode")

;;;; add loadpath temporary elisp directory
(add-load-path-subdirs (expand-file-name "~/.emacs.d/trial-elisp"))

;;;; for sqlite3 program
(setq sql-sqlite-program "f_sqlite3")

;;;; auto-install
;; (require 'auto-install)
(autoload 'auto-install-from-buffer "auto-install" nil t)
(autoload 'auto-install-from-url "auto-install" nil t)
(autoload 'auto-install-from-emacswiki "auto-install" nil t)
(autoload 'auto-install-from-gist "auto-install" nil t)
(setq auto-install-directory "~/.emacs.d/trial-elisp/")

;;;; migemo.el
(when (executable-find "cmigemo")
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-directory (expand-file-name "/usr/local/share/migemo/utf-8"))
  ;; (setq migemo-dictionary (expand-file-name "migemo-dict" migemo-directory))
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (require 'migemo)
  ;;; yomoyaさんによる改良でemacs終了時に確認するようになったのでそれを無効に
  (defadvice migemo-init (after fix-with-query-exit activate)
    (when (and migemo-process
               (eq (process-status migemo-process) 'run)
               (process-query-on-exit-flag migemo-process))
      (set-process-query-on-exit-flag migemo-process nil))))
(ly:eval-after-load 'org
  (ly:eval-after-load 'migemo
    (defun org-migemo-occur (regexp &optional keep-previous)
      (interactive "sRegexp: \nP")
      (let ((cnt (org-occur (migemo-get-pattern regexp) keep-previous)))
        (message "%d match(es) for regexp %s with migemo." cnt regexp)))))

;;;; 使い捨てコード用のファイルを作る
;;;; http://d.hatena.ne.jp/rubikitch/20080923/1222104034
(defvar open-junk-file-path "~/s/junk/")
(defun open-junk-file ()
  (interactive)
  (let* ((file (expand-file-name
                (format-time-string
                 "%Y/%m/junk-%Y%m%d-%H%M%S." (current-time))
                open-junk-file-path))
         (dir (file-name-directory file)))
    (make-directory dir t)
    (find-file-other-window (read-string "Junk Code: " file))))

;;;; for eshell
(ly:eval-after-load 'esh-ext
  (setq eshell-force-execution t)
  (add-hook 'eshell-alternate-command-hook 'ly:eshell-fix-execute-script)
  (defun ly:eshell-fix-execute-script (file)
    (when (string= file "/bin/bash")
      (eshell-search-path "bash"))))
(setq eshell-history-size 1024)

;;;; color-theme configure (need frame parameter after)
(and (load "zen-and-art") (color-theme-zen-and-art))
