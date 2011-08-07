;;;; Emacs 23.x font setting
;; (set-default-font "ＭＳ ゴシック-12")
;; (set-default-font "M+ 1m-12")
;; (set-default-font "ゆたぽん（コーディング）Backsl-12")
;; (set-default-font "Osaka－等幅-12")
(set-face-attribute 'default nil
                    :family "ゆたぽん（コーディング）Backsl"
                    :height 120)
;; (set-face-attribute 'default nil
;;                     :family "Osaka－等幅"
;;                     :height 120)
;;; 漢字部分だけ別フォントにするなどの設定
;; (set-fontset-font "fontset-default" ;;(frame-parameter nil 'font)
;;                   'japanese-jisx0208
;;                   '("ＭＳ ゴシック" . "unicode-bmp")) ;;(font-spec :family "<fontname>")
;;; for new frame
(setq initial-frame-alist
      (append `(
                ;; (font . ,(frame-parameter nil 'font)) ;;この部分はset-default-fontした後でないと意味無し
                ;; (width . 120)
                ;; (height . 40)
                ;; (alpha . (85 85))
                )
              initial-frame-alist))
(setq default-frame-alist initial-frame-alist)

;;;; coding-system
(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)
(setq file-name-coding-system 'cp932)
(setq default-buffer-file-coding-system 'utf-8-unix)
;; (setq default-process-coding-system '(utf-8-dos . utf-8-unix))
;; (set-default-coding-systems 'utf-8)


;;;; clmemo
(defvar clmemo-directory "e:/Document/My Dropbox/howm/")
(setq user-full-name "lugecy"
      user-mail-address "lugecy@GogoTea")
(when (ly:custom-conf-load "clmemo")
  (setq clmemo-file-name (concat clmemo-directory "memo/clmemo.howm"))
  (clmemo-recent-header-picks))
(autoload 'snap-record "snap" "generate general bookmark." t)
(autoload 'snap-play "snap" "generate general bookmark." t)
(ly:eval-after-load 'snap
  (setq snap-abbrev `(("clmemo" "" ,clmemo-directory))))

;;;; org-remember
(setq org-directory (concat clmemo-directory "memo/"))
(setq org-default-notes-file (concat org-directory "task_suck.org")
      org-agenda-files '("e:/Document/My Dropbox/howm/memo/agenda.org")
      org-reverse-note-order '(("task_suck" . "Task-suck")
                               ("idea_suck" . "Idea-suck")
                               ("org-journal" . "Journal")))
(setq ly:org-workspace (concat clmemo-directory "memo/agenda.org"))
(setq org-journal-file (concat clmemo-directory "memo/org-journal.org"))
(add-to-list 'org-remember-templates `("Journal" ?j "** %?\n   %T\n" ,org-journal-file "Journal") t)
(global-set-key (kbd "<M-f9>") (lambda () (interactive) (find-file ly:org-workspace)))

;;;; howm
;; (ly:custom-conf-load "howm")
;; (when (featurep 'howm)
;;   (setq howm-directory clmemo-directory)
;;   (setq howm-keyword-file (concat howm-directory "howm-keys")))

;;;; 使い捨てコード用のファイルを作る
;;;; http://d.hatena.ne.jp/rubikitch/20080923/1222104034
(defvar open-junk-file-path "~/trial/junk/")
(defun open-junk-file ()
  (interactive)
  (let* ((file (expand-file-name
                (format-time-string
                 "%Y/%m/junk-%Y%m%d-%H%M%S." (current-time))
                open-junk-file-path))
         (dir (file-name-directory file)))
    (make-directory dir t)
    (find-file-other-window (read-string "Junk Code: " file))))

;;; ddskk
(when (ly:custom-conf-load "skk")
  (setq skk-server-host "localhost")
  (setq skk-server-portnum 1178)
  ;; (setq skk-large-jisyo "~/packages/etc/skk/SKK-JISYO.L")
  (setq skk-aux-large-jisyo "e:/Var/skkime/SKK-JISYO-unicode.L")
  (setq skk-jisyo "e:/Var/skkime/skk-user-jisyo")
  (setq skk-jisyo-code 'utf-16-le-unix)
  (ly:custom-conf-load "ac-skk"))

;;;; migemo.el
(when (executable-find "cmigemo")
  (require 'migemo)
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs" "-i" "\g"))
  (setq migemo-dictionary (expand-file-name "e:/cygwin/usr/local/share/cmigemo/migemo-dict"))
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil))

;;;; for cygwin
(when (and (featurep 'dos-w32)
           (executable-find "mount.exe")
           (locate-library "cygwin-mount"))
  (require 'cygwin-mount)
  (cygwin-mount-activate))

;;;; shell / ansi-term
(setq shell-file-name "bash"
      explicit-shell-file-name "f_zsh.exe")
(defadvice ansi-term (after coding-set activate)
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))

;;;; cygwin上のMercurial用設定
(setq ahg-subprocess-coding-system 'utf-8)
(setenv "HGENCODING" "utf-8")
(setq ahg-commit-use-file t) ;; my patch add option

;;;; auto-install
;; (require 'auto-install)
(autoload 'auto-install-from-buffer "auto-install" nil t)
(autoload 'auto-install-from-url "auto-install" nil t)
(autoload 'auto-install-from-emacswiki "auto-install" nil t)
(setq auto-install-directory "~/.emacs.d/trial-elisp/")
(setq auto-install-batch-list
      '(("private-anything-batch" nil nil
         ("http://www.emacswiki.org/emacs/download/anything.el" ; Main library
          "http://www.emacswiki.org/emacs/download/anything-config.el" ; Configuration for anything.el
          "http://www.emacswiki.org/emacs/download/anything-match-plugin.el" ; Matching algorithm humanely
          "http://www.emacswiki.org/emacs/download/anything-complete.el" ; Completion
          "http://www.emacswiki.org/emacs/download/anything-show-completion.el" ; Show completion prettily
          ))))

;; diredからupgradeするためのコマンド
(defun ly:check-with-update-elisp ()
  (interactive)
  (when (ly:check-exist-emacswiki)
    (sit-for 1)
    (auto-install-from-dired)))

(defun ly:check-exist-emacswiki ()
  (interactive)
  (let (find-p)
    (if (and (boundp 'auto-install-package-name-list)
             auto-install-package-name-list)
        (let ((filename (dired-get-filename nil t)))
          (if (and filename
                   (not (file-directory-p filename))
                   (member (file-name-nondirectory filename) auto-install-package-name-list))
              (progn
                (message "exist on emacswiki" filename)
                (setq find-p t))
            (message "No exist")))
      (message "Not boundp or nil package-name-list"))
    find-p))

;;;; flymake on cygwin (gcc is symlink)
;; (setq flymake-cc-program "gcc-3")

;;;; flymake-elisp-mode for gogotea cygwin (redefine)
(defvar elisplint-path "e:/cygwin/usr/local/bin/elisplint.sh")
(when (file-exists-p elisplint-path)
  (defun flymake-elisp-init ()
    (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-inplace))
           (local-file  (file-relative-name
                         temp-file
                         (file-name-directory buffer-file-name))))
      (list "bash" (list elisplint-path local-file)))))

;;;; for anything-c-source-file-search
(setq anything-c-source-file-search-program "e:/cygwin/bin/find.exe")
(defun my-get-source-directory (path) "e:/cygwin/home/lugecy/.emacs.d/")

;;;; for c-eldoc (on MinGW)
(setq c-eldoc-includes "")

;;;; for cygwin-1.7
(setenv "CYGWIN" "nodosfilewarning error_start=dumper")

;;;; for collaborate dwm-win32
(setq elscreen-display-tab t)

;;;; msysgit を magit で使う
(setq magit-git-executable "e:/Program/Git/bin/git.exe")

;;;; for horizontally window split
(setq split-width-threshold 160)

;;;; for woman cache
(setq woman-cache-filename (expand-file-name "~/.wmncache.el"))

;;;; for Twittering-mode.el
(ly:custom-conf-load "twittering-mode")

;;;; configure open url
(setq browse-url-generic-program "e:/Program/Mozilla Firefox/firefox.exe"
      browse-url-browser-function 'browse-url-generic)

;;;; for web service client
(defun ly:web-clientize ()
  (interactive)
  (setq kill-emacs-hook nil)
  (when (and (server-running-p)
             server-process)
    (server-force-delete))
  (dolist (timer-sym '(ly:idle-save-ac-comphist ly:idle-save-session))
    (when (symbol-value timer-sym)
      (cancel-timer (symbol-value timer-sym))
      (set timer-sym nil))))

;;;; for lgrep, rgrep
(setq find-program "/usr/bin/find")
(setq grep-find-use-xargs t)
(ly:eval-after-load 'grep
  (add-to-list 'grep-find-ignored-files "!*")
  (add-to-list 'grep-find-ignored-directories "uthist/root-obj"))

;;;; for rurema/refe
(autoload 'anything-rurima "anything-rurima" nil t)
(setq anything-rurima-index-file "~/.rurema/doctree/rurema.e")
(autoload 'anything-refe "anything-refe-kitokitoki.el" nil t)
(setq anything-refe-index-file "~/.rurema/refe.index")
(setq anything-refe-command "rurema")

;;;; for tramp
(setq tramp-default-method "sshx")
(add-to-list 'recentf-exclude "^/sshx?")
