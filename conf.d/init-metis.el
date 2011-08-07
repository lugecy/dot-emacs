;;;; Emacs 23.x font setting
;; (set-default-font "VL ゴシック-10")
;; (set-fontset-font "fontset-default" ;;(frame-parameter nil 'font)
;;                   'japanese-jisx0208
;;                   '("VL ゴシック-10" . "unicode-bmp"))
(setq initial-frame-alist
      (append `(;;(font . ,(frame-parameter nil 'font)) ;;この部分はset-default-fontした後でないと意味無し
                (width . 105)
                (height . 40)
                )
              initial-frame-alist))
(setq default-frame-alist initial-frame-alist)

;;;; coding-system
(set-language-environment 'japanese) ;; linuxだとeuc-jpが先になる？
(prefer-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
;; (setq default-buffer-file-coding-system 'utf-8-unix)
;; (setq default-process-coding-system '(japanese-shift-jis-dos . japanese-shift-jis-unix))

;;;; clmemo
(defvar clmemo-directory (expand-file-name "~/memo/"))
(setq user-full-name "lugecy"
      user-mail-address "lugecy@metis")
(when (ly:custom-conf-load "clmemo")
  (setq clmemo-file-name (concat clmemo-directory "clmemo.howm"))
  (clmemo-recent-header-picks))
(autoload 'snap-record "snap" "generate general bookmark." t)
(autoload 'snap-play "snap" "generate general bookmark." t)

;; (defvar clmemo-directory "e:/Document/My Dropbox/howm/")
;; (setq user-full-name "lugecy")
;; (setq user-mail-address "lugecy@GogoTea")
;; (load "conf-clmemo")
;; (when (featurep 'clmemo)
;;   (setq clmemo-file-name (concat clmemo-directory "memo/clmemo.howm"))
;;   (clmemo-recent-header-picks))

;;;; howm
;; (load "conf-howm")
;; (when (featurep 'howm)
;;   (setq howm-directory clmemo-directory)
;;   (setq howm-keyword-file (concat howm-directory "howm-keys")))

;;;; 使い捨てコード用のファイルを作る
;;;; http://d.hatena.ne.jp/rubikitch/20080923/1222104034
;;(defvar open-junk-file-path (concat clmemo-directory "memo/"))
;; (defun open-junk-file ()
;;   (interactive)
;;   (let* ((file (expand-file-name
;;                 (format-time-string
;;                  "%Y/%m/%d/%Y%m%d-%H%M%S." (current-time))
;;                 open-junk-file-path))
;;          (dir (file-name-directory file)))
;;     (make-directory dir t)
;;     (find-file-other-window (read-string "Junk Code: " file))))

;;; ddskk
;; (when (load "conf-skk")
;;   (setq skk-server-host "kuro-box")
;;   (setq skk-server-portnum 1178)
;;   (setq skk-large-jisyo "~/packages/etc/skk/SKK-JISYO.L")
;;   (setq skk-aux-large-jisyo "e:/Var/skkime/SKK-JISYO-unicode.L")
;;   (setq skk-jisyo "e:/sysprofile/skkime/skk-user-jisyo")
;;   (setq skk-jisyo-code 'utf-16-le-unix))

;;;; uim.el
(require 'uim)
(global-set-key (kbd "C-x C-j") 'uim-mode)
(setq uim-show-im-name t)
(setq uim-show-im-mode t)
(add-hook 'after-init-hook 'uim-reorder-minor-mode-map-alist)
;; (require 'uim-leim)
;; (set-input-method "japanese-anthy-uim")
;; (toggle-input-method nil)
;; (global-set-key "\C-x\C-j" 'toggle-input-method)
;; (setq uim-default-im-prop '("action_anthy_hiragana"))

;;;; X window clipboard setting
(setq x-select-enable-clipboard t)

;;;; migemo.el
;; (when (executable-find "cmigemo")
;;   (require 'migemo)
;;   (setq migemo-command "cmigemo")
;;   (setq migemo-options '("-q" "--emacs" "-i" "\g"))
;;   (setq migemo-dictionary (expand-file-name "../../etc/cmigemo/migemo-dict" (invocation-directory)))
;;   (setq migemo-user-dictionary nil)
;;   (setq migemo-regex-dictionary nil))

;; JMANページの設定
;; (setq woman-manpath (list (expand-file-name "~/man")))
;; (setq woman-manpath (list (expand-file-name "../../etc/man" (invocation-directory))))

;;;; custom keymap
(global-set-key (kbd "C-'") 'anything-for-open)
(global-set-key (kbd "C-c C-'") 'anything-for-current-buffer)
(global-set-key (kbd "C-M-'") 'anything-elisp-apropos)

;;;; daemon起動時にterminalでの文字化け防止
(defun ly:set-terminal-configure (frame &optional keep-window-configuration)
  (with-selected-frame frame
    (keyboard-translate ?\C-h 'backspace)
    (set-terminal-parameter frame 'background-mode 'dark)))
(add-hook 'after-make-frame-functions 'ly:set-terminal-configure t)
