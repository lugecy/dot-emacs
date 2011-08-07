;;;; Emacs 23.x font setting
(set-default-font "VL ゴシック-10")
(set-fontset-font "fontset-default" ;;(frame-parameter nil 'font)
                  'japanese-jisx0208
                  '("VL ゴシック-10" . "unicode-bmp"))
(setq initial-frame-alist
      (append `(
                (width . 105)
                (height . 35)
                (font . ,(frame-parameter nil 'font)) ;;この部分はset-default-fontした後でないと意味無し
                ;; (alpha . (85 85))
                )
              initial-frame-alist))
(setq default-frame-alist initial-frame-alist)

;;;; coding-system
(set-language-environment 'japanese) ;; linuxだとeuc-jpが先になる？
(prefer-coding-system 'utf-8-unix)
;; (setq default-buffer-file-coding-system 'utf-8-unix)
;; (setq default-process-coding-system '(japanese-shift-jis-dos . japanese-shift-jis-unix))

;;;; clmemo
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
;; (when (and (string-match "linux" system-configuration)
;; 		   (locate-library "uim"))
;;   (require 'uim)
;;   (global-set-key (kbd "C-x C-j") 'uim-mode)
;;   (setq uim-show-im-name t)
;;   (setq uim-show-im-mode t))

;;;; X window clipboard setting
;; (setq x-select-enable-clipboard t)

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

