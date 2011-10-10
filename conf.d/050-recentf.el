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

