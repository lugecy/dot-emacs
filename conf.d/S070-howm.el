;; -*- coding:utf-8 -*-
(setq howm-menu-lang 'ja)
;;(setq howm-file-name-format "memo/%Y/%Y%m%d-%H%M%S.howm")
(setq howm-file-name-format "memo/%Y/%m/%d/%Y%m%d-%H%M%S.howm")
(setq howm-menu-file "howm-menu")
;;(setq howm-keyword-file (concat howm-directory "howm-keys"))
(setq dired-bind-jump nil)
(require 'howm)

(setq howm-menu-allow
      (append '(symbol-value) howm-menu-allow))
;;;; experimental features
(setq howm-todo-separators
      '((0 . "----------↑超過----------")
	(nil . "----------潜伏↓----------")))
(setq howm-view-use-grep t ; view grepにコマンドgrepを使う
      howm-view-search-in-result-correctly nil) ; Non-nilで，絞り込みの動作がバッファ単位ではなくメモ単位になる(重い)
;; 酔歩間隔
(setq howm-random-walk-wait 5) ;; default 2

;(howm-setup-change-log)
;;;; howm + chmemo でタイトルを拾う設定
(eval-after-load "howm-view"
  '(setq howm-view-title-regexp
         (concat howm-view-title-regexp
                 ;; "\\|^[0-9-]+[ \t]+<.*>$")))
                 ;; "\\|^[0-9-]+[ \t]+\\w+[ \t]+<.*>$")))
                 "\\|^\t\\* "           ; change-log タイトル
                 "\\|^;+= +.*$" ; open-junk-file でのコメント内タイトル(Emacs lisp)
                 "\\|^\\*+ " ; org files の見出し
                 )))

;;;; 拡張子howmでhowm-modeに
(add-hook 'find-file-hook (lambda () (if (string-match "\\.howm$" (buffer-file-name)) (howm-mode t))))

;;;; ファイルへのリンクを生成するelisp
(require 'snap)

;;;; viperでもaction-lockを素直に動作させる
(eval-after-load "viper-init"
  '(progn
     (defvar action-lock-original-return-vi-state 'viper-next-line-at-bol)
     (defvar action-lock-original-return-insert-state (lookup-key global-map (kbd "C-m")))
     (defun ttm/howm-set-al-origin-return-on-viper (state)
       (setq action-lock-original-return
             (cond ((eq state 'vi-state) action-lock-original-return-vi-state)
                   (t action-lock-original-return-insert-state))))
     (defun ttm/howm-add-viper-return ()
       (if action-lock-mode
           (progn
             (viper-add-local-keys 'vi-state '(("\C-m" . action-lock-magic-return)))
             (viper-add-local-keys 'insert-state '(("\C-m" . action-lock-magic-return)))
             (ttm/howm-set-al-origin-return-on-viper viper-current-state))
         (viper-add-local-keys 'vi-state '(("\C-m" . nil)))
         (viper-add-local-keys 'insert-state '(("\C-m" . nil)))
         (ttm/howm-set-al-origin-return-on-viper viper-current-state)))
     (setq viper-vi-state-hook '(viper-restore-cursor-type)) ; hook変数はlistが望ましい
     (setq viper-insert-state-hook '(viper-set-insert-cursor-type))
     (defun init-ttm/howm ()
       (add-hook 'action-lock-mode-on-hook 'ttm/howm-add-viper-return)
       (add-hook 'viper-vi-state-hook 'ttm/howm-add-viper-return nil t)
       (add-hook 'viper-insert-state-hook 'ttm/howm-add-viper-return nil t)
       )
     (add-hook 'howm-mode-hook 'init-ttm/howm)

     ;; clmemo-inline-date-modeでviper-emacs-stateにならずとも挿入できるように
     (eval-after-load "clmemo"
       '(progn
          (defvar action-lock-original-return-vi-state-bak nil)
          (defvar action-lock-original-return-insert-state-bak nil)
          (defun action-lock-backup-origin-return ()
            (when action-lock-mode
              (setq action-lock-original-return-vi-state-bak action-lock-original-return-vi-state
                    action-lock-original-return-insert-state-bak action-lock-original-return-insert-state)
              (setq action-lock-original-return-vi-state 'clmemo-inline-date-insert-today
                    action-lock-original-return-insert-state 'clmemo-inline-date-insert-today)
              (ttm/howm-add-viper-return)))
          (defadvice clmemo-inline-date-insert-today (after clmemo-inline-date-with-viper-howm activate)
            (setq action-lock-original-return-vi-state action-lock-original-return-vi-state-bak
                  action-lock-original-return-insert-state action-lock-original-return-insert-state-bak)
            (setq action-lock-original-return-vi-state-bak nil
                  action-lock-original-return-insert-state-bak nil)
            (ttm/howm-set-al-origin-return-on-viper viper-current-state))
          (add-hook 'clmemo-inline-date-mode-hook 'action-lock-backup-origin-return)))))

;;;; アクティヴなTODOだけを表示
(global-set-key (kbd "C-c , T") 'howm-list-todo)
(define-key howm-mode-map (kbd "C-c , T") 'howm-list-todo)
(global-set-key (kbd "C-c , t") 'howm-list-active-todo) ;; default howm-insert-dtime
(define-key howm-mode-map (kbd "C-c , t") 'howm-list-active-todo) ;; default howm-insert-dtime
