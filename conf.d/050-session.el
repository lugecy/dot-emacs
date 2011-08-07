;;;; session.el (最近開いたファイル・実行したコマンド(M-x)・開いたファイルのカーソル位置を
;;;; 保存する)
(require 'session)
(setq session-globals-include '((kill-ring 50)
                                (session-file-alist 500 t)
                                (file-name-history 1000))
      history-length             t
      session-globals-max-size   1000
      session-globals-max-string 100000000
      session-undo-check         -1
      session-save-file-coding-system 'utf-8-unix)
(add-to-list 'session-globals-exclude 'anything-c-adaptive-history)
(add-to-list 'session-globals-exclude 'historyf-history)
(add-hook 'after-init-hook 'session-initialize)
(setq ly:idle-save-session (run-with-idle-timer 180 t 'session-save-session))
