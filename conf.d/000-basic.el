;;;; 勝手にリージョンを作成させないために・・ emacs23 ではdefault
(transient-mark-mode t)

;;;; C-u C-SPC C=SPC .. で pop-mark-command を連続実行になるように
(setq set-mark-command-repeat-pop t)

;;;; バックアップファイルを別フォルダに隔離
;(setq make-backup-files t)
(setq backup-directory-alist
      (cons (cons "\\.*$" (expand-file-name "~/.emacs.d/backup"))
            backup-directory-alist))

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

;;;; for Cygwin shell
(setq shell-command-switch "-c")
(setq explicit-bash-args '("--noediting" "-i"))
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(setq comint-prompt-read-only t)

;;;; ファイルを保存した時に勝手にパーミッションが変更されるのを防ぐ
;; (setq vc-mistrust-permissions t)
;; 不要なvc-backendを無効に
;; (setq vc-handled-backends '(Git Hg RCS))
(setq vc-handled-backends '(Git))

;;;; message buffer の長さを伸長 (default: 100)
(setq messages-buffer-max-lines 1000)

;;;; 同名のbuffer-nameを判別しやすくする
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
;; mode-lineのbuffer-nameをdirectory名まで常に表示する
(setq uniquify-min-dir-content 1)

;;;; minibufferでの入力履歴から重複を削除
(setq history-delete-duplicates t)

;;;; kill-emacs 時に確認する
(setq confirm-kill-emacs 'yes-or-no-p)

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

;;;; python-modeでffap-guessが固まるので(emacs.pyが必要)
(ly:eval-after-load 'python
  (ly:eval-after-load 'ffap
    (setq ffap-alist (assq-delete-all 'python-mode ffap-alist))))
