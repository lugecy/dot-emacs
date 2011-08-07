;;;; flymake でリアルタイム文法チェック - とりあえず暇だったし何となく始めたブログ
;;;; http://d.hatena.ne.jp/khiker/20070630/emacs_ruby_flymake
(require 'flymake)
(defun ly:flymake-conf-ruby-mode ()
  "Confiture flymake-mode for ruby-mode."
  (let ((func (if (featurep 'popup)
                  'flymake-display-err-menu-for-current-line
                'credmp/flymake-display-err-minibuf)))
    (define-key ruby-mode-map (kbd "C-c d") func)
    (when (fboundp 'anything-flymake)
      (define-key ruby-mode-map (kbd "C-c D") 'anything-flymake))))
(defun ly:flymake-conf-c-mode ()
  "Configure flymake-mode for c-mode-common-mode."
  (let ((func (if (featurep 'popup)
                  'flymake-display-err-menu-for-current-line
                'credmp/flymake-display-err-minibuf)))
    (define-key c-mode-map (kbd "C-c d") func)
    (when (fboundp 'anything-flymake)
      (define-key c-mode-map (kbd "C-c D") 'anything-flymake))))
(add-hook 'ruby-mode-hook 'ly:flymake-conf-ruby-mode)
(add-hook 'c-mode-common-hook 'ly:flymake-conf-c-mode)

;; Invoke ruby with '-c' to get syntax checking
(defun flymake-ruby-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "ruby" (list "-c" local-file))))
(push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)

;;;; Makefile 無しでflymakeを有効に(default: flymake-simple-make-init)
(defvar flymake-cc-program "gcc")
(defun flymake-cc-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list flymake-cc-program (list "-Wall" "-Wextra" "-fsyntax-only" local-file))))
(push '("\\.c$" flymake-cc-init) flymake-allowed-file-name-masks)

;;;; エラー内容を表示する
;; ミニバッファ表示用
(defun credmp/flymake-display-err-minibuf ()
  "Displays the error/warning for the current line in the minibuffer"
  (interactive)
  (let* ((line-no             (flymake-current-line-no))
         (line-err-info-list  (nth 0 (flymake-find-err-info flymake-err-info line-no)))
         (count               (length line-err-info-list))
         )
    (while (> count 0)
      (when line-err-info-list
        (let* ((file       (flymake-ler-file (nth (1- count) line-err-info-list)))
               (full-file  (flymake-ler-full-file (nth (1- count) line-err-info-list)))
               (text (flymake-ler-text (nth (1- count) line-err-info-list)))
               (line       (flymake-ler-line (nth (1- count) line-err-info-list))))
          (message "[%s] %s" line text)
          )
        )
      (setq count (1- count)))))

;; flymake 現在行のエラーをpopup.elのツールチップで表示する
;; gist: 292827 - GitHub
;; http://gist.github.com/292827
(ly:eval-after-load "popup"
    (defun flymake-display-err-menu-for-current-line ()
      (interactive)
      (let* ((line-no (flymake-current-line-no))
             (line-err-info-list (nth 0 (flymake-find-err-info flymake-err-info line-no))))
        (when line-err-info-list
          (let* ((count (length line-err-info-list))
                 (menu-item-text "")
                 (menu-item-list '()))
            (while (> count 0)
              (setq menu-item-text (flymake-ler-text (nth (1- count) line-err-info-list)))
              (let* ((file (flymake-ler-file (nth (1- count) line-err-info-list)))
                     (line (flymake-ler-line (nth (1- count) line-err-info-list))))
                (if file
                    (setq menu-item-text (concat menu-item-text " - " file "(" (format "%d" line) ")"))))
              (push menu-item-text menu-item-list)
              (setq count (1- count))
              )
            (popup-tip (mapconcat #'identity menu-item-list "\n")))))))

;; anythingを使ってflymakeのエラー行を表示する - TODO： 後で書く。
;; http://d.hatena.ne.jp/kiris60/20091003/1254579747
(eval-after-load "anything"
  '(progn
     (setq anything-c-source-flymake
           '((name . "Flymake")
             (init . (lambda ()
                       (setq anything-flymake-err-list
                             (loop for err-info in flymake-err-info
                                   for err = (nth 1 err-info)
                                   append err))))
             (candidates
              . (lambda ()
                  (mapcar
                   (lambda (err)
                     (let* ((text (flymake-ler-text err))
                            (line (flymake-ler-line err)))
                       (cons (format "[%s] %s" line text) err)))
                   anything-flymake-err-list)))
             (action
              . (("Goto line" . (lambda (candidate) (goto-line (flymake-ler-line candidate) anything-current-buffer)))))))

     (defun anything-flymake ()
       (interactive)
       (anything (list anything-c-source-flymake)))))
